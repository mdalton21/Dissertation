## -----------------------------------------------------------------------------
##
## [ PROJ ] Gender Quotas and Electoral Violence
## [ FILE ] data_clean_visualization.R
## [ AUTH ] Maya A. Dalton; mad6821@psu.edu
## [ INIT ] 28 November 2023
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("haven", "readxl", "dplyr", "tidyverse", "ggplot2")
sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
dir.create(dat_dir, recursive = TRUE, showWarnings = FALSE)
fig_path <- file.path(root, "figures")
tab_path <- file.path(root, "tables")
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_path, recursive = TRUE, showWarnings = FALSE)

## load datasets
ecav <- read_excel(file.path(dat_dir, "ecav.xls"))
vdem <- read_csv(file.path(dat_dir, "vdem.csv"))
qog.ts <- read_csv(file.path(dat_dir, "qog_ts.csv"))

## -----------------------------------------------------------------------------
## cleaning and subsetting ecav, vdem, and qog
## -----------------------------------------------------------------------------

## mutate election year in ecav
ecav <- ecav %>%
  mutate(year = format(as.Date(Date, format="%Y/%m/%d"),"%Y"), 
         year = as.numeric(year),
         election_year = format(as.Date(Electiondate, format="%Y/%m/%d"),"%Y"),
         election_year = as.numeric(election_year))  

## select variables of interest from vdem: year, country, quota vars, add't mods
vdem.c <- vdem %>%
  dplyr::select(year, country_name, v2lgqugen)

## cleaning quota variables to binary
vdem.c <- vdem.c %>%
  mutate(quota_bi = case_when(
    v2lgqugen == 0 ~ 0,
    v2lgqugen %in% c(1:4) ~ 1
  ))

## select variables of interest from qog: year, country, gdp, pop, married
qog.c <- qog.ts %>%
  dplyr::select(cname, year, wdi_gdpcapcur, wdi_pop, wdi_wofm15)

## -----------------------------------------------------------------------------
## cleaning and merging regional ACLED data
## -----------------------------------------------------------------------------

## grant data files (use regular expression to pull only right ones
files <- list.files(file.path(dat_dir, "ACLED"), full.names = TRUE)

## map read all files
acled <- map(files,
                ~ read_csv(.x, show_col_types = FALSE) |> 
               filter(str_detect(notes, "election")) |> # select election events
               mutate(elect_vio = ifelse(sub_event_type == "Peaceful protest", 0, 1)) |> # subset for violent/non-violent 
               select(country, year, elect_vio, disorder_type, event_type, sub_event_type, actor1, actor2)
             ) |>
  bind_rows() |>
  drop_na(elect_vio)

## -----------------------------------------------------------------------------
## Merge ECAV data for main analysis
## -----------------------------------------------------------------------------

df_ecav <- ecav %>%
  left_join(vdem.c, by = c('country'='country_name', "year")) %>% # join with vdem
  left_join(qog.c, by = c('country'='cname', "year")) %>% # join with qog
  mutate(Actor1Type = case_when( # recode missing
    Actor1Type == -99 ~ NA,
    Actor1Type %in% c(1:5) ~ Actor1Type
  ), Target1Type = case_when(
    Target1Type == -99 ~ NA,
    Target1Type %in% c(1:5) ~ Target1Type
  )) %>%
  select(country, year, elect_vio = EventViolence, gdp_pc = wdi_gdpcapcur, wdi_pop, # select vars
         quota_ord = v2lgqugen, quota_bi, civil_lib = v2x_gencl, wom_married15 = wdi_wofm15,
         actors = Actor1Type, targets = Target1Type) %>%
  drop_na(quota_ord) %>%
  group_by(country) %>%
  mutate(lag_ev = dplyr::lag(elect_vio, n = 1, default = NA))

write_csv(df_ecav, file.path(dat_dir, "df_ecav.csv"))

## -----------------------------------------------------------------------------
## Merge ACLED data for robustness check
## -----------------------------------------------------------------------------

df_acled <- acled %>%
  left_join(vdem.c, by=c('country'='country_name', "year")) %>%
  left_join(qog.c, by=c('country'='cname', "year")) %>%
  drop_na(v2lgqugen) %>%
  group_by(country) %>%
  mutate(lag_ev = dplyr::lag(elect_vio, n = 1, default = NA),
         quota_bi = case_when(
           v2lgqugen == 0 ~ 0,
           v2lgqugen %in% c(1:4) ~ 1
         )) %>%
  select(country, year, elect_vio, gdp_pc = wdi_gdpcapcur, wdi_pop,
         quota_ord = v2lgqugen, quota_bi, civil_lib = v2x_gencl, wom_married15 = wdi_wofm15, lag_ev) 

write_csv(df_acled, file.path(dat_dir, "df_acled.csv"))

## -----------------------------------------------------------------------------
## Visualizations of VDEM Data (Quotas)
## -----------------------------------------------------------------------------

## -------------------------------------
## quotas start and end dates
## -------------------------------------

quotas.df <- vdem.c %>%
  drop_na() %>%
  group_by(country_name) %>%
  subset(v2lgqugen %in% c(1:4)) %>% # has a quota
  mutate(start_date = min(year),
         end_date = max(year)) %>%
  select(country_name, start_date, end_date) %>%
  distinct()

## calculate the length of each quota period in days
quotas.df$length <- as.numeric(quotas.df$end_date - quotas.df$start_date)

## identify the maximum year in the QuotaEnd column
max_year <- max(quotas.df$end_date)

## reorder the data based on the length of the quota periods
quotas.df <- quotas.df %>%
  arrange(desc(length)) 

## subset into two halves for easier plotting
quotas.df1 <- quotas.df[1:40,]
quotas.df2 <- quotas.df[41:85,]


## plot first half of data
png(file.path(fig_path, "quotas_length1.png"), res=100)
ggplot(quotas.df1) +
  geom_segment(aes(x = start_date, xend = end_date,
                   y = reorder(country_name, length), yend = country_name,
                   color = ifelse(country_name == "Kenya", "Kenya", "Other"))) +
  geom_point(aes(x = start_date, y = reorder(country_name, length),
                 color = ifelse(country_name == "Kenya", "Kenya", "Other"))) +
  geom_point(data = subset(quotas.df1, end_date != max_year),
             aes(x = end_date, y = country_name,
                 color = ifelse(country_name == "Kenya", "Kenya", "Other"))) +
  scale_color_manual(values = c("Kenya" = "red", "Other" = "slateblue")) +
  labs(x = "Year",
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6, angle = 30),
        axis.text.x = element_text(size = 6, angle = 30),
        legend.position = "none") # Hide legend if unnecessary
dev.off()

## plot second half of data
png(file.path(fig_path, "quotas_length2.png"), res=100)
ggplot(quotas.df2) +
  geom_segment(aes(x = start_date, xend = end_date, # add line
                   y = reorder(country_name, length), yend = country_name), color = "slateblue") +
  geom_point(aes(x = start_date, y = reorder(country_name, length)), color = "slateblue") + # add point for start
  geom_point(data = subset(quotas.df2, end_date != max_year), # add point only if ending before 2021
             aes(x = end_date, y = country_name), color = "slateblue") +
  labs(x = "Year",
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size=6, angle=30),
        axis.text.x = element_text(size=6, angle=30))
dev.off()

## -------------------------------------
## quotas counts (binary) - not in text
## -------------------------------------
png(file.path(fig_path, "quotas_count.png"), res=100)
vdem.c %>% 
  drop_na(v2lgqugen) %>%
  group_by(country_name) %>%
  summarise(
    quotas = if_else(any(v2lgqugen %in% 1:4),
                     "Quota",
                     "No Quota"),
    .groups = "drop"
  ) %>%
  count(quotas) %>%
  ggplot(aes(x=reorder(quotas, -n), y=n))+
  geom_bar(fill="black", stat="identity")+
  labs(x="", y="Count", title="Number of Gender Quotas")
dev.off()

## -------------------------------------
## quotas counts (ordinal) - appendix
## -------------------------------------
png(file.path(fig_path, "quotas_ordinal.png"), res=100)
vdem.c %>% 
  mutate(quotas = case_when(
    v2lgqugen == 0 ~ NA,
    v2lgqugen == 1 ~ "No Sanctions",
    v2lgqugen == 2 ~ "Weak Sanctions",
    v2lgqugen == 3 ~ "Strong Sanctions",
    v2lgqugen == 4 ~ "Reserved Seats",
  )) %>%
  drop_na(quotas) %>%
  group_by(country_name, quotas) %>%
  tally() %>%
  ggplot(aes(x=reorder(quotas, -n), y=n))+
  geom_bar(fill="black", stat="identity")+
  labs(x="", y="Count") + 
  theme_minimal()
dev.off()


## -----------------------------------------------------------------------------
## Visualizations of ECAV Data
## -----------------------------------------------------------------------------

## -------------------------------------
## trend line of EV over time
## -------------------------------------

png(file.path(fig_path, "ev_occur_ecav.png"), res=100)
df_ecav %>%
  group_by(year) %>%
  mutate(total_vio = sum(elect_vio)) %>%
  ggplot(aes(x=year, y=total_vio)) +
    geom_line(stat="smooth")+
    ylim(0, 1000)+
    labs(y="Electoral Violence Events", x="Year", 
         title="Electoral Violence Over Time (ECAV)")+
    theme_minimal()
dev.off()

## -------------------------------------
## count of violent/non-violent - not in text
## -------------------------------------
png(file.path(fig_path, "ev_count_ecav.png"), res=100)
df_ecav %>%
  mutate(elect_vio = case_when(
    elect_vio == 1 ~ "Violence",
    elect_vio == 0 ~ "No Violence"
  )) %>%
  ggplot(aes(x=elect_vio))+
    geom_bar(fill="slateblue")+
    labs(x="", y="Count", title="Violent Elections (ECAV)") +
    theme_minimal()
dev.off()

## -------------------------------------
## moderator variable by quota status - % woman married at 15
## -------------------------------------
png(file.path(fig_path, "marry_ecav.png"), res=100)
df_ecav %>%
  mutate(quota_bi = case_when(
    quota_bi == 1 ~ "Quota",
    quota_bi == 0 ~ "No Quota"
  )) %>%
  drop_na(wom_married15) %>%
  ggplot(aes(x=wom_married15))+
  geom_histogram(fill="black", color="white", bins=30)+
  facet_wrap(~as.factor(quota_bi)) +
  labs(x="% of Women Married Young", y="Count of Countries", 
       #title="% of Women First Married at 15 by\nQuota Status (ECAV)"
       ) + 
  theme_minimal()
dev.off()

## -------------------------------------
## relationship between ev and married - not in text
## -------------------------------------
png(file.path(fig_path, "marry_ev_ecav.png"), res=100)
df_ecav %>%
  group_by(year) %>%
  mutate(total_vio = sum(elect_vio)) %>%
  drop_na(wom_married15) %>%
  ggplot(aes(x=wom_married15, y=total_vio))+
  geom_point()+
  labs(x="% Women Married by 15", y="Total Violence Elections", 
       title="% Married by 15 and\nElectoral Violence (ECAV)") + 
  theme_minimal()
dev.off()

## -------------------------------------
## list of countries in ECAV
## -------------------------------------
cnames <- unique(df_ecav$country)
cname.file <- paste(cnames, collapse = ", ")
cat(cname.file, file=file.path(tab_path, "cname-ecav.tex"))

## -----------------------------------------------------------------------------
## Visualizations of ACLED Data
## -----------------------------------------------------------------------------

## -------------------------------------
## trend line of EV over time - not in text
## -------------------------------------

png(file.path(fig_path, "ev_occur_acled.png"), res=100)
df_acled %>%
  group_by(year) %>%
  mutate(total_vio = sum(elect_vio, na.rm=T)) %>%
  ggplot(aes(x=year, y=total_vio)) +
    geom_line(stat="smooth")+
    labs(y="Electoral Violence Events", x="Year", 
         title="Electoral Violence Over Time (ACLED)") + 
    theme_minimal()
dev.off()

## -------------------------------------
## count of EV occurences - not in text
## -------------------------------------
png(file.path(fig_path, "ev_count_acled.png"), res=100)
df_acled %>%
  mutate(elect_vio = case_when(
    elect_vio == 1 ~ "Violence",
    elect_vio == 0 ~ "No Violence"
  )) %>%
  subset(!is.na(c(elect_vio))) %>%
  ggplot(aes(x=elect_vio))+
  geom_bar(fill="black")+
  labs(x="", y="Count", title="Violence Elections (ACLED)") + 
  theme_minimal()
dev.off()

## -------------------------------------
## moderator variable by quota status - % woman married at 15
## -------------------------------------
png(file.path(fig_path, "marry_acled.png"), res=100)
df_acled %>%
  mutate(quota_bi = case_when(
    quota_bi == 1 ~ "Quota",
    quota_bi == 0 ~ "No Quota"
  )) %>%
  drop_na(wom_married15) %>%
  ggplot(aes(x=wom_married15))+
  geom_histogram(fill="black", color="white", bins=30)+
  facet_wrap(~as.factor(quota_bi)) +
  labs(x="% of Women Married by 15", y="Count of Countries", 
       title="% of Women First Married at 15 by\nQuota Status (ACLED)") + 
  theme_minimal()
dev.off()

## -------------------------------------
## relationship between ev and mod - not in text
## -------------------------------------
png(file.path(fig_path, "marry_ev_acled.png"), res=100)
df_acled %>%
  group_by(year) %>%
  mutate(total_vio = sum(elect_vio)) %>%
  drop_na(wom_married15) %>%
  ggplot(aes(x=wom_married15, y=total_vio))+
  geom_point()+
  labs(x="% Women Married by 15", y="Total Violence Elections", 
       title="% Married by 15 and\nElectoral Violence (ECAV)") + 
  theme_minimal()
dev.off()

## -------------------------------------
## list of countries in ACLED
## -------------------------------------
cnames <- unique(df_acled$country)
cnames <- sort(cnames)
cname.file <- paste(cnames, collapse = ", ")
cat(cname.file, file=file.path(tab_path, "cname-acled.tex"))

