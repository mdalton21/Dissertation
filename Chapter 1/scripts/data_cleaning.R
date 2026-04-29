## -----------------------------------------------------------------------------
##
## [ PROJ ] Civilian Electoral Contention
## [ FILE ] data_cleaning.R
## [ AUTH ] Maya A. Dalton; mad6821@psu.edu
## [ INIT ] 06 January 2026
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("haven", "readxl", "dplyr", "tidyverse", "ggplot2",
          "lfe", "tools", "forcats", "fixest", "vtable", "lme4", "ggeffects")
sapply(libs, require, character.only = TRUE)

## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
dir.create(dat_dir, recursive = TRUE, showWarnings = FALSE)
fig_path <- file.path(root, "figures")
dir.create(fig_path, recursive = TRUE, showWarnings = FALSE)

## load datasets
ecav <- read_csv(file.path(dat_dir, "ecav_main.csv"))
qog.ts <- read_csv(file.path(dat_dir, "qog_ts.csv"))
nelda <- read_excel(file.path(dat_dir, "NELDA.xls"))
vdem <- read_csv(file.path(dat_dir, "vdem.csv"))
ucdp <- read_csv(file.path(dat_dir, "ucdp_ged.csv"))
ciri <- read_excel(file.path(dat_dir, "ciri.xlsx"))

## load acled
## grant data files (use regular expression to pull only right ones
files <- list.files(file.path(dat_dir, "ACLED"), full.names = TRUE)

## map read all files
acled <- map(files,
             ~ read_csv(.x, show_col_types = FALSE) |> 
               filter(str_detect(event_type, "Violence against civilians")) |> # select civ violence 
               select(country, year, disorder_type, event_type, sub_event_type)) |>
  bind_rows()

## -----------------------------------------------------------------------------
## cleaning and subsetting datasets
## -----------------------------------------------------------------------------

## select variables of interest from qog: year, country, gdp, pop
qog.c <- qog.ts %>%
  mutate(l_gdppc = log(wdi_gdpcapcur),
         l_pop = log(wdi_pop)) %>%
  dplyr::select(cname, year, 
                l_gdppc,                            ## logged gdp
                l_pop,                              ## logged pop
                indep_jud = bti_ij)                 ## judicial independence

## select variables of interest from nelda: year, country, etc. 
nelda.c <- nelda %>%
  dplyr::select(country, year, 
                opposition_ban = nelda13,           ## opposition allowed? 
                reports_fraud = nelda28,            ## reports of fraud widespread? 
                elect_monitors = nelda45)           ## int'l monitors? 

## select variables of interest from nelda: year, country, etc. 
vdem.c <- vdem %>%
  mutate(e_p_polity = case_when(
    e_p_polity == '-66' ~ NA,
    e_p_polity == '-77' ~ NA,
    e_p_polity == '-88' ~ NA,
    TRUE ~ e_p_polity
  )) %>%
  dplyr::select(country = country_name, year, 
                polity = e_p_polity,                ## regime type
                govt_intimidation = v2elintim,      ## intimidation by govt
                elect_boycott = v2elboycot,         ## election boycott 
                cso_idx = v2x_cspart,               ## civil society participation
                elect_idx = v2xel_frefair,          ## clean elections index
                corrupt_jud = v2jucorrdc,           ## judicial corruption 
                peaceful_assemb = v2caassemb)       ## peaceful assembly

## select variables of interest from ucdp: year, country, etc. 
ucdp.c <- ucdp %>%
  dplyr::select(country, year, side_a, side_b, 
                deaths_civilians, type_of_violence) %>%
  filter(type_of_violence == 3) %>%
  dplyr::group_by(country, year) %>%
  dplyr::summarise(
    deaths_civilians = sum(deaths_civilians, na.rm = TRUE),
    .groups = "drop"
  )

## select variables of interest from ciri: country, year, physical integrity index
ciri.c <- ciri %>%
  dplyr::select(country = CTRY, year = YEAR,
                phys_int = PHYSINT) 

## aggregate violence variable from acled
acled.c <- acled %>%
  dplyr::group_by(country, year) %>%
  dplyr::summarise(
    civ_violence = sum(event_type == "Violence against civilians", na.rm = TRUE),
    .groups = "drop"
  )

## mutate date, select variables, remove unknown actors
ecav.c <- ecav %>%
  mutate(year = format(as.Date(Electiondate, format="%Y/%m/%d"),"%Y"), 
         year = as.numeric(year),
         election_year = format(as.Date(Electiondate, format="%Y/%m/%d"),"%Y"),
         election_year = as.numeric(election_year)) %>%
  select(country, election_year, year, EventViolence, ev_count, Actor1Type, ActorNameNew) %>%
  filter(Actor1Type != -99)

## ecav actors from string to ordinal
ecav.c <- ecav.c %>%
  mutate(ActorTypeNew = case_when(
      ActorNameNew %in% c("Government Workers", "Government", "Government Officials", 
                          "Government Leader", "Former Government") ~ 1,
      ActorNameNew %in% c("Civilians", "Party Supporters", "Students", "Political Activists",
                          "Party Youth", "Opposition Supporters", "Opposition Activists", "Protestors") ~ 2,
      ActorNameNew %in% c("Political Party", "Political Candidate", "Party Workers", 
                          "Party Members", "Party Leaders") ~ 3,
      ActorNameNew %in% c("Opposition Party", "Opposition Leaders", "Opposition Candidate") ~ 4, 
      ActorNameNew %in% c("Election Officials", "Election Workers", "Election Commission") ~ 5,
      ActorNameNew %in% c("Militant Group", "Terrorist Group", "Rebel Group", "Armed Group") ~ 6,
      ActorNameNew %in% c("Military", "Police") ~ 7,
      ActorNameNew %in% c("Media") ~ 8,
      ActorNameNew %in% c("United States", "Foreign Entities") ~ 9,
      ActorNameNew %in% c("Unknown") ~ NA,
    )
  )

## merge ecav with other data sets 
df <- ecav.c %>%
  left_join(qog.c, by = c('country'='cname', "year")) %>%
  left_join(nelda.c, by = c('country', 'year')) %>%
  left_join(vdem.c, by = c('country', 'year')) %>%
  left_join(ucdp.c, by = c('country', 'year')) %>%
  left_join(ciri.c, by = c('country', 'year')) %>%
  left_join(acled.c, by = c('country', 'year')) %>%
  mutate(opposition_ban = case_when(        # mutate NELDA to binary 
            opposition_ban == 'yes' ~ 1, 
            opposition_ban == 'no' ~ 0,
            opposition_ban == 'N/A' ~ NA,
            opposition_ban == 'unclear' ~ NA),
         reports_fraud = case_when(
           reports_fraud == 'yes' ~ 1, 
           reports_fraud == 'no' ~ 0,
           reports_fraud == 'N/A' ~ NA,
           reports_fraud == 'unclear' ~ NA),
         elect_monitors = case_when(
           elect_monitors == 'yes' ~ 1, 
           elect_monitors == 'no' ~ 0,
           elect_monitors == 'N/A' ~ NA,
           elect_monitors == 'unclear' ~ NA)) %>%
  select(country, year, elect_vio = EventViolence, ActorNameNew, ActorTypeNew,    ## ecav vars
         civ_deaths = deaths_civilians, l_gdppc, l_pop, indep_jud,                ## ucdp & qog vars
         opposition_ban, reports_fraud, elect_monitors,                           ## nelda vars
         govt_intimidation, elect_boycott, cso_idx, elect_idx,                    ## vdem vars
         corrupt_jud, peaceful_assemb, polity,
         phys_int, civ_violence) %>%                                              ## ciri/acled vars
  group_by(country) %>%
  mutate(lag_ev = dplyr::lag(elect_vio, n = 1, default = NA),
         lag_civ_deaths = dplyr::lag(civ_deaths, n = 1, default = NA),
         lag_civ_violence = dplyr::lag(civ_violence, n = 1, default = NA))

df_civilians <- df %>%
  subset(ActorNameNew == "Civilians") 

write_csv(df_civilians, file.path(dat_dir, "df_civilians.csv"))

## -----------------------------------------------------------------------------
## Visualizations
## -----------------------------------------------------------------------------

## -------------------------------------
## actor counts
## -------------------------------------

png(file.path(fig_path, "actors.png"), res=100)
df %>% 
  mutate(actors = case_when(
    ActorTypeNew == 1 ~ "Government",
    ActorTypeNew == 2 ~ "Civilians",
    ActorTypeNew == 3 ~ "Party",
    ActorTypeNew == 4 ~ "Opposition",
    ActorTypeNew == 5 ~ "Election Officials",
    ActorTypeNew == 6 ~ "Non-State Armed Groups",
    ActorTypeNew == 7 ~ "Military/Police",
    ActorTypeNew == 8 ~ "Media",
    ActorTypeNew == 9 ~ "United States"
  )) %>%
  drop_na(actors) %>%
  group_by(actors) %>%
  tally() %>%
  ggplot(aes(x=reorder(actors, -n), y=n))+
    geom_bar(fill="black", stat="identity")+
    labs(x="", y="Count", title="Electoral Violence by Actors (ECAV)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 
dev.off()

## -------------------------------------
## count of violent/non-violent by civilians
## -------------------------------------

####### CIVILIAN BAR PLOTS
png(file.path(fig_path, "civilians_bar.png"), res=100)
df_civilians %>%
  mutate(
    elect_vio = case_when(
      elect_vio == 1 ~ "Violence",
      elect_vio == 0 ~ "Non-Violence"
    ) 
  )%>%
  ggplot(aes(x=elect_vio))+
  geom_bar(fill="firebrick") +
  geom_label(aes(label = ..count.., 
                 y = ..count.. + 3),  # Adjust padding dynamically
             stat = "count",
             size = 3, 
             fontface = "bold", 
             fill = "white",  # White background
             color = "black", 
             label.size = 0.1) +  # Thin border around the box
  labs(x="", y="Count", title="Violence vs. Non-Violence by Civilians (ECAV)") +
  theme_minimal()
dev.off()

## -------------------------------------
## summary table
## -------------------------------------

df_sub <- df_civilians %>%
  select(country, year, elect_vio, lag_ev,                        ## ecav vars
         l_gdppc, l_pop, indep_jud, civ_deaths, lag_civ_deaths,   ## ucdp & qog vars
         opposition_ban, reports_fraud, elect_monitors,           ## nelda vars
         govt_intimidation, elect_boycott, cso_idx, elect_idx,    ## vdem vars
         corrupt_jud, peaceful_assemb, polity,
         phys_int, civ_violence, lag_civ_violence)                ## ciri/acled vars

var.labs <- data.frame(var = c('year', 'elect_vio', 
                               'corrupt_jud', 'indep_jud',  
                               'opposition_ban', 'elect_monitors', 'govt_intimidation', 'elect_boycott',
                               'reports_fraud', 'elect_idx', 'cso_idx', 'peaceful_assemb', 'phys_int', 
                               'lag_ev', 'civ_deaths', 'lag_civ_deaths', 'civ_violence', 'lag_civ_violence',
                               'polity',   'l_gdppc', 'l_pop'),
                       labels = c('Year', 'Electoral Violence', 
                                  "Corrupt Judiciary", "Independent Judiciary", 
                                  "Banned Opposition", "Int'l Monitors", "Intimidation by Government", "Election Boycott", 
                                  "Reports of Election Fraud", "Clean Elections Index", 
                                  "CSO Participation", "Peaceful Assembly", "Physical Integrity Index", 
                                  "EV(t-1)", "Civilian Deaths", "Civilian Deaths (t-1)",
                                  "Violence Against Civilians", "Violence Against Civilians (t-1)", 
                                  "Polity", "GDP (logged)", "Population (logged)"))


st(df_sub, labels=var.labs, summ=c('notNA(x)','min(x)','max(x)', 'mean(x)','sd(x)'), 
   summ.names = c('N','Min','Max', 'Mean','Sd'), out='latex', title="Summary Statistics",
   file=file.path(fig_path, "sumtable"))

## -------------------------------------
## list of countries in df
## -------------------------------------
cnames <- unique(df_civilians$country)
cnames <- sort(cnames)
cname.file <- paste(cnames, collapse = ", ")
cat(cname.file, file=file.path(fig_path, "cnames.tex"))

