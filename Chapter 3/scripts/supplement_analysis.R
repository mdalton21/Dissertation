## -----------------------------------------------------------------------------
##
## [ PROJ ] Gender Quotas and Electoral Violence
## [ FILE ] supplement_analysis.R
## [ AUTH ] Maya A. Dalton; mad6821@psu.edu
## [ INIT ] 14 November 2024
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("haven", "readxl", "dplyr", "tidyverse", "ggplot2", "stargazer",
          "vtable", "lfe", "sjPlot", "sjmisc", "broom", "rdd", "rdrobust")
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

## read in datasets for analysis
df_acled <- read_csv(file.path(dat_dir, "df_acled.csv"))
df_ecav <- read_csv(file.path(dat_dir, "df_ecav.csv"))

## -----------------------------------------------------------------------------
## Supplementary Analysis - Addt'l Gender Norm Measures
## -----------------------------------------------------------------------------

qog.ts <- read_csv(file.path(dat_dir, "qog_ts.csv"))

## select variables of interest from qog: year, country, gdp, pop, married
qog.c <- qog.ts %>%
  dplyr::select(country = cname, year, 
                perc_secondary_w = bl_lsf, 
                wbl_idx = wdi_wombuslawi, 
                fertility = wdi_fertility)

df_ecav <- df_ecav %>%
  left_join(qog.c, by = c('country', "year")) # join with qog

df_ecav <- df_ecav %>% drop_na(perc_secondary_w)

## -------------------------------------
## Fixed Effects - Secondary School
## -------------------------------------
alt.m1 <- felm(elect_vio  ~ quota_bi + perc_secondary_w + quota_bi*perc_secondary_w + 
                  log(gdp_pc) + log(wdi_pop) + 
                  lag_ev | country + year | 0 | country, data = df_ecav, keepCX = TRUE)
summary(alt.m1)

## -------------------------------------
## Fixed Effects - WBL Index
## -------------------------------------
alt.m2 <- felm(elect_vio  ~ quota_bi + wbl_idx + quota_bi*wbl_idx + 
                  log(gdp_pc) + log(wdi_pop) + 
                  lag_ev | country + year | 0 | country, data = df_ecav, keepCX = TRUE)
summary(alt.m2)

## -------------------------------------
## Fixed Effects - Fertility Rate
## -------------------------------------
alt.m3 <- felm(elect_vio  ~ quota_bi + fertility + quota_bi*fertility + 
                 log(gdp_pc) + log(wdi_pop) + 
                 lag_ev | country + year | 0 | country, data = df_ecav, keepCX = TRUE)
summary(alt.m3)

## -------------------------------------
## stargazer table
## -------------------------------------
stargazer(alt.m1, alt.m2, alt.m3, header = FALSE, font.size = "footnotesize", no.space=T,
          title="Effect of Gender Quotas on Electoral Violence (VDEM)", 
          dep.var.labels = "Electoral Violence (VDEM)", 
          column.labels = c("Secondary School", "WBL Index", "Fertility"), 
          model.names = FALSE, keep=c("quota_bi", "perc_secondary_w", "wbl_idx", "fertility",
                                      "quota_bi*perc_secondary_w", "quota_bi*wbl_idx", "quota_bi*fertility", 
                                      "gdp_pc", "wdi_pop", "elect_vio", "lag_ev"), 
          covariate.labels = c("Gender Quota", "Perc. Women with Secondary School", 
                               "Women Business and the Law (WBL) Index", "Fertility Rate",
                               "log(GDPPC)", "log(Population)",
                               "Electoral Violence (t-1)",
                               "Quota*Secondary School", "Quota*WBL Index", "Quota*Fertility"), 
          out=file.path(tab_path, "alt_models.tex"))

## -----------------------------------------------------------------------------
## Supplementary Analysis - Types of Quota (non-binary, placement, and threshold)
## -----------------------------------------------------------------------------

## -------------------------------------
## Logit Model - Ordinal Quotas
## -------------------------------------

## ECAV ------------------------------------------------------------------------
ecav_quota_ord <- felm(elect_vio ~ quota_ord + wom_married15 + quota_ord*wom_married15 + log(gdp_pc) + log(wdi_pop) + lag_ev | country + year | 0 | country, 
                       data = df_ecav, keepCX = TRUE)
summary(ecav_quota_ord)
# no sanctions - diminishes by 18%
# weak sanctions - diminishes by 37.5%
# strong sanctions - diminishes by 56.3%
# reserved seats - diminishes by 75%
# no sanctions, % wom = 10 - increases by 1.02%
# weak sanctions, % wom = 10 - increases by 2.04
# strong sanctions, % wom = 10 - increases by 3.07%
# reserved seats, % wom = 10 - increases by 4.09%

## -------------------------------------
## coefficient plots
## -------------------------------------

## Extract coefficients and confidence intervals
coef_df <- tidy(ecav_quota_ord, conf.int = TRUE) %>%
  mutate(term = case_when(
    term == "lag_ev" ~ "EV (t-1)",
    term == "log(gdp_pc)" ~ "log(GDP)",
    term == "log(wdi_pop)" ~ "log(Population)",
    term == "quota_ord" ~ "Quota (Ordinal)",
    term == "quota_ord:wom_married15" ~ "Quota (Ordinal)*Married",
    term == "wom_married15" ~ "Married"))

## Create the coefficient plot
png(file.path(fig_path, "ecav_ordinal.png"), res=100)
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept=0, linewidth = 0.8, linetype=3, color="red") +
  labs(subtitle = "Effect of Gender Quotas Sanctions on Electoral Violence (ECAV)",
       x = "",
       y = "Estimate") +
  coord_flip() +
  theme_minimal()
dev.off()

## -------------------------------------
## stargazer table
## -------------------------------------

stargazer(ecav_quota_ord, header = FALSE, font.size = "footnotesize", no.space=T,
          title="Effect of Gender Quota Sanctions on Electoral Violence (ECAV)", 
          dep.var.labels = "Electoral Violence (ECAV)", 
          model.names = FALSE, keep=c("quota_ord", "wom_married15", "quota:wom_married15", "gdp_pc", 
                                      "wdi_pop", "elect_vio", "lag_ev"), 
          covariate.labels = c("Gender Quota (Ordinal)", "% Married at 15", "log(GDPPC)", "log(Population)",
                               "Electoral Violence (t-1)", "Quota (Ordinal)*Married"),
          out=file.path(tab_path, "ecav_ordinal.tex"))

################################################################################
################################################################################
################################################################################

## ACLED ------------------------------------------------------------------------
acled_quota_ord <- felm(elect_vio  ~ quota_ord + wom_married15 + quota_ord*wom_married15 + log(gdp_pc) + log(wdi_pop) + 
                          lag_ev | country + year | 0 | country, data = df_acled, keepCX = TRUE)
summary(acled_quota_ord)

# EV risk increases w/ quota sanction strength, but not significant 
# no sanctions, % wom = 10 - increases by 1%
# weak sanctions, % wom = 10 - increases by 2%
# strong sanctions, % wom = 10 - increases by 3%
# reserved seats, % wom = 10 - increases by 4%

## -------------------------------------
## coefficient plots
## -------------------------------------

## Extract coefficients and confidence intervals
coef_df <- tidy(acled_quota_ord, conf.int = TRUE) %>%
  mutate(term = case_when(
    term == "lag_ev" ~ "EV (t-1)",
    term == "log(gdp_pc)" ~ "log(GDP)",
    term == "log(wdi_pop)" ~ "log(Population)",
    term == "quota_ord" ~ "Quota (Ordinal)",
    term == "quota_ord:wom_married15" ~ "Quota (Ordinal)*Married",
    term == "wom_married15" ~ "Married"))

## Create the coefficient plot
png(file.path(fig_path, "acled_ordinal.png"), res=100)
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept=0, linewidth = 0.8, linetype=3, color="red") +
  labs(subtitle = "Effect of Gender Quotas Sanctions on Electoral Violence (ACLED)",
       x = "",
       y = "Estimate") +
  coord_flip() +
  theme_minimal()
dev.off()

## -------------------------------------
## stargazer table
## -------------------------------------

stargazer(acled_quota_ord, header = FALSE, font.size = "footnotesize", no.space=T,
          title="Effect of Gender Quota Sanctions on Electoral Violence (ACLED)", 
          dep.var.labels = "Electoral Violence (ACLED)", 
          model.names = FALSE, keep=c("quota_ord", "wom_married15", "quota:wom_married15", "gdp_pc", 
                                      "wdi_pop", "elect_vio", "lag_ev"), 
          covariate.labels = c("Gender Quota (Ordinal)", "% Married at 15", "log(GDPPC)", "log(Population)",
                               "Electoral Violence (t-1)", "Quota (Ordinal)*Married"),
          out=file.path(tab_path, "acled_ordinal.tex"))

## -----------------------------------------------------------------------------
## Supplementary Analysis - RDD using Quota implemented as cutoff
## -----------------------------------------------------------------------------

## -------------------------------------
## prepare data for RDD - ECAV
## -------------------------------------
set.seed(123)
df_rdd <- df_ecav %>%
  group_by(country) %>%
  mutate(quota_year = ifelse(quota_bi == 1, year, NA), # getting years w/ quotas
         quota_start = ifelse(all(is.na(quota_year)), NA, min(quota_year, na.rm = TRUE)), # start year
         run_var = year - quota_start, # length between year and quota start
         run_var = ifelse(is.na(run_var), -30, run_var), # running var w/o quotas
         quota_trt = ifelse(run_var >= 0, 1, 0), # trt var
         run_var_jittered = run_var + rnorm(n(), mean = 0, sd = 0.01)) # error terms to running var

## -------------------------------------
## RDD Estimate
## -------------------------------------
cutoff_mod <- felm(elect_vio ~ run_var + wom_married15 + run_var*wom_married15 + log(gdp_pc) + log(wdi_pop) + lag_ev | country + year | 0 | country, 
                   data = df_rdd, keepCX = TRUE)

summary(cutoff_mod) 

stargazer(cutoff_mod, header = FALSE, font.size = "footnotesize", no.space=T,
          title="Effect of Time Since Quota Implementation on Electoral Violence (ECAV)", 
          dep.var.labels = "Electoral Violence (ECAV)", 
          model.names = FALSE, keep=c("run_var", "wom_married15", "run_var:wom_married15", "gdp_pc", 
                                      "wdi_pop", "elect_vio", "lag_ev"), 
          covariate.labels = c("Years Since Quota", "% Married at 15", "log(GDPPC)", "log(Population)",
                               "Electoral Violence (t-1)", "Quota (Years)*Married"),
          out=file.path(tab_path, "ecav_quota_years.tex"))

# for each year after quota implementation, EV increases by ~ 4%
# at 8% of women married young, inc is less than 1%

## -------------------------------------
## RDD plot
## -------------------------------------
png(file.path(fig_path, "ecav_rdd.png"), res=100)
ggplot(df_rdd, aes(x = run_var, y = elect_vio)) +
  stat_summary_bin(fun = sum, bins = 20, geom = "point") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Electoral Violence Events around Quota Implementation",
       x = "Years Since Quota Implementation",
       y = "Sum of Electoral Violence Events (ECAV)") + 
  theme_minimal()
dev.off()

################################################################################
################################################################################
################################################################################

## -------------------------------------
## prepare data for RDD - ACLED
## -------------------------------------
set.seed(123)
df_rdd <- df_acled %>%
  group_by(country) %>%
  mutate(quota_year = ifelse(quota_bi == 1, year, NA), # getting years w/ quotas
         quota_start = ifelse(all(is.na(quota_year)), NA, min(quota_year, na.rm = TRUE)), # start year
         run_var = year - quota_start, # length between year and quota start
         run_var = ifelse(is.na(run_var), -30, run_var), # running var w/o quotas
         quota_trt = ifelse(run_var >= 0, 1, 0), # trt var
         run_var_jittered = run_var + rnorm(n(), mean = 0, sd = 0.01)) # error terms to running var

## -------------------------------------
## RDD Estimate
## -------------------------------------
rd_est <- rdrobust(df_rdd$elect_vio, df_rdd$run_var_jittered, 
                   covs = df_rdd$wom_married15,
                   cluster = df_rdd$year,  all=TRUE)
summary(rd_est)

cutoff_mod <- felm(elect_vio ~ run_var + wom_married15 + run_var*wom_married15 + log(gdp_pc) + log(wdi_pop) + lag_ev | country + year | 0 | country, 
                   data = df_rdd, keepCX = TRUE)

summary(cutoff_mod) # effects are same as ECAV

## -------------------------------------
## RDD plot
## -------------------------------------
ggplot(df_rdd, aes(x = run_var, y = elect_vio)) +
  stat_summary_bin(fun = sum, bins = 20, geom = "point") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "RDD Plot of Electoral Violence around Quota Implementation",
       x = "Years Since Quota Implementation",
       y = "Sum of Electoral Violence Events (ACLED)") + 
  theme_minimal()

## -----------------------------------------------------------------------------
## Supplementary Analysis - Women's Civil Liberty Index 
## -----------------------------------------------------------------------------
## -------------------------------------
## Logit Model - ECAV
## -------------------------------------

ecav_civil <- felm(elect_vio ~ quota_bi + civil_lib + quota_bi*civil_lib + log(gdp_pc) + log(wdi_pop) + lag_ev | country + year | 0 | country, 
                   data = df_ecav, keepCX = TRUE)
summary(ecav_civil)

## -------------------------------------
## coefficient plots
## -------------------------------------

## Extract coefficients and confidence intervals
coef_df <- tidy(ecav_civil, conf.int = TRUE) %>%
  mutate(term = case_when(
    term == "lag_ev" ~ "EV (t-1)",
    term == "log(gdp_pc)" ~ "log(GDP)",
    term == "log(wdi_pop)" ~ "log(Population)",
    term == "quota_bi" ~ "Quota",
    term == "quota_bi:civil_lib" ~ "Quota*Civil Liberties",
    term == "civil_lib" ~ "Civil Liberties"))

## Create the coefficient plot
png(file.path(fig_path, "results_ecav_civ.png"), res=100)
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept=0, size = 0.8, linetype=3, color="red") +
  labs(subtitle = "Effect of Gender Quotas on\nElectoral Violence (ECAV)",
       x = "",
       y = "Estimate") +
  coord_flip() +
  theme_minimal()
dev.off()

## -------------------------------------
## marginal effects plot 
## -------------------------------------
beta.hat <- coef(ecav_civil)
vcov1 <- vcov(ecav_civil)
z0 <- seq(min(df_ecav$civil_lib, na.rm=T), max(df_ecav$civil_lib, na.rm=T), length.out = 1000)
dy.dx <- beta.hat["quota_bi"] + beta.hat["quota_bi:civil_lib"]*z0
se.dy.dx <- sqrt(vcov1["quota_bi", "quota_bi"] + (z0^2)*vcov1["quota_bi:civil_lib", "quota_bi:civil_lib"] + 2*z0*vcov1["quota_bi", "quota_bi:civil_lib"])
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

png(file.path(fig_path, "ME_ecav_civ.png"), res=100)
ggplot(data=NULL, aes(x=z0, y=dy.dx)) +
  geom_line(aes(z0, dy.dx),size = 1) +
  geom_line(aes(z0, lwr), size = 1, linetype = 2, color="maroon") +
  geom_line(aes(z0, upr), size = 1, linetype = 2, color="maroon") +
  geom_hline(yintercept=0, size = 1, linetype=3) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3, color="lightgray")+
  labs(x="Women's Civil Liberties", y="Marginal Effects",
       title=paste("Marginal Effects of Gender Quotas on\nElectoral Violence"), 
       subtitle="Varied with Women's Civil Liberties (ECAV)",
       cex=4)+
  theme_minimal()
dev.off()

################################################################################
################################################################################
################################################################################

## -------------------------------------
## Logit Model - ACLED
## -------------------------------------

acled_civil <- felm(elect_vio  ~ quota_bi + civil_lib + quota_bi*civil_lib + log(gdp_pc) + log(wdi_pop) + 
                      lag_ev | country + year | 0 | country, data = df_acled, keepCX = TRUE)
summary(acled_civil)

## -------------------------------------
## coefficient plot 
## -------------------------------------

## Extract coefficients and confidence intervals
coef_df <- tidy(acled_civil, conf.int = TRUE) %>%
  mutate(term = case_when(
    term == "lag_ev" ~ "EV (t-1)",
    term == "log(gdp_pc)" ~ "log(GDP)",
    term == "log(wdi_pop)" ~ "log(Population)",
    term == "quota_bi" ~ "Quota",
    term == "quota_bi:civil_lib" ~ "Quota*Civil Liberties",
    term == "civil_lib" ~ "Civil Liberties"))

## Create the coefficient plot
png(file.path(fig_path, "results_acled_civ.png"), res=100)
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept=0, size = 0.8, linetype=3, color="red") +
  labs(subtitle = "Effect of Gender Quotas on\nElectoral Violence (ACLED)",
       x = "",
       y = "Estimate") +
  coord_flip() +
  theme_minimal()
dev.off()

## -------------------------------------
## marginal effects plot
## -------------------------------------
beta.hat <- coef(acled_civil)
vcov1 <- vcov(acled_civil)
z0 <- seq(min(df_acled$civil_lib, na.rm=T), max(df_acled$civil_lib, na.rm=T), length.out = 1000)
dy.dx <- beta.hat["quota_bi"] + beta.hat["quota_bi:civil_lib"]*z0
se.dy.dx <- sqrt(vcov1["quota_bi", "quota_bi"] + (z0^2)*vcov1["quota_bi:civil_lib", "quota_bi:civil_lib"] + 2*z0*vcov1["quota_bi", "quota_bi:civil_lib"])
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

png(file.path(fig_path, "ME_acled_civ.png"), res=100)
ggplot(data=NULL, aes(x=z0, y=dy.dx)) +
  geom_line(aes(z0, dy.dx),size = 1) +
  geom_line(aes(z0, lwr), size = 1, linetype = 2, color="maroon") +
  geom_line(aes(z0, upr), size = 1, linetype = 2, color="maroon") +
  geom_hline(yintercept=0, size = 1, linetype=3) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3, color="lightgray")+
  labs(x="Women's Civil Liberties", y="Marginal Effects",
       title=paste("Marginal Effects of Gender Quotas on\nElectoral Violence"), 
       subtitle="Varied with Women's Civil Liberties (ACLED)",
       cex=4) +
  theme_minimal()
dev.off()


## -----------------------------------------------------------------------------
## Supplementary Analysis - Kenya and/or Pakistan
## -----------------------------------------------------------------------------

