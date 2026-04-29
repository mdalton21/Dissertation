## -----------------------------------------------------------------------------
##
## [ PROJ ] Gender Quotas and Electoral Violence
## [ FILE ] data_analysis.R
## [ AUTH ] Maya A. Dalton; mad6821@psu.edu
## [ INIT ] 28 November 2023
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("haven", "readxl", "dplyr", "tidyverse", "ggplot2", "stargazer",
          "vtable", "lfe", "sjPlot", "sjmisc", "broom", "rdd", "rdrobust")
sapply(libs, require, character.only = TRUE)

## paths
## paths (./scripts as working directory)
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
dat_dir <- file.path(root, "data")
fig_dir <- file.path(root, "figures")
fig_path <- "~/Dropbox/Apps/Overleaf/Dalton EV Quotas (CH. 3)/Figures"
tab_path <- "~/Dropbox/Apps/Overleaf/Dalton EV Quotas (CH. 3)/Tables"

## read in datasets for analysis
df_acled <- read_csv(file.path(dat_dir, "df_acled.csv"))
df_ecav <- read_csv(file.path(dat_dir, "df_ecav.csv"))

## -----------------------------------------------------------------------------
## Summary Tables
## -----------------------------------------------------------------------------
## -------------------------------------
## ECAV
## -------------------------------------
df_ecav2 <- df_ecav %>%
  select(country, year, quota_bi, elect_vio, wom_married15, gdp_pc)

var.labs <- data.frame(var = c('year', 'quota', 'elect_vio', 'wom_married15', 'gdp_pc'),
                       labels = c('Year', 'Gender Quota', 'Electoral Violence', 
                                  "% Married at 15", 'GDP'))

st(df_ecav2, labels=var.labs, summ=c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), 
   summ.names = c('N','Mean','Sd','Min','Max'), out='latex', title="Summary Statistics - ECAV",
   file=file.path(tab_path, "sumtable_ecav"))

## -------------------------------------
## ACLED
## -------------------------------------

df_acled2 <- df_acled %>%
  select(country, year, quota_bi, elect_vio, wom_married15, gdp_pc)

var.labs <- data.frame(var = c('year', 'quota', 'elect_vio', 'wom_married15', 'gdp_pc'),
                       labels = c('Year', 'Gender Quota', 'Electoral Violence', 
                                  "% Married at 15", 'GDP'))

st(df_acled2, labels=var.labs, summ=c('notNA(x)','mean(x)','sd(x)','min(x)','max(x)'), 
   summ.names = c('N','Mean','Sd','Min','Max'), out='latex', title="Summary Statistics - ACLED",
   file=file.path(tab_path, "sumtable_acled"))

## -----------------------------------------------------------------------------
## Logit Model: ECAV
## -----------------------------------------------------------------------------

ecav_married <- felm(elect_vio ~ quota_bi + wom_married15 + quota_bi*wom_married15 + log(gdp_pc) + log(wdi_pop) + lag_ev | country + year | 0 | country, 
                data = df_ecav, keepCX = TRUE)
summary(ecav_married)

## -------------------------------------
## coefficient plots
## -------------------------------------

## Extract coefficients and confidence intervals
coef_df <- tidy(ecav_married, conf.int = TRUE) %>%
  filter(term == c("quota_bi", "wom_married15", "quota_bi:wom_married15")) %>%
  mutate(term = case_when(
    term == "quota_bi" ~ "Quota",
    term == "quota_bi:wom_married15" ~ "Quota*Married",
    term == "wom_married15" ~ "Married")) 

## Create the coefficient plot
png(file.path(fig_path, "results_ecav.png"), res=100)
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept=0, size = 0.8, linetype=3, color="red") +
  labs(#subtitle = "Effect of Gender Quotas on\nElectoral Violence (ECAV)",
       x = "",
       y = "Estimate") +
  coord_flip() +
  theme_minimal()
dev.off()

## -------------------------------------
## marginal effects plot 
## -------------------------------------
beta.hat <- coef(ecav_married)
vcov1 <- vcov(ecav_married)
z0 <- seq(min(df_ecav$wom_married15, na.rm=T), max(df_ecav$wom_married15, na.rm=T), length.out = 1000)
dy.dx <- beta.hat["quota_bi"] + beta.hat["quota_bi:wom_married15"]*z0
se.dy.dx <- sqrt(vcov1["quota_bi", "quota_bi"] + (z0^2)*vcov1["quota_bi:wom_married15", "quota_bi:wom_married15"] + 2*z0*vcov1["quota_bi", "quota_bi:wom_married15"])
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

png(file.path(fig_path, "ME_ecav.png"), res=100)
ggplot(data=NULL, aes(x=z0, y=dy.dx)) +
  geom_line(aes(z0, dy.dx),size = 1) +
  geom_line(aes(z0, lwr), size = 1, linetype = 2, color="maroon") +
  geom_line(aes(z0, upr), size = 1, linetype = 2, color="maroon") +
  geom_hline(yintercept=0, size = 1, linetype=3) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3, color="lightgray")+
  labs(x="% Women Married at 15", y="Marginal Effects",
       #title=paste("Marginal Effects of Gender Quotas on\nElectoral Violence"), 
       #subtitle="Varied with % Women Married at 15 (ECAV)",
       cex=4)+
  theme_minimal()
dev.off()

## -------------------------------------
## stargazer table
## -------------------------------------

stargazer(ecav_married, header = FALSE, font.size = "footnotesize", no.space=T,
          title="Effect of Gender Quotas on Electoral Violence (ECAV)", 
          dep.var.labels = "Electoral Violence (ECAV)", 
          model.names = FALSE, keep=c("quota", "wom_married15", "quota:wom_married15", "gdp_pc", 
                                      "wdi_pop", "elect_vio", "lag_ev"), 
          covariate.labels = c("Gender Quota", "\% Married at 15", "log(GDPPC)", "log(Population)",
                               "Electoral Violence (t-1)", "Quota*Married"),
          out=file.path(tab_path, "results_ecav.tex"))

## -----------------------------------------------------------------------------
## Logit Model: ACLED
## -----------------------------------------------------------------------------

acled_married <- felm(elect_vio  ~ quota_bi + wom_married15 + quota_bi*wom_married15 + log(gdp_pc) + log(wdi_pop) + 
                   lag_ev | country + year | 0 | country, data = df_acled, keepCX = TRUE)
summary(acled_married)

## -------------------------------------
## coefficient plot 
## -------------------------------------

## Extract coefficients and confidence intervals
coef_df <- tidy(acled_married, conf.int = TRUE) %>%
  filter(term == c("quota_bi", "wom_married15", "quota_bi:wom_married15")) %>%
  mutate(term = case_when(
    term == "quota_bi" ~ "Quota",
    term == "quota_bi:wom_married15" ~ "Quota*Married",
    term == "wom_married15" ~ "Married")) 

## Create the coefficient plot
png(file.path(fig_path, "results_acled.png"), res=100)
ggplot(coef_df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  geom_hline(yintercept=0, size = 0.8, linetype=3, color="red") +
  labs(#subtitle = "Effect of Gender Quotas on\nElectoral Violence (ACLED)",
       x = "",
       y = "Estimate") +
  coord_flip() +
  theme_minimal()
dev.off()

## -------------------------------------
## marginal effects plot 
## -------------------------------------
beta.hat <- coef(acled_married)
vcov1 <- vcov(acled_married)
z0 <- seq(min(df_acled$wom_married15, na.rm=T), max(df_acled$wom_married15, na.rm=T), length.out = 1000)
dy.dx <- beta.hat["quota_bi"] + beta.hat["quota_bi:wom_married15"]*z0
se.dy.dx <- sqrt(vcov1["quota_bi", "quota_bi"] + (z0^2)*vcov1["quota_bi:wom_married15", "quota_bi:wom_married15"] + 2*z0*vcov1["quota_bi", "quota_bi:wom_married15"])
upr <- dy.dx + 1.96*se.dy.dx
lwr <- dy.dx - 1.96*se.dy.dx

png(file.path(fig_path, "ME_acled.png"), res=100)
ggplot(data=NULL, aes(x=z0, y=dy.dx)) +
  geom_line(aes(z0, dy.dx),size = 1) +
  geom_line(aes(z0, lwr), size = 1, linetype = 2, color="maroon") +
  geom_line(aes(z0, upr), size = 1, linetype = 2, color="maroon") +
  geom_hline(yintercept=0, size = 1, linetype=3) +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.3, color="lightgray")+
  labs(x="% Women Married at 15", y="Marginal Effects",
       #title=paste("Marginal Effects of Gender Quotas on\nElectoral Violence"), 
       #subtitle="Varied with % Women Married at 15 (ACLED)",
       cex=4) +
  theme_minimal()
dev.off()


## -------------------------------------
## stargazer table - % married
## -------------------------------------

stargazer(mod_acled, header = FALSE, font.size = "footnotesize", no.space=T,
          title="Effect of Gender Quotas on Electoral Violence (ACLED)", 
          dep.var.labels = "Electoral Violence (ACLED)", 
          model.names = FALSE, keep=c("quota", "wom_married15", "quota:wom_married15", "gdp_pc", 
                                      "wdi_pop", "elect_vio", "lag_ev"), 
          covariate.labels = c("Gender Quota", "\% Married at 15", "log(GDPPC)", "log(Population)",
                               "Electoral Violence (t-1)", "Quota*Married"),
          out=file.path(tab_path, "results_acled.tex"))

## -----------------------------------------------------------------------------
## end script
## -----------------------------------------------------------------------------
