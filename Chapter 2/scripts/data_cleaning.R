
rm(list=ls())

# Load libraries
library(readxl)
library(network)
library(stringi)

# Set working directory
setwd("~/OneDrive - The Pennsylvania State University/Dissertation/(2) Networks/data")

# Load in ECAV dataset
ecav <- read_excel("ecav.xls")

# Create EV sum variable
ecav <- ecav %>%
  group_by(Actor1Name, Target1Name) %>% # only actor/targets
  mutate(ev_count = sum(EventViolence, na.rm=T)) # create EV sum variable

# Remove unknown actors and subset, and subset with attributes
ecav.s <- ecav %>%
  subset(Actor1Name != "unknown" & Actor1Name != "Unknown" & Target1Name != "Unknown") %>%
  select(Actor1Name, Target1Name, ev_count, Actor1Type, Target1Type) %>%
  distinct()

# Change actor & target names to title case
ecav.s$Actor1Name <- tools::toTitleCase(ecav.s$Actor1Name)
ecav.s$Target1Name <- tools::toTitleCase(ecav.s$Target1Name)

# Cleaning to top 50 most frequent actors
tab <- table(ecav.s$Actor1Name)
tab_s <- sort(tab)
top50.act <- tail(names(tab_s), 50)

# Cleaning to top 50 most frequent targets
tab <- table(ecav.s$Target1Name)
tab_s <- sort(tab)
top50.tar <- tail(tools::toTitleCase(names(tab_s)), 50)

# Subset df for these actors/targets
ecav.s2 <- subset(ecav.s, Actor1Name %in% top50.act & Target1Name %in% top50.tar) %>%
  distinct()

ecav.s <- ecav.s %>% # Cleaning duplicate labels
  mutate(Actor1Name = case_match(Actor1Name,
                                 "Israeli government" ~ "Government",
                                 "opposition supporters" ~ "Opposition supporters",
                                 "protestors" ~ "Protestors",
                                 "Protesters" ~ "Protestors",
                                 "students" ~ "Students",
                                 "Us Troops" ~ "US Troops",
                                 .default = as.character(Actor1Name)
  ),
  Target1Name = case_when(Target1Name,
                          "Ukrainian government" ~ "Government",
                          "UN election workers" ~ "UN Workers",
                          "Election Employees" ~ "Election Workers", 
                          "U.N. Workers" ~ "UN Workers",
                          "Afghan Aid Workers" ~ "Aid Workers",
                          "Contracted Afghan Guards" ~ "Afghan Guards",
                          "Afghan Intelligence Officers" ~ "Afghan Intelligence Agents", 
                          "Opposition Deputy Leader" ~ "Opposition Leader",
                          "Opposition Party Member" ~ "Opposition Members",
                          "Former President Ershad" ~ "President Ershad",
                          "Opposition Acitivists Students" ~ "Opposition Acitivists",
                          "Civilian" ~ "Citizens", 
                          "Citizen" ~ "Citizens",
                          "Opposition Leaders" ~ "Opposition Leader",
                          "Leader Opposition" ~ "Opposition Leader", 
                          "Opposition Party Leader Home" ~ "Opposition Leader",
                          "Opposition Party Senior Officials" ~ "Opposition Members",
                          "Opposition Party Spokesman" ~ "Opposition Members", 
                          "Opposition Candidate Campaign Leader" ~ "Opposition Campaign Leader",
                          "Moslem Militants" ~ "Muslim Militants",
                          "Moslem Brotherhood Members" ~ "Muslim Brotherhood Members",
                          "Moslem Brotherhood" ~ "Muslim Brotherhood",
                          "Moslem Brotherhood Supporters" ~ "Muslim Brotherhood Supporters",
                          "Moslem Brotherhood Candidate" ~ "Muslim Brotherhood Candidate", 
                          "Supporters of Brotherhood Candidiate" ~ "Muslim Brotherhood Supporters",
                          "Campaign Workers Moslem Brotherhood" ~ "Muslim Brotherhood Workers",
                          "Moslem Brotherhood Workers" ~ "Muslim Brotherhood Workers",
                          "Brotherhood Members" ~ "Muslim Brotherhood Members",
                          "Supporters of Moslim Brotherhood" ~ "Muslim Brotherhood Supporters",
                          "Moslim Brotherhood Supporters" ~ "Muslim Brotherhood Supporters",
                          "Moslim Brotherhood Deputies" ~ "Muslim Brotherhood Workers",
                          "Muslim Brotherhood Campaigner" ~ "Muslim Brotherhood Workers",
                          "Supporters of an Independent Candidate" ~ "Supporters of Independent Candidates",
                          "Supporters of Indipendent Candidate Omar Amer" ~ "Supporters of Independent Candidates",
                          "Supporters of Independent Candidate Umar Mutawalli" ~ "Supporters of Independent Candidates",
                          "Supporters of Candidate Omar Amer" ~ "Supporters of Independent Candidates",
                          "Muslim Brotherhood Member Omar Darrag" ~ "Muslim Brotherhood Members",
                          "Muslsim Brotherhood Members" ~ "Muslim Brotherhood Members",
                          "Muslim Brotherhood Memebrs" ~ "Muslim Brotherhood Members", 
                          "Members Opposition Groups" ~ "Opposition Members", 
                          "Opposition Candidate Ayman Nur" ~ "Opposition Candidate",
                          "Candidate Muslim Brotherhood" ~ "Muslim Brotherhood Candidate",
                          "Supporters of Islamist Canidate Abdel Hamed el_Senoussia" ~ "Supporters of Islamist Canidate",
                          "Supporters of Independent Canidate Mohammed Khader" ~ "Supporters of Independent Canidates",
                          "Supporters of Nasserite Candidate Hamdeen Sabahi" ~ "Supporters of Nasserite Candidate",
                          "Independent Candidate Faisal Ibrahim Hassanein" ~ "Independent Candidate",
                          "Supporters of Nasserite Candidate Hamdeen Sabahi" ~ "Supporters of Nasserite Candidate",
                          "Muslim Brotherhood Candidate Saber Zaher" ~ "Muslim Brotherhood Candidate",
                          "Muslim Brotherhood Followers" ~ "Muslim Brotherhood Supporters",
                          "Wafd Party Candidate Mohammed Mohammed Wandil" ~ "Wafd Party Candidate",
                          "Al-Ghad Party Member Ayman Nour" ~ "Al-Ghad Party Member",
                          "Muslim Brotherhood Member" ~ "Muslim Brotherhood Members",
                          "Muslim Brotherhood Representatives" ~ "Muslim Brotherhood Members",
                          .default = as.character(Target1Name)
  ))

unique(ecav.s$Actor1Name)

write.csv(ecav.s, "ecav_net.csv")



