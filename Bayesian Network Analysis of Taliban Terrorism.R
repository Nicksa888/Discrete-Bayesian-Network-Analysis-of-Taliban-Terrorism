##########################
##########################
#### Project Contents ####
##########################
##########################

# Discrete Bayesian Network Analysis of Taliban Terrorism #

# This project will provide two separate analyses. The first will explore a network comprising binary variables, while the second will examine the network based on categorical variables with several levels.

#########################
#########################
#### Clear Workspace ####
#########################
#########################

rm(list = ls()) 
# clear global environment to remove all loaded data sets, functions and so on.

###################
###################
#### Libraries ####
###################
###################

library(easypackages) # enables the libraries function for easier library loading
suppressPackageStartupMessages(
  libraries("bnlearn", # for Bayesian Network functionality
            "dplyr",
            "data.table", # for set.names function
            "tidyverse" # for fct_lump function
  ))

###############################
###############################
#### Set Working Directory ####
###############################
###############################

setwd("C:/Final Data Sets/Trinity College PhD 2021/GTD Data")

#####################
#####################
#### Obtain Data ####
#####################
#####################

GTD <- read.csv("globalterrorismdb_0919dist.csv")

########################
########################
#### Filtering Data ####
########################
########################

################################
# Filter Doubt Terrorism == No #
################################

# This removes all the non - terrorism violence as such incidents are coded yes
# Yes means it is doubted such violence is terrorism

GTDDT <- GTD %>% dplyr::filter(doubtterr == 0)
str(GTDDT)
dim(GTDDT)

###########################
# Filter Specificity == 1 #
###########################

# This removes all rows where the geographic coordinates have not been verified
# This is important because province and city variables are used in the modeling, so it is necessary to know exactly where each attack occurred.

GTDDTS <- GTDDT %>% dplyr::filter(specificity == 1)
names(GTDDTS)
dim(GTDDTS)

##################
# Filter Taliban #
##################

Taliban <- GTDDTS %>% dplyr::filter(gname == "Taliban")

dim(Taliban)

##################
# Rename Columns #
##################

library(data.table) # for set.names function

setnames(Taliban, old = c("iyear", "imonth", "iday", "provstate", "city", "multiple", "success", "suicide", "attacktype1_txt", "gname", "targtype1_txt", "weaptype1_txt", "nkill", "nwound"), new = c("Year", "Month", "Day", "Province", "City", "Multiple", "Success", "Suicide", "Attack", "Group", "Target", "Weapon", "Dead", "Wounded"))

Taliban <- dplyr::select(Taliban, Year, Month, Province, City, Suicide, Multiple, Success, Attack, Target, Group, Weapon, Dead)

colSums(is.na(Taliban)) # count the NA's in each column

Taliban <- na.omit(Taliban) # remove NA's
colSums(is.na(Taliban))

setwd("C:/R Portfolio/Bayesian Network")
write.csv(Taliban, file = "Taliban.csv", row.names = F)
Taliban <- read.csv("Taliban.csv")
names(Taliban)

Taliban$Target <- as.factor(Taliban$Target)

###########################
# Taliban Variable Counts #
###########################

# Year #

t <- Taliban %>%
  count(Year, sort = T)
View(t)

# Month #

t <- Taliban %>%
  count(Month, sort = T)
View(t)

# City #

t <- Taliban %>%
  count(City, sort = T)
View(t)

# Province #

t <- Taliban %>%
  count(Province, sort = T)
View(t)

# Suicide #

t <- Taliban %>%
  count(Suicide, sort = T)
View(t)

# Multiple #

t <- Taliban %>%
  count(Multiple, sort = T)
View(t)

# Success #

t <- Taliban %>%
  count(Success, sort = T)
View(t)

# Attack #

t <- Taliban %>%
  count(Attack, sort = T)
View(t)

# Target #

t <- Taliban %>%
  count(Target, sort = T)
View(t)

# Weapon #

t <- Taliban %>%
  count(Weapon, sort = T)
View(t)

# Dead #

t <- Taliban %>%
  count(Dead, sort = T)
View(t)

##############################################################
# Automatically Collate together small count category levels #
##############################################################

# Target #

Taliban <- Taliban %>%
  mutate(Target = fct_lump(fct_infreq(Target), n = 5)) %>%
  group_by(Target)

levels(Taliban$Target)

##########################
# Recode Variable Levels #
##########################

# Attack #

Taliban <- Taliban %>% 
  mutate(Attack = recode(Attack, 
                    "Bombing/Explosion" = "Bomb", 
                    "Facility/Infrastructure Attack" = "Infrastructure",
                    "Hostage Taking (Kidnapping)" = "HostageKidnap",
                    "Hostage Taking (Barricade Incident)" = "HostageBarricade",
                    "Armed Assault" = "ArmedAssault",
                    "Unarmed Assault" = "UnarmedAssault",
                    "Unknown" = "UnknownAttack"))

levels(Taliban$Attack)

# Target #

Taliban <- Taliban %>% 
  mutate(Target = recode(Target, 
                         "Private Citizens & Property" = "Private", 
                         "Government (General)" = "GovernmentGeneral",
                         "Other" = "OtherTarget"))

levels(Taliban$Target)
names(Taliban)

###############################
# Provincial - Attack Network #
###############################

# Province and City variables are correlated with each, and so are separated
# Attack and Weapon are correlated with each other, and so are separated
# The analysis only concerns provincial and attack type variables. Weapon type and city variables could feature in a future analysis.

Taliban <- dplyr::select(Taliban, Province, Suicide, Multiple, Success, Attack, Target, Group, Dead)
names(Taliban)

####################
# One Hot Encoding #
####################

# All variables are converted into separate binary variables where each categorical level becomes such a binary variable. The purpose is to indicate Bayesian network association between specific variables, rather than attack and target variables which each comprise several levels.

############
# Province #
############

for(unique_value in unique(Taliban$Province)){
  
  Taliban[paste("Province", unique_value, sep = ".")] <- ifelse(Taliban$Province == unique_value, 1, 0)
}

##########
# Target #
##########

for(unique_value in unique(Taliban$Target)){
  
  Taliban[paste("Target", unique_value, sep = ".")] <- ifelse(Taliban$Target == unique_value, 1, 0)
}

##########
# Attack #
##########

for(unique_value in unique(Taliban$Attack)){
  
  Taliban[paste("Attack", unique_value, sep = ".")] <- ifelse(Taliban$Attack == unique_value, 1, 0)
}

########
# Dead #
########

for(unique_value in unique(Taliban$Dead)){
  
  Taliban[paste("Dead", unique_value, sep = ".")] <- ifelse(Taliban$Dead == unique_value, 1, 0)
}

# Save Data File #

setwd("C:/R Portfolio/Bayesian Network")
write.csv(Taliban, file = "Tbn.csv", row.names = F)
Tbn <- read.csv("Tbn.csv")
names(Tbn)

Tbn_Corrected <- read.csv("Tbn_Corrected.csv")
names(Tbn_Corrected)
dim(Tbn_Corrected)

################
# Data Filters #
################

# To simplify the network construction, and to avoid situations where the total of pairwise counts between any two totals were less than ten, the five provinces with most attacks were selected, as was the five dead totals with the highest counts
# The five provinces are: Helmand, Kandahar, Kabul, Ghazni and Kunduz
# The five dead counts are zero, one, two, three and four
# The five targets with the highest attack counts are police, private, GovernmentGeneral, Military and Business
# The four attack types with the highest counts are Bomb, Armed Assault, Assassination and Hostage Kidnap. Unknown attack has a higher count than hostage kidnap, but it is overlooked as any attack could be an unknown attack.

Taliban4 <- dplyr::select(Tbn_Corrected, Helmand, Kandahar, Kabul, Ghazni, Kunduz, Police, Private, GovernmentGeneral, Military, Business, Bomb, ArmedAssault, Assassination, HostageKidnap, ZeroDead, OneDead, TwoDead, ThreeDead, FourDead)
names(Taliban4)
str(Taliban4)
dim(Taliban4)

####################
####################
# Network Creation #
####################
####################

dag2 <- empty.graph(nodes = c("Helmand", "Kandahar", "Kabul", "Ghazni", "Kunduz", "Police", "Private", "GovernmentGeneral", "Military", "Business", "Bomb", "ArmedAssault", "Assassination",  "HostageKidnap", "ZeroDead", "OneDead", "TwoDead", "ThreeDead", "FourDead"
))

arc.set = matrix(c("Bomb", "Business", 
                   "Bomb", "GovernmentGeneral", 
                   "Bomb", "Military",
                   "Bomb", "Police",
                   "Bomb", "Private",
                   "Bomb", "Helmand",
                   "Bomb", "Kandahar",
                   "Bomb", "Kabul",
                   "Bomb", "Ghazni",
                   "Bomb", "Kunduz",
                   "Bomb", "ZeroDead",
                   "Bomb", "OneDead",
                   "Bomb", "TwoDead",
                   "Bomb", "ThreeDead",
                   "Bomb", "FourDead",
                   "ArmedAssault", "Business", 
                   "ArmedAssault", "GovernmentGeneral", 
                   "ArmedAssault", "Military",
                   "ArmedAssault", "Police",
                   "ArmedAssault", "Private",
                   "ArmedAssault", "Helmand",
                   "ArmedAssault", "Kandahar",
                   "ArmedAssault", "Kabul",
                   "ArmedAssault", "Ghazni",
                   "ArmedAssault", "Kunduz",
                   "ArmedAssault", "ZeroDead",
                   "ArmedAssault", "OneDead",
                   "ArmedAssault", "TwoDead",
                   "ArmedAssault", "ThreeDead",
                   "ArmedAssault", "FourDead",
                   "Assassination", "Business", 
                   "Assassination", "GovernmentGeneral", 
                   "Assassination", "Military",
                   "Assassination", "Police",
                   "Assassination", "Private",
                   "Assassination", "Helmand",
                   "Assassination", "Kandahar",
                   "Assassination", "Kabul",
                   "Assassination", "Ghazni",
                   "Assassination", "Kunduz",
                   "Assassination", "ZeroDead",
                   "Assassination", "OneDead",
                   "Assassination", "TwoDead",
                   "Assassination", "ThreeDead",
                   "Assassination", "FourDead",
                   "HostageKidnap", "Business", 
                   "HostageKidnap", "GovernmentGeneral", 
                   "HostageKidnap", "Military",
                   "HostageKidnap", "Police",
                   "HostageKidnap", "Private",
                   "HostageKidnap", "Helmand",
                   "HostageKidnap", "Kandahar",
                   "HostageKidnap", "Kabul",
                   "HostageKidnap", "Ghazni",
                   "HostageKidnap", "Kunduz",
                   "HostageKidnap", "ZeroDead",
                   "HostageKidnap", "OneDead",
                   "HostageKidnap", "TwoDead",
                   "HostageKidnap", "ThreeDead",
                   "HostageKidnap", "FourDead",
                   "Business", "Helmand",
                   "Business", "Kandahar",
                   "Business", "Kabul",
                   "Business", "Ghazni",
                   "Business", "Kunduz",
                   "Business", "ZeroDead",
                   "Business", "OneDead",
                   "Business", "TwoDead",
                   "Business", "ThreeDead",
                   "Business", "FourDead",
                   "GovernmentGeneral", "Helmand",
                   "GovernmentGeneral", "Kandahar",
                   "GovernmentGeneral", "Kabul",
                   "GovernmentGeneral", "Ghazni",
                   "GovernmentGeneral", "Kunduz",
                   "GovernmentGeneral", "ZeroDead",
                   "GovernmentGeneral", "OneDead",
                   "GovernmentGeneral", "TwoDead",
                   "GovernmentGeneral", "ThreeDead",
                   "GovernmentGeneral", "FourDead",
                   "Military", "Helmand",
                   "Military", "Kandahar",
                   "Military", "Kabul",
                   "Military", "Ghazni",
                   "Military", "Kunduz",
                   "Military", "ZeroDead",
                   "Military", "OneDead",
                   "Military", "TwoDead",
                   "Military", "ThreeDead",
                   "Military", "FourDead",
                   "Police", "Helmand",
                   "Police", "Kandahar",
                   "Police", "Kabul",
                   "Police", "Ghazni",
                   "Police", "Kunduz",
                   "Police", "ZeroDead",
                   "Police", "OneDead",
                   "Police", "TwoDead",
                   "Police", "ThreeDead",
                   "Police", "FourDead",
                   "Private", "Helmand",
                   "Private", "Kandahar",
                   "Private", "Kabul",
                   "Private", "Ghazni",
                   "Private", "Kunduz",
                   "Private", "ZeroDead",
                   "Private", "OneDead",
                   "Private", "TwoDead",
                   "Private", "ThreeDead",
                   "Private", "FourDead",
                   "Helmand", "ZeroDead",
                   "Helmand", "OneDead",
                   "Helmand", "TwoDead",
                   "Helmand", "ThreeDead",
                   "Helmand", "FourDead",
                   "Kandahar", "ZeroDead",
                   "Kandahar", "OneDead",
                   "Kandahar", "TwoDead",
                   "Kandahar", "ThreeDead",
                   "Kandahar", "FourDead",
                   "Kabul", "ZeroDead",
                   "Kabul", "OneDead",
                   "Kabul", "TwoDead",
                   "Kabul", "ThreeDead",
                   "Kabul", "FourDead",
                   "Ghazni", "ZeroDead",
                   "Ghazni", "OneDead",
                   "Ghazni", "TwoDead",
                   "Ghazni", "ThreeDead",
                   "Ghazni", "FourDead",
                   "Kunduz", "ZeroDead",
                   "Kunduz", "OneDead",
                   "Kunduz", "TwoDead",
                   "Kunduz", "ThreeDead",
                   "Kunduz", "FourDead"
                   ),
                              ncol = 2, byrow = TRUE,
                             dimnames = list(NULL, c("from", "to")))

arcs(dag2) = arc.set

graphviz.plot(dag2, 
              layout = "dot", 
              shape = "ellipse")

##################################
# Convert variables into factors #
##################################

names <- names(Taliban4)
Taliban4[names] <- lapply(Taliban4[names], factor)
str(Taliban4)

###########################
# Province Node Blacklist #
###########################

# This blacklist informs R to avoid creating any arcs between these arcs. For instance, there is no point creating arcs between two provincial variables or two attack type variables.

BA_BL <- matrix(c(
  "Helmand", "Kandahar",
  "Kandahar", "Helmand",
  "Helmand", "Kabul",
  "Kabul", "Helmand",
  "Helmand", "Ghazni",
  "Ghazni", "Helmand",
  "Helmand", "Kunduz",
  "Kunduz", "Helmand",
  "Kandahar", "Kabul",
  "Kabul", "Kandahar",
  "Kandahar", "Ghazni",
  "Ghazni", "Kandahar",
  "Kandahar", "Kunduz",
  "Kunduz", "Kandahar",
  "Kabul", "Ghazni",
  "Ghazni", "Kabul",
  "Kabul", "Kunduz",
  "Kunduz", "Kabul",
  "Ghazni", "Kunduz",
  "Kunduz", "Ghazni",
  "Business", "GovernmentGeneral",
  "GovernmentGeneral", "Business",
  "Business", "Military",
  "Military", "Business",
  "Business", "Police",
  "Police", "Business",
  "Business", "Private",
  "Private", "Business",
  "GovernmentGeneral", "Military",
  "Military", "GovernmentGeneral",
  "GovernmentGeneral", "Police",
  "Police", "GovernmentGeneral",
  "GovernmentGeneral", "Private",
  "Private", "GovernmentGeneral",
  "Military", "Police",
  "Police", "Military",
  "Private", "Military",
  "Military", "Private",
  "Private", "Police",
  "Police", "Private",
  "Bomb", "ArmedAssault",
  "ArmedAssault", "Bomb",
  "Bomb", "Assassination",
  "Assassination", "Bomb",
  "Bomb", "HostageKidnap",
  "HostageKidnap", "Bomb",
  "ArmedAssault", "Assassination",
  "Assassination", "ArmedAssault",
  "ArmedAssault", "HostageKidnap",
  "HostageKidnap", "ArmedAssault",
  "Assassination", "HostageKidnap",
  "HostageKidnap", "Assassination",
  "ZeroDead", "OneDead",
  "OneDead", "ZeroDead",
  "ZeroDead", "TwoDead",
  "TwoDead", "ZeroDead",
  "ZeroDead", "ThreeDead",
  "ThreeDead", "ZeroDead",
  "ZeroDead", "FourDead",
  "FourDead", "ZeroDead",
  "OneDead", "TwoDead",
  "TwoDead", "OneDead",
  "OneDead", "ThreeDead",
  "ThreeDead", "OneDead",
  "OneDead", "FourDead",
  "FourDead", "OneDead",
  "TwoDead", "ThreeDead",
  "ThreeDead", "TwoDead",
  "TwoDead", "FourDead",
  "FourDead", "TwoDead",
  "ThreeDead", "FourDead",
  "FourDead", "ThreeDead"
),
ncol = 2, 
byrow = TRUE,
dimnames = list(NULL, c("from", "to")))

###############################
###############################
# Create Score Based Networks #
###############################
###############################

# R features two score based networks, Tabu and Hill Climbing (HC)

########
# Tabu #
########

?tabu
set.seed(226)
Tabu_BA <- tabu(Taliban4,
                blacklist = BA_BL)
Tabu_BA

graphviz.plot(Tabu_BA, 
              layout = "dot", 
              shape = "ellipse")

?graphviz.plot

#################
# Hill Climbing #
#################

set.seed(227)
HC_BA <- hc(Taliban4, 
            blacklist = BA_BL)

HC_BA

graphviz.plot(HC_BA, 
              layout = "dot", 
              shape = "ellipse")

##################################
# Network Goodness of Fit Scores #
##################################

# HC #
######

score(HC_BA, Taliban4, type = "bde")
score(HC_BA, Taliban4, type = "aic")
score(HC_BA, Taliban4, type = "bic")
score(HC_BA, Taliban4, type = "loglik")

# Tabu #
########

score(Tabu_BA, Taliban4, type = "bde")
score(Tabu_BA, Taliban4, type = "aic")
score(Tabu_BA, Taliban4, type = "bic")
score(Tabu_BA, Taliban4, type = "loglik")

# For bde, aic, bic and loglik scores, Tabu is lower (closer to zero), so is a better model
 
?score

#######################################
#######################################
# Using the Discrete Bayesian Network #
#######################################
#######################################

########################################
# Using the Bayesian Network Structure #
########################################

# To determine if there is any conditional dependence, the dsep funcion ca be used. Where two variables are connected to each other through a third, then of the dsep is false, this indicates that there is conditional dependence if the path through the third variable is not blocked. The path only becomes blocked if the two variables condition on the third

dsep(Tabu_BA, x = "GovernmentGeneral", y = "Business")
# [1] FALSE
dsep(Tabu_BA, x = "GovernmentGeneral", y = "OneDead")
# [1] FALSE

# The false output indicates the variables are not deseparated and so are conditionally dependant on each other

path(Tabu_BA, from = "GovernmentGeneral", to = "Business")
dsep(Tabu_BA, x = "GovernmentGeneral", y = "Business", z = "Assassination")
# When assassination is conditioned on, the path is blocked and they become desparated.


################################
################################
# Probabilistic Representation #
################################
################################

# Because Tabu is a better fitting model according to all scoring metrics, it will be used rather than HC to represent the probabilities

###########################################################
# Estimating the parameters: Conditional Probability Tables
###########################################################

# Maximum Likelihood Estimates

bn.mle <- bn.fit(Tabu_BA, Taliban4, method = "mle")

# Bayesian Estimation #

bn.bayes <- bn.fit(Tabu_BA, Taliban4, method = "bayes", iss = 10)

?bn.fit

##################################
# Conditional Probability Tables #
##################################

# Maximum Likelihood Estimates #

bn.mle$Bomb 
# Conditional probability table:
# ArmedAssault Assassination          Bomb HostageKidnap   OtherAttack 
# 0.22993062    0.08787578    0.45887017    0.08259002    0.14073340 

bn.mle$Target

bn.mle$Province

bn.mle$Dead

# Bayesian Setting #

bn.bayes$Attack 
# Conditional probability table:
# ArmedAssault Assassination          Bomb HostageKidnap   OtherAttack 
# 0.22992074    0.08791281    0.45878468    0.08262880    0.14075297  

bn.bayes$Target

bn.bayes$Province

bn.bayes$Dead

###################
# Exact Inference #
###################

# To install RGBL - needed for gRain:
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("RBGL")

library(gRain)
library(gRbase)

junction <- compile(as.grain(bn.bayes))

# The probability of bomb attack considering data as a whole

querygrain(junction, nodes = "Bomb")$Bomb

# The probability of actual Bomb attack (indicated by 1) in Helmand

group <- setEvidence(junction, nodes = "Bomb", states = "1")
querygrain(group, nodes = "Helmand")$Helmand

# The probability of actual Bomb attack (indicated by 1) against Government General Target

group <- setEvidence(junction, nodes = "Bomb", states = "1")
querygrain(group, nodes = "GovernmentGeneral")$GovernmentGeneral

# The probability of actual Bomb attack with one dead (indicated by 1) against Government General Target for Bomb attack and in Helmand
# The probability of attack against Governmental General Target per province and per attack

BT <- setEvidence(junction, nodes = "OneDead", states = "1")
GPA <- querygrain(BT, nodes = c("Bomb", "Helmand"),
                  type = "joint")

GPA

SxT <- cpdist(bn.bayes, nodes = c("Bomb", "Helmand"),
               evidence = (OneDead == "1"))


#########################
# Approximate inference #
#########################

# Approximate probabilistic inference of attack against Police Target in Helmand Province based on maximum likelihood estimate

################################
################################
# Conditional Independence Tests
################################
################################

# Conditional independence tests focus on the presence of individual arcs. As
# each arc indicates a probabilistic dependence, conditional independence tests
# assess whether that probabilistic dependence is supported by
# the data. If the null hypothesis (of conditional independence) is rejected, the arc can be considered for inclusion in the DAG.

ci.test("Kabul", "ZeroDead", c("ArmedAssault", "Police"), test = "mi", data = Taliban4)

ci.test("Kabul", "ZeroDead", c("ArmedAssault", "Police"), test = "x2", data = Taliban4)

# Both tests generate insignificant p-values, thus indicating that dependent relationship encoded by kabul and ZeroDead is not significant given its parents.

#################
# Arc Strengths #
#################

arc.strength(Tabu_BA, Taliban4, criterion = "x2")

# All arcs have very significant p-values, which means they are well supported by the data

dag4 <- set.arc(dag2, from = "ZeroDead", to = "Kabul")
nparams(dag4, Taliban4)

score(dag4, data = Taliban4, type = "bic")
score(dag2, data = Taliban4, type = "bic")


# The bic score for the Bayesian Network that features the new arc, has a lower score than the network without it, which suggests adding this arc is actually beneficial to the network

score(Tabu_BA, data = Taliban4, type = "bic")

# The bic score for the learned network via Tabu algorithm, has a much lower score.

arc.strength(Tabu_BA, Taliban4, criterion = "bic")

# The results indicate that removal of any arc learned by Tabu would worsen the bic score, thus reducing the goodness of fit to the data. Therefore, the network learned by the Tabu algorithm is a good fit to the data

arc.strength(dag2, data = Taliban4, criterion = "bic")

# All the arcs apart from Assassination GovernmentGeneral would worsen the bic score, thus indicating it is not a good fit to the data.


########################
########################
# 2nd Network Analysis #
########################
########################

# This analysis features combined categorical levels per variable

################################
################################
# Probabilistic Representation #
################################
################################

# Here, it is necessary to use categorical variables with several levels

Taliban <- dplyr::select(Taliban, Province, Attack, Target, Dead)
names(Taliban)

##################################
# Convert variables into factors #
##################################

Taliban2 <- Taliban

names <- names(Taliban2)
Taliban2[names] <- lapply(Taliban2[names], factor)
str(Taliban2)

write.csv(Taliban2, file = "Taliban2.csv", row.names = F)
Taliban2 <- read.csv("Taliban2.csv")

##################################
# Convert variables into factors #
##################################

names <- names(Taliban2)
Taliban2[names] <- lapply(Taliban2[names], factor)
str(Taliban2)
glimpse(Taliban2)

##############################################################
# Automatically Collate together small count category levels #
##############################################################

levels(Taliban2$Target)

t <- Taliban2 %>%
     count(Target, sort = T)
View(t)

# Target #

Taliban2 <- Taliban2 %>%
  mutate(Target = fct_lump(fct_infreq(Target), n = 5)) %>%
  group_by(Target)

levels(Taliban2$Target)

# Recode Target Levels #

Taliban2 <- Taliban2 %>% 
  mutate(Target = recode(Target, 
                         "Private Citizens & Property" = "Private", 
                         "Government (General)" = "GovernmentGeneral",
                         "Business" = "OtherTarget",
                         "Other" = "OtherTarget"))

levels(Taliban2$Target)

# Attack #

Taliban2 <- Taliban2 %>%
  mutate(Attack = fct_lump(fct_infreq(Attack), n = 5)) %>%
  group_by(Attack)

levels(Taliban2$Attack)

# Recode Attack Levels #

Taliban2 <- Taliban2 %>% 
  mutate(Attack = recode(Attack, 
                         "Bombing/Explosion" = "Bomb", 
                         "Armed Assault" = "ArmedAssault",
                         "Hostage Taking (Kidnapping)" = "HostageKidnap",
                         "Unknown" = "OtherAttack",
                         "Facility/Infrastructure Attack" = "OtherAttack",
                         "Hostage Taking (Barricade Incident)" = "OtherAttack",
                         "Other" = "OtherAttack"))

levels(Taliban2$Attack)

# Province #

Taliban2 <- Taliban2 %>%
  mutate(Province = fct_lump_n(Province, 
                               n = 5, 
                               other_level = "OtherProvince")) %>%
                               group_by(Province) 

###########################
# Taliban Province Counts #
###########################

# Province #

t <- Taliban2 %>%
  count(Province, sort = T)
View(t)

# Recode Province Levels #

# These are the additional provinces that remain after lumping all provinces with small counts

Taliban2 <- Taliban2 %>% 
  mutate(Province = recode(Province, 
                           "Kunduz" = "OtherProvince", 
                           "Farah" = "OtherProvince",
                           "Faryab" = "OtherProvince",
                           "Logar" = "OtherProvince",
                           "Nangarhar" = "OtherProvince",
                           "Herat" = "OtherProvince",
                           "Khost" = "OtherProvince"))
levels(Taliban2$Province)

###########################
# Taliban Province Counts #
###########################

# Province #

t <- Taliban2 %>%
  count(Province, sort = T)
View(t)

###########################################################
# Recode small count category levels in the Dead Variable #
###########################################################

Taliban2$Dead <- recode(Taliban2$Dead, 
                        '0' = "ZeroDead", 
                        '1' = "OneDead", 
                        '2' = "TwoDead", 
                        '3' = "ThreeDead", 
                        .default = "OtherDead")
levels(Taliban2$Dead)

str(Taliban2)
glimpse(Taliban2)

write.csv(Taliban2, file = "Taliban2.csv", row.names = F)
Taliban3 <- read.csv("Taliban2.csv")

names <- names(Taliban3)
Taliban3[names] <- lapply(Taliban3[names], factor)
str(Taliban3)

str(Taliban3)
glimpse(Taliban2)

# Identify variable levels #

levels(Taliban3$Province)
levels(Taliban3$Attack)
levels(Taliban3$Target)
levels(Taliban3$Dead)

##################
# Create Network #
##################

dag <- empty.graph(nodes = c("Province", 
                             "Attack", 
                             "Target", 
                             "Dead"))

arc.set = matrix(c("Attack", to = "Province",
                   "Target", to = "Province",
                   "Dead", to = "Province",
                   "Attack", to = "Target",
                   "Attack", to = "Dead",
                   "Dead", to = "Target"),
ncol = 2, byrow = TRUE,
dimnames = list(NULL, c("from", "to")))

arcs(dag) = arc.set

###########################################################
# Estimating the parameters: Conditional Probability Tables
###########################################################

# Maximum Likelihood Estimates

bn.mle <- bn.fit(dag, Taliban3, method = "mle")

# Bayesian Estimation #

bn.bayes <- bn.fit(dag, Taliban3, method = "bayes", iss = 10)

?bn.fit
arc.strength(dag, Taliban3, criterion = "bic")

##################################
# Conditional Probability Tables #
##################################

# Maximum Likelihood Estimates #

# Probability of Attack #

bn.mle$Attack 
# Conditional probability table:
# ArmedAssault Assassination          Bomb HostageKidnap   OtherAttack 
# 0.22993062    0.08787578    0.45887017    0.08259002    0.14073340 

bn.fit.barchart(bn.mle$Attack, main = "Attack",
                xlab = "Pr(Attack)", ylab = "")

bn.fit.dotplot(bn.mle$Attack, main = "Attack",
                xlab = "Pr(Attack)", ylab = "")

# Probability of Target conditional on Dead and Attack #

bn.mle$Target

bn.fit.barchart(bn.mle$Target, main = "Travel",
                 xlab = "Pr(Target | Dead, Attack)", ylab = "")

bn.fit.dotplot(bn.mle$Target, main = "Travel",
                xlab = "Pr(Target | Dead, Attack)", ylab = "")

# Probability of Province conditional on Dead, Target and Attack #

bn.mle$Province

bn.fit.barchart(bn.mle$Province, main = "Province",
                xlab = "Pr(Province | Dead, Target, Attack)", ylab = "")

# Probability of Dead conditional on Attack #

bn.mle$Dead

bn.fit.barchart(bn.mle$Dead, main = "Dead",
                xlab = "Pr(Dead | Attack)", ylab = "")

# Bayesian Setting #

bn.bayes$Attack 
# Conditional probability table:
# ArmedAssault Assassination          Bomb HostageKidnap   OtherAttack 
# 0.22992074    0.08791281    0.45878468    0.08262880    0.14075297  

bn.bayes$Target

bn.bayes$Province

bn.bayes$Dead

# Exact Inference

# To install RGBL - needed for gRain:
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("RBGL")

library(gRain)
library(gRbase)

junction <- compile(as.grain(bn.bayes))

# The probability of attack in each Province considering data as a whole

querygrain(junction, nodes = "Province")$Province

# The probability of attack per Province with Bomb Attack

group <- setEvidence(junction, nodes = "Attack", states = "Bomb")
querygrain(group, nodes = "Province")$Province

# The probability of attack per Target with Bomb Attack

group <- setEvidence(junction, nodes = "Attack", states = "Bomb")
querygrain(group, nodes = "Target")$Target

# The probability of attack against Governmental General Target per province and per attack

BT <- setEvidence(junction, nodes = "Target", states = "GovernmentGeneral")
GPA <- querygrain(BT, nodes = c("Province", "Attack"),
           type = "joint")

GPA

#########################
# Approximate inference #
#########################

# Approximate probabilistic inference of attack against Police Target in Kabul Province based on maximum likelihood estimate

cpquery(bn.mle, event = (Target == "Police"),
        evidence = (Province == "Kabul"))

cpquery(bn.bayes, event = (Target == "Police"),
        evidence = (Helmand == "1"))

SxT <- cpdist(bn.bayes, nodes = c("Attack", "Province"),
               evidence = (Target == "Police"))
head(SxT)

#################
# Arc Strengths #
#################

arc.strength(HC_BA, Taliban4, criterion = "bic")
