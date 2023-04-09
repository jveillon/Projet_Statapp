#In this script, we will proceed to an ACP-I analysis

library(readr)
library(dplyr)
library(tidyr)
library(APCI)
library(tidyverse)

data <- read_csv("data_small_data_APC.csv")


#Delete the one individual who have NA values for the age variables

data <- subset(data, data$HHIDPN != 32570030)


#We first start with a data formatting to obtain a data set in which one row
#correspond to a interview (for example : the individual 1010 interviewed during
# the 2nd wave)
data_APC <-  data.frame(matrix(nrow = 0, ncol = 4))
names(data_APC) <- c("HHIDPN","WAVE","AGE","HEALTH_INDEX")

for (i in (1:14)){
  columns <- c("HHIDPN",paste("INW",as.character(i),sep=""),
               paste("R",as.character(i),"AGEY_B",sep=""),
               paste("GHI",as.character(i),sep =""))
  data_temp <- select(data,columns)
  names(data_temp) <- c("HHIDPN","WAVE","AGE","HEALTH_INDEX")
  data_temp <- data_temp[data_temp$WAVE == 1,]
  data_temp$WAVE[data_temp$WAVE == 1] <- i
  data_APC <- rbind(data_APC,data_temp)
}


#Now we create a categorial variable for the age

data_APC["AGE_CAT"] <- NA
data_APC$AGE_CAT[data_APC$AGE < 30] <- 1
data_APC$AGE_CAT[data_APC$AGE < 40 & data_APC$AGE >= 30] <- 2
data_APC$AGE_CAT[data_APC$AGE < 50 & data_APC$AGE >= 40] <- 3
data_APC$AGE_CAT[data_APC$AGE < 60 & data_APC$AGE >= 50] <- 4
data_APC$AGE_CAT[data_APC$AGE < 70 & data_APC$AGE >= 60] <- 5
data_APC$AGE_CAT[data_APC$AGE < 80 & data_APC$AGE >= 70] <- 6
data_APC$AGE_CAT[data_APC$AGE < 90 & data_APC$AGE >= 80] <- 7
data_APC$AGE_CAT[data_APC$AGE >= 90] <- 8

data_APC["AGE_CAT_L"] <- NA
data_APC$AGE_CAT_L[data_APC$AGE_CAT_L == 1] <- "-29"
data_APC$AGE_CAT_L[data_APC$AGE_CAT_L == 2] <- "30-39"
data_APC$AGE_CAT_L[data_APC$AGE_CAT_L == 3] <- "40-49"
data_APC$AGE_CAT_L[data_APC$AGE_CAT_L == 4] <- "50-59"
data_APC$AGE_CAT_L[data_APC$AGE_CAT_L == 5] <- "60-69"
data_APC$AGE_CAT_L[data_APC$AGE_CAT_L == 6] <- "70-79"
data_APC$AGE_CAT_L[data_APC$AGE_CAT_L == 7] <- "80-89"
data_APC$AGE_CAT_L[data_APC$AGE_CAT_L == 8] <- "90-"

#Realization of the Age-Period-Cohort analysis 

APCI <- apci(outcome = "HEALTH_INDEX",age ="AGE_CAT", period = "WAVE",
             cohort = NULL, weight = NULL, covariate = NULL,
             data =data_APC, family ="gaussian",
             dev.test = FALSE, print = TRUE)

# Deviance test

deviance_globale <- data.frame(anova(glm(HEALTH_INDEX~AGE_CAT*WAVE, 
                                         family="gaussian", data=data_APC), test ="F"))
names(deviance_globale) <- c("DL", "Déviance","DL résid", "Déviance résid",
                             "Stat F","p-valeur") 
print(deviance_globale)

library("writexl")
write_xlsx(deviance_globale,"results_Deviance_test.xlsx")

# Age effect

age_effect <- data.frame(APCI$age_effect)
for (x in names(age_effect)[names(age_effect)!="sig"]){
  age_effect[x] <- as.numeric(unlist(age_effect[x]))
}
names(age_effect) <- c("Groupe","Effet_age","Erreur_type","p_valeur","Sig")

age_effect$Sig[age_effect$Sig =="   "] <- "ns"
age_effect$Sig[age_effect$Sig =="+"] <- "<0.1"
age_effect$Sig[age_effect$Sig =="*  "] <- "<0.05"
age_effect$Sig[age_effect$Sig =="** "] <- "<0.01"
age_effect$Sig[age_effect$Sig =="***"] <- "<0.001"

age_effect["Libelle"] <- c("-29", "30-39","40-49","50-59","60-69","70-79","80-89","90-")

print(age_effect)
write_xlsx(age_effect,"results_age_effect.xlsx")

# Period effect

period_effect <- data.frame(APCI$period_effect)
for (x in names(period_effect)[names(period_effect)!="sig"]){
  period_effect[x] <- as.numeric(unlist(period_effect[x]))
}
names(period_effect) <- c("Période","Effet_période","Erreur_type","p_valeur","Sig")

period_effect$Sig[period_effect$Sig =="   "] <- "ns"
period_effect$Sig[period_effect$Sig =="+"] <- "<0.1"
period_effect$Sig[period_effect$Sig =="*  "] <- "<0.05"
period_effect$Sig[period_effect$Sig =="** "] <- "<0.01"
period_effect$Sig[period_effect$Sig =="***"] <- "<0.001"

print(period_effect)
write_xlsx(period_effect,"results_period_effect.xlsx")

# Cohort effect

cohort_average <- data.frame(APCI$cohort_average)

for (x in names(cohort_average)[names(cohort_average) !="sig" &
                                names(cohort_average) !="cohort_group"]){
  cohort_average[x] <- as.numeric(unlist(cohort_average[x]))
}
names(cohort_average) <- c("Cohorte","Effet_moyen","Erreur_type","Statistique_T",
                           "p_valeur","Sig")
cohort_average$Sig[cohort_average$Sig =="   "] <- "ns"
cohort_average$Sig[cohort_average$Sig =="+"] <- "<0.1"
cohort_average$Sig[cohort_average$Sig =="*  "] <- "<0.05"
cohort_average$Sig[cohort_average$Sig =="** "] <- "<0.01"
cohort_average$Sig[cohort_average$Sig =="***"] <- "<0.001"

print(cohort_average)
write_xlsx(cohort_average,"results_cohort_effect.xlsx")
