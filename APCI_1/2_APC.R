#In this script, we will proceed to an ACP-I analysis

library(readr)
library(dplyr)
library(tidyr)
library(APCI)
library(tidyverse)

data <- read_csv("small_data_APC.csv")


#Delete the one individual who have NA values for the age variables

data <- subset(data, data$HHIDPN != 32570030)


#We first start with a data formatting to obtain a data set in which we have only
#four column, HHIDPN, a column which

data_APC <-  data.frame(matrix(nrow = 0, ncol = 4))
names(data_APC) <- c("HHIDPN","WAVE","AGE","HEALTH_INDEX")

for (i in (1:14)){
  columns <- c("HHIDPN",paste("INW",as.character(i),sep=""),
               paste("R",as.character(i),"AGEY_B",sep=""),
               paste("index_w",as.character(i),sep =""))
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

#Realization of the Age-Period-Cohort analysis 


APCI <- apci(outcome = "HEALTH_INDEX",age ="AGE_CAT", period = "WAVE",
             cohort = NULL, weight = NULL, covariate = NULL,
             data =data_APC, family ="gaussian",
             dev.test = FALSE, print = TRUE)
