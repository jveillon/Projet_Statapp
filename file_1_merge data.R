rm(list=ls())
getwd()                    
dir() 

setwd("C:/Users/julie/OneDrive/Documents/2A/Statapp/randhrs1992_2018v2_SAS")
library(haven)
data1 <- read_sas("randhrs1992_2018v2.sas7bdat")

setwd("C:/Users/julie/OneDrive/Documents/2A/Statapp/PGENSCORE4r3")
data2 <- read_sas("pgenscore4a_r.sas7bdat")
data3 <- read_sas("pgenscore4e_r.sas7bdat")

data2$HHIDPN <- 1000*as.numeric(data2$HHID)+as.numeric(data2$PN)
data3$HHIDPN <- 1000*as.numeric(data3$HHID)+as.numeric(data3$PN)

library(dplyr)
genetics <- merge(data2,data3,by="HHIDPN", all=TRUE)
merged_inter <- inner_join(data1, genetics, by="HHIDPN")


merged_all <- full_join(data1, genetics, by="HHIDPN")

write.csv(merged_inter, "merged_inter.csv", row.names=FALSE)
write.csv(merged_all, "merged_all.csv", row.names=FALSE)
