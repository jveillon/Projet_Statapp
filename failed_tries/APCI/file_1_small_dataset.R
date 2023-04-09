#In this script, we create a smallest dataset with only the needed variables for
#the APC analysis

library(readr)
library(dplyr)

Data <- read_csv("data_03.csv")

#Lists of varaibles names
index <- c()
INW <- c()
age <- c()
for (i in (1:14)){
  index <- c(index, paste("GHI",as.character(i),sep=""))
  INW <- c(INW, paste("INW",as.character(i),sep=""))
  age <- c(age,paste("R",as.character(i),"AGEY_B",sep=""))
}

#Select the variables
small_data <- select(Data, c("HHIDPN",INW,age,index))

#Write the smallest dataset to csv
write.csv(small_data,"data_small_data_APC.csv", row.names=FALSE)
