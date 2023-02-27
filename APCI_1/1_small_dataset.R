#In this script, we create a smallest dataset with only the needed varaibles for
#the APC analysis

library(readr)
library(dplyr)

Data <- read_csv("data_health_index.csv")

index <- c()
INW <- c()
age <- c()
for (i in (1:14)){
  index <- c(index, paste("index_w",as.character(i),sep=""))
  INW <- c(INW, paste("INW",as.character(i),sep=""))
  age <- c(age,paste("R",as.character(i),"AGEY_B",sep=""))
}

small_data <- select(Data, c("HHIDPN",INW,age,index))

write.csv(small_data,"small_data_APC.csv", row.names=FALSE)
