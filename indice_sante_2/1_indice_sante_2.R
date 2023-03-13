library(readr)
library(haven)
library(dplyr)
library("stringr")
library("FactoMineR")
library("factoextra")
library("missMDA")
library(mice)
library(tidyr)

data <- read_csv("data_health_index.csv")


#Delete the one individual who have NA values for the age variables

data <- subset(data, data$HHIDPN != 32570030)


###General health index###

#Firstly, we create an general health index for each individual by taking the age
#in consideration
#Here we don't consider the potential death

#Selection of interesting variables for the index (same for every wave)

var_temp_cat <- c("HOMCAR","HIBP","DIAB","CANCR",
                  "LUNG","HEART","STROK","PSYCH","ARTHR","BACK","DRINK",
                  "SMOKEN","STATUS","SLFMEM")
var_temp_quant <- c("HSPTIM","NRSTIM","DOCTIM","BMI","AGEY_B")

#We create a column to know if the varaible is categorial or quatitative
CAT_CONT <- c( rep(c("cat"),each = length(var_temp_cat)), rep(c("cont"),each = length(var_temp_quant)))
VAR_indice <- data.frame(CAT_CONT)

#We create a dataframe to store the names of the variables used for the index
#there is also the column "categorial or quatitative"
for(i in (1:14)){
  columns_names_index <- c()
  for (x in var_temp_cat){
    columns_names_index <- c(columns_names_index,paste("R",as.character(i),x,sep=""))
  }
  for (x in var_temp_quant){
    columns_names_index <- c(columns_names_index,paste("R",as.character(i),x,sep=""))
  }
  VAR_indice[paste("health_var_",as.character(i),sep="")] <- columns_names_index
}

#We add the mental health variables which have a different name for the wave 1

var_temp_add <- c("cat","R1DEPREX")
for (i in (2:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"DEPRES",sep=""))
}
VAR_indice <- rbind(VAR_indice,var_temp_add)

var_temp_add <- c("cat","R1EFFORX")
for (i in (2:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"EFFORT",sep=""))
}
VAR_indice <- rbind(VAR_indice,var_temp_add)

var_temp_add <- c("cat","R1SLEEPX")
for (i in (2:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"SLEEPR",sep=""))
}
VAR_indice <- rbind(VAR_indice,var_temp_add)

var_temp_add <- c("cat","R1WHAPPX")
for (i in (2:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"WHAPPY",sep=""))
}
VAR_indice <- rbind(VAR_indice,var_temp_add)

var_temp_add <- c("cat","R1FLONEX")
for (i in (2:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"FLONE",sep=""))
}
VAR_indice <- rbind(VAR_indice,var_temp_add)

var_temp_add <- c("cat","R1FSADX")
for (i in (2:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"FSAD",sep=""))
}
VAR_indice <- rbind(VAR_indice,var_temp_add)

var_temp_add <- c("cat","R1GOINGX")
for (i in (2:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"GOING",sep=""))
}
VAR_indice <- rbind(VAR_indice,var_temp_add)

var_temp_add <- c("cat","R1ENLIFX")
for (i in (2:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"ENLIFE",sep=""))
}
VAR_indice <- rbind(VAR_indice,var_temp_add)

#We also add the variables about physical activity
var_temp_add <- c("cat")
for (i in (1:6)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"VIGACT",sep=""))
}
for (i in (7:14)){
  var_temp_add <- c(var_temp_add,paste("R",as.character(i),"VGACTX",sep=""))
}

VAR_indice <- rbind(VAR_indice,var_temp_add)


#Now we will formate the data

list_names <- c(c("HHIDPN","WAVE"),sub('..', '', VAR_indice[,2]))

data_index <-  data.frame(matrix(nrow = 0, ncol = length(list_names)))

names(data_index) <- list_names

for (i in (1:14)){
  columns <- c("HHIDPN",paste("INW",as.character(i),sep=""), c(VAR_indice[,i+1]))
  data_temp <- select(data,columns)
  names(data_temp) <- list_names
  data_temp <- data_temp[data_temp$WAVE == 1,]
  data_temp$WAVE[data_temp$WAVE == 1] <- i
  data_index <- rbind(data_index,data_temp)
}


#We convert the class of categorial variables from numeric to character


for (j in (1:length(VAR_indice[,1]))){
  if (VAR_indice[j,1]=="cat"){
    data_index[j+2] <- lapply(data_index[j+2], as.character)
  }
}

#Replacing Na values with "NA" for the categorial variable, we create here a
#new categoy
data_index <- data_index %>% mutate_if(is.character, ~replace_na(., "NA"))

#Selection of the columns used for the FAMD
data_temp <- select(data_index,-c("HHIDPN","WAVE"))

#Imputation of the missing numeric values by PCA with "mice" library
missing <- mice(data = data_temp, m = 3)
data_temp <- complete(missing,2)

#FAMD to create the index of health
FAMD <- FAMD (data_temp, ncp = 1, sup.var = NULL, ind = NULL, graph = FALSE)
raw_index <- FAMD$ind$coord

#Standardization of the index 
data_index["index"] <- (raw_index - mean(raw_index)) / sd(raw_index)

#We create new columns for the health index
data_inw <-  data.frame(data$HHIDPN)
names(data_inw) <- c("HHIDPN")
compteur = 0
for (j in data_inw$HHIDPN){
  data_inw_temp <- data_index[data_index$HHIDPN == j,]
  compteur = compteur +1
  for (k in data_inw_temp$WAVE){
    data_inw[compteur,paste("index_2_w",as.character(k),sep="")] <- data_inw_temp$index[data_inw_temp$WAVE == k]
  }
}

final_data <- merge (x=data , y=data_inw , by="HHIDPN" , all=TRUE)

write.csv(final_data,"data_health_index_2.csv", row.names=FALSE)
