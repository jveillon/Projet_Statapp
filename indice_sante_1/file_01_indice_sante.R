library(readr)
library(haven)
library(dplyr)
library("stringr")
library("FactoMineR")
library("factoextra")
library("missMDA")
library(mice)
library(tidyr)

merged_all <- read_csv("data_merged_all.csv")

#We check how many spouses are not considered as a whole observation
#SiHHIDPN which are not in HHIDPN

var <- c()
for(i in (1:14)){
  #indentifying variable names
  name <-  paste("S",as.character(i),sep="")
  name <- paste(name,"HHIDPN",sep="")
  #list of spouses who are not in HHIDPN
  spouses <- merged_all[name][merged_all[name] != 0 & is.na(merged_all[name])==FALSE]
  for (x in spouses){
    if((x %in% merged_all$HHIDPN)==FALSE){
      var <- c(var,x)
    }
  }
}

#Deleting the duplicates#
var_s <- var[!duplicated(var)]

#We only have few spouses who are not in HHIDPN (1382), so we delete them

columns_names <- c(names(merged_all))
columns_begin_s <- tapply(columns_names,substr(columns_names,1,1),identity)$S
data <- select(merged_all, setdiff(columns_names,columns_begin_s))



###General health index###

#Firstly, we create an general health index for each individual and each wave 
#Here we don't consider the potential death

#Selection of interesting variables for the index (same for every wave)

var_temp_cat <- c("HOMCAR","HIBP","DIAB","CANCR",
                  "LUNG","HEART","STROK","PSYCH","ARTHR","BACK","DRINK",
                  "SMOKEN","STATUS","SLFMEM")
var_temp_quant <- c("HSPTIM","NRSTIM","DOCTIM","BMI")

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
data_index <- data.frame(data)

##Now we create the health index by wave

#For the percentage of explained variance

Variance = data.frame(matrix(nrow = 0, ncol = 4))

for (i in (1:14)){
  #Selection of the necessary part of the dataframe to create the index
  list_var <- c(c(VAR_indice[,paste("health_var_",as.character(i),sep="")]),"HHIDPN",paste("INW",as.character(i),sep=""))
  data_temp <- select(data_index,list_var)
  #Selection of the individual involved in the wave
  names(data_temp)[names(data_temp) == paste("INW",as.character(i),sep="")] <- "INW"
  data_temp <- data_temp[data_temp$INW == 1,]
  #We convert the class of categorial variables from numeric to character
  for (j in (1:length(VAR_indice[,1]))){
    if (VAR_indice[j,1]=="cat"){
      data_temp[j] <- lapply(data_temp[j], as.character)
    }
  }
  #Replacing Na values with "NA" for the categorial variable, we create here a
  #new categoy
  data_temp <- data_temp %>% mutate_if(is.character, ~replace_na(., "NA"))
  #Selection of the columns used for the FAMD
  data_temp_2 <- select(data_temp,-c("HHIDPN","INW"))
  #Imputation of the missing numeric values by PCA with "mice" library
  missing <- mice(data = data_temp_2, m = 3)
  data_temp_2 <- complete(missing,2)
  #FAMD to create the index of health
  FAMD <- FAMD (data_temp_2, ncp = 1, sup.var = NULL, ind = NULL, graph = FALSE)
  raw_index <- FAMD$ind$coord
  #Standardization of the index (mean already equal to zero)
  data_temp[paste("index_w",as.character(i),sep="")] <- raw_index / sd(raw_index)
  #Adding the index column to the data frame
  data_temp_index <- select(data_temp,c("HHIDPN",paste("index_w",as.character(i),sep="")))
  data_index <- merge(x=data_index, y=data_temp_index, by="HHIDPN", all.x=TRUE)
  #DataSet with the percentage of explained variance per wave
  Variance <- rbind(Variance,c(i,c(FAMD$eig)))
}

#Print the dataset of explained variance
names(Variance) <- c("wave","eigenvalue","variance.percent","cumulative.variance.percent")
print(Variance)
library("writexl")
write_xlsx(Variance,"results_Explained_variance.xlsx")


#Exportation to csv
write.csv(data_index,"data_health_index.csv", row.names=FALSE)
