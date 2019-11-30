# NFL Predict
# 2019-11-25 
# Olivier Belhumeur
# belhumeurolivier@gmail.com

path_base <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path_base)

# Libraries ###################################################################################
{library("rvest")
library("plyr")
library("lubridate")
library("rpart")
library("rpart.plot")
library("abind")
library("rstudioapi")  
}

# Base Infos ##################################################################################
source("NFL_Base_Info.R")
# NFL Web Scraping ############################################################################
source("NFL_Functions.R")
# Predict Scores ##############################################################################

Data_Week <- Stat_UTD(Save_RDS = T)
List_Data_Teams <- lapply(seq(length(Complete_Names)), function(teamz) aperm(Data_Week[teamz,,,], c(2,1,3)))
List_Stats_Teams <- lapply(seq(length(Complete_Names)), function(teamz) List_Data_Teams[[teamz]][,10:19,])

Var_Exp <- c("Yards", "FirstDowns", "Int", "Fum", "Points_VS", "Yards_VS", "Firstdowns_VS", "Int_VS", "Fum_VS")
for(teamz in seq(length(Complete_Names))){
colnames(List_Stats_Teams[[teamz]]) <-  c("Points", Var_Exp)
}

names(List_Data_Teams) <-City_Team_Sign[,"Team_Abb"]
names(List_Stats_Teams) <-City_Team_Sign[,"Team_Abb"]


k_fold <- numeric(11)

for(testing in 1:11){
  set.seed(0)
  m <- 60
  p <- 0.8
  
  training <- seq(11)[-testing]
  
  rez <- as.numeric(GB$Points[training])
  
  for(i in seq(m)){
    vecbin <- sample(seq(length(Var_Exp)),rbinom(1, length(Var_Exp), p))
    temp_matrix <- GB[training, vecbin]
    GB_rpart_temp <- rpart(rez ~., data = temp_matrix,   control = rpart.control(minbucket = 3))
    assign(paste("GB_rpart_temp", i, sep = ""), GB_rpart_temp)
    rez <- rez - predict(GB_rpart_temp)
  }
  
  rez_fin <- 0
  new_data <- GB[testing,]
  rez_suivi <- as.numeric(m)
  for (i in seq(m)){
    rez_fin = rez_fin + predict(eval(parse(text = paste("GB_rpart_temp", i, sep = ""))), newdata = new_data)
    rez_suivi[i] <- rez_fin
  }

  print(c(GB$Points[testing],rez_fin))
  plot(rez_suivi)
  k_fold[testing] <- rez_fin
}
point_max <- max(GB$Points,k_fold)
plot(GB$Points,round(k_fold),xlim = c(0,point_max),ylim = c(0,point_max))
lines(c(0,100),c(0,100))
lines(c(0,100),c(0,100)-3,col = "blue");lines(c(0,100),c(0,100)+3,col = "blue")
lines(c(0,100),c(0,100)-8,col = "red");lines(c(0,100),c(0,100)+8,col = "red")

summary(GB$Points - k_fold)

















