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
library("GMCM")
    
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

RM_NA_List_Stats_Teams <- lapply(seq(nrow(City_Team_Sign)), function(teamz) lapply(seq(dim(List_Stats_Teams[[teamz]])[3]), function(Yearz) List_Stats_Teams[[teamz]][!is.na(List_Stats_Teams[[teamz]][,1,Yearz]),,Yearz]))
Cummean_List_Stats_Teams <- lapply(seq(nrow(City_Team_Sign)), function(teamz) lapply(seq(dim(List_Stats_Teams[[teamz]])[3]), function(Yearz) apply(List_Stats_Teams[[teamz]][!is.na(List_Stats_Teams[[teamz]][,1,Yearz]),,Yearz], 2, GMCM:::cummean)))

names(List_Data_Teams) <-City_Team_Sign[,"Team_Abb"]
names(List_Stats_Teams) <-City_Team_Sign[,"Team_Abb"]
names(RM_NA_List_Stats_Teams) <- City_Team_Sign[,"Team_Abb"]
names(Cummean_List_Stats_Teams) <- City_Team_Sign[,"Team_Abb"]


for(i in seq(nrow(City_Team_Sign))){

Teamz_Statz <- RM_NA_List_Stats_Teams[[i]][[2]]
Teamz_Statz_N <- apply(Teamz_Statz, 2, as.numeric)[,-1]

Nb_Games_Played <- nrow(Teamz_Statz)

k_fold <- numeric(Nb_Games_Played)

for(testing in seq(Nb_Games_Played)){
  
  set.seed(0)
  m <- 20
  p <- 0.8
  
  training <- seq(Nb_Games_Played)[-testing]
  
  
  Points_ref <- as.numeric(Teamz_Statz[,"Points"])[-testing]
  Points <- Points_ref
  for(i in seq(m)){
    vecbin <- sample(seq(length(Var_Exp)),rbinom(1, length(Var_Exp), p))
    temp_matrix <- data.frame(cbind(Points, Teamz_Statz_N[training, vecbin]))
    team_rpart_temp <- rpart(Points~., data = temp_matrix,   control = rpart.control(minbucket = 2))
    assign(paste("team_rpart_temp", i, sep = ""), team_rpart_temp)
    Points <- Points - predict(team_rpart_temp)
  }
  
  rez_fin <- 0
  new_data <- data.frame(t(as.numeric(Teamz_Statz[testing,])))
  names(new_data) <- c("Points", Var_Exp)
  rez_suivi <- numeric(m)
  for (i in seq(m)){
    rez_fin = rez_fin + predict(eval(parse(text = paste("team_rpart_temp", i, sep = ""))), newdata = new_data)
    rez_suivi[i] <- rez_fin
  }

  print(c(Teamz_Statz[,"Points"][testing],rez_fin))
  plot(rez_suivi)
  k_fold[testing] <- rez_fin
}


}
point_max <- max(as.numeric(RM_NA_List_Stats_Teams[[i]][[2]][,"Points"]),k_fold)
plot(as.numeric(RM_NA_List_Stats_Teams[[i]][[2]][,"Points"]),round(k_fold),xlim = c(0,point_max),ylim = c(0,point_max))
lines(c(0,100),c(0,100))
lines(c(0,100),c(0,100)-3,col = "blue");lines(c(0,100),c(0,100)+3,col = "blue")
lines(c(0,100),c(0,100)-8,col = "red");lines(c(0,100),c(0,100)+8,col = "red")

summary(Points_ref - k_fold)

















