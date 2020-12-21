#install.packages("missForest")
#library(missForest)
#library(rsample)
#library(rpart)
#library(purrr)
#library(vtreat)
#library(xgboost)

# NFL Predict
# 2019-11-25 
# Olivier Belhumeur
# belhumeurolivier@gmail.com
install.packages("rstudioapi")
library(rstudioapi)
path_base <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path_base)

# Libraries ###################################################################################

source("NFL_Packages.R")


# Base Infos ##################################################################################
source("NFL_Base_Info.R")
# NFL Web Scraping ############################################################################
source("NFL_Functions.R")
# Predict Scores ##############################################################################

Data_NFL <- Stat_UTD(Save_RDS = T)
Data_Team_Szn <- Data_NFL[[1]]
Data_Szn_Week <- Data_NFL[[2]]


# Format Data #################################################################################

C_Data_Team_Szn <- lapply(seq(32), function(Team) lapply(seq(2), function(Szn) cbind(Data_Team_Szn[[Team]][[Szn]][,1:10], apply(Data_Team_Szn[[Team]][[Szn]][,11:20],2,GMCM:::cummean))))

for(Team in seq(length(Complete_Names))){
  for(Szn in seq(length(C_Data_Team_Szn[[1]]))){
    colnames(C_Data_Team_Szn[[Team]][[Szn]]) <- c(colnames(Data_Team_Szn[[1]][[1]])[1:10], sprintf("C_%s",  colnames(Data_Team_Szn[[1]][[1]])[11:20]))
  }
}




SZN_Games <- lapply(seq(2), function(Szn) do.call("rbind", Data_Szn_Week[[Szn]]))


All_games <- do.call("rbind", SZN_Games)

# Rank----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Ranking_Stats_Szn <-unlist(list(lapply((sprintf("C_%s",  colnames(Data_Team_Szn[[1]][[1]])[11:15])),
                           function(Stats) lapply(seq(2),
                                                  function(Szn) t(do.call("rbind",lapply(seq(16), function(Week)
                                                    33 - as.numeric(rank(unlist(sapply(seq(32),
                                                                                             function(Team) C_Data_Team_Szn[[Team]][[Szn]][[Stats]][Week])), ties.method = "average"))))))),
                           lapply((sprintf("C_%s",  colnames(Data_Team_Szn[[1]][[1]])[16:20])),
                                                                                                                                                                            function(Stats) lapply(seq(2),
                                                                                                                                                                                                   function(Szn) t(do.call("rbind",lapply(seq(16), function(Week)
                                                                                                                                                                                                     as.numeric(rank(unlist(sapply(seq(32),
                                                                                                                                                                                                                                              function(Team) C_Data_Team_Szn[[Team]][[Szn]][[Stats]][Week])), ties.method = "average" ) ))))))), recursive = F)

# Cumulative Ranking----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Ranking_Team_Szn <- lapply(seq(32), function(Team) lapply(seq(2), function(Szn) t(do.call("rbind", lapply(seq(length(Ranking_Stats_Szn)), function(Stats)  Ranking_Stats_Szn[[Stats]][[Szn]][Team,])))))
C_Ranking_Team_Szn <- lapply(seq(32), function(Team) lapply(seq(2), function(Szn) t(do.call("rbind", lapply(seq(length(Ranking_Stats_Szn)), function(Stats)  GMCM:::cummean(Ranking_Stats_Szn[[Stats]][[Szn]][Team,]))))))

Rank_E_Team_Szn <- lapply(seq(32), function(Team) lapply(seq(2), function(Szn) do.call("rbind",  lapply(seq(16),
       function(Week) C_Ranking_Team_Szn[[as.numeric(Data_Team_Szn[[Team]][[Szn]]$Rang_Alpha_VS)[Week]]][[Szn]][Week,]))))

                                                                                                                                                                                                  
C_Rank_E_Team_Szn <- lapply(seq(32), function(Team) lapply(seq(2), function(Szn) apply(Rank_E_Team_Szn[[Team]][[Szn]],2,GMCM:::cummean)))


for(Team in seq(32)){
  for(Szn in seq(2)){
    colnames(Ranking_Team_Szn[[Team]][[Szn]]) <- sprintf("Rank_%s",  colnames(Data_Team_Szn[[1]][[1]])[11:20])
    colnames(C_Ranking_Team_Szn[[Team]][[Szn]]) <- sprintf("C_Rank_%s",  colnames(Data_Team_Szn[[1]][[1]])[11:20])
    
    colnames(Rank_E_Team_Szn[[Team]][[Szn]]) <- sprintf("Rank_E_%s",  colnames(Data_Team_Szn[[1]][[1]])[11:20])
    colnames(C_Rank_E_Team_Szn[[Team]][[Szn]]) <- sprintf("C_Rank_E_%s",  colnames(Data_Team_Szn[[1]][[1]])[11:20])
    }
}
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

DataRank_Team_Szn <- lapply(seq(32), function(Team) lapply(seq(2), function(Szn) cbind(Data_Team_Szn[[Team]][[Szn]], Ranking_Team_Szn[[Team]][[Szn]], Rank_E_Team_Szn[[Team]][[Szn]])))
C_DataRank_Team_Szn <- lapply(seq(32), function(Team) lapply(seq(2), function(Szn) cbind(C_Data_Team_Szn[[Team]][[Szn]], C_Ranking_Team_Szn[[Team]][[Szn]], C_Rank_E_Team_Szn[[Team]][[Szn]])))


Total_Games_Szn_Data <- lapply(seq(2), function(Szn) do.call("rbind", lapply(seq(32), function(Team) DataRank_Team_Szn[[Team]][[Szn]])))

Total_Games_Szn_C.Data <- lapply(seq(2), function(Szn) do.call("rbind", lapply(seq(32), function(Team) C_DataRank_Team_Szn[[Team]][[Szn]])))

Total_Games_Rank_C.Rank <- lapply(seq(2), function(Szn)lapply(seq(32), function(Team) merge(DataRank_Team_Szn[[Team]][[Szn]], C_DataRank_Team_Szn[[Team]][[Szn]])))



C_E_Stat_Szn_Team <- lapply(seq(2), function(Szn) lapply(seq(32), function(Team) t(sapply(seq(15), function(Games)  Total_Games_Rank_C.Rank[[Szn]][[as.numeric(unlist(Total_Games_Rank_C.Rank[[Szn]][[Team]][,7]))[(Games+1)]]][Games,41:50]))))

for(Szn in seq(2)){
  
for(Team in seq(32)){
  colnames(C_E_Stat_Szn_Team[[Szn]][[Team]]) <- paste("C_E", substr(colnames(C_E_Stat_Szn_Team[[Szn]][[Team]]),2, 20), sep = "")
}
}


Decal_Total_wo_order_Szn_Team <- lapply(seq(2), function(Szn) lapply(seq(32), function(Team) cbind(Total_Games_Rank_C.Rank[[Szn]][[Team]][2:16, c(6, 11:20)], Total_Games_Rank_C.Rank[[Szn]][[Team]][1:15, -c(6,11:20)], C_E_Stat_Szn_Team[[Szn]][[Team]])))

Decal_Total_Szn_Team <- lapply(seq(2), function(Szn) lapply(seq(32), function(Team) Decal_Total_wo_order_Szn_Team[[Szn]][[Team]][,c("Game_Nb", "Team_Abb", "Rang_Alpha","Team_Abb_VS", "Rang_Alpha_VS",   
                   "Outcome", "Points", 
                   "Points_VS", "Yards", "FirstDowns", "Int", "Fum", "Yards_VS",  "FirstDowns_VS",  "Int_VS", "Fum_VS","HomeField", 
                   "C_Points", "C_Yards", "C_FirstDowns", "C_Int",  "C_Fum", 
                   "C_Points_VS", "C_Yards_VS","C_FirstDowns_VS", "C_Int_VS", "C_Fum_VS", 
                   "C_E_Points", "C_E_Yards", "C_E_FirstDowns","C_E_Int", "C_E_Fum", 
                   "C_E_Points_VS", "C_E_Yards_VS", "C_E_FirstDowns_VS", "C_E_Int_VS", "C_E_Fum_VS", 
                   "Rank_Points", "Rank_Yards", "Rank_FirstDowns", "Rank_Int", "Rank_Fum",
                   "Rank_Points_VS", "Rank_Yards_VS", "Rank_FirstDowns_VS", "Rank_Int_VS", "Rank_Fum_VS",
                   "Rank_E_Points", "Rank_E_Yards", "Rank_E_FirstDowns",  "Rank_E_Int", "Rank_E_Fum",
                   "Rank_E_Points_VS", "Rank_E_Yards_VS", "Rank_E_FirstDowns_VS", "Rank_E_Int_VS", "Rank_E_Fum_VS", 
                   "C_Rank_Points", "C_Rank_Yards", "C_Rank_FirstDowns", "C_Rank_Int", "C_Rank_Fum",
                   "C_Rank_Points_VS", "C_Rank_Yards_VS","C_Rank_FirstDowns_VS", "C_Rank_Int_VS", "C_Rank_Fum_VS",
                   "C_Rank_E_Points", "C_Rank_E_Yards", "C_Rank_E_FirstDowns", "C_Rank_E_Int", "C_Rank_E_Fum", 
                   "C_Rank_E_Points_VS", "C_Rank_E_Yards_VS", "C_Rank_E_FirstDowns_VS", "C_Rank_E_Int_VS", "C_Rank_E_Fum_VS")]))

for(Szn in seq(2)){
for(Team in seq(32)){
  Decal_Total_Szn_Team[[Szn]][[Team]]$Game_Nb <- Decal_Total_Szn_Team[[Szn]][[Team]]$Game_Nb+1
}
}
#Cant_Know_Before_Or_Useless <- c("Rang_Alpha", "Rang_Alpha_VS", "Outcome", "Yards", "FirstDowns", "Int", "Fum", "Points_VS", "Yards_VS", "FirstDowns_VS", "Int_VS", "Fum_VS")
Cant_Know_Before_Or_Useless <- c("Rang_Alpha", "Rang_Alpha_VS", "Outcome")
Test_V_KB_Total_Szn_Team <- lapply(seq(2), function(Szn) lapply(seq(32), function(Team) Decal_Total_Szn_Team[[Szn]][[Team]][,-..Cant_Know_Before_Or_Useless]))



Total_Decal_2018 <- do.call("rbind", Test_V_KB_Total_Szn_Team[[1]])
Total_Decal_2019 <- do.call("rbind", Test_V_KB_Total_Szn_Team[[2]])
Total_Decal <- rbind(Total_Decal_2018, Total_Decal_2019)

Total_Decal$Points <- as.numeric(Total_Decal$Points)
Total_Decal$Team_Abb <- as.factor(Total_Decal$Team_Abb) 
Total_Decal$Team_Abb_VS <- as.factor(Total_Decal$Team_Abb_VS)
Total_Decal$HomeField <- as.factor(Total_Decal$HomeField)

Data_test <- cbind(Total_Decal[,c(2:3)],apply(as.matrix(Total_Decal[,-c(1:3)]), 2, as.numeric) )
# Prediction ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#test <- function(modelz, data){
#  switch()
#}

#lm(Points~., data = Data_train),

test <- function(Data_Train, Data_Predict, Model){
  

Predict_Value <- switch(Model,
"LM" = unlist(predict(object = lm(Points~., data = Data_train) , newdata = Data_Predict, type = "response")),
"GLM_POIS" =  glm(Points~., data = Data_train, family = "poisson"),
"GLM_NBIN"= glm.nb(Points~., data = Data_train),
"MissRF"= 2,
"XgBoost"= 2,
"XgBoost_No_RF"= 2,
"Weight_AVG_Value"= 2,
)
return(Predict_Value)
}

Vec_Models
for(modelz in Vec_Models){
  List_Modelz[[modelz]] <- 
}
  
#XGBoost function ############################################################################  
source("NFL_XGB_fun.R")


NFL_Predict <- function(Data = Data_test, from = 481, to = 960, Vec_Models = c("LM", "GLM_POIS", "GLM_NBIN","MissRF", "XgBoost", "XgBoost_No_RF", "Weight_AVG_Value")){
  

min_game <- from
max_game <- to
Actual_Models <-  c("Actual Value", Vec_Models)
nb_model <- length(Actual_Models)
ntry <- length(min_game:max_game)
Result <- matrix(numeric(ntry *nb_model), ncol =nb_model)
colnames(Result) <- Actual_Models

#Weight_AVG_Value <- numeric(ntry)
Best_Model_table <- numeric(nb_model-1)
names(Best_Model_table) <- Actual_Models[-1]

Sample_test<- (min_game:max_game)

for(i in seq(ntry)){  
  
  Rm <- Sample_test[i]
  Data_train <- Data[1:(Rm-1),]
  Data_predict <- Data[Rm,]
  Pts_actual <- unlist(Data_predict[,3])
  Data_predict[,(3:12)] <- NA
  
  

  
  
  
  lm_mod <- lm(Points~., data = Data_train)
  glm_pois <- glm(Points~., data = Data_train, family = "poisson")
  glm_nbin <- glm.nb(Points~., data = Data_train)
  
  #glm_elasticnet <- glmnetUtils::cv.glmnet(Points~., data = Data[-Rm,], family = "poisson")
  Pts_actual <- unlist(Data_predict[,3])
  Data_predict[,(3:12)] <- NA
  data.imputed <- missForest(rbind(Data_train,Data_predict))$ximp
  
  ##XGBoost no RF
  new_data <- Data[(1:(480+i)),-(4:12)]
  
  
  Result[i,1] <- Pts_actual
  Result[i,2] <- unlist(predict(lm_mod, newdata = data.imputed[Rm,-3]))
  Result[i,3] <- unlist(predict(glm_pois, newdata = data.imputed[Rm,-3], type = "response"))
  Result[i,4] <- unlist(predict(glm_nbin, newdata = data.imputed[Rm,-3], type = "response"))
  Result[i,5] <- unlist(data.imputed[Rm,3])
  Result[i,6] <- mean(replicate(10,(XGB_VALUE(Data_Impute = data.imputed))))
  Result[i,7] <- XGB_VALUE(new_data)
  
  if(i == 1){
    Result[i,8] <- mean(Result[i, -1])
  }else{
    
    Best_model_order <- which.min(sapply(seq(nb_model-1), function(j) abs(Result[(i-1),(j+1)]- Result[(i-1),1])))
    Best_Model_table[Best_model_order] <- Best_Model_table[Best_model_order] +1

    Result[i,8] <- weighted.mean(Result[i,-1], Best_Model_table)
    }

  print(i)  
  print(paste(round(i/ntry,3)*100, "%", sep = ""))
  
  
}
}
#tot <- cbind(Result, Weight_AVG_Value)

Rd_Result <- apply(Result, 2, round)

Best_model_order_Final <- sapply(seq(ntry), function(i) which.min(sapply(seq(nb_model-1), function(j) abs(Result[i,(j+1)]- Result[i,1]))))
Best_Model_table_Final <- tabulate(Best_model_order_Final, nb_model-1)
names(Best_Model_table_Final) <- Actual_Models[-1]
Best_Model_table_Final

# PTS_average <- sapply(seq(ntry), function(i) weighted.mean(Result[i,-1], Best_Model_table))
# 
# PTS_average


par(mfrow =c(nb_model-1,2))

dif_abs <- do.call("rbind", lapply(seq(ntry), function(i) sapply(seq(nb_model-1), function(j) abs(Result[i,(j+1)]- Result[i,1]))))
dif <- do.call("rbind", lapply(seq(ntry), function(i) sapply(seq(nb_model-1), function(j) Result[i,(j+1)]- Result[i,1])))

#saveRDS(Result, file = "Resultats_2020_04_28")
#Result <- read_rds("Resultats_2020_04_27")

hist(dif[,1], breaks =15)
plot(dif[,1])
hist(dif[,2], breaks =15)
plot(dif[,2])
hist(dif[,3], breaks =15)
plot(dif[,3])
hist(dif[,4], breaks =15)
plot(dif[,4])
hist(dif[,5], breaks =15)
plot(dif[,5])
hist(dif[,6], breaks =15)
plot(dif[,6])
hist(dif[,7], breaks =15)
plot(dif[,7])

mean(dif[,1]^2)
mean(dif[,2]^2)
mean(dif[,3]^2)
mean(dif[,4]^2)
mean(dif[,5]^2)
mean(dif[,6]^2)
mean(dif[,7]^2)

mean(dif[,1])
mean(dif[,2])
mean(dif[,3])
mean(dif[,4])
mean(dif[,5])
mean(dif[,6])
mean(dif[,7])

mean(dif_abs[,1])
mean(dif_abs[,2])
mean(dif_abs[,3])
mean(dif_abs[,4])
mean(dif_abs[,5])
mean(dif_abs[,6])
mean(dif_abs[,7])


PTS_average <- sapply(seq(ntry), function(i) weighted.mean(Result[i,-1], Best_Model_table))

PTS_average


wm_pts <- cbind(Result, PTS_average)

wm_pts

global <- cbind(Result, values)


colnames(global) <- c(colnames(Result), "XgBoost_no_FR")

Best_model_order_add_wm_pts <- sapply(seq(ntry), function(i) which.min(sapply(seq(nb_model), function(j) abs(global[i,(j+1)]- global[i,1]))))
Best_Model_table_add_wm_pts <- tabulate(Best_model_order_add_wm_pts, nb_model)



var(wm_pts[,7] - wm_pts[,1])
var(global[,6]- global[,1])
var(global[,7] - global[,1])









# Data_predict
# 
# xgb_v1 <- xgboost(data = Data_train[,-"Points"], label = unlist(Data_train[,"Points"]), nrounds = 800, verbose = F, missing = NA)
# preds_v1 <- predict(xgb_v1, newdata = test_v1, missing = NA)




Data_train <- Data_test[1:481,]
fac_df <- Data_train[,c("Team_Abb", "Team_Abb_VS")]


ff <-  ~ . - 1
mf <- model.frame(formula = ff, data = fac_df, na.action = na.pass)
fac_mat_v1 <- model.matrix(object = ff, data = mf, contrasts.arg = map(mf, ~ contrasts(., contrasts = F)))



mat <-  as.matrix(cbind(fac_mat_v1, Data_train[, -c("Team_Abb", "Team_Abb_VS")]))
Output_PTS <- as.matrix(Data_train[, "Points"])

train <- mat[-Rm,]
train_Output_PTS <- Output_PTS[-Rm,]

pred <- as.matrix(t(mat[Rm,]))
pred[1,65:74] <- NA

xgb_v1 <- xgboost(data = train, label = train_Output_PTS, nrounds = 800, verbose = F, missing = NA)
preds_v1 <- predict(xgb_v1, newdata = pred, missing = NA)


