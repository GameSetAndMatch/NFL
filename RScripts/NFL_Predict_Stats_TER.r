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
# LM ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# # Data_test$Team_Abb <- as.factor(Data_test$Team_Abb) 
# # Data_test$Team_Abb_VS <- as.factor(Data_test$Team_Abb_VS)
# # Data_test$HomeField <- as.factor(Data_test$HomeField)
# 
# 
# 
# 
# 
# min_game <- 481
# max_game <- 485
# nb_model <- 5
# ntry <- length(min_game:max_game)
# Result <- matrix(numeric(ntry *nb_model), ncol =nb_model)
# colnames(Result) <- c("Actual Value", "LM", "GLM POIS", "GLM NBIN","MissRF")
# 
# 
# Sample_test<- (min_game:max_game)
#   for(i in seq(ntry)){  
#     
# Rm <- Sample_test[i]
# Data <- Data_test[-(min(960, (Rm+1)):960),]
# 
# lm_mod <- lm(Points~., data = Data[-Rm,])
# glm_pois <- glm(Points~., data = Data[-Rm,], family = "poisson")
# glm_nbin <- glm.nb(Points~., data = Data[-Rm,])
# 
# #glm_elasticnet <- glmnetUtils::cv.glmnet(Points~., data = Data[-Rm,], family = "poisson")
# 
# Data[Rm,(3:12)] <- NA
# data.imputed <- missForest(Data)$ximp
# 
# Result[i,1] <- unlist(Data_test[Rm,3])
# Result[i,2] <- unlist(predict(lm_mod, newdata = data.imputed[Rm,-3]))
# Result[i,3] <- unlist(predict(glm_pois, newdata = data.imputed[Rm,-3], type = "response"))
# Result[i,4] <- unlist(predict(glm_nbin, newdata = data.imputed[Rm,-3], type = "response"))
# Result[i,5] <- unlist(data.imputed[Rm,3])
# 
# print(paste(round(i/ntry,3)*100, "%", sep = ""))
# 
# 
# }
# 
# 
# Best_Model <- tabulate(sapply(seq(ntry), function(i) which.min(sapply(seq(4), function(j) abs(Result[i,(j+1)]- Result[i,1])))), 4)
# names(Best_Model) <- c("LM", "GLM POIS", "GLM NBIN", "MissRF")
# print(Best_Model)
# 
# par(mfrow =c(4,2))
# 
# dif_abs <- do.call("rbind", lapply(seq(ntry), function(i) sapply(seq(4), function(j) abs(Result[i,(j+1)]- Result[i,1]))))
# dif <- do.call("rbind", lapply(seq(ntry), function(i) sapply(seq(4), function(j) Result[i,(j+1)]- Result[i,1])))
# 
# #saveRDS(Result, file = "Resultats")
# 
# hist(dif[,1])
# plot(dif[,1])
# hist(dif[,2])
# plot(dif[,2])
# hist(dif[,3])
# plot(dif[,3])
# hist(dif[,4])
# plot(dif[,4])
# 
# sum(dif[-480,1]^2)
# sum(dif[-480,2]^2)
# sum(dif[-480,3]^2)
# sum(dif[-480,4]^2)
# 
# #Models à tester
# # Elastic net
# # RF
# # Xgboost
# # Gam
# # flexmix ?
# # NN ?
# 
# # Estimer les var avec miss forest ou non ????
# xg
# 
# ?
# 
# 
# 
# #Elastic net#######################################
# 
# glm_elasticnet_alpha0_cvfit <- glmnetUtils::cv.glmnet(Points~., data = Data[-959,], family = "poisson", alpha = 0)
# listeLambda <- seq(glm_elasticnet_alpha0_cvfit$lambda[1], glm_elasticnet_alpha0_cvfit$lambda[length(glm_elasticnet_alpha0_cvfit$lambda)], length = 10000)
# 
# fit_alpha0 <- glmnetUtils::glmnet(Points~., data = Data[-959,], family = "poisson", alpha = 0, lambda = listeLambda)
# 
# 
# 
# plot(fit_alpha0, xvar = "lambda", label = "T")
# exp(predict(fit_alpha0, data.imputed[959,-3])[1,1], type = "Link" )





#RF########
#Xgboost########
#GAM########
#FLEMIX########
#NN########
















# yo_elastic <- glmnetUtils::glmnet(Points~., data = data.frame(LM_Test)[1:14,], type.measure="mse", 
#                                   alpha=1, family="gaussian", nfolds = 4)
# 
# predict(yo_elastic, newdata = data.frame(LM_Test)[15,-1], s = yo_elastic$lambda.min)


#Test_V_KB[,c("Game_Nb", "Team_Abb", "Team_Abb_VS", "HomeField")]
#C_games2019 <- C_games2019[,-c(1:9)]

# hist(as.numeric(Test_V_KB$Points))
# hist(Test_V_KB$Rank_Points)







#yooo <- as.formula(paste(colnames(games2019)[1], paste(sprintf("as.numeric(%s)", colnames(games2019)[-1]), collapse=" + "), sep=" ~ "))
#C_yooo <- as.formula(paste(colnames(C_games2019)[1], paste(sprintf("as.numeric(%s)", colnames(C_games2019)[-1]), collapse=" + "), sep=" ~ "))

#test_R_Part <- rpart(Test_V_KB$Points~., data = Test_V_KB)
#paste(colnames(C_games2019)[10], paste(sprintf("as.numeric(%s)", colnames(C_games2019)[-(c(1:9, (c(1,13,14,15,20,25,27,29,30)+9)))]), collapse=" + "), sep=" ~ ")

# Games_2019_Combined
# 
# C_yooo_R <- as.formula(paste(colnames(C_games2019)[11], paste(sprintf("as.numeric(%s)", colnames(C_games2019)[-(c(1:10, (c(1,13,14,15,20,25,27,29,30)+10)))]), collapse=" + "), sep=" ~ "))
# 
# meep <- lm(yooo, data = games2019)
# C_meep <- lm(C_yooo, data = C_games2019)
# C_meep_R <- lm(C_yooo_R, data = C_games2019)


#colnames(All_games_Combined)[-(1:10)]


#colnames(Games_2019_Combined[[12]])[11]
#colnames(Games_2019_Combined[[12]])[-(1:11)]


#Test_Decal_2019 <- do.call("rbind", lapply(seq(1), function(Team) cbind(Games_2019_Combined[[Team]][2:16,11], Games_2019_Combined[[Team]][1:15, -(1:40)])))
#lm_Decal_formula <- as.formula(paste(colnames(Test_Decal_2019)[1], paste(sprintf("as.numeric(%s)", colnames(Test_Decal_2019)[-1]), collapse=" + "), sep=" ~ "))

#Test_Decal_2019 <- do.call("rbind", lapply(seq(1), function(Team) cbind(Games_2019_Combined[[Team]][2:16,11], Games_2019_Combined[[Team]][1:15, c(11, 1, 4, 5, 7, 9 , 22:80)])))
#c(11, 1, 4, 5, 7, 9 , 22:70)


#Decal_test_LM <- lm(lm_Decal_formula, data = Test_Decal_2019)
#summary(Decal_test_LM)


#Tot_Decal_2019 <- data.table(do.call("rbind", lapply(seq(1), function(Team) cbind(Games_2019_Combined[[Team]][2:16,11], Games_2019_Combined[[Team]][1:15,]))))

#Data_rpart_Tot_decal_2019 <- do.call("rbind", lapply(seq(32), function(Team) cbind(Games_2019_Combined[[Team]][2:16,11], Games_2019_Combined[[Team]][1:15, c(1, 4, 5, 9 , 21:70)])))

# rpart_Decal_formula <- as.formula(
#   paste(colnames(Data_rpart_Tot_decal_2019)[1],
#         paste(c(sprintf("as.character(%s)",as.character(colnames(Data_rpart_Tot_decal_2019)[c(2, 4)])),
#                 sprintf("as.numeric(%s)", colnames(Data_rpart_Tot_decal_2019)[c(3, 5:55)])), collapse=" + " ),
#         sep=" ~ ")
#   )
#test_rpart <- rpart(rpart_Decal_formula, Data_rpart_Tot_decal_2019)
#tes <- randomForest(rpart_Decal_formula, data =  Data_rpart_Tot_decal_2019)
# paste(sprintf("as.character(%s)",as.character(colnames(Data_rpart_Tot_decal_2019)[c(2, 4)])), collapse=" + ")
# sprintf("as.numeric(%s)", colnames(Data_rpart_Tot_decal_2019)[c(3, 5:55)])

# df1 <- data.frame("A" = c(1,2), "B"= c("YOes", "Wages"), "D"= c("YOal", "Wagal"))
# df2 <- data.frame("A" = c(1,2), "B"= c("YOes", "Wages"), "C"= c("YO", "Wag"))
# merge(df1,df2)















# data(iris)
# set.seed(71)
# iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
#                         proximity=TRUE)
# 
# 
# 
# 
# alpha0.fit <- glmnetUtils::cv.glmnet(as.numeric(Points)~., data =  data.frame(LM_Test)[c(1:13,15),], type.measure="mse", 
#                                      alpha=1, family="gaussian", nfolds = 4 ,grouped=FALSE)

## now let's run the Testing dataset on the model created for
## alpha = 0 (i.e. Ridge Regression).
# for(i in seq(15)){
#   
#   alpha1.fit <- glmnetUtils::cv.glmnet(as.numeric(Points)~., data =  data.frame(LM_Test)[-i,], type.measure="mse", 
#                                        alpha=.5, family="gaussian", nfolds = 4 ,grouped=FALSE)
# 
# alpha1.predicted <-   round(predict(alpha1.fit,  data.frame(LM_Test)[i,-1], s=alpha0.fit$lambda.min))
# print(alpha1.predicted)
# print(data.frame(LM_Test)[i,1])
# }
## s = is the "size" of the penalty that we want to use, and
# set.seed(42)
# Data_test_xgb <- Data_test[1:960,]
# train <- Data_test_2018[1:480,]
# 
# test <- Data_test_2018[481,-(3:12)]
# 
# 
# 
# features <- setdiff(names(train), "Points")									# nom des variables
# treatplan <- vtreat::designTreatmentsZ(train, features, verbose = F)		
# 
# 
# new_vars <- treatplan %>% magrittr::use_series(scoreFrame) %>% dplyr::filter(code %in% c("clean","lev")) %>% magrittr::use_series(varName)
# ## Preparer le jeu d'apprentisssage:
# features_train <- vtreat::prepare(treatplan, train, varRestriction = new_vars) %>% as.matrix()
# data.xgb_train <- cbind(train$Points, features_train)
# colnames(data.xgb_train)[1] <- "Points"
# 
# 
# 
# 
# features <- setdiff(names(test), "Points")									# nom des variables
# treatplan <- vtreat::designTreatmentsZ(test, features, verbose = F)		
# 
# 
# new_vars <- treatplan %>% magrittr::use_series(scoreFrame) %>% dplyr::filter(code %in% c("clean","lev")) %>% magrittr::use_series(varName)
# ## Preparer le jeu d'apprentisssage:
# features_train <- vtreat::prepare(treatplan, test, varRestriction = new_vars) %>% as.matrix()
# data.xgb_test <- cbind(test$Points, features_train)
# colnames(data.xgb_test)[1] <- "Points"
# 
# 
# trainm <- sparse.model.matrix(Points ~ .-1, data = train)
# head(trainm)
# train_label <- train$Points
# train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = unlist(train_label))
# 
# testm <- sparse.model.matrix(Points ~ .-1, data = test)
# head(testm)
# test_label <- test$Points
# test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = unlist(test_label))
# 
# 
# params_xgb <- list(train= train_matrix, test = test_matrix)
# 
# 
# 
# xgb_model1 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 500, max_depth =3, eval_metric = "rmse",
#                         eta =0.1, watchlist = params_xgb, early_stopping_rounds = 30)
# #Stopping. [42]	train-rmse:3.881881	test-rmse:7.039956
# 
# xgb_model2 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 800, max_depth =3, eval_metric = "rmse",
#                         eta =0.1, watchlist = params_xgb, early_stopping_rounds = 30)
# #Stopping. [42] train-rmse:3.881881	test-rmse:7.039957
# 
# 
# 
# xgb_model3 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 800, max_depth =6, eval_metric = "rmse",
#                         eta =0.1, watchlist = params_xgb, early_stopping_rounds = 30)
# #Stopping. [41]	train-rmse:1.030049	test-rmse:7.901068
# 
# 
# 
# xgb_model4 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 800, max_depth =2, eval_metric = "rmse",
#                         eta =0.1, watchlist = params_xgb, early_stopping_rounds = 30)
# #Stopping. [74]	train-rmse:4.597352	test-rmse:6.885380
# 
# xgb_model5 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 1000, max_depth =2, eval_metric = "rmse",
#                         eta =0.05, watchlist = params_xgb, early_stopping_rounds = 200)
# #Stopping. Best iteration: [121]	train-rmse:5.015891	test-rmse:6.898094
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# test_xgb
# test_predictm <- sparse.model.matrix(Points ~ .-1, data = test_xgb)
# head(test_predictm)
# test_label <- test_predictm$Points
# 
# test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = unlist(test_label))
# 
# 
# test_matrix[241,]
# 
# predict(xgb_model4, newdata = test_matrix, class = "response")
# 
# 
# 
# testm <- sparse.model.matrix(Points ~ .-1, data = test)
# head(testm)
# test_label <- test$Points
# test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = unlist(test_label))



















# 
# test_impute_XGB <- data.imputed
# 
# 
# 
# Result_XGB <- matrix(numeric(480 *2), ncol =2)
# colnames(Result_XGB) <- c("Actual", "XGB")
# prop_train <- 0.5
# 
# 
# #Data_test_xgb <- Data_test[1:960,]
# for(i in seq(480)){
#   set.seed(42)
#   Data_test_xgb <- test_impute_XGB[seq(480+i), ]
#   
#   
#   
#   nb_sample <- round((480+i-1) * prop_train)
#   sample_train <- sample(1:(480+i-1), nb_sample, replace = F)
# 
# train <- Data_test_xgb[sample_train,]
# 
# test <- Data_test_xgb[-sample_train,]
# test_row_pred <- nrow(Data_test_xgb)- nb_sample
# 
# 
# 
# trainm <- sparse.model.matrix(Points ~ .-1, data = train)
# head(trainm)
# train_label <- train$Points
# train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = unlist(train_label))
# 
# 
# 
# 
# testm <- sparse.model.matrix(Points ~ . , data = test)
# head(testm)
# test_label <- test$Points
# test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = unlist(test_label))
# 
# 
# 
# 
# params_xgb <- list(train= train_matrix, test = test_matrix)
# 
# 
# 
# # xgb_model1 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 500, max_depth =3, eval_metric = "rmse",
# #                         eta =0.1, watchlist = params_xgb, early_stopping_rounds = 30)
# # #Stopping. [42]	train-rmse:3.881881	test-rmse:7.039956
# # 
# # xgb_model2 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 800, max_depth =3, eval_metric = "rmse",
# #                         eta =0.1, watchlist = params_xgb, early_stopping_rounds = 30)
# # #Stopping. [42] train-rmse:3.881881	test-rmse:7.039957
# # 
# # 
# # 
# # xgb_model3 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 800, max_depth =6, eval_metric = "rmse",
# #                         eta =0.1, watchlist = params_xgb, early_stopping_rounds = 30)
# #Stopping. [41]	train-rmse:1.030049	test-rmse:7.901068
# 
# 
# 
# xgb_model4 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 800, max_depth =2, eval_metric = "rmse",
#                         eta =0.1, watchlist = params_xgb, early_stopping_rounds = 30)
# #Stopping. [74]	train-rmse:4.597352	test-rmse:6.885380
# # 
# # xgb_model5 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= 1000, max_depth =2, eval_metric = "rmse",
# #                         eta =0.05, watchlist = params_xgb, early_stopping_rounds = 200)
# #Stopping. Best iteration: [121]	train-rmse:5.015891	test-rmse:6.898094
# 
# Result_XGB[i,1] <- unlist(Data_test[(480 + i),3])
# 
# colnames(pred_matrix)[1] <- xgb_model4$feature_names[1]
# Result_XGB[i,2] <-  predict(xgb_model4, newdata = pred_matrix, class = "response")
# 
# 
# }



  
  
  
  
XGB_VALUE <- function(Data_Impute, prop_train = 0.5,  N_rounds= 800, Max_Depth = 2,LR_E = 0.1, ESR = 30){
  

  
  nb_sample <- round(nrow(Data_Impute) * prop_train)
  sample_train <- sample(seq(nrow(Data_Impute)-1), nb_sample, replace = F)
  
  train <- Data_Impute[sample_train,]
  
  test <- Data_Impute[-sample_train,]
  test_row_pred <- nrow(Data_Impute)- nb_sample
  
  
  
  trainm <- sparse.model.matrix(Points ~ .-1, data = train)

  train_label <- train$Points
  train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = unlist(train_label))
  
  
  testm <- sparse.model.matrix(Points ~ . , data = test)

  test_label <- test$Points
  test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = unlist(test_label))
  
  
  predm <- sparse.model.matrix(Points ~ . , data = test[test_row_pred, ])
  
  pred_label <- test[test_row_pred, ]$Points
  pred_matrix <- xgb.DMatrix(data = as.matrix(predm), label = unlist(pred_label))
  
  
  
  params_xgb <- list(train= train_matrix, test = test_matrix)
  
  
  xgb_model4 <- xgb.train(data = train_matrix, booster = "gbtree", nrounds= N_rounds, max_depth =Max_Depth, eval_metric = "rmse",
                          eta =LR_E, watchlist = params_xgb, early_stopping_rounds = ESR)

  

  
  colnames(pred_matrix)[1] <- xgb_model4$feature_names[1]
  Result_XGB <-  predict(xgb_model4, newdata = pred_matrix, class = "response")
  
  return(Result_XGB)
}


XGB_VALUE(data.imputed)






min_game <- 481
max_game <- 960
nb_model <- 6
ntry <- length(min_game:max_game)
Result <- matrix(numeric(ntry *nb_model), ncol =nb_model)
colnames(Result) <- c("Actual Value", "LM", "GLM POIS", "GLM NBIN","MissRF", "XgBoost")


Sample_test<- (min_game:max_game)
for(i in seq(ntry)){  
  
  Rm <- Sample_test[i]
  Data <- Data_test[1:Rm,]
  
  lm_mod <- lm(Points~., data = Data[-Rm,])
  glm_pois <- glm(Points~., data = Data[-Rm,], family = "poisson")
  glm_nbin <- glm.nb(Points~., data = Data[-Rm,])
  
  #glm_elasticnet <- glmnetUtils::cv.glmnet(Points~., data = Data[-Rm,], family = "poisson")
  
  Data[Rm,(3:12)] <- NA
  data.imputed <- missForest(Data)$ximp
  
  Result[i,1] <- unlist(Data_test[Rm,3])
  Result[i,2] <- unlist(predict(lm_mod, newdata = data.imputed[Rm,-3]))
  Result[i,3] <- unlist(predict(glm_pois, newdata = data.imputed[Rm,-3], type = "response"))
  Result[i,4] <- unlist(predict(glm_nbin, newdata = data.imputed[Rm,-3], type = "response"))
  Result[i,5] <- unlist(data.imputed[Rm,3])
  Result[i,6] <-mean(replicate(10,(XGB_VALUE(Data_Impute = data.imputed))))
  
  print(paste(round(i/ntry,3)*100, "%", sep = ""))
  
  
}
#Result <- readRDS("Resultats_2020_04_27")


Best_Model <- tabulate(sapply(seq(ntry), function(i) which.min(sapply(seq(nb_model-1), function(j) abs(Result[i,(j+1)]- Result[i,1])))), nb_model-1)
names(Best_Model) <- c("LM", "GLM POIS", "GLM NBIN", "MissRF", "XgBoost")
print(Best_Model)

par(mfrow =c(nb_model-1,2))

dif_abs <- do.call("rbind", lapply(seq(ntry), function(i) sapply(seq(nb_model-1), function(j) abs(Result[i,(j+1)]- Result[i,1]))))
dif <- do.call("rbind", lapply(seq(ntry), function(i) sapply(seq(nb_model-1), function(j) Result[i,(j+1)]- Result[i,1])))

#saveRDS(Result, file = "Resultats_2020_04_27")
par(mfrow = c(1,1))

#LM #########################################################
hist(dif[,1], density = 60, main = "Distribution résidus modèle linéaire (prédit - observé)", xlab = "Points de différence", ylab = "Fréquence", col = "red", breaks = 12)
plot(dif[,1], main = "graphique résidus modèle linéaire (prédit - observé)", xlab = "Parties", ylab = "Différence")
abline(h = 0, col = "blue", lw = 2)


hist(dif[,2], density = 60, main = "Distribution résidus GLM Poisson (prédit - observé)", xlab = "Points de différence", ylab = "Fréquence", col = "red", breaks = 12)
plot(dif[,2], main = "graphique résidus modèle GLM Poisson (prédit - observé)", xlab = "Parties", ylab = "Différence") 
abline(h = 0, col = "blue", lw = 2)


hist(dif[,3], density = 60, main = "Distribution résidus GLM Binomiale négative (prédit - observé)", xlab = "Points de différence", ylab = "Fréquence", col = "red", breaks = 12)
plot(dif[,3], main = "graphique résidus modèle GLM Binomiale négative (prédit - observé)", xlab = "Parties", ylab = "Différence") 
abline(h = 0, col = "blue", lw = 2)


hist(dif[,4], density = 60, main = "Distribution résidus Random Forest (prédit - observé)", xlab = "Points de différence", ylab = "Fréquence", col = "red", breaks = 12)
plot(dif[,4], main = "graphique résidus modèle Random Forest (prédit - observé)", xlab = "Parties", ylab = "Différence") 
abline(h = 0, col = "blue", lw = 2)


hist(dif[,5], density = 60, main = "Distribution résidus XgBoost (prédit - observé)", xlab = "Points de différence", ylab = "Fréquence", col = "red", breaks = 12)
plot(dif[,5], main = "graphique résidus modèle XgBoost (prédit - observé)", xlab = "Parties", ylab = "Différence") 
abline(h = 0, col = "blue", lw = 2)

sum(dif[,1]^2)/(ntry -1)
sum(dif[,2]^2)/(ntry -1)
sum(dif[,3]^2)/(ntry -1)
sum(dif[,4]^2)/(ntry -1)
sum(dif[,5]^2)/(ntry -1)


mean(abs(dif[,1]))
mean(abs(dif[,2]))
mean(abs(dif[,3]))
mean(abs(dif[,4]))
mean(abs(dif[,5]))


sd(dif[,1])
sd(dif[,2])
sd(dif[,3])
sd(dif[,4])
sd(dif[,5])
