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