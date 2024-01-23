library(readr)
library(tidyverse)
library(caret)
library(mlbench)
library(MLmetrics)

churn <-read.csv("churn.csv")

#1.Split data 
TTS <- function(data, size = 0.8) {
  set.seed(19)
  ## change label to factor
  churn$churn <- as.factor(churn$churn)
  n <- nrow(data)
  train_id <- sample(1:n, size * n)
  train_df <- data[train_id, ]
  test_df <- data[-train_id, ]
  return(list(train_df, test_df))
}

prep_churn <- TTS(churn, size = 0.8)
prep_churn[[1]] # train data
prep_churn[[2]] # test data 


#2.train data .
ctrl <- trainControl(method = "cv",
                     number = 5)

model <- train(churn~internationalplan + voicemailplan
               + totaldaycharge + numbercustomerservicecalls, # 4 predictorตัวแปรต้น
               data = prep_churn[[1]],
               method = "glm",#logistic regression model.
               trControl = ctrl)
              


#3 score 
#'pred_churn' is a character vector
# Convert 'pred_churn' to factor with levels matching 'prep_churn[[2]]$churn'

pred_chrun <- predict(model,newdata = prep_churn[[2]])


#4 evaluate 
conf_matrix <- confusionMatrix(pred_churn, factor(prep_churn[[2]]$churn))
print(conf_matrix)
print(model)










  
