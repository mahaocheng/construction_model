# TODO: Add comment
# 
# Author: lenovo
###############################################################################

dataset = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
                   sep = ",", header = F, na.strings = "?")
head(dataset)
## write.table(dataset, "C://Users//lenovo//workspace//construction_model//creditcard.csv", sep=",")
sapply(dataset, function(x) sum(is.na(x)))
sapply(dataset, class)
set.seed(123)
dataset <- na.omit(dataset)
n <- dim(dataset)[1]
index <- sample(n, round(0.7 * n))
train <- dataset[index, ]
test <- dataset[-index, ]
dim(train)
dim(test)
dataset2 <- dataset
library(plyr) 
library(dplyr)
IntoFactor <- function(x) { 
  if (class(x) == "factor") {
    n <- length(x)
    data.fac <- data.frame(x = x, y = 1 : n)
    output <- model.matrix(y ~ x, data.fac)[, -1]
    # Convert factor into dummy variable matrix
  } else {
    output <- x
    # if x is numeric, output is x
  }
  return(output)
}
IntoFactor(dataset$V4)[1:5, ]

dataset2 <- colwise(IntoFactor)(dataset2)
dataset2 <- do.call(cbind, dataset2)
dataset2 <- as.data.frame(dataset2)
head(dataset2)
dim(dataset2)
logit.model <- glm(V16 ~ . , data = train, family = "binomial")
logit.response <- predict(logit.model, test, type = "response")
logit.predict <- ifelse(logit.response > 0.5, "+", "-")
table(logit.predict, test$V16)
logit.predict
accurancy1 <- mean(logit.predict == test$V16)
accurancy1
library(caret)

ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
set.seed(300)
m_gbm <- train(V16 ~ . , data=train, method = "gbm",  metric = "Kappa", trControl = ctrl)

gbm.predict <- predict(m_gbm, test)
table(gbm.predict, test$V16)
accurancy2 <- mean(gbm.predict == test$V16)
accurancy2
##¡¡devtools::install_github('dmlc/xgboost',subdir='R-package')
require(xgboost)
require(methods)
require(plyr)
set.seed(123)
index <- sample(n, round(0.7 * n))
train.xg <- dataset2[index, ]
test.xg <- dataset2[-index, ]
label <- as.matrix(train.xg[, 38, drop = F])
data <- as.matrix(train.xg[, -38, drop = F])
data2 <- as.matrix(test.xg[, -38, drop = F])
label2 <- as.matrix(test.xg[, 38, drop = F])
# weight <- as.numeric(dtrain[[32]]) * testsize / length(label)
xgmat <- xgb.DMatrix(data, label = label, missing = -10000)
param <- list("objective" = "binary:logistic", "bst:eta" = 1, 
    "bst:max_depth" = 2, "eval_metric" = "logloss", "silent" = 1,
    "nthread" = 16, "min_child_weight" = 1.45)
nround <- 275
bst <- xgb.train(param, xgmat, nround )
res1 <- predict(bst, data2)
pre1 <- ifelse(res1 > 0.5, 1, 0)
table(pre1, label2)
# weight <- as.numeric(dtrain[[32]]) * testsize / length(label)
xgmat <- xgb.DMatrix(data, label = label, missing = -10000)
param <- list("objective" = "binary:logistic", "bst:eta" = 1, 
    "bst:max_depth" = 2, "eval_metric" = "logloss", "silent" = 1, 
    "nthread" = 16, "min_child_weight" = 1.45)
nround <- 275
bst <- xgb.train(param, xgmat, nround )
res1 <- predict(bst, data2)
pre1 <- ifelse(res1 > 0.5, 1, 0)
table(pre1, label2)
accurancy4 <- mean(pre1 == label2)
accurancy4 