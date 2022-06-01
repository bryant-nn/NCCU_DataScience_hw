url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv'
redwine <- read.csv(url,sep = ';')
url1 <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv'
whitewine <- read.csv(url1,sep = ';')
data <- rbind(redwine,whitewine)
#训练集、测试集划分
set.seed(17)  
index <-  which( (1:nrow(data))%%3 == 0 )
train <- data[-index,]
test <- data[index,]


library("xgboost")
library("Matrix")
train_matrix <- sparse.model.matrix(quality ~ .-1, data = train)
test_matrix <- sparse.model.matrix(quality ~ .-1, data = test)

train_label <- as.numeric(train$quality>6)
test_label <-  as.numeric(test$quality>6)


train_fin <- list(data=train_matrix,label=train_label) 
test_fin <- list(data=test_matrix,label=test_label) 

dtrain <- xgb.DMatrix(data = train_fin$data, label = train_fin$label) 
dtest <- xgb.DMatrix(data = test_fin$data, label = test_fin$label)

#模型训练
xgb <- xgboost(data = dtrain,max_depth=6, eta=0.5,  
               objective='binary:logistic', nround=25)
#重要重要性排序 
importance <- xgb.importance(train_matrix@Dimnames[[2]], model = xgb)  
head(importance)
xgb.ggplot.importance(importance)