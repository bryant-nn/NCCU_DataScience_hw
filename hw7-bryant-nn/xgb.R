
#install.packages("xgboost",repos = "http://cran.us.r-project.org")
library("xgboost")
library("Matrix")
library(dplyr)

#input
args = commandArgs(trailingOnly=TRUE)

in_d <- read.csv('hw7_train.csv')
test_data <- read.csv('hw7_test.csv')

#delete the first column
in_d = in_d[,-1]
test_data = test_data[,-1]
#print(colnames(in_d))

input_d <- in_d[,-1]
var <- colnames(input_d)
#print(colnames(input_d))

i <- 1
k <- 10
t <- 8346/k
t <- floor(t)

newdf <- data.frame()
tem <- c(1:8346)

set.seed(2123)
max <- 0

while(i<=k){
  
  a <- sample(tem,8346,replace = F)
  
  test_d <- in_d[a[1:t],]
  #val_d <- in_d[a[(t+1):(2*t)],]
  train_d <- in_d[a[(t+1):8346],]
  
  ## Train Model
  
  
  train_matrix <- sparse.model.matrix(label ~ .-1, data = train_d)
  test_matrix <- sparse.model.matrix(label ~ .-1, data = test_d)
  
  train_label <- as.numeric(train_d$label>0)
  test_label <- as.numeric(test_d$label>0)
  
  train_fin <- list(data=train_matrix,label=train_label)
  test_fin <- list(data=test_matrix,label=test_label)
  
  dtrain <- xgb.DMatrix(data = train_fin$data, label = train_fin$label)
  dtest <- xgb.DMatrix(data = test_fin$data, label = test_fin$label)
  
  #模型训练
  xgb <- xgboost(data = dtrain,max_depth=6, eta=1,  
                 objective='binary:logistic',
                 nthread = 2,
  
  #重要重要性排序 
  importance <- xgb.importance(train_matrix@Dimnames[[2]], model = xgb)  
  
  
  #print(model:train_d)
  pre_xgb = round(predict(xgb,newdata = dtrain))
  
  resultframe <- data.frame(truth=train_d$label,
                            pred=pre_xgb)
  resultframe <- mutate(resultframe, pred = ifelse(pred > 0.5, 1, -1))
  rtab <- table(resultframe)
  
  train_a <- round((sum(diag(rtab))) / (sum(rtab)),2)
  #print(model:val_d)
  
  
  
  #print(model:test_d)
  pre_xgb = round(predict(xgb,newdata = dtest))
  resultframe <- data.frame(truth=test_d$label,
                            pred=pre_xgb)
  resultframe <- mutate(resultframe, pred = ifelse(pred > 0.5, 1, -1))
  rtab <- table(resultframe)
  
  test_a <- round((sum(diag(rtab))) / (sum(rtab)),2)
  
  f <- "fold"
  e <- paste(f,i,sep='')
  df <- data.frame(e,train_a,test_a)
  names(df) <-c("set","training","test")
  newdf <- rbind(newdf,df)
  
  #store the best model
  if( ( test_a) > max){
    max = ( test_a)
    final_model <- xgb
  }
  
  i <- i+1
}

#dataframe + last row (calculate average)
lastdf <- data.frame("ave.",round( mean(newdf[,2]) , 2),round(mean(newdf[,3]),2))
names(lastdf) <- c("set","training","test")
lastdf <- rbind(newdf,lastdf)

write.table(lastdf, file = 'scoreXGB.csv', row.names = F, quote = F,sep = ",")


#make  predict data frame
#pred=predict(final_model,newdata=test_data, type="response")
#a <- c(0:2086)
#predict = data.frame(id = a,label = pred)
#predict <- mutate(predict, label = ifelse(label > 0.5, 1, -1))
#write.table(predict, file='outputXGB.csv', row.names = F, quote = F,sep = ",")

