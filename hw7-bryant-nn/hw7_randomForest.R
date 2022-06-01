library("randomForest")
library(dplyr)
#input
args = commandArgs(trailingOnly=TRUE)

in_d <- read.csv('hw7_train.csv')
test_data <- read.csv('hw7_test.csv')

#remove low corelation column
#in_d = in_d[,!colnames(in_d) %in% c('feature8','feature12')]

#delete the first column
in_d = in_d[,-1]
test_data = test_data[,-1]
#print(dim(in_d))

# -------------------
#set.seed(5123512)
input_d <- in_d[,-1]
var <- colnames(input_d)

# -------------------



i <- 1
k <- 10
t <- 8346/k
t <- floor(t)

newdf <- data.frame()
tem <- c(1:8346)

set.seed(2123)
max <- 0

#------
in_d <- mutate(in_d, label = ifelse(label > 0.1, 1, 0))

while(i<=k){
  
  a <- sample(tem,8346,replace = F)
  
  test_d <- in_d[a[1:t],]
  #val_d <- in_d[a[(t+1):(2*t)],]
  train_d <- in_d[a[(t+1):8346],]
  
  
  # model using random forest tree
  
  #model <- randomForest(x = train_d[,var], y = as.factor(train_d$label),
   #                      ntree = 90,  importance = T)
  
  #model <- randomForest(x = train_d[,var], y = (train_d$label),
  #                      ntree = 90,  importance = T)
  
  # model using glm
  model <- glm(formula = label ~ . ,
               family = binomial(link='logit'),
               epsilon = 1e-14,
               data = train_d)
  
  #print(model:train_d)
  resultframe <- data.frame(truth=train_d$label,
                            pred=predict(model, newdata = train_d))
  resultframe <- mutate(resultframe, pred = ifelse(pred > 0.5, 1, -1))
  
  rtab <- table(resultframe)
  train_a <- round((sum(diag(rtab))) / (sum(rtab)), 2)
  
  #print(model:val_d)
  
  
  
  #print(model:test_d)
  resultframe <- data.frame(truth=test_d$label,
                            pred=predict(model,newdata=test_d))
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
    final_model <- model
  }
  
  i <- i+1
}

#dataframe + last row (calculate average)
lastdf <- data.frame("ave.",round( mean(newdf[,2]) , 2),round(mean(newdf[,3]),2))
names(lastdf) <- c("set","training","test")
lastdf <- rbind(newdf,lastdf)

write.table(lastdf, file = 'scoreRF.csv', row.names = F, quote = F,sep = ",")


#make  predict data frame
pred=predict(final_model,newdata=test_data, type="response")
a <- c(0:2086)
predict = data.frame(id = a,label = pred)
predict <- mutate(predict, label = ifelse(label > 0.5, 1, -1))
write.table(predict, file='outputRF.csv', row.names = F, quote = F,sep = ",")
