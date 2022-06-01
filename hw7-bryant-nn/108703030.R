library('rpart')
library("randomForest")

#input
args = commandArgs(trailingOnly=TRUE)

in_d <- read.csv('hw7_train.csv')
test_data <- read.csv('hw7_test.csv')


# no null in train data
#for(i in c(1:20)){
  
#  a <- paste('feature',i)
#  print(i)
  
#  print(sum(is.na(test_data$a)))
#}

# see corelation 
#cor(x = in_d$label, y = in_d)

#remove low corelation column
in_d = in_d[,!colnames(in_d) %in% c('feature8','feature12','feature7','feature9','feature11','feature13','feature17','feature19','feature20')]

#delete the first column
in_d = in_d[,-1]
test_data = test_data[,-1]
#print(dim(in_d))



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
  val_d <- in_d[a[(t+1):(2*t)],]
  train_d <- in_d[a[((2*t)+1):8346],]
  
  
  # model using decision tree
  model <- rpart(formula = train_d$label ~ . ,
                 data=train_d, control=rpart.control(maxdepth=5),
                 method="class" )
  
  
  
  #print(model:train_d)
  resultframe <- data.frame(truth=train_d$label,
                            pred=predict(model,newdata=train_d, type="class"))
  rtab <- table(resultframe)
  train_a <- round((sum(diag(rtab))) / (sum(rtab)), 2)
  
  #print(model:val_d)
  resultframe <- data.frame(truth=val_d$label,
                            pred=predict(model,newdata=val_d, type="class"))
  rtab <- table(resultframe)
  
  val_a <- round((sum(diag(rtab))) / (sum(rtab)),2)
  
  
  #print(model:test_d)
  resultframe <- data.frame(truth=test_d$label,
                            pred=predict(model,newdata=test_d, type="class"))
  rtab <- table(resultframe)
  test_a <- round((sum(diag(rtab))) / (sum(rtab)),2)
  
  f <- "fold"
  e <- paste(f,i,sep='')
  df <- data.frame(e,train_a,val_a,test_a)
  names(df) <-c("set","training","validation","test")
  newdf <- rbind(newdf,df)
  
  #store the best model
  if( (val_a + test_a) > max){
    max = (val_a + test_a)
    final_model <- model
  }
  
  i <- i+1
}

#dataframe + last row (calculate average)
lastdf <- data.frame("ave.",round( mean(newdf[,2]) , 2),round(mean(newdf[,3]),2),round(mean(newdf[,4]),2))
names(lastdf) <- c("set","training","validation","test")
lastdf <- rbind(newdf,lastdf)

write.table(lastdf, file = 'score.csv', row.names = F, quote = F,sep = ",")


#make  predict data frame
pred=predict(final_model,newdata=test_data, type="class")
a <- c(0:2086)
predict = data.frame(id = a,label = pred)
write.table(predict, file='output.csv', row.names = F, quote = F,sep = ",")
