
library('rpart')
library('caret')
packageName <- "randomForest"
if(!(packageName %in% rownames(installed.packages()))) {
  install.packages("randomForest")
}
library("randomForest")

#input
args = commandArgs(trailingOnly=TRUE)

# parse parameters
i <- 1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    k <- args[i+1]
    i <- i + 1
  }else if(args[i] == "--train"){
    train_path <- args[i+1]
    i <- i+1
  }else if(args[i] == "--test"){
    test_path <- args[i+1]
    i <- i+1
  }else if(args[i] == "--report"){
    report_path <- args[i+1]
    i <- i+1
  }else if(args[i] == "--predict"){
    predict_path <- args[i+1]
    i <- i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}
in_d <- read.csv(train_path)
test_data <- read.csv(test_path)

#preprocess
#null age = ( average age <- 29.7 )
for(i in c(1:891) ){
  if( is.na(in_d[i,6]) ){
    in_d[i,6] = 29.7
  }
}
for(i in c(1:418) ){
  if( is.na(test_data[i,5]) ){
    test_data[i,5] = 28
  }
}

temp <- test_data$PassengerId
#delete useless data
in_d = in_d[,!colnames(in_d) %in% c('PassengerId','Name','SibSp', 'Parch','Cabin', 'Ticket','Embarked')]
test_data = test_data[,!colnames(test_data) %in% c('PassengerId','Name','SibSp', 'Parch','Cabin', 'Ticket','Embarked')]
test_data[153,4] = 44
#save the path now
original_path = getwd()

i <- 1
k <- as.numeric(k)
t <- 891/k
t <- floor(t)

newdf <- data.frame()
tem <- c(1:891)

set.seed(120)
max <- 0

in_d$Survived <- as.character(in_d$Survived)
in_d$Survived <- as.factor(in_d$Survived)
while(i<=k){
  
  a <- sample(tem,891,replace = F)
  
  test_d <- in_d[a[1:t],]
  val_d <- in_d[a[(t+1):(2*t)],]
  train_d <- in_d[a[((2*t)+1):891],]

  # model using decision tree
  model <- rpart(Survived ~ Pclass + Sex + Age + Fare ,
                 data=train_d, control=rpart.control(maxdepth=15),
                 method="class" )
  
  
  model <- randomForest(Survived ~ Pclass + Sex + Age + Fare ,
                        data = train_d, na.action = na.omit)
  #print(model:train_d)
  resultframe <- data.frame(truth=train_d$Survived,
                            pred=predict(model,newdata=train_d, type="class"))
  rtab <- table(resultframe)
  train_a <- round((sum(diag(rtab))) / (sum(rtab)), 2)
  
  #print(model:val_d)
  resultframe <- data.frame(truth=val_d$Survived,
                            pred=predict(model,newdata=val_d, type="class"))
  rtab <- table(resultframe)
  
  val_a <- round((sum(diag(rtab))) / (sum(rtab)),2)
  
  
  #print(model:test_d)
  resultframe <- data.frame(truth=test_d$Survived,
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

#make report dir
out_file <- basename(report_path)
out_path <- gsub(out_file,"",report_path)
dir.create(out_path, recursive = TRUE ,showWarnings = FALSE)

# write report file
if(out_path != "")
  setwd(out_path)
write.table(lastdf, file=out_file, row.names = F, quote = F,sep = ",")
setwd(original_path)

#make  predict data frame
pred=predict(final_model,newdata=test_data, type="class")
predict = data.frame(PassengerID = temp, 
                  Survived = pred)

#make predict dir
out_file <- basename(predict_path)
out_path <- gsub(out_file,"",predict_path)
dir.create(out_path, recursive = TRUE ,showWarnings = FALSE)


#write predict file
if(out_path != "")
  setwd(out_path)
write.table(predict, file=out_file, row.names = F, quote = F,sep = ",")