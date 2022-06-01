#install.packages("ROCR", repos = "http://cran.us.r-project.org")

library('rpart')
library('ROCR')
library('mgcv')

# function mode 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

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

#save the path now
original_path = getwd()

#delete the first column
in_d = in_d[,-1]
test_data = test_data[,-1]

#delete null variable
#in_d$MonthlyIncome <- NULL
#in_d$NumberOfDependents <- NULL
#test_data$MonthlyIncome <- NULL
#test_data$NumberOfDependents <- NULL

#input null value
in_d$MonthlyIncome[is.na(in_d$MonthlyIncome)] <- mean(in_d$MonthlyIncome, na.rm = T) 
in_d$NumberOfDependents[is.na(in_d$NumberOfDependents)] <- getmode(in_d$NumberOfDependents)

test_data$MonthlyIncome[is.na(test_data$MonthlyIncome)] <- mean(test_data$MonthlyIncome, na.rm = T) 
test_data$NumberOfDependents[is.na(test_data$NumberOfDependents)] <- getmode(test_data$NumberOfDependents)

# Corelation between target variable and other variable 
#cor(x = in_d$SeriousDlqin2yrs, y = in_d)

# Encoding the target feature as factor
#in_d$SeriousDlqin2yrs = factor(in_d$SeriousDlqin2yrs, levels = c(0,1))

# Feature Scaling
in_d[-1] = scale(in_d[-1])
test_data[-1] = scale(test_data[-1])

i <- 1
k <- as.numeric(k)
t <- 15000/k
t <- floor(t)

newdf <- data.frame()
tem <- c(1:15000)

set.seed(215)
max <- 0


while(i<=k){
  
  a <- sample(tem,15000,replace = F)
  
  test_d <- in_d[a[1:t],]
  val_d <- in_d[a[(t+1):(2*t)],]
  train_d <- in_d[a[((2*t)+1):15000],]
  
  # model using glm
  model <- glm(formula = train_d$SeriousDlqin2yrs ~ . ,
                  family = binomial(link='probit'),
                  epsilon = 1e-14,
                  data = train_d)
  #print(summary(model))
  
  
 
  
#  model <- randomForest(Survived ~ Pclass + Sex + Age + Fare ,
#                        data = train_d, na.action = na.omit)
  
  #print(model:train_d)
  pred = predict(model,newdata=train_d[-1], type="response")
                         
  #print(table(y = train_d$SeriousDlqin2yrs, glmPred = pred > 0.5))
  
  # compute auc 
  eval <- prediction(pred,train_d$SeriousDlqin2yrs)
  train_auc <- round(attributes(performance(eval,'auc'))$y.values[[1]], 2)
 
  
  
  # print(model:val_d)
  pred = predict(model,newdata=val_d[-1], type="response")
  
  # compute auc 
  eval <- prediction(pred,val_d$SeriousDlqin2yrs)
  val_auc <- round(attributes(performance(eval,'auc'))$y.values[[1]], 2)
  
  
  
  #print(model:test_d)
  pred = predict(model,newdata=test_d[-1], type="response")
  
  # compute auc 
  eval <- prediction(pred,test_d$SeriousDlqin2yrs)
  test_auc <- round(attributes(performance(eval,'auc'))$y.values[[1]], 2)
  
  
  f <- "fold"
  e <- paste(f,i,sep='')
  df <- data.frame(e,train_auc, val_auc, test_auc)
  names(df) <-c("set","training","validation","test")
  newdf <- rbind(newdf,df)
  
  #store the best model
  if( (val_auc + test_auc) > max){
    max = (val_auc + test_auc)
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
pred=predict(final_model,newdata=test_data, type="response")
a <- c(1:101503)
predict = data.frame(Id = a,Probability = pred)

#make predict dir
out_file <- basename(predict_path)
out_path <- gsub(out_file,"",predict_path)
dir.create(out_path, recursive = TRUE ,showWarnings = FALSE)


#write predict file
if(out_path != "")
  setwd(out_path)
write.table(predict, file=out_file, row.names = F, quote = F,sep = ",")