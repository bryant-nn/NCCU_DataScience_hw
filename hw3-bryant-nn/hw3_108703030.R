install.packages("rpart")
library('rpart')
install.packages("caret",repos = "http://cran.us.r-project.org")
library('caret')
#input
args = commandArgs(trailingOnly=TRUE)

if(length(args) == 0){
  stop("USAGE: Rscript hw3_108703030.R input fail", call.=FALSE)
}

# parse parameters
i <- 1 
while(i < length(args))
{
  if(args[i] == "--fold"){
    k <- args[i+1]
    i <- i + 1
  }else if(args[i] == "--input"){
    in_path <- args[i+1]
    i <- i+1
  }else if(args[i] == "--output"){
    out_f <- args[i+1]
    i <- i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

#check in_path
if(file.exists(in_path) == FALSE){
  print("Input doesn't exist !")
}else{
#read input data
d <- read.csv(in_path, header = F)

#decrease dimension
d1 <- d[,seq(3,5602,1)]

index <-  findCorrelation(d1,cutoff=0.0002,verbose=F,names=T)

d1=d[,!colnames(d) %in% index]
d1=d1[,!colnames(d1) %in% c('V1','V5603')]
print(dim(d1))
i <- 1
k <- as.numeric(k)
t <- 805/k
t <- floor(t)

newdf <- data.frame()
tem <- c(1:805)
set.seed(2123)
a <- sample(tem,805,replace = F)

while(i<=k){
  test_d <- d1[a[(((i-1)*t)+1):(t*i)],]
  val_d <- d1[a[((i*t)+1):((i+1)*t)],]
  train_d <- d1[c(a['1':((i-1)*t)],a[(((i+1)*t)+1):'805']),]
  if(i == 1){
    train_d <- d1[a[(2*t+1):'805'],]
    test_d <- d1[a['1':t],]
    val_d <- d1[a[(t+1):(2*t)],]
  }
  if(i == k-1){
    train_d <- d1[a['1':((i-1)*t)],]
  }
  if(i == k){
    test_d <- d1[a[(((i-1)*t)+1):'805'],]
    val_d <- d1[a['1':t],]
    train_d <- d1[a[(t+1):((i-1)*t)],]
  }
  
  # model using decision tree
  model <- rpart(V2 ~ .,
                 data=train_d, control=rpart.control(maxdepth=5),
                 method="class")
  
  #print(model:train_d)
  resultframe <- data.frame(truth=train_d$V2,
                            pred=predict(model,newdata=train_d, type="class"))
  rtab <- table(resultframe)
  train_a <- round((sum(diag(rtab))) / (sum(rtab)), 2)
  
  #print(model:val_d)
  resultframe <- data.frame(truth=val_d$V2,
                            pred=predict(model,newdata=val_d, type="class"))
  rtab <- table(resultframe)
  val_a <- round((sum(diag(rtab))) / (sum(rtab)),2)
  
  
  #print(model:test_d)
  resultframe <- data.frame(truth=test_d$V2,
                            pred=predict(model,newdata=test_d, type="class"))
  rtab <- table(resultframe)
  test_a <- round((sum(diag(rtab))) / (sum(rtab)),2)
  
  f <- "fold"
  e <- paste(f,i,sep='')
  df <- data.frame(e,train_a,val_a,test_a)
  names(df) <-c("set","training","validation","test")
  newdf <- rbind(newdf,df)
  
  i <- i+1
}

#dataframe + last row (calculate average)
lastdf <- data.frame("ave.",round( mean(newdf[,2]) , 2),round(mean(newdf[,3]),2),round(mean(newdf[,4]),2))
names(lastdf) <- c("set","training","validation","test")
lastdf <- rbind(newdf,lastdf)

#make dir
out_file <- basename(out_f)
out_path <- gsub(out_file,"",out_f)
dir.create(out_path, recursive = TRUE ,showWarnings = FALSE)
  
# write file
if(out_path != "")
  setwd(out_path)
write.table(lastdf, file=out_file, row.names = F, quote = F,sep = ",")
}

