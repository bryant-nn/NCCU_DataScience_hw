
#input
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
  stop("USAGE: Rscript hw2_108703030.R fail", call.=FALSE)
}

# parse parameters
i <- 1 
while(i < length(args))
{
  if(args[i] == "--target"){
    pst<-args[i+1]
    i<-i+1
  }else if(args[i] == "--input"){
    j<-grep("-", c(args[(i+1):length(args)], "-"))[1]
    files<-args[(i+1):(i+j-1)]
    i<-i+j-1
  }else if(args[i] == "--badthre"){
    threshold <- args[i+1]
    i <- i+1
  }else if(args[i] == "--output"){
    out_f<-args[i+1]
    i<-i+1
  }else{
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}
if(pst == "good"){
  ngt <- "bad"
}else{
  ngt <- "good"
}
# pst -> good/bad 
# threshold -> 0.6 ...之類的
# j-1 -> input num
# files[1] -> input path
# out_f -> output path

newdf <- data.frame()
for(file in files)
{
  name<-gsub(".csv", "", basename(file))
  d<-read.table(file, header=T,sep=",")
  tab <- table(factor(d$reference , levels = c(pst,ngt)),factor(d$pred.score > threshold , levels = c(pst == "bad",pst == "good") ) )
  

  sen <- tab[1,1]/(tab[1,1]+tab[1,2])
  spe <- tab[2,2]/(tab[2,1]+tab[2,2])
  pre <- tab[1,1]/(tab[1,1]+tab[2,1])
  f1 <- round(2*pre*sen/(pre+sen) , 2)
  
  # LOG LIKELIHOOD
  likeli_model <- sum(ifelse(d$reference=="bad", log(d$pred.score), log(1-d$pred.score)))
  
  #null module's likelihood
  pNull <- sum(ifelse(d$reference=='bad',1,0))/dim(d)[[1]]
  likeli_nullModel <- sum(ifelse(d$reference=='bad',1,0))*log(pNull) + sum(ifelse(d$reference=='bad',0,1))*log(1-pNull)
  
  #pseudo R-squared
  pseudo_R_squard <- 1-(likeli_model/likeli_nullModel)
  
  df <- data.frame(name,
                   round(sen,digits=2),
                   round(spe,digits=2),
                   round(f1,digits=2),
                   round(likeli_model,2),
                   round(pseudo_R_squard,2)
                   )
  names(df) <- c("method","sensitivity","specificity","F1","logLikelihood","pseudoRsquared")
  

  newdf <- rbind(newdf,df)
}

#find max method name
m1 <- which.max(newdf$sensitivity)
m2 <- which.max(newdf$specificity)
m3 <- which.max(newdf$F1)
m4 <- which.max(newdf$logLikelihood)
m5 <- which.max(newdf$pseudoRsquared)

# calculate final data frame & merge 
last_df <- data.frame("max",newdf[m1,1],newdf[m2,1],newdf[m3,1],newdf[m4,1],newdf[m5,1])
names(last_df) <- c("method","sensitivity","specificity","F1","logLikelihood","pseudoRsquared")
last_df <- rbind(newdf,last_df)

# make dir
out_file <- basename(out_f)
out_path <- gsub(out_file,"",out_f)
dir.create(out_path, recursive = TRUE ,showWarnings = FALSE)

# write file
if(out_path != "")
  setwd(out_path)
write.table(last_df, file=out_file, row.names = F, quote = F,sep = ",")

#print(last_df)