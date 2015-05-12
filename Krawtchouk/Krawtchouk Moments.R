library(jpeg)
library(IM)
library(randomForest)
library(sampling)
library(klaR)

################################################################
### Set working directory
setwd("/Users/nicholashockensmith/Desktop/Big Data/Project 2/train")
train.class.folders = substring(list.dirs(),3)[-1]
train.data = NULL
response = NULL

### Read in Training Data
for(i in 1:length(train.class.folders)) {
  setwd(paste("/Users/nicholashockensmith/Desktop/Big Data/Project 2/train/", train.class.folders[i], sep = ""))
  data.file.names = list.files()
  train.data = c(train.data, sapply(data.file.names, function(x) {readJPEG(x, native=T)}))
  response = c(response, rep(train.class.folders[i], length(data.file.names)))
}

### Calculates Image Moments via the IM package
## See http://cran.r-project.org/web/packages/IM/IM.pdf for 
## alternative moment kernels! 
N<-9
train.moment = matrix(NA,N*N,length(train.data))
for(i in 1:length(train.data)) { 
#   #--- Zeros and Ones ---#
#   temp.train<-log(-1*train.data[[i]][])
#   temp.train[temp.train<1]<-0
#   temp.train[temp.train>1]<-1
#   train.moment[,i] = as.vector(momentObj(temp.train, type="krawt", order=N+1, 0.5)@moments[1:N,1:N])
  #--- Training Set ---#
  train.moment[,i] = as.vector(momentObj(log(-1*train.data[[i]][]), type="krawt", order=N+1, 0.5)@moments[1:N,1:N])
}
train.moment <- t(train.moment)
dim(train.moment)

setwd("/Users/nicholashockensmith/Desktop/Big Data/Project 2/test")
test.data = NULL
data.file.names.test = list.files()
test.data = sapply(data.file.names.test, function(x) {readJPEG(x, native=T)})
#-- ii. More Image Moments via the IM package
test.moment = matrix(NA,N*N,length(test.data))
for(i in 1:length(test.data)) { 
  test.moment[,i] = as.vector(momentObj(log(-1*test.data[[i]][]), type="krawt", order=N+1, 0.5)@moments[1:N,1:N])
}
test.moment<-t(test.moment)
dim(test.moment)

### Kevin's Features! ###
binfeat.train <- read.csv("~/Desktop/Big Data Repository/ST599_Project2/ST599_Project2/BinCountDataSet/bincount_train.txt")
binfeat.test <- read.csv("~/Desktop/Big Data Repository/ST599_Project2/ST599_Project2/BinCountDataSet/bincount_test.txt")
kevfeat.train<-binfeat.train[,-11]
kevfeat.test<-binfeat.test[,-11]
ardvark.train<-cbind(train.moment,kevfeat.train)
ardvark.test<-cbind(test.moment,kevfeat.test)

################################################################
###---Run in the case you want to measure accuracy in the
###---training set! Otherwise, skip ahead to the 
###---Monopoly reference!
## 2a. Random Forest on trainging set
# Random Sample for Training and Validation Sets
z<-sample(1:30336,20000)
# trainging set
t.t<-train.moment[z,]
t.v<-train.moment[-z,]
# Validation set
r.t<-response[z]
r.v<-response[-z]
# Training and Validation Data Frames
train<-data.frame(t.t)
#--- k=1: Random Forest
#--- k=2: Naive Bayes
k=1
if(k==1){
  # Random Forrest #
  krawt.rf.p<-randomForest(x=t.t,y=as.factor(r.t),ntree=500)
  ## 2b. Correct Classification from Prediction Function
  pred.krawt.rf.p<-predict(krawt.rf.p,newdata=t.v)
  tabby.krawt<-table(observed=r.v,predicted=pred.krawt.rf.p)
  sum(diag(tabby.krawt))/sum(tabby.krawt)
  ## 3. Write test .CSV file for Kaggle Submission
  setwd("/Users/nicholashockensmith/Desktop/Big Data/Project 2")
  write.csv(cbind(r.v,predict(krawt.rf.p,newdata=t.v,type="prob")),file="krawt.rf.p.csv")
  }else{
    # Naive Bayes #
    nbay<-NaiveBayes(t.t,as.factor(r.t))
    ## 2b. Correct Classification from Prediction Function
    pred.nbay<-predict(nbay,newdata=t.v)
    ## 3. Write test .CSV file for Kaggle Submission
    setwd("/Users/nicholashockensmith/Desktop/Big Data/Project 2")
    write.csv(pred.nbay,file="nbay.csv")   
  }
}
################################################################


################################################################ 
###---Here-in lie the Monopoly reference!
### Random Forest! Do pass go, do collect $100! Time for Kaggle!
### Naive Bayes! Do pass go, do collect $100! Time for Kaggle!
setwd("/Users/nicholashockensmith/Desktop/Big Data/Project 2")
#--- k=1: Random Forest
#--- k=2: Naive Bayes
#--- k=3: Combined Features
k=3
if(k==1){
  ## 1a. Random Forest on Whole Data Set
  krawt.rf<-randomForest(x=train.moment,y=as.factor(response),ntree=500)
  ## 1b. Correct Classification/ Confusion Matrix/ OOB
  confucius<-krawt.rf$confusion
  sum(diag(confucius))/sum(confucius)
  ## 2. Write the prediction model to .CSV 3. Write test .CSV file for Kaggle Submission
  write.csv(cbind(data.file.names.test,predict(krawt.rf,newdata=test.moment,type="prob")),file="KaggleSubmissionKrawtchoukFire21.csv")
  }else{
    if(k==2){
      ## 1. Naive Bayes on Whole Data Set
      krawt.nbay<-NaiveBayes(train.moment,as.factor(response))
      ## 2. Write the prediction model to .CSV 3. Write test .CSV file for Kaggle Submission
      write.csv(predict(krawt.nbay,newdata=test.moment),file="KaggleSubmissionKrawtchoukNBayFire.csv")
    }else{
      ## 1a. Random Forest on Whole Data Set
      krawt.rf<-randomForest(x=ardvark.train,y=as.factor(response),ntree=500)
      ## 1b. Correct Classification/ Confusion Matrix/ OOB
      confucius<-krawt.rf$confusion
      sum(diag(confucius))/sum(confucius)
      ## 2. Write the prediction model to .CSV 3. Write test .CSV file for Kaggle Submission
      write.csv(cbind(data.file.names.test,predict(krawt.rf,newdata=ardvark.test,type="prob")),file="KaggleSubmissionArdvarkFire21.csv")   
    }
}

################################################################