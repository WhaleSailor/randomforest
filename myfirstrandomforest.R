rm(list=ls())
install.packages("randomForest")
library(plyr)
install.packages("plyr")
library(randomForest)
library(plyr)
point <- read.csv(file = "attribute.csv")
names(point)
CVgroup <- function(k,datasize,seed){
  cvlist <- list()
  set.seed(seed)
  n <- rep(1:k,ceiling(datasize/k))[1:datasize]
  temp <- sample(n,datasize)
  x <- 1:k
  dataseq <- 1:datasize
  cvlist <- lapply(x,function(x) dataseq[temp==x])
  return(cvlist)
}
k <- 10
datasize <- nrow(point)
cvlist <- CVgroup(k, datasize, 8)

train <- point[-cvlist[[1]],c(1:12)] 
test <- point[cvlist[[1]],c(1:12)]   
trainobj <- point[-cvlist[[1]],1] 
testobj <- point[cvlist[[1]],1] 
rf_1 <- randomForest(AGB~.,data=train,mtry=3,importance=TRUE,maxnodes = 10)
print(rf_1)
pred <- predict(rf_1,test)
mean_t = mean((pred-testobj)^2)  
RMSE_t <- sqrt(mean_t)   
R2_t <- cor(pred,testobj)^2   
RMSE_t
R2_t
tpred <- predict(rf_1,point[2:12])
RMSE <- sqrt(mean((tpred-point[,1])^2))  
R2 <- cor(tpred,point[,1])^2    
RMSE
R2
obj_AGB = point[,1] 
View(obj_AGB)
View(tpred)   
plot(obj_AGB,tpred)


