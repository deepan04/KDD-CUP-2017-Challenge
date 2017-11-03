setwd("//home//dhaval//Dhaval//Data Science//Projects//Urban Networks//KDD Cup 2017")
library(glmnet)
df=as.data.frame(read.csv("FINAL_BINARY_FILE_WITH_SPEED.csv"))
tollgate_3=as.data.frame(df[df$tollgate_id==3,])
tollgate_3_entry=as.data.frame(tollgate_3[tollgate_3$direction==0,])
tollgate_3_exit=as.data.frame(tollgate_3[tollgate_3$direction==1,])
x=model.matrix(volume~.,tollgate_3_entry)[,-1]
y=tollgate_3_entry$volume
head(x,2)
grid =10^ seq (10,-2, length =100)
set.seed (1)
train = sample (1: nrow(x), nrow(x)/2)
test =(- train )
y.test =y[ test]
set.seed(1)
cv.out =cv.glmnet (x[train ,], y[ train ], alpha =1)
plot(cv.out )
bestlam =cv.out$lambda.min
bestlam
lasso.mod =glmnet (x[train ,], y[ train ], alpha =1, lambda =grid)
plot(lasso.mod )
lasso.pred= predict(lasso.mod ,s=bestlam , newx=x[test ,])
mean (( lasso.pred -y.test)^2) #MSE
mean (abs((lasso.pred -y.test)/lasso.pred)) #MAPE
# ===========================
# Random Forest
library(randomForest)
library(boot)
#t3_entry_copy=tollgate_3_entry
#t3_entry_copy$volume=NULL
train_rf=sample (1: nrow(tollgate_3_entry), nrow(tollgate_3_entry)*2/3)
tempx=tollgate_3_entry[-train_rf,]
cv.error.10= rep (0 ,10)
for (i in 1:10) {
  rf.mod=randomForest(volume~.,data=tollgate_3_entry,importance=TRUE)
  cv.error.10[ i]= cv.glm (tollgate_3_entry ,rf.mod ,K =10) $delta [1]
  }

rf.mod=randomForest(volume~.,data=tollgate_3_entry,mtry=43,importance=TRUE,subset=train_rf)
rf.mod
yhat.bag=predict(rf.mod,newdata = tollgate_3_entry[-train_rf,])
mean (abs((yhat.bag -tempx$volume)/yhat.bag)) #MAPE
length(colnames(tollgate_3_entry))
length(train_rf)
#======================================
library(glmnet)
df_without_weather=as.data.frame(read.csv("FINAL_BINARY_FILE_WITHOUT_WEATHER.csv"))
head(df_without_weather,2)
tollgate_3=as.data.frame(df_without_weather[df_without_weather$tollgate_id==3,])
tollgate_3_entry=as.data.frame(tollgate_3[tollgate_3$direction==0,])
head(tollgate_3_entry,2)
temp=as.data.frame(read.csv("TEMP_FILE.csv"))
plot(tollgate_3_entry$Hour_0,tollgate_3_entry$volume,pch=19)
library(e1071)
peakHours_Monday=as.data.frame(read.csv("Peak_Hours.csv"))
head(peakHours_Monday,2)
svm_model=svm(peakHours_Monday$volume~.)