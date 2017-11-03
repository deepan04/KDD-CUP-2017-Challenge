#Data Read
data_without_weather=read.csv("D:/Masters/Urban netwrks/kdd cup/KDDData/R analysis/without_holidays.csv")

#Tolgate 3 entry 0 testtttttt
tollgate_3=as.data.frame(data_without_weather[data_without_weather$tollgate_id==3,])
tollgate_3_entry=as.data.frame(tollgate_3[tollgate_3$direction==0,])
tollgate_3_exit=as.data.frame(tollgate_3[tollgate_3$direction==1,])


#check data type
colnames(data_without_weather)
str(data_without_weather)

#To see the how variable variance is varying
apply(data_test,2,var)

#Pca
pca=prcomp(data_without_weather, scale=T)
plot(pca)
pca$rotation

# used to find correlation. PC1 applies equal weight to volume and A which shows correlation.
pca$rotation=-pca$rotation
pca$x=-pca$x
biplot (pca , scale =0) 

#compute standard deviation of each principal component
std_dev <- pca$sdev

#compute variance
pr_var <- std_dev^2

#check variance of first 10 components
pr_var[1:10]

#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

#scree plot
plot(prop_varex, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",ylab = "Cumulative Proportion of Variance Explained", type = "b")

#add a training set with principal components
train.data <- data.frame(volume = data_without_weather$volume, pca$x)

#we are interested in first 34 PCAs
train.data <- train.data[,1:35]

#install and load
install.packages('randomForest')
library('randomForest')


#Make a Formula
varNames <- names(train.data)

# Exclude ID or Response variable
varNames <- varNames[!varNames %in% c("volume")]

# add + sign between exploratory variables
varNames1 <- paste(varNames, collapse = "+")

# Add response variable and convert to a formula object
rf.form <- as.formula(paste("volume", varNames1, sep = " ~ "))

#determine ntree
model=randomForest(rf.form,data=train.data,ntree=500,importance=TRUE)
plot(model)

#Random Forest (Test) and cross validation
error<-0
size=dim(train.data)[1]
t=round(size/10)-2
for (i in 1:10){
  s1=(i-1)*t + 1
  s2=t*i
  sub=s1:s2
  print(c(s1,s2))
  train=train.data[-sub,]
  test=train.data[sub,]
  model=randomForest(rf.form,data=train,ntree=100,importance=TRUE)
  prediction=predict(model,test)
  
  error[i]=mean(abs(prediction-test[,1])/test[,1])   #MAPE value
  print(error[i])
}

MAPE=mean(error)
MAPE







