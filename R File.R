library(rminer)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library("plot3D")
library(plot3Drgl)
library( plotly )
library(dplyr)
library(olsrr)
library(VGAM)
library(pracma)
library(arules)
library(randomForest)
library(tidyverse)
library(skimr)
library(knitr)
library(caTools)
library(car)

URL="http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip"

temp=tempfile() # temporary file
download.file(URL,temp) # download file to temporary
# unzip file and load into data.frame:
math=read.table(unz(temp,"student-mat.csv"),sep=";",header=TRUE)
por=read.table(unz(temp,"student-por.csv"),sep=";",header=TRUE)
cat("student performance math:",nelems(math),"\n")
cat("student performance por:",nelems(por),"\n")
print(class(math)) # show class
print(names(math)) # show attributes
# save data.frame to csv file:
write.table(math,file="math.csv",row.names=FALSE,col.names=TRUE)
write.table(por,file="por.csv",row.names=FALSE,col.names=TRUE)

math=read.table(file="math.csv",header=TRUE) # read previously saved file

por=read.table(file="por.csv",header=TRUE) # read previously saved file

por = read.csv(file='student-por.csv',header=TRUE)

math2=math

#mat=read.table(file="/Users/tom/OneDrive - HKUST Connect/MATH4993/Final.Project/student-mat.csv",sep=',',header=TRUE) # read previously saved file

math_train=read.table(file="math_train.csv",sep=',',header=TRUE)
temp=discretize(math2$absences,method='fixed',breaks=c(-1,10,Inf),labels=c(0,1))

math2$absences=temp
hist(as.numeric(temp))
math2$absences=
# EDA

# Group comparision 

scatter3D(math[math$sex=='F',]$G1,math[math$sex=='F',]$G2,math[math$sex=='F',]$G3,col = "blue",theta = 45, phi = 15)

scatter3D(math[math$sex=='M',]$G1,math[math$sex=='M',]$G2,math[math$sex=='M',]$G3,col = "blue",theta = 45, phi = 15)

temp_val = as.numeric(as.factor(math$sex))-1

scatter3D(math$G1,math$G2,math$G3,colvar = as.numeric(as.factor(math$failures)),theta = 45,phi=30)


#Count the number of points in each 3d grid 

count3d <- function(df,max_val,g_size)
{
  
  return_val<- c()
    
  for(k in seq(0,max_val,by=g_size))
  {
    for(j in seq(0,max_val,by=g_size))
    {
      for(i in seq(0,max_val,by=g_size))
      {
        return_val<-c(return_val,nrow(filter(df,(x>i-1)&(x<i+g_size-1)&(y>j-1)&(y<j+g_size-1)&(z>k-1)&(z<k+g_size-1))))
      }
    }
  }
  
  return(return_val)
}

# auxiliary function that returns data frame of given attribute and attribute value 
#and its response variables

get_df <- function(df,attribute_name,attribute_val)
{
  temp = df[df[,attribute_name]==attribute_val,]
  r_val=data.frame(x=temp$G1,y=temp$G2,z=temp$G3)
  return(r_val)
}

# inverse of get_df, returns dataframe of attribute which does not equal the 
# attribute value

not_get_df <- function(df,attribute_name,attribute_val)
{
  temp =df[df[,attribute_name]!=attribute_val,]
  r_val =data.frame(x=temp$G1,y=temp$G2,z=temp$G3)
  return(r_val)
}



get_mean <- function(df,attribute_name,attribute_val)
{
  
  temp = df[df[,attribute_name]==attribute_val,]
  temp2 = df[df[,attribute_name]!=attribute_val,]
  res = c(attribute_name,attribute_val,round(mean(temp$G1),3),round(mean(temp$G2),3),round(mean(temp$G3),3))
  res1= c(attribute_name,paste('not',attribute_val,sep=' '),round(mean(temp2$G1),3),round(mean(temp2$G2),3),round(mean(temp2$G3),3))
  return(c(res,res1))
  
}

get_mean(math,'sex','M')[1:5]

bootstrap_p<-function(count1,count2,B=10000)
{
  c1 = count1/sum(count1)
  c2 = count2/sum(count2)
  
  count12 = count1+count2
  tpi_0 = count12/sum(count12)
  
  tobs = 4*sum((sqrt(c1)-sqrt(c2))^2)*(sum(count1)*sum(count2)/sum(count12))
  
  Tnmb = rep(0,B)
  for (i in 1:B)
  {
    Bcount1 = rmultinom(n=1,size=sum(count1),prob=tpi_0)
    B1 = Bcount1/sum(Bcount1)
    
    Bcount2 =rmultinom(n=1,size=sum(count2),prob=tpi_0)
    B2= Bcount2/sum(Bcount2)
    
    Bcount12=Bcount1+Bcount2
    B12 = Bcount12/sum(Bcount12)
    
    Tnmb[i]= 4*sum((sqrt(B1)-sqrt(B2))^2)*(sum(Bcount1)*sum(Bcount2)/sum(Bcount12))
  }
  return_list<-list('p'=sum(Tnmb>=tobs)/B,'T'=Tnmb)
  return(return_list)
}

#res =bootstrap_p(count3d(get_df(math,'sex','M'),20,7),count3d(get_df(math,'sex','F'),20,7),20000)

#res =bootstrap_p(count3d(get_df(math,'sex','M'),20,7),count3d(not_get_df(math,'sex','M'),20,7),20000)

p_val= rep(0,300)
for (i in 1:300)
{
  p_val[i]=bootstrap_p(count3d(get_df(math,'Mjob','teacher'),20,5),count3d(not_get_df(math,'Mjob','teacher'),20,5),1000*i)$p
}

plot(p_val,xlab = 'Bootstrap-N in 000\'s',ylab='p-value',main='Example Permutation Test\'s p-value distribution')

hist(p_val,xlab='p-values',main='Histogram of p-value distribution')


hist(res$T)

result[1] 
vec=result[2]==unique(math[,result[1]])

TRUE %in% vec

# Iterate through distribution comparison for all possible attributes and their values

get_result <-function(df,max_val,grid_size,bootstrap_num,sig_level)
{
  result<-c()
  for (i in 1:30)
  {
    
    res <-c()
    for (j in 1:length(unique(df[,i])))
    {
      
      if(sum(df[,i]==unique(df[,i])[j])<25)
      {
        next
      }
      else
      {
        
        p = bootstrap_p(count3d(get_df(df,colnames(df)[i],unique(df[,i])[j]),max_val,grid_size),count3d(not_get_df(df,colnames(df)[i],unique(df[,i])[j]),max_val,grid_size),bootstrap_num)$p
        
        if(p >=sig_level)
        {
          next
        }
        else
        {
          res <- c(res,unique(df[,i])[j],p)
        }
        
      }
      
    }
    result<-c(result,colnames(df)[i],res)
  }
  return(result)
}

result=get_result(math2,20,5,300000,0.05)

result2 = get_result(por,20,5,300000,0.05)

hist(math$G3)

print(result2)
length(result2)
k=2
k=k+2
get_mean_result <-function(df,res)
{
  ret <- c()
  i=1
  while (i<length(res)-1)
  {
    #print(i)
    
    vec=res[i+1]==(unique(df[,res[i]]))
    #print(TRUE%in%vec)
    if(TRUE%in%vec)
    {

      ret <- c(ret, get_mean(df,res[i],res[i+1])[1:5],res[i+2],get_mean(df,res[i],res[i+1])[6:10],res[i+2])
      
      i=i+3
      vec2=res[i]==unique(df[,res[i-3]])
      if(TRUE%in%vec2)
      {
        ret <- c(ret, get_mean(df,res[i-3],res[i])[1:5],res[i+1],get_mean(df,res[i-3],res[i])[6:10],res[i+1])
        i=i+2
        vec3 =res[i]==unique(df[,res[i-5]])
        if(TRUE%in%vec3)
        {
          ret <- c(ret, get_mean(df,res[i-5],res[i])[1:5],res[i+1],get_mean(df,res[i-5],res[i])[6:10],res[i+1])
          i=i+2
        }
      }
    }
    else
    {
      i=i+1
    }
    
  }
  return(ret)
}

ltry <- linspace(0, 6, n = 100)
lltry <- length(ltry)
psi <- matrix(as.numeric(NA),length(math_tr$G3) , lltry)
for (ii in 1:lltry)
  psi[, ii] <- yeo.johnson(math_tr$G3, lambda = ltry[ii])

hist(psi[,24])
shapiro.test(psi[,24])
test = list(2,3)

mat_mean=get_mean_result(math2,result)
length(mat_mean)
por_mean= get_mean_result(por,result2)
length(por_mean)
mat_mean = matrix(mat_mean,nrow=30,byrow=TRUE)

mat_table=table(mat_mean)
dim(mat_mean)
write.csv( mat_mean,'mat_table.csv')

por_mean = matrix(por_mean,nrow=54,byrow=TRUE)

print(mat_mean)
print(por_mean)

length(por_mean)
length(mat_mean)

plot_attributes<-function(df,attribute_name,attribute_val)
{
  scatter3D(df$G1,df$G2,df$G3,colvar=df[,attribute_name]==attribute_val,colkey=list("1",'0'),cex=1,pch = 16,showscale = FALSE)
}

plot_attributes(math,'studytime',1)

result2

get_mean(por,'failures',0)
get_mean(por,'failures',1)

## Predictive modelling

# Convert G3 into binary and multiclass labels
pass = cut(math$G3,c(-1,9,20),c('fail','pass'))

par(mfrow=c(1,3)) 
plot(pass,main="pass")
hist(math$G3,col="blue",main="G3",xlab="")

new_mat = cbind(math,pass) 
#write.table(new_mat,"new_mat.csv",sep=',',row.names=FALSE,col.names=TRUE)

# Read the new data
math = read.table(file="new_mat.csv",sep=',',header=TRUE)
# Train test split
set.seed(4993) 
split = sample.split(math, SplitRatio = 0.8)
math_train = subset(math, split == TRUE)
math_test = subset(math, split == FALSE)
#write.table(math_train,"math_train.csv",sep=',',row.names=FALSE,col.names=TRUE)
math_train = subset(math_train, select = -c(G1) )
math_test = subset(math_test, select = -c(G1) )
# Linear Regression models with variable selection (Numerical variables only)
linreg = lm(G3~age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+Walc+health+absences+G1,data=math_train)
summary(linreg)

# Check VIF
ols_vif_tol(linreg)

# Model Diagnostic (# Use three tests for normality checking (majority voting))
extresid = rstudent(linreg)
pred = predict(linreg)

# Externally studentized resiudal plot
plot(pred, extresid)

# Normal plot of extresid
qqnorm(extresid)
qqline(extresid)

# Sharpio-Wilk test
shapiro.test(extresid)

# Cramer-von Mises test
# install.packages('nortest')
library(nortest)
cvm.test(extresid)

# Anderson-Darling test
ad.test(extresid)

h = hist(math_train$G3, plot = FALSE)
plot(h, xlab = "G3", ylab = "Frequency", main = "Histogram of G3",col="gray")

## Not normal distribution, do transformation
# Yeo Johnson transformation 
ltry <- linspace(0, 6, n = 100)
lltry <- length(ltry)
psi <- matrix(as.numeric(NA),length(math_train$G3) , lltry)
for (ii in 1:lltry)
  psi[, ii] <- yeo.johnson(math_train$G3, lambda = ltry[ii])

h = hist(psi[,24], plot = FALSE)
plot(h, xlab = "G3", ylab = "Frequency", main = "Histogram of G3",col="gray")

# Linear Regression models with variable selection (Numerical variables only)
linreg = lm(psi[,24]~age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+Walc+health+absences+G1,data=math_train)
extresid = rstudent(linreg)
pred = predict(linreg)
plot(pred, extresid)
qqnorm(extresid)
qqline(extresid)
shapiro.test(extresid)
library(nortest)
cvm.test(extresid)
ad.test(extresid)

# Oversampling (training set)
library(ROSE) 
over <- ovun.sample(pass~.,data=math_train,method="over",N=418)$data
table(over$pass)
summary(over)
math_train = over
mat_trainX = math_train[,c("age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences","G1")]
mat_testX = math_test[,c("age","Medu","Fedu","traveltime","studytime","failures","famrel","freetime","goout","Dalc","Walc","health","absences","G1")]
mat_trainY = math_train[,"pass"]
mat_testY = math_test[,"pass"]
#write.table(math_train,"math_over.csv",sep=',',row.names=FALSE,col.names=TRUE)

## Use rminer package to build models
X = 2:31
Y = which(names(math_train)=="pass") 
cat("output class:",class(math_train[,Y]),"\n")

# char-> string -> factor
factored_math_train = data.frame(unclass(math_train),stringsAsFactors=TRUE)
factored_math_test = data.frame(unclass(math_test),stringsAsFactors=TRUE)
# Converting factors into numerical values  

#must_convert<-sapply(factored_math,is.factor)
#factored_math2 <-sapply(factored_math[,must_convert],unclass)
#out1<-cbind(factored_math[,!must_convert],factored_math2)
#col_order <- c(c(colnames(math)))
#out <- out1[,col_order]

math_train = factored_math_train
math_test = factored_math_test

g3 = which(names(math_train)=="G3") 
y = math_test$G3
cat("output class:",class(math_train[,g3]),"\n")
hold = holdout(math_train$G3,ratio=1,seed=4993)

# Random forest regression
rf_score = fit(G3~.,math_train[hold$tr,c(X,g3)],model="randomForest",ntree=500)
rf_score_pred = predict(rf_score,math_test)
rf_error = mmetric(y,rf_score_pred,metric=c("RMSE","MAE","R22"))
error = paste("RF, RMSE=",round(rf_error[1],2),", MAE=",round(rf_error[2],2),", R2=",round(rf_error[3],2),sep=" ")
mgraph(y,rf_score_pred,graph="RSC",main="Random Forest") 
cat(error,"\n")

# SVR
SVM_score = fit(G3~.,math_train[H$tr,c(X,g3)],model="ksvm",kernel="laplacedot",C=4)
SVM_score_pred = predict(SVM_score,math_test)
SVM_error = mmetric(y,SVM_score_pred,metric=c("RMSE","MAE","R22"))
error = paste("SVM, =",round(SVM_error[1],2),", MAE=",round(SVM_error[2],2),", R2=",round(SVM_error[3],2),sep=" ")
mgraph(y,SVM_score_pred,graph="RSC",main="Support Vector Machine") 
cat(error,"\n")

## Binary classification

# Decision Tree
require(tree)
dtree = tree(pass~.,math_train[,c(X,Y)])
dtree_pred = predict(dtree,math_test)
print(mmetric(math_test$pass,dtree_pred,"CONF"))

# SVM
SVM = fit(pass~.,math_train[,c(X,Y)],model="ksvm",kernel="laplacedot",C=4) # fit a support vector machine 
SVM_pred = predict(SVM,math_test)
print(mmetric(math_test$pass,SVM_pred,"CONF"))

# Random Forest
library(randomForest)
library(tidyverse)
library(skimr)
library(knitr)
s = list(smethod="grid",search=list(mtry=c(1,2,3,4,5),ntree=c(100,200,300,400,450,500,600,700,800)),convex=c(0,1,2))
rf = randomForest(pass~.,data=math_train[,c(X,Y)],trControl=s, importance=TRUE)
print(rf)
imp = importance(rf)
create_rfplot = function(rf, type){
  imp = importance(rf)
  featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[,1])
  p <- ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity",  fill = "deepskyblue4",width = 0.65) +
    scale_fill_brewer(palette = "Spectral") +
    coord_flip() + 
    theme_light(base_size = 20) +
    theme(axis.title.x = element_text(size = 14, color = "black"),
          axis.title.y = element_blank(),
          axis.text.x  = element_text(size = 14, color = "black"),
          axis.text.y  = element_text(size = 12, color = "black")) 
  return(p)
}
create_rfplot(rf, type = 2)   
rf_pred = predict(rf,math_test)
print(mmetric(math_test$pass,rf_pred,"CONF"))

# Logistic regression
logisticr=fit(pass~age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+Walc+health+absences+G1,data=math_train,model="multinom") # Logistic Regression, sbs - standard backward selection
logistic_pred = predict(logisticr,math_test)
print(mmetric(math_test$pass,logistic_pred,"CONF"))

# L1-regularized logistic regression
library(LiblineaR)
logisticr = LiblineaR(as.matrix(mat_trainX),mat_trainY)
predictY = predict(logisticr,as.matrix(mat_testX))$predictions
print(mmetric(as.factor(mat_testY),as.factor(predictY),"CONF"))
