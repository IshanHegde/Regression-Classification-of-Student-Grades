library(rminer)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library("plot3D")
library(plot3Drgl)
library(dplyr)
library(rminer)
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student
.zip"
temp=tempfile() # temporary file
download.file(URL,temp) # download file to temporary
# unzip file and load into data.frame:
math=read.table(unz(temp,"student-mat.csv"),sep=";",header=TRUE)
cat("student performance math:",nelems(math),"\n")
print(class(math)) # show class
print(names(math)) # show attributes
# save data.frame to csv file:
write.table(math,file="math.csv",row.names=FALSE,col.names=TRUE)

math=read.table(file="math.csv",header=TRUE) # read previously saved file



# EDA

# Group comparision 


scatter3D(math[math$sex=='F',]$G1,math[math$sex=='F',]$G2,math[math$sex=='F',]$G3,col = "blue",theta = 45, phi = 15)

scatter3D(math[math$sex=='M',]$G1,math[math$sex=='M',]$G2,math[math$sex=='M',]$G3,col = "blue",theta = 45, phi = 15)

temp_val = as.numeric(as.factor(math$sex))-1

scatter3D(math$G1,math$G2,math$G3,colvar = as.numeric(as.factor(math$failures)),theta = 45,phi=30)



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

get_df <- function(df,attribute_name,attribute_val)
{
  temp = df[df[,attribute_name]==attribute_val,]
  r_val=data.frame(x=temp$G1,y=temp$G2,z=temp$G3)
  return(r_val)
}


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
  return(sum(Tnmb>=tobs)/B)
  
}


bootstrap_p(count3d(get_df(math,'sex','M'),20,7),count3d(get_df(math,'sex','F'),20,7),10000)



# Dr. Yu's 2d example

g2treat_count = c(0,0,0,0,0,1,0,0,0,2,7,4,0,0,19,186)
g2treat = g2treat_count/sum(g2treat_count)

g2control_count = c(rep(0,8),1,3,8,3,0,1,22,178)
g2control = g2control_count/sum(g2control_count)

g2count =g2treat_count+g2control_count
pi_0 = g2count/sum(g2count)

T_obs = 4*sum((sqrt(g2treat)-sqrt(g2control))^2)*(sum(g2treat_count)*sum(g2control_count)/sum(g2count))

M=16
p3 = 1-pchisq(T_obs,df=M-1)

p3

B= 100000
Tnm_b = rep(0,B)

for (i in 1:B)
  {
  
  g2treat_count
  g2treat_Bcount =rmultinom(n=1,size=sum(g2treat_count),prob=pi_0)
  g2treatB = g2treat_Bcount/sum(g2treat_Bcount)
  
  g2control_Bcount = rmultinom(n=1,size=sum(g2control_count),prob=pi_0)
  g2controlB = g2control_Bcount/sum(g2control_Bcount)
  
  g2_Bcount=g2treat_Bcount +g2control_Bcount
  g2B =g2_Bcount/sum(g2_Bcount)
  
  Tnm_b[i]=4*sum((sqrt(g2treatB)-sqrt(g2controlB))^2)*(sum(g2treat_Bcount)*sum(g2control_Bcount)/sum(g2_Bcount))
  
  }

pB = sum(Tnm_b>=T_obs)/B

pB




# Predictive modelling
library("caTools")
set.seed(4993)
math = read.csv('/Users/tom/OneDrive - HKUST Connect/MATH4993/Final.Project/student-mat.csv',sep = ',', head = T)
colnames(mat)

pass = cut(math$G3,c(-1,9,20),c('fail','pass'))
five=cut(math$G3,c(-1,9,11,13,15,20),c("F","D","C","B","A"))

pdf("math-grades.pdf")
par(mfrow=c(1,3)) 
plot(pass,main="pass")
plot(five,main="five") 
hist(math$G3,col="gray",main="G3",xlab="")
dev.off() 

new_mat = cbind(math,pass,five) 
write.table(new_mat,"new_mat.csv",sep=',',row.names=FALSE,col.names=TRUE)

math=read.table(file="new_mat.csv",sep=',',header=TRUE)

# read the data file
#split = sample.split(math, SplitRatio = 0.8)
#math_train = subset(math, split == TRUE)
#math_test = subset(math, split == FALSE)

# rminer packages
inputs=2:29
bout=which(names(math)=="pass") 
cat("output class:",class(math[,bout]),"\n")


# char-> string -> factor

factored_math =data.frame(unclass(math),stringsAsFactors=TRUE)



# Converting factors into numerical values  

#must_convert<-sapply(factored_math,is.factor)
#factored_math2 <-sapply(factored_math[,must_convert],unclass)
#out1<-cbind(factored_math[,!must_convert],factored_math2)
#col_order <- c(c(colnames(math)))
#out <- out1[,col_order]


math=factored_math

bout=which(names(math)=="pass") 
cat("output class:",class(math[,bout]),"\n")

B1=fit(pass~.,math[,c(inputs)],model="rpart")
print(B1@object)
pdf("trees-1.pdf")
plot(B1@object,uniform=TRUE,branch=0,compress=TRUE) 
text(B1@object,xpd=TRUE,fancy=TRUE,fwidth=0.2,fheight=0.2) 
dev.off()

B4=fit(pass~.,math[,c(inputs,bout)],model="ksvm") # fit a support vector machine 
print(B4@object)

# Random forest regression
inputs = 2:30
# select outputs: regression task 
g3=which(names(math)=="G3") 
cat("output class:",class(math[,g3]),"\n")
H=holdout(math$G3,ratio=0.8,seed=4993)
print("holdout:")
print(summary(H))
R1=fit(G3~.,math[H$tr,c(inputs,g3)],model="randomForest")
P1=predict(R1,math[H$ts,c(inputs,g3)])
target1=math[H$ts,]$G3
e1=mmetric(target1,P1,metric=c("MAE","R22"))
error=paste("RF, holdout: MAE=",round(e1[1],2),", R2=",round(e1[2],2),sep=" ")
pdf("rf-1.pdf") 
mgraph(target1,P1,graph="RSC",Grid=10,main=error) 
dev.off() 
cat(error,"\n")

# Binary Classification
bout=which(names(math)=="pass") 
cat("output class:",class(math[,bout]),"\n") 
bmath=math[,c(inputs,bout)] # for easy use 
y=bmath$pass # target
# fit rpart to all data, pure class modeling (no probabilities) 
B1=fit(pass~.,bmath,model="rpart",task="class") # fit a decision tree 
P1=predict(B1,bmath) # class predictions print(P1[1]) # show 1st prediction 
m=mmetric(y,P1,metric=c("ACC","ACCLASS")) print(m) # accuracy, accuracy per class 
m=mmetric(y,P1,metric=c("CONF")) # a) 
print(m$conf) # confusion matrix 
m=mmetric(y,P1,metric=c("ALL")) 
print(round(m,1)) # all pure class metrics

# fit rpart to all data, default probabilistic modeling 
B2=fit(pass~.,bmath,model="rpart",task="prob") # fit a decision tree 
P2=predict(B2,bmath) # predicted probabilities p
rint(P2[1,]) # show 1st prediction 
m=mmetric(y,P2,metric=c("ACC"),TC=2,D=0.5) 
print(m) # accuracy, accuracy per class 
m=mmetric(y,P2,metric=c("CONF"),TC=2,D=0.1) # equal to a) 
print(m$conf) # confusion matrix 
m =mmetric(y,P2,metric=c("AUC","AUCCLASS")) 
print(m) # AUC, AUC per class 
m=mmetric(y,P2,metric=c("ALL")) 
print(round(m,1)) # all prob metrics

# ROC and LIFT curve: 
txt=paste(levels(y)[2],"AUC:",round(mmetric(y,P2,metric="AUC",TC=2),2)) 
mgraph(y,P2,graph="ROC",baseline=TRUE,Grid=10,main=txt,TC=2,PDF="roc-1") 
txt=paste(levels(y)[2],"ALIFT:",round(mmetric(y,P2,metric="ALIFT",TC=2),2)) 
mgraph(y,P2,graph="LIFT",baseline=TRUE,Grid=10,main=txt,TC=2,PDF="lift-1")

# Linear Regression models with variable selection (Numerical variables only)
fit = lm(G3~age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+Walc+health+absences,data=math_train)
summary(fit)

library("bestglm")
library("leaps")
designx=cbind(math_train$age,math_train$Medu,math_train$Fedu, math_train$traveltime,math_train$studytime,math_train$failures,math_train$famrel,
              math_train$freetime,math_train$goout,math_train$Dalc,math_train$Walc,math_train$health,
              math_train$absences)
Xy = cbind(as.data.frame(designx), math_train$G3)
bestglm(Xy, IC = "AIC")$BestModel

# BIC 
bestglm(Xy, IC = "BIC")$BestModel

# Backward Elmination 
fit_full = lm(G3~age+Medu+Fedu+traveltime+studytime+failures+famrel+freetime+goout+Dalc+Walc+health+absences,data=math_train)
fitB = step(fit_full, direction='backward')
fitB

# Model Diagnostic (# Use three tests for normality checking (majority voting))
extresid = rstudent(fit)
pred = predict(fit)

### Externally studentized resiudal plot
plot(pred, extresid)

### Normal plot of extresid
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