library(rminer)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library("plot3D")
library(plot3Drgl)
library( plotly )
library(dplyr)
library(rminer)
library(VGAM)
library(pracma)
library(arules)
URL="http://archive.ics.uci.edu/ml/machine-learning-databases/00320/student
.zip"
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
ltry[21]
lltry <- length(ltry)
psi <- matrix(as.numeric(NA),length(math$G3) , lltry)
for (ii in 1:lltry)
  psi[, ii] <- yeo.johnson(math$G3, lambda = ltry[ii])

hist(psi[,21])
shapiro.test(psi[,21])
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

# Predictive modelling

# Convert G3 into binary and multiclass labels
pass = cut(math$G3,c(-1,9,20),c('fail','pass'))
five = cut(math$G3,c(-1,9,11,13,15,20),c("F","D","C","B","A"))

par(mfrow=c(1,3)) 
plot(pass,main="pass")
plot(five,main="five") 
hist(math$G3,col="gray",main="G3",xlab="")

new_mat = cbind(math,pass,five) 
write.table(new_mat,"new_mat.csv",sep=',',row.names=FALSE,col.names=TRUE)

# Read the new data
math = read.table(file="new_mat.csv",sep=',',header=TRUE)

# Train test split
split = sample.split(math, SplitRatio = 0.8)
math_train = subset(math, split == TRUE)
math_test = subset(math, split == FALSE)

# Oversampling (training set)
library(ROSE) 
over <- ovun.sample(pass~.,data=math_train,method="over",N=420)$data
table(over$pass)
summary(over)
math_train = over

# Model Diagnostic (# Use three tests for normality checking (majority voting))
extresid = rstudent(fit)
pred = predict(fit)

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

## Not normal distribution, do transformation

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

## Use rminer package to build models
inputs=2:30
bout=which(names(math_train)=="pass") 
cat("output class:",class(math_train[,bout]),"\n")

# char-> string -> factor
factored_math =data.frame(unclass(math_train),stringsAsFactors=TRUE)

# Converting factors into numerical values  

#must_convert<-sapply(factored_math,is.factor)
#factored_math2 <-sapply(factored_math[,must_convert],unclass)
#out1<-cbind(factored_math[,!must_convert],factored_math2)
#col_order <- c(c(colnames(math)))
#out <- out1[,col_order]

math_train = factored_math

B1=fit(pass~.,math_train[,c(inputs,bout)],model="rpart")
print(B1@object)
plot(B1@object,uniform=TRUE,branch=0,compress=TRUE) 
text(B1@object,xpd=TRUE,fancy=TRUE,fwidth=0.2,fheight=0.2) 

B4=fit(pass~.,math_train[,c(inputs,bout)],model="ksvm") # fit a support vector machine 
print(B4@object)

# Random forest regression
inputs = 2:30
# select outputs: regression task 
g3=which(names(math_train)=="G3") 
cat("output class:",class(math_train[,g3]),"\n")
H=holdout(math_train$G3,ratio=1,seed=4993)
print("holdout:")
print(summary(H))
R1=fit(G3~.,math_train[H$tr,c(inputs,g3)],model="randomForest")
P1=predict(R1,math_test)
target1=math_test$G3
e1=mmetric(target1,P1,metric=c("MAE","R22"))
error=paste("RF, holdout: MAE=",round(e1[1],2),", R2=",round(e1[2],2),sep=" ")
mgraph(target1,P1,graph="RSC",Grid=10,main=error) 
cat(error,"\n")

# Binary Classification
bout=which(names(math_train)=="pass") 
cat("output class:",class(math_train[,bout]),"\n") 
bmath=math_train[,c(inputs,bout)] # for easy use 
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

I = Importance(B2, cmath[H$tr,])
print(round(I$imp,digits=5)) 
imax=which.max(I$imp)
L=list(runs=1,sen=t(I$imp),sresponses=I$sresponses)
par(mar=c(2.0,2.0,2.0,2.0))
mgraph(L,graph="IMP",leg=names(cmath),col="gray",Grid=10,PDF="imp-1") 
txt=paste("VEC curve for",names(cmath)[imax],"influence on class",levels(y) [TC])
mgraph(L,graph="VEC",xval=imax,Grid=10,data=cmath[H$tr,],TC=1,main=txt,PDF= "vec-1")

txt=paste(levels(y)[2],"ALIFT:",round(mmetric(y,P2,metric="ALIFT",TC=2),2)) 
mgraph(y,P2,graph="LIFT",baseline=TRUE,Grid=10,main=txt,TC=2,PDF="lift-1")

#
inputs=1:32 # all except pass and five
g3=which(names(math_train)=="G3")
cat("output class:",class(math_train[,g3]),"\n")
rmath=math_train[,c(inputs,g3)] # for easy use
y=rmath$g3 # target

# mining for randomForest, external 5-fold, 20 Runs (=100 fitted models)
M1=mining(G3~.,rmath,model="randomForest",method=c("kfold",5,123),Runs=20)
m=mmetric(M1,metric=c("MAE","RMSE")) # 2 metrics:
print(m) # show metrics for each run
mi=meanint(m[,1])
cat("RF MAE values:",round(mi$mean,2),"+-",round(mi$int,2),"\n")

# regression scatter plot:
txt=paste("G3 MAE:",round(mi$mean,2))
mgraph(M1,graph="RSC",Grid=10,main=txt,PDF="rsc-1")

# REC curve, comparison with multiple regression: "mr":
M2=mining(G3~.,rmath,model="mr",method=c("kfold",5,123),Runs=20)
L=vector("list",2) # list of minings
L[[1]]=M1
L[[2]]=M2
mgraph(L,graph="REC",leg=c("randomForest","mr"),main="REC curve",xval=10,PDF="rec-1")

# input importance 
mgraph(M1,graph="imp",leg=names(rmath),xval=15,PDF="rec-2")