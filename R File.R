library(rminer)
library(ggplot2)
library(gridExtra)
library(ggExtra)
library("plot3D")
library(plot3Drgl)
library(dplyr)
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



