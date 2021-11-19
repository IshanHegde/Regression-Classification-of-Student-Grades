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

write.table(math,file="math.csv",row.names=FALSE,col.names=TRUE)


# EDA

# Group comparision 

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

B= 10000
Tnm_b = rep(0,B)


g2treat_count
g2treat_Bcount =rmultinom(n=1,size=(g2treat_count),prob=pi_0)
g2treatB = g2treat_Bcount/sum(g2treat_Bcount)

g2control_Bcount =                rmultinom(n=1,size=sum(g2control_count),prob=pi_0)

print(g2treatB)

g2controlB = g2control_Bcount/sum(g2control_Bcount)

g2_Bcount=g2treat_Bcount +g2control_Bcount
g2B =g2_Bcount/sum(g2_Bcount)

Tnm_b[1]=4*sum((sqrt(g2treatB)-sqrt(g2controlB))^2)*(sum(g2treat_Bcount)*sum(g2control_Bcount)/sum(g2_Bcount))




Tnm_b[1]

pB = sum(Tnm_b>=T_obs)/B

pB

# Predictive modelling



