#using package rDEA

library("rDEA")
data=read.csv("Engg.csv",head=T)

name=data$Name
NIRF_Rank=data$NIRF_Rank
faculty=data$Faculty
expense=data$Expenditure
grad=data$Graduates
scopus=data$Scopus
fund=data$Rfunding

X=matrix(0,50,2)
X[,1]=faculty
X[,2]=expense

Y=matrix(0,50,3)
Y[,1]=grad
Y[,2]=scopus
Y[,3]=fund


#i=2
#index=c(1:i-1,(i+1):50)

d_c=dea(X,Y,X,Y,model="output",RTS="constant")
lam_c=d_c$lambda
theta_c=d_c$thetaOpt


d_v=dea(X,Y,X,Y,model="output",RTS="variable")
lam_v=d_v$lambda
theta_v=d_v$thetaOpt

d_n=dea(X,Y,X,Y,model="output",RTS="non-increasing")
lam_n=d_n$lambda
theta_n=d_n$thetaOpt


df=transform(data,eff_CRS=theta_c,eff_VRS=theta_v,eff_NIRS=theta_n,eff_c_rank=rank(-theta_c))
View(df)

write.csv(file="Engg1.csv",x=df)

##### For management colleges
data1=read.csv("Mgmt.csv",head=T)

name=data1$Name
NIRF_Rank=data1$NIRF_Rank
faculty=data1$Faculty
expense=data1$Expenditure
grad=data1$Graduates
scopus=data1$Scopus
fund=data1$Rfunding

X=matrix(0,50,2)
X[,1]=faculty
X[,2]=expense

Y=matrix(0,50,3)
Y[,1]=grad
Y[,2]=scopus
Y[,3]=fund


#i=2
#index=c(1:i-1,(i+1):50)

d_c=dea(X,Y,X,Y,model="output",RTS="constant")
lam_c=d_c$lambda
theta_c=d_c$thetaOpt


d_v=dea(X,Y,X,Y,model="output",RTS="variable")
lam_v=d_v$lambda
theta_v=d_v$thetaOpt

d_n=dea(X,Y,X,Y,model="output",RTS="non-increasing")
lam_n=d_n$lambda
theta_n=d_n$thetaOpt

df1=transform(data1,eff_CRS=theta_c,eff_VRS=theta_v,eff_NIRS=theta_n,eff_c_rank=rank(-theta_c))


write.csv(file="Mgmt1.csv",x=df1)

#for clashes, using supereffeciency
library(Benchmarking)







#######################################################
#### Analysis
#######################################################

