#=======================#
# Canonical Correlation #
#=======================#

rm(list = ls())

library(dplyr)
library(candisc)

setwd("H:/Masters_of_Statistics/Semester_3/Multivariate/Assignments/1/Dataset")
load("wvs.Rdata")


#=============================#
# Creating Y and X Variables  #
#=============================#


## Creating datasets
religion<-wvs[,c(30:32)]
crimes<-wvs[,c(11:15)]
justifiability <-wvs[,c(16:29)]
country <-wvs[,c(1,33)] # variable 1 included to avoid problems when adding countries to Z variables. 

Yvar <- justifiability
Xvar <- cbind(religion,crimes)

## Standardizing Variables
zyvar<-scale(Yvar,center=TRUE,scale=TRUE)
zxvar<-scale(Xvar,center=TRUE,scale=TRUE)

## Filtering by Country 

zyvar.c <- cbind(zyvar, country)
zyvar.c <- zyvar.c[,-15]
zyvar.c <- filter(zyvar.c, country == "Malaysia" |  country == "Netherlands")
names(zyvar.c)

zxvar.c <- cbind(zxvar, country)
zxvar.c <- zxvar.c[,-9]
zxvar.c <- filter(zxvar.c, country == "Malaysia" |  country == "Netherlands")
names(zxvar.c)


zdata <- cbind(zyvar.c,zxvar.c)
names(zdata)

#================================#
# Canonical correlation analysis #
#================================#
cancor.out<-cancor(cbind(J_claiming_benefits,J_avoiding_fare,J_stealing_property, J_cheating_taxes,           
                         J_accept_bribe,J_homosexuality, J_prostitution,J_abortion, J_divorce,J_sex_before_marriage,      
                         J_suicide,J_beat_wife, J_parents_beating_children,J_violence) ~ 
                     R_attend_religious_services+R_pray+ R_importance_God +CR_robberies+CR_alcohol+                 
                     CR_police_military+ CR_racist_behavior+CR_drug_sale,data=zdata)
summary(cancor.out)

#canonical loadings
cancor.out$structure$X.xscores
cancor.out$structure$Y.yscores

#canonical variates
cancor.out$scores$X
cancor.out$scores$Y

head(can1,20)
#Plots
can1<-cbind(cancor.out$scores$X[,1],cancor.out$scores$Y[,1])
rownames(can1)<-as.character(zdata[,15])
plot(can1,xlab="u1",ylab="t1",xlim=c(-3,3),ylim=c(-5,5), main = "R(U1,T1)")
#identify points in the plot
identify(can1,labels=as.character(zdata[,15]),col="Red", size=1)

can2<-cbind(cancor.out$scores$X[,2],cancor.out$scores$Y[,2])
rownames(can2)<-as.character(zdata[,15])
plot(can2,xlab="u2",ylab="t2",xlim=c(-4,3),ylim=c(-6,5.5),main = "R(U2,T2)")
#identify points in the plot
identify(can2,labels=as.character(zdata[,15]),col="Red")

can3<-cbind(cancor.out$scores$X[,3],cancor.out$scores$Y[,3])
rownames(can3)<-as.character(zdata[,15])
plot(can3,xlab="u3",ylab="t3",xlim=c(-5,4),ylim=c(-7,6),main = "R(U3,T3)")
#identify points in the plot
identify(can3,labels=as.character(zdata[,15]),col="Red")

can4<-cbind(cancor.out$scores$X[,4],cancor.out$scores$Y[,4])
rownames(can4)<-as.character(zdata[,15])
plot(can4,xlab="u4",ylab="t4",xlim=c(-5,4),ylim=c(-7,6),main = "R(U4,T4)")
#identify points in the plot
identify(can3,labels=as.character(zdata[,15]),col="Red")

can5<-cbind(cancor.out$scores$X[,5],cancor.out$scores$Y[,5])
rownames(can5)<-as.character(zdata[,15])
plot(can5,xlab="u5",ylab="t5",xlim=c(-5,5),ylim=c(-7,7),main = "R(U5,T5)")
#identify points in the plot
identify(can3,labels=as.character(zdata[,15]),col="Red")

#==============#
# Redundancies #
#==============#

redu<-redundancy(cancor.out)
round(redu$Ycan,5)
