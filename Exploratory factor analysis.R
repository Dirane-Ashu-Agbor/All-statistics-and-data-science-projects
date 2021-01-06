#==============================#
# Exploratory Factor Analysis  #
#==============================#
rm(list = ls())
library(GPArotation)
library(psych)
library(maptools)
library(psych)


load(file="H:/Masters_of_Statistics/Semester_3/Multivariate/Assignments/1/wvs.RData")
View(wvs)

wvs14<- wvs[,c(16:29)]

#====================================#
# Standardizing 14 Items in table 3  #
#====================================#

sum(is.na(wvs14))
#Checking no missing data 

#standardizing dataset
zwvs14<-as.data.frame(scale((wvs14),center=TRUE,scale=TRUE))
head(zwvs14)

#====================================#
# Finding The Number of Components   #
#====================================#

#PCA
pc<-prcomp(zwvs14)
summary(pc)
print(pc)

#screeplot
screeplot(pc,type="lines", main = " PCA Scree-Plot")

#====================================#
#    Factor Analysis Model           #
#====================================#

## estimate EFA with 3 common factors
efa1<-factanal(~J_claiming_benefits +J_avoiding_fare +J_stealing_property+ 
                    J_cheating_taxes+ J_accept_bribe+ J_homosexuality+
                    J_prostitution+ J_abortion+ J_divorce+ J_sex_before_marriage+
                    J_suicide+ J_beat_wife+ J_parents_beating_children+ 
                    J_violence,data= zwvs14,factors=3,rotation="none")

print(efa1,cutoff=0)
#The factors do not seem to explain the variables very well

## Using (fa) to get complete loadings
efa_cor<-fa(zwvs14,nfactors=3,rotate="none",covar=FALSE,fm="ml")
efa_cor
#Almost the same results produced by factanal

#===================#
# plot of loadings  #
#===================#
par(mfrow=c(1,3))

par(pty="s")
plot(efa1$loadings,xlim=c(-10,10),ylim=c(-1,1),cex=2, col="blue")
pointLabel(efa1$loadings,rownames(efa1$loadings),cex=1)
abline(h=0, col="red", lwd=1, lty=2)
abline(v=0, col="red", lwd=1, lty=2)

plot(efa1$loadings[,1],efa1$loadings[,3],xlim=c(-10,10),
     ylim=c(-1,1),cex=2, col="blue", xlab="Factor 1", ylab="Factor 3")
pointLabel(efa1$loadings,rownames(efa1$loadings),cex=1)
abline(h=0, col="red", lwd=1, lty=2)
abline(v=0, col="red", lwd=1, lty=2)

plot(efa1$loadings[,2],efa1$loadings[,3],xlim=c(-10,10),
        ylim=c(-1,1),cex=2, col="blue", xlab="Factor 2", ylab="Factor 3")
pointLabel(efa1$loadings,rownames(efa1$loadings),cex=1)
abline(h=0, col="red", lwd=1, lty=2)
abline(v=0, col="red", lwd=1, lty=2)


#=============================#
#    Varimax Rotation         #
#=============================#

## analysis 3-factor model with varimax rotation
efa_vmx<-factanal(~J_claiming_benefits +J_avoiding_fare +J_stealing_property+ 
                    J_cheating_taxes+ J_accept_bribe+ J_homosexuality+
                    J_prostitution+ J_abortion+ J_divorce+ J_sex_before_marriage+
                    J_suicide+ J_beat_wife+ J_parents_beating_children+ 
                    J_violence
                    ,factors=3,method="mle",rotation="varimax",data=zwvs14, scores = "regression")

print(efa_vmx,cutoff=0)

efa_vmx$scores

#=================================#
#  plot of loadings with Varimax  #
#=================================#
par(mfrow=c(1,3))

par(pty="s")
plot(efa_vmx$loadings,xlim=c(-10,10),ylim=c(-1,1),cex=2, col="blue")
pointLabel(efa_vmx$loadings,rownames(efa_vmx$loadings),cex=1)
abline(h=0, col="red", lwd=1, lty=2)
abline(v=0, col="red", lwd=1, lty=2)

plot(efa_vmx$loadings[,1],efa_vmx$loadings[,3],xlim=c(-10,10),
     ylim=c(-1,1),cex=2, col="blue", xlab="Factor 1", ylab="Factor 3")
pointLabel(efa_vmx$loadings,rownames(efa_vmx$loadings),cex=1)
abline(h=0, col="red", lwd=1, lty=2)
abline(v=0, col="red", lwd=1, lty=2)

plot(efa_vmx$loadings[,2],efa_vmx$loadings[,3],xlim=c(-10,10),
     ylim=c(-1,1),cex=2, col="blue", xlab="Factor 2", ylab="Factor 3")
pointLabel(efa_vmx$loadings,rownames(efa_vmx$loadings),cex=1)
abline(h=0, col="red", lwd=1, lty=2)
abline(v=0, col="red", lwd=1, lty=2)

## computing communalities
round(1-efa_vmx$uniqueness,3)

## printing loadings
print(efa_vmx$loadings,cutoff=0,digits=3)

## compute residual correlations
cormat<-cor(wvs14)
resid<-cormat-(crossprod(t(efa_vmx$loadings))+diag(efa_vmx$uniquenesses))
resid
resid1 <- as.data.frame(resid)
#Exporting residuals correlations to Excel 
library(writexl)
write_xlsx(resid1,"H:/Masters_of_Statistics/Semester_3/Multivariate/Assignments/1//residvmx.xlsx")


##compute proportion of residual correlations with absolute value larger than 0.05 below the diagonal
n<-sum(ifelse(abs(resid)>0.05,1,0))/2
print(n/(14*13/2))

#====================================#
#    Oblique Rotation                #
#====================================#

# extract 4 factors and use oblique rotation
efa_obl <- fa(zwvs14,3,rotate="oblimin",fm="mle")
print(efa_obl,cutoff=0)

# residual correlation
resid2<-cor(zwvs14)-
  (efa_obl$loadings%*%efa_obl$Phi%*%t(efa_obl$loadings)+
   diag(efa_obl$uniquenesses))

#compute proportion of residual correlations with absolute value larger than 0.05 below the diagonal

n2<-sum(ifelse(abs(resid2)>0.05,1,0))/2
print(n2/(14*13/2))

#=================================================#
#  Saving and Visualizing Factors' Distribution   #
#=================================================#
par(mfrow=c(1,1))

#======================================#
# Distributions with Varimax Rotation  #
#======================================#

efa_vmx$scores
scores_vmx<-cbind(wvs[,32:33],efa_vmx$scores)
View(scores_vmx)


par(pty="s")
par(las = 1,cex=0.6,cex.main=1.5) # all axis labels horizontal
par(oma=c(1,4,1,1)) ## increase space at the left of the figure for labels

boxplot(scores_vmx$Factor1~scores_vmx$country,horizontal=TRUE,xlab="Factor Scores",ylab="",main="F1: Sexuality or Interpersonal Matters")
boxplot(scores_vmx$Factor2~scores_vmx$country,horizontal=TRUE,xlab="Factor Scores",ylab="",main="F2: Financial Affairs")
boxplot(scores_vmx$Factor3~scores_vmx$country,horizontal=TRUE,xlab="Factor Scores",ylab="",main="F3: Domestic Violence")

#=====================================#
# Distributions with Oblique Rotation #
#=====================================# 
scores_obl<-cbind(wvs[,32:33],efa_obl$scores)
head(scores_obl)

par(pty="s")
par(las = 1,cex=0.6,cex.main=1.5) # all axis labels horizontal
par(oma=c(1,4,1,1)) ## increase space at the left of the figure for labels

boxplot(scores_obl$ML1~scores_obl$country,horizontal=TRUE,xlab="Factor Scores",ylab="",main="F1: Economic Affairs")
boxplot(scores_obl$ML2~scores_obl$country,horizontal=TRUE,xlab="Factor Scores",ylab="",main="F2: Sexuality or Interpersonal Matters")
boxplot(scores_obl$ML3~scores_obl$country,horizontal=TRUE,xlab="Factor Scores",ylab="",main="F3: Domestic Violence")

