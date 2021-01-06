
# manually upload data into R
setwd("C:/Users/diran/Downloads/Multivariate Statistics/Assignment 1")
load("wvs.Rdata")
mydata<-wvs
colnames(mydata)
table(mydata$country)


# subset
sem <- subset.data.frame(mydata, select=c("country","J_claiming_benefits","J_avoiding_fare","J_stealing_property",
                                          "J_cheating_taxes","J_accept_bribe","J_homosexuality","J_prostitution",
                                          "J_abortion","J_divorce","J_sex_before_marriage","J_suicide","J_beat_wife",
                                          "J_parents_beating_children","J_violence"))
sem2<-sem[sem$country %in% c("Malaysia", "Netherlands"), ]

# subset dataset without country column
sem3<-sem2[,2:15]

# check missing values
sum(is.na(sem3))


library(psych)  ######PCA/EFA amongst many other things!
library(readxl) ######reads excel
library(REdaS) ######produces KMO and Bartletts test
library(maptools)
library(GPArotation)
library(semTools)
library(semPlot)

#------------ WE START WITH EXPLORATORY FACTOR ANALYSIS--------------------#

#compute covariance matrix and correlation matrix
covmat<-cov(sem3)
colnames(covmat)<-c("","","","","","","","","","","","","","")
round(covmat,2)

cormat<-cor(sem3)
colnames(cormat)<-c("","","","","","","","","","","","","","")
round(cormat,2)

#############

bart_spher(sem3) ###### produces Bartletts test of spherecity (you want this to be significant)
KMO(sem3)       ###### Kaiser-Meyer-Olkin measure, you want to be above .7

########## using Kaisers rule, Eigenvalues>1 represent valid factors

### set nfactors to n items, in this case there are 14 items so we state nfactors=14
#####oblimin is selected as the rotation although this is default for factor analysis (variamx is default for pca)
##orthagonal roatations availible ="none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" "bifactor" 
##oblique roatations availible "Promax", "promax", "oblimin", "simplimax", "bentlerQ, "geominQ" "biquartimin" "cluster" 

fa(covmat, nfactors = 14, rotate =  "oblimin" )  


fa(covmat, nfactors = 3, rotate =  "oblimin" )

################### you can produce a figure 

M1<-fa(sem3, nfactors = 3, rotate =  "oblimin" ) ##save the analysis as the object m1
fa.diagram(M1,main=" Three factor solution")  ## produce a figure with the title "" note fa.diagram still works for PCA

# write factor loadings on excel csv file
M1csv<-round(unclass(M1$loadings),2)
M1csv

write.csv(M1csv,"C:/Users/diran/Downloads/Multivariate Statistics/Assignment 1/efa-factor-loadings.csv")


#---------------------- CONFIRMATORY FACTOR ANALYSIS---------------------------#

library(lavaan) ### does LAtent VAriable ANalysis
library(lavaanPlot) ### make plots

############### turn off scientfic notation

options(scipen = 999)

### fit your model in which each item is constrained to have a loading on one factor
### The =~ (equals tilde) symbol means measured by so factor one is measured by ATGC1, ATGC2 and ATGC3 and so on

model1 <- 'civic immorality =~ NA*J_stealing_property + J_cheating_taxes + J_claiming_benefits + J_avoiding_fare + J_accept_bribe
          sexual openmindedness =~ NA*J_abortion + J_sex_before_marriage + J_divorce + J_homosexuality + J_prostitution + J_suicide
          domestic violence =~ NA*J_beat_wife + J_parents_beating_children + J_violence
civic immorality ~~1*civic immorality
sexual openmindedness ~~1*sexual openmindedness
domestic violence ~~1*domestic violence'


fitbefore_mi <- cfa(model1, sample.cov = covmat, sample.nobs = 2473)                         #### fit the above model             
summary(fitbefore_mi, standardized=TRUE, ci=TRUE, fit.measures=TRUE) #### display the results. Standardised = TRUE gives beta values (std.all column), ci=TRUE gives 95%CIs

#ask for standardized solution
standardizedSolution(fitbefore_mi)

#modification indicies tell you if your model can be improved by using covariances, 
#a modification index is the chi squared reduction adding a covariance will have, 
# we have set it to ten. Covariance should only be added were logical  

modif<-modindices(fitbefore_mi, sort = TRUE)

# write model modification indices on excel csv file
modif

write.csv(modif,"C:/Users/diran/Downloads/Multivariate Statistics/Assignment 1/modif-indices.csv")

## plot
semPaths (fitbefore_mi ," model "," stand ",style =" LISREL ",rotation =1, edge.color =" black ",edge.label.cex =1)


## Modification indicates that we could improve model by adding covariances
## add two tilde's between the two add a covariance i.e. ~~

model2 <- 'civic morality =~ NA*J_stealing_property + J_cheating_taxes + J_claiming_benefits + J_avoiding_fare + J_accept_bribe
          sexual openmindedness =~ NA*J_abortion + J_sex_before_marriage + J_divorce + J_homosexuality + J_prostitution + J_suicide
          domestic violence =~ NA*J_beat_wife + J_parents_beating_children + J_violence
civic morality ~~1*civic morality
sexual openmindedness ~~1*sexual openmindedness
domestic violence ~~1*domestic violence
                 J_sex_before_marriage ~~ J_homosexuality
                 J_sex_before_marriage ~~ J_suicide
                      J_prostitution ~~ J_suicide
                       J_abortion ~~ J_sex_before_marriage
                        J_abortion ~~ J_suicide
                       J_claiming_benefits ~~ J_avoiding_fare
                       J_claiming_benefits ~~ J_accept_bribe'
# we also tried adding covariance between J_beat_wife ~~ J_violence but we obtain warning about from R about matrix not being positive definite

fitafter_mi <- cfa(model2, sample.cov = covmat, sample.nobs = 2473)
summary(fitafter_mi, ci=TRUE, standardized=TRUE, fit.measures=TRUE)

#ask for standardized solution
stdsolution<-standardizedSolution(fitafter_mi)

# write model standardized solution on excel csv file
stdsolution

write.csv(stdsolution,"C:/Users/diran/Downloads/Multivariate Statistics/Assignment 1/standardized-solution-improved.csv")

# goodness of fit measures for model before and after modification indices

CFAmodel1<-fitmeasures(fitbefore_mi,c("chisq","pvalue","df","cfi","tli","rmsea","srmr"))
CFAmodel2<-fitmeasures(fitafter_mi,c("chisq","pvalue","df","cfi","tli","rmsea","srmr"))
fit1<-rbind(CFAmodel1,CFAmodel2)
round(fit1,3)

# write model goodness of fit measures on excel csv file
Goodnessfit<-round(fit1,3)
Goodnessfit

write.csv(Goodnessfit,"C:/Users/diran/Downloads/Multivariate Statistics/Assignment 1/goodness-offit.csv")

## plot
semPaths (fitafter_mi ," model "," stand ",style =" LISREL ",rotation =1, edge.color =" black ",edge.label.cex =1)

## all of these things are standard, you fit the model you have created 'fitcov' in this case. I have asked for coefficients
## coefs=T and for these to be standardized stand =T.
## if you want covariances between facts add ,covs=T, 

# plot displaying factors and no covariances between factors
lavaanPlot(model = fitafter_mi, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"),
           coefs = T, stand=T)
# plot displaying covariances between factors
lavaanPlot(model = fitafter_mi, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"),
           coefs = T, stand=T, covs = T)


#-------------------------------MULTI-GROUP ANALYSIS---------------------------------#

model3 <- 'civic morality =~ 1*J_stealing_property + J_cheating_taxes + J_claiming_benefits + J_avoiding_fare + J_accept_bribe
          sexual openmindedness =~ 1*J_abortion + J_sex_before_marriage + J_divorce + J_homosexuality + J_prostitution + J_suicide
          domestic violence =~ 1*J_beat_wife + J_parents_beating_children + J_violence
civic morality ~~civic morality
sexual openmindedness ~~sexual openmindedness
domestic violence ~~domestic violence
             J_sex_before_marriage ~~ J_homosexuality
                 J_sex_before_marriage ~~ J_suicide
                      J_prostitution ~~ J_suicide
                       J_abortion ~~ J_sex_before_marriage
                        J_abortion ~~ J_suicide
                       J_claiming_benefits ~~ J_avoiding_fare
                       J_claiming_benefits ~~ J_accept_bribe'

#####  Use raw data sem2 above, for multigroup analysis because it contains group variable "country" 

#configural measurement invariance model
config<-cfa(model3, data=sem2, group="country")
summary(config,fit.measures=TRUE)
standardizedSolution(config)

#metric measurement invariance model
metric<-cfa(model3, data=sem2, group="country", group.equal="loadings")
summary(metric,fit.measures=TRUE)
standardizedSolution(metric)

#strong measurement invariance model
strong<-cfa(model3, data=sem2, group="country", group.equal=c("loadings","intercepts"))
summary(strong,fit.measures=TRUE)

#compare models using a likelihood ratio test
anova(config,metric)
anova(config,strong)
#summarize fitmeasures
fitconfig<-fitmeasures(config,c("chisq","pvalue","df","cfi","tli","rmsea","srmr"))
fitmetric<-fitmeasures(metric,c("chisq","pvalue","df","cfi","tli","rmsea","srmr"))
fitstrong<-fitmeasures(strong,c("chisq","pvalue","df","cfi","tli","rmsea","srmr"))
fitmgcfa<-rbind(fitconfig,fitmetric,fitstrong)
rownames(fitmgcfa)<-c("configural","metric","strong")
round(fitmgcfa,3)

# write model goodness of fit measures on excel csv file
Goodnessfitmgcfa<-round(fitmgcfa,3)
Goodnessfitmgcfa

write.csv(Goodnessfitmgcfa,"C:/Users/diran/Downloads/Multivariate Statistics/Assignment 1/multigroup-analysis.csv")
