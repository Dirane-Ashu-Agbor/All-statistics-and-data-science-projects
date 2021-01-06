require("lavaan")
library("GPArotation")
library("psych")
library("maptools")
require("semPlot")

load("C:/Users/oziel/Desktop/KU Leuven material/Classes Third Semester/Multivariate Statistics/wvs(1).Rdata")
data1=wvs[wvs$country=="Netherlands"|wvs$country=="Malaysia",16:33]
attach(data1)
na.omit(data1)

#specify model
sem1<-'#measurement model
        FA =~J_claiming_benefits+J_avoiding_fare+J_stealing_property+J_cheating_taxes+J_accept_bribe
        SIM =~J_homosexuality+J_prostitution+J_abortion+J_divorce+J_sex_before_marriage+J_suicide
        DV =~J_beat_wife+J_parents_beating_children+J_violence
        Religiousness=~R_attend_religious_services+R_pray+R_importance_God


#structural model
FA ~Religiousness
SIM ~Religiousness
DV ~Religiousness

'





#estimate model
fitsem2=sem(sem1, data = data1, sample.nobs=2473)
summary(fitsem2)
#path diagram 
win.graph()
pathdiagram<-semPaths(fitsem2,whatLabels="std", intercepts=FALSE, style="lisrel",
                      nCharNodes=0, 
                      nCharEdges=0,
                      curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)  
#standardized solution
standardizedSolution(fitsem2)
fitmeasures(fitsem2,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
MI=modificationIndices(fitsem2)

#By group analysis

fitsem3=sem(sem1, data = data1, sample.nobs=2473, group="country")
summary(fitsem3)
standardizedsolution(fitsem3)
fitmeasures(fitsem3,c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
pathdiagram<-semPaths(fitsem3,whatLabels="std", intercepts=FALSE, style="lisrel",
                      nCharNodes=0, 
                      nCharEdges=0,
                      curveAdjacent = TRUE,title=TRUE, layout="tree2",curvePivot=TRUE)
anova(fitsem2,fitsem3)
