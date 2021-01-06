#PCA
#clearing environment loading data
rm(list=ls())
detach("package:ggbiplot", unload=TRUE)
load("C:/Users/kebro/OneDrive/KU_Leuven/KUL Multiv/wvs(1).Rdata")
wvs=wvs[complete.cases(wvs),]

#necessary packages
#may need to install ggbiplot from github through the commented lines of code
#it's a lot better than the default biplot function provided in r

#install.packages("devtools")
#library(devtools)
#install_github("vqv/ggbiplot")
library(tidyverse)

#PCA Data Prep
wvs_1.10=wvs%>%select(c(1:10,33))%>%
  mutate(creative_s=scale(V_creative),
         rich_s=scale(V_rich),
         secure_s=scale(V_secure),
         spoil_oneself_s=scale(V_spoil_oneself),
         do_good_s=scale(V_do_good),
         be_successful_s=scale(V_be_successful),
         exciting_life_s=scale(V_exciting_life),
         behave_properly_s=scale(V_behave_properly),
         protect_environment_s=scale(V_protect_environment),
         tradition_s=scale(V_tradition))%>%
  select(-c(1:10))

#Data Prep 2: 34x10 table of country means
dec_places=3
country_means=wvs_1.10%>%
  group_by(country)%>%
  summarise(mean_creative=round(mean(creative_s),dec_places),
            mean_rich=round(mean(rich_s),dec_places),
            mean_secure=round(mean(secure_s),dec_places),
            mean_spoil_oneself=round(mean(spoil_oneself_s),dec_places),
            mean_do_good=round(mean(do_good_s),dec_places),
            mean_be_successful=round(mean(be_successful_s),dec_places),
            mean_exciting_life=round(mean(exciting_life_s),dec_places),
            mean_behave_properly=round(mean(behave_properly_s),dec_places),
            mean_protect_environemnt=round(mean(protect_environment_s),dec_places),
            mean_tradition=round(mean(tradition_s),dec_places))
head(country_means)

#correlation plot
library(ggcorrplot)
corr=cor(wvs_1.10[,-1])
ggcorrplot(corr)

#pca and summary statistics
data_pcs=prcomp(wvs_1.10[,-1],scale=F)
summary(data_pcs)

#scree plots of the PC eigenvector matrix
library(ggbiplot)
ggscreeplot(data_pcs,"pev")
ggscreeplot(data_pcs,"cev")

#PCA:Biplot pc1:2
ggbiplot(data_pcs,obs.scale = 1, var.scale = 1,circle = T,
         varname.size = 4,alpha=.5)+xlim(-10,10)+ylim(-7,7)

#PCA:Biplot pc1:3
ggbiplot(data_pcs,obs.scale = 1, var.scale = 1,circle = T,
         varname.size = 4,alpha=.5,choices = c(1,3))+xlim(-10,10)+ylim(-8,8)

#PCA:Biplot pc2:3
ggbiplot(data_pcs,obs.scale = 1, var.scale = 1,circle = T,
         varname.size = 4,alpha=.5,choices = 2:3)+xlim(-7,7)+ylim(-6,6)

#PCA biplot countries grouped
ggbiplot(data_pcs,obs.scale = 1, var.scale = 1,
         groups=wvs_1.10$country,circle = T,ellipse=T)+
  scale_color_discrete(name="")+
  theme(legend.direction = "vertical", legend.position = "left")+
  xlim(-6,10)+ylim(-5,7)

#assign each country a region and assign that region to an observation
region_finder=function(x){
  if(x %in% c("Armenia","Belarus","Estonia","Netherlands",
              "Poland","Romania","Russia","Slovenia","Sweden",
              "Ukraine")){
    return("Europe")
  }
  if(x %in% c("Australia","New Zealand")){
    return("Oceania")
  }
  if(x %in% c("Chile","Colombia","Ecuador","Mexico","Peru","Uruguay")){
    return("Latin America")
  }
  if(x %in% c("Azerbaijan","Cyprus","Libya","Uzbekistan","Pakistan",
              "Kyrgyzstan","Kazakhstan")){
    return("Middle East")
  }
  if(x %in% c("China","Malaysia","Singapore","Taiwan","Philippines")){
    return("East Asia")
  }
  if(x%in% c("Ghana","Nigeria","Rwanda","Zimbabwe")){
    return("Africa")
  }
  
}
region=rep("",32684)
for (i in 1:32684) {
  region[i]=region_finder(wvs_1.10$country[i])
}
wvs_1.10_regions=wvs_1.10%>%
  mutate(region=region)

#biplot observations grouped by region
ggbiplot(data_pcs,obs.scale = 1, var.scale = 1,
         groups=wvs_1.10_regions$region,circle = T,ellipse=T)+
  scale_color_discrete(name="")+xlim(-6,10)+ylim(-5,7)+
  theme(legend.direction = "vertical", legend.position = "left")


#this chunk generates comparative biplots for one
#specified primary nation vs all other nations and saves them to a folder
#on your computer
primary_country="Netherlands"
for(i in unique(wvs_1.10$country)){
  country_data=wvs[wvs$country%in%c(i,primary_country),]
  country_data_scale=country_data%>%select(c(1:10,33))%>%
    mutate(creative_s=scale(V_creative),
           rich_s=scale(V_rich),
           secure_s=scale(V_secure),
           spoil_oneself_s=scale(V_spoil_oneself),
           do_good_s=scale(V_do_good),
           be_successful_s=scale(V_be_successful),
           exciting_life_s=scale(V_exciting_life),
           behave_properly_s=scale(V_behave_properly),
           protect_environment_s=scale(V_protect_environment),
           tradition_s=scale(V_tradition))%>%
    select(-c(1:10))
  pcs_country=prcomp(country_data_scale[,-1],scale. = F)
  ggbiplot(pcs_country,obs.scale = 1,groups=country_data_scale[,1],var.scale = 1,circle = T,
           varname.size = 4,alpha=.5)+ggtitle(paste(i,"vs.",primary_country))+xlim(-11,11)+ylim(-10,10)
  filename=paste0(i,"_",primary_country,"Biplot.png")
  ggsave(filename = filename,
         path="C:/Users/kebro/OneDrive/KU_Leuven/KUL Multiv/CountryBiplots")
}  
detach("package:ggbiplot", unload=TRUE)
