#loading packages
library(car)

#set working directory
setwd()

#TASK1

flycatpop=read.csv(file="Biomdata_Task1.csv", header = TRUE, sep = ";", dec=",", fill = TRUE, comment.char="")

#check structure
str(flycatpop)


#making collinar model of previous predation and previous density (COLLINEAR)
prevflypop_model = lm(dens ~prevpred+prevdens+prevpred:prevdens, data= flycatpop)
summary(prevflypop_model)

#statistical result with anova
Anova(prevflypop_model, Type = "III")
coef(prevflypop_model)

#making separate models for previous predation and previous density (SEPARATE)
prevpred_model = lm(dens ~prevpred, data= flycatpop)
summary(prevpred_model)
Anova(prevpred_model, Type = "III")
coef(prevpred_model)

prevdens_model = lm(dens ~prevdens, data= flycatpop)
Anova(prevdens_model, Type = "III")
summary(prevdens_model)
coef(prevdens_model)

#testing effect of previous predation on previous density
prevpreddens_model = lm(prevdens~prevpred, data= flycatpop)
summary(prevpreddens_model)
Anova(prevpreddens_model, Type = "III")
coef(prevpreddens_model)

#adding the new residuals to the dataset as a column to the data set
flycatpop2=cbind(flycatpop,resld=resid(prevpreddens_model))
colnames(flycatpop2)

#Calculate the model with residuals, thereby removing collinearity (RESIDUAL)
resflycatpop_model=lm(dens ~ prevpred + resld, data = flycatpop2)
Anova(resflycatpop_model)
coef(resflycatpop_model)




#TASK2
#import data
Aselluspops=read.csv(file="Behav_Task2.csv", header = TRUE, sep = ",", dec=",", fill = TRUE, comment.char="")

#check data structure
str(Aselluspops)

#change to factors
Aselluspops$sex<-as.factor(Aselluspops$sex)
Aselluspops$population<-as.factor(Aselluspops$population)
Aselluspops$light.regime<-as.factor(Aselluspops$light.regime)

str(Aselluspops)

#plot
hist(Aselluspops$exploration)
boxplot(exploration ~ population,data=Aselluspops)
boxplot(exploration ~ sex,data=Aselluspops)
boxplot(exploration ~ light.regime,data=Aselluspops)

#load libaries
library(car); library(lme4); library(emmeans); library(MASS)

#making glm model
Asellmodel<-glm.nb(exploration ~ population + sex + light.regime,data=Aselluspops)

summary(Asellmodel)

#statistical analysis 
Anova(Asellmodel,type="III")
#difference in population and sex

#post hoc test
emmeans(Asellmodel,list(pairwise~population),adjust="tukey",type="response")
#gotes lake differ from dunakeszi moor and a from molnar janos cave
emmeans(Asellmodel,list(pairwise~sex),adjust="tukey",type="response")
#female has lower exploration rate than male


#TASK3
#load libraries
library(vegan)
library(scales)
library(ggplot2)
library(ggforce)
library(ggrepel)
library(dplyr)
library(dendextend)
library(devtools)
library(pairwiseAdonis) 


#import data
Soildata=read.csv(file="Soildata_Task3.txt", row.names=1, header = T,sep = "\t")

str(Soildata)
#standardization is not needed -> all data on the same scale (0-100) and all represent the same (coverage)
Soildata[,1:3]=lapply(Soildata[,1:3],as.factor)
str(Soildata)

# PCA: Principal Component Analysis----
plantcov=rda(Soildata[,4:60])
summary(plantcov)
# Check, how good is the resulted PCA
summary(plantcov)$cont$importance
screeplot(plantcov,bstick = T,type = c("barplot", "lines"))

# Visualisations
ordipointlabel(plantcov)
biplot(plantcov)
barplot(plantcov$CA$v[,1],main="PC1 scores")
barplot(plantcov$CA$v[,2],main="PC2 scores")

# Check details of PCA
plantcov$CA$v
(PC1_O=round(summary(plantcov)$cont$importance[2,1]*100,digits=1))
(PC2_O=round(summary(plantcov)$cont$importance[2,2]*100,digits=1))

# Extract scores from PCA
plantcov_xy <- scores(plantcov, display = c("sites","species"), scaling = 2)
plantcov_xy_biplot=as.data.frame(plantcov_xy$species)
plantcov_xy_biplot=cbind(row.names(plantcov_xy_biplot),plantcov_xy_biplot)
colnames(plantcov_xy_biplot)[1]="Variables"
plantcov_xy=cbind(Soildata[,1:3],plantcov_xy$sites)

#PCA plot
Plot_plantcov=ggplot(plantcov_xy,aes(x=PC1, y=PC2)) + 
  geom_point(aes(x=PC1, y=PC2,shape=Treat1,colour=Treat2),size=4) + # add the point markers
  scale_shape_manual(values = c(7,16)) + # shape of points
  scale_colour_manual(values = c("black","blue","purple","red"),
                      breaks=c("W1","W2","W3","W4")) + # colour of points
  labs(title="PCA for unstandardized data",
       x=paste("PC1 (",round(PC1_O,digits = 1),"%)",sep=""),
       y=paste("PC2 (",round(PC2_O,digits = 1),"%)",sep=""))
Plot_plantcov
Plot_plantcov+geom_mark_hull(aes_string(fill = Soildata$Treat1,label = Soildata$Treat1),concavity = 10,alpha=0.1,show.legend = F,con.cap = 0)
Plot_plantcov+geom_mark_hull(aes_string(fill = Soildata$Treat2,label = Soildata$Treat2),concavity = 10,alpha=0.1,show.legend = F,con.cap = 0)

# PCA biplot with ggplot
Plot_plantcov_biplot=ggplot(plantcov_xy,aes(x=PC1, y=PC2)) + 
  geom_point(aes(x=PC1, y=PC2,shape=Treat1,colour=Treat2),size=4) + # add the point markers
  scale_shape_manual(values = c(7,16)) + # shape of points
  scale_colour_manual(values = c("black","blue","purple","red"),
                      breaks=c("W1","W2","W3","W4")) + 
  geom_text_repel(data=plantcov_xy_biplot,aes_string(x = "PC1", y = "PC2",label="Variables"),size = 3, color= "black",fontface = 2)+
  geom_segment(data=plantcov_xy_biplot, mapping=aes(x=0, y=0, xend=PC1, yend=PC2), arrow=arrow(length = unit(0.1, "inches")), linewidth=0.7,color="orange") +
  labs(title="PCA for range unstandardized data",
       x=paste("PC1 (",round(PC1_O,digits = 1),"%)",sep=""),
       y=paste("PC2 (",round(PC2_O,digits = 1),"%)",sep=""))
Plot_plantcov_biplot

#permanova (statistical)
adonis2(Soildata[,4:60]~Treat1,method = "euclidean",permu = 10000, data=Soildata)
adonis2(Soildata[,4:60]~Treat2,method = "euclidean",permu = 10000, data=Soildata)
adonis2(Soildata[,4:60]~Block,method = "euclidean",permu = 10000, data=Soildata)
#treat2 have significant effect
#treat1 has  significance
#the block has no significance 


#post hoc test for significant results(no need for treat1-> binomial)
pairwise.adonis(Soildata[,4:60],Soildata$Treat2)

# PCA plot with ggplot for describing goodness of orientation
Plot_plantcov_par=ggplot(plantcov_xy,aes(x=PC1, y=PC2)) + 
  geom_point(aes(x=PC1, y=PC2,shape=Treat1,colour=Treat2),size=4) + # add the point markers
  scale_shape_manual(values = c(7,16)) + # shape of points
  scale_colour_manual(values = c("black","blue","purple","red"),
                      breaks=c("W1","W2","W3","W4")) + # colour of points
  geom_text_repel(data=plantcov_xy,aes_string(x = "PC1", y = "PC2",label="Block"),size = 3, color = "black",fontface = 2)+
  labs(title="PCA for unstandardized data",
       x=paste("PC1 (",round(PC1_O,digits = 1),"%)",sep=""),
       y=paste("PC2 (",round(PC2_O,digits = 1),"%)",sep=""))
Plot_plantcov_par
Plot_plantcov_par+geom_mark_hull(aes_string(fill = Soildata$Block,label = Soildata$Block),concavity = 10,alpha=0.1,show.legend = F,con.cap = 0)




