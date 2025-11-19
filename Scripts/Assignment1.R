library(dplyr)
library(rstatix)
library(car)

#Task1
# Data about mushrooms (3 group), each group have 40 replicate (large sample size)
# data structure: 2 columns, Mushroom_yield and Mushroom_strain
# Question. It has differences among the 3 strain?

#Loading data
mushrooms<-read.table(file="BM_U17M7U_Task1.txt", header = T, sep='\t', dec = ".", na.strings = "NA", fill=T)


#visualise data
boxplot(Mushroom_yield~Mushroom_strain, data=mushrooms,xlab = "Mushroom strains", ylab="Mushroom yield (kg/bag)", boxwex=0.3)
#no outliers

# one-way ANOVA testing (unrelated samples, more groups, continous variable)

#H0: The is no difference between the groups mean.
#H1: At least one of the group has different mean.

# - Assumptions
#- Normality
shapiro.test(mushrooms[mushrooms$Mushroom_strain=="H35",1])
shapiro.test(mushrooms[mushrooms$Mushroom_strain=="L357",1])
shapiro.test(mushrooms[mushrooms$Mushroom_strain=="P70",1])
# each p value above 0.05 -> they are normally distributed

#Homogeneity of variance
library(car)
bartlett.test(Mushroom_yield~Mushroom_strain, mushrooms)
# p value above 0.05 -> the data is homogeneous
leveneTest(Mushroom_yield~Mushroom_strain, mushrooms)
# Pr(>F) is above 0.05 ->the data is homogeneous-> we can do ANOVA

# Test statistic (ANOVA test)
oneway_anova <- aov(Mushroom_yield~Mushroom_strain, mushrooms)
anova(oneway_anova)

oneway.test(Mushroom_yield~Mushroom_strain, data = mushrooms, var.equal = F)

# Pr(>F) is under 0.05! we reject the h0 hypothesis -> there is difference at least one of the groups
#need a post hoc test to find which exactly differ

#Tukey's posthoc
TukeyHSD(oneway_anova, conf.level=0.95) # 5% of uncertainty 
# p adj -> 1 group differ from the other two

#Report: The ANOVA suggests there is statictically significant difference at least one of the group (F = 9.2228, num df = 2, denom df = 117, p-value = 0.0001908).
#the post hoc test (tukey) suggests the that the P70 strain statistically significantly differ from the other two (H35, L357) group (p adj is 0.0016860 and 0.0005049), and no statistically significantly difference between the H35, L357 groups (p adj is 0.9363689)





#Task2
# About heigh and femur length  correlation (large sample size)
#Question: what is the degree of linear association between them?
#Additional: making linear model to predict stature and present femoral length and stature

#Loading data
ost_collection<-read.table(file="BM_U17M7U_Task2.txt", header = T, sep='\t', dec = ".", na.strings = "NA", fill=T)

#Pearson correlation

# H0 there isn't linear correlation
# H1 there is linear correlation

#Plot the variable 
plot(ost_collection$Femur, ost_collection$Stature, xlab = "Femur length (mm)", ylab="Stature (cm)" )

#Assumptions for correlation
#Normality
shapiro.test(ost_collection$Stature)
shapiro.test(ost_collection$Femur)
# both have p value above 0.05 -> they are normally distributed

cor.test(ost_collection$Femur, ost_collection$Stature)
# correlation coefficient is 0.5966607 -> they are moderatly positively correlated

#A priori assumptions
shapiro.test(ost_collection$Stature)
shapiro.test(ost_collection$Femur)

#Building a linear model
model<-lm(Stature~Femur, data = ost_collection)
summary(model)

#model perfom better than you expected by chance (F-statistic: 54.17 on 1 and 98 DF,  p-value: 5.718e-11)
#regression model accounts for 35.6% of the variability of outcome measure (Multiple R-squared:  0.356)

#visualise model
plot(ost_collection$Femur, ost_collection$Stature, xlab="Femur (mm)", ylab="Stature (cm)")
abline(lm(Stature~Femur, data = ost_collection), col = "blue")

#Residual analysis
#Linearity, Randomness, Independence of the errors
plot(model, pch=16, col="blue",lwd=2, which=1) #randomly distributed, no periodicity
#Homoscedasticity
plot(model, pch=16, col="blue",lwd=2, which=3) #more or less horizontal line
#Normality
plot(model, pch=16, col="blue",lwd=2, which=2) #qq plot for analyzing the normality
shapiro.test(model$residuals) # not significant, p value> 0.05 -> normal distribution

plot(model)

#Prediction based on the model
#Predict a single value
#creating function
estimate<-function(x) {
  x<-model$coefficients[1]+model$coefficients[2]*x
  return(x)
}
#using 390mm of femur length for example prediction
estimate(390)

#the result is 167.4 cm -> the model is working



#Task3
# About GGT levels in group sample, comparing 2 group (smaller sample size)
# Question: is there any difference between the 2 groups?

#Loading data
blood_samples<-read.table(file="BM_U17M7U_Task3.txt", header = T, sep='\t', dec = ".", na.strings = "NA", fill=T)

#visualising
blood_samples_df <- as.data.frame(blood_samples)
boxplot(blood_samples_df, xlab = "Groups", ylab="GGT (IU/L)", boxwex=0.3)
#we can see outliers

# for 2 sample t-test
# H0:  mean level of GGT in Group1 (no liver disease) = mean level of GGT in Group2 (liver disease)
# H1: mean level of GGT in Group1 (no liver disease) != mean level of GGT in Group2 (liver disease)

#Assumptions
#Normality
shapiro.test(blood_samples$Group1)# p value < 0.05, no normal distribution! ->Wilcoxon rank sum test (Mann-Whitney U test)
shapiro.test(blood_samples$Group2)

#using Wilcoxon rank sum test (non normal distribution, small sample size, have outliers)
# do not care outliers as its a  non parametrics test

#new hypothesis needed

#H0: the distribution of the groups are identical
#H1: not identical distribution

#Test statistic
wilcox.test(blood_samples$Group1 ,blood_samples$Group2, paired=F, exact=T)# 2 numerical variable (group of interest) paired is false, exact is refer exact p value
# the p value <0.05 -> reject H0 -> the test indicate that there is statistically significant difference between the distribution of the two groups








