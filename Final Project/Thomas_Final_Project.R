setwd("C:/GitHub/morgan/Final Project")

#arthrain = arthropod and rainfall data (Wise_DH_Lensing_JR_2019)
#soiltemp = soil minimum and maximum temperature data (KY_Soil_Data)

install.packages("readxl")
library(readxl)

soiltemp.tibble <- read_excel("KY Soil Data.xlsx")
soiltemp <- as.data.frame(soiltemp.tibble)
head(soiltemp)

arthrain.tibble <- read_excel("Wise_DH_Lensing_JR_2019.xlsx")
arthrain <- as.data.frame(arthrain.tibble)
head(arthrain)

df <- merge(soiltemp, arthrain, by = "period")
df
df$roundmaxtemp <- format(round(df$avgmaxtemp, 0), nsmall = 0)
df$roundmintemp <- format(round(df$avgmintemp, 0), nsmall = 0)

library(spdep)
library(adespatial)
library(vegan)
  
KY_Soil_Data.csv <- read.csv("KY_Soil_Data.csv", header=T)
Wise_DH_Lensing_JR_2019.csv <- read.csv("Wise_DH_Lensing_JR_2019.csv", header=T)
KY_Soil_Data.mat <- as.matrix(KY_Soil_Data.csv)
Wise_DH_Lensing_JR_2019.mat <- as.matrix(Wise_DH_Lensing_JR_2019.csv)

#REDUNDANCY ANALYSES

#Soil Temperature Controlling Arthropods
soiltemp.rda <- rda(df[,9:89]~avgmaxtemp + avgmintemp, data=df)
soiltemp.rda
RsquareAdj(soiltemp.rda)
anova(soiltemp.rda, perm.max = 10000)
summary(soiltemp.rda)

#0.2917 constrained, 0.7083 unconstrained
#RsquaredAdj = 0.2741938
#p = 0.001
#Proportion Explained RDA1 - 28.2% explained
#Proportion Explained RDA2 - 1% explained

#About 29.2% of the variance in arthropod abundance is explained by soil temperature. Because this is greater than the variance explained by rainfall levels, it has a more significant impact on arthropod abundance.
#This leaves about 70.8% of unexplained variance in arthropod abundance.

plot(soiltemp.rda)
plot(soiltemp.rda, type = "n", ylim = c(1,2), display = c("sites", "species"), main = "Soil Temperature and Arthropods", xlab = "RDA1 (0.282%)", ylab = "RDA2 (0.010%)")
text(soiltemp.rda, display = "sites", labels = as.numeric(df$roundmintemp, df$roundmaxtemp), col = "blue3", cex = 0.65)
text(soiltemp.rda, display = "species", labels = as.character(colnames(df[,9:89])), col = "darkmagenta", cex = 0.65)

#Rainfall Controlling Arthropods
rainfall.rda <- rda(df[,9:89]~trt, data=df)
rainfall.rda
RsquareAdj(rainfall.rda)
anova(rainfall.rda, perm.max = 10000)
summary(rainfall.rda)

#0.08946 constrained, 0.91054 unconstrained
#RsquaredAdj = 0.06697337
#p = 0.009
#Proportion Explained RDA1 - 8.1% explained
#Proportion Explained RDA2 - 0.8% explained

#About 8.95% of the variance in arthropod abundance is explained by rainfall level.
#This leaves about 91.1% of unexplained variance in arthropod abundance.

plot(rainfall.rda)
plot(rainfall.rda, type = "n", ylim = c(1,2), display = c("sites", "species"), main = "Rainfall and Arthropods", xlab = "RDA1 (0.082%)", ylab = "RDA2 (0.008%)")
text(rainfall.rda, display = "sites", labels = as.character(df$trt), col = "blue3", cex = 0.65)
text(rainfall.rda, display = "species", labels = as.character(colnames(df[,9:89])), col = "darkmagenta", cex = 0.65)

#GLM AND GAM MODELS

library(MASS)
library(MuMIn)
library(mgcv)

#Khypo
glm.Khypo <- glm(Khypo~trt + avgmaxtemp + avgmintemp, family = gaussian, data = df)
summary(glm.Khypo)
plot(glm.Khypo$residuals)

#AIC = 1312.9; all variables have same p-values

gam.Khypo <- gam(Khypo~trt + avgmaxtemp * avgmintemp, family = poisson, data = df)
summary(gam.Khypo)
plot(gam.Khypo$residuals)
AIC(gam.Khypo)
hist(df$Khypo, breaks = 50)

#Adjusted R-Squared = 0.445
#Deviance Explained = 69%
#All variables have same p-values

#Kacarina
glm.Kacarina <- glm(Khypo~trt + avgmaxtemp + avgmintemp, family = poisson, data = df)
summary(glm.Kacarina)
plot(glm.Kacarina$residuals)

#AIC = 17431; all variables had the same p-values

gam.Kacarina <- gam(Kacarina~trt + avgmaxtemp * avgmintemp, family = gaussian, data = df)
summary(gam.Kacarina)
plot(gam.Kacarina$residuals)
AIC(gam.Kacarina)
hist(df$Kacarina)
#Adjusted R-Squared = 0.628
#Deviance Explained = 70.1%
#All variables have same p-values

plot(Kacarina~avgmaxtemp, data = df)
plot(Kacarina~avgmintemp, data = df)

plot(Khypo~avgmintemp, data = df)
plot(Khypo~avgmaxtemp, data = df)
