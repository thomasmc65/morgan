setwd("C:/GitHub/morgan/Final Project")

#arthrain = arthropod and rainfall data (Wise_DH_Lensing_JR_2019)
#soiltemp = soil minimum and maximum temperature data (KY_Soil_Data)

install.packages("readxl")
library(readxl)

#Reading in the soil temperature excel dataset and making it into a data frame.
soiltemp.tibble <- read_excel("KY Soil Data.xlsx")
soiltemp <- as.data.frame(soiltemp.tibble)
head(soiltemp)

#Reading in the arthropod and rainfall excel dataset and making it into a data frame.
arthrain.tibble <- read_excel("Wise_DH_Lensing_JR_2019.xlsx")
arthrain <- as.data.frame(arthrain.tibble)
head(arthrain)

#Merging the two datasets based on the period of sample collection and rounding the temperature values to integers.
df <- merge(soiltemp, arthrain, by = "period")
df
df$roundmaxtemp <- format(round(df$avgmaxtemp, 0), nsmall = 0)
df$roundmintemp <- format(round(df$avgmintemp, 0), nsmall = 0)

library(spdep)
library(adespatial)
library(vegan)

#Converting the excel data files into csv files to be used in RDA analyses.
KY_Soil_Data.csv <- read.csv("KY_Soil_Data.csv", header=T)
Wise_DH_Lensing_JR_2019.csv <- read.csv("Wise_DH_Lensing_JR_2019.csv", header=T)
KY_Soil_Data.mat <- as.matrix(KY_Soil_Data.csv)
Wise_DH_Lensing_JR_2019.mat <- as.matrix(Wise_DH_Lensing_JR_2019.csv)

#REDUNDANCY ANALYSES: Performed to determine whether soil temperatures or rainfall levels had a more significant impact on arthropod species.

#Soil Temperature Controlling Arthropods
soiltemp.rda <- rda(df[,9:89]~avgmaxtemp + avgmintemp, data=df)
soiltemp.rda
RsquareAdj(soiltemp.rda)
anova(soiltemp.rda, perm.max = 10000)
summary(soiltemp.rda)
#Constrained = 0.2917 = 29.2%
#Unconstrained = 0.7083 = 70.8%
#RsquaredAdj = 0.2741938
#p = 0.001
#Proportion Explained RDA1 = 0.2815 = 28.2% explained
#Proportion Explained RDA2 = 0.0102 = 1.02% explained

#About 29.2% of the variance in arthropod abundance is explained by soil temperature. Because this is greater than the variance explained by rainfall levels, it has a more significant impact on arthropod abundance.
#This leaves about 70.8% of unexplained variance in arthropod abundance.

plot(soiltemp.rda)
plot(soiltemp.rda, type = "n", ylim = c(1,2), display = c("sites", "species"), main = "Soil Temperature and Arthropods", xlab = "RDA1 (28.2%)", ylab = "RDA2 (1.02%)")
text(soiltemp.rda, display = "sites", labels = as.numeric(df$roundmintemp, df$roundmaxtemp), col = "blue3", cex = 0.65)
text(soiltemp.rda, display = "species", labels = as.character(colnames(df[,9:89])), col = "darkmagenta", cex = 0.65)

#Rainfall Controlling Arthropods
rainfall.rda <- rda(df[,9:89]~trt, data=df)
rainfall.rda
RsquareAdj(rainfall.rda)
anova(rainfall.rda, perm.max = 10000)
summary(rainfall.rda)
#Constrained = 0.08946 = 8.95%
#Unconstrained = 0.91054 = 91.1%
#RsquaredAdj = 0.06697337
#p = 0.009
#Proportion Explained RDA1 = 0.08145 = 8.15% explained
#Proportion Explained RDA2 = 0.008002 = 0.800% explained

#About 8.95% of the variance in arthropod abundance is explained by rainfall level.
#This leaves about 91.1% of unexplained variance in arthropod abundance.

plot(rainfall.rda)
plot(rainfall.rda, type = "n", ylim = c(1,2), display = c("sites", "species"), main = "Rainfall and Arthropods", xlab = "RDA1 (8.15%)", ylab = "RDA2 (0.800%)")
text(rainfall.rda, display = "sites", labels = as.character(df$trt), col = "blue3", cex = 0.65)
text(rainfall.rda, display = "species", labels = as.character(colnames(df[,9:89])), col = "darkmagenta", cex = 0.65)

#GLM AND GAM MODELS: Both were done for each species found to be significant in the RDA analyses above (Khypo and Kacarina).

library(MASS)
library(MuMIn)
library(mgcv)

#Khypo - Hypogastruridae (Spider) Kempson Sample

glm.Khypo <- glm(Khypo~trt + avgmaxtemp + avgmintemp, family = gaussian, data = df)
summary(glm.Khypo)
plot(glm.Khypo$residuals)
#AIC = 1312.9
#Most to least significant (p-values): avgmintemp (0.0551), avgmaxtemp (0.0862), trto (0.0868)

gam.Khypo <- gam(Khypo~trt + avgmaxtemp * avgmintemp, family = poisson, data = df)
summary(gam.Khypo)
plot(gam.Khypo$residuals)
AIC(gam.Khypo)
hist(df$Khypo, breaks = 50)
#AIC = 17133.71
#Adjusted R-Squared = 0.445
#Deviance Explained = 69%
#All variables have same p-values

#Kacarina - Acarina (Mite) Kempson Sample

glm.Kacarina <- glm(Khypo~trt + avgmaxtemp + avgmintemp, family = poisson, data = df)
summary(glm.Kacarina)
plot(glm.Kacarina$residuals)
#AIC = 17431
#All variables have same p-values

gam.Kacarina <- gam(Kacarina~trt + avgmaxtemp * avgmintemp, family = gaussian, data = df)
summary(gam.Kacarina)
plot(gam.Kacarina$residuals)
AIC(gam.Kacarina)
hist(df$Kacarina) #Normal distribution?
#AIC = 1340.289
#Adjusted R-Squared = 0.688
#Deviance Explained = 70.6%
#Most to least significant (p-values): avgmintemp (< 2e-16), avgmaxtemp:avgmintemp (3.39e-12), avgmaxtemp (0.00364), trtl (0.00111)

#Creating a linear model for Acarina as this was the species found to be most significantly impacted by soil temperature.
plot(Kacarina~avgmaxtemp, data = df, main = "Acarina (Kempson Sample)", xlab = "Average Maximum Soil Temperature (Degrees F)", ylab = "Acarina")
plot(Kacarina~avgmintemp, data = df, main = "Acarina (Kempson Sample)", xlab = "Average Minimum Soil Temperature (Degrees F)", ylab = "Acarina")
#What is the y axis showing/what are its units?