setwd("C:/GitHub/morgan/Final Project")

#arthrain = arthropod and rainfall data (Wise_DH_Lensing_JR_2019)
#soiltemp = soil minimum and maximum temperature data (KY Soil Data)

install.packages("readxl")
library(readxl)

soiltemp.tibble <- read_excel("KY Soil Data.xlsx")
soiltemp <- as.data.frame(soiltemp.tibble)
head(soiltemp)

arthrain.tibble <- read_excel("Wise_DH_Lensing_JR_2019.xlsx")
arthrain <- as.data.frame(arthrain.tibble)
head(arthrain)

?merge
df <- merge(soiltemp, arthrain, by = "period")
df

glm.Kthom <- glm(Kthom~trt + avgmaxtemp + avgmintemp, family = poisson, data = df)
summary(glm.Kthom)

glm.Ksalt <- glm(Ksalt~trt + avgmaxtemp + avgmintemp, family = poisson, data = df)
summary(glm.Ksalt)

glm.Ktitan <- glm(Ktitan~trt + avgmaxtemp * avgmintemp, family = poisson, data = df)
summary(glm.Ktitan)

#--------------------------------------------------------------------------------

library(spdep)
library(adespatial)
library(vegan)
  
KY_Soil_Data.csv <- read.csv("KY_Soil_Data.csv", header=T)
Wise_DH_Lensing_JR_2019.csv <- read.csv("Wise_DH_Lensing_JR_2019.csv", header=T)
KY_Soil_Data.mat <- as.matrix(KY_Soil_Data.csv)
Wise_DH_Lensing_JR_2019.mat <- as.matrix(Wise_DH_Lensing_JR_2019.csv)

KY_Soil_Data.mat
Wise_DH_Lensing_JR_2019.mat

#Soil Temp Controlling Arthropods
soiltemp.rda <- rda(df[,9:89]~avgmaxtemp + avgmintemp, data=df)
soiltemp.rda
anova(soiltemp.rda, perm.max = 10000)
RsquareAdj(soiltemp.rda)

#____ conditional, ____ constrained
#RsquaredAdj = ____

#Rainfall Controlling Arthropods
rainfall.rda <- rda(df[,9:89]~trt, data=df)
rainfall.rda
anova(rainfall.rda, perm.max = 10000)
RsquareAdj(rainfall.rda)

#____ conditional, ____ constrained
#RsquaredAdj = ____
