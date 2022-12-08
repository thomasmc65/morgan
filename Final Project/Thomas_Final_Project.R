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

#Soil Temperature Controlling Arthropods
soiltemp.rda <- rda(df[,9:89]~avgmaxtemp + avgmintemp, data=df)
soiltemp.rda
RsquareAdj(soiltemp.rda)
anova(soiltemp.rda, perm.max = 10000)
summary(soiltemp.rda)

#0.2917 constrained, 0.7083 unconstrained
#RsquaredAdj = 0.2741938
#p = 0.001

#About 29.2% of the variance in arthropod abundance is explained by soil temperature. Because this is greater than the variance explained by rainfall levels, it has a more significant impact on arthropod abundance.
#This leaves about 70.8% of unexplained variance in arthropod abundance.

#Rainfall Controlling Arthropods
rainfall.rda <- rda(df[,9:89]~trt, data=df)
rainfall.rda
RsquareAdj(rainfall.rda)
anova(rainfall.rda, perm.max = 10000)
summary(rainfall.rda)

#0.08946 constrained, 0.91054 unconstrained
#RsquaredAdj = 0.06697337
#p = 0.009

#About 8.95% of the variance in arthropod abundance is explained by rainfall level.
#This leaves about 91.1% of unexplained variance in arthropod abundance.

plot(soiltemp.rda)
plot(soiltemp.rda, type = "n", ylim = c(1,2), display = c("sites", "species"))
text(soiltemp.rda, display = "sites", labels = as.numeric(df$roundmintemp, df$roundmaxtemp), col = "blue3", cex = 0.65)
text(soiltemp.rda, display = "species", labels = as.character(colnames(df[,9:89])), col = "darkmagenta", cex = 0.65)

plot(rainfall.rda)
plot(rainfall.rda, type = "n", ylim = c(1,2), display = c("sites", "species"))
text(rainfall.rda, display = "sites", labels = as.character(df$trt), col = "blue3", cex = 0.65)
text(rainfall.rda, display = "species", labels = as.character(colnames(df[,9:89])), col = "darkmagenta", cex = 0.65)

#Arrows? Axes? How to interpret the rda plots
