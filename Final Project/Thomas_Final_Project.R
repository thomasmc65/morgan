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

glm <- glm(Kthom~trt + avgmaxtemp + avgmintemp, family = poisson, data = df)
glm

#What do I do about there being so many different sampled arthropod species? The above GLM is literally just one of them.