# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.

#For the invertebrate community, the predictor variables that will be used to complete an RDA with variance partitioning include 

#do rda w/ different community

library(readxl)
setwd("C:/GitHub/morgan/Week 9")
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic <- as.data.frame(abiotic.tibble)
head(abiotic)

invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invert <- as.data.frame(invert.tibble)
head(invert)

abiotic.names <- paste(abiotic$Parcel, abiotic$Land_Use)
abiotic$names <- abiotic.names
head(abiotic)

invert.names <- paste(invert$Parcel, invert$Landuse)
invert$names <- invert.names
head(invert)

abiotic.means <- aggregate(x = abiotic, by = list(abiotic$names), FUN = "mean")
head(abiotic.means)
invert.means <- aggregate(x = invert, by = list(invert$names), FUN = "mean")
head(invert.means)

abiotic.means1 <- abiotic.means[,c(-1,-2,-3,-5,-6,-16)]
abiotic.means2 <- as.data.frame(sapply(abiotic.means1, as.numeric))
invert.means1 <- invert.means[-5,c(-1:-3,-73)]
invert.means2 <- as.data.frame(sapply(invert.means1, as.numeric))

library(vegan)
colnames(abiotic.means2)
ord <- rda(invert.means2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
ord
anova(ord)
plot(ord, ylim = c(-2,2), xlim = c(-5,5))  
ord <- rda(invert.means2 ~., abiotic.means2)

#53% of the variance is explained by this redundancy analysis.

# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

mod1 <- lm(invert.means2$Diptera ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means2)
summary(mod1)
AIC(mod1) #113.9488
summary(mod1)$adj.r.squared #0.228516
#Remove pH, K, and Mg.

mod2 <- lm(invert.means2$Diptera ~ totalN + Perc_ash + Ca + Al + TotalP + OlsenP, abiotic.means2)
summary(mod2)
AIC(mod2) #110.1255
summary(mod2)$adj.r.squared #0.3511541
#Remove Al and TotalP.

mod3 <- lm(invert.means2$Diptera ~ totalN*OlsenP + Perc_ash + Ca + Al + TotalP, abiotic.means2)
summary(mod3)
AIC(mod3) #91.83614
summary(mod3)$adj.r.squared #0.7566873

plot(mod3$residuals)

#From model 3, the interactive effect between total nitrogen and reactive phosphorous combined with the percentage of ash rest, calcium, aluminum, and total phosphorous a lower AIC when compared to the other two tested linear models.
#These results make sense as nitrogen and phosphorous are limiting nutrients in an ecosystem.

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.

#These results relate to one another in that 