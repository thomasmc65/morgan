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

abiotic.means1 <- abiotic.means[,c(-2,-3,-5,-6,-16)]
abiotic.means1 <- sapply(abiotic.means1, as.numeric)
invert.means1 <- invert.means[,-2:-3]
invert.means1 <- sapply(invert.means1, as.numeric)

library(vegan)
colnames(abiotic.means1)
ord <- rda(invert.means1 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.means1)
ord

ord <- rda(invert.means1 ~., abiotic.means1)


# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.



# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.


