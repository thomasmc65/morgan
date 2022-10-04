# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific Reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your GitHub repository. (6 points)
  # Export and save the plot you've created. (2 points)
  # Zoom into your plot to look at the distribution for different strains.

# Do all of the strains in the plot have the same distributions (yes/no)? (2 pt) 

#All the strains in the plot do not have the same distributions.

# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (2 pts)

#The authors used a Kruskal-Wallis test rather than ANOVA to compare the strains because Kruskal-Wallis does not assume that the data are normal and does assume that
#groups with different standard deviations have different distributions. On the other hand, ANOVA assumes that the variation within the groups is equal, which is not the case in this data set.

# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
  # (2) - The replication time (data$RepTime.sec)
      # 3 points each
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))

data <- read.csv(file=("C:/GitHub/morgan/Week 5/Figure4Data.csv"), header=T)

library(fitdistrplus)
library(logspline)

#(1) Poisson
one.col <- data$Num.Cells.Progeny
hist(one.col, main = "Number of Cells of Progeny")
fitp <- fitdist(c(na.exclude(one.col)), distr = "pois")
fitp

#(1) Negative Binomial
one.col <- data$Num.Cells.Progeny
hist(one.col, main = "Number of Cells of Progeny")
fitnb <- fitdist(c(na.exclude(one.col)), distr = "nbinom")
fitnb

#(1) Logistic
one.col <- data$Num.Cells.Progeny
hist(one.col, main = "Number of Cells of Progeny")
fit.logis <- fitdist(c(na.exclude(one.col)), distr = "logis")
fit.logis

gofstat(list(fitp, fitnb, fit.logis), chisqbreaks=c(1,2,4,8,16,32,64))

#(2) Poisson
one.col <- data$RepTime.sec
hist(one.col, main = "Replication Time")
fitp <- fitdist(c(na.exclude(one.col)), distr = "pois")
fitp

#(2) Negative Binomial
one.col <- data$RepTime.sec
hist(one.col, main = "Replication Time")
fitnb <- fitdist(c(na.exclude(one.col)), distr = "nbinom")
fitnb

#(2) Logistic
one.col <- data$RepTime.sec
hist(one.col, main = "Replication Time")
fit.logis <- fitdist(c(na.exclude(one.col)), distr = "logis")
fit.logis

gofstat(list(fitp, fitnb, fit.logis), chisqbreaks=c(1,2,4,8,16,32,64))

# Based on the AIC scores, which distribution is the best fit for: (4 pts)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?
  # (2) - The replication time (data$RepTime.sec)?

#(1) The distribution that is the best fit for the number of cells in the progeny is the negative binomial as it has the lowest AIC value.
#(2) The distribution that is the best fit for the replication time is also the negative binomial, which has the lowest AIC value.

# Plot a generic histogram for the replication time (data$RepTime.sec) (2 pt)

one.col <- data$RepTime.sec
hist(one.col, main = "Replication Time")

# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (6 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.

#One hypothesis for an evolutionary process represented by the two tallest bars in the histogram is that predation played a role in the development of multicellularity.
#With increased stress from predators, replication time decreased, which would have increased the number of cells of progeny.