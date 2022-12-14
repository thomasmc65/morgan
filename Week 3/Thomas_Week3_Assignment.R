# (1) Approximately how many hours ahead of Sunbury (yellow) was the peak flow in Lewisburg (red) during the 2011 flood? (2 pt)
#The peak flow in Lewisburg (3:00 pm) was approximately 9 hours ahead of the peak flow in Sunbury (Midnight) during the 2011 flood.


# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable? (4 pts)
#One reason why information on the time between peak flow events up- and downstream could be valuable is for predicting flooding. Also, it helps with better understanding the stream's hydrology and discharge.

# Package scavenger hunt! (12 pts each)

## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, manipulate a parameter within the function to create a new result. 
        # Common options might be allele frequency, population size, fitness level, etc. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
       
          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.

install.packages("learnPopGen")
library(learnPopGen)
genetic.drift(p0=0.5, Ne=20, nrep=10, time=100, show="p", pause=0.1)
object<-genetic.drift(p0=0.5, Ne=20, nrep=10, time=100, show="p", pause=0.1)
plot(object,show="p")

genetic.drift(p0=0.7, Ne=50, nrep=25, time=150, show="genotypes", pause=0.1)
object<-genetic.drift(p0=0.7, Ne=50, nrep=25, time=150, show="genotypes", pause=0.1)
plot(object,show="genotypes")

## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 
        # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
        # If there are two diversity metrics in the example script, neither of these will count as the modified script.
        # Hint: If the function can "only" caluclate Simpson's diversity, the inverse of Simpson's diversity is another common metric. 
        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
        
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
          # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.

install.packages("vegan")#you added all kinds of fun packages here! 
library(vegan)
install.packages("permute")
install.packages("lattice")
library(permute)
library(lattice)
install.packages("diveRsity")
library(diveRsity)

x <- c(15, 6, 4, 0, 3, 0)
diversity(x, index = "simpson", groups = 6, equalize.groups = FALSE, MARGIN = 1, base = exp(1))
simpson.unb(x, inverse = FALSE)
#[1] 0.6587302

#diversity(df, index = "simpson")
#index = c("simpson","shannon"",...)

x <- c(12, 4, 3, 1, 2, 1)
diversity(x, index = "invsimpson", groups = 6, equalize.groups = FALSE, MARGIN = 1, base = exp(1))
invsimpson(x, inverse = TRUE)#not a function
#6 
#1
?diversity
setwd("C:/GitHub/morgan")
jpeg(file = "Thomas Genetic Drift Ref Manual Example.jpeg")
par(family = "serif")
dev.off()
jpeg(file = "Thomas Genetic Drift Manipulated Example.jpeg")
par(family = "serif")
dev.off()