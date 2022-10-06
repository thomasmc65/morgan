# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."

library(MASS)
library (MuMIn)
library(mgcv)

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from the tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each) 
    # In one model only include additive effects.
    # In the other model include one interactive effect.
    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.

setwd("C:/GitHub/morgan/Week6")
df <- read.csv("Toscano_Griffen_Data.csv")
df

?family

#Additive Effects
gam.mod1 <- gam(activity.level~claw.width + carapace.width + toadfish.cue.treatment, family = binomial, random = list(ID=~ 1), data = df)
summary(gam.mod1)
r.squaredGAM(gam.mod1)

#Interactive Effect
gam.mod2 <- gam(activity.level~claw.width * carapace.width * toadfish.cue.treatment, family = binomial, random = list(ID=~ 1), data = df)
summary(gam.mod2)
r.squaredGAM(gam.mod2)

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
df$prop.cons <- df$eaten/df$prey 

# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)


# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)


# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(gam.mod1$residuals, ylim = c(-.1,.1))
plot(gam.mod2$residuals, ylim = c(-.1,.1))

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)


AIC(gam.mod1, gam.mod2)

# (Q4) - Which model is a better fit? (2 pt)


# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)







