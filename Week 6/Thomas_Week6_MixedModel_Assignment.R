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

df <- read.csv(file=("C:/GitHub/morgan/Week 6/Toscano_Griffen_Data.csv"), header=T)
df
head(df)

#Additive Effects
glmm.mod1 <- glmmPQL(activity.level~claw.width + carapace.width + toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod1)

#Interactive Effect
glmm.mod2 <- glmmPQL(activity.level~claw.width * carapace.width + toadfish.cue.treatment, family = binomial, random = ~ 1 | block, data = df)
summary(glmm.mod2)

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming df= your data frame (feel free to change that):
df$prop.cons <- df$eaten/df$prey

# (Q1) - The code in line 8 is performing two operations at once. What are they? (2 pts)
#The two operations the code in line 8 is performing are 

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, specifically, did the results change? (5 pts)
#The interactive effect did not change which variables predict proportional consumption. For both of the additive and interactive effect models, toadfish cue treatments had the lowest p-value.

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
plot(glmm.mod1$residuals, ylim = c(-.1,.1))
plot(glmm.mod2$residuals, ylim = c(-.1,.1))
#Neither model is a good fit because there is too obvious of a pattern in each residual plot. The data is not random enough.

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)

#Additive Effects
gam.mod1 <- gam(activity.level~claw.width + carapace.width + toadfish.cue.treatment, family = binomial, random = list(ID=~ 1), data = df)
summary(gam.mod1)
#R-sq.(adj) =  0.313

#Interactive Effect
gam.mod2 <- gam(activity.level~claw.width * carapace.width * toadfish.cue.treatment, family = binomial, random = list(ID=~ 1), data = df)
summary(gam.mod2)
#R-sq.(adj) =   0.32

AIC(gam.mod1, gam.mod2)
#          df AIC
#gam.mod1  4  454.4434
#gam.mod2  8  463.6771

# (Q4) - Which model is a better fit? (2 pt)
#Among the two generalized additive models, it is unclear as to whether the additive or interactive model is a better fit.
#This is because the neither of the models has both the higher R-squared and the lower AIC.
#But, because the R-squared values are so close between the two, the additive model likely is a better fit as its AIC is lower.

# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)
plot(gam.mod1$residuals, ylim = c(-.1,.1))
plot(gam.mod2$residuals, ylim = c(-.1,.1))

#Based on the residuals of the generalized additive models, these results are likely correct as the residuals are random for both models.