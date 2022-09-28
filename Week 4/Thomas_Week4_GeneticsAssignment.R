# Look at the plot and model results for our Dryad data in the tutorial. Part 1: Without knowing which points represent which groups, 
  # Give one explanation for why these data might be difficult to draw spatial inferences about genes.(3 points)

#These data might be difficult to use to draw spatial inferences about genes because many of them are clustered together on the plot. This makes it difficult to understand relationships between the different regions.
#On the right track - the biggest issue is that they are clustered unevenly (aka an unbalanced sample - I will explain this version later in the semester).
  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (3 points), and EXPLAIN WHY (4 points).

#The grey points are likely the most correlated as they are clustered closest together and have the fewest outliers compared to the data points of different regions.

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)
install.packages("stability")
library(stability)
data(ge_data)

# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.

ge_data
head(ge_data)
mod.env <- lm(ge_data$Yield ~ ge_data$Env)
mod.gen <- lm(ge_data$Yield ~ ge_data$Gen)
mod.env
mod.gen

# Test the significance of both models and look at the model summary. (3 points each)
  # Which model is a better fit to explain the yield response, and WHY? (6 points)
  # Hint: Does one model seem more likely to be over-fitted?

anova(mod.env)
summary(mod.env)
#p-value/Pr(>F) < 2.2e-16, R-squared = 0.4315
anova(mod.gen)
summary(mod.gen)
#p-value/Pr(>F) < 1.417e-11, R-squared = 0.07706

#The model that is a better fit to explain the yield response is the environment because it has a larger R-squared value. This indicates that the relationship is stronger between yield response and the environment.
#The other model is also over-fitted.
# Which environment would be your very WORST choice for generating a strong yield response? (2 points)

#The environment that would be the worst choice for generating a strong yield response is Sargodha because it has the largest p-value, meaning it is the least statistically significant out of all the environments (p = 0.7138).