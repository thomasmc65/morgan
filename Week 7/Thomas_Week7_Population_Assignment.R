# Load the "anytime" and "ggplot2" packages to complete this week's assignment.

install.packages("anytime")
library("anytime")
install.packages("ggplot2")
library("ggplot2")

# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.

data <- read.csv(file=("C:/GitHub/morgan/Week 7/Plankton_move_average.csv"), header=T)
data
head(data)

#Used the following lines to format the date and remove NAs from the dataset:
data$Date <- as.Date(data$Date, origin = "0001-01-01") # Setting values to "day zero".
data <- na.omit(data)
data
head(data)

#Plot these population data over time with the following code:
ggplot(data)  +
  xlab("Numeric Date") + ylab("Density Individuals")+
  geom_line(data=data, aes(Date, D.mendotae), color="black", alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, LimncalanusF+LimncalanusM), color="orange",  alpha = 0.7, size=1)+ # adding males and females together, hint: this is actually spelled Limnocalanus
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  theme_bw() 

# Export this plot to have on hand for reference in the next section of the assignment (and upload with your script).

# (1) - Which species is most likely to be r-selected prey and which its primary predator? (2 pts)
#The species most likely to be r-selected prey is Limnocalanus and the primary predator is D. mendotae. 
#This is because the population spikes in Limnocalanus happen first and are then followed by a spike in the predator species, D. mendotae.
#This indicates that D. mendotae relies on Limnocalanus as a food source because when Limnocalanus declines, so does D. mendotae.
#Backward (probably) because those peaks are actually a response to the previous peak - magnitude matters just as much as timing in this case.

# What is one relationship the third species MIGHT have to the first two? (2 pts)
#The third species (Bythotrephes) might be the prey of the primary predator's prey species (Limnocalanus).

#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:
LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = .5, beta = 1, gamma = .4, delta = .3)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "Numeric Date", ylab = "Density Individuals")
legend("topright", c("D. mendotae", "Limnocalanus"), lty = c(1,2), col = c(1,2), box.lwd = 0)
#They die off every time I run this model. Did you get the plot you exported from these values?
library(deSolve)

# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)
#Alpha represents the rate of prey population growth.
#Beta represents the rate of predation.
#Gamma represents the rate of prey consumption as population stability.
#Delta represents the rate of prey consumption as predator die-off.

# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.
# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
#Beta was increased to represent a rise in the rate of predation on Limnocalanus by D. mendotae.
#Alpha was decreased to represent a decrease in Limnocalanus population growth and a slow in population fluctuations.
#Gamma was increased to represent that the increase of Limnocalanus consumption helped to stabilize the population.
#Delta was decreased to represent that the rate of Limnocalanus consumption as die-off of D. mendotae was not happening.

# Are there other parameter changes that could have created the same end result? (2 pts)
#Other parameter changes that could have created the same end result include the introduction of a new predator species and competition between that species and D. mendotae.
#True, but the question was about the coeffecient changes.

# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey)
