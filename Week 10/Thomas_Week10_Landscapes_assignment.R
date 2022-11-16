# Load the packages from this week's tutorial.
#In the tutorial we looked at the community as a whole and the swimmers which ultimately matched a prediction we had for their distribution.

setwd("C:/GitHub/morgan/Week 10")
library(spdep)
library(adespatial)
library(vegan)

#Part 1: Look at two other subsets of the community and determine the relative influence of space and habitat on each following the methods in the tutorial. (10 points)
#The options include groupings by taxonomy, where Diptera (true flies) have the strongest flight ability, Trichoptera the 2nd strongest, 
    #Ephememeroptera are 3rd, and non insects are 4th...because they don't have wings.
#Groupings by habits include the swimmers (off limits for the assignment) as most mobile, sprawlers as 2nd (they move in search of food, but not quickly),
    #and the clingers come in last (they might move up and down on individual rocks).

PatchLatLon.csv <- read.csv("PatchLatLon.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
Diptera.csv <- read.csv("Diptera.csv", header=T)
Clingers.csv <- read.csv("Clingers.csv", header=T)

PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Diptera.mat <- as.matrix(Diptera.csv)
Clingers.mat <- as.matrix(Clingers.csv)

nb<-cell2nb(3,30,"queen")
nb1 <- droplinks(nb, 19, sym=TRUE)
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)

bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))

aem.ev <- aem(aem.build.binary=bin.mat)
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]
aem.df

#Diptera

Diptera.rda <- rda(Diptera.mat, as.data.frame(aem.df))
Diptera.r2a <- RsquareAdj(Diptera.rda)$adj.r.squared
aem.fwd <- forward.sel(Diptera.mat,aem.df, adjR2thresh=Diptera.r2a)
aem.fwd$order

#Space controlling habitat
SpaceNoHab.rda <- rda(Diptera.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)

#0.40 conditional, 0.43 constrained
#RsquaredAdj = 0.10

#Habitat controlling space
HabNoSpace.rda <- rda(Diptera.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

#0.79 conditional, 0.042 constrained
#RsquaredAdj = 0.13

#Clingers

Clingers.rda <- rda(Clingers.mat, as.data.frame(aem.df))
Clingers.r2a <- RsquareAdj(Clingers.rda)$adj.r.squared
aem.fwd <- forward.sel(Clingers.mat,aem.df, adjR2thresh=Clingers.r2a)
aem.fwd$order

#Space controlling habitat
SpaceNoHab.rda <- rda(Clingers.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)

#0.26 conditional, 0.49 constrained
#RsquaredAdj = 0.44

#Habitat controlling space
HabNoSpace.rda <- rda(Clingers.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

#0.69 conditional, 0.044 constrained
#RsquaredAdj = 0.011

#Part 2: What is your interpretation of the pattern for each group individually, and the two in comparison, based on their mobility? (5 points)

#Space controlling habitat is more influential for Diptera than habitat controlling space. About 4% was explained by habitat and 43% by space, this leaves about 36% for space in union with habitat.
#The same was true for the clingers, with space controlling habitat bring more influential than habitat controlling space. About 4% was explained by habitat and nearly 50% by space, leaveing about 21% for space in union with habitat.
#In comparison, the two communities (Diptera and the clingers) had pretty similar patterns.
  #True - if you squint and think about them relative to one another the pattern of more space for Clingers also makes sense.

#Part 3: For each of your chosen groups of bugs, perform variable selection for the habitat data rather than the AEM data. Which habitat variables are significant for each? (10 points)
  # Definitions for the habitat column names:
    #Inorg = total suspended inorganic solids in the water column
    #Organ = total suspended organic solids in the water column
    #Chla = Chlorophyll a concentration from benthic rocks collected in-stream
    #BOM = total benthic organic matter in the sample
    #Depth = water depth
    #Flow	= water velocity where samples were collected
    #Fines = Percent of the substrate as "fines" i.e. small particles too small to measure
    #AveAr = The average size of rocks where each sample was collected

#Diptera and habitat

Diptera.rda <- rda(Diptera.mat, as.data.frame(HabitatbyPatch.csv))
Diptera.r2a <- RsquareAdj(Diptera.rda)$adj.r.squared
aem.fwd <- forward.sel(Diptera.mat,HabitatbyPatch.csv, adjR2thresh=Diptera.r2a)
aem.fwd

#Clingers and habitat

Clingers.rda <- rda(Clingers.mat, as.data.frame(HabitatbyPatch.csv))
Clingers.r2a <- RsquareAdj(Clingers.rda)$adj.r.squared
aem.fwd <- forward.sel(Clingers.mat,HabitatbyPatch.csv, adjR2thresh=Clingers.r2a)
aem.fwd

#For Diptera, the habitat variables that were significant include chlorophyll (Chla), water depth (Depth), water velocity (Flow), and the average size of rocks (AveAr).
#For clingers, the only habitat variable that was significant was water depth (Depth). 

#Part 4: How do you expect selecting both the spatial and the habitat variables would change the results of the RDAs from Part 1 above? (5 points)
  #(You do not need to redo the RDAs, unless you *want* to.)

#Selecting both the spatial and the habitat variables would provide more insight as to why space has more of an effect than habitat for the two communities as indicated by the most significant variables.
#Since space was clearly more significant, we likely would not see a large increase in habitat, at least not for the clingers but maybe for Diptera considering the difference in the number of significant variables between the two communities.
#Diptera's habitat may become significant along with its space, but it will not be as significant as space.
#All of this might be true - the real change is that you are likely overfitting the habitat right now without first selecting the meaningful variables.
#So you might "steal" from the conditional variance to increase habitat, likely the total unconstrainted variance will go up though.
