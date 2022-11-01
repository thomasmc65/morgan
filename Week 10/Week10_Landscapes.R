setwd("C:/GitHub/R4Eco_2022/Week10")
#Packages for this week include two to create asymmetrical eigenvector maps (AEMs).
library(spdep)
library(adespatial)
library(vegan)

# These data are benthic macroinvertebrate (aka bug) samples from the West Branch Susquehanna River collected in 2012.
# Sample codes refer to a riffle number for the West Branch, with smaller numbers upstream (R5, R6, etc.).
#These data have already been chopped up by taxonomic and habit groups, for your convenience.
PatchLatLon.csv <- read.csv("PatchLatLon.csv", header=T)
BugsByPatch.csv <- read.csv("BugsByPatch.csv", header=T)
HabitatbyPatch.csv <- read.csv("HabitatbyPatch.csv", header=T)
Swimmers.csv <- read.csv("Swimmers.csv", header=T)
#Formatting data as matrices works better than data frames for some of the functions this week.
PatchLatLon.mat <- as.matrix(PatchLatLon.csv[,-1])
BugsByPatch.mat <- as.matrix(BugsByPatch.csv)
HabitatbyPatch.mat <- as.matrix(HabitatbyPatch.csv)
Swimmers.mat <- as.matrix(Swimmers.csv)


#First we need to build the "empty" network - how should we expect points to interact with one another in the matrix?
nb<-cell2nb(3,30,"queen") #three columns, 30 rows long, 90 nodes.
nb1 <- droplinks(nb, 19, sym=TRUE) #these drop specific values from the network based on missing data points.
nb2 <- droplinks(nb1, 22, sym=TRUE)
nb3 <- droplinks(nb2, 25, sym=TRUE)
nb4 <- droplinks(nb3, 30, sym=TRUE)


#Map the empty network onto real lat/lon data, aka sampling locations.
bin.mat <- aem.build.binary(nb4, PatchLatLon.mat, unit.angle = "degrees", rot.angle = 90, rm.same.y = TRUE, plot.connexions = TRUE)
#How does this network compare to the real lat/lon of the points?
plot(PatchLatLon.mat[,2]~PatchLatLon.mat[,3], xlim = rev(c(76.75,77)))

#Now weight the relationships between these real locations on a prescribed network:
aem.ev <- aem(aem.build.binary=bin.mat)
#Remove the rows where links were dropped and focus on vector data frame
aem.df <- aem.ev$vectors[c(-19,-22,-25,-30),]
#this creates a LOT of variables. How do we choose which to use?
aem.df

#We will use forward selection in this case because of the sheer number of variables.
Space.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df))
Space.r2a <- RsquareAdj(Space.rda)$adj.r.squared

aem.fwd <- forward.sel(BugsByPatch.mat,aem.df, adjR2thresh=Space.r2a)

#To identify which variables are important, you can identify them in order:
aem.fwd$order
#Those variables can then be selected from within the original AEM data frame.
#Notice the last part of this new use of rda() - there is a third matrix for habitat data!
#To determine how much variance is explained by spatial relationships relative to local habitat we need to compare two rda results.
#One rda for space controlling for habitat, and one for habitat controlling for space.
SpaceNoHab.rda <- rda(BugsByPatch.mat, as.data.frame(aem.df[,aem.fwd$order]), HabitatbyPatch.mat)
SpaceNoHab.rda 
anova(SpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SpaceNoHab.rda)

#And the habitat controlling for space:
HabNoSpace.rda <- rda(BugsByPatch.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,aem.fwd$order]))
HabNoSpace.rda 
anova(HabNoSpace.rda, perm.max = 10000)
RsquareAdj(HabNoSpace.rda)

#Now look at the variance explained by each:
#Unconstrained variance is the same in each, but the constrained and conditional values change!
#SpaceNoHab - 46% constrained and 26% conditional
#HabNoSpace - 4.9% constrained and 67% conditional
  #How is this possible? Some variance can only be explained by the synergistic relationships of habitat that varies predictably with space.


#Functional Groups:
#We can subset the whole community by particular traits to see if they have different relationships to space or the environment.

#We will look at swimmers, aka bugs that can swim really well and choose where to go locally, but not regionally.
#First need to redo the variable selection so that to match this subset of the community.
SwimSpace.rda <- rda(Swimmers.mat, as.data.frame(aem.df))
SwimSpace.r2a <- RsquareAdj(SwimSpace.rda)$adj.r.squared

Swimaem.fwd <- forward.sel(Swimmers.mat,as.data.frame(aem.df), adjR2thresh=Space.r2a)

#The rest of this should look the same, just substituting the right AEM vectors and the swimmer data.
SwimSpaceNoHab.rda <- rda(Swimmers.mat, as.data.frame(aem.df[,Swimaem.fwd$order]), HabitatbyPatch.mat)
SwimSpaceNoHab.rda 
anova(SwimSpaceNoHab.rda, perm.max = 10000)
RsquareAdj(SwimSpaceNoHab.rda)

SwimHabNoSpace.rda <- rda(Swimmers.mat, HabitatbyPatch.mat, as.data.frame(aem.df[,Swimaem.fwd$order]))
SwimHabNoSpace.rda 
anova(SwimHabNoSpace.rda, perm.max = 10000)
RsquareAdj(SwimHabNoSpace.rda)

#How do these compare to the full community?
#SwimSpaceNoHab -- 33% constrained and 26% conditional
#SwimHabNoSpace -- 10% constrained and 50% conditional

#While habitat did not increase THAT much, it is now significant! So the community response matched this prediction.
