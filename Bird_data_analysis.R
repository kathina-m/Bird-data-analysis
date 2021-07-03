library(vegan)
library(tidyverse)

### BIRD DATA ANALYSIS  ###

#### READ IN DATA ####
## bird and environmental data seperately 
bird<-read.csv("data/Bird_community.csv", sep=",", header=TRUE, row.names = 1)
bird[is.na(bird)] <- 0
env<-read.csv("data/bird_environment.csv", sep = ";", dec = ",", header = TRUE)
env[is.na(env)] <- 0

## in one dataframe
dat<-read.csv("data/bird_dataset.csv", sep = ",",header=TRUE)

hist(rowSums(bird),
     col = "grey", # colors of bins
     main = "Bird Community Sampling", # plot title
     xlab = "Number of individuals") # x-axis title

#### SPECIES ACCUMULATION CURVE ####
# how good was sampling effort in the ecosystem types to assess species richness? 

SAC_park <- specaccum(subset(bird, env$category == "park"))
SAC_fore <- specaccum(subset(bird, env$category == "forest"))

plot(SAC_park, xlab = "plots", ylab = "species richness", main="species accumulation curve")
plot(SAC_fore, xlab = "plots", ylab = "species richness", main="species accumulation curve", col="green", add = T)
legend("bottomright", legend = c("park","forest"), col = c("black","green"),lwd=1, bty = "n")

# rarefaction (to smallest species size in dataset) to make different sites comparable
