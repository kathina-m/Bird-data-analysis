library(vegan)
library(tidyverse)

### BIRD DATA ANALYSIS  ###

#### READ IN DATA ####
## bird and environmental data seperately 
# bird<-read.csv("data/data2/Bird_community.csv", sep=",", header=TRUE, row.names = 1)
# bird[is.na(bird)] <- 0
# env<-read.csv("data/data2/bird_environment.csv", sep = ";", dec = ",", header = TRUE)
# env[is.na(env)] <- 0

## in one dataframe
dat <- read.csv("data/birds_dataset.csv", sep = ";", header=TRUE, dec = ".")
dat$category <- as.factor(dat$category)

## adding richness and abundances
dat$rich<-specnumber(dat[,5:32]) #species richness
dat$abund<-rowSums(dat[,5:32]) #abundances
dat$rarerich<-rarefy(dat[,5:32],min(dat$abund)) #rarefied richness based on the subsample with the lowest number of individuals

## looking at the data
boxplot(rich~category,data=dat)
boxplot(log(abund+1)~category,data=dat)
boxplot(rarerich~category,data=dat)

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

#### RAREFIED VS. OBSERVED SPECIES ####
# (rarefied to smallest species size in dataset) to make different sites comparable
plot(dat$rich, dat$rarerich, main = "Observed vs. Rarefied Richness",
     xlab = "Observed No. of Species",
     ylab = "Rarefied No. of Species", pch = 16)
abline(0, 1) # 1:1 line

#### ORDINATION WITH NMDS ####
# to check for differences in species composition
nmd1<-metaMDS(dat[,5:28],distance="horn",k=2) #NMDS analysis based on Morisita-Horn-Index as a dissimilarity measure
plot(nmd1,display="species",type="t") #plot results
points(nmd1,pch=c(16,17)[as.numeric(as.factor(dat$category))],cex=1.4) #add sampling points
legend("topright",pch=c(16,17),c("Forest","Park"), cex = 0.7) #add legend
ef<-envfit(nmd1,dat[,33:43]) #check for correlation of dissimilarity gradients with environmental variables
ef #results
plot(ef,p.max=0.05) #add significant environmental variables to the NMDS plot

orditkplot(nmd1)

