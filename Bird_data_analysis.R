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
dat$site <- as.factor(dat$site)

## adding richness and abundances
dat$rich <- specnumber(dat[,4:31]) #species richness
dat$abund <- rowSums(dat[,4:31]) #abundances
dat$rarerich <- rarefy(dat[,4:31],min(dat$abund)) #rarefied richness based on the subsample with the lowest number of individuals

## looking at the data
boxplot(rich ~ category, data = dat)
boxplot(log(abund+1) ~ category, data = dat)
boxplot(rarerich ~ category, data = dat)

hist(rowSums(dat[,4:31]),
     col = "grey", # colors of bins
     main = "Bird Community Sampling", # plot title
     xlab = "Number of individuals") # x-axis title

#### LINEAR MODELS ####
## check for difference between habitat types
require(nlme) #package required for mixed effects models
mod1 <- lme(rich ~ category,random = (~1|site), data = dat) #model structure, random=... specifies how the data are structured (subsamples nested in study site)
summary(mod1) #model output - important is the "fixed effects" part. Here "forest" is hiding in the "Intercept" and the category park-row is showing the difference between park and forest
plot(mod1) #check for homogeneity of variances (data points should have similar vertical spread along the x-axis)
qqnorm(mod1, ~ resid(.,type="p"), abline=c(0,1)) #check for normality of residuals (should not be completely off the line)

## include environmental variables
round(cor(dat[,34:44]), 2) #check which predictor variables are strongly correlated (below -0.7 or above 0.7) - highly correlated variables should not be included together in the same model (select only one of them, e.g. the one more strongly related to the response variable)

mod2 <- lme(rich ~ category + canopy_cover + n_tree_spec + n_tree_ind + dbh_min + n_microhabitats + temperature, random = (~1|site), data = dat, method="ML") #initial, full model with all potential predictor variables
summary(mod2)
require(MASS) #needed for the stepAIC command
mod3 <- stepAIC(mod2) #model simplification based on AIC-value of the model
summary(mod3) #final model which includes only the most important predictors
plot(mod3) #check for homogeneity of variances (data points should have similar vertical spread along the x-axis)
qqnorm(mod3, ~ resid(.,type="p"), abline=c(0,1)) #check for normality of residuals (should not be completely off the line)



#### SPECIES ACCUMULATION CURVE ####
# how good was sampling effort in the ecosystem types to assess species richness? 

SAC_park <- specaccum(subset(dat[,4:31], dat$category == "park"))
SAC_fore <- specaccum(subset(dat[,4:31], dat$category == "forest"))

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
nmd1 <- metaMDS(dat[,4:31], distance="horn", k=2) #NMDS analysis based on Morisita-Horn-Index as a dissimilarity measure
plot(nmd1, display="species", type="t") #plot results
points(nmd1, pch=c(16,17)[as.numeric(as.factor(dat$category))], cex=1.4) #add sampling points
legend("topright",pch=c(16,17),c("Forest","Park"), cex = 0.7) #add legend
ef <- envfit(nmd1,dat[,32:43]) #check for correlation of dissimilarity gradients with environmental variables
ef #results
plot(ef,p.max=0.05) #add significant environmental variables to the NMDS plot

orditkplot(nmd1)

