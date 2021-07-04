#####################################################################
#1 Data preparation (note I added a "site" column at the beginning of your data table, which codes the six study sites as 1 to 6)

#1a Import data (depends on your data format - here it's based on a .txt file. For csv files, the command is read.csv or read.csv2)
#dat<-read.table(file.choose(),h=T,sep="\t")
dat<- read.csv("data/birds_dataset.csv", sep=";")
#datenv <- read.csv("data/bird_environment.csv", sep = ";")
#1b create additional response variables


#dat$plot1 = apply(dat, c(1:3,5:31), sum, na.rm=TRUE)
#birds$plot2 = apply(birds[ ,7:9], 1, sum, na.rm=TRUE)

library(vegan)
dat$rich<-specnumber(dat[,5:32]) #species richness
dat$abund<-rowSums(dat[,5:32], na.rm = TRUE) #abundances
dat$rarerich<-rarefy(dat[,5:32],min(dat$abund)) #rarefied richness based on the subsample with the lowest number of individuals


#1c look at the data
boxplot(rich~category,data=dat)
boxplot(log(abund+1)~category,data=dat)
boxplot(rarerich~category,data=dat)


######################################################################
#2 Data analysis

#2a linear models: check for difference between habitat types
require(nlme) #package required for mixed effects models
mod1<-lme(rich~category,random=(~1|site),data=dat) #model structure, random=... specifies how the data are structured (subsamples nested in study site)
summary(mod1) #model output - important is the "fixed effects" part. Here "forest" is hiding in the "Intercept" and the categorypark-row is showing the difference between park and forest
plot(mod1) #check for homogeneity of variances (data points should have similar vertical spread along the x-axis)
qqnorm(mod1,~resid(.,type="p"),abline=c(0,1)) #check for normality of residuals (should not be completely off the line)


#2b linear models: include environmental variables
round(cor(dat[,34:43]),2) #check which predictor variables are strongly correlated (below -0.7 or above 0.7) - highly correlated variables should not be included together in the same model (select only one of them, e.g. the one more strongly related to the response variable)

mod2<-lme(rich~category+canopy_cover+n_tree_spec+n_tree_ind+dbh_min+n_microhabitats+temperature,random=(~1|site),data=dat,method="ML") #initial, full model with all potential predictor variables
summary(mod2)
require(MASS) #needed for the stepAIC command
mod3<-stepAIC(mod2) #model simplification based on AIC-value of the model
summary(mod3) #final model which includes only the most important predictors
plot(mod3) #check for homogeneity of variances (data points should have similar vertical spread along the x-axis)
qqnorm(mod3,~resid(.,type="p"),abline=c(0,1)) #check for normality of residuals (should not be completely off the line)


#significant relationships can be plotted
require(ggplot2)
ggplot(dat,aes(x=dbh_min,y=rich)) +
  geom_point() +
  geom_smooth(method=lm,se=TRUE)
#figures need to be made a bit nicer (larger font size, meaningful axis labels, ...)
#see ggplot help in R (?ggplot) and tutorials in the www


#2c Ordination with NMDS (to check for differences in species composition)
nmd1<-metaMDS(dat[,5:31],distance="horn",k=2) #NMDS analysis based on Morisita-Horn-Index as a dissimilarity measure
plot(nmd1,display="species",type="t") #plot results
points(nmd1,pch=c(16,17)[as.numeric(as.factor(dat$category))],cex=1.4) #add sampling points
legend("topright",pch=c(16,17),c("Forest","Park")) #add legend
ef<-envfit(nmd1,dat[,34:42]) #check for correlation of dissimilarity gradients with environmental variables
ef #results
plot(ef,p.max=0.05) #add significant environmental variables to the NMDS plot


###################################################################
#3. what could still be done:
#richness estimation (species accumulation curve)
#see also iNEXT package for some nice ways to work with species estimation/accumulation: https://cran.r-project.org/web/packages/iNEXT/vignettes/Introduction.html

#richness + abundance only for forest species

#additive diversity partitioning:
#see here for a general intro https://onlinelibrary.wiley.com/doi/pdf/10.1034/j.1600-0706.2002.990101.x
#can be easily done in Excel, but there's also an R command in the vegan package: https://www.rdocumentation.org/packages/vegan/versions/2.4-2/topics/adipart

#(functional diversity) - would need the FD package in R and the dfFD command. If interested, I can send some code.

# ...

#vegan tutorial:
#http://www2.uaem.mx/r-mirror/web/packages/vegan/vignettes/intro-vegan.pdf
#look at ordiplot, ordilabel etc. to see how the NMDS plot can be made a bit nicer
