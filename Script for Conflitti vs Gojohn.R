# Senior theses anlayses
## Analysis of Conflitti data
## Need packages 
library(tidyverse) 
library(ggplot2)
library(vegan)
library(lattice)
library(FactoMineR)
library(mapview)
library(sf) 
library(tigris)
library(leaflet)
library(RColorBrewer)
library(gplots)

### first we want to get a more comprehensive measurement of urbanization attributes that might affect movement
### Conflitti et al. used...
### we want to use principle components analysis to get a composite variable that incorporates....
setwd("C:/Users/River/OneDrive/Documents/Thesis Project 2023")

Conflitti_Data<-read.csv("Conflitti Data File for R.csv")
testdata<-Conflitti_Data%>%select(c(houseDensity,treeCanopyPerc,roadPerc,grassShrubPerc,otherPavedPerc,green1Perc))
pca1= prcomp(testdata[,1:6], scale. = TRUE)
circle <- function(center = c(0, 0), npoints = 100) {
  
  r = 1
  
  tt = seq(0, 2 * pi, length = npoints)
  
  xx = center[1] + r * cos(tt)
  
  yy = center[1] + r * sin(tt)
  
  return(data.frame(x = xx, y = yy))
  
}

corcir = circle(c(0, 0), npoints = 100)
correlations = as.data.frame(cor(na.omit(testdata[,1:6]), pca1$x))
arrows = data.frame(x1 = c(0, 0,0,0,0,0), y1 = c(0, 0,0,0,0,0), x2 = correlations$PC1,
                    
                    y2 = correlations$PC2)
ggplot() + geom_path(data = corcir, aes(x = x, y = y), colour = "gray65") +
  
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") +
  
  geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) +
  
  geom_hline(yintercept = 0, colour = "gray65") + geom_vline(xintercept = 0,Colour = "gray65") + 
  xlim(-1.1, 1.1) + ylim(-1.1, 1.1) + labs(x = "pc1 aixs",
                                           
                                           y = "pc2 axis") + ggtitle("Circle of correlations")
Conflitti_Data$impPerc<-Conflitti_Data$roadPerc+Conflitti_Data$otherPavedPerc
Conflitti_Data$canopy.parks<-Conflitti_Data$green1Perc+Conflitti_Data$treeCanopyPerc
gr3 <- Conflitti_Data %>% select(houseDensity,canopy.parks,impPerc)
gr3.pca <-rda(gr3, scale=TRUE)

gr3.pca

options(max.print=50)

summary(gr3.pca)
gradient_loadings3 <- gr3.pca$CA$v[,1:2] #only use first two PCs (aka this code only extracts the first 2 PC scores)

gradient_pc_scores3<- scores(gr3.pca,  display = "sites")
pcs3<-as.data.frame(gradient_pc_scores3)

pcs3<-pcs3%>%rename(PC1.3=PC1)%>%rename(PC2.3=PC2)

Conflitti_Data<-bind_cols(Conflitti_Data, pcs3)
Conflitti_Data

#Regression

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
summary(Conflitti_Data)
#cor(houseDensity$treeCanopyPerc, roadPerc$grassShrubPerc)
#hist(houseDensity$treeCanopyPerc, roadPerc$grassShrubPerc)
#plot(testdata ~ grassShrubPerc, data=testdata)
testdata.lm<-lm(aveMeanFD ~ PC1.3, data = Conflitti_Data)
summary( testdata.lm)
par(mfrow=c(2,2))
plot(testdata.lm)
hist(Conflitti_Data$aveMeanFD)
hist(log(Conflitti_Data$aveMeanFD))

testdata.lm<-lm(log(aveMeanFD) ~ PC1.3, data = Conflitti_Data)
summary(testdata.lm)
par(mfrow=c(2,2))
plot(testdata.lm)

par(mfrow=c(1,1))

testdata.lm<-lm(log(aveMeanFD) ~ impPerc, data = Conflitti_Data)
summary(testdata.lm)
par(mfrow=c(2,2))
plot(testdata.lm)


testdata.lm<-lm(log(aveMeanFD) ~ canopy.parks, data = Conflitti_Data)
summary(testdata.lm)
par(mfrow=c(2,2))
plot(testdata.lm)

testdata.lm<-lm(log(aveMeanFD) ~ houseDensity, data = Conflitti_Data)
summary(testdata.lm)
par(mfrow=c(2,2))
plot(testdata.lm)

testdata.lm<-lm(log(aveMeanFD) ~ houseDensity+canopy.parks+impPerc, data = Conflitti_Data)
summary(testdata.lm)
library(car)
Anova(testdata.lm, type=3)
par(mfrow=c(2,2))
plot(testdata.lm)

Conflitti_Data$sitN<-rownames(Conflitti_Data)

Conflitti_Data.sm<-subset(Conflitti_Data, !(sitN=="sit29"))

testdata.lm<-lm(log(aveMeanFD) ~ houseDensity+canopy.parks+impPerc, data = Conflitti_Data.sm)
summary(testdata.lm)
library(car)
Anova(testdata.lm, type=3)


#expand.grid()
#plotting.data<-expand.grid(
#  houseDensity = seq(min(Conflitti_Data$houseDensity), max(Conflitti_Data$houseDensity), length.out=30),
#  treeCanopyPerc=c(min(Conflitti_Data$treeCanopyPerc), mean(Conflitti_Data$treeCanopyPerc), max(Conflitti_Data$treeCanopyPerc)))
#plotting.data$predicted.y <- predict.lm(testdata.lm, newdata=plotting.data)
#plotting.data$houseDensity <- round(plotting.data$houseDensity, digits = 2)
#plotting.data$houseDensity <- as.factor(plotting.data$houseDensity)
Conflitti.plot <- ggplot(Conflitti_Data, aes(x=PC1.3, y=aveMeanFD)) +
  scale_y_continuous(breaks=c(0,5,50,500,5000,50000),trans="log")+
  ylab("Mean foraging distance (m)")+
  xlab("Urbanization (PC1)")+
  geom_point()
Conflitti.plot

#add figures for all

Conflitti.plot <- ggplot(Conflitti_Data, aes(x=impPerc, y=aveMeanFD)) +
  scale_y_continuous(breaks=c(0,5,50,500,5000,50000),trans="log")+
  geom_smooth(method='lm', color="black")+
  ylab("Mean foraging distance (m)")+
  xlab("Percent impervious surface")+
  geom_point()
Conflitti.plot

Conflitti.plot <- ggplot(Conflitti_Data, aes(x=canopy.parks, y=aveMeanFD)) +
  scale_y_continuous(breaks=c(0,5,50,500,5000,50000),trans="log")+
  ylab("Mean foraging distance (m)")+
  xlab("Percent woodland and park")+
  geom_point()
Conflitti.plot

Conflitti.plot <- ggplot(Conflitti_Data, aes(x=houseDensity, y=aveMeanFD)) +
  scale_y_continuous(breaks=c(0,5,50,500,5000,50000),trans="log")+
  geom_smooth(method='lm', color="black")+
  ylab("Mean foraging distance (m)")+
  
  Conflitti.plot
