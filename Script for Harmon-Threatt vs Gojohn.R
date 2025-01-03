setwd("C:/Users/River/OneDrive/Documents/Thesis Project 2023")
ChiHT<-read.csv("Harmon-Threatt Vs Gojohn.csv")
HTVG<-subset(ChiHT,select=c(Genus...Species,Number.Moved,Number.Moved.1))
#file_path <- "HTVG"
#HTVGDAta<- read.delim(file_path, row.names = 1)
#saveRDS(HTVGDAta,"Harmon-Threat Vs Gojohn.rda"
#head(HTVGDAta)
library(gplots)

chisq <- chisq.test(HTVG[c(1,3,5:9),2:3]) #rerun for proprotional data (removing those rows where neither of us recaptured)
chisq
# Observed counts
chisq$observed
# Expected counts
round(chisq$expected,2)
#Nature of the dependence between the row and the column variables
round(chisq$residuals, 3)
library(corrplot)
#visualize Pearson's residuals using the package corrplot:
#corrplot(chisq$residuals, is.cor = FALSE)
#Positive residuals are in blue, Negative residuals are in red
#The contribution (in %) of a given cell to the total Chi-square score is calculated as follow:
#contrib <- 100*chisq$residuals^2/chisq$statistic
#round(contrib, 3)

# Visualize the contribution
#corrplot(contrib, is.cor = FALSE)
# printing the p-value:
#chisq$p.value
# printing the mean:
#chisq$estimate

# 1. convert the data as a table
rownames(HTVG)<-HTVG[,1]
colnames(HTVG)[2]<-"Harmon-Threatt"
colnames(HTVG)[3]<-"Gojohn"
dt <- as.table(as.matrix(HTVG[,2:3]))
# 2. Graph
balloonplot(t(dt), main ="Harmon-Threatt vs. Gojohn", xlab ="", ylab="Taxon", cum.margins=FALSE,
            label = FALSE, show.margins = FALSE)

#rerun for proprotional data
chisq <- chisq.test(HTVG[c(1,3,5:9),2:3])
chisq
fisher.test(HTVG[c(1,3,5:9),2:3], alternative="less")
# Observed counts
chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)
library(corrplot)
# 1. convert the data as a table
rownames(HTVG)<-HTVG[,1]
colnames(HTVG)[2]<-"Harmon-Threatt"
colnames(HTVG)[3]<-"Gojohn"
dt <- as.table(as.matrix(HTVG[,2:3]))
# 2. Graph
balloonplot(t(dt), main ="Harmon-Threatt vs. Gojohn", xlab ="", ylab="Taxon", cum.margins=FALSE,
            label = FALSE, show.margins = FALSE)

#can't test for differences between proportion recaptured in urban vs suburban habitats because recapture rates were too low
