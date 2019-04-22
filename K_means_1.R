library('utils')
library("clues")
library("mclust")
rm(list=ls())
setwd("/Users/sxs2561/Documents/AcademicAssignments/stat_557/project_3/datamining_project_3/data")


#### READ DATA
########### Read data ##########
# data.test=read.csv("testing.csv")
# data.train=read.csv("training.csv")
data=read.csv("Frogs_MFCCs.csv")
data<-na.omit(data)

# 
# x <- subset(data[,1:22], select=-Family)
# y <- data$Family
# 

x <- subset(data[,1:22])
y.family <- data$Family
y.genus <- data$Genus
y.species <- data$Species
y.id <- data$RecordID
# x.wf <- data[which(as.factor(data$class) == ("water") | as.factor(data$class) == ("farm")),]
# y.wf <- x.wf$class
# y.wf<-as.vector(y.wf)
# x.wf <- subset(x.wf , select = -class)
# is.data.frame(x.wf)

is.data.frame(x)
### making random initialization
set.seed(9)
numberOfClusters=4
centerIndexes=sample(1:nrow(x),numberOfClusters,replace = FALSE)
print(centerIndexes)
centers=x[centerIndexes,c(0:length(x))]
#### APPLIED KMEANS CLUSTERING WITH 2 CLUSTERS
# set.seed(7)
km.out=kmeans(x =x,centers =numberOfClusters)

print(adjustedRandIndex(y.species,km.out$cluster))
