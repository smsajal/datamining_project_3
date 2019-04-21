library('utils')
library("clues")
library("mclust")
rm(list=ls())
setwd("/Users/sxs2561/Documents/AcademicAssignments/stat_557/project_3/datamining_project_3/data")

set.seed(391)
#### READ DATA
########### Read data ##########
data.test=read.csv("testing.csv")
data.train=read.csv("training.csv")
data=read.csv("training.csv")

x <- subset(data, select=-class)
y <- data$class

x.wf <- data[which(as.factor(data$class) == ("water") | as.factor(data$class) == ("farm")),]
y.wf <- x.wf$class
y.wf<-as.vector(y.wf)
x.wf <- subset(x.wf , select = -class)
is.data.frame(x.wf)


### making random initialization
numberOfClusters=6
centerIndexes=sample(1:nrow(x.wf),numberOfClusters,replace = FALSE)
print(centerIndexes)
centers=x.wf[centerIndexes,c(0:length(x.wf))]
#### APPLIED KMEANS CLUSTERING WITH 2 CLUSTERS
# set.seed(7)
km.out=kmeans(x = x.wf,centers=centers)

print(adjustedRandIndex(y.wf,km.out$cluster))
