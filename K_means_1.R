library('utils')
library("clues")
rm(list=ls())
setwd("/Users/sxs2561/Documents/AcademicAssignments/stat_557/project_3/datamining_project_3/data")

#### READ DATA
data.test=read.csv("testing_k_final.csv")
data.train=read.csv("training_k_final.csv")
data=read.csv("training_k_final.csv")

x <- subset(data, select=-class)
y <- data$class

# x<- scale(x)


print(length(y))
km.out=kmeans(x = x,6)
print(length(km.out$cluster))


print(adjustedRand(y,km.out$cluster))
# print(km.out)
plot(x=x$X20150720_N,y=x$X20150226_N,col=(km.out$cluster+1),pch=20,cex=2)
