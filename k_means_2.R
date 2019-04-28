library('utils')
library("clues")
library("mclust")
library("useful")
library("factoextra")
rm(list = ls())
setwd(
    "/Users/sxs2561/Documents/AcademicAssignments/stat_557/project_3/datamining_project_3/data"
)


#### READ DATA
########### Read data ##########

data = read.csv("Frogs_MFCCs.csv")
na.omit(data)
data <-
    data[which(
        as.factor(data$Species) == ("AdenomeraAndre") |
            as.factor(data$Species) == ("AdenomeraHylaedactylus") |
            as.factor(data$Species) == ("HypsiboasCordobae") |
            as.factor(data$Species) == ("HypsiboasCinerascens")
    ) , ]
x.wf <- subset(data[, 1:22])
y.wf <- data$Species
print(dim(data)) #5743   26

# #Elbow Method for finding the optimal number of clusters
# set.seed(123)
# # Compute and plot wss for k = 2 to k = 15.
# k.max <- 15
#
# data <- x.wf
#
# wss <- sapply(2:k.max,
#               function(k){kmeans(data, k,iter.max = 15 )$tot.withinss})
# print((wss))
# plot(2:k.max, wss,
#      type="b", pch = 19, frame = FALSE,
#      xlab="Number of clusters K",
#      ylab="Total within-clusters sum of squares")
#aRandIdx=c()
#for (i in 1:100) {
set.seed(3)
numberOfClusters = 4
centerIndexes = sample(1:nrow(x.wf), numberOfClusters, replace = FALSE)
centers = x.wf[centerIndexes, c(0:length(x.wf))]
km.out = kmeans(x = x.wf, centers = centers)

fviz_cluster(km.out,data=x.wf)


print('hello')    
    #aRandIdx[i]<-adjustedRandIndex(y.wf,km.out$cluster)
#}
#plot(x=c(1:length(aRandIdx)),y=aRandIdx,main = "Effect of Random Initialization on k-means performance for k=9",xlab = "Seed",ylab = "Adjusted Rand Index",pch=18,col='blue',cex=2.2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5)
#print(mean(aRandIdx))
