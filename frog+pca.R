library('utils')
library(tree)
library(randomForest)
library("clues")
library("mclust")

rm(list=ls())
setwd("/Users/Sherlock/Box Sync/PSU Spr19/STAT 557/datamining_project_2/data")
#setwd("/Users/sxs2561/Documents/AcademicAssignments/stat_557/project_3/datamining_project_3/data")

data=read.csv("frog.csv")

#data <- data[which(as.factor(data$Species) == ("AdenomeraAndre") |  as.factor(data$Species) == ("AdenomeraHylaedactylus") | as.factor(data$Species) == ("Ameeregatrivittata") | as.factor(data$Species) == ("HypsiboasCinerascens") ) ,]
#data <- data[which(as.factor(data$Species) == ("LeptodactylusFuscus") | as.factor(data$Species) == ("AdenomeraAndre") |  as.factor(data$Species) == ("AdenomeraHylaedactylus") | as.factor(data$Species) == ("Ameeregatrivittata") | as.factor(data$Species) == ("HypsiboasCinerascens") ) ,]
data <- data[which(  as.factor(data$Species) == ("AdenomeraAndre") |  as.factor(data$Species) == ("AdenomeraHylaedactylus") | as.factor(data$Species) == ("HypsiboasCordobae") | as.factor(data$Species) == ("HypsiboasCinerascens") ) ,]
# 

#as.factor(data$Species) == ("LeptodactylusFuscus") 
#LeptodactylusFuscus

print(colnames(data))

#data = na.omit(data)
print(dim(data))

x.wf <- subset(data[,1:22])
print(unique(y.wf))
print(x.wf)
y.wf <- data$Species
print(y.wf)
#as.vector(data$Species)

#print(y.wf[1:10])



data=read.csv("frog.csv")
na.omit(data)
data <- data[which(  as.factor(data$Species) == ("AdenomeraAndre") |  as.factor(data$Species) == ("AdenomeraHylaedactylus") | as.factor(data$Species) == ("HypsiboasCordobae") | as.factor(data$Species) == ("HypsiboasCinerascens") ) ,]
x.wf <- subset(data[,1:22])
y.wf <- data$Species
print(dim(data)) #5743   26




# full data together
# cmDatatraining = cbind(x.wf, y.wf)
# colnames(cmDatatraining)[colnames(cmDatatraining)=="y.wf"] <- "class"
# print(cmDatatraining)
require(ggplot2)


print(x.wf)
pca_res <- prcomp(as.matrix(x.wf), center = TRUE, scale. = FALSE)
plot_data <- cbind(as.data.frame(pca_res$x[,1:2]), labels = y.wf)
print(pca_res$x[,1:2])
ggplot(plot_data, aes(x = PC1, y = PC2, colour = labels)) +
  geom_point()

summary(pca_res)

#Elbow Method for finding the optimal number of clusters
#set.seed(100)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
scaled_data <- scale(x.wf)
data <- scaled_data

wss <- sapply(2:k.max, 
              function(k){kmeans(data, k,iter.max = 15 )$tot.withinss})
print((wss))
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


pca_res <- prcomp(as.matrix(x.wf), center = TRUE, scale. = FALSE)
x.wf <- pca_res$x[, 1:2]

k = seq(1, 100, by=1)
for (i in 1:length(k)){
  ### making random initialization
  numberOfClusters=4
  #centerIndexes=sample(1:nrow(x.wf),numberOfClusters,replace = FALSE)
  #print(centerIndexes)
  # centers=x.wf[centerIndexes,c(0:length(x.wf))]
  # 
  #  a <- (centers[1,]  )
  #  b <- (centers[2,]  )
  # print(a)
  # a[a>0] = -100
  # b = 0
  # centers <- rbind(a, b)
  
  #print(centers)
  #### APPLIED KMEANS CLUSTERING WITH 2 CLUSTERS
  # set.seed(7)
  km.out=kmeans(x = x.wf, centers=numberOfClusters)
  # print(km.out$centers)
 
  #print(as.numeric(y.wf$class))
  print(adjustedRandIndex((y.wf), km.out$cluster))
}


plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels=factor(km.out$cluster))
#plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels=factor(y.wf))
ggplot(plot_data, aes(x = PC1, y = PC2, color = labels)) +
  geom_point() +
  geom_point(data=as.data.frame(km.out$centers), aes(x=PC1,y=PC2, color='center')) +
  geom_point(data=as.data.frame(km.out$centers), aes(x=PC1,y=PC2, color='center'), size=52, alpha=.3)




library("mclust")
library("fossil")


########### GMM ############
gmm <- Mclust(x.wf, G = 7, verbose = interactive())
summary(gmm, parameters = TRUE)
plot(gmm, what="classification")
print( adjustedRandIndex(gmm$classification, y.wf))
gmm$modelName
gmm$classification

adjR = list()

## Effect of k
k = seq(2, 12, by=1)
for (i in 1:length(k)){
  gmm <- Mclust(x.wf, G = k[i], verbose = interactive())
  adjR[i] <- adjustedRandIndex(gmm$classification, y.wf)
}

plot(x= k, y = adjR, type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
print(adjR)



globalAdj = list()
print(system.time(gmmO <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.2), verbose = TRUE)))
globalAdj <- adjustedRandIndex(gmmO$classification, y.wf)  




pca_res = prcomp(as.matrix(x.wf), center = TRUE, scale. = FALSE)
x.wf = pca_res$x[, 1:2]

### RUN YOUR PCA CODE

plot(gmm0, what="classification")



seeds = seq(1, 5, by=1)
k = seq(7, 7, by=1)
adjR = rep(0, length(k))
print(adjR)
## Effect of k
for (i in 1:length(k)){
  for (j in 1:length(seeds)){
    set.seed(seeds[j])
    print(system.time(gmmO <- Mclust(x.wf, G=k[i], prior = priorControl(functionName = "defaultPrior", shrinkage = 0.2), verbose = TRUE)))
    adjR[i] <- adjR[i] + adjustedRandIndex(gmmO$classification, y.wf)  
  }
  adjR[i] <- adjR[i]/length(seeds)
}
print(adjR)

plot(x= k, y = adjR, type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")

# KMEANS

pca_res = prcomp(as.matrix(x.wf), center = TRUE, scale. = TRUE)
x.wf = pca_res$x[, 1:2]

x.wf <- subset(data[,1:22])
seeds = seq(1, 100, by=1)
k = seq(3, 3, by=1)
adjR = rep(0, length(k))
for (i in 1:length(k)){
  for (j in 1:length(seeds)){
    set.seed(seeds[j])
    print(system.time(km.out <- kmeans(x = x.wf, centers=k[i])))
    adjR[i] <- adjR[i] + adjustedRandIndex((y.wf), km.out$cluster)
  }
  adjR[i] <- adjR[i]/length(seeds)
}
plot(x= k, y = adjR, type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
print(adjR)

# print(km.out$centers)

#print(as.numeric(y.wf$class))
print(adjustedRandIndex((y.wf), km.out$cluster))

km.out <- kmeans(x = x.wf, centers=3)
#plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels=factor(km.out$cluster))
plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels=factor(y.wf))
ggplot(plot_data, aes(x = PC1, y = PC2, color = labels)) +
  geom_point() +
  geom_point(data=as.data.frame(km.out$centers), aes(x=PC1,y=PC2, color='center')) +
  geom_point(data=as.data.frame(km.out$centers), aes(x=PC1,y=PC2, color='center'), size=52, alpha=.3)



plot(gmm0, what="classification")

gmm <- Mclust(x.wf, G = 3, verbose = interactive())
summary(gmm, parameters = TRUE)


gmmO = Mclust(x.wf, G=k[i], prior = priorControl(functionName = "defaultPrior", shrinkage = 0.2), verbose = TRUE)
plot(gmm0, what='classification')


plot(x= k, y = adjR, type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
print(adjR)


## Effect of shrinkage
values = seq(0, 1, by=0.1)
for (i in 1:length(values)){
  print(system.time(gmmO <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = values[i]), verbose = TRUE)))
  adjR[i] <- adjustedRandIndex(gmmO$classification, y.wf)  
}


## Effect of shrinkage
values = seq(0, 1, by=0.1)
for (i in 1:length(values)){
  print(system.time(gmmO <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = values[i]), verbose = TRUE)))
  adjR[i] <- adjustedRandIndex(gmmO$classification, y.wf)  
}


plot(x= values, y = adjR, type = "b", pch = 22, xlab = "prior shrinkage", ylab = "adjusted Rand index")
print(adjR)
      