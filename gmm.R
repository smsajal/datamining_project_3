library("mclust")
library("fossil")

rm(list=ls())

setwd("/Users/rxh655/Documents/Spring2019/STAT557/Project3/ProjectCode/datamining_project_3/data")

########### Read data ##########
# data.test=read.csv("data/testing.csv")
# data.train=read.csv("data/training.csv")
# data=read.csv("data/training.csv")
# 
# x <- subset(data, select=-class)
# y <- data$class
# 
# x.wf <- data[which(as.factor(data$class) == ("water") | as.factor(data$class) == ("farm")),]
# y.wf <- x.wf$class
# x.wf <- subset(x.wf , select = -class)

data = read.csv("Frogs_MFCCs.csv")
data <- data[which(  as.factor(data$Species) == ("AdenomeraAndre") |  as.factor(data$Species) == ("AdenomeraHylaedactylus") | as.factor(data$Species) == ("HypsiboasCordobae") | as.factor(data$Species) == ("HypsiboasCinerascens") ) ,]

x <- subset(data[,1:22])
y.family <- data$Family
y.genus <- data$Genus
y.species <- data$Species
y.id <- data$RecordID
########### GMM ############

# bic <- mclustBIC(x.wf, G=2)
# 
# plot(bic)
?mclustBIC

for(i in 1:5)
{
  set.seed(i)
  gmm <- Mclust(x, G = 1:15, verbose = interactive())
  print(gmm$G)
}


gmm <- Mclust(x, G = 1:15, verbose = interactive())
dr <- MclustDR(gmm, lambda = 1)
plot(dr, what = "scatterplot")
plot(dr, what = "evalues")

drs <- MclustDRsubsel(dr)
summary(drs)
table(class, drs$class)
plot(drs, what = "scatterplot")

gmm$G
dr$G
for(j in 3:3)
{
  #jpeg(paste('rplot',j,'.jpg'))
  attach(mtcars)
  par(mfrow=c(5,5))
  for(i in 1:22)
  {
    
    #print(x[1])
    #print(paste(length(x$MFCCs_.1), length(x[[1]])))
    plot(x = x[[11]], y = x[[14]], col = c("red", "blue", "green", "yellow", "black", "purple", "pink", "azure", "brown")[gmm$classification], xlab = "MFCC21", ylab = "MFCC22")
    
  }
  #dev.off()
}

plot(x = x$MFCCs_10, y = x$MFCCs_20, col = c("red", "blue", "green", "yellow", "black", "purple", "pink", "azure", "brown")[gmm$classification], xlab = "MFCC21", ylab = "MFCC22")

summary(gmm, parameters = TRUE)
adjustedRandIndex(gmm$classification, y.wf)
adjustedRandIndex(y.wf, gmm$classification)


gmm$parameters
gmm$classification

set.seed(9)

adjR = list()
adjGenus = list()
adjSpecies = list()
adjId = list()

idx = 1
for(i in 2:15)
{
  # set.seed(1)
  temp = c()
  for(j in 1:1)
  {
    set.seed(j)
    gmm = Mclust(x, G = i, verbose = interactive())
    temp[j] = adjustedRandIndex(gmm$classification, y.species)
  }
  #adjR[idx] = adjustedRandIndex(gmm$classification, y.family)
  #adjGenus[idx] = adjustedRandIndex(gmm$classification, y.genus)
  adjSpecies[idx] = mean(temp)
  #adjId[idx] = adjustedRandIndex(gmm$classification, y.id)
  idx = idx+1
}
plot(x= seq(2,15,1), y = adjSpecies, main = "Species", type = "b", pch = 22, xlab = "k", ylab = "Average adjusted Rand index")


attach(mtcars)
par(mfrow=c(2,2))
plot(x= c(2,3, 4, 5, 6, 8, 10, 11, 12, 13), y = adjR, main = "Family", type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
plot(x= c(2,3, 4, 5, 6, 8, 10, 11, 12, 13), y = adjGenus, main = "Genus", type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
plot(x= c(2,3, 4, 5, 6, 8, 10, 11, 12, 13), y = adjSpecies, main = "Species", type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
plot(x= c(2,3, 4, 5, 6, 8, 10, 11, 12, 13), y = adjId, main = "ID", type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")

adjSpecies[1]


#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15

data <- x.wf

wss <- sapply(2:k.max, 
              function(k){Mclust(x, k,verbose = interactive() )$class})
print((wss))
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

###### elbow method end ########

adjR = list()
set.seed(1)
gmm1 <- Mclust(x, G=3, verbose = interactive())
adjR[1] = adjustedRandIndex(gmm1$classification, y.species)  # 0.06403
adjR[1]

set.seed(1)
gmm2 <- Mclust(x, G=3, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.1), verbose = interactive())
adjR[2] = adjustedRandIndex(gmm2$classification, y.species)  # 0.06403

set.seed(1)
gmm3 <- Mclust(x, G=3, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.2), verbose = interactive())
adjR[3] = adjustedRandIndex(gmm3$classification, y.species)  # 0.06421

set.seed(1)
gmm4 <- Mclust(x, G=3, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.4), verbose = interactive())
adjR[4] = adjustedRandIndex(gmm4$classification, y.species)  # 0.06401

set.seed(1)
gmm5 <- Mclust(x, G=3, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.7), verbose = interactive())
adjR[5] = adjustedRandIndex(gmm5$classification, y.species)  # 0.06333

plot(x= c(0,.1, .2, .4, .7), y = adjR, type = "b", pch = 22, xlab = "prior shrinkage", ylab = "adjusted Rand index")

adjR[2]
