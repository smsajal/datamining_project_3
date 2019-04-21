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

gmm <- Mclust(x.wf, G = 2, verbose = interactive())
summary(gmm, parameters = TRUE)
adjustedRandIndex(gmm$classification, y.wf)
adjustedRandIndex(y.wf, gmm$classification)


gmm$modelName
gmm$classification

set.seed(1)

adjR = list()
adjGenus = list()
adjSpecies = list()
adjId = list()
gmm <- Mclust(x, G = 2, verbose = interactive())
adjR[1] = adjustedRandIndex(gmm$classification, y.family)
adjGenus[1] = adjustedRandIndex(gmm$classification, y.genus)
adjSpecies[1] = adjustedRandIndex(gmm$classification, y.species)
adjId[1] = adjustedRandIndex(gmm$classification, y.id)


gmm2 <- Mclust(x, G = 3, verbose = interactive())
adjR[2] = adjustedRandIndex(gmm2$classification, y.family)
adjGenus[2] = adjustedRandIndex(gmm2$classification, y.genus)
adjSpecies[2] = adjustedRandIndex(gmm2$classification, y.species)
adjId[2] = adjustedRandIndex(gmm2$classification, y.id)


gmm3 <- Mclust(x, G = 4, verbose = interactive())
adjR[3] = adjustedRandIndex(gmm3$classification, y.family)
adjGenus[3] = adjustedRandIndex(gmm3$classification, y.genus)
adjSpecies[3] = adjustedRandIndex(gmm3$classification, y.species)
adjId[3] = adjustedRandIndex(gmm3$classification, y.id)


gmm4 <- Mclust(x, G = 5, verbose = interactive())
adjR[4] = adjustedRandIndex(gmm4$classification, y.family)
adjGenus[4] = adjustedRandIndex(gmm4$classification, y.genus)
adjSpecies[4] = adjustedRandIndex(gmm4$classification, y.species)
adjId[4] = adjustedRandIndex(gmm4$classification, y.id)


gmm5 <- Mclust(x, G = 6, verbose = interactive())
adjR[5] = adjustedRandIndex(gmm5$classification, y.family)
adjGenus[5] = adjustedRandIndex(gmm5$classification, y.genus)
adjSpecies[5] = adjustedRandIndex(gmm5$classification, y.species)
adjId[5] = adjustedRandIndex(gmm5$classification, y.id)

gmm6 <- Mclust(x, G = 8, verbose = interactive())
adjR[6] = adjustedRandIndex(gmm6$classification, y.family)
adjGenus[6] = adjustedRandIndex(gmm6$classification, y.genus)
adjSpecies[6] = adjustedRandIndex(gmm6$classification, y.species)
adjId[6] = adjustedRandIndex(gmm6$classification, y.id)


gmm7 <- Mclust(x, G = 10, verbose = interactive())
adjR[7] = adjustedRandIndex(gmm7$classification, y.family)
adjGenus[7] = adjustedRandIndex(gmm7$classification, y.genus)
adjSpecies[7] = adjustedRandIndex(gmm7$classification, y.species)
adjId[7] = adjustedRandIndex(gmm7$classification, y.id)

adjSpecies[7]

attach(mtcars)
par(mfrow=c(2,2))
plot(x= c(2,3, 4, 5, 6, 8, 10), y = adjR, main = "Family", type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
plot(x= c(2,3, 4, 5, 6, 8, 10), y = adjGenus, main = "Genus", type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
plot(x= c(2,3, 4, 5, 6, 8, 10), y = adjSpecies, main = "Species", type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")
plot(x= c(2,3, 4, 5, 6, 8, 10), y = adjId, main = "ID", type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")

adjR = list()
gmm1 <- Mclust(x, G=10, verbose = interactive())
adjR[1] = adjustedRandIndex(gmm1$classification, y.species)  # 0.06407
adjR[1]

gmm2 <- Mclust(x, G=10, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.1), verbose = interactive())
adjR[2] = adjustedRandIndex(gmm2$classification, y.species)  # 0.06407

gmm3 <- Mclust(x, G=10, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.2), verbose = interactive())
adjR[3] = adjustedRandIndex(gmm3$classification, y.species)  # 0.06421


gmm4 <- Mclust(x, G=10, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.4), verbose = interactive())
adjR[4] = adjustedRandIndex(gmm4$classification, y.species)  # 0.06401


gmm5 <- Mclust(x, G=10, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.7), verbose = interactive())
adjR[5] = adjustedRandIndex(gmm5$classification, y.species)  # 0.06387

plot(x= c(0,.1, .2, .4, .7), y = adjR, type = "b", pch = 22, xlab = "prior shrinkage", ylab = "adjusted Rand index")

adjR[2]
