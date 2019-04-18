library("mclust")
library("fossil")

rm(list=ls())

setwd("/Users/rxh655/Documents/Spring2019/STAT557/Project3/ProjectCode/datamining_project_3")

########### Read data ##########
data.test=read.csv("data/testing.csv")
data.train=read.csv("data/training.csv")
data=read.csv("data/training.csv")

x <- subset(data, select=-class)
y <- data$class

x.wf <- data[which(as.factor(data$class) == ("water") | as.factor(data$class) == ("farm")),]
y.wf <- x.wf$class
x.wf <- subset(x.wf , select = -class)

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


adjR = list()
gmm <- Mclust(x.wf, G = 2, verbose = interactive())
adjR[1] = adjustedRandIndex(gmm$classification, y.wf)


gmm2 <- Mclust(x.wf, G = 3, verbose = interactive())
adjR[2] = adjustedRandIndex(gmm2$classification, y.wf)


gmm3 <- Mclust(x.wf, G = 4, verbose = interactive())
adjR[3] = adjustedRandIndex(gmm3$classification, y.wf)


gmm4 <- Mclust(x.wf, G = 5, verbose = interactive())
adjR[4] = adjustedRandIndex(gmm4$classification, y.wf)


gmm5 <- Mclust(x.wf, G = 6, verbose = interactive())
adjR[5] = adjustedRandIndex(gmm5$classification, y.wf)

plot(x= c(2,3, 4, 5, 6), y = adjR, type = "b", pch = 22, xlab = "k", ylab = "adjusted Rand index")


gmm2 <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.1), verbose = TRUE)
adjR[2] = adjustedRandIndex(gmm2$classification, y.wf)  # 0.06407

gmm3 <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.2), verbose = TRUE)
adjR[3] = adjustedRandIndex(gmm3$classification, y.wf)  # 0.06421


gmm4 <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.4), verbose = TRUE)
adjR[4] = adjustedRandIndex(gmm4$classification, y.wf)  # 0.06401


gmm5 <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.7), verbose = TRUE)
adjR[5] = adjustedRandIndex(gmm5$classification, y.wf)  # 0.06387

plot(x= c(0,.1, .2, .4, .7), y = adjR, type = "b", pch = 22, xlab = "prior shrinkage", ylab = "adjusted Rand index")

adjR[2]
