library("mclust")

rm(list=ls())

setwd("/Users/rxh655/Documents/Spring2019/STAT557/Project3/ProjectCode/datamining_project_3")

########### Read data ##########
data.test=read.csv("data/testing.csv")
data.train=read.csv("data/training.csv")
data=read.csv("data/training.csv")

x <- subset(data, select=-class)
y <- data$class

x.wf <- data[which(as.factor(data$class) == ("water") | as.factor(data$class) == ("forest")),]
y.wf <- x.wf$class
x.wf <- subset(x.wf , select = -class)

########### GMM ############

# bic <- mclustBIC(x.wf, G=2)
# 
# plot(bic)
?mclustBIC

gmm <- Mclust(x.wf, G = 2, verbose = interactive(), initialization = list(hc(x.wf, modelName = "VVV")))
summary(gmm, parameters = TRUE)
adjustedRandIndex(gmm$classification, y.wf)

gmm$modelName
gmm$classification


adjR = list()

adjR[1] = adjustedRandIndex(gmm$classification, y.wf)
?Mclust

gmm2 <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.1), verbose = TRUE)
adjR[2] = adjustedRandIndex(gmm2$classification, y.wf)  # 0.06407

gmm3 <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.2), verbose = TRUE)
adjR[3] = adjustedRandIndex(gmm3$classification, y.wf)  # 0.06421


gmm4 <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.4), verbose = TRUE)
adjR[4] = adjustedRandIndex(gmm4$classification, y.wf)  # 0.06401


gmm5 <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = 0.7), verbose = TRUE)
adjR[5] = adjustedRandIndex(gmm5$classification, y.wf)  # 0.06387

plot(x= c(0,.1, .2, .4, .7), y = adjR, type = "b", pch = 22, xlab = "prior shrinkage", ylab = "adjusted Rand index")
