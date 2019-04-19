rm(list=ls())
setwd("/Users/Sherlock/Box Sync/PSU Spr19/STAT 557/datamining_project_2/")
#setwd("/Users/rxh655/Documents/Spring2019/STAT557/Project3/ProjectCode/datamining_project_3")

#### READ DATA
data=read.csv("data/training.csv")

data <- data[which(as.factor(data$class) == ("farm") |  as.factor(data$class) == ("water")) ,]

x <- subset(data, select=-class)
y <- data$class

x.wf <- x
y.wf <- y


###########################
#   Include this for PCA
###########################
# ### PCA Visualization

mydata <-data
require(ggplot2)

pca_res <- prcomp(as.matrix(mydata[, 2:29]), center = TRUE, scale. = FALSE)
print(pca_res$x[, 1:2])

x.wf <- pca_res$x[, 1:2]
y.wf <- y

plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels = mydata[, 1])

ggplot(plot_data, aes(x = PC1, y = PC2, colour = labels)) +
  geom_point()

summary(pca_res)


# #compute standard deviation of each principal component
# std_dev <- pca_res$sdev
# 
# #compute variance
# pr_var <- std_dev^2
# 
# #check variance of first 10 components
# print(pr_var[1:10])
# 
# #proportion of variance explained
# prop_varex <- pr_var/sum(pr_var)
# prop_varex[1:20]
# 
# #scree plot
# plot(prop_varex, xlab = "Principal Component",
#        ylab = "Proportion of Variance Explained",
#        type = "b")
# #cumulative scree plot
# plot(cumsum(prop_varex), xlab = "Principal Component",
#        ylab = "Cumulative Proportion of Variance Explained",
#        type = "b")
###########################
#   End of  PCA
###########################


library("mclust")
library("fossil")


########### GMM ############
gmm <- Mclust(x.wf, G = 2, verbose = interactive())
summary(gmm, parameters = TRUE)

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


## Effect of shrinkage
values = seq(0, 1, by=0.1)
for (i in 1:length(values)){
  print(system.time(gmmO <- Mclust(x.wf, G=2, prior = priorControl(functionName = "defaultPrior", shrinkage = values[i]), verbose = TRUE)))
  adjR[i] <- adjustedRandIndex(gmmO$classification, y.wf)  
}


plot(x= values, y = adjR, type = "b", pch = 22, xlab = "prior shrinkage", ylab = "adjusted Rand index")
print(adjR)
