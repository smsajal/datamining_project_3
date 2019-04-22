library('utils')
library("clues")
library("mclust")
rm(list=ls())
setwd("/Users/sxs2561/Documents/AcademicAssignments/stat_557/project_3/datamining_project_3/data")


#### READ DATA
########### Read data ##########

data=read.csv("frog.csv")
na.omit(data)
data <- data[which(  as.factor(data$Species) == ("AdenomeraAndre") |  as.factor(data$Species) == ("AdenomeraHylaedactylus") | as.factor(data$Species) == ("HypsiboasCordobae") | as.factor(data$Species) == ("HypsiboasCinerascens") ) ,]
x.wf <- subset(data[,1:22])
y.wf <- data$Species
print(dim(data)) #5743   26