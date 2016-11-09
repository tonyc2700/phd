library(cluster)
# library(fpc)
library(NbClust)

completeData <- function(db){
	db[rowSums(is.na(db))==0,]
}	



library(foreign)
db <- "abdn-cleanse.sav"
dataset <- read.spss(db, to.data.frame=TRUE)
dataset[complete.cases(dataset),]

data <- quakes
library(cluster.datasets)
# set.seed(123)
# # Compute and plot wss for k = 2 to k = 15
# k.max <- 15 # Maximal number of clusters
# wss <- sapply(1:k.max, 
#         function(k){kmeans(data, k, nstart=10 )$tot.withinss})
# plot(1:k.max, wss,
#        type="b", pch = 19, frame = FALSE, 
#        xlab="Number of clusters K",
#        ylab="Total within-clusters sum of squares")
# abline(v = 3, lty =2)


set.seed(123)
res.nb <- NbClust(data, distance = "euclidean",
                  min.nc = 2, max.nc = 10, 
                  method = "complete", index ="silhouette") 
res.nb # print the results