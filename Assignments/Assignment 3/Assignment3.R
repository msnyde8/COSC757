### Pre-clustering setup
## Package installation and/or load library 
#install packages
install.packages("cluster")
install.packages("fpc")
#load libraries
library(cluster)
library(fpc)

## Data setup
irisData <- read.csv(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Assignments/Assignment2/UCI data/try1/iris.data.txt", header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("sLength", "sWidth", "pLength", "pWidth", "class") )
plot(irisData)
newData <- cbind(irisData$pWidth, irisData$sWidth)
plot(newData)


## Density-based approach: DBSCAN
#Run dbscan with eps = .05 and MinPts = 5
dbr <- dbscan(newData, eps=.05, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = .10 and MinPts = 5
dbr <- dbscan(newData, eps=.10, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = .20 and MinPts = 5 
dbr <- dbscan(newData, eps=.20, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = .50 and MinPts = 5 
dbr <- dbscan(newData, eps=.50, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = .65 and MinPts = 5 
dbr <- dbscan(newData, eps=.65, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = .60 and MinPts = 5 
dbr <- dbscan(newData, eps=.60, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = .61 and MinPts = 5 
dbr <- dbscan(newData, eps=.61, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

# Silhouette plot
d <- dist(newData)
sil <- silhouette(dbr$cluster,d)
plot(sil)



## Partitioning approach: K-means
plot(newData)

#run k-means with k = 2
km2 <- kmeans(newData, centers = 2) 
km2
#visualize results colored by cluster
plot(newData, col=km2$cluster)
#plot cluster centers
points(km2$centers,pch='x',cex=1.5)

#run k-means with k = 3
km3 <- kmeans(newData, centers = 3) 
km3
#visualize results colored by cluster
plot(newData, col=km3$cluster)
#plot cluster centers
points(km3$centers,pch='x',cex=1.5)

#evaluating k-means in R
#create distance matrix for cluster.stats
distm <- dist(newData)
#calculate cluster statistics for km2
cstatskm2 = cluster.stats(distm,km2$cluster)
#calculate cluster statistics for km3
cstatskm3 = cluster.stats(distm,km3$cluster) 
#evaluate between and within cluster distances
cstatskm2$average.between
cstatskm2$average.within
cstatskm3$average.between
cstatskm3$average.within

## Partitioning approach: K-Medoids
distm <- dist(newData)

#run PAM with k = 2
pam2 <- pam(newData, 2)
#visualize results colored by cluster
plot(newData, col=pam2$cluster)
#plot cluster centers
points(pam2$medoids,pch='O',cex=1.5)

#run PAM with k = 3
pam3 <- pam(newData, 3)
#visualize results colored by cluster
plot(newData, col=pam3$cluster)
#plot cluster centers
points(pam3$medoids,pch='O',cex=1.5)

#evaluate PAM
cstatspam2 <- cluster.stats(distm,pam2$cluster)
cstatspam3 <- cluster.stats(distm,pam3$cluster)

#evaluate between and within cluster distances
cstatskm2$average.between
cstatskm2$average.within
cstatskm3$average.between
cstatskm3$average.within


## Hierarchical approach: AGNES - Single linkage
agn <- agnes(newData, diss=FALSE, stand=FALSE, method="single") 
dend_agn <- as.dendrogram(agn)
plot(dend_agn, xlab="Index of Data Points", ylab = "Steps", main = "Single-Linkage Clustering")

## Hierarchical approach: AGNES - Complete linkage
agn <- agnes(newData, diss=FALSE, stand=FALSE, method="complete") 
dend_agn <- as.dendrogram(agn)
plot(dend_agn, xlab="Index of Data Points", ylab = "Steps", main = "Complete-Linkage Clustering")


