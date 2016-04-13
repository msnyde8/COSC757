### Pre-clustering setup
## Package installation and/or load library 
#install packages
install.packages("cluster")
install.packages("fpc")
#load libraries
library(cluster)
library(fpc)

## Data setup
#irisData <- read.csv(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Assignments/Assignment2/UCI data/try1/iris.data.txt", header = FALSE, sep = ",", stringsAsFactors = TRUE, col.names = c("sLength", "sWidth", "pLength", "pWidth", "class") )
#plot(irisData)
paperData <- read.csv(file = "C:/Users/maryjoyce/Documents/Towson/Spring 2016/COSC 757/Assignments/Assignment3/UCI data/[UCI] AAAI-13 Accepted Papers - Papers.csv", header = FALSE, stringsAsFactors = TRUE, col.names = c("title", "keywords", "topics", "hlKeywords", "abstract") )
plot(paperData)
newData <- cbind(paperData$topics, paperData$hlKeywords)
plot(newData)

## Density-based approach: DBSCAN
#Run dbscan with eps = 5 and MinPts = 5
dbr <- dbscan(newData, eps=5, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = 10 and MinPts = 5
dbr <- dbscan(newData, eps=10, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = 15 and MinPts = 5 
dbr <- dbscan(newData, eps=15, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = 11 and MinPts = 5 
dbr <- dbscan(newData, eps=11, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = 12 and MinPts = 5 
dbr <- dbscan(newData, eps=12, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = 11.5 and MinPts = 5 
dbr <- dbscan(newData, eps=11.5, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

#Run dbscan with eps = 11.1 and MinPts = 5 
dbr <- dbscan(newData, eps=11.1, MinPts=5)
str(dbr)
plot(newData, col=dbr$cluster+1L)

# Silhouette plot
d <- dist(newData)
sil <- silhouette(dbr$cluster,d)
plot(sil)



## Partitioning approach: K-means
plot(newData)
myData <- newData
wss<-(nrow(myData)-1)*sum(apply(myData,2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(myData,
                                       centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups")


#run k-means with k = 8
km8 <- kmeans(newData, centers = 8) 
km8
#visualize results colored by cluster
plot(newData, col=km8$cluster)
#plot cluster centers
points(km8$centers,pch='x',cex=1.5)

#run k-means with k = 10
km10 <- kmeans(newData, centers = 10)
km10
#visualize results colored by cluster
plot(newData, col=km10$cluster)
#plot cluster centers
points(km10$centers,pch='x',cex=1.5)

#evaluating k-means in R
#create distance matrix for cluster.stats
distm <- dist(newData)
#calculate cluster statistics for km8
cstatskm8 = cluster.stats(distm,km8$cluster)
#calculate cluster statistics for km10
cstatskm10 = cluster.stats(distm,km10$cluster) 
#evaluate between and within cluster distances
cstatskm8$average.between
cstatskm8$average.within
cstatskm10$average.between
cstatskm10$average.within

## Partitioning approach: K-Medoids
distm <- dist(newData)

#run PAM with k = 8
pam8 <- pam(newData, 8)
#visualize results colored by cluster
plot(newData, col=pam8$cluster)
#plot cluster centers
points(pam8$medoids,pch='O',cex=1.5)

#run PAM with k = 10
pam10 <- pam(newData, 10)
#visualize results colored by cluster
plot(newData, col=pam10$cluster)
#plot cluster centers
points(pam10$medoids,pch='O',cex=1.5)

#evaluate PAM
cstatspam8 <- cluster.stats(distm,pam8$cluster)
cstatspam10 <- cluster.stats(distm,pam10$cluster)

#evaluate between and within cluster distances
cstatspam8$average.between
cstatspam8$average.within
cstatspam10$average.between
cstatspam10$average.within


## Hierarchical approach: AGNES - Single linkage
agn <- agnes(newData, diss=FALSE, stand=FALSE, method="single") 
dend_agn <- as.dendrogram(agn)
plot(dend_agn, xlab="Index of Data Points", ylab = "Steps", main = "Single-Linkage Clustering")

## Hierarchical approach: AGNES - Complete linkage
agn <- agnes(newData, diss=FALSE, stand=FALSE, method="complete") 
dend_agn <- as.dendrogram(agn)
plot(dend_agn, xlab="Index of Data Points", ylab = "Steps", main = "Complete-Linkage Clustering")


