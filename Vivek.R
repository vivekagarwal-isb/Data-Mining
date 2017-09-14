setwd("C:/Users/Shivu/Desktop/ISB/Term 2/DMG/Individual asignment")

library(cluster)
library(readxl)
library(dummies)
library(dendextend)
library(ggplot2)
library(corrplot)
library(kohonen)
########################################################### Data Reading and Preprocessing ######################################

# Reading file
df1 <- read_excel("EastWestAirlinesCluster.xlsx", sheet = "data")

################################################## Hierarchical clustering without normalizing the data #########################
d1 <- dist(df1[,-c(1)], method = "euclidean")
clust1 <- hclust(d1, "ward.D")
plot(clust1, hang = -1) # display dendrogram
rect.hclust(clust1, k = 3, border = "red")


################################################### Hierarchical clustering ends part 1(a) ############################################



########################################################### Data Preprocessing ######################################


# Removing index colums as it has no value

df <- df1[,-c(1)]
df$cc1_miles <- as.factor(df$cc1_miles)
df$cc2_miles <- as.factor(df$cc2_miles)
df$cc3_miles <- as.factor(df$cc3_miles)


# Since cc1_miles, cc2_miles, cc3_miles are ordinal variable and hence need to create dummy variable to bring into the model

df_cc1 <- dummy(df$cc1_miles)
df_cc2 <- dummy(df$cc2_miles)
df_cc3 <- dummy(df$cc3_miles)
df <- df[,-c(3,4,5)]
df_new <- cbind(df,df_cc1,df_cc2,df_cc3)
df_scaled <- scale(df_new)

################################################### Hierarchical clustering ############################################
d <- dist(df_scaled, method = "euclidean")
clust <- hclust(d, "ward.D")
plot(clust, hang = -1) # display dendrogram
rect.hclust(clust, k = 3, border = "red")

# Cutting tree 
groups <- stats::cutree(clust, k=3) # cut tree into 3 clusters
membership<-as.matrix(groups)
cluster1 <- subset(df1,membership[,1]==1)
cluster1$clusterid <- 1
cluster2 <- subset(df1,membership[,1]==2)
cluster2$clusterid <- 2
cluster3 <- subset(df1,membership[,1]==3)
cluster3$clusterid <- 3

# different color for dendrogram
branch <- as.dendrogram(clust)
plot(color_branches(branch,k=3))
rect.hclust(clust, k = 3, border = "red")

################################################### Hierarchical clustering ends ############################################


########################## Finding cluster centroids###################################

centroid <- rbind(cluster1,cluster2,cluster3)

aggregate(centroid[,2:12], list(centroid$clusterid), mean)

########################## Checking stability of clusters ##############################

sample1 <- df_scaled[sample(nrow(df_scaled), size = 0.95*nrow(df_scaled), replace = FALSE),]
sample_clust <- hclust(dist(sample1, method = "euclidean"), "ward.D")
plot(sample_clust, hang = -1) # display dendrogram

sample2 <- df_scaled[sample(nrow(df_scaled), size = 0.95*nrow(df_scaled), replace = FALSE),]
sample_clust2 <- hclust(dist(sample2, method = "euclidean"), "ward.D")
plot(sample_clust2, hang = -1) # display dendrogram


##############################################################################################################################
## Determine number of clusters using K - means without scaling the data
set.seed(101)
Cluster_Variability <- matrix(nrow=20, ncol=1)
for (i in 1:20) Cluster_Variability[i] <- kmeans(df_new,centers=i,iter.max = 100, nstart=4)$tot.withinss
plot(1:20, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot
abline(v = 3, lty =2)


## Determine number of clusters using K - means with scaling the data
set.seed(1001)
Cluster_Variability1 <- matrix(nrow=20, ncol=1)
for (i in 1:20) Cluster_Variability1[i] <- kmeans(df_scaled,centers=i,iter.max = 100, nstart=4)$tot.withinss
plot(1:20, Cluster_Variability1, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot
abline(v = 10, lty =2)

set.seed(1001)
kclust <- kmeans(df_scaled,centers = 3,iter.max = 100,nstart = 4)
clusterid <- as.data.frame(kclust$cluster)
#####################33aggregatimg Kmeans cluster ID######################
df_kmeans <- cbind(df1,clusterid)
aggregate(df_kmeans[,2:12], list(df_kmeans$`kclust$cluster`), mean)



########################################### Wine data Set ############################################################


wine <- read.table("wine.data.txt", sep = ",")
colnames(wine) <- c("Class", "Alcohol" ,"Malic acid", "Ash", "Alcalinity of ash", "Magnesium","Total phenols", "Flavanoids", "Nonflavanoid phenols",
                    "Proanthocyanins","Color intensity","Hue","OD280/OD315 of diluted wines","Proline")
wine$Class <- as.factor(wine$Class)
scaled <- scale(wine[,2:14])


pcaObj <- princomp(scaled, cor = TRUE, scores = TRUE, covmat = NULL)
pcaObj$loadings
summary(pcaObj)
biplot(pcaObj)

## Some PCA Visualization
barplot(pcaObj$loadings)
var <- (pcaObj$sdev)^2
var_proportion <- var / sum(var)
plot(cumsum(var_proportion), xlab = "Principal components", ylab = "Varinace Explained")
abline(h=0.9, v=8)

corrplot(cor(scaled), method="number", diag = FALSE, col = "black")


library(ggfortify)
autoplot(pcaObj, data = wine, colour = 'Class', loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 4)

######################################### part 3 - All Constitutent ##############################################################


set.seed(20)
wine.som <- som(scaled, grid = somgrid(4, 3, "hexagonal"))
plot(wine.som, main = "Wine data")
plot(wine.som, type="changes")
plot(wine.som, type = "count")
plot(wine.som, type="dist.neighbours")
plot(wine.som, type = "mapping")
plot(wine.som, type = "codes")

wine_data <- wine.som$codes[[1]] 

set.seed(1001)
Cluster_Variability <- matrix(nrow=12, ncol=1)
for (i in 1:12) Cluster_Variability[i] <- kmeans(wine_data,centers=i,iter.max = 100, nstart=4)$tot.withinss
plot(1:12, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares") ## Elbow curve or Scree plot
abline(v = 3, lty =2)

## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(wine_data)), 3)
pal_wines <- colorRampPalette(c("red", "yellow", "blue"))
# plot these results:
windows()
plot(wine.som, type="codes", main = "Clusters",palette.name = pal_wines,labels = as.integer(wine$Type))
add.cluster.boundaries(wine.som, som_cluster)

#############################################

d1 <- dist(scaled, method = "euclidean")
clust1 <- hclust(d1, "ward.D")
plot(clust1, hang = -1) # display dendrogram
rect.hclust(clust1, k = 3, border = "red")
groups2 <- stats::cutree(clust1, k=3)
membership1<-as.matrix(groups2)
cluster11 <- subset(wine,membership1[,1]==1)
cluster11$clusterid <- 1
cluster22 <- subset(wine,membership1[,1]==2)
cluster22$clusterid <- 2
cluster33 <- subset(wine,membership1[,1]==3)
cluster33$clusterid <- 3
centroid1 <- rbind(cluster11,cluster22,cluster33)

aggregate(centroid1[,2:14], list(centroid1$clusterid), mean)
plot(clust1, data = wine, colour = 'Clusterid')

table(centroid1$Class, centroid1$clusterid)

######################################### part 3 - First 2 PC Scores ##############################################################

data <- pcaObj$scores[,1:2]
d2 <- dist(data, method = "euclidean")
clust2 <- hclust(d2, "ward.D")
plot(clust2, hang = -1) # display dendrogram
rect.hclust(clust2, k = 3, border = "red")
groups3 <- stats::cutree(clust2, k=3)
membership1<-as.matrix(groups3)
cluster_pca1 <- subset(wine,membership1[,1]==1)
cluster_pca1$clusterid <- 1
cluster_pca2 <- subset(wine,membership1[,1]==2)
cluster_pca2$clusterid <- 2
cluster_pca3 <- subset(wine,membership1[,1]==3)
cluster_pca3$clusterid <- 3
centroid3 <- rbind(cluster_pca1,cluster_pca2,cluster_pca3)

aggregate(centroid3[,2:14], list(centroid3$clusterid), mean)

table(centroid3$clusterid, centroid3$Class)

comparison_cluster <- cbind(centroid1$clusterid, centroid3$clusterid)

comp <- as.data.frame(comparison_cluster)

colnames(comparison_cluster) <- c("All Constituents", "PCA")

table(comp$`All Constituents`, comp$PCA)

##################################################################### Assignment 1 Ends##################################################

