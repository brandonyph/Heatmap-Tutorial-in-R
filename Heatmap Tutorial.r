## data import
library(ggplot2) 		#heatmap plotting
library(tidyverse)  		# data manipulation
library(cluster)    		# clustering algorithms

#------------------------------------
#Creating Dummy Data
#------------------------------------
Data <- data.frame(replicate(10,sample(0:1,25,rep=TRUE)))
row.names(Data) <- c('Gene1','Gene2','Gene3','Gene4','Gene5','Gene6','Gene7','Gene8','Gene9','Gene10','Gene11','Gene12','Gene13','Gene14','Gene15','Gene16','Gene17','Gene18','Gene19','Gene20','Gene21','Gene22','Gene23','Gene24','Gene25')
colnames(Data) <-  c("L1", "L2", "F1", "F2", "E1", "E2", "M1", "M2", "P1", "P2")

#-------------------------------------------------
# Data Scaling and Converting dataframe to Matrix
#-------------------------------------------------
Data.log.scale <- scale(log10(Data + 1))
y <- as.matrix(Data.log.scale)

#-------------------------------------------------
# Clustering Algorithm
#-------------------------------------------------

hr <- hclust(as.dist(1 - cor(t(y), method = "pearson")), method = "complete")
hc <- hclust(as.dist(1 - cor(y, method = "spearman")), method = "complete")
mycl <-  cutree(hr, h = max(hr$height) / 1.01)
mycolhc <-   rainbow(length(unique(mycl)), start = 0.1, end = 0.9)
mycolhc <- mycolhc[as.vector(mycl)]
cluster <- as.matrix(mycl)

mycol <-  colorpanel(75, "red", "black", "green") # or try redgreen(75)

#-------------------------------------------------
# Heatmap generation
#-------------------------------------------------

heatmap.2(
  y,
  Rowv = as.dendrogram(hr),
  Colv = as.dendrogram(hc),
  col = mycol,
  density.info = "none",
  trace = "none",
  dendrogram = "both",
  scale = "row",
  labRow = NULL,
  labCol = NULL,
  margins = c(5, 10),
  RowSideColors = mycolhc
)



#-------------------------------------------------
# Alternatively, use pheatmap package
#-------------------------------------------------
library(pheatmap)

pheatmap(y) #default parameter

#parameters to modify. Try changing cluster_cols and clusters_rows to turn on and off hierarchical clustering 

pheatmap(y ,kmeans_k = NA, breaks = NA, border_color = "grey60",
          cellwidth = NA, cellheight = NA, scale = "none", cluster_rows = TRUE,
          cluster_cols = TRUE, clustering_distance_rows = "euclidean",
          clustering_distance_cols = "euclidean", clustering_method = "complete")


#--------------------------------------------------------------------------
# Sub cluster determination and clustering using kmeans
#---------------------------------------------------------------------------

library(factoextra) # clustering algorithms & visualization

## optimal cluster determination
fviz_nbclust(y, kmeans, method = "wss")
fviz_nbclust(y, kmeans, method = "silhouette")

final <- kmeans(y, 4, nstart = 25)
fviz_cluster(final, data = y)

#------------------------------------------------------
# Saving file as high quality PNG
#------------------------------------------------------

# Open a png file
png(filename ="this is the filename of the plot.png", width = 720, height = 720, units = "px", pointsize = 12, res=300)

# 2. Create a plot
pheatmap(y)

#close the device
dev.off() 

