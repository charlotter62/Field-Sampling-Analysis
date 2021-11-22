#Title: K-Means Colorado Sites
#Author: Charlotte Rivard
#Date: 5/4/21

#----Data Proprocessing------#
setwd("/Volumes/GoogleDrive/Shared drives/Rangeland Carbon/Colorado/field_sample_selection/")
library(readr)
#dataset <- read_csv("./hanks/hanks_10000_with_soilgrid.csv")
dataset <- read_csv("./harts/harts_10000_with_soilgrid.csv")
View(dataset)
dataset$aspect <- sin(pi*(dataset$aspect)/360) #Adjust aspect since 0 and 360 are same
subset <- dataset[,2:9]
#subset <- dataset[-which(dataset$om_0_15 < 0),2:9]
df <- data.frame(scale(subset))
View(df)

ncomp = 5
#------K-means---------#
#https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
set.seed(1)
kmeans_basic <- kmeans(df,centers=ncomp,nstart = 1)
kmeans_basic_table <- data.frame(kmeans_basic$size, kmeans_basic$centers)
kmeans_basic_df <- data.frame(Cluster = kmeans_basic$cluster, df)

# Proportions of each cluster
propdf <- data.frame(t(table(kmeans_basic_df$Cluster)))[,2:3]
colnames(propdf) <- c("cluster","counts")
propdf <- propdf[order(propdf$counts),]
propdf$prop <- propdf$counts/sum(propdf$counts)
propdf$perc <- round(propdf$prop*100)

library(ggplot2)
ggplot(data=propdf, aes(x=counts, y=cluster, fill=cluster)) +
  geom_bar(stat="identity") + geom_text(aes(label=counts), hjust=+0.5, size=3.5)
#barplot(table(kmeans_basic_df$Cluster))

#-----Scree plot------#
#https://towardsdatascience.com/how-to-use-and-visualize-k-means-clustering-in-r-19264374a53c
library(ggplot2)
library(factoextra)
factoextra::fviz_nbclust(df, kmeans, nstart=1, method = "wss") + 
  geom_vline(xintercept = ncomp, linetype = 1)

#----PCA plot-------#
p <- fviz_cluster(kmeans_basic, data = df, geom = c("point"),ellipse.type = "euclid")
pg <- ggplot_build(p)
p
#----Mapping Points---#
#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

#geodf <- data.frame(dataset[-which(dataset$om_0_15 < 0),".geo"])
geodf <- data.frame(dataset[,".geo"])
geodf$latlon <- gsub("[\\[\\]]", "", regmatches(geodf$.geo, gregexpr("\\[.*?\\]", geodf$.geo)))
geodf$latitude <- sapply(1:nrow(geodf),
                         function(i){
                           row <- geodf$latlon[i]
                           split <- strsplit(row,",")[[1]][2]
                           sub <- substr(split,1,nchar(split)-1)
                           return(as.numeric(sub))
                         })
geodf$longitude <- sapply(1:nrow(geodf),
                          function(i){
                            row <- geodf$latlon[i]
                            split <- strsplit(row,",")[[1]][1]
                            sub <- substr(split,2,nchar(split))
                            return(as.numeric(sub))
                          })
#write.csv(geodf[,c("latitude","longitude")], file="hanks_latlon.csv",row.names=FALSE)
geodf$cluster <- kmeans_basic_df$Cluster

#install.packages("ggmap")
library(ggmap)
geo_kmeans_df <- data.frame(geodf,kmeans_basic_df)
bbox <- make_bbox(lat = latitude, lon = longitude, data = geodf)
bbox
big <- get_map(location = bbox, source = "google", maptype = "satellite")

ggmap(big) + 
  geom_point(data = geodf, mapping = aes(x = longitude, y = latitude, color = as.factor(cluster)))


#-----Selecting 100 samples-------#
library(clhs)

pcadata <- data.frame(pg[["data"]][[1]])
finaldf <- data.frame(geodf[,3:5],df)

#get rid of clusters 3 & 4 bc they are dry
finaldf <- finaldf[finaldf$cluster!=3,] 
finaldf <- finaldf[finaldf$cluster!=4,]

#recalculate proportions
propdf <- data.frame(t(table(finaldf$cluster)))[,2:3]
colnames(propdf) <- c("cluster","counts")
propdf <- propdf[order(propdf$counts),]
propdf$prop <- propdf$counts/sum(propdf$counts)
propdf$perc <- round(propdf$prop*100)

#replot proportions
colors <- hue_pal()(ncomp)
colors <- colors[-c(3,4)]
show_col(colors)

library(ggplot2)
ggplot(data=propdf, aes(x=counts, y=cluster, fill=cluster)) +
  geom_bar(stat="identity") + geom_text(aes(label=counts), hjust=+0.5, size=3.5)+
  scale_fill_manual(values = colors)
  
samples <- data.frame()

#i <- 2
#oneclust <- finaldf[finaldf$cluster==i,]
##CLHS Sampling of this group
#clusterperc <- propdf$perc[propdf$cluster==i]
#sample_indices <- clhs(oneclust[,4:11], size = clusterperc, progress = TRUE, iter = 500)
#thesesamples <- oneclust[sample_indices,]
#samples <- rbind(samples,thesesamples)
clusterlist <- c(1,2,5)
for(j in 1:ncomp){
  i <- clusterlist[j]
  oneclust <- finaldf[finaldf$cluster==i,]
  #CLHS Sampling of this group
  clusterperc <- propdf$perc[propdf$cluster==i]
  sample_indices <- clhs(oneclust[,4:11], size = clusterperc, progress = TRUE, iter = 500)
  thesesamples <- oneclust[sample_indices,]
  samples <- rbind(samples,thesesamples)
}
#write.csv(samples, file="./harts/harts_samples.csv", row.names=FALSE)
write.csv(samples, file="./harts/harts_samples_no3-4.csv", row.names=FALSE)

#Map Samples
samples_geodf <- geodf[row.names(samples),]
ggmap(big)+
  geom_point(data = samples_geodf, mapping = aes(x = longitude, y = latitude, fill = as.factor(cluster)),color="Black",pch=21)+
  scale_fill_manual(values = colors)

#Map samples w/ full dataset
colors <- hue_pal()(ncomp)
colors <- colors[-c(3,4)]
ggmap(big) + 
  geom_point(data = geodf, mapping = aes(x = longitude, y = latitude, color = as.factor(cluster)))+
  geom_point(data = samples_geodf, mapping = aes(x = longitude, y = latitude, fill = as.factor(cluster)),color="Black",pch=21)+
  scale_fill_manual(values = colors)

#Plot in PCA space
library(scales)
colors <- hue_pal()(ncomp)

#show_col(colors)   
pcasamples <- pcadata[row.names(samples),]
fviz_cluster(kmeans_basic, data = df, geom = c("point"),ellipse.type = "euclid")+
  geom_point(data=pcasamples, aes(x,y, fill=as.factor(pcasamples$group)), size=2, color="black",pch=21)+
  scale_fill_manual(values = colors)

