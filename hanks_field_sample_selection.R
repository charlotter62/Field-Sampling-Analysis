#Title: K-Means Colorado Sites
#Author: Charlotte Rivard
#Date: 5/4/21

#----Data Proprocessing------#
setwd("/Volumes/GoogleDrive/Shared drives/Rangeland Carbon/Colorado/field_sample_selection/")
library(readr)
dataset <- read_csv("./hanks/hanks_10000_with_soilgrid.csv")
#dataset <- read_csv("./harts/harts_10000_with_soilgrid.csv")
View(dataset)
dataset$aspect <- sin(pi*(dataset$aspect)/360) #Adjust aspect since 0 and 360 are same
subset <- dataset[,2:9]
#subset <- dataset[-which(dataset$om_0_15 < 0),2:9]
df <- data.frame(scale(subset))
View(df)

ncomp = 7
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
#write.csv(geodf[,c("latitude","longitude")], file="hanks_10000_latlon.csv",row.names=FALSE)
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
road_distance <- read_csv("./hanks/hank_samples_distance_toRoad.csv")
road_distance$cluster <- geodf$cluster
finaldf <- data.frame(road_distance,df)

#For each cluster
samples <- data.frame()

for(i in 1:ncomp){
  
  oneclust <- finaldf[finaldf$cluster==i,]
  ggmap(big) + 
    geom_point(data = oneclust, mapping = aes(x = longitude, y = latitude, color = HubDist, fill=HubDist))
  
  #Just samples close to road
  oneclustclose <- oneclust[oneclust$HubDist<=0.373,] #Up to 600m away
  oneclustclose <- oneclustclose[oneclustclose$HubDist>=0.031,] #At least 50m away
  
  ggmap(big) + 
    geom_point(data = oneclustclose, mapping = aes(x = longitude, y = latitude, color = HubDist, fill=HubDist))
  
  #CLHS Sampling of this group
  clusterperc <- propdf$perc[propdf$cluster==i]
  sample_indices <- clhs(oneclustclose[,6:13], size = clusterperc, progress = TRUE, iter = 500)
  thesesamples <- oneclustclose[sample_indices,]
  ggmap(big) + 
    geom_point(data = samples, mapping = aes(x = longitude, y = latitude, color = HubDist, fill=HubDist))
  
  samples <- rbind(samples,thesesamples)
}
write.csv(samples, file="./hanks/hank_samples.csv", row.names=FALSE)

#load roads
library(rgdal)
roads <- readOGR("./shapefile/Hanks_Valley_Roads-line.shp")
roads <-spTransform(roads, CRS("+init=epsg:4326"))
library(broom)
roads <- tidy(roads)

#Plot samples on map
ggmap(big)+
  geom_path(data = fortify(roads),aes(long, lat, group = group))+
  geom_point(data = samples, mapping = aes(x = longitude, y = latitude, fill = as.factor(cluster)),color="Black",pch=21)

#Plot in PCA space
library(scales)
colors <- hue_pal()(ncomp)
#show_col(colors)   
pcadata <- data.frame(pg[["data"]][[1]])
pcasamples <- pcadata[row.names(samples),]
fviz_cluster(kmeans_basic, data = df, geom = c("point"),ellipse.type = "euclid")+
  geom_point(data=pcasamples, aes(x,y, fill=as.factor(pcasamples$group)), size=2, color="black",pch=21)+
  scale_fill_manual(values = colors)
