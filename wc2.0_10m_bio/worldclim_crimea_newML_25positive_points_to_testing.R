#Extract climate data from WorldClim.org tiles for several locations and make data table
#Kathryn Turner Sept 16, 2013
rm(list= ls())
#download and unzip all relevant WorldClim geoTIFF files into a single directory.
#I used the highest resolution (~1km2), but should work for other resolutions too.
setwd('/home/mikhail/Documents/lsh17/worldclim/wc2.0_10m_bio/')
dir()#load packages: raster, rgdal, foreach
library(rgdal)
library(raster)
library(foreach)

#Read names of all files in directory into a list
#from http://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-data-frames
filenames <- list.files(path="~/data_directory/")
filenames <- grep('*.tif', list.files(), value = T)

#Load all geoTIFF files
for(i in filenames){
  #filepath <- file.path("~/data_directory/",i)
  filepath <- file.path(i)
  assign(i, raster(filepath))
}

#check that all files loaded properly by raster
#from http://stackoverflow.com/questions/15387727/use-object-names-as-list-names-in-r
list <- mget(filenames, envir=globalenv())

for(i in list){
  if (hasValues(i)==FALSE){
    print(i,"hasValues error")
  }
  if (inMemory(i)==TRUE){
    print(i, "inMemory error")
  }
  else{
    print("All checked out!")
  }
}

#to determine region of interest

library(maps)
crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='gray90', fill=TRUE)  

crimea_x_lims <- c(32.5, 36.6) 
crimea_y_lims <- c(44.4, 46)

abline(v = crimea_x_lims)
abline(h = crimea_y_lims)
#  # load table of latitude and longitude for locations of interest

Latitude <- seq(from = crimea_y_lims[1], to = crimea_y_lims[2], by = 0.125)
Longitude <- seq(from = crimea_x_lims[1], to = crimea_x_lims[2], by = 0.125)
lat_long<- expand.grid(Latitude = Latitude, Longitude = Longitude)
Pop <- as.factor(1:nrow(lat_long))
pop <- cbind(lat_long, Pop)


###pop <- cbind(Latitude, Longitude, Pop)
#pop <- read.table("pop.txt", header=T, sep="\t")
#pop <- readLines('pop.txt')
row.names(pop) <- pop$Pop
#pop$Pop <- as.factor(pop$Pop)
 head(pop)

####NOTE: WorldClim data, even at highest res, is averaged over 1 km2.
#If your location is too close to a coast (i.e. less than 1 km2),
#there will not be any information (nothing but NAs) in this data set.
#Therefore, it may be necessary to approximate some locations by moving
#them inland. I did this using Google Earth.

 load('/home/mikhail/Documents/lsh17/worldclim/clim_complete_crimea_0125grid.rda')
#load location coordinates as SpatialPoints
for(i in pop$Pop){
  assign(i,SpatialPoints(as.matrix(t(c(pop[i,2], pop[i,1])))))
}

#check that SpatialPoints load correctly from geoTIFFs
poplist <- mget(levels(pop$Pop), envir=globalenv())

tiffvector <- unlist(list)

#Optional quality check step. For smaller datasets, will tell you which population locations should be adjusted,
#in other words, which rows are all NA. See Note above, line 51. Or check after extracting data, see line 
#foreach(p=poplist, .combine='rbind') %:%
 # foreach(t=tiffvector, .combine='cbind') %do%{
  #  is.na(extract(t,p))
  #} #may take a while

#make climate data table
# #

#climate <- foreach(p=poplist, .combine='rbind') %:%
 #foreach(t=tiffvector, .combine='cbind') %do%{
  #myValue<-extract(t, p)
  #} #may take a while

#tidy table
popnames <- sort(as.character(pop$Pop))
clim <- as.data.frame(climate, row.names=popnames)
colnames(clim) <- filenames
##save(clim, file = '/home/mikhail/Documents/lsh17/worldclim/clim_complete_crimea_0125grid.rda')
#To check for populations that need to be adjusted/rows that are all NAs (See note, line 51)
#find rows that are all NAs, these are likely populations too close to large bodies of water
movepops <- clim[rowSums(is.na(clim)) == ncol(clim),]
#adjust population coordinates in pop df and re-run from line 63

head(clim)

#write table
#write.table(clim, file="/home/mikhail/Documents/lsh17/bioclimdata5_more_acc.txt")

#load table
#clim <- read.table("bioclimdata.txt", header=TRUE)
# #clim <- read.table( file="/home/mikhail/Documents/lsh17/bioclimdata5_more_acc.txt", header =T)
#now you have table of values and also a lot of NAs. So see "squishr" gist at https://gist.github.com/kgturner/6644150!
load('/home/mikhail/Documents/lsh17/worldclim/clim_complete_crimea_0125grid.rda')






#####mine
unique(lapply(clim, length))

#res <- list()
#tmps <- list()

#output <- c()
#for (i in pop$Pop) {
 # for (j in seq_along(clim)) {
  #   tmp <- clim[[j]][as.numeric(i)]
   #  tmps[as.numeric(i)] <- tmp
    # print(str(unlist(tmps)))
       #}
#  res[[as.numeric(i)]] <- tmps
  #output <- cbind(output, unlist(tmps))
#}

res <- c()
for (i in clim) {
  res <- cbind(res, i)
}

rownames(res) <- as.numeric(popnames)

#pop_reordered <- c()
#for (i in (rownames(res))) {
 # pop_reordered <- rbind(pop_reordered, pop[as.numeric(i),])
#}

res <- cbind(pop, res) # not pop_reordered

colnames(res)[4:22] <- names(clim)

plot(y = pop$Longitude, x = pop$Latitude, type = 'n')
for (i in 1:nrow(res)){
  for (j in 5:23){
  points(x = res[i,1], y = res[i,2],pch =1, lwd = (res[i,16]-min(res[,16], na.rm = T)), col ='royalblue')#, col = rainbow(vectocol)[which((vectocol)==vectocol[res[i,17]])])
  points(x = res[i,1], y = res[i,2],pch =1, lwd = (res[i,10]-min(res[,10], na.rm = T)), col ='papayawhip')#, col = rainbow(vectocol)[which((vectocol)==vectocol[res[i,17]])])
  }
}

vectocol <- (min(sort(unique(na.omit(res[,17])))):max(sort(unique(na.omit(res[,17])))))
library(ggplot2)
#res <- as.data.frame(res)
ggplot(data = res, mapping = aes(x = res[,2], y = res[,1], lwd = res[,10], alpha = res[,6], color = res[,9]))+geom_point()


#bioclimatic variables

raw <- 'BIO1 = Annual Mean Temperature, BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp)), BIO3 = Isothermality (BIO2/BIO7) (* 100), BIO4 = Temperature Seasonality (standard deviation *100), BIO5 = Max Temperature of Warmest Month, BIO6 = Min Temperature of Coldest Month, BIO7 = Temperature Annual Range (BIO5-BIO6), BIO8 = Mean Temperature of Wettest Quarter, BIO9 = Mean Temperature of Driest Quarter, BIO10 = Mean Temperature of Warmest Quarter, BIO11 = Mean Temperature of Coldest Quarter, BIO12 = Annual Precipitation, BIO13 = Precipitation of Wettest Month, BIO14 = Precipitation of Driest Month, BIO15 = Precipitation Seasonality (Coefficient of Variation), BIO16 = Precipitation of Wettest Quarter, BIO17 = Precipitation of Driest Quarter, BIO18 = Precipitation of Warmest Quarter, BIO19 = Precipitation of Coldest Quarter'
#raw1 <- gsub('BIO.*? = ', '', raw)
bioclim_vars <- unlist(strsplit(raw, split = ', '))
bioclim_vars <- substr(bioclim_vars, start = 8, 100)

names(res)[4:22] <- bioclim_vars

###  ggplot(data = res, mapping = aes(y = res$Latitude, x = res$Longitude, lwd = res$` Annual Precipitation`*2, alpha = res$`Annual Mean Temperature`, color = res$`Max Temperature of Warmest Month`, fill = res$`Temperature Annual Range (BIO5-BIO6)`))+geom_point()+scale_colour_gradientn(colours=rev(rainbow(7)))

#clusterization
  #which(res$Latitude<46)
res_no_nas <- res[complete.cases(res),]

colnames(res_no_nas) <- filenames
 #res_no_nas <- res_no_nas[which(res_no_nas$Latitude<46),]
#res_no_nas_matrix <- scale(as.matrix(res_no_nas))
res_no_nas_scaled <- scale(res_no_nas[,-c(1:3)], center = T, scale = T)
hclusted <- hclust(dist(res_no_nas_scaled), method = 'ward.D2')

#hclusted_all <- hclust(dist(res))
plot(hclusted)
#plot(hclusted_all)

#dend <- order.dendrogram(as.dendrogram(hclusted))

cut_hclusted <- cutree(hclusted, k = 5)

svg('/home/mikhail/Documents/lsh17/worldclim/ward_clustering_2_10_clusters.svg', height = 8, width = 8)
lwd= 4
par(mfrow = c(3,3),
    mar = rep(1.3,4))
for (i in 2:10){
  tmp <- cutree(hclusted, k = i)
  plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(length(unique(tmp)))[tmp], asp = 1, pch = 12, lwd = lwd, main = paste0('Worldclim data in ', i,' clusters (Ward method)'), ylim = crimea_y_lims, xlim = crimea_x_lims)
  lines(x = crimea$x, y = crimea$y)
}
dev.off()


dir.create('/home/mikhail/Documents/lsh17/worldclim/clusters_crimea_to_gif')
for (i in 2:10){
  tmp <- cutree(hclusted, k = i)
  plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(9)[tmp], asp = 1, pch = 20, xlab = 'Longitude', ylab = 'Latitude', main = paste0('Worldclim data in ', i,' clusters (Ward method)'))
  lines(x = crimea$x, y = crimea$y)
  
  png(paste0('/home/mikhail/Documents/lsh17/worldclim/clusters_crimea_to_gif/clusters_crimea_to_gif_', i, '.png'), width = 1000, height = 1000, res = 200)
  plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(9)[tmp], asp = 1, pch = 20, xlab = 'Longitude', ylab = 'Latitude', main = paste0('Worldclim data in ', i,' clusters (Ward method)'))
  dev.off()
  }

system('convert -delay 100 /home/mikhail/Documents/lsh17/worldclim_clusters_crimea_to_gif/*.png /home/mikhail/Documents/lsh17/worldclim_clusters_crimea_to_gif/crimea_10_clusters_hclust.gif')

svg('/home/mikhail/Documents/lsh17/worldclim/kmeans_clustering_2_10_clusters.svg', height = 8, width = 8)
lwd= 4
par(mfrow = c(3,3),
    mar = rep(1.3,4))
for (i in 2:10){
  tmp <- kmeans(res_no_nas_scaled, i)$cluster
  plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(length(unique(tmp)))[tmp], asp = 1, pch = 12, lwd = lwd, main = paste0('Worldclim data in ', i,' clusters (k-means method)'))
  lines(x = crimea$x, y = crimea$y)
}
dev.off()

library(cluster)
svg('/home/mikhail/Documents/lsh17/worldclim/pam_clustering_2_10_clusters.svg', height = 8, width = 8)
lwd= 4
par(mfrow = c(3,3),
    mar = rep(1.3,4))
for (i in 2:10){
  tmp <- pam(res_no_nas_scaled, i)$cluster
  plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(length(unique(tmp)))[tmp], asp = 1, pch = 12, lwd = lwd, main = paste0('Worldclim data in ', i,' clusters (PAM method)'))
  lines(x = crimea$x, y = crimea$y)
}
dev.off()


# three clustering methods
svg('/home/mikhail/Documents/lsh17/worldclim/3_methods_clustering_2_10_clusters.svg', height = 8, width = 24)
lwd= 4
par(mfrow = c(3,9),
    mar = rep(1.3,4))
for (i in 2:10){
    tmp <- cutree(hclusted, k = i)
    plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(length(unique(tmp)))[tmp], asp = 1, pch = 12, lwd = lwd, main = paste0(i,' clusters (Ward method)'), ylim = crimea_y_lims, xlim = crimea_x_lims)
    lines(x = crimea$x, y = crimea$y)
}
for (i in 2:10){
  tmp <- kmeans(res_no_nas_scaled, i)$cluster
  plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(length(unique(tmp)))[tmp], asp = 1, pch = 12, lwd = lwd, main = paste0(i,' clusters (k-means method)'))
  lines(x = crimea$x, y = crimea$y)
}
for (i in 2:10){
  tmp <- pam(res_no_nas_scaled, i)$cluster
  plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(length(unique(tmp)))[tmp], asp = 1, pch = 12, lwd = lwd, main = paste0(i,' clusters (PAM method)'))
  lines(x = crimea$x, y = crimea$y)
} 
dev.off()
##cut_hclusted_all <- cutree(hclusted_all, k = 4)
 
#rainbow(length(unique(cut_hclusted)))[cut_hclusted]
ggplot(data = res_no_nas, mapping = aes(y = res_no_nas$Latitude, x = res_no_nas$Longitude, lwd = res_no_nas$` Annual Precipitation`, alpha = res_no_nas$`Annual Mean Temperature`, color = rainbow(length(unique(cut_hclusted)))[cut_hclusted]))+geom_point()+coord_fixed()
#plot(x = crimea$x, y = crimea$y, xlim = c(32.5, 36.6), ylim = c(44.4, 46), xlab = 'Longitude', ylab = 'Latitude',type = 'l', asp = 1)
plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, col = rainbow(length(unique(cut_hclusted)))[cut_hclusted], asp = 1, pch = 20)
lines(x = crimea$x, y = crimea$y)
# #ggplot(data = res, mapping = aes(y = res$Latitude, x = res$Longitude, lwd = res$` Annual Precipitation`, alpha = res$`Annual Mean Temperature`, color = rainbow(length(unique(cut_hclusted_all)))[cut_hclusted_all]))+geom_point()+scale_colour_identity()

#res_no_nas <- res
k=5
kmeansed <- kmeans(res_no_nas_scaled, k)
ggplot(data = res_no_nas, mapping = aes(y= res_no_nas$Latitude, x = res_no_nas$Longitude, lwd = res_no_nas$` Annual Precipitation`, alpha = res_no_nas$`Annual Mean Temperature`, color = rainbow(length(unique(kmeansed$cluster)))[kmeansed$cluster]))+geom_point()+scale_colour_identity()+ coord_fixed()
##(colours=rev(rainbow(length(unique(cut_hclusted)))))

#scaled data prior to clusterization
library(RColorBrewer)
colvec <- brewer.pal(k, name = 'Dark2')
kmeansed <- kmeans(scale(res_no_nas[,-(1:3)], center = T, scale = T), 3)
ggplot(data = res_no_nas, mapping = aes(y= res_no_nas$Latitude, x = res_no_nas$Longitude, lwd = res_no_nas$` Annual Precipitation`, alpha = res_no_nas$`Annual Mean Temperature`, color = colvec[kmeansed$cluster]))+geom_point()+scale_colour_identity()+ coord_fixed()

#dend <- order.dendrogram(as.dendrogram(hclusted))

#rainbow(length(unique(cut_hclusted)))[cut_hclusted]
ggplot(data = res_no_nas, mapping = aes(y = res_no_nas$Latitude, x = res_no_nas$Longitude, lwd = res_no_nas$` Annual Precipitation`, alpha = res_no_nas$`Annual Mean Temperature`, color = rainbow(length(unique(cut_hclusted)))[cut_hclusted]))+geom_point()+coord_fixed()


ggplot(data = res_no_nas, mapping = aes(y = res_no_nas$Latitude, x = res_no_nas$Longitude, color = res_no_nas$` Annual Precipitation`))+geom_point(size = 0.5)+coord_fixed()
ggplot(data = res_no_nas, mapping = aes(y = res_no_nas$Latitude, x = res_no_nas$Longitude)) + geom_point(aes(size = 0.5, color = res_no_nas$`Annual Mean Temperature`))+coord_fixed()


library(factoextra)
library(FactoMineR)

#HCPC(res, nb.clust = 0, min = 3, max = NULL, graph = TRUE)

#res: Either the result of a factor analysis or a data frame.
#nb.clust: an integer specifying the number of clusters. Possible values are:
  #0: the tree is cut at the level the user clicks on
#-1: the tree is automatically cut at the suggested level
#Any positive integer: the tree is cut with nb.clusters clusters
#min, max: the minimum and the maximum number of clusters to be generated, respectively
#graph: if TRUE, graphics are displayed

# #Case of continuous variables

#We start by computing again the principal component analysis (PCA). The argument ncp = 3 is used in the function PCA() to keep only the first three principal components. Next, the HCPC is applied on the result of the PCA.

library(FactoMineR)
library(factoextra)
# Compute PCA with ncp = 3
res.pca <- PCA(res_no_nas[,-c(1:3)], ncp = 3, graph = FALSE)
# Compute hierarchical clustering on principal components
#res.hcpc <- HCPC(res.pca, graph = T)

#To visualize the dendrogram generated by the hierarchical clustering, we’ll use the function fviz_dend() [factoextra package]:
  
#fviz_dend(res.hcpc, 
 #           cex = 0.7,                     # Label size
  #          palette = "jco",               # Color palette see ?ggpubr::ggpar
   #         rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
    #        rect_border = "jco",           # Rectangle color
     #       labels_track_height = 0.8      # Augment the room for labels
#)

#The dendrogram suggests 3 clusters solution.

#It’s possible to visualize individuals on the principal component map and to color individuals according to the cluster they belong to. The function fviz_cluster() [in factoextra] can be used to visualize individuals clusters.

#fviz_cluster(res.hcpc,
 #            repel = TRUE,            # Avoid label overlapping
  #           show.clust.cent = TRUE, # Show cluster centers
   #          palette = "jco",         # Color palette see ?ggpubr::ggpar
    #         ggtheme = theme_minimal(),
     #        main = "Factor map",labelsize = .00001
#)

#You can also draw a three dimensional plot combining the hierarchical clustering and the factorial map using the R base function plot():
  
# Principal components + tree
plot(res.hcpc, choice = "3D.map")

#The function HCPC() returns a list containing:
  
#  data.clust: The original data with a supplementary column called class containing the partition.
#desc.var: The variables describing clusters
#desc.ind: The more typical individuals of each cluster
#desc.axes: The axes describing clusters

#To display the original data with cluster assignments, type this:
  
head(res.hcpc$data.clust, 10)

#In the table above, the last column contains the cluster assignments.

#To display quantitative variables that describe the most each cluster, type this:
  
res.hcpc$desc.var$quanti

#…and so on …

#Similarly, to show principal dimensions that are the most associated with clusters, type this:
  
res.hcpc$desc.axes$quanti

#Finally, representative individuals of each cluster can be extracted as follow:
  
res.hcpc$desc.ind$para


# #PCA, no clustering. base R


#Compute PCA in R using prcomp()

res.pca <- prcomp(res_no_nas[,-c(1:3)], scale = TRUE)

#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.

fviz_eig(res.pca)
abline(h = 12)
#Graph of individuals. Individuals with a similar profile are grouped together.

require(gridExtra)
colvec <- rainbow(length(unique(cut_hclusted)))[cut_hclusted]
plot1 <- fviz_pca_ind(res.pca,axes = c(1,2),
                      col.var = "contrib", # Variables color
                      #col.ind = rainbow(length(unique(cut_hclusted)))[cut_hclusted], 
                      col.ind = colvec,
                      label = 'var',# Color by the quality of representation
                      #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)
plot2 <- fviz_pca_ind(res.pca,axes = c(1,3),
                      col.var = "contrib", # Variables color
                      #col.ind = rainbow(length(unique(cut_hclusted)))[cut_hclusted], 
                      col.ind = colvec,
                      label = 'var',# Color by the quality of representation
                      #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)
plot3 <- fviz_pca_ind(res.pca,axes = c(2,3),
                      col.var = "contrib", # Variables color
                      #col.ind = rainbow(length(unique(cut_hclusted)))[cut_hclusted], 
                      col.ind = colvec,
                      label = 'var',# Color by the quality of representation
                      #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)

plot4 <- fviz_pca_var(res.pca,axes = c(1,2),
                      col.var = "contrib", # Variables color
                      # Color palette see ?ggpubr::ggpar#col.ind = rainbow(length(unique(cut_hclusted)))[cut_hclusted], 
                      #col.ind = cut_hclusted,
                      label = 'var',# Color by the quality of representation
                      #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)
plot5 <- fviz_pca_var(res.pca,axes = c(1,3),
                      col.var = "contrib", # Variables color
                      #col.ind = rainbow(length(unique(cut_hclusted)))[cut_hclusted], 
                      #col.ind = cut_hclusted,
                      label = 'var',# Color by the quality of representation
                      #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)
plot6 <- fviz_pca_var(res.pca,axes = c(2,3),
                      col.var = "contrib", # Variables color
                      #col.ind = rainbow(length(unique(cut_hclusted)))[cut_hclusted], 
                      #col.ind = cut_hclusted,
                      label = 'var',# Color by the quality of representation
                      #gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                      repel = TRUE     # Avoid text overlapping
)
plot7 <- ggplot(data = res_no_nas, mapping = aes(y = res_no_nas$Latitude, x = res_no_nas$Longitude, color = colvec))+geom_point()+coord_fixed()+theme(legend.position="none")
#plot7 <- plot(x = crimea$x, y = crimea$y, xlim = c(32.5, 36.6), ylim = c(44.4, 46), xlab = 'Longitude', ylab = 'Latitude',type = 'l', asp = 1)
svg('/home/mikhail/Documents/lsh17/pca_inds_var_cluster_map.svg', height = 15, width = 15)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, ncol=3)
dev.off()


#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.

fviz_pca_var(res.pca,
             label = 'none',
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Biplot of individuals and variables

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "contrib", # Variables color
                col.ind = cut_hclusted,
                label = 'var'# Individuals color
)

#biplot(res.pca)
#maps

library(dismo)
g <- gmap(x = cbind(res$Longitude, res$Latitude), type = 'roadmap' )

plot(g, inter=TRUE)


library(maps)
crimea <- map(ylim=c(44.3, 46), xlim=c(32.5,36.6), col='gray90', fill=TRUE)  

crimea_x_lims <- c(32.5, 36.6) 
crimea_y_lims <- c(44.4, 46)

abline(v = crimea_x_lims)
abline(h = crimea_y_lims)
#crimea_x <- crimea$x[c(which(crimea$x > 32.5), which(crimea$x > 36.6))]
#crimea_y <- crimea$y[c(which(crimea$y > 44.4), which(crimea$y > 46))]

plot(x = crimea$x, y = crimea$y, xlim = c(32.5, 36.6), ylim = c(44.4, 46), xlab = 'Longitude', ylab = 'Latitude',type = 'l', asp = 1)
#ggplot(data = crimea), aes(x = crimea$x, y = crimea$y))+geom_line()

library(mapdata)

map <- map( ylim=range(res$Latitude), xlim=range(res$Longitude), col='gray90')
points(x = P.tragium$Latitude, y = P.tragium$Longitude)

plot(x = map$x[which(map$x %in% res$Latitude)], y = map$y[which(map$y %in% range(res$Longitude))], type = 'l')
ggplot(data = map, aes(x = map$x, y = map$y, col =1))
+geom_contour()
# # supervised ml


species <- readxl::read_xlsx('/home/mikhail/Documents/lsh17/species-points_final_update.xlsx')[,1:3]
par (mar = c(15,3,2,2))
barplot(sort(table(species$Species), decreasing = T)[1:20], las = 2)
P.tragium <- species[which(species$Species=='Pimpinella tragium Vill.'),]
#smaller clim for P.tragium


P.tragium_lat_long<- as.data.frame(cbind(as.numeric(P.tragium$Latitude), as.numeric(P.tragium$Longitude)))
P.tragium_Pop <- as.factor(1:nrow(P.tragium_lat_long))
P.tragium_pop <- cbind((P.tragium_lat_long), P.tragium_Pop)
colnames(P.tragium_pop)[1:2] <- c('Latitude', 'Longitude')

###pop <- cbind(Latitude, Longitude, Pop)
#pop <- read.table("pop.txt", header=T, sep="\t")
#pop <- readLines('pop.txt')
row.names(P.tragium_pop) <- P.tragium_pop$P.tragium_Pop
#pop$Pop <- as.factor(pop$Pop)
head(P.tragium_pop)
####NOTE: WorldClim data, even at highest res, is averaged over 1 km2.
#If your location is too close to a coast (i.e. less than 1 km2),
#there will not be any information (nothing but NAs) in this data set.
#Therefore, it may be necessary to approximate some locations by moving
#them inland. I did this using Google Earth.

#load location coordinates as SpatialPoints
for(i in P.tragium_pop$P.tragium_Pop){
  assign(i,SpatialPoints(as.matrix(t(c(P.tragium_pop[i,2], P.tragium_pop[i,1])))))
}

#check that SpatialPoints load correctly from geoTIFFs
P.tragium_poplist <- mget(levels(P.tragium_pop$P.tragium_Pop), envir=globalenv())


tiffvector <- unlist(list)

#Optional quality check step. For smaller datasets, will tell you which population locations should be adjusted,
#in other words, which rows are all NA. See Note above, line 51. Or check after extracting data, see line 
#foreach(p=poplist, .combine='rbind') %:%
# foreach(t=tiffvector, .combine='cbind') %do%{
#  is.na(extract(t,p))
#} #may take a while

#make climate data table
# #

#P.tragium_climate <- foreach(p=P.tragium_poplist, .combine='rbind') %:%
  #foreach(t=tiffvector, .combine='cbind') %do%{
    #myValue<-extract(t, p)
  #} #may take a while
#save(P.tragium_climate, file = '/home/mikhail/Documents/lsh17/P.tragium_climate.rda')



# ml part
load('/home/mikhail/Documents/lsh17/worldclim/P.tragium_climate.rda')
P.tragium_popnames <- sort(as.character(P.tragium_pop$P.tragium_Pop))
P.tragium_clim <- as.data.frame(P.tragium_climate, row.names=P.tragium_popnames)
colnames(P.tragium_clim) <- filenames


P.tragium_res <- c()
for (i in P.tragium_clim) {
  P.tragium_res <- cbind(P.tragium_res, i)
}

rownames(P.tragium_res) <- as.numeric(P.tragium_popnames)

P.tragium_res <- cbind(P.tragium_pop, P.tragium_res) # not pop_reordered

colnames(P.tragium_res)[4:22] <- names(P.tragium_clim)

plot(x = crimea$x, y = crimea$y, xlim = c(32.5, 36.6), ylim = c(44.4, 46), xlab = 'Longitude', ylab = 'Latitude',type = 'l', asp = 1, main = 'Pimpinella tragium distribution')
points(x = P.tragium$Longitude, y = P.tragium$Latitude, col =1, pch = 16, lwd = 2)
#  save(P.tragium_clim, file = '/home/mikhail/Documents/lsh17/wc2.0_10m_bio/P.tragium_clim.rda')
#ggplot(data = res_no_nas, mapping = aes(y= res_no_nas$Latitude, x = res_no_nas$Longitude, lwd = res_no_nas$` Annual Precipitation`, alpha = res_no_nas$`Annual Mean Temperature`, color = colvec[kmeansed$cluster]))+geom_point()+scale_colour_identity()+ coord_fixed()+geom_point(data = P.tragium, aes(x = P.tragium$Longitude, y= P.tragium$Latitude, size = 1, colour = 1, alpha =1))

#ggplot(data = P.tragium_pop, aes(x = P.tragium$Longitude, y= P.tragium$Latitude, size = 1, colour = 1, alpha =1))+geom_point()+coord_fixed()


#library(caret)
#to_train <- cbind(rbind(clim, P.tragium_clim), c(rep('no', nrow(clim)), rep('P.tragium', nrow(P.tragium_clim))))
#to_train_no_nas <-   (to_train[complete.cases(to_train),])     
#colnames(to_train_no_nas)[20] <- 'Class'


library(maxnet)

colnames(P.tragium_res) <- NULL -> colnames(P.tragium_res)
rownames(res_no_nas) <- NULL -> colnames(res_no_nas)

#P.tragium_climate_no_nas <- P.tragium_climate[complete.cases(P.tragium_climate),] #1 NA!
    
P.tragium_variables <- (P.tragium_res[1:131,4:23])#!
P.tragium_coords <- (P.tragium_res[1:131,1:2])


set.seed(999)

rand_inds_background <- sample(nrow(res_no_nas), nrow(res_no_nas))
background_all_250_shuffled <-  res_no_nas[rand_inds_background, ] 

background_130points_variables <- unname(as.matrix(background_all_250_shuffled[1:130,4:22]))
background_130points_coords <- as.matrix(background_all_250_shuffled[1:130,1:2])

background_rest_120points_variables <- as.matrix(background_all_250_shuffled[-c(1:130),4:22])
background_rest_120points_coords <- as.matrix(background_all_250_shuffled[-c(1:130),1:2])

#tragium <- rbind(P.tragium_all_points, background_130points)
#p <- c(rep(1, nrow(P.tragium_all_points)), rep(0, nrow(background_130points)))

#for testing 

#uniform input data

num_to_testing <- 106
P.tragium_first_106points_coords <- unname(as.matrix(P.tragium_coords[1:num_to_testing,]))

P.tragium_first_106points <- unname(as.matrix(P.tragium_variables[1:num_to_testing,]))
P.tragium_rest_25points <- as.matrix(P.tragium_variables[-c(1:num_to_testing),])

P.tragium_first_106points_coords <- unname(as.matrix(P.tragium_coords[1:num_to_testing,]))
P.tragium_rest_25points_coords <- as.matrix(P.tragium_coords[-c(1:num_to_testing),])


colnames(background_130points_variables) <- colnames(P.tragium_first_106points)

p_train <- c(rep(0, nrow(background_130points_variables)), rep(1, nrow(P.tragium_first_106points)))
p_test <- c(rep(0, nrow(background_rest_120points_coords)), rep(1, nrow(P.tragium_rest_25points)))

training <- ((rbind(background_130points_variables, P.tragium_first_106points)))
testing <- ((rbind(as.matrix(background_rest_120points_variables), P.tragium_rest_25points)))
testing_coords <-  as.matrix(unname(rbind(as.matrix(background_rest_120points_coords), P.tragium_rest_25points_coords)))


colnames(training) <- colnames(P.tragium_rest_25points)
scaled_training <- as.data.frame(scale(training, center = T, scale = T))
colnames(scaled_training) <- names(P.tragium_clim)

#NA embedded

which(!complete.cases(training))

df_training <- as.data.frame(scale(training[complete.cases(training),], center = T, scale = T))
colnames(df_training) <- names(P.tragium_clim)

p_train_nonas <- p_train[-which(!complete.cases(training))]
testing_coords_nonas <- testing_coords[-which(!complete.cases(training)),]

df_testing <- as.data.frame(scale(testing[complete.cases(testing),], center = T, scale = T))
colnames(df_testing) <- names(P.tragium_clim)
##rownames(training) <- 1:nrow(training)
##colnames(training) <- colnames(res_no_nas)

model <- maxnet(p = p_train_nonas, data = df_training,  maxnet.formula(p_train_nonas, df_training))
plot(model, type="cloglog")

pred_maxnet <- predict(model, as.data.frame(scale(df_testing, center = T, scale = T)), s = "lambda.min")

which(pred_maxnet>2)
#getting coordinates for prediction
#maxnet_coords_over05 <- res_no_nas[-c(1:130),1:3][which(pred_maxnet>0.5),1:3]

#setting cutoff value

cutoff <- -1
maxnet_coords_over05 <- as.data.frame(testing_coords_nonas[which(pred_maxnet> cutoff),])
colnames(maxnet_coords_over05) <- colnames(res)[1:2]
maxnet_coords_over3 <- testing_coords_nonas[which(pred_maxnet>3),]

predicted_p_test <- rep(0, length(p_test))
predicted_p_test[which(pred_maxnet> cutoff)] <- 1

confusionMatrix(predicted_p_test, p_test)
maxnet_cm_accuracy <- confusionMatrix(predicted_p_test, p_test)$overall['Accuracy']
maxnet_cm_sensitivity <- confusionMatrix(predicted_p_test, p_test)$byClass['Sensitivity']
maxnet_cm_specificity <- confusionMatrix(predicted_p_test, p_test)$byClass['Specificity']

library(caret)
#common settings
rownames(df_training)# <- seq_along(rownames(training))
training$Class <- as.factor(p_train)
df_trainingClass <- df_training
df_trainingClass$Class <- as.factor(p_train_nonas)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
#  savePredictions = T,
  ## Estimate class probabilities
 # classProbs = TRUE
  ## Evaluate performance using 
  ## the following function
  #summaryFunction = twoClassSummary
  )
#caret train in a loop

methods_to_loop <- c('plsRglm', 'LogitBoost','gbm', 'mlpML', 'nb', 'rf', 'svmRadial')

accuracies <- c()
mean_accuracies <- c()

cm_accuracies <- c()
cm_sensitivities <- c()
cm_specificities <- c()
for (i in methods_to_loop){
  set.seed(825)
  tmpFit1 <- caret::train(Class ~ ., data = df_trainingClass, #removing metadata columns, but no class column
                        method = i, 
                        trControl = fitControl,
  #                      metric = 'ROC',
                        ## This last option is actually one
                        ## for gbm() that passes through
                        verbose = FALSE,
                        preProcess = c('center','scale')
  )
  #gbmFit1
  print(paste0(i, tmpFit1$results$Accuracy))
  accuracies <- c(accuracies, paste0(tmpFit1$results$Accuracy, collapse = ', '))
  mean_accuracies <- c(mean_accuracies, mean(tmpFit1$results$Accuracy))
  assign(paste0(i, 'Fit'), tmpFit1)
  tmp2 <- predict(tmpFit1, newdata = df_testing)
  
  #dummy <- matrix(NA, nrow(P.tragium_rest_25points), ncol = 2)
  tmp3  <- testing_coords[which(tmp2 ==1),]

  assign(paste0('predicted_coord_', i), tmp3)
  
  
  #accuracies using confusion matrix
  cm_accuracies <- c(cm_accuracies, confusionMatrix(tmp2, reference = p_test)$overall['Accuracy'])
  cm_sensitivities <- c(cm_sensitivities, confusionMatrix(tmp2, reference = p_test)$byClass['Sensitivity'])
  cm_specificities <- c(cm_specificities, confusionMatrix(tmp2, reference = p_test)$byClass['Specificity'])
}

par(mfrow = c(3,3),
    mar = rep(1,4))
#svg('/home/mikhail/Documents/lsh17/maxent_p.tragium_several_ps.svg', width = 8, height = 8)
#svg('/home/mikhail/Documents/lsh17/maxent_p.tragium_p_1.svg', width = 8, height = 8)
lwd = 9
pch = 22
alpha = 15

plot(x = crimea$x, y = crimea$y, xlim = c(32.5, 36.6), ylim = c(44.4, 46), xlab = 'Longitude', ylab = 'Latitude',type = 'l', asp = 1, main = 'Maxent')
points(x = P.tragium$Longitude, y = P.tragium$Latitude, col =1, pch = pch, lwd = lwd)
#points(x = maxnet_coords_over05$Longitude, y = maxnet_coords_over05$Latitude, col = adjustcolor("red", alpha=0.15),  pch = pch, lwd = lwd)
points(x = maxnet_coords_over05$Longitude, y = maxnet_coords_over05$Latitude, col = adjustcolor("red", alpha=1),  pch = pch, lwd = lwd)
#points(x = maxnet_coords_over2$Longitude, y = maxnet_coords_over2$Latitude, col = adjustcolor("red", alpha=1),  pch = pch, lwd = lwd)
#dev.off()

#care methods plotting in loop

#caret_methods <- grep('predicted_coord_', x = ls(), value = T)
caret_preds_coords <- grep('predicted_coord_', x = ls(), value = T)

colnames(res_no_nas) <- colnames(res)
for (i in caret_preds_coords){
  plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, type = 'n', asp = 1, pch = 20, main = (gsub(pattern = 'predicted_coord_*', replacement = '', gsub(pattern = '*Fit1', replacement = '', i))))
  lines(x = crimea$x, y = crimea$y)
  points(x = P.tragium$Longitude, y = P.tragium$Latitude, col =1, pch = pch, lwd = lwd)
  #plot(x = crimea$x, y = crimea$y, xlim = c(32.5, 36.6), ylim = c(44.4, 46), xlab = 'Longitude', ylab = 'Latitude',type = 'l', asp = 1, main = 'Pimpinella tragium distribution')
  points(x = get(i)[,2], y = get(i)[,1], col = adjustcolor("red", alpha=1),  pch = pch, lwd = lwd)
  #dev.off()
}

plot(x = res_no_nas$Longitude, y = res_no_nas$Latitude, type = 'n', asp = 1, pch = 20, xaxt = 'n', yaxt = 'n', ylab = '', xlab='', bty = 'n', main = '')
legend('center', legend = c('Samples collected', 'Prediction'),  fill = c(1,2), bty = 'n')

names
barplot(mean_accuracies, names.arg = methods_to_loop)

#ones based on confusion matrices

par(mfrow = c(1,3),
    oma = c(5,2,2,2))
ylim = c(0, 0.85)
barplot(c(maxnet_cm_accuracy, cm_accuracies), names.arg = c('maxnet',methods_to_loop), las = 2, main = 'Accuracy values', ylim = ylim)
barplot(c(maxnet_cm_sensitivity, cm_sensitivities), names.arg = c('maxnet',methods_to_loop), las = 2, main = 'Sensitivity values', ylim = ylim)
barplot(c(maxnet_cm_specificity, cm_specificities), names.arg = c('maxnet',methods_to_loop), las = 2, main = 'Specificity values', ylim = ylim)

#ROCs

library(pROC)
# Select a parameter setting
selectedIndices <- tmpFit1$pred$mtry == 2
# Plot:
plot.roc(rfFit$pred$obs[selectedIndices],
         rfFit$pred$M[selectedIndices])


data(aSAH)

# Basic example
roc(aSAH$outcome, aSAH$s100b,
    levels=c("Good", "Poor"))

roc(as.numeric(pred_maxnet>3), pred_maxnet)

predicted_y <- pred_maxnet
predicted_y[pred_maxnet>2]  <- 1
predicted_y[pred_maxnet<=2] <- 0
Yactual                       <- mat[1:t, ncol(mat)]
confusion_matrix              <- ftable(Yactual, predicted_y)
accuracy                      <- 100* (sum(diag(confusion_matrix)) / length(predicted_y))




#ROC Curves

# Calculate sensitivity and false positive measures for logit model (glm)

fity_ypos <- FullcovModel$fitted[y == 1]
fity_yneg <- FullcovModel$fitted[y == 0]

sort_fity <- sort(FullcovModel$fitted.values)

sens <- 0
spec_c <- 0


  sens <- c(sens, mean(fity_ypos >= sort_fity[i]))
  spec_c <- c(spec_c, mean(fity_yneg >= sort_fity[i]))
  
} 

# roc curve on glm model. Calculate sensitivity and false positive measure for random forest model

fity_ypos2 <- as.numeric(rf$pred[y == 1]) - 1
fity_yneg2 <- as.numeric(rf$pred[y == 0]) - 1

sort_fity2 <- as.numeric(sort(rf$pred)) - 1

sens2 <- 0
spec_c2 <- 0

for (i in length(sort_fity2):1){
  sens2 <- (c(sens2, mean(fity_ypos2 >= sort_fity2[i])))
  spec_c2 <- (c(spec_c2, mean(fity_yneg2 >= sort_fity2[i])))
} 

# plot ROC curves

plot(spec_c, sens, xlim = c(0, 1), ylim = c(0, 1), type = "l", 
     xlab = "false positive rate", ylab = "true positive rate", col = 'blue')
abline(0, 1, col= "black")

lines(spec_c2, sens2, col='green')
legend("topleft", legend = c("logit","random forest") , pch = 15, bty = 'n', col = c("blue","green"))


#their glmnet

library(ROCR)
library(glmnet)
library(caret)

df <- data.matrix(… ) # dataframe w/ predictor variables & a response variable
# col1 = response var; # cols 2:10 = predictor vars

# Create training subset for model development & testing set for model performance testing
inTrain <- createDataPartition(df$ResponsVar, p = .75, list = FALSE)
Train <- df[ inTrain, ]
Test <- df[ -inTrain, ]

# Run model over training dataset
lasso.model <- cv.glmnet(x = Train[,2:10], y = Train[,1], 
                         family = 'binomial', type.measure = 'auc')

# Apply model to testing dataset
Test$lasso.prob <- predict(lasso.model,type="response", 
                           newx = Test[,2:10], s = 'lambda.min')
pred <- prediction(Test$lasso.prob, Test$ResponseVar)

# calculate probabilities for TPR/FPR for predictions
perf <- performance(pred,"tpr","fpr")
performance(pred,"auc") # shows calculated AUC for model
plot(perf,colorize=FALSE, col="black") # plot ROC curve
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

#Mine

pred_maxnet <- predict(model, type="logistic", as.data.frame(scale(test, center = T, scale = T)), s = "lambda.min")
pred <- prediction(Test$lasso.prob, Test$ResponseVar)



# alternative inpt data - some positive signals in testing set

#for testing 

#uniform input data

background_250points <- as.matrix(res_no_nas[,4:22])
P.tragium_toMl <- P.tragium_all_points

background_P.tragium_380points <- rbind(background_250points, P.tragium_toMl)

set.seed(999)
rand_inds <- sample(nrow(background_P.tragium_380points), nrow(background_P.tragium_380points))
p_380points <- c(rep(0, nrow(background_250points)), rep(1, nrow(P.tragium_toMl)))
background_P.tragium_380points <- background_P.tragium_380points[rand_inds,]
p_380points <- p_380points[rand_inds]


mat = as.matrix(unname(background_P.tragium_380points[1:200,]))
test = as.matrix(unname(background_P.tragium_380points[-c(1:200),]))

model <- maxnet(p_380points[1:200], as.data.frame(scale(mat, center = T, scale = T)),  maxnet.formula(p_380points[1:200],as.data.frame(scale(mat, center = T, scale = T))))
pred_maxnet <- predict(model, as.data.frame(scale(test, center = T, scale = T)), s = "lambda.min")

#caret models ROC

# 
caret_fits <- grep('Fit', x = ls(), value = T)


L <- list(gbmFit=gbmFit,LogitBoostFit=LogitBoostFit,mlpMLFit=mlpMLFit,nbFit=nbFit,plsRglmFit=plsRglmFit,rfFit=rfFit,svmRadialFit=svmRadialFit)

results <- resamples(L)
# Table comparison
summary(results)

#

