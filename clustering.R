library("readxl")

data = read_excel("/home/marlon/mainfolder/marlon/USFQ/DataMining/8_K_Means/P8/dataset.xlsx")

#Data treatment
data = as.data.frame(lapply(data, as.numeric))

str(data, list.len=ncol(data))

summary(data)

#Dealing with NA's
nasClear = function(x){
    data_without_nas = x
    for(i in 1:ncol(data)){
        data_without_nas[is.na(data_without_nas[,i]), i] = mean(data_without_nas[,i], na.rm = T)
    }
    return(data_without_nas)
}

clear_data = nasClear(data)

summary(clear_data)

#Normalization
normalize = function(x){
    if((max(x) - min(x)) != 0){
        return((x - min(x)) / (max(x) - min(x))) 
    }else{
        return(x)
    }
}

normalize_data = as.data.frame(lapply(clear_data, normalize))

str(normalize_data, list.len = ncol(normalize_data))

#K Means

wss = 0

library(cluster)

for(i in 1:10){
    km.out = kmeans(normalize_data, centers = i, nstart = 25)
    wss[i] = km.out$tot.withinss
}

plot(1:10, wss, type = "b",
    xlab = "Number of Clusters",
    ylab = "Within groups sum of squares")


avg_sil <- function(k) {
    km.res <- kmeans(normalize_data, centers = k, nstart = 25)
    ss <- silhouette(km.res$cluster, dist(normalize_data))
    mean(ss[, 3])
}

k.values <- 2:10

library(purrr)

avg_sil_values <- map_dbl(k.values, avg_sil)

    plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")

k = 3

km_final = kmeans(normalize_data, centers = k, nstart = 25)
index  = data.frame("cluster" = km_final$cluster)

library(Rtsne)

tsne_output = Rtsne(normalize_data, PCA = F, dims = 2, check_duplicates = F)

head(tsne_output$Y)

tsne_plot = data.frame(tsne_x = tsne_output$Y[, 1], tsne_y = tsne_output$Y[,2], cluster = as.factor(km_final$cluster))
 
library(ggplot2)
ggplot(tsne_plot, aes(x = tsne_x, y = tsne_y,color = cluster, shape = cluster)) + 
    geom_point() + 
    ggtitle("t-SNE") +
    theme_bw()

