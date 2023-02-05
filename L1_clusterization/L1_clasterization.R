real_estate_data <- read.csv("real_estate_ufa.csv", sep=";")
real_estate_scaled_data = scale(real_estate_data)

cluster_stats <- factoextra::fviz_nbclust(real_estate_scaled_data, kmeans, method = "silhouette")
n_clust<-cluster_stats$data
max_cluster<-as.numeric(n_clust$clusters[which.max(n_clust$y)])

clusters <- kmeans(real_estate_scaled_data, 4, iter.max = 10, nstart = 10)
plot(real_estate_data, col=clusters$cluster)

unscaled_centers <- t(apply(clusters$centers, 1, function(r)r*attr(real_estate_scaled_data,'scaled:scale') + attr(real_estate_scaled_data, 'scaled:center')))
print(unscaled_centers)