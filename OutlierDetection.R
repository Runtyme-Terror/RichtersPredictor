source("/home/trevi042/Desktop/data mining/project/k-nearest-neighbors.R")
source("/home/trevi042/Desktop/data mining/project/k-means-clustering.R")

#function to allow usage recompute_centroids from kmeans file
create_predicted_cluster_info = function(labels){
  labels = as.vector(labels)
  num.pts.per.cluster = integer(length(unique(labels)))
  
  for(i in unique(labels)){
    num.pts.per.cluster[i] = sum(labels == i)
  }
  
  return(list(labels,num.pts.per.cluster))
}

#function that removes top.k.fartest.pts
remove_points = function(train.data,top.k.fartest.pts,k){
  num.cols = ncol(train.data)
  num.rows = nrow(train.data)
  train.data.matrix = data.matrix(train.data)
  top.k.fartest.pts = data.matrix(top.k.fartest.pts)
  for(x in 1:k)
    for(i in 1:num.rows)
      #if (i%%10000 == 1){print(i/10000)}
      if(sum(train.data.matrix[i,] == top.k.fartest.pts[x,1:num.cols]) == num.cols){
        train.data = train.data[-c(i),]
        break
      }
  return(train.data)
}

#outlier function from scratch
#returns data with removed outliers
Outlier_Removal = function(train,labels,percent,similarityMetric = euclidean_distance){
  predict.cluster.info = create_predicted_cluster_info(labels)
  num.of.clusters = length(unique(labels))
  centroids = recompute.centroids(train,predict.cluster.info,num.of.clusters)

  train.data = train
  train.data$labels = labels
  k = ceiling(nrow(train)*percent/num.of.clusters)
  
  for(i in 1:num.of.clusters){
    train.data.with.distances = find_distances(train.data,train,centroids[i,])
    current.cluster.data = filter(train.data.with.distances,labels == i)
    current.cluster.data = current.cluster.data[order(-current.cluster.data$distance),] #sorts by largest distance
    print(current.cluster.data$distance)
    top.k.fartest.pts = head(current.cluster.data,k) #picks the top k
    train.data = remove_points(train.data,top.k.fartest.pts,k)
  }
  return (list(train.data[,1:ncol(train)],train.data$labels))
}

#Outlier function works as following:
#is takes in a certain percentage of the points farthestaway from the centroid and removes them. 




