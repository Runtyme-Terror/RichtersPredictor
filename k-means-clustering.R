library(ggplot2)

#finds randoms centroid
rand_centroids = function(df,k){
  ran_numbers = sample(1:nrow(df), k, replace = FALSE)
  centroids = c()
  for (i in 1:k)
    centroids = rbind(centroids,df[ran_numbers[i],])
  return(centroids)
}

#calculates euclidean distance
euclidean_distance = function(v1,v2){
  return (sqrt(sum((v1-v2)^2)))
}

#finds and returns the closest centroid
assign_closest_cluster = function (i,df,centroids,similarityMetric){
  smallest.distance = 9999999
  num.centroids = nrow(centroids) 
  
  for (y in 1:num.centroids) { #current cluster
    distance = similarityMetric(as.numeric(df[i,]),as.numeric(centroids[y,])) #euclidean_distance between point in d.f and centroid
    if (distance < smallest.distance){
      smallest.distance = distance
      cluster = y
    } #end if
  } #end num.centroids loop
  return(cluster)
}

#returns a list of two cointaining the cluster vector and the number of points per cluster
find_cluster = function (df,centroids,similarityMetric){
  predicted.cluster = integer(nrow(df)) #parrell array that assigns each point to the its correct cluster
  num.pts.per.cluster = integer(nrow(centroids))
  
  for (x in 1:nrow(df)) { 
    predicted.cluster[x] = assign_closest_cluster(x,df,centroids,similarityMetric)
    num.pts.per.cluster[predicted.cluster[x]] = num.pts.per.cluster[predicted.cluster[x]] + 1
  } 
  for(i in 1:nrow(centroids)){
    num.pts.per.cluster[i] = sum(predicted.cluster == i)
  }
  
  return(list(predicted.cluster,num.pts.per.cluster))
}

recompute_centroids = function(df,predicted.cluster.info,k){
  clusters = vector(mode = "list", length = k)
  pt.size = ncol(df)
  cluster.counter = rep(1,k)
  centroids = c()
  
  for(i in 1:k){ #initializing points per clusters
    a = matrix(ncol = pt.size, nrow = predicted.cluster.info[[2]][i])
    clusters[[i]] = a
  }
  for (x in 1:nrow(df)) { #current row in the df
    for (y in 1:k) {
      if(predicted.cluster.info[[1]][x] == y){
        clusters[[y]][cluster.counter[y],] = as.numeric(df[x,])
        cluster.counter[y] = cluster.counter[y] +1
        break
      } #ends if stmt
    } #ends 1:k cluster loop
  } #ends 1:nrow(df) data frame loop 
  
  for (i in 1:k){
    centroids = rbind(centroids, colMeans(clusters[[i]]))
  }
  return(centroids)
}

#kmeans function from scratch
#returns a cluster vector
km = function(df,k, max.iter = 10, centroids = rand_centroids(df,k), 
                                   similarityMetric = euclidean_distance){
  for (i in 1:max.iter){
    #finds the clusters based centroid picked by the user
    #or by the rand_centroids function.
    predicted.cluster.info = find_cluster(df, centroids,similarityMetric) 
    if ((i > 1) & ((all.equal(prev.num.pts.per.cluster, 
                              predicted.cluster.info[2]) == 1) 
                   | i == max.iter))
      break #stops if converged or reached max iterations
    
    prev.num.pts.per.cluster = predicted.cluster.info[2] #needed to see if convergences
    
    centroids = recompute_centroids(df, predicted.cluster.info, k)
  }
  return(predicted.cluster.info)
}

plotCluster = function(dat, x.var, y.var,cluster_vector){
  cluster_vector = as.factor(cluster_vector)
  x.var = enquo(x.var)
  y.var = enquo(y.var)
  ggp = ggplot(data = dat, aes(x = !! x.var, y = !! y.var, color = cluster_vector )) + geom_point()
  
  return(ggp)
}
