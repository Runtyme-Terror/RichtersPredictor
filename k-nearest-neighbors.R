library(Rfast)

#returns the most frequent number or string in a vector
getMode <- function(v) {
  uniqv <- unique(v)
  frequcy.per.label = tabulate(match(v, uniqv))
  if ( (length(unique(frequcy.per.label)) == 1) & (length(frequcy.per.label) > 1) )
    mode = getMode(head(v,length(v)-1))
  else
    mode = uniqv[which.max(frequcy.per.label)]
  return (mode)
}

euclidean_distance_for_knn = function(a_matrix,b_matrix){
  #print(a_matrix)
  return(sqrt(rowsums((a_matrix - b_matrix)^2)))
}

#finds the distance between current.pt and reest of the points 
#and returns the data frame with the distances as a column
find_distances = function (train.data, train, current.pt){ #current.pt can be a centroid->for Outlier detection
  train = data.matrix(train.data[,1:ncol(train)])
  test = data.matrix(current.pt)
  test = rep_row(test,nrow(train.data))
  train = matrix(train, nrow = nrow(train.data),ncol = ncol(train))
  test = matrix(test, nrow = nrow(test),ncol = ncol(test))
  
  train.data$distance  = euclidean_distance_for_knn(train,test) #this is the imporant part
  
  return(train.data)
}

#returns the top k in the data frame. 
get_k_closest_pts = function(train.data.with.distances,k){
  train.data.with.distances = train.data.with.distances[order(train.data.with.distances$distance),] #sorts by smallest distance
  return(head(train.data.with.distances,k)) #picks the top k
}

#k nearest-neighbors function from scratch
#returns a parrallel vector wear each number represt its predicted value
#this function is lightly less accurate because of the way it account for ties.
#when in a tie, it picks the top k-1 values and returns the predicted value based on k-1
k.nn = function(train,test,labels,k = 1){
  test.data = test
  
  if(typeof(labels) == "character")
    test.data$labels = character(nrow(test))
  else
    test.data$labels = NA
  
  train.data = train
  train.data$labels = labels
  
    for(pt in 1:nrow(test)){
      train.data.with.distances = find_distances(train.data,train,test[pt,])
      train.data.with.distances = train.data.with.distances[order(train.data.with.distances$distance),] #sorts by smallest distance
      top.k.closest.pts = head(train.data.with.distances,k) #picks the top k
      test.data$labels[pt] = getMode(top.k.closest.pts$label)
    }

  return(test.data$labels)
}