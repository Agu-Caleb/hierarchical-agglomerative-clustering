#Task 1
#Implement the hierarchical agglomerative clustering with the following
#linkage: single, complete, average and centroid


clustering <- function(row_original_data, index_matrix){
  col = row_original_data-1
  clusters = matrix(rep(0, len=row_original_data*col), nrow=row_original_data, ncol=col)
  for (i in 1:col) {
    if(i != 1){
      # check previous cluster
      c1 = max(clusters[index_matrix[i,][1],])
      c2 = max(clusters[index_matrix[i,][2],])
      print(sprintf("iteration: %1.f, Merge c1: %1.f and c2: %1.f", i, c1, c2))
      if((c1 != 0) & (c2 != 0)){
        index_1 = which(clusters == c1, TRUE)
        index_2 = which(clusters == c2, TRUE)
        for (n in 1:dim(index_1)[1]) {
          clusters[index_1[n,1], i] = i
        }
        for (n in 1:dim(index_2)[1]) {
          clusters[index_2[n,1], i] = i
        }
      } else if((c1 == 0) & (c2 != 0)){
        clusters[which(clusters == c2, TRUE)[,1],i] = i
      } else if((c1 != 0) & (c2 == 0)){
        clusters[which(clusters == c1, TRUE)[,1],i] = i
      }
    }
    clusters[index_matrix[i,][1], i] = i
    clusters[index_matrix[i,][2], i] = i
  }
  clusters[, i] = i
  return(clusters)
}



linkage <- function(original, data, type){
  index_matrix = matrix(rep(0, len=sum(data!=0)), ncol=2)
  if (type == "single"){
    print("single")
    for (i in 1:sum(data!=0)/2) {
      index_matrix[i,1] = orderFun(data, "min", n=i)[1]
      index_matrix[i,2] = orderFun(data, "min", n=i)[2]
    }
    clusters = clustering(dim(original)[1], index_matrix)
  } else if(type == "complete"){
    for (i in 1:sum(data!=0)/2) {
      index_matrix[i,1] = orderFun(data, "max", n=i)[1]
      index_matrix[i,2] = orderFun(data, "max", n=i)[2]
    }
    clusters = clustering(dim(original)[1], index_matrix)
  } else if(type == "average"){
    print("average")
    for (i in 1:sum(data!=0)/2) {
      index_matrix[i,1] = orderFun(data, "mean", n=i)[1]
      index_matrix[i,2] = orderFun(data, "mean", n=i)[2]
    }
    clusters = clustering(dim(original)[1], index_matrix)
  } 
  return(clusters)
}

#THe ordering function needed by the plot to avoid collusion, inspired by https://github.com/bwlewis/hclust_in_R/
orderFun <- function(x, arg, n){
  dummy = as.vector(x)
  len = length(dummy)
  if(n > len){N = length(dummy)}
  result = sort(dummy)
  if(arg == "min"){
    return(which(x == result[result>0][n], TRUE))
  } else if(arg == "max"){
    # take a sequence of 0 into account by adding sum(result==0) to the index
    return(which(x == result[result>0][len-n+1-sum(result==0)], TRUE))
    
  } else if(arg == "mean"){
    # debug purpose
    return(which(x == result[result>0][len-n+1-mean(result==0)], TRUE))
    sprintf("%1.f th Min: %f, %1.f th Max: %f", n, result[n], len-n+1, result[len-n+1])
  }
}
#One of the problems with hierarchical clustering is that, it does not tell us how many clusters there are, 
#or where to cut the dendrogram to form clusters, 
cut_tree <- function(mat, k_clusters){
  len = dim(mat)[1]
  for (i in k_clusters:len-1) {
    result_mat = apply(mat[,1:i], 1, which.max)
    temp = unique(result_mat)
    if(length(temp)[1] >= k_clusters){
      return(result_mat)
    }
  }
}




calc_distance <- function(data){
  data_length = dim(data)[1]
  result = matrix(ncol=data_length, nrow=data_length)
  for (i in 1:data_length) {
    for (n in 1:data_length) {
      if(i < n){
        result[i,n] = 0
      } else{	
        result[i,n] = sqrt(sum(data[i,] - data[n,])**2)
      }
    }
  }
  return(result)
}

plot_selected_cluster <- function(result,X){
  plot(X, col=(result))
  #dev.off()
}
#i noticed the data files were not getting imported despite being in the same folder as the R script so i had to use
#setwd command to point to the directory
setwd("C:/Users/oagu1/Desktop/MSC/Data Analysis/Assignment 3/")

nci = read.table('nci.data.txt')
#print(nci)
labels = read.table('label.txt')
#Preprocessing the data set
#transposing the data set
nci = t(nci)
nci=nci[rowSums(nci==0) !=ncol(nci),]
# normalizing the data
nci = scale(nci)
typeof(nci)
#Getting the unique labels
labels = dim(unique(labels))[1]
res.dist <- dist(as.matrix(dist(nci)), method = "euclidean")
print(res.dist)
chosen_cluster = (linkage(nci, calc_distance(nci), "complete"))
chooseK= (cut_tree(chosen_cluster,elem))
x <- as.data.frame(t(x))
#grp <- cutree(x, k = 1)
plot_selected_cluster(chooseK, nci)

#print(argument)

#plot(argument)
#km.out <- kmeans(nci, 14)
##km.out
#plot(X, col=(km.out$cluster+1), main="K-Means Clustering Results with k=14", xlab="", ylab="", pch=20, cex=2)




