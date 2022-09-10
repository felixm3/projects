gen_2_clusters_con <- function(cluster_points = c(200, 200), con_vars = 2, con_dist = 3, seed = 100){
  
  # cluster_points: points per cluster
  # con_vars: how many continuous variables?
  # con_dist: determines distance between cluster centers; clusters closer if smaller
  
  # load required packages
  
  library(mvtnorm) # for function rmvnorm
  library(clusterGeneration) # for function rcorrmatrix
  
  set.seed(seed)
  
  cl1 <- rmvnorm(cluster_points[1], 
                 mean = runif(con_vars, min = -con_dist, max = con_dist), 
                 sigma = rcorrmatrix(con_vars)) 
  cl2 <- rmvnorm(cluster_points[2], 
                 mean = runif(con_vars, min = -con_dist, max = con_dist), 
                 sigma = rcorrmatrix(con_vars))
  
  clust_labels <- c(rep(2, cluster_points[1]), rep(4, cluster_points[2]))
  
  z_con <- as.data.frame(rbind(cl1, cl2))
  names(z_con) <- paste0('con', 1:con_vars)
  
  return(list(data = z_con, labels = clust_labels))
  
}

gen_2_clusters_cat <- function(cluster_points = c(200, 200), cat_vars = 2, overlap = "low", seed = 100){
  # cluster_points: points per cluster
  # cat_vars: how many categorical variables?
  # overlap: ('low', 'medium', 'high') determines overlap in categorical variables; Each categorical variable can take on 5 levels drawn from a discrete uniform distribution.For low overlap, categorical variable A has five levels 1, 2, 3, 4, 5 in Cluster 1 and five levels 5, 6, 7, 8, 9 in Cluster 2. For medium overlap, categorical variable A has five levels 1, 2, 3, 4, 5 in Cluster 1 and has five levels 4, 5, 6, 7, 8 in Cluster 2. For high overlap, categorical variable A has five levels 1, 2, 3, 4, 5 in Cluster 1 and has five levels 3, 4, 5, 6, 7 in Cluster 2.
  
  set.seed(seed)
  
  if (!(overlap %in% c("low", "medium", "high"))){
    stop("overlap must be one of 'low', 'medium', 'high'")
  }
  
  M <- matrix(nrow = sum(cluster_points), ncol = cat_vars)
  
  lo <- function(){
    for (i in 1:cat_vars){
      M[, i] <<- c(ceiling(runif(cluster_points[1], min = 0, max = 5)), 
                   ceiling(runif(cluster_points[2], min = 4, max = 9)))
    }
  }
  
  med <- function(){
    for (i in 1:cat_vars){
      M[, i] <<- c(ceiling(runif(cluster_points[1], min = 0, max = 5)), 
                   ceiling(runif(cluster_points[2], min = 3, max = 8)))
    }
  }
  
  hi <- function(){
    for (i in 1:cat_vars){
      M[, i] <<- c(ceiling(runif(cluster_points[1], min = 0, max = 5)), 
                   ceiling(runif(cluster_points[2], min = 2, max = 7)))
    }
  }
  
  switch(overlap,
         low = lo(),
         medium = med(),
         high = hi(), 
         stop("overlap must be one of 'low', 'medium', 'high'"))
  
  clust_labels <- c(rep(2, cluster_points[1]), rep(4, cluster_points[2]))
  
  class(M) <- 'character'
  z_cat <- as.data.frame(M, stringsAsFactors = TRUE)
  names(z_cat) <- paste0('cat', 1:cat_vars)
  
  #z_cat <- apply(z_cat, 2, as.factor)
  
  return(list(data = z_cat, labels = clust_labels))
}

gen_2_clusters_mixed <- function(cluster_points = c(200, 200), con_vars = 2, con_dist = 3, seed = 7777, cat_vars = 2, overlap = "low"){
  # combines the functions gen_2_clusters_con and gen_2_clusters_cat
  
  z_con <- gen_2_clusters_con(cluster_points, con_vars, con_dist, seed)
  z_cat <- gen_2_clusters_cat(cluster_points, cat_vars, overlap, seed)
  
  return(list(data = cbind(z_con$data, z_cat$data), labels = z_con$labels))
}