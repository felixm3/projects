speClusMixed <- function(z, centers = 2, NN = 10, MM = 0, cat_wgts = c(0.9999, 0.999, seq(0.99, 0.01, -0.01), 0.001, 0.0001), starts = 10, iterations = 300, verbose = FALSE) {
  # v220920
  # uses specc algo to pick sigma 1/max(as.matrix(dist(Filter(is.numeric, z$data))))
  # calculate Gaussian kernel similarity for numeric variables
  # simple matching similarity for categorical variables
  # add the two using some weight
  # perform spectral clustering on this similarity matrix
  # take in a dataframe x where all categorical columns are coded as 'factor'
  
  library(cluster) # for daisy -> used for SMC for cat vars
  library(MixGHD) # for ARI
  library(RSpectra) # for eigs_sym(A, k)
  
  
  x <- z$data # data in z
  labels <- z$labels # labels in z
  
  x_num <- Filter(is.numeric, x) # numeric columns of x
  x_cat <- Filter(is.factor, x) # categorical columns of x
  
  # simple matching coefficient for categorical variables
  ifelse(length(x_cat) > 0, x_cat_dist <- as.matrix(daisy(x_cat)), x_cat_dist <- cat_wgts <- 0)				
  #head(x_cat)
  #head(x_cat_dist)
  # all entries of x_cat_dist are between 0 and 1
  
  ifelse(length(x_num) > 0, num_wgts <- 1 - cat_wgts, x_num_dist <- num_wgts <- 0)
  
  # Euclidean distance for numerical
  if(length(x_num) > 0) x_num_dist <- as.matrix(dist(x_num))
  
  x_num_dist <- x_num_dist/max(x_num_dist) # now x_num_dist entries are between 0 and 1
  #x_num_dist_sorted <- t(apply(x_num_dist, 1, sort)) # sort each row of x_num_dist in increasing order
  #x_nn_0 <- x_num_dist_sorted[, -1] # resulting column 1 is distance from each point to 1st-NN, col 2 is dist to 2nd-NN, etc
  #x_nn_0_col_med <- apply(x_nn_0, 2, median)
  #tmpsig <- x_nn_0_col_med[NN:(9 + NN)]
  
  
  # create matrix to store output
  ARI_mat_big <- matrix(ncol = 5, dimnames = list(NULL, c('cat_wgt', 'sigma', 'bt/wt_ss', 'tot_wt_ss', 'ARI')))
  #ptm <- proc.time()
  for (i in 1:length(cat_wgts)) { # (i in 1:length(cat_wgts))
    if (verbose) print(i)
    ktmp <- num_wgts[i] * x_num_dist + cat_wgts[i] * x_cat_dist
    ktmp_sorted <- t(apply(ktmp, 1, sort))
    x_nn_0 <- ktmp_sorted[, -1] # resulting column 1 is distance from each point to 1st-NN, col 2 is dist to 2nd-NN, etc
    x_nn_0_col_med <- apply(x_nn_0, 2, median)
    
    # ignore points at distance 0
    tmpsigA <- x_nn_0_col_med[x_nn_0_col_med > 0]
    tmpsig <- tmpsigA[NN:(MM + NN)]
    
    # matrix to store results for each sigma
    ARI_mat_small <- matrix(nrow = length(tmpsig), ncol = 5, dimnames = list(NULL, c('cat_wgt', 'sigma', 'bt/wt_ss', 'tot_wt_ss', 'ARI')))
    for (j in 1:length(tmpsig)) { # (j in 1:length(tmpsig))
      if (verbose) print(paste(i, '-', j))
      ka <- exp((-(ktmp^2))/(2 * (tmpsig[j]^2))) # matrix of pairwise similarities using Gaussian kernel
      diag(ka) <- 0 # set diagonal to 0 thus creating NJW affinity matrix A
      d <- 1/sqrt(rowSums(ka)) # NJW D^(-1/2) matrix
      #print(d)
      if (!any(d == Inf) && !any(is.na(d)) && (max(d)[1] - min(d)[1] < 10^4)) { # check d: no Inf, NA and max not >> min
        #print('in spectral loop')
        l <- d * ka %*% diag(d) # NJW graph Laplacian L
        xi <- eigs_sym(l, centers, opts = list(maxitr = 4000))$vectors # first k eigenvectors
        yi <- xi/sqrt(rowSums(xi^2))
        res <- kmeans(yi, centers, iterations, nstart = starts) # do kmeans on normalized eigenvectors
        #diss[j] <- res$tot.withinss # tot.withinss for each sigma
        
        ARI_mat_small[j, 'cat_wgt'] <- cat_wgts[i]
        ARI_mat_small[j, 'sigma'] <- tmpsig[j]
        ARI_mat_small[j, 'bt/wt_ss'] <- res$betweenss/res$tot.withinss
        ARI_mat_small[j, 'tot_wt_ss'] <- res$tot.withinss
        ARI_mat_small[j, 'ARI'] <- ARI(res$cluster, z$labels)
        
      } # end if (!any(d == Inf) && !any(is.na(d)) && (max(d)[1] - min(d)[1] < 10^4))
      
    } # for (j in 1:length(tmpsig)) 
    #proc.time() - ptm
    ARI_mat_big <- rbind(ARI_mat_big, ARI_mat_small)
  } # end for (i in 1:length(cat_wgts)) 
  ARI_mat_big <- ARI_mat_big[-1, ]
  #proc.time() - ptm
  #ARI_mat_big
  #which.max(ARI_mat_big[, 'bt/wt_ss'])
  #ARI_mat_big[which.max(ARI_mat_big[, 'bt/wt_ss']), ]
  #which.max(ARI_mat_big[, 'ARI'])
  #ARI_mat_big[which.max(ARI_mat_big[, 'ARI']), ]
  #plot(ARI_mat_big[, c('sigma', 'ARI')], type = 'b')
  #?plot
  
  return(list(ARI_mat_big = ARI_mat_big, 
              x_nn_0_col_med = x_nn_0_col_med, 
              ARI = ARI_mat_big[which.max(ARI_mat_big[, 'bt/wt_ss']), ], 
              ARI_best_possible = ARI_mat_big[which.max(ARI_mat_big[, 'ARI']), ]))
} # end function speclus_v_200708

