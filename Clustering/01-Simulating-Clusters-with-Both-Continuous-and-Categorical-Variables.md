Simulating Clusters with Both Continuous and Categorical Variables
================
2022-08-11

## Continuous Variables

Convex clusters of continuous variables can be generated simply as
multivariate Gaussians.

``` r
# load packages

library(mvtnorm) # for function rmvnorm
library(clusterGeneration) # for function rcorrmatrix
```

    ## Loading required package: MASS

Generate two clusters. To generate a Gaussian cluster, we need to
specify the cluster mean vector and the cluster covariance matrix.

``` r
cluster_points <- c(200, 200) # 200 points per cluster
num_vars <- 2 # how many continuous variables?
num_min_max <- 3 # determines distance between cluster centers; clusters closer if lower

# generate numerical variables
set.seed(100)
cl1 <- rmvnorm(cluster_points[1], 
               mean = runif(num_vars, min = -num_min_max, max = num_min_max), 
                            rcorrmatrix(num_vars)) 
cl2 <- rmvnorm(cluster_points[2], 
               mean = runif(num_vars, min = -num_min_max, max = num_min_max), 
                            rcorrmatrix(num_vars))

z_con <- as.data.frame(rbind(cl1, cl2))
names(z_con) <- paste0('num', 1:num_vars) 
```

Let’s look at the clusters

``` r
pairs(z_con, col = c(rep(2, cluster_points[1]), rep(4, cluster_points[2])))
```

<img src="01-Simulating-Clusters-with-Both-Continuous-and-Categorical-Variables_files/figure-gfm/unnamed-chunk-3-1.png" width="672" />

``` r
R.Version()
```

    ## $platform
    ## [1] "x86_64-apple-darwin13.4.0"
    ## 
    ## $arch
    ## [1] "x86_64"
    ## 
    ## $os
    ## [1] "darwin13.4.0"
    ## 
    ## $system
    ## [1] "x86_64, darwin13.4.0"
    ## 
    ## $status
    ## [1] ""
    ## 
    ## $major
    ## [1] "4"
    ## 
    ## $minor
    ## [1] "1.3"
    ## 
    ## $year
    ## [1] "2022"
    ## 
    ## $month
    ## [1] "03"
    ## 
    ## $day
    ## [1] "10"
    ## 
    ## $`svn rev`
    ## [1] "81868"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 4.1.3 (2022-03-10)"
    ## 
    ## $nickname
    ## [1] "One Push-Up"

``` r
R.version
```

    ##                _                           
    ## platform       x86_64-apple-darwin13.4.0   
    ## arch           x86_64                      
    ## os             darwin13.4.0                
    ## system         x86_64, darwin13.4.0        
    ## status                                     
    ## major          4                           
    ## minor          1.3                         
    ## year           2022                        
    ## month          03                          
    ## day            10                          
    ## svn rev        81868                       
    ## language       R                           
    ## version.string R version 4.1.3 (2022-03-10)
    ## nickname       One Push-Up

``` r
version
```

    ##                _                           
    ## platform       x86_64-apple-darwin13.4.0   
    ## arch           x86_64                      
    ## os             darwin13.4.0                
    ## system         x86_64, darwin13.4.0        
    ## status                                     
    ## major          4                           
    ## minor          1.3                         
    ## year           2022                        
    ## month          03                          
    ## day            10                          
    ## svn rev        81868                       
    ## language       R                           
    ## version.string R version 4.1.3 (2022-03-10)
    ## nickname       One Push-Up

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

``` r
summary(cars)
```

    ##      speed           dist       
    ##  Min.   : 4.0   Min.   :  2.00  
    ##  1st Qu.:12.0   1st Qu.: 26.00  
    ##  Median :15.0   Median : 36.00  
    ##  Mean   :15.4   Mean   : 42.98  
    ##  3rd Qu.:19.0   3rd Qu.: 56.00  
    ##  Max.   :25.0   Max.   :120.00

## Including Plots

You can also embed plots, for example:

<img src="01-Simulating-Clusters-with-Both-Continuous-and-Categorical-Variables_files/figure-gfm/pressure-1.png" width="672" />

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
