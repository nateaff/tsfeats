
<!-- README.md is generated from README.Rmd. Please edit that file -->
tsfeats
=======

Provides wrappers to some time series features. Many features are wrappers for functions from other packages and some parameters are fixed within the feature function.

Example
=======

Compute a single feature. The initial ouput is just the original output of the function. The `clean_feature` function returns a dataframe with the feature.

``` r
library(tsfeats) 
res <- fd_variogram(rnorm(1000))
clean_feature(res)
#>   fd_variogram
#> 1     1.936435
```

In the next example we generate two groups of functions from an ARMA model with varying parameters. The `get_feature` function computes and cleans features and returns a single dataframe.

``` r
# Generate 10 samples from 2 ARMA models
t1 <- replicate(10, arima.sim(n = 500, list(ar = c(0.78, -0.5), ma = c(-5, 0,5))))
t2 <- replicate(10, arima.sim(n = 500, list(ar = c(0.91, -0.2), ma = c(-5, 0,5))))

# The features to compute
features <- c(bandpower, spectral_entropy, 
              permutation_entropy, fd_variogram, hurst)

# Compute features on each group
df1 <- get_features(t1, features, id = "1")
df2 <- get_features(t2, features, id = "2")

# Bandpower defaults to typical EEG frequency bands  
df1[1, 1:5]
#>        delta    theta     alpha      beta      gamma
#> V1 -8.088636 -6.70501 -8.714072 -3.887339 -0.8767714
```

A plot comparing the distribution of each feature for the two groups.

``` r
library(ggplot2)
library(reshape2)
# Plot feature densities for each group
data_long <- reshape2::melt(rbind(df1, df2), id.vars = c("id"))

gg <- ggplot(data_long, aes(x=value, fill=id)) +
             geom_density(alpha = 0.55, 
                      aes(y = ..density..), 
                      color = NA, 
                      position = "identity")
gg + facet_wrap(~variable, ncol = 3, scales = "free") + 
        labs(x = "", y = "") + 
        scale_fill_manual(values = c("gray30","darkolivegreen"))
```

![](figures/README-unnamed-chunk-4-1.png)

Installation
============

``` r
install.packages("devtools") 
devtools::install_github("nwaff/tsfeats")
```
