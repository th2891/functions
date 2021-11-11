Visualization
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = 0.6, 
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "virids"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Z cores (subtract mean and divide by sd)

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.80269858 -0.08063824  0.24140589 -0.98907124  0.79809409 -0.31978867
    ##  [7]  0.92828000 -1.50656877  0.59498031 -2.10750508 -0.35823477 -0.37187769
    ## [13] -0.39986816 -1.16520256  0.45089906  2.09484875  0.09621978 -0.46774032
    ## [19]  1.38750609 -0.01650614 -0.83446743  1.15955395  0.17185267 -0.23325271
    ## [25]  1.72977978

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
 
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -0.80269858 -0.08063824  0.24140589 -0.98907124  0.79809409 -0.31978867
    ##  [7]  0.92828000 -1.50656877  0.59498031 -2.10750508 -0.35823477 -0.37187769
    ## [13] -0.39986816 -1.16520256  0.45089906  2.09484875  0.09621978 -0.46774032
    ## [19]  1.38750609 -0.01650614 -0.83446743  1.15955395  0.17185267 -0.23325271
    ## [25]  1.72977978

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  1.406782398 -1.301702653  1.966675143 -0.229259176 -0.027406541
    ##  [6]  0.359486689  0.637493002  0.307536379  0.864674234 -1.221708946
    ## [11] -0.246718193  1.522121816 -0.001382501  0.224135781 -1.299274876
    ## [16]  1.417902622  0.663337451 -0.706882585 -0.264163705 -1.557094709
    ## [21] -0.194481361  0.423868732 -1.226099634  1.747097282  0.534572468
    ## [26] -1.724786885 -0.595105288  0.098239455 -1.426250856 -0.415936575
    ## [31] -0.904721117 -0.290435618  0.266743094 -0.091162315  1.394407258
    ## [36]  1.573150098 -1.472064284  0.377724052  0.197689967 -0.787000104

functions working…not working

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores(c("my", "name", "is", "jeff"))
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in x - mean(x): non-numeric argument to binary operator

``` r
z_scores(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

-   trying agian with functions
    -   fixing outputs from above ^

``` r
 z_scores = function(x) {
   
   if (!is.numeric(x)) {
     stop("x needs to be numeric")
   }
   
   if (length(x) < 3) {
     stop("x should have at least 3 numbers")
   }
  
  z = (x - mean(x)) / sd(x)
 
  return(z)
}
```

## Multiple outputs

``` r
mean_and_sd = function(x) {
   
   if (!is.numeric(x)) {
     stop("x needs to be numeric")
   }
   
   if (length(x) < 3) {
     stop("x should have at least 3 numbers")
   }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x, 
      sd = sd_x
    )
 
  return(output_df)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.96  3.58

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.323
