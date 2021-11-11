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

    ##  [1]  0.74039731 -2.29436665 -1.79486715 -0.29218568  2.12639707  0.65476237
    ##  [7] -0.24920392  1.15166623  0.23928616  0.26493570  1.51984518 -0.24361048
    ## [13]  0.71714553 -0.19815928 -1.16919899  0.17053693 -0.80135335  0.26757227
    ## [19] -0.16838087  0.38879216 -0.49164447  0.33307778  0.65312020  0.03176344
    ## [25] -1.55632749

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
 
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  0.74039731 -2.29436665 -1.79486715 -0.29218568  2.12639707  0.65476237
    ##  [7] -0.24920392  1.15166623  0.23928616  0.26493570  1.51984518 -0.24361048
    ## [13]  0.71714553 -0.19815928 -1.16919899  0.17053693 -0.80135335  0.26757227
    ## [19] -0.16838087  0.38879216 -0.49164447  0.33307778  0.65312020  0.03176344
    ## [25] -1.55632749

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -0.16131239  0.64651438 -0.73815144 -1.53116209 -0.64879175  1.63559745
    ##  [7] -0.32874910 -1.35641807  1.41142493 -0.45204048  0.37040320 -0.18360205
    ## [13] -1.27095030  1.03695711 -0.38882934  0.49443934 -0.66611237  1.22874146
    ## [19]  0.97286714 -0.72132735  1.07610444 -0.15521845  0.09164628  0.39526359
    ## [25] -0.08960564  2.10583040  1.61141857 -0.51338006 -0.93214222  0.41580591
    ## [31] -2.57984357  0.21747740  0.70191834 -0.01186167 -0.72237450 -0.39086932
    ## [37] -0.82379532  0.35769138  1.24728364 -1.35084747

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
    ## 1  4.19  3.73

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.277

## Different sample sizes, means, and sds

``` r
sim_data = 
  tibble( 
    x = rnorm(30, mean = 2, sd = 3)
    )

sim_data %>%
  summarize(
    mean = mean(x), 
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.37  3.08

function that simulates data, computes mean and sd

``` r
sim_mean_sd = function(n, mu, sigma) {
 
  #do checks on inputs
  
  sim_data = 
    tibble( 
    x = rnorm(n, mean = mu, sd = sigma)
    )

sim_data %>%
  summarize(
    mean = mean(x), 
    sd = sd(x)
  )
  
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.23  3.63
