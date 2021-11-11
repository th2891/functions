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

    ##  [1]  1.83988602  0.91290760  0.06791094 -0.11959362  0.30875994 -1.16251677
    ##  [7] -1.20132767  0.44769803 -1.36310870  0.55164726  1.29239098 -0.76083989
    ## [13]  1.99764369 -1.15214644  0.45989835 -1.08647225  0.45099787 -0.10059010
    ## [19] -0.87720111  0.88004311 -0.75176113 -0.38949111 -0.32781741  1.25919966
    ## [25] -1.17611724

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
 
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1]  1.83988602  0.91290760  0.06791094 -0.11959362  0.30875994 -1.16251677
    ##  [7] -1.20132767  0.44769803 -1.36310870  0.55164726  1.29239098 -0.76083989
    ## [13]  1.99764369 -1.15214644  0.45989835 -1.08647225  0.45099787 -0.10059010
    ## [19] -0.87720111  0.88004311 -0.75176113 -0.38949111 -0.32781741  1.25919966
    ## [25] -1.17611724

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -0.73741339 -0.17039982  0.27795672 -1.56964271  0.54792180  3.27334205
    ##  [7] -0.59742803  1.94929660  0.16727416  1.08001663  0.18392971  0.34524277
    ## [13]  0.03695878 -0.18893218  1.37835990 -0.23811528 -0.19395027 -0.25641197
    ## [19] -0.84478527 -0.39949872 -0.73944188 -1.61366647 -0.55063447 -0.17818697
    ## [25] -1.52098955  1.08528030  0.23221340 -0.25042629  0.13891357 -1.23504151
    ## [31] -0.11024663 -0.43101153 -1.03271544 -1.05149703  1.18725470 -0.04390846
    ## [37]  0.89705104 -0.72317290  1.60056276  0.29594186

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
