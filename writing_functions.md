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
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

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

    ##  [1] -0.49516586 -1.08139220  0.76660130 -0.34251467 -1.02322419  0.04791122
    ##  [7]  0.47823145  0.32610307 -0.40636330  1.44241043  0.16280363  1.04541825
    ## [13]  0.88328926 -0.07885822 -1.02434580 -0.20974318 -1.56139778  0.61601932
    ## [19] -1.42032628  1.24211539  1.60751541 -0.86624026 -0.26437480  1.72961303
    ## [25] -1.57408520

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
 
  return(z)
}

z_scores(x = x_vec)
```

    ##  [1] -0.49516586 -1.08139220  0.76660130 -0.34251467 -1.02322419  0.04791122
    ##  [7]  0.47823145  0.32610307 -0.40636330  1.44241043  0.16280363  1.04541825
    ## [13]  0.88328926 -0.07885822 -1.02434580 -0.20974318 -1.56139778  0.61601932
    ## [19] -1.42032628  1.24211539  1.60751541 -0.86624026 -0.26437480  1.72961303
    ## [25] -1.57408520

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -2.01093839  1.08333949  0.93470266 -0.19810066  0.04359585 -0.32641164
    ##  [7] -0.05964467 -0.28254524  0.24417958  0.21805946 -1.40817666  1.26212225
    ## [13] -0.81099197 -0.24013577 -0.59106004 -0.39347766 -1.00121279 -0.47554885
    ## [19] -0.79217316  0.99996575  0.12640868  0.08334345 -2.21448471  2.26680614
    ## [25]  0.40640727 -0.52223076  0.86020997  0.03958493  0.59284795 -1.28196493
    ## [31]  1.97450666 -0.09739375  1.13022292 -0.93112256  1.83663817 -0.41037046
    ## [37] -0.53248903  0.01920697  1.15012145 -0.69179589

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
    ## 1  5.30  4.52

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.317

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
    ## 1  1.30  3.45

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
    ## 1  4.31  2.55

Revist napolean dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() 
 

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() 

reviews = 
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
)
```

lots of pages of reviews

Write a function that gets reviews based on page url

``` r
get_page_reviews = function(page_url) {
 
  page_html = read_html(page_url)

  review_titles = 
    page_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() 
   
  
  review_text = 
    page_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() 
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
  )
  
  return(reviews)
  
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
get_page_reviews(urls[1]), 
get_page_reviews(urls[2]), 
get_page_reviews(urls[3]), 
get_page_reviews(urls[4]), 
get_page_reviews(urls[5])) 
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 it was                                                5.0 ou… "\n  mad good …
    ##  2 Fun!                                                  4.0 ou… "\n  Fun and e…
    ##  3 Vintage                                               5.0 ou… "\n  Easy to o…
    ##  4 too many commercials                                  1.0 ou… "\n  5 minutes…
    ##  5 this film is so good!                                 5.0 ou… "\n  VOTE FOR …
    ##  6 Good movie                                            5.0 ou… "\n  Weird sto…
    ##  7 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  8 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  9 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ## 10 Classic Film                                          5.0 ou… "\n  Had to or…
    ## # … with 40 more rows

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
