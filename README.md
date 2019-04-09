
<!-- README.md is generated from README.Rmd. Please edit that file -->
rwmisc
======

Miscellaneous functions.

Installation
------------

``` r
devtools::install_github("rwright88/rwmisc")
```

Examples
--------

An alternative to `base::summary()` for data frames:

``` r
library(rwmisc)

summary2(airquality)
```

    #> # A tibble: 6 x 10
    #>   name    type      n   d_na   mean    p0   p25   p50   p75  p100
    #>   <chr>   <chr> <int>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    #> 1 Ozone   int     153 0.242   42.1    1    18    31.5  63.2 168  
    #> 2 Solar.R int     153 0.0458 186.     7   116.  205   259.  334  
    #> 3 Wind    dbl     153 0        9.96   1.7   7.4   9.7  11.5  20.7
    #> 4 Temp    int     153 0       77.9   56    72    79    85    97  
    #> 5 Month   int     153 0        6.99   5     6     7     8     9  
    #> 6 Day     int     153 0       15.8    1     8    16    23    31
