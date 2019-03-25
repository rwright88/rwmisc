
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

    #>      name type   n    d_na    mean   p0   p25   p50    p75  p100
    #> 1   Ozone  int 153 0.24183  42.129  1.0  18.0  31.5  63.25 168.0
    #> 2 Solar.R  int 153 0.04575 185.932  7.0 115.8 205.0 258.75 334.0
    #> 3    Wind  dbl 153 0.00000   9.958  1.7   7.4   9.7  11.50  20.7
    #> 4    Temp  int 153 0.00000  77.882 56.0  72.0  79.0  85.00  97.0
    #> 5   Month  int 153 0.00000   6.993  5.0   6.0   7.0   8.00   9.0
    #> 6     Day  int 153 0.00000  15.804  1.0   8.0  16.0  23.00  31.0
