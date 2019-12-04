
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rwmisc

Miscellaneous functions.

## Installation

``` r
devtools::install_github("rwright88/rwmisc")
```

## Example

An alternative to `base::summary()` for data frames:

``` r
library(rwmisc)

summary2(airquality)
```

    #>       name type   n       d_na       mean   p0    p25   p50    p75  p100
    #> 1:   Ozone  int 153 0.24183007  42.129310  1.0  18.00  31.5  63.25 168.0
    #> 2: Solar.R  int 153 0.04575163 185.931507  7.0 115.75 205.0 258.75 334.0
    #> 3:    Wind  dbl 153 0.00000000   9.957516  1.7   7.40   9.7  11.50  20.7
    #> 4:    Temp  int 153 0.00000000  77.882353 56.0  72.00  79.0  85.00  97.0
    #> 5:   Month  int 153 0.00000000   6.993464  5.0   6.00   7.0   8.00   9.0
    #> 6:     Day  int 153 0.00000000  15.803922  1.0   8.00  16.0  23.00  31.0
