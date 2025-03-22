# ggIRCCC

<!-- badges: start -->
<!-- badges: end -->

The goal of ggIRCCC is to ...

## Installation

You can install the development version of ggIRCCC from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("hedgehog2nd/ggIRCCC")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mirt)
res <- mirt(data, itemtype = "graded")
library(ggIRCCC)
gg_irccc(res, item = 1)
```

