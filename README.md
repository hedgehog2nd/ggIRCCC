# ggIRT

<!-- badges: start -->
<!-- badges: end -->


## Overview
The `ggIRT` R package provides functions for visualizing the results of analyses using the `mirt` or `ltm` packages. The following can be done with this package  

- Visualization of item characteristic curves (`gg_irccc`)  
- Visualization of item response category characteristic curves (`gg_irccc`)  
- Visualization of test information curves (`gg_tic`). Currently, only the `mirt` package is supported.  


The following functions are not implemented, but these are planned to be added in the future  

- Visualization of ICC for each group  
- Visualization of IRCCC for each group  
- Visualization of TIC based on analysis results obtained from the ltm package  
- Visualization of test characteristic curves  


## Installation

You can install the development version of ggIRCCC from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hedgehog2nd/ggIRT")
```

## Usage

After installation, you can load this package by using the `library` function. You can draw an IRCCC by passing the analysis results `res` and item numbers of the `mirt` package as arguments to the `gg_irccc` function. If you set the `grm` argument to FALSE, you can draw the ICC for a binary IRT. The ggplot2 is used for visualization. It is also possible to change axis labels and apply original themes.  
You can draw a TIC by passing the analysis results of the `mirt` package as an argument to the `gg_tic` function.

``` r
library(ggIRT)
library(mirt)

res <- mirt(data, itemtype = "graded")

gg_irccc(res, item = 1)
gg <- gg_irccc(res, item = 1)
gg + theme_bw() + labs(x = "THETA", y = "PROB.", title = "IRCCC item 1")

gg_tic(res)
```

## Depends  
This package requires the following;  

- R (>= 4.1.0)  
- ggplot2  
- mirt  
- ltm  
- stats  

