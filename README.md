# ggIRT  
[![pkgdown site](https://img.shields.io/badge/docs-pkgdown-blue)](https://hedgehog2nd.github.io/ggIRT/)

<!-- badges: start -->

<!-- badges: end -->

## Overview

The `ggIRT` R package provides functions for visualizing the results of analyses using the `mirt` or `ltm` packages. The following can be done with this package:  

- Visualization of item characteristic curves (`gg_irccc`)  
- Visualization of item response category characteristic curves (`gg_irccc`)  
- Visualization of test information curves (`gg_tic`)  
- Visualization of item information curves (`gg_iic`)  

The following functions are not implemented, but these are planned to be added in the future

-   Visualization of ICC for multiple group  
-   Visualization of IRCCC for multiple group  
-   Visualization of test characteristic curves  

## Installation

You can install the development version of `ggIRT` from [GitHub](https://github.com/hedgehog2nd/ggIRT) with:  

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

## Functions  

- `gg_irccc`: Visualize Item Response Category Characteristic Curves (IRCCC) from mirt or ltm object.[`detail`](https://hedgehog2nd.github.io/ggIRT/reference/gg_irccc.html)
- `gg_iic`: Visualize Item Information Curves (IIC) from mirt or ltm object.[`detail`](https://hedgehog2nd.github.io/ggIRT/reference/gg_iic.html)
- `gg_tic`: Visualize Test Information Curves (TIC) from mirt or ltm object.[`detail`](https://hedgehog2nd.github.io/ggIRT/reference/gg_tic.html)

## Depends

This package requires the following:  

- `R` (>= 4.1.0)  
- `ggplot2`  
- `mirt`  
- `ltm`  

## Version History

version 1.0.0  
Updated on March 26, 2025 (JST) 

- Create a README file.  
- Create a help file.  
- Item information curve output is now supported (`gg_iic`).  
- `gg_tic` function now supports `ltm` and `grm` objects.
- `gg_tic` function now supports binary IRT.
- To avoid a conflict with the `ltm` function, the `ltm` argument has been changed to `use_ltm`.  
- fix `gg_irccc` function.

version 0.1.0  
March 24, 2025 (JST) 

- released  
