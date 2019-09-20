# screenr
[![Build Status](https://travis-ci.org/mimikwang/screenr.svg?branch=master)](https://travis-ci.org/mimikwang/screenr) [![codecov](https://codecov.io/gh/mimikwang/screenr/branch/master/graph/badge.svg)](https://codecov.io/gh/mimikwang/screenr)

## Overview
The purpose of screenr is to provide a quick way to screen responses and factors one to one.

## Installation
```{r}
devtools::install_github("mimikwang/screenr")
```

## Getting Started
```{r}
library(screenr)

df_screen <- screen_aov(mtcars, c("mpg"), c("wt", "cyl"))

# df_screen$results
#
# Response              <chr> "mpg",        "mpg"
# Factor                <chr> "wt",         "cyl"
# N                     <int> 32,           32
# Pval                  <dbl> 1.293959e-10, 6.112687e-10
# SS.Residual           <dbl> 278.3219,     308.3342
# SS.Factor             <dbl> 847.7252,     817.7130
# Response.Mean         <dbl> 20.09062,     20.09062
# Response.Sd           <dbl> 6.026948,     6.026948
# Response.Shapiro.Pval <dbl> 0.1228814,    0.1228814
# Factor.Mean           <dbl> 3.21725,      6.18750
# Factor.Sd             <dbl> 0.9784574,    1.7859216
# Factor.Shapiro.Pval   <dbl> 9.265499e-02, 6.058338e-06
```
