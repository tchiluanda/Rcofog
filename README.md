
# Rcofog

<!-- badges: start -->
<!-- badges: end -->

The goal of Rcofog is to retrieve financial data about functions of government  

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tchiluanda/Rcofog")

```

## Example

Here you can see how to draw a sankey graphic with COFOG data:

``` r
library(Rcofog)
dataExpenseFlow(year=2020) %>% 
graphExpenseFlow()

```

