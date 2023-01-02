
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BehavioralDataAnalysis

<!-- badges: start -->
<!-- badges: end -->

The goal of BehavioralDataAnalysis is to provide functions to help you
analyze behavioral data, i.e., data that represents the behavior of
human beings such as customers and employees. In particular, I believe
that there are two aspects of behavioral data that are worth
emphasizing: - it doesn’t obey a normal distribution nearly as often as
we assume. It can be asymmetrical (skewed), fat-tailed, kurtotic,
present multiple peaks and what have you. - we’re generally interested
in understanding what causes a behavior, so that we can affect it–e.g.,
increase customer spending or reduce employee churn. This requires the
use of experimental or quasi-experimental methods, many of which make
our data even less “well-behaved”, statistically speaking.

Both of these aspects call for dedicated analytical approaches, which is
what this package is about. I describe in more details this
“Causal-Behavioral Framework”, as I call it, in my book [Behavioral Data
Analysis with R and
Python](https://smile.amazon.com/Behavioral-Data-Analysis-Python-Customer-Driven-ebook/dp/B0979QYPWD/)
(O’Reilly Media). But you can totally use this package without reading
the book, and I’ve tried to make the documentation self-sustaining.

## Installation

You can install the development version of BehavioralDataAnalysis from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("FlorentBuissonOReilly/BehavioralDataAnalysis")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(BehavioralDataAnalysis)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
