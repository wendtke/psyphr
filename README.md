
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psyphr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The `psyphr` R package turns
[psychophysilology](https://en.wikipedia.org/wiki/Psychophysiology) lab
data from proprietary equipments into a
[tidy](http://vita.had.co.nz/papers/tidy-data.pdf) form, wherever
approapriate, for downstream analysis.

The package is currently under active development. Currently, only
[EDA](https://support.mindwaretech.com/manuals/software/eda/3-2/) and
[HRV](https://support.mindwaretech.com/manuals/software/hrv/3-2/) data
from MindWare is supported.

## TODO

The development team considers the following for the package’s future:

  - Customizable data QA utilities, with meaningful defaults
  - Common visualization schemes
  - Study compilation utility
  - Compatibility on data feeds from more vendors’ equipments

## Installation

You can install the released version of psyphr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("psyphr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("wendtke/psyphr")
```
