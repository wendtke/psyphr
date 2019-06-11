
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psyphr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The `psyphr` R package turns
[psychophysiological](https://en.wikipedia.org/wiki/Psychophysiology)
data from proprietary data collection and processing systems into a
[tidy](http://vita.had.co.nz/papers/tidy-data.pdf) form for downstream
analysis and visualization.

The package is currently under active development. Currently, only
[Electrodermal
Activity](https://support.mindwaretech.com/manuals/software/eda/3-2/)
and [Heart Rate
Variability](https://support.mindwaretech.com/manuals/software/hrv/3-2/)
output data from [MindWare Technologies](https://www.mindwaretech.com/)
are supported.

## TODO

The development team considers the following for the package’s future:

  - Customizable data QA utilities, with meaningful defaults
  - Common visualization schemes
  - Study compilation utility
  - Compatibility on data feeds from other popular data collection and
    processing systems (e.g., [BIOPAC](https://www.biopac.com/))

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
