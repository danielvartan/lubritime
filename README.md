
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lubritime

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/gipso/lubritime/workflows/R-CMD-check/badge.svg)](https://github.com/gipso/lubritime/actions)
[![Codecov test
coverage](https://codecov.io/gh/gipso/lubritime/branch/main/graph/badge.svg)](https://codecov.io/gh/gipso/lubritime?branch=main)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://gipso.github.io/lubritime/CODE_OF_CONDUCT.html)
<!-- badges: end -->

## Overview

`lubritime` is an extension for the
[lubridate](https://github.com/tidyverse/lubridate) package from
tidyverse, adding new features to deal with temporal objects.

Please note that `lubritime` is not related in any way with `RStudio` or
the `lubridate` developer team.

## Prerequisites

You only need to have some familiarity with the [R programming
language](https://www.r-project.org/) to use `lubritime` main functions.

In case you don’t feel comfortable with R, we strongly recommend
checking Hadley Wickham and Garrett Grolemund’s free and online book [R
for data Science](https://r4ds.had.co.nz/) and the Coursera course from
John Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r)
(free for audit students).

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("gipso/lubritime")
```

## Citation

If you use `lubritime` in your research, please consider citing it. We
put a lot of work to build and maintain a free and open-source R
package. You can find the `lubritime` citation below.

``` r
citation("lubritime")
#> 
#> To cite {lubritime} in publications use:
#> 
#>   Vartanian, D., & Pedrazzoli, M. (2022). {lubritime}: an extension for
#>   the {lubridate} package. https://gipso.github.io/lubritime/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Unpublished{,
#>     title = {{lubritime}: an extension for the {lubridate} package},
#>     author = {Daniel Vartanian and Mario Pedrazzoli},
#>     year = {2022},
#>     url = {https://gipso.github.io/lubritime/},
#>     note = {Lifecycle: experimental},
#>   }
```

## Contributing

We welcome contributions, including bug reports. Take a moment to review
our [Guidelines for
Contributing](https://gipso.github.io/lubritime/CONTRIBUTING.html).

Please note that `actverse` is released with a [Contributor Code of
Conduct](https://gipso.github.io/lubritime/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
