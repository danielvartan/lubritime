
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lubritime

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/giperbio/lubritime/workflows/R-CMD-check/badge.svg)](https://github.com/giperbio/lubritime/actions)
[![Codecov test
coverage](https://codecov.io/gh/giperbio/lubritime/branch/main/graph/badge.svg)](https://app.codecov.io/gh/giperbio/lubritime?branch=main)
[![License:
MIT](https://img.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
[![Contributor
Covenant](https://img.shields.io/badge/Contributor%20Covenant-v2.0%20adopted-ff69b4.svg)](https://giperbio.github.io/lubritime/CODE_OF_CONDUCT.html)
<!-- badges: end -->

## Overview

`lubritime` is an extension for the
[lubridate](https://github.com/tidyverse/lubridate) package from
[tidyverse](https://www.tidyverse.org/), adding new features to deal
with temporal objects.

Please note that `lubritime` is not related in any way with
`Posit/RStudio` or the `lubridate` developer team.

## Prerequisites

You need to have some familiarity with the [R programming
language](https://www.r-project.org/) and with the
[lubridate](https://lubridate.tidyverse.org/) and
[hms](https://hms.tidyverse.org/) packages from
[tidyverse](https://www.tidyverse.org/) to use `lubritime` main
functions.

If you don’t feel comfortable with R, we strongly recommend checking
Hadley Wickham and Garrett Grolemund free and online book [R for Data
Science](https://r4ds.had.co.nz/) and the Coursera course from John
Hopkins University [Data Science: Foundations using
R](https://www.coursera.org/specializations/data-science-foundations-r)
(free for audit students).

Please refer to the [lubridate](https://lubridate.tidyverse.org/) and
[hms](https://hms.tidyverse.org/) package documentation to learn more
about them. These two are essential packages to deal with date/time data
in R. We also recommend that you read the [Dates and
times](https://r4ds.had.co.nz/dates-and-times.html) chapter from Wickham
& Grolemund’s book [R for Data Science](https://r4ds.had.co.nz/).

## Installation

You can install `lubritime` with:

``` r
# install.packages("remotes")
remotes::install_github("giperbio/lubritime")
```

## Citation

If you use `lubritime` in your research, please consider citing it. We
put a lot of work to build and maintain a free and open-source R
package. You can find the citation below.

``` r
citation("lubritime")
#> 
#> To cite {lubritime} in publications use:
#> 
#>   Vartanian, D. (2023). {lubritime}: an extension for the {lubridate}
#>   package. R package version 0.0.0.9000.
#>   https://giperbio.github.io/lubritime/
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Unpublished{,
#>     title = {{lubritime}: an extension for the {lubridate} package},
#>     author = {Daniel Vartanian},
#>     year = {2023},
#>     url = {https://giperbio.github.io/lubritime/},
#>     note = {R package version 0.0.0.9000},
#>   }
```

## Contributing

We welcome contributions, including bug reports.

Take a moment to review our [Guidelines for
Contributing](https://giperbio.github.io/lubritime/CONTRIBUTING.html).

<br>

Become an `lubritime` supporter!

Click [here](https://github.com/sponsors/danielvartan) to make a
donation. Please indicate the `lubritime` package in your donation
message.
