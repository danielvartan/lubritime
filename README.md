
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lubritime <a href='https://gipso.github.io/lubritime'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

![CRAN status](https://www.r-pkg.org/badges/version/lubritime)
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
[![Buy Me A Coffee donate
button](https://img.shields.io/badge/buy%20me%20a%20coffee-donate-yellow.svg)](https://ko-fi.com/danielvartan)
<!-- badges: end -->

## Overview

`lubritime` is an extension for the
[lubridate](https://github.com/tidyverse/lubridate) package from
tidyverse, adding new features to deal with temporal objects.

Please note that `lubritime` is not related in any way with `RStudio` or
the `lubridate` developer team.

> Please note that this package is currently on the development stage
> and have not yet been [peer
> reviewed](https://devguide.ropensci.org/softwarereviewintro.html).

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

`lubritime` is still at the
[experimental](https://lifecycle.r-lib.org/articles/stages.html#experimental)
stage of development. That means people can use the package and provide
feedback, but it comes with no promises for long term stability.

You can install `lubritime` from GitHub with:

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
#>   Vartanian, D., Pedrazzoli, M. (2021). {lubritime}: An extension for
#>   the {lubridate} package. Retrieved from
#>   https://gipso.github.io/lubritime/.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Unpublished{,
#>     title = {{lubritime}: An extension for the {lubridate} package},
#>     author = {Daniel Vartanian and Mario Pedrazzoli},
#>     year = {2021},
#>     url = {https://gipso.github.io/lubritime/},
#>     note = {Lifecycle: experimental},
#>   }
```

## Support `lubritime`

[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/danielvartan)

Working with science in Brazil is a daily challenge. There are few
funding opportunities available and their value is not enough to live
on. Added to this, every day Brazilian science suffers from deep cuts in
funding, which requires researchers to always look for other sources of
income.

If this package helps you in any way or you simply want to support the
author’s work, please consider donating or even creating a membership
subscription (if you can!). Your support will help with the author’s
scientific pursuit and with the package maintenance.

To make a donation click on the [Ko-fi](https://ko-fi.com/danielvartan)
button above. Please indicate the `lubritime` package in your donation
message.

Thank you!
