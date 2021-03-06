
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galacticEdTools

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/379769787.svg)](https://zenodo.org/badge/latestdoi/379769787)

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

An
[<img src="man/figures/Rlogo.svg" alt="R-stats logo" width="24px" height="auto">](https://www.r-project.org/)
package to support K-16 education and scicomm. This code was developed
by [Galactic Polymath Education Studio](www.galacticpolymath.com) to
solve teaching and visualization challenges associated with our lessons.
The functions do a variety of things, from making it easy to generate a
dated phylogeny that’s ready for biology presentations in minutes to
turning any ggplot graph into a fun puzzle by enciphering the graph
labels. There’s lots more to come!

## Installation

> \*Before you start (or if you run into errors), consider updating [R
> to ver 4.1.0 or better](https://mirror.las.iastate.edu/CRAN/) and
> [Rstudio ver 1.4.1717 or
> better](https://www.rstudio.com/products/rstudio/download/).

The package is not on CRAN yet, but you can install the latest version
from [GitHub](https://github.com/galacticpolymath/galacticEdTools) with:

``` r
# install.packages("devtools") #you need devtools to install from github
devtools::install_github("galacticpolymath/galacticEdTools")
#* may have to install a fair number of dependencies/updates (I'll work on streamlining this at some point)

#load it
require(galacticEdTools)
#require(datelife) If you get an error about taxa not being found in any chronograms, try this
```

## What can you do with galacticEdTools?

#### Make a dated phylogeny for teaching about evolution in just a line or two with [`showPhylo()`](https://galacticpolymath.github.io/galacticEdTools/reference/showPhylo.html)

<img src="man/figures/marsupials.png">

## And a growing number of other things. Check out the [Reference Section](reference/index.html)

## Is this package useful to you? There are several ways you can support its continued development.

1.  Use it! And tag
    <img src="man/figures/twitter_logo.png" style="padding-right: 2px" width="16px" alt="twitter logo">[@galacticPM](https://twitter.com/GalacticPM)
    &
    <img src="man/figures/twitter_logo.png" style="padding-right: 2px" width="16px" alt="twitter logo">[@mattwilkinsbio](https://twitter.com/mattwilkinsbio)
    in a Tweet showcasing what you made.
2.  [Sign up for our mailing list](https://eepurl.com/g_kQ4T) and let
    G5-16 teachers in your network know about our [free lessons and
    tools](https://www.galacticpolymath.com).
3.  If you’re a researcher who needs to do outreach, [hire
    us](https://www.galacticpolymath.com/hire-us) to make your Broader
    Impacts an easier lift, with higher production, and greater reach.
4.  Help us out! We want to streamline the transfer of knowledge from
    Academia into K-12 schools by offering unbelievably high quality,
    interdisciplinary lessons that are free for teachers. As a
    self-funded startup, we depend on volunteer and friendly contract
    labor from skilled developers (react.js, HTML, CSS, R, D3), graphic
    designers, artists, education experts and social media influencers.
    If you want to be part of the GP team, reach out to
    <matt@galacticpolymath.com>.
