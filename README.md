
<!-- README.md is generated from README.Rmd. Please edit that file -->

# galacticEdTools

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

GalacticEdTools is an
[<img src="man/figures/Rlogo.svg" alt="R-logo" width="24px" height="auto">](https://www.r-project.org/)
package to support K-16 education and scicomm. This code was developed
by [Galactic Polymath Education Studio](www.galacticpolymath.com) to
solve teaching and visualization challenges associated with our lessons.
Functions do a variety of things, from making it easy to generate a
dated phylogeny that’s ready for biology presentations in minutes to
turning any ggplot graph into a fun puzzle by enciphering the graph
labels. There’s lots more to come\!

## Installation

You can install the released version of galacticEdTools from
[GitHub](https://github.com/galacticpolymath/galacticEdTools) with:

``` r
# install.packages("devtools") #install devtools if you don't have it
devtools::install_github("galacticpolymath/galacticEdTools")
#> Using github PAT from envvar GITHUB_PAT
#> Skipping install of 'galacticEdTools' from a github remote, the SHA1 (b1afa7e2) has not changed since last install.
#>   Use `force = TRUE` to force installation
#you may have to install a fair number of dependencies/updates (I'll work on streamlining this at some point)

#load it
require(galacticEdTools)
#> Loading required package: galacticEdTools
#> Loading required package: datelife
```

## Example

Ever wanted to make a phylogeny to—for example—show that birds are
*actually* dinosaurs? Did you end up freehanding it in Powerpoint? To
teach evolution, we need to be able to make scientifically accurate
trees on the fly. The showPhylo() function aims to make this as simple
as possible, freeing you up to focus on building out the lesson around
this evolutionary visualization.

So let’s pick some dinosaurs, a dino-actin’ bird like the common
grackle, and a really improbable bird like the red-billed streamertail
hummingbird. I’ll make a vector of these species’ scientific names.

And with no changes to the plot, here’s what it should look like.

``` r
plot(dinos)
```

<img src="man/figures/README-first-dinophylo-1.png" width="100%" />

By default, pic=“wiki”, meaning taxonomic images are pulled in from the
appertaining Wikipedia entry (if available). You can also use phyloPics
(silhouettes, as shown below), or supply your own images. Check out the
vignette for showPhyloPic() to learn about more customizations.

``` r
plot(dino2)
```

<img src="man/figures/README-phyloPic-dino-phylogeny-1.png" width="100%" />
