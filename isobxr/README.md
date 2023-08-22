
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isobxr <img src="man/figures/README-logo.png" align="right" height="200px" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/isobxr)](https://CRAN.R-project.org/package=isobxr)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/isobxr?color=blue)](https://r-pkg.org/pkg/isobxr)
<!-- badges: end -->

The [isobxr](https://ttacail.github.io/isobxr_web/) package is a set of
R tools designed to perform and explore stable isotope box modelling of
open or closed systems. It allows users to develop isotopic box models
of their system of interest, explore the behavior of these systems in
both static (*e.g.*, at steady state) or dynamic modes (*e.g.*, in
reaction to a perturbation) and build complex scenarios.  
The package also comes with tools to sweep the space of parameters in
both static and dynamic modes, and find the parameter values allowing to
fit simulations with observations.

Check the [isobxr website](https://ttacail.github.io/isobxr_web/) for
more information on [isobxr](https://ttacail.github.io/isobxr_web/) and
introduction to its rationale.

## Installation

<!-- START copy/pasted from isobxr_website install instructions -->

### Released version

The isobxr package is now on the [CRAN](https://cran.r-project.org/)
(Comprehensive R Archive Network) and can be found on its [isobxr CRAN
page](https://CRAN.R-project.org/package=isobxr).

It is available as a released package and can be installed directly from
CRAN by calling:

``` r
install.packages("isobxr")
```

### Development version

The [isobxr](https://ttacail.github.io/isobxr/) package is available as
a source package in its development version from the [GitHub isobxr
master repository](https://github.com/ttacail/isobxr).

The isobxr development version can be installed as follows:

- Download and install/update [R](https://cran.r-project.org/) and
  [Rstudio](https://posit.co/download/rstudio-desktop/).

- Download and install package **devtools** as follows:

``` r
install.packages("devtools")
```

- Download and install [isobxr](https://ttacail.github.io/isobxr/)
  source package.

``` r
devtools::install_github("ttacail/isobxr/isobxr", build_vignettes = TRUE) # FALSE if no pandoc/Rstudio 
```

<!-- END copy/pasted from isobxr_website install instructions -->

The full description of installation process can be found
[here](https://ttacail.github.io/isobxr_web/vgn_01_Installation.html).

## Principles and tutorials

<!-- START copy/pasted from isobxr_website index page -->

The documentation about underlying theory and isobxr use is made fully
available to user in the [package
website](https://ttacail.github.io/isobxr_web/index.html).

On this website you can find the following items.

1.  [How can I install isobxr and demo
    files?](https://ttacail.github.io/isobxr_web/vgn_01_Installation.html)

2.  [What is
    isobxr?](https://ttacail.github.io/isobxr_web/vgn_02_General_presentation.html)

3.  Box models single runs with run_isobxr: [general
    presentation](https://ttacail.github.io/isobxr_web/vgn_03_Run_isobxr_presentation.html)
    and
    [tutorial](https://ttacail.github.io/isobxr_web/vgn_04_Run_isobxr_tutorial.html)

4.  Box models scenario runs with compose_isobxr: [general
    presentation](https://ttacail.github.io/isobxr_web/vgn_05_compose_isobxr.html)
    and
    [tutorial](https://ttacail.github.io/isobxr_web/vgn_06_compose_isobxr_tutorial.html)

5.  Sweep parameters at the final state: [general
    presentation](https://ttacail.github.io/isobxr_web/vgn_07_sweep_steady.html)

6.  Sweep parameters in response to perturbation: [general
    presentation](https://ttacail.github.io/isobxr_web/vgn_08_sweep_dyn.html)

7.  Use the shiny offline html app to plot compose_isobxr, sweep_steady
    and sweep_dyn outputs: [general
    presentation](https://ttacail.github.io/isobxr_web/vgn_09_runShinyPlots.html)

<!-- END copy/pasted from isobxr_website index page -->

## Cite isobxr

The isobxr should be cited when used in scientific publications. You can
retrieve the citation formats by calling:

``` r
citation("isobxr")
```
