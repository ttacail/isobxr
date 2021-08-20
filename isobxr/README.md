
<!-- README.md is generated from README.Rmd. Please edit that file -->
# isobxr <img src="man/figures/README-logo.png" align="right" height="200px" />

<!-- badges: start -->
<!-- badges: end -->
The [isobxr](https://ttacail.github.io/isobxr_web/) package is a set of R tools designed to perform and explore stable isotope box modelling of open or closed systems. It allows users to develop and test isotopic box models of their system of interest, explore the behavior of these systems in both static (*e.g.*, at steady state) or dynamic modes (*e.g.*, in reaction to a perturbation), build complex scenarios, as well as sweep the space of parameters in both static and dynamic modes.

Check the [isobxr website](https://ttacail.github.io/isobxr_web/) for more information on [isobxr](https://ttacail.github.io/isobxr_web/) and introduction to its rationale.

## Installation

<!-- ## CRAN version -->
<!-- You can install the released version of isobxr from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("isobxr") -->
<!-- ``` -->
### Development version

The [isobxr](https://ttacail.github.io/isobxr/) is available as a source package from [GitHub](https://github.com/).

The package can be installed as follows:

1.  Download and install/update [R](https://cran.r-project.org/) and [Rstudio](https://www.rstudio.com/products/rstudio/download/).

2.  Download and install package **devtools** as follows:

``` r
install.packages("devtools")
```

1.  Download and install [isobxr](https://ttacail.github.io/isobxr/) source package.

``` r
devtools::install_github("ttacail/isobxr/isobxr", build_vignettes = TRUE) # FALSE if no pandoc/Rstudio 
```

The full description of installation process can be found [here](https://ttacail.github.io/isobxr_web/vgn_01_Installation.html).

## Principles and tutorials

The documentation about underlying theory and isobxr use is made fully available to user in the [package website](https://ttacail.github.io/isobxr_web/index.html).

On this website you can find the following items.

1.  [How can I install isobxr and demo files?](https://ttacail.github.io/isobxr_web/vgn_01_Installation.html)

2.  [What is isobxr ?](https://ttacail.github.io/isobxr_web/vgn_02_General_presentation.html)

3.  Box models single runs with run\_isobxr: [general presentation](https://ttacail.github.io/isobxr_web/vgn_03_Run_isobxr_presentation.html) and [tutorial](https://ttacail.github.io/isobxr_web/vgn_04_Run_isobxr_tutorial.html)

4.  Box models scenario runs with compose\_isobxr: [general presentation](https://ttacail.github.io/isobxr_web/vgn_05_compose_isobxr.html) and [tutorial](https://ttacail.github.io/isobxr_web/vgn_06_compose_isobxr_tutorial.html)

5.  Sweep parameters at the final state: [general presentation](https://ttacail.github.io/isobxr_web/vgn_07_sweep_steady.html)

6.  Sweep parameters in response to perturbation: [general presentation](https://ttacail.github.io/isobxr_web/vgn_08_sweep_dyn.html)

7.  Use the shiny offline html app to plot compose\_isobxr, sweep\_steady and sweep\_dyn outputs: [general presentation](https://ttacail.github.io/isobxr_web/vgn_09_runShinyPlots.html)
