# isobxr

<!-- badges: start -->
<!-- badges: end -->

The [isobxr](https://ttacail.github.io/isobxr/) package is a set of R tools designed to perform and explore stable isotope box
modelling of open or closed systems.
It allows users to develop and test isotopic box models of their system of interest, explore the behavior of these systems
in both static (*e.g.*, at steady state) or dynamic modes (*e.g.*, in reaction to a perturbation), build complex scenarios, 
as well as sweep the space of parameters in both static and dynamic modes.

## Installation

The **isobxr** is available as a source package from [GitHub](https://github.com/).

1. Download and install/update [R](https://cran.r-project.org/).

2. Download and install package **devtools**

``` r
install.packages("devtools")
```

3. Download and install **isobxr** source package.

``` r
devtools::install_github("ttacail/isobxr/isobxr", build_vignettes = TRUE) # FALSE if no pandoc/Rstudio 
```

The **isobxr** package is working under R 3.5.2 to 4.0.3 versions.

It's portability has been proof checked for the following platforms: 

1. Mac OS 10.12.6 & 10.13.6

2. Linux (Ubuntu 20.04.1 LTS)

3. Windows 10

## Principle and use

The documentation about underlying theory and isobxr utilization is made fully available to user in the package vignettes.

The vignettes can be found online: 

1. [Using isobxr](https://ttacail.github.io/isobxr/articles/isobxr_vignette.html)

The user is notably invited to go through the [Using isobxr](https://ttacail.github.io/isobxr/articles/isobxr_vignette.html) vignette up to the [Running isobxr](https://ttacail.github.io/isobxr/articles/isobxr_vignette.html#running-isobxr-box-models-with-run-isobxr) section for single box model runs and then if desired to the [Compose isobxr scenarios](https://ttacail.github.io/isobxr/articles/isobxr_vignette.html#compose-isobxr-scenarios-with-compose-isobxr) section.

The vignettes can also be locally loaded on R or Rstudio after the package installation as follows:
``` r
library(isobxr)
browseVignettes("isobxr")
```

## Example
This is a basic example which shows you how to run a simple box model, using the demo_ABCD models, available on demand.

``` r
library(isobxr)

run_isobxr(workdir =  "~/DEMO_ABCD", # isobxr master file work. dir.
           SERIES_ID = "ABC_balanced_closed", # series ID of the set of runs
           flux_list_name = "Fx1_ABC_bal", # which flux list from FLUXES sheet
           coeff_list_name = "a1", # which coefficients list from COEFFS sheet 
           t_lim = 2500, # how long do I want to run
           nb_steps = 250, # how many steps over this run duration
           time_units = c("days", "years"), # run time units (days), plot time units (years)
           PLOT_evD = TRUE) # export plot as pdf 
```

