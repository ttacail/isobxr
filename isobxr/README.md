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

1. [Install isobxr](https://ttacail.github.io/isobxr/articles/vgn_01_Installation.html)
2. [General presentation](https://ttacail.github.io/isobxr/articles/vgn_02_General_presentation.html)
3. [Run_isobxr: presentation](https://ttacail.github.io/isobxr/articles/vgn_03_Run_isobxr_presentation.html)
4. [Run_isobxr: tutorial](https://ttacail.github.io/isobxr/articles/vgn_04_Run_isobxr_tutorial.html)

The vignettes can also be locally loaded on R or Rstudio after the package installation as follows:
``` r
library(isobxr)
browseVignettes("isobxr")
```

## Example
This is a basic example which shows you how to run a simple box model, using the demo_ABCD models, available on demand.

``` r
library(isobxr)

run_isobxr(workdir = workdir_ABC, # work. directory
           SERIES_ID = "ABC_closed_balanced", # name of the series of runs
           flux_list_name = "Fx1_ABC_closed_bal", # use this list of fluxes/sizes
           coeff_list_name = "a1", # use list a1 of fractionation coeffs.
           t_lim = 2500, # run the model over 2500 days
           nb_steps = 250, # calculate system state in 250 steps
           time_units = c("d", "yr"), # run time units (days), plot time units (years)
           to_DIGEST_evD_PLOT = TRUE, # export plot as pdf
           to_DIGEST_CSV_XLS = TRUE, # export all data as csv and xlsx
           to_DIGEST_DIAGRAMS = TRUE) # export system diagrams as pdf
```

