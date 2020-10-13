# isobxr

<!-- badges: start -->
<!-- badges: end -->

The **isobxr** package is a set of R tools designed to perform and explore stable isotope box
modeling of open or closed systems.
The aim of this code is to provide a ready-to-use tool allowing users to 
develop and test isotopic box models of their system of interest. 
It allows the user to explore the behavior of these systems
in both static (*e.g.*, at steady state) or dynamic modes (*e.g.*, in reaction to a perturbation), build complex scenarios, 
as well as sweep the space of parameters in both static and dynamic modes.

## Installation

You can install the released version of isobxr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ttacail/isobxr/isobxr", build_vignettes = TRUE)
```

## Princple and use

All principles and functions are described in the package vignette.

## Example

This is a basic example which shows you how to run a simple box model. 

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

