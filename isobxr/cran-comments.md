## Resubmission 26/08/2021
This is a resubmission. In this version I have: 

* Added \value to all .Rd files and explained the functions results, 
    including the missing Rd-tags: 
      multiplot.Rd: \value
      quiet.Rd: \value
      shinobxr_app.Rd: \value
      time_converter.Rd: \value
    and the newly created internal, not exported functions: 
      using_extdata_tutorial.Rd
      plot_diagram.Rd
      to_tmpdir.Rd

* Removed any use of the installed.packages() function. 

* Implemented new output data management by preventing any default writing 
  in any user's home filespaces. All outputs are writen by default in 
  a temporary directory. In all functions writing files 
  (ana_slvr, num_slvr, run_isobxr, compose_isobxr, sweep_dyn, sweep_steady), 
  the user needs to change the save_run_outputs argument to TRUE.
  As a result, the functions do not write by default or in the
  examples/vignettes/tests in the user's home filespace (including the
  package directory and getwd()).

## Resubmission 20/08/2021
This is a resubmission. In this version I have: 

* Reduced the size of the tarball below 5 MB

* inlcuded a doi reference to the Description field of DESCRIPTION.

## Test environments
* local OS X install, R 4.0.3
* Ubuntu Linux 20.04.1 LTS, R-devel, GCC
* Fedora Linux, R-devel, clang, gfortran
* Windows Server 2008 R2 SP1, R-patched, 32/64 bit
* Windows Server 2008 R2 SP1, R-release, 32/64 bit
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit


## R CMD check results
There were no ERRORs, or WARNINGs or NOTEs at local R CMD check.

There will be one NOTE: 
  * This is the first sumbission of this package to CRAN.



## Downstream dependencies


