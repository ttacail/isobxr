# isobxr v2.0.0

This is a new major release introducing new data management protocols and new functions.
Old functions are replaced for sake of clarity.

The following exported functions were replaced as follows:
* ana_slvr() is replaced by solve_analytically()
* num_slvr() is replaced by solve_numerically()
* compose_isobxr() is replaced by sim.scenario()
* run_isobxr() is replaced by sim.single_run()
* sweep_dyn() is replaced by sweep.dyn_2D()
* sweep_steady() is replaced by sweep.final_nD()

The shinobxr_app() function was removed and replaced by new plotting functions:
* plot_dyn_2D()
* plot_scenario()

Additionally, the following functions are now exported:
* plot_relaxation()
* plot_single_run()
* read.dyn_2D_master()
* read.isobxr_master()
* read.scenario_master()
* read.final_nD_master()
* merge_FINnD_chunks()
* fit.final_space()

# isobxr v1.0.1

This is a new patch version fixing bugs. In this version I have made the following changes:

* Prevents any file writing outside tempdir in debian systems 
  for the examples/vignettes/tests
  (removing any pdf outputs in tempdir)

* Proper addressing of tempdir, 
  ensuring proper post-run clearing of tempdir including in windows systems

## Test environments
* OS X install (10.13.6), R 4.0.3 (local)
* Ubuntu Linux 20.04.1 LTS, R-devel, GCC (r-hub)
* Fedora Linux, R-devel, clang, gfortran  (r-hub)
* Debian Linux, R-devel, clang  (r-hub)
* Debian Linux, R-devel, GCC  (r-hub)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (r-hub)


## R CMD check results

* 0 errors | 0 warnings | 0 notes 

## Downstream dependencies

