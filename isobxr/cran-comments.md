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

## Test environments
* OS X install (10.15.7), R 4.2.1 (local)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (rhub)
* Fedora Linux, R-devel, clang, gfortran (rhub)
* Windows Server 2022, R-devel, 64 bit (rhub)

## R CMD check results

* 0 errors | 0 warnings | 0 notes 
