# isobxr 2.1.0

## Bug fixes

* sim.single_run(): with dplyr >= 1.1, the initial sizes and deltas could be assigned
  to the wrong boxes in sim.scenario() runs after the first one (boxes sorted in a
  different order). Initial state and box metadata are now matched to the box list.
* sim.single_run(): FORCING_SIZE and FORCING_RAYLEIGH were checked against the wrong list.
* Plot data summaries updated for dplyr >= 1.1 (multi-row summarise).
* In the LOG, SIZE.t0 and DELTA.t0 are now listed in the order of BOXES_ID_list.

## sweep.final_nD()

* sweep.final_nD.2() is removed: sweep.final_nD() runs chunks in parallel
  (mc.cores, default all cores but one; one core on Windows).
* New ask_confirmation argument.
* Each chunk is checked for failed workers and missing runs.
* Each chunk's runs are appended to 1_LOG.csv instead of rewriting the whole file,
  and the temporary LOG header is written once (fixes a possible silent loss of LOG rows).

## sim.scenario(), sweep.dyn_2D()

* Runs no longer read 1_LOG.csv, which makes them much faster in folders with a large LOG.
  sim.single_run() returns its LOG row (invisibly) when return_data = FALSE.

## fit.final_space()

* fit.final_space_2() and fit.final_space_3() (development versions) are merged into
  fit.final_space(), which is faster and uses less memory.
* parameter_subsets are now applied before selecting the fitted runs: the fit searches
  the best combination of boxes among the runs within the subsets. Numerical limits are inclusive.
* The observation file may not list a box twice for the same delta.ref; observed boxes
  absent from the simulated system are ignored with a message.
* Custom expressions keep their written form in reports (e.g. custom.100*f.A_B/f.B_C).
* The LDA page is drawn again when print_lda = TRUE.
* export_fit_data only writes the RDS file when save_outputs = TRUE.

## Dependencies

* ggplot2 >= 3.4.0 (linewidth); parallel added to Imports.

# isobxr 2.0.0

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

# isobxr 1.0.1

## bug fixes

* prevent writing of (pdf) files outside tempdir in all examples/vignettes/tests (debian systems issue)

* fixed addressing of tempdir, ensuring proper post-run clearing of tempdir including in windows systems

# isobxr 1.0.0 (Released: 2021-09-02)

* first released version
