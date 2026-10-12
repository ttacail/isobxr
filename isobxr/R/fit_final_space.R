#  #_________________________________________________________________________80char
#' Fit n parameters to observations
#'
#' @description  A function to find the combinations of values of n parameters
#' producing final state delta values fitting within confidence intervals of observations.
#'
#' @param workdir Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
#' of the dynamic sweep master file (e.g., \strong{\emph{0_EXPLO_DYN_MASTER.xlsx}}) \cr
#' and where output files will be stored if saved by user. \cr
#' (character string)
#' @param obs_file_name Name of csv file containing observations with csv extension. \cr
#' Stored in workdir. Example: "observations.csv" \cr
#' Should contain the following columns: \cr
#' \enumerate{
#' \item \strong{BOX_ID}: BOX ID (e.g., A, OCEAN...) as defined in isobxr master file.
#' \item \strong{delta.def} definition of delta value, e.g., d18O
#' \item \strong{delta.ref} BOX_ID of reservoir used as a reference.
#' \item \strong{obs.delta} average observed delta numerical value
#' \item \strong{obs.CI} confidence interval of delta value
#' \item \strong{obs.CI.def} definition of confidence interval, e.g., 95% ci
#' \item \strong{obs.file} name of data source file
#' }
#' Each box should appear only once for a given delta.ref.
#' @param sweep_space_digest_folders Name of sweep.final_nD digest directory. \cr
#' Should start with "4_FINnD" and end with "_digest"
#' @param fit_name Name given to specific fit. If NULL,
#' output are named after date and time of fit.
#' @param output_dir Destination directory for fit outputs. If NULL, outputs are stored in
#' sweep_space_digest_folders directory. Default is NULL.
#' @param delta_reference_box BOX ID of reference box, used to calculate difference
#' between any box delta and reference box delta. Default is NaN (no reference box). \cr
#' delta_reference_box should match at least one of the values declared
#' in the delta.ref column of observation csv file.
#' @param excluded_boxes list of boxes to exclude from fit. Default is NULL.
#' @param print_correlogram If TRUE, includes correlograms to final report when applicable. \cr
#' Default is FALSE. \cr
#' @param print_lda If TRUE, includes linear discriminant analysis to final report
#' when applicable. \cr
#' Default is FALSE.
#' @param print_LS_surfaces If TRUE, includes surfaces of least squarred residuals
#' to final report when applicable. \cr
#' Default is FALSE.
#' @param print_density_distributions If TRUE, includes density (violin) plot of all
#' and fitted simulations together with observed ranges. \cr
#' Default is FALSE.
#' @param parameter_subsets List of limits vectors for parameters to subset before fit. \cr
#' Only the runs within all subsets are used to search for the runs fitting the observations. \cr
#' For instance: list(swp.A.A_B = c(1, 1.00001))
#' to subset the swept fractionation factor from box A to B between 1 and 1.00001 (limits included). \cr
#' For lists of fluxes or coefficients, give the names of the lists to keep,
#' e.g., list(swp.flux_list_name = c("flux_1", "flux_2")).
#' @param custom_expressions Vector of expressions to add to the list of fitted parameters. \cr
#' For instance: c("m0.A/f.A_B") to add the ratios of mass of A over A to B flux
#' to the list of parameters. They can be used in parameter_subsets as "custom.<expression>".
#' @param save_outputs If TRUE, saves the pdf report and the xlsx data report in output_dir. \cr
#' If FALSE, plots are printed in the R session and the data report is returned. \cr
#' Default is FALSE.
#' @param export_fit_data If TRUE (and save_outputs is TRUE), also saves the data report
#' and the fitted runs as an RDS file. Default is FALSE.
#' @param custom.n_bins number of bins for custom variables in frequency distributions plots.
#' Default is NULL.
#'
#' @return If save_outputs is FALSE, the data report (list).
#' Otherwise NULL, the reports being saved in output_dir.
#'
#' @export
fit.final_space <- function(workdir,
                            obs_file_name,
                            sweep_space_digest_folders,
                            fit_name = NULL,
                            output_dir = NULL,
                            delta_reference_box = NaN,
                            excluded_boxes = NULL,
                            print_correlogram = FALSE,
                            print_lda = FALSE,
                            print_LS_surfaces = FALSE,
                            print_density_distributions = FALSE,
                            parameter_subsets = NULL,
                            custom_expressions = NULL,
                            save_outputs = FALSE,
                            export_fit_data = FALSE,
                            custom.n_bins = NULL){

  ###### 1. initiate ######
  ##### _ a. store and rename arguments ######
  arguments <- c(as.list(environment())) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    t() %>%
    as.data.frame()

  names(arguments) <- c("value")
  arguments$argument <- rownames(arguments)
  arguments <- arguments[,c("argument", "value")]
  rownames(arguments) <- seq(1,nrow(arguments),1)

  bx.excluded <- excluded_boxes
  dir.output <- output_dir
  dir.sweep_space_digest <- sweep_space_digest_folders
  dir.workdir <- workdir
  file.observations <- obs_file_name
  file.output <- fit_name
  delta.ref <- delta_reference_box
  remove(excluded_boxes, output_dir, sweep_space_digest_folders,
         workdir, obs_file_name, fit_name, delta_reference_box)

  if(is.null(dir.output)) dir.output <- dir.sweep_space_digest

  SERIES_RUN_ID <- sim.delta <- in.boxes_to_fit <- BOX_ID <- CI_fitted.boxes.n <-
    CI_fitted.boxes.IDs <- in.all.subset_param <- in.all_fit_boxes.obs.CI <- obs.delta <-
    delta.def <- obs.CI <- sim_obs.abs_offset <- sq_offset <- NULL

  ##### _ b. set wd  ######
  dir.old <- getwd()
  on.exit(setwd(dir.old), add = TRUE)
  setwd(dir.workdir)

  ##### _ c. argument checkup ######
  if (length(delta.ref) != 1) rlang::abort("delta_reference_box length should be one.")
  use_delta.ref <- !(is.numeric(delta.ref) && is.nan(delta.ref))
  if (use_delta.ref && !is.character(delta.ref)){
    rlang::abort("delta_reference_box should be a box name (string) or NaN.")
  }

  ##### _ d. define output name ######
  if(is.null(file.output)){
    file.output <- paste0("CI_fit_",
                          Sys.time() %>%
                            stringr::str_replace_all(pattern = " ", replacement = "_") %>%
                            stringr::str_replace_all(pattern = ":", replacement = "") %>%
                            stringr::str_replace_all(pattern = "-", replacement = "")
    )
  }

  ###### 2. import observations ######
  observations <- data.table::fread(file = file.observations, data.table = FALSE)
  names.obs <- c("BOX_ID", "delta.def", "delta.ref", "obs.delta", "obs.CI", "obs.CI.def", "obs.file")
  if(!all(names.obs %in% names(observations))){
    rlang::abort(paste0("Headers in observations file should include: ",
                        paste(names.obs, collapse = ", ")))
  }
  observations <- observations[, names.obs]
  observations$BOX_ID <- as.character(observations$BOX_ID)
  observations$delta.ref <- as.character(observations$delta.ref)

  # observations for the given reference box ("NaN" when no reference box)
  observations <- observations[which(observations$delta.ref == as.character(delta.ref)), ]
  if (nrow(observations) == 0){
    rlang::abort(paste("observations file is empty for given delta_reference_box:", delta.ref))
  }
  rownames(observations) <- NULL

  # each box should appear only once for the given reference box
  bx.obs.duplicated <- unique(observations$BOX_ID[duplicated(observations$BOX_ID)])
  if (length(bx.obs.duplicated) > 0){
    rlang::abort(paste0("The following boxes appear more than once in the observations file for delta.ref ",
                        delta.ref, ": ", paste(bx.obs.duplicated, collapse = ", "), "\n",
                        "Keep one row per box for a given delta.ref."))
  }

  ###### 3. import sweep space ######
  ##### _ a. digest files ######
  # optional for later release: merging multiple swept spaces.
  # would require verification of compatibility
  # (same system, same sets of parameters, only different resolutions and ranges)
  dir.sweep_space_digest.loc <- dir.sweep_space_digest[1]
  if (!dir.exists(dir.sweep_space_digest.loc)){
    rlang::abort(paste(dir.sweep_space_digest.loc, "directory not found."))
  }
  if (!stringr::str_ends(dir.sweep_space_digest.loc, pattern = "0_digest")){
    rlang::abort(paste(dir.sweep_space_digest.loc, "should be a sweep.final_nD digest directory (ending with 0_digest)."))
  }
  digest_file <- function(pattern){
    f <- list.files(dir.sweep_space_digest.loc, pattern = pattern)
    if (length(f) != 1) rlang::abort(paste0("Expected one ", pattern, " file in ", dir.sweep_space_digest.loc,
                                            ", found ", length(f), "."))
    normalizePath(file.path(dir.sweep_space_digest.loc, f))
  }
  file.sweep_std   <- digest_file("_merged_results\\.RDS$")
  file.log         <- digest_file("_merged_LOG\\.csv$")
  file.sweep_space <- digest_file("_merged_param_space\\.RDS$")

  ##### _ b. boxes of the simulated system ######
  LOG_SERIES <- data.table::fread(file.log, nrows = 1, data.table = FALSE)
  split_boxes <- function(x) unlist(stringr::str_split(x, pattern = "_"))
  bx.sim.all <- split_boxes(LOG_SERIES[1, "BOXES_ID_list"])
  bx.sim.infinite <- split_boxes(LOG_SERIES[1, "INFINITE_BOXES_list"])
  bx.sim.finite <- bx.sim.all[!bx.sim.all %in% bx.sim.infinite]
  bx.sim.disconnected <- split_boxes(LOG_SERIES[1, "DISCONNECTED_BOXES"])
  bx.sim.connected_finite <- bx.sim.finite[!bx.sim.finite %in% bx.sim.disconnected]
  bx.sim <- list(all = bx.sim.all,
                 infinite = bx.sim.infinite,
                 finite = bx.sim.finite,
                 disconnected = bx.sim.disconnected,
                 connected.finite = bx.sim.connected_finite)
  remove(LOG_SERIES, split_boxes,
         bx.sim.all, bx.sim.infinite, bx.sim.finite,
         bx.sim.disconnected, bx.sim.connected_finite)

  if (use_delta.ref && !delta.ref %in% bx.sim$all){
    rlang::abort(paste0("delta_reference_box ", delta.ref, " is not a box of the simulated system."))
  }
  bx.obs.not_simulated <- observations$BOX_ID[!observations$BOX_ID %in% bx.sim$all]
  if (length(bx.obs.not_simulated) > 0){
    rlang::inform(paste0("\U2757 The following observed boxes are not in the simulated system and are ignored: ",
                         paste(bx.obs.not_simulated, collapse = ", ")))
  }

  ##### _ c. results and swept parameters (one row per run) ######
  DF_wide <- readRDS(file = file.sweep_std)
  attributes(DF_wide)$out.attrs <- NULL
  sweeped_space <- readRDS(file = file.sweep_space)
  attributes(sweeped_space)$out.attrs <- NULL
  sweeped_space <- as.data.frame(sweeped_space)
  names(sweeped_space) <- paste0("swp.", names(sweeped_space))
  # swept quantitative parameters (A, D, S, R) and lists of fluxes and coefficients
  keep_swp <- grep("^swp\\.(A|D|S|R)\\.|^swp\\.(flux_list_name|coeff_list_name)",
                   names(sweeped_space), value = TRUE)
  DF_wide <- cbind(as.data.frame(DF_wide), sweeped_space[, keep_swp, drop = FALSE])
  remove(sweeped_space)
  data.table::setDT(DF_wide)
  for (nm in names(DF_wide)[vapply(DF_wide, is.character, logical(1))]){
    data.table::set(DF_wide, j = nm, value = as.factor(DF_wide[[nm]]))
  }
  quiet(gc())

  ##### _ d. custom expressions ######
  if (!is.null(custom_expressions)){
    data.table::setalloccol(DF_wide)
    for (expr in custom_expressions){
      data.table::set(DF_wide, j = paste0("custom.", expr),
                      value = eval(parse(text = expr), envir = DF_wide, enclos = parent.frame()))
    }
  }

  ##### _ e. delta values relative to reference box ######
  bx.measure <- names(DF_wide)[names(DF_wide) %in% bx.sim$all]
  if (use_delta.ref){
    ref_delta <- data.table::copy(DF_wide[[delta.ref]])
    for (bx in bx.measure) data.table::set(DF_wide, j = bx, value = DF_wide[[bx]] - ref_delta)
    remove(ref_delta)
  }

  ##### _ f. one row per run and box ######
  id_cols <- c("SERIES_RUN_ID", keep_swp, names(DF_wide)[startsWith(names(DF_wide), "custom.")])
  DF <- data.table::melt(DF_wide,
                         id.vars = id_cols,
                         measure.vars = bx.measure,
                         variable.name = "BOX_ID",
                         value.name = "sim.delta",
                         variable.factor = TRUE)
  remove(DF_wide, id_cols, bx.measure, keep_swp)
  quiet(gc())
  data.table::set(DF, j = "delta.ref", value = as.character(delta.ref))

  # add observations (one row per box, see 2.)
  obs_rows <- match(as.character(DF$BOX_ID), observations$BOX_ID)
  for (col in c("delta.def", "obs.delta", "obs.CI", "obs.CI.def", "obs.file")){
    data.table::set(DF, j = col, value = observations[[col]][obs_rows])
  }
  remove(obs_rows)

  ###### 4. targeted boxes ######
  data.table::set(DF, j = "in.boxes_to_fit", value = !(as.character(DF$BOX_ID) %in% bx.excluded))
  bx.targetted_initial <- bx.targetted <- DF$BOX_ID[DF$in.boxes_to_fit] %>%
    unique() %>%
    as.character() %>%
    sort()
  bx.observed <- observations[!(is.na(observations$obs.delta) & is.na(observations$obs.CI)), "BOX_ID"] %>%
    unique() %>%
    as.character() %>%
    sort()
  bx.targetted_obs <- bx.targetted[bx.targetted %in% bx.observed]
  bx.targetted_no_obs <- bx.targetted[!bx.targetted %in% bx.observed]
  if (length(bx.targetted_no_obs) != 0){
    rlang::inform(paste0("\U2757 The following boxes are targetted for CI-fit but observation is missing. ", "\n",
                         "   They will be removed from targetted boxes.", "\n   ",
                         paste(bx.targetted_no_obs, collapse = ", ")))
    bx.targetted <- bx.targetted_obs
    data.table::set(DF, i = which(as.character(DF$BOX_ID) %in% bx.targetted_no_obs),
                    j = "in.boxes_to_fit", value = FALSE)
  }
  if (length(bx.targetted) == 0) rlang::abort("No box to fit (all boxes are excluded or not observed).")
  bx.not_targetted <- bx.sim$all[!bx.sim$all %in% bx.targetted]
  bx.fit <- list(observed = bx.observed,
                 targetted = bx.targetted,
                 targetted_initial = bx.targetted_initial,
                 targetted_obs = bx.targetted_obs,
                 targetted_no_obs = bx.targetted_no_obs,
                 not_targetted = bx.not_targetted)
  remove(bx.targetted,
         bx.targetted_initial,
         bx.targetted_obs,
         bx.targetted_no_obs,
         bx.not_targetted)

  ###### 5. parameter subsets ######
  if (!is.null(parameter_subsets)){
    if (is.null(names(parameter_subsets)) || any(names(parameter_subsets) == "")){
      rlang::abort("parameter_subsets should be a named list.")
    }
    in.all.subset <- rep(TRUE, nrow(DF))
    for (name.subset in names(parameter_subsets)){
      if (!name.subset %in% names(DF)){
        rlang::abort(paste0("The ", name.subset,
                            " parameter_subsets name was not found in current swept space (swp.) or custom expressions (custom.)."))
      }
      limits <- parameter_subsets[[name.subset]]
      values <- DF[[name.subset]]
      if (is.character(limits)){
        in.subset <- as.character(values) %in% limits
      } else if (is.numeric(limits)){
        in.subset <- !is.na(values) & values >= min(limits) & values <= max(limits)
      } else {
        rlang::abort(paste0("Subset for ", name.subset, " should be a numerical or character vector."))
      }
      if (!any(in.subset)) rlang::abort(paste0("Subset for ", name.subset, " is out of bound."))
      data.table::set(DF, j = paste0("in.subset.", name.subset), value = in.subset)
      in.all.subset <- in.all.subset & in.subset
    }
    if (!any(in.all.subset)){
      rlang::abort("No run lies within all parameter subsets. \n Redefine parameter_subsets.")
    }
    data.table::set(DF, j = "in.all.subset_param", value = in.all.subset)
    remove(in.all.subset, in.subset, limits, values, name.subset)
  } else {
    data.table::set(DF, j = "in.all.subset_param", value = TRUE)
  }
  quiet(gc())

  ###### 6. CI fit ######
  ##### _ a. runs within observed confidence intervals ######
  data.table::set(DF, j = "min.obs.delta", value = DF$obs.delta - DF$obs.CI)
  data.table::set(DF, j = "max.obs.delta", value = DF$obs.delta + DF$obs.CI)
  data.table::set(DF, j = "in.obs.CI", value = DF$sim.delta >= DF$min.obs.delta & DF$sim.delta <= DF$max.obs.delta)

  # only the runs within all parameter subsets are used to search for fitted runs
  rows.in <- which(DF$in.obs.CI & DF$in.boxes_to_fit & DF$in.all.subset_param)
  SERIES_RUN_ID.in <- data.frame(SERIES_RUN_ID = DF$SERIES_RUN_ID[rows.in],
                                 BOX_ID = DF$BOX_ID[rows.in]) %>%
    dplyr::group_by(SERIES_RUN_ID) %>%
    dplyr::summarise(CI_fitted.boxes.n = dplyr::n(),
                     CI_fitted.boxes.IDs = paste(sort(as.character(unique(BOX_ID))), collapse = ", "),
                     .groups = "drop") %>%
    as.data.frame()
  remove(rows.in)
  if (nrow(SERIES_RUN_ID.in) == 0){
    rlang::abort("No run within the parameter subsets falls within the observed confidence interval of any targetted box.")
  }
  SERIES_RUN_ID.in <- SERIES_RUN_ID.in[order(SERIES_RUN_ID.in$CI_fitted.boxes.n, decreasing = T),]
  CI_fits.box_combinations <- SERIES_RUN_ID.in %>%
    dplyr::count(CI_fitted.boxes.n, CI_fitted.boxes.IDs) %>%
    dplyr::arrange(-CI_fitted.boxes.n)

  ##### _ b. fitted boxes and fitted runs ######
  if (max(SERIES_RUN_ID.in$CI_fitted.boxes.n) == length(bx.fit$targetted)){
    bx.fit$CI_fit.successful <- bx.fit$targetted
    SERIES_RUN_ID.in.all_boxes_in <-
      as.character(SERIES_RUN_ID.in[SERIES_RUN_ID.in$CI_fitted.boxes.n == length(bx.fit$targetted),
                                    "SERIES_RUN_ID"])
    bx.CIfit_selected <- NULL
  } else {
    print(CI_fits.box_combinations)
    bx.CIfit_selected <-
      utils::select.list(choices= CI_fits.box_combinations$CI_fitted.boxes.IDs,
                         preselect = 1,
                         title = paste("\U2757 You intend to fit the following boxes: ",
                                       paste(bx.fit$targetted,
                                             collapse = ", "), "\n",
                                       "Only the combinations below simultaneously fall within observed confidence intervals.", "\n",
                                       "Run a new CI-fit with an updated list of boxes to be excluded from fit (excluded_boxes)", "\n",
                                       "or select the combination to be CI-fitted: ", sep = ""))
    if (identical(bx.CIfit_selected, "")) rlang::abort("No combination of boxes selected.")
    bx.fit$CI_fit.successful <- unlist(stringr::str_split(bx.CIfit_selected, pattern = ", "))
    SERIES_RUN_ID.in.all_boxes_in <-
      as.character(SERIES_RUN_ID.in[SERIES_RUN_ID.in$CI_fitted.boxes.IDs == bx.CIfit_selected, "SERIES_RUN_ID"])
  }
  bx.fit$CI_fit.failed <- bx.fit$targetted[!bx.fit$targetted %in% bx.fit$CI_fit.successful]
  data.table::set(DF, j = "in.all_fit_boxes.obs.CI",
                  value = as.character(DF$SERIES_RUN_ID) %in% SERIES_RUN_ID.in.all_boxes_in)
  remove(CI_fits.box_combinations, SERIES_RUN_ID.in, SERIES_RUN_ID.in.all_boxes_in)
  quiet(gc())

  ##### _ c. counts ######
  runs_with <- function(rows) length(unique(DF$SERIES_RUN_ID[rows]))
  fit.counts <- NULL
  fit.counts$sims.all <- runs_with(seq_len(nrow(DF)))
  fit.counts$sims.in.param_subsets <- runs_with(which(DF$in.all.subset_param))
  fit.counts$sims.in.CIfit <- runs_with(which(DF$in.all_fit_boxes.obs.CI))
  fit.counts$sims.in.CIfit.param_subsets <- runs_with(which(DF$in.all_fit_boxes.obs.CI & DF$in.all.subset_param))
  fit.counts$sim_delta.fitted_boxes <- sum(DF$in.all_fit_boxes.obs.CI & DF$in.all.subset_param &
                                             as.character(DF$BOX_ID) %in% bx.fit$CI_fit.successful)
  remove(runs_with)

  ##### _ d. sum of squared residuals of fitted boxes (all runs) ######
  rows.fitted_boxes <- which(as.character(DF$BOX_ID) %in% bx.fit$CI_fit.successful)
  DF_fitness_stats <- data.frame(SERIES_RUN_ID = DF$SERIES_RUN_ID[rows.fitted_boxes],
                                 sq_offset = (DF$sim.delta[rows.fitted_boxes] - DF$obs.delta[rows.fitted_boxes])**2) %>%
    dplyr::group_by(SERIES_RUN_ID) %>%
    dplyr::summarise(SSR = sum(sq_offset), .groups = "drop") %>%
    as.data.frame()
  data.table::set(DF, j = "SSR",
                  value = DF_fitness_stats$SSR[match(as.character(DF$SERIES_RUN_ID),
                                                     as.character(DF_fitness_stats$SERIES_RUN_ID))])
  remove(DF_fitness_stats, rows.fitted_boxes)
  data.table::setDF(DF)
  quiet(gc())

  ###### 7. plots ######
  sweep_headers <- names(DF)[stringr::str_starts(names(DF), pattern = "swp.")]
  names.swp.ADSR <- c(sweep_headers[stringr::str_starts(sweep_headers, pattern = "swp.A.")],
                      sweep_headers[stringr::str_starts(sweep_headers, pattern = "swp.D.")],
                      sweep_headers[stringr::str_starts(sweep_headers, pattern = "swp.S.")],
                      sweep_headers[stringr::str_starts(sweep_headers, pattern = "swp.R.")],
                      names(DF)[stringr::str_starts(names(DF), pattern = "custom.")]) # quantitative params
  names.swp.flux_list_name <- sweep_headers[stringr::str_starts(sweep_headers, pattern = "swp.flux_list_name")]
  names.swp.coeff_list_name <- sweep_headers[stringr::str_starts(sweep_headers, pattern = "swp.coeff_list_name")]
  names.swp.lists <- c(names.swp.flux_list_name,
                       names.swp.coeff_list_name)
  remove(sweep_headers)

  # columns present in DF among the ones needed by a plot
  DF_cols <- function(cols) DF[, names(DF)[names(DF) %in% cols], drop = FALSE]

  if (length(names.swp.ADSR) != 0){
    plot.freq_ADSR <- plot_freq_ADSR(DF = DF,
                                     names.swp.ADSR = names.swp.ADSR,
                                     parameter_subsets = parameter_subsets,
                                     custom.n_bins = custom.n_bins)
  }

  if (length(names.swp.flux_list_name) != 0){
    plot.freq_flux <- plot_freq_flux(DF = DF_cols(c("swp.flux_list_name", "in.subset.swp.flux_list_name",
                                                    "in.all.subset_param", "in.all_fit_boxes.obs.CI")),
                                     parameter_subsets = parameter_subsets)
  }

  if (length(names.swp.coeff_list_name) != 0){
    plot.freq_coeff <- plot_freq_coeff(DF = DF_cols(c("swp.coeff_list_name", "in.subset.swp.coeff_list_name",
                                                      "in.all.subset_param", "in.all_fit_boxes.obs.CI")),
                                       parameter_subsets = parameter_subsets)
  }

  plot.sim_obs <- plot_sim_obs(DF_cols(c("in.all_fit_boxes.obs.CI", "in.all.subset_param", "SERIES_RUN_ID",
                                         "BOX_ID", "in.boxes_to_fit", "obs.delta", "obs.CI", "sim.delta")),
                               bx.fit, fit.counts)
  quiet(gc())

  if (print_density_distributions){
    plot.sim_distrib <- plot_sim_distrib(DF, fit.counts, observations, bx.fit)
    quiet(gc())
  }

  if (print_LS_surfaces){
    if (length(names.swp.ADSR) != 0) plots.LS_surfaces_ADSR <- plot_LS_surfaces_ADSR(DF, names.swp.ADSR)
    if (length(names.swp.lists) != 0)  plots.LS_surfaces_lists <- plot_LS_surfaces_lists(DF, names.swp.lists)
  }

  if(length(names.swp.ADSR)>0 & print_correlogram){
    plot.correlogram.CI_fit.all <- plot_correlogram_all_CIfit(DF, names.swp.ADSR)
  }

  if (print_lda) plot.lda <- plot_lda(DF, names.swp.ADSR)

  ###### 8. data report ######
  n_digits <- 3
  report.sim_obs <- DF_cols(c("BOX_ID", "in.boxes_to_fit", "in.all_fit_boxes.obs.CI", "in.all.subset_param",
                              "delta.def", "delta.ref", "obs.delta", "obs.CI", "sim.delta")) %>%
    dplyr::filter(in.all_fit_boxes.obs.CI == TRUE & in.all.subset_param == TRUE) %>%
    dplyr::group_by(BOX_ID,
                    in.boxes_to_fit) %>%
    dplyr::summarise(fitted.boxes = unique(in.boxes_to_fit),
                     delta.def = unique(delta.def),
                     delta.ref = unique(delta.ref),
                     obs.delta = suppressWarnings(mean(obs.delta, na.rm = T)),
                     obs.CI = dec_n(mean(obs.CI), n_digits),
                     sim_obs.abs_offset = dec_n(abs(mean(sim.delta) - suppressWarnings(mean(obs.delta))), n_digits),
                     sim.n = dplyr::n(),
                     sim.05 =  dec_n(stats::quantile(sim.delta, .05), n_digits),
                     sim.25 =  dec_n(stats::quantile(sim.delta, .25), n_digits),
                     sim.50 =  dec_n(stats::quantile(sim.delta, .50), n_digits),
                     sim.75 =  dec_n(stats::quantile(sim.delta, .75), n_digits),
                     sim.95 =  dec_n(stats::quantile(sim.delta, .95), n_digits),
                     sim.min = dec_n(min(sim.delta), n_digits),
                     sim.mean = dec_n(mean(sim.delta, na.rm = T), n_digits),
                     sim.max = dec_n(max(sim.delta), n_digits),
                     sim.2sd = 2*stats::sd(sim.delta),
                     .groups = "drop_last") %>%
    as.data.frame() %>%
    dplyr::arrange(sim_obs.abs_offset)

  # fitted.boxes: TRUE (fitted), FALSE (targetted, not fitted), NA (not targetted)
  report.sim_obs$fitted.boxes <- NA
  report.sim_obs[report.sim_obs$BOX_ID %in% bx.fit$CI_fit.successful, "fitted.boxes"] <- TRUE
  report.sim_obs[report.sim_obs$BOX_ID %in% bx.fit$CI_fit.failed, "fitted.boxes"] <- FALSE
  report.sim_obs$sim.2se <- dec_n(report.sim_obs$sim.2sd/sqrt(report.sim_obs$sim.n),n_digits)
  report.sim_obs$sim.2sd <- dec_n(report.sim_obs$sim.2sd,n_digits)

  data.report <- list(arguments = arguments,
                      sim_vs_obs = cbind(fit_name = file.output, report.sim_obs))

  if (c("plot.freq_ADSR") %in% ls()){
    data.report <- c(data.report,
                     list(QuantPars_DescStats = cbind(fit_name = file.output, plot.freq_ADSR$stats),
                          QuantPars_FitFreqs = cbind(fit_name = file.output, plot.freq_ADSR$freqs)))
  }

  if (c("plot.freq_flux") %in% ls()){
    data.report <- c(data.report,
                     list(FluxLists_FitFreqs = cbind(fit_name = file.output, plot.freq_flux$freqs)))
  }

  if (c("plot.freq_coeff") %in% ls()){
    data.report <- c(data.report,
                     list(CoeffLists_FitFreqs = cbind(fit_name = file.output, plot.freq_coeff$freqs)))
  }

  ###### 9. save / print ######
  if (save_outputs){
    if (!dir.exists(dir.output)) dir.create(dir.output, recursive = TRUE)
    writexl::write_xlsx(data.report, paste0(dir.output, "/", file.output, "_data_report.xlsx"))
    if (export_fit_data){
      data.report$fitted_data <- cbind(fit_name = file.output,
                                       DF[DF$in.all_fit_boxes.obs.CI == TRUE & DF$in.all.subset_param == TRUE, ])
      saveRDS(data.report,
              paste0(dir.output, "/", file.output, "_CI_fit_data.RDS"))
      data.report$fitted_data <- NULL
    }
  } else if (export_fit_data){
    rlang::inform("\U2757 export_fit_data is only used when save_outputs = TRUE.")
  }

  suppressWarnings({
    suppressMessages({
      if (save_outputs){
        grDevices::pdf(width = 21/2.54, height = 29.7/2.54,
                       file = paste0(dir.output, "/", file.output, ".pdf"))
      }

      if (is.null(bx.CIfit_selected)){
        message_fit <- paste("\n", "Observation-Simulation fit", "\n", "\n",
                             "All targeted boxes successfully CI-fitted", "\n")
        message_color <- "darkgreen"
      } else {
        message_fit <- paste("\n", "Observation-Simulation fit", "\n","\n",
                             "! NO CONVERGENCE ! \n All targeted boxes could not be fitted", sep = "")
        message_color <- "red"
      }

      gridExtra::grid.arrange(top = grid::textGrob( message_fit,
                                                    gp = grid::gpar(fontface = "bold", col = message_color)),
                              plot.sim_obs)

      page_title <- "Frequency plots"
      if (all(c("plot.freq_coeff", "plot.freq_flux", "plot.freq_ADSR") %in% ls())){
        gridExtra::grid.arrange(top = grid::textGrob( page_title , gp = grid::gpar(fontface = "bold")),
                                gridExtra::arrangeGrob(
                                  gridExtra::arrangeGrob(plot.freq_flux$plot,
                                                         plot.freq_coeff$plot, ncol = 2),
                                  plot.freq_ADSR$plot,
                                  ncol = 1,
                                  heights = c(1,3)
                                ))
      } else if (all(c("plot.freq_flux", "plot.freq_ADSR") %in% ls())) {
        gridExtra::grid.arrange(top = grid::textGrob( page_title , gp = grid::gpar(fontface = "bold")),
                                gridExtra::arrangeGrob(
                                  plot.freq_flux$plot,
                                  plot.freq_ADSR$plot,
                                  ncol = 1,
                                  heights = c(1,1.3)
                                ))
      } else if (all(c("plot.freq_coeff", "plot.freq_ADSR") %in% ls())) {
        gridExtra::grid.arrange(top = grid::textGrob( page_title , gp = grid::gpar(fontface = "bold")),
                                gridExtra::arrangeGrob(
                                  plot.freq_coeff$plot,
                                  plot.freq_ADSR$plot,
                                  ncol = 1,
                                  heights = c(1,3)
                                ))
      } else if (all(c("plot.freq_coeff", "plot.freq_flux") %in% ls())) {
        gridExtra::grid.arrange(top = grid::textGrob( page_title , gp = grid::gpar(fontface = "bold")),
                                gridExtra::arrangeGrob(
                                  plot.freq_coeff$plot,
                                  plot.freq_flux$plot,
                                  ncol = 1,
                                  heights = c(1,1)
                                ))
      } else if (all(c("plot.freq_coeff") %in% ls())) {
        gridExtra::grid.arrange(top = grid::textGrob( page_title, gp = grid::gpar(fontface = "bold")),
                                plot.freq_coeff$plot)
      } else if (all(c("plot.freq_flux") %in% ls())) {
        gridExtra::grid.arrange(top = grid::textGrob( page_title, gp = grid::gpar(fontface = "bold")),
                                plot.freq_flux$plot)
      } else if (all(c("plot.freq_ADSR") %in% ls())) {
        gridExtra::grid.arrange(top = grid::textGrob( page_title, gp = grid::gpar(fontface = "bold")),
                                plot.freq_ADSR$plot)
      }

      if (all(c("plot.sim_distrib") %in% ls())){
        page_title <- "Distributions of simulated isotope compositions"
        gridExtra::grid.arrange(top = grid::textGrob( page_title,
                                                      gp = grid::gpar(fontface = "bold", col = message_color)),
                                plot.sim_distrib
        )
      }

      if (all(c("plots.LS_surfaces_lists", "plots.LS_surfaces_ADSR") %in% ls())){
        gridExtra::grid.arrange(top = grid::textGrob( "Least squares surface plots", gp = grid::gpar(fontface = "bold")),
                                gridExtra::arrangeGrob(plots.LS_surfaces_lists$plot.surface_lists_all,
                                                       plots.LS_surfaces_ADSR$plot.all,
                                                       ncol = 1, heights= c(2,3)))
      } else if (all(c("plots.LS_surfaces_lists") %in% ls())){
        gridExtra::grid.arrange(top = grid::textGrob("Least squares surface plots",
                                                     gp = grid::gpar(fontface = "bold")),
                                plots.LS_surfaces_lists$plot.surface_lists_all)
      } else if (all(c("plots.LS_surfaces_ADSR") %in% ls())){
        gridExtra::grid.arrange(top = grid::textGrob("Least squares surface plots",
                                                     gp = grid::gpar(fontface = "bold")),
                                plots.LS_surfaces_ADSR$plot.all)
      }

      if (all(c("plot.correlogram.CI_fit.all") %in% ls())){
        print(plot.correlogram.CI_fit.all +
                ggplot2::labs(title = "Full correlogram (all CI-fitted quantitative parameters)"))
      }

      if (all(c("plot.lda") %in% ls())) {
        gridExtra::grid.arrange(top = grid::textGrob( "Linear discriminant analysis (all CI-fitted quantitative parameters)",
                                                      gp = grid::gpar(fontface = "bold")),
                                plot.lda)
      }

      if (save_outputs){
        grDevices::dev.off()
      }
    })
  })

  remove(DF)
  quiet(gc())

  if (!save_outputs){
    return(data.report)
  }
}
