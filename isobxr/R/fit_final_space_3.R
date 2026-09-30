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
#' @param sweep_space_digest_folders Name of sweep.final_nD digest directory. \cr
#' Should start with "4_FINnD" and end with "_digest"
#' @param fit_name Name given to specific fit. If NULL,
#' output are named after date and time of fit.
#' @param output_dir Destination directory for fit outputs. If NULL, outputs are stored in
#' sweep_space_digest_folders directory. Default is NULL.
#' @param delta_reference_box BOX ID of reference box, used to calculate difference
#' between any box delta and reference box delta. Default is NaN. \cr
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
#' For instance: list(swp.A.A_B = c(1, 1.00001))
#' to subset the swept fractionation factor from box A to B between 1 and 1.00001.
#' @param custom_expressions Vector of expressions to add to the list of fitted parameters. \cr
#' For instance: c("m0.A/f.A_B") to add the ratios of mass of A over A to B flux
#' to the list of parameters.
#' @param save_outputs If TRUE, saves all run outputs to local working directory (workdir). \cr
#' By default, run outputs are stored in a temporary directory and erased if not saved.
#' Default is FALSE.
#' @param export_fit_data If TRUE, exports fitted data as csv and rds files.
#' @param custom.n_bins number of bins for custom variables in frequency distributions plots.
#' Default is NULL.
#'
#' @return A observation fit graphical report, in R session or exported as pdf, and a data report as R list or xlsx if required.
#'
#' @export
fit.final_space_3 <- function(workdir,
                              obs_file_name,
                              sweep_space_digest_folders,
                              fit_name = NULL,
                              output_dir = NULL,
                              delta_reference_box = NaN,
                              excluded_boxes = NULL,
                              print_correlogram = FALSE,
                              print_lda = FALSE,
                              print_LS_surfaces = FALSE,
                              parameter_subsets = NULL,
                              custom_expressions = NULL,
                              save_outputs = FALSE,
                              export_fit_data = FALSE,
                              custom.n_bins = NULL) {
  # #  = = = = = = = = = = = = = =
  # # x) debug ####
  # #  = = = = = = = = = = = = = =
  # # clear workspace
  # if(!is.null(dev.list())) dev.off()
  # ## Clear console
  # ## cat("\014")
  # ## Clean workspace
  # rm(list=ls())
  # gc()
  #
  # library(data.table)
  # library(stringr)
  #
  # # debug arguments
  # fit_name = NULL
  # output_dir = NULL
  # delta_reference_box = NaN
  # excluded_boxes = NULL
  # print_correlogram = FALSE
  # print_lda = FALSE
  # print_LS_surfaces = FALSE
  # parameter_subsets = NULL
  # custom_expressions = NULL
  # save_outputs = FALSE
  # export_fit_data = FALSE
  # custom.n_bins = NULL
  # print_density_distributions = FALSE
  #
  # workdir = "/Users/sz18642/OneDrive - University of Bristol/CGL_Ca_Gives_Life/Projets/ENos dCa/0_ENOS_boxmod/3_ENOS_R/2_models/2_sweep_FINnD_human_NIR_1"
  # obs_file_name = "obs_matrix_all.csv"
  # sweep_space_digest_folders = "4_FINnD_0_SWEEP_FINnD_001_000_digest"
  # fit_name = "2_obs_matrix_all_vDIET_BNE_UR_FEC_DIET"
  # # output_dir = NULL
  # delta_reference_box = "PLAf"
  # excluded_boxes = c("WASTE", "PLAf", "GIT", "ITG", "KDN", "PLAp", "rBNE", "ST")
  # print_correlogram = FALSE
  # # print_lda = FALSE
  # # print_LS_surfaces = TRUE
  # custom_expressions = c("f.KDN_UR", "100*f.KDN_UR/f.PLAf_KDN", "f.PLAf_GIT")
  # save_outputs = TRUE
  # # export_fit_data = FALSE
  # # parameter_subsets =  list(custom.f.KDN_UR = c(100, 300))

  #  = = = = = = = = = = = = = =
  # 0) Setup / utilities ####
  #  = = = = = = = = = = = = = =

  # suppressPackageStartupMessages({
  #   library(data.table)
  #   library(stringr)
  #   library(ggplot2)
  # })

  quiet_gc <- function() invisible(gc(FALSE, TRUE, TRUE))
  data.table::setDTthreads(max(parallel::detectCores() - 1L, 1L))

  print_density_distributions = FALSE

  # Capture arguments (small list; avoid copying via t()...t())
  arguments <- as.list(environment())

  # Sanitize inputs / local aliases
  bx.excluded               <- excluded_boxes
  dir.output                <- output_dir
  dir.sweep_space_digest    <- sweep_space_digest_folders
  dir.workdir               <- workdir
  file.observations         <- obs_file_name
  file.output               <- fit_name
  delta.ref                 <- delta_reference_box

  # Default output dir
  if (is.null(dir.output)) dir.output <- dir.sweep_space_digest

  # Ensure working directory restored when exiting
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(dir.workdir)

  # Validate delta.ref scalar
  if (length(delta.ref) > 1) stop("delta_reference_box length should be 1")

  # Auto fit_name
  if (is.null(file.output)) {
    file.output <- paste0(
      "CI_fit_",
      str_replace_all(Sys.time(), " ", "_") |>
        str_replace_all(":", "") |> str_replace_all("-", "")
    )
  }

  #  = = = = = = = = = = = = = =
  # 1) Read observations ####
  #  = = = = = = = = = = = = = =
  observations <- data.table::fread(file.observations, data.table = TRUE)
  # Expected columns (per original)
  expected_obs <- c("BOX_ID", "delta.def", "delta.ref", "obs.delta", "obs.CI", "obs.CI.def", "obs.file")
  if (!all(expected_obs %in% names(observations))) {
    stop("Headers in observations file must include: ", paste(expected_obs, collapse = ", "))
  }

  # Filter observations to the requested delta.ref (or mark as "NaN")
  if (!is.na(delta.ref) && !is.nan(delta.ref)) {
    if (!is.character(delta.ref)) stop("delta_reference_box must be a string when not NaN")
    observations <- observations[delta.ref == delta.ref]
    if (nrow(observations) == 0) stop("No observations for given delta.ref: ", delta.ref)
  } else {
    # observations[, delta.ref := "NaN"]
    data.table::set(observations, j = delta.ref, value = "NaN")
  }

  #  = = = = = = = = = = = = = =
  # 2) Read sweep-space digest ####
  #  = = = = = = = = = = = = = =
  # NOTE: Only processing the first digest folder (as per your active code path)
  if (length(dir.sweep_space_digest) < 1L)
    stop("sweep_space_digest_folders must contain at least one path")

  dir.sweep_space_digest.loc <- dir.sweep_space_digest[1]
  if (!dir.exists(dir.sweep_space_digest.loc))
    stop("Digest directory does not exist: ", dir.sweep_space_digest.loc)

  if (!stringr::str_ends(dir.sweep_space_digest.loc, "0_digest"))
    stop(dir.sweep_space_digest.loc, " must be a *0_digest directory")

  # Discover files
  file.sweep_std   <- normalizePath(file.path(dir.sweep_space_digest.loc,
                                              list.files(dir.sweep_space_digest.loc, pattern = "*_merged_results.RDS")))
  file.log         <- normalizePath(file.path(dir.sweep_space_digest.loc,
                                              list.files(dir.sweep_space_digest.loc, pattern = "*_merged_LOG.csv")))
  file.sweep_space <- normalizePath(file.path(dir.sweep_space_digest.loc,
                                              list.files(dir.sweep_space_digest.loc, pattern = "*_merged_param_space.RDS")))

  # Read wide simulation (standard) & swept parameters
  DF_wide <- readRDS(file.sweep_std)  # data.frame or data.table
  base::attributes(DF_wide)$out.attrs <- NULL
  data.table::setDT(DF_wide)                      # convert in-place

  # Convert character columns to factor in-place (no full copy)
  chr_cols <- names(DF_wide)[vapply(DF_wide, is.character, TRUE)]
  for (nm in chr_cols) data.table::set(DF_wide, j = nm, value = as.factor(DF_wide[[nm]]))

  # Read param space and keep only columns you later use
  sweeped_space <- readRDS(file.sweep_space)
  base::attributes(sweeped_space)$out.attrs <- NULL
  data.table::setDT(sweeped_space)

  data.table::setnames(sweeped_space, old = names(sweeped_space), new = paste0("swp.", names(sweeped_space)))
  keep_swp <- grep("^swp\\.(A|D|S|R|flux_list_name|coeff_list_name)", names(sweeped_space), value = TRUE)

  if (length(keep_swp)) {
    data.table::setDT(sweeped_space)
    DF_wide <- cbind(DF_wide, sweeped_space[, keep_swp, with = FALSE])
  }

  rm(sweeped_space); quiet_gc()

  # Parse LOG to identify box columns to melt
  LOG_SERIES <- data.table::fread(file.log, nrows = 1L, data.table = TRUE)
  bx.sim.all        <- unlist(stringr::str_split(LOG_SERIES[1, "BOXES_ID_list"], pattern = "_"))
  bx.sim.infinite   <- unlist(stringr::str_split(LOG_SERIES[1, "INFINITE_BOXES_list"], pattern = "_"))
  bx.sim.finite     <- base::setdiff(bx.sim.all, bx.sim.infinite)
  bx.sim.disconn    <- unlist(stringr::str_split(LOG_SERIES[1, "DISCONNECTED_BOXES"], pattern = "_"))
  bx.sim.connfinite <- base::setdiff(bx.sim.finite, bx.sim.disconn)
  bx.sim <- list(all = bx.sim.all,
                 infinite = bx.sim.infinite,
                 finite = bx.sim.finite,
                 disconnected = bx.sim.disconn,
                 connected.finite = bx.sim.connfinite)
  rm(LOG_SERIES, bx.sim.infinite, bx.sim.finite, bx.sim.disconn, bx.sim.connfinite); quiet_gc()

  # Optional cleaning hook (your helper)
  if (exists("clear_subset")) DF_wide <- clear_subset(DF_wide)

  #  = = = = = = = = = = = = = =
  # 3) Optional custom expressions (in-place) ####
  #  = = = = = = = = = = = = = =
  # Stable row id to match reference values by row
  data.table::setDT(DF_wide)  # optional: pre-allocate column slots
  # DF_wide[, rid := .I]
  data.table::set(DF_wide, j = "rid", value = seq_len(nrow(DF_wide)))

  custom.calculate <- function(df, col_name, expr){
    col_name <- paste0("custom.", col_name )
    df %>% dplyr::mutate({{col_name}} := rlang::eval_tidy(dplyr::enquo(expr), df))
  }

  if (!is.null(custom_expressions)){
    for (i in 1:length(custom_expressions)){
      DF_wide <- custom.calculate(df = DF_wide,
                                       col_name = custom_expressions[i],
                                       eval(parse(text=custom_expressions[i])))
    }
  }

  remove(custom.calculate); quiet_gc()

  #  = = = = = = = = = = = = = =
  # 4) Melt to long, then subtract delta.ref (no wide-wide copy) ####
  #  = = = = = = = = = = = = = =

  # Fetch vector of reference values once (if applicable)
  ref_vec <- NULL
  if (!is.na(delta.ref) && !is.nan(delta.ref)) {
    if (!delta.ref %in% names(DF_wide))
      stop("delta_reference_box '", delta.ref, "' not found among DF columns")
    ref_vec <- DF_wide[[delta.ref]]
  }

  # Melt ONLY the box columns; keep minimal id.vars to control size
  id_cols <- c("rid", "SERIES_RUN_ID", keep_swp, paste0("custom.",custom_expressions))
  DF_long <- data.table::melt(
    DF_wide,
    id.vars        = id_cols,
    measure.vars   = base::intersect(bx.sim$all, names(DF_wide)),
    variable.name  = "BOX_ID",
    value.name     = "sim.delta",
    variable.factor = FALSE
  )

  # Subtract reference by row (if provided)
  if (!is.null(ref_vec)) {
    data.table::set(DF_long, j = "sim.delta", value = DF_long$sim.delta - ref_vec[DF_long$rid])
    data.table::set(DF_long, j = "delta.ref", value = delta.ref)  # if you just want to keep it
  } else {
    data.table::set(DF_long, j = "delta.ref", value = rep("NaN", nrow(DF_long)))
  }

  # Wide object no longer needed
  DF_wide <- NULL; rm(DF_wide); quiet_gc()

  # = = = = = = = = = = = = = =
  # 5) Join observations (keyed join, no copies) ####
  #  = = = = = = = = = = = = = =

  data.table::setDT(observations)
  observations <- observations[, c("delta.def", "delta.ref", "BOX_ID", "obs.delta", "obs.CI", "obs.CI.def", "obs.file"), with = FALSE]

  DF_long <- merge(DF_long, observations,
                   by = c("delta.ref", "BOX_ID"),
                   all.x = TRUE, sort = FALSE)
  data.table::setDT(DF_long)

  #  = = = = = = = = = = = = = =
  # 6) Flags: targeted boxes, parameter subsets ####
  #  = = = = = = = = = = = = = =
  data.table::set(DF_long,  j = "in.boxes_to_fit", value = !(DF_long$BOX_ID %in% bx.excluded))

  # Parameter subset flags → all TRUE across interested subset columns

  if (!is.null(parameter_subsets)) {
    subset_cols <- character(0)

    for (nm in names(parameter_subsets)) {
      if (!nm %in% names(DF_long)) stop("Parameter '", nm, "' not found in DF_long")
      target <- parameter_subsets[[nm]]
      colnm  <- paste0("in.subset.", nm)

      # Compute logical vector
      if (is.character(target)) {
        val <- DF_long[[nm]] %in% target
      } else {
        lo <- min(target); hi <- max(target)
        val <- DF_long[[nm]] >= lo & DF_long[[nm]] <= hi
      }

      # Assign in-place with set()
      data.table::set(DF_long, j = colnm, value = val)

      subset_cols <- c(subset_cols, colnm)
    }

    # Final "all-subsets" flag
    data.table::setDT(DF_long)
    # subset_cols was built earlier (e.g., "in.subset.a", "in.subset.b", ...)

    if (length(subset_cols) == 0L) {
      all_subset <- rep(TRUE, nrow(DF_long))
    } else {
      # Ensure DF_long is a data.table; with=FALSE then returns a 2-D table
      data.table::setDT(DF_long)

      # Grab the logical columns as a data.table and make it a logical matrix
      subDT <- DF_long[, subset_cols, with = FALSE]
      # If some flags are NA, treat them as FALSE (so "all TRUE" really means all TRUE)
      for (j in seq_along(subset_cols)) {
        v <- subDT[[j]]
        v[is.na(v)] <- FALSE
        subDT[[j]] <- v
      }
      m <- as.matrix(subDT)              # now guaranteed 2-D
      all_subset <- rowSums(!m) == 0L    # TRUE only if all columns are TRUE
    }

    # write back without :=
    data.table::set(DF_long, j = "in.all.subset_param", value = all_subset)

    remove(all_subset); quiet_gc()
  } else {
    # If no subsets defined, just set TRUE for all rows
    data.table::set(DF_long, j = "in.all.subset_param", value = rep(TRUE, nrow(DF_long)))
  }

  ############################################################################################################

  #  = = = = = = = = = = = = = =
  # 7) CI window + fit selection ####
  #  = = = = = = = = = = = = = =

  ## A) Compute CI bounds (no :=)
  data.table::set(DF_long, j = "min.obs.delta",
                  value = DF_long$obs.delta - DF_long$obs.CI)
  data.table::set(DF_long, j = "max.obs.delta",
                  value = DF_long$obs.delta + DF_long$obs.CI)

  ## B) In-CI flag (no :=)
  data.table::set(DF_long, j = "in.obs.CI",
                  value = DF_long$sim.delta >= DF_long$min.obs.delta &
                    DF_long$sim.delta <= DF_long$max.obs.delta)

  # ## C) Target sets (unchanged logic)
  DF_long <- data.table::as.data.table(DF_long)
  data.table::setDT(DF_long)

  bx.targetted_initial <- sort(base::unique(DF_long[DF_long$in.boxes_to_fit == TRUE, "BOX_ID", with = FALSE]))
  bx.observed <- sort(base::unique(DF_long[!(is.na(DF_long$obs.delta) & is.na(DF_long$obs.CI)), "BOX_ID", with = FALSE]))
  bx.targetted_obs <- base::intersect(bx.targetted_initial, bx.observed)
  bx.targetted_no_obs <- base::setdiff(bx.targetted_initial, bx.observed)

  if (length(bx.targetted_no_obs)) {
    message("\u2757 Targeted boxes with missing observation removed: ",
            paste(bx.targetted_no_obs, collapse = ", "))
  }

  bx.targetted <- bx.targetted_obs

  ## D) Mark boxes with no obs as not targeted (no :=)
  idx_notarget <- which(DF_long$BOX_ID %in% bx.targetted_no_obs)

  if (length(idx_notarget)) {
    data.table::set(DF_long, i = idx_notarget, j = "in.boxes_to_fit", value = FALSE)
  }

  ## E) Count fitted boxes per run (unchanged)
  # ci_by_run <- cbind(data.table(SERIES_RUN_ID = DF_long[DF_long$in.obs.CI & DF_long$in.boxes_to_fit & DF_long$in.all.subset_param, "SERIES_RUN_ID", with = FALSE ]),
  #                    data.table::data.table(CI_fitted.boxes.n = data.table::uniqueN(DF_long$BOX_ID),
  #                                           CI_fitted.boxes.IDs = paste(sort(unique(DF_long$BOX_ID)), collapse = ", ")))
  # ci_by_run <-

  ci_by_run <-
    DF_long %>%
    filter(in.obs.CI & in.boxes_to_fit & in.all.subset_param) %>%
    group_by(SERIES_RUN_ID) %>%
    summarise(CI_fitted.boxes.n = n(),
              CI_fitted.boxes.IDs = paste(sort(unique(BOX_ID)), collapse = ", ")) %>%
    data.table::as.data.table()

  ## F) Choose best set (unchanged logic)
  best_n <- if (nrow(ci_by_run)) max(ci_by_run$CI_fitted.boxes.n) else 0L
  if (best_n == length(bx.targetted)) {
    bx.fit.successful  <- bx.targetted
    sel_runs           <- ci_by_run[ci_by_run$CI_fitted.boxes.n == best_n, "SERIES_RUN_ID", with = FALSE] %>% as.character()
    bx.CIfit_selected  <- NULL
    message("All targeted boxes successfully CI-fitted.")
  } else {

    if (nrow(ci_by_run) > 0) {

      top <- ci_by_run %>%
        group_by(CI_fitted.boxes.IDs) %>%
        summarise(CI_fitted.boxes.IDs = unique(CI_fitted.boxes.IDs),
                  CI_fitted.boxes.n = unique(CI_fitted.boxes.n),
                  n.fits = n()) %>%
        arrange(-CI_fitted.boxes.n)

      print(top %>% as.data.frame())

      bx.CIfit_selected <-
        utils::select.list(choices= top$CI_fitted.boxes.IDs,
                           preselect = 1,
                           title = paste("\U2757 You intend to fit the following boxes: ",
                                         paste(bx.targetted,
                                               collapse = ", "), "\n",
                                         "Only the combinations below simultaneously fall within observed confidence intervals.", "\n",
                                         "Run a new CI-fit with an updated list of boxes to be excluded from fit (excluded_boxes)", "\n",
                                         "or select the combination to be CI-fitted: ", sep = ""))

      bx.fit.successful <-  unlist(stringr::str_split(bx.CIfit_selected, pattern = ", "))

      print(paste0("Selected: ", paste0(bx.fit.successful, collapse = ", ")))

      sel_runs <- ci_by_run %>%
        filter(CI_fitted.boxes.IDs == paste0(bx.fit.successful, collapse = ", ")) %>%
        pull(SERIES_RUN_ID) %>%
        as.character()

    } else {
      rlang::abort("No fit.")
    }
  }

  bx.fit.failed <- base::setdiff(bx.targetted, bx.fit.successful)


  ## G) Tag rows belonging to selected runs (no :=)
  data.table::set(DF_long, j = "in.all_fit_boxes.obs.CI",
                  value = DF_long$SERIES_RUN_ID %in% sel_runs)

  #  = = = = = = = = = = = = = =
  # 8) SSR per run (for successful boxes) ####
  #  = = = = = = = = = = = = = =
  ## H) Compute run-level totals and attach (no := join update)
  if (length(bx.fit.successful) > 0) {
    SSR_by_run <-
      DF_long %>%
      filter(BOX_ID %in% bx.fit.successful) %>%
      group_by(SERIES_RUN_ID) %>%
      summarise(SSR = sum((sim.delta - obs.delta)^2)) %>% data.table::as.data.table()

    # Merge (keeps DF_long order; no keys required)
    DF_long <- merge(DF_long, SSR_by_run, by = "SERIES_RUN_ID", all.x = TRUE, sort = FALSE)

    remove(SSR_by_run); quiet_gc()

  }

  #  = = = = = = = = = = = = = =
  # 9) Prepare summaries / reports ####
  #  = = = = = = = = = = = = = =

  if (!exists("dec_n")) dec_n <- function(x, n) round(x, n)  # fallback

  report.sim_obs <- DF_long %>% as.data.frame() %>%
    filter(in.all_fit_boxes.obs.CI == TRUE & in.all.subset_param == TRUE) %>%
    group_by(BOX_ID, in.boxes_to_fit) %>%
    summarise(fitted.boxes = unique(in.boxes_to_fit),
              delta.def    = unique(delta.def),
              delta.ref    = unique(delta.ref),
              obs.delta    = suppressWarnings(mean(obs.delta, na.rm = TRUE)),
              obs.CI       = dec_n(mean(obs.CI, na.rm = TRUE), 3),
              sim_obs.abs_offset = dec_n(abs(mean(sim.delta, na.rm = TRUE) -
                                               suppressWarnings(mean(obs.delta, na.rm = TRUE))), 3),
              sim.n   = n(),
              sim.05  = dec_n(quantile(sim.delta, .05, na.rm = TRUE), 3),
              sim.25  = dec_n(quantile(sim.delta, .25, na.rm = TRUE), 3),
              sim.50  = dec_n(quantile(sim.delta, .50, na.rm = TRUE), 3),
              sim.75  = dec_n(quantile(sim.delta, .75, na.rm = TRUE), 3),
              sim.95  = dec_n(quantile(sim.delta, .95, na.rm = TRUE), 3),
              sim.min = dec_n(min(sim.delta, na.rm = TRUE), 3),
              sim.mean= dec_n(mean(sim.delta, na.rm = TRUE), 3),
              sim.max = dec_n(max(sim.delta, na.rm = TRUE), 3),
              sim.2sd = dec_n(2 * stats::sd(sim.delta, na.rm = TRUE), 3),
              sim.2se = dec_n(as.numeric(sim.2sd) / sqrt(sim.n), 4)) %>%
    arrange(sim_obs.abs_offset) %>% data.table::as.data.table()

  ## J) Add fitted-box flags (no :=)
  report.sim_obs$fitted.boxes <- FALSE
  if (length(bx.fit.successful)) {
    idx_true  <- which(report.sim_obs$BOX_ID %in% bx.fit.successful)
    if (length(idx_true))  report.sim_obs$fitted.boxes[idx_true]  <- TRUE
  }
  if (length(bx.fit.failed)) {
    idx_false <- which(report.sim_obs$BOX_ID %in% bx.fit.failed)
    if (length(idx_false)) report.sim_obs$fitted.boxes[idx_false] <- FALSE
  }

  ## L) Start Building data.report (unchanged aside from no :=)
  data.report <- list(
    sim_vs_obs  = cbind(fit_name = file.output, as.data.frame(report.sim_obs))
  )

  ################################################################################################################################################  PLOTS
  # Optional frequency / LS / density plots (computed -> printed/saved immediately)
  # Keep only small stats tables in memory to attach to data.report

  ## bx.fit ####
  bx.fit <- list(all = bx.sim$all,
                 observed = bx.observed,
                 targetted = bx.targetted,
                 targetted_initial = bx.targetted_initial,
                 targetted_obs = bx.targetted_obs,
                 targetted_no_obs = bx.targetted_no_obs,
                 not_targetted = setdiff(bx.sim$all, bx.targetted),
                 CI_fit.successful = bx.fit.successful,
                 CI_fit.failed = bx.fit.failed)



  ## fit.counts ####
  fit.counts <- NULL

  fit.counts$sims.all <- DF_long %>% dplyr::count(SERIES_RUN_ID) %>% nrow()

  fit.counts$sims.in.param_subsets <- DF_long %>%
    dplyr::filter(in.all.subset_param == TRUE) %>%
    dplyr::count(SERIES_RUN_ID) %>% nrow()

  fit.counts$sims.in.CIfit <- DF_long %>%
    dplyr::filter(in.all_fit_boxes.obs.CI == TRUE) %>% dplyr::count(SERIES_RUN_ID) %>% nrow()

  fit.counts$sims.in.CIfit.param_subsets <- DF_long %>%
    dplyr::filter(in.all_fit_boxes.obs.CI == TRUE,
                  in.all.subset_param == TRUE) %>% dplyr::count(SERIES_RUN_ID) %>% nrow()

  fit.counts$sim_delta.fitted_boxes <- DF_long %>%
    dplyr::filter(in.all_fit_boxes.obs.CI == TRUE,
                  in.all.subset_param == TRUE,
                  BOX_ID %in% bx.fit$CI_fit.successful
    ) %>% dplyr::count() %>% as.numeric()

  ## plot_freq_ADSR ####
  if (exists("plot_freq_ADSR")) {
    sweep_headers <- names(DF_long)[startsWith(names(DF_long), "swp.")]
    names.swp.ADSR <- c(
      sweep_headers[startsWith(sweep_headers, "swp.A.")],
      sweep_headers[startsWith(sweep_headers, "swp.D.")],
      sweep_headers[startsWith(sweep_headers, "swp.S.")],
      sweep_headers[startsWith(sweep_headers, "swp.R.")],
      names(DF_long)[startsWith(names(DF_long), "custom.")]
    )
  } else {
    names.swp.ADSR <- character(0)
  }

  if (length(names.swp.ADSR) != 0){
    plot.freq_ADSR <-
      plot_freq_ADSR(DF = as.data.frame(DF_long),
                     names.swp.ADSR = names.swp.ADSR,
                     parameter_subsets = parameter_subsets,
                     custom.n_bins = custom.n_bins)
    data.report$QuantPars_DescStats <- cbind(fit_name = file.output, plot.freq_ADSR$stats)
    data.report$QuantPars_FitFreqs  <- cbind(fit_name = file.output, plot.freq_ADSR$freqs)
  }

  ## plot_freq_flux ####
  names.swp.flux_list_name <- sweep_headers[stringr::str_starts(sweep_headers, pattern = "swp.flux_list_name")]
  if (length(names.swp.flux_list_name) != 0){
    DF.filter_loc <- DF_long[, c("in.all.subset_param", "in.all_fit_boxes.obs.CI", "swp.flux_list_name"), with = FALSE]
    plot.freq_flux <-
      plot_freq_flux(DF = DF.filter_loc,
                     parameter_subsets = parameter_subsets)
    data.report$FluxLists_FitFreqs <- cbind(fit_name = file.output, plot.freq_flux$freqs)
    remove(DF.filter_loc); quiet_gc()
  }

  # ## plot_freq_coeff ####
  names.swp.coeff_list_name <- sweep_headers[stringr::str_starts(sweep_headers, pattern = "swp.coeff_list_name")]
  if (length(names.swp.coeff_list_name) != 0){
    DF.filter_loc <-  DF_long[, c("swp.coeff_list_name", "in.all_fit_boxes.obs.CI", "in.subset.swp.coeff_list_name"), with = FALSE]
    plot.freq_coeff <- plot_freq_coeff(DF = DF.filter_loc,
                                       parameter_subsets = parameter_subsets)
    data.report$CoeffLists_FitFreqs <- cbind(fit_name = file.output, plot.freq_coeff$freqs)
    remove(DF.filter_loc); quiet_gc()
  }

  ## plot_sim_obs ####
  DF.filter_loc <- DF_long[, c("in.all_fit_boxes.obs.CI", "in.all.subset_param",
                               "SERIES_RUN_ID", "BOX_ID", "in.boxes_to_fit", "obs.delta", "obs.CI", "sim.delta")]
  plot.sim_obs <- plot_sim_obs(DF.filter_loc,
                               fit.counts = fit.counts,
                               bx.fit = bx.fit)
  remove(DF.filter_loc); quiet(gc())


  ## plot_sim_distrib #### MUTED FOR NOW
  if (print_density_distributions && exists("plot_sim_distrib")) {
    plot.sim_distrib <- plot_sim_distrib(
      as.data.frame(DF_long),
      bx.fit = bx.fit,
      observations =  observations,
      fit.counts = fit.counts)
    quiet_gc()
  }

  ## plot_LS_surfaces_ADSR ####
  names.swp.lists <- c(names.swp.flux_list_name,
                       names.swp.coeff_list_name)
  if (print_LS_surfaces){
    ### a. plot qp least squares surfaces
    if (length(names.swp.ADSR) != 0) plots.LS_surfaces_ADSR <- plot_LS_surfaces_ADSR(DF_long, names.swp.ADSR)
    ### b. plot surface lists least square
    if (length(names.swp.lists) != 0)  plots.LS_surfaces_lists <- plot_LS_surfaces_lists(DF_long, names.swp.lists)
  }

  ## plot_correlogram_all_CIfit ####
  if(length(names.swp.ADSR) > 0 & print_correlogram){
    #### _ a. correlogram for least squares surfaces only MUTED
    # plot.correlogram.CI_fit.LS <- plot_correlogram_LS_surfaces(DF, names.swp.ADSR)
    #### _ b. correlogram over full CI-fit
    plot.correlogram.CI_fit.all <- plot_correlogram_all_CIfit(DF_long, names.swp.ADSR)
  }

  ## plot.lda ####
  if (print_lda) plot.lda <- plot_lda(DF_long, names.swp.ADSR)

  #  = = = = = = = = = = = = = =
  # 10) Save / export (memory friendly) ####
  #  = = = = = = = = = = = = = =
  if (save_outputs && !dir.exists(dir.output)) dir.create(dir.output, recursive = TRUE)

  ### (A) Save report ####
  data.report$arguments <-
    arguments %>% t() %>%
    as.data.frame() %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>% t() %>%
    as.data.frame() %>%
    rename(value = V1)

  data.report$arguments <- data.report$arguments %>%
    mutate(argument = rownames(data.report$arguments), .before = "value") %>%
    clear_subset()

  if (save_outputs){
    writexl::write_xlsx(data.report, paste0(dir.output, "/", file.output, "_data_report.xlsx"))
  }

  ### (B) Export fitted data (optional)—use RDS (compact) or Feather ####
  if (export_fit_data && save_outputs) {
    fitted_rows <- DF_long[DF_long$in.all_fit_boxes.obs.CI == TRUE & DF_long$in.all.subset_param == TRUE, ]
    saveRDS(list(fit_name = file.output, fitted_data = fitted_rows),
            file.path(dir.output, paste0(file.output, "_CI_fit_data.RDS")))
    remove(fitted_rows); quiet_gc()
  }

  ### (C) Figures: print and discard sequentially ####
  if (save_outputs) {

      suppressWarnings({
      suppressMessages({

        if (save_outputs){
          pdf(width = 21/2.54, height = 29.7/2.54,
              file = paste0(dir.output, "/", file.output, ".pdf"))
        }

        #### ____ p1 : sim_obs

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

        #### ____ p2 : frequency plots
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
                                    # labels = c("A", "B"),
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

        #### ____ p3 : simulation distribution plot

        if (all(c("plot.sim_distrib") %in% ls())){
          page_title <- "Distributions of simulated isotope compositions"
          gridExtra::grid.arrange(top = grid::textGrob( page_title,
                                                        gp = grid::gpar(fontface = "bold", col = message_color)),
                                  plot.sim_distrib
                                  # gridExtra::arrangeGrob(plot.sim_distrib$all,
                                  #             plot.sim_distrib$CI_fit, ncol = 1)
          )
        }

        #### ____ p4a-b : qp surface plots / all and CI
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

        #### ____ p5 : CI-fit surface correlogram / DEACTIVATED
        # if (all(c("plot.correlogram.CI_fit.LS") %in% ls())){
        #   print(plot.correlogram.CI_fit.LS +
        #           ggplot2::labs(title = "Least square surfaces correlogram (all CI-fitted quantitative parameters)"))
        # }

        #### ____ p6a-b : correlogram
        if (all(c("plot.correlogram.CI_fit.all") %in% ls())){
          print(plot.correlogram.CI_fit.all +
                  ggplot2::labs(title = "Full correlogram (all CI-fitted quantitative parameters)"))
        }

        if (all(c("plot.") %in% ls())) {
          gridExtra::grid.arrange(top = grid::textGrob( "Linear discriminant analysis (all CI-fitted quantitative parameters)",
                                                        gp = grid::gpar(fontface = "bold")),
                                  plot.lda)
        }

        if (save_outputs){
          dev.off()
        }

      })
    })

  }

  #  = = = = = = = = = = = = = =
  ## 11) Return
  #  = = = = = = = = = = = = = =
  if (!save_outputs) {
    return(data.report)                        # light summary when not saving
  } else {
    invisible(NULL)                            # outputs written to disk
  }

}
