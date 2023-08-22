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
#'
#' @return A observation fit graphical report, in R session or exported as pdf, and a data report as R list or xlsx if required.
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
                            parameter_subsets = NULL,
                            custom_expressions = NULL,
                            save_outputs = FALSE,
                            export_fit_data = FALSE){

  # # ###### 0. clear workspace ######
  # if(!is.null(dev.list())) dev.off()
  # ## Clear console
  # ## cat("\014")
  # ## Clean workspace
  # rm(list=ls())
  # gc()
  #
  # # # ###### 0b. debug arguments ######
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
  #
  # workdir <- workdir_ABCD <- "/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/1_ABCD_dev"
  # obs_file_name = "observations_SWEEP_FINnD_demo_true_final.csv"
  # sweep_space_digest_folders = "4_FINnD_0_SWEEP_FINnD_demo_001_000_digest"
  # fit_name = NULL
  # output_dir = NULL
  # delta_reference_box = NaN
  # excluded_boxes = c("SINK")
  # print_correlogram = TRUE
  # print_lda = FALSE
  # print_LS_surfaces = TRUE
  # parameter_subsets = NULL
  # custom_expressions = NULL
  # save_outputs = FALSE
  # export_fit_data = FALSE

  ###### 1. initiate ######
  # source("/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/0_sweep_space_functions/fit_sweep_space_plot_fns.R")

  # store arguments
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

  # rename arguments
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

  SERIES_RUN_ID <- sim.delta <- min.obs.delta <-  max.obs.delta <- in.obs.CI <-
    in.boxes_to_fit <-  BOX_ID <- CI_fitted.boxes.n <- CI_fitted.boxes.IDs <-
    in.all.subset_param <-  in.all_fit_boxes.obs.CI <-  obs.delta <-  delta.def <-
    obs.CI <- sim_obs.abs_offset <- NULL

  # turn custom_expressions strings into relation calls
  custom.calculate <- function(df, col_name, expr){
    col_name <- paste0("custom.", col_name )
    df %>% dplyr::mutate({{col_name}} := rlang::eval_tidy(dplyr::enquo(expr), df))
  }

  ##### _ b. set wd  ######
  dir.old <- getwd()
  on.exit(setwd(dir.old), add = TRUE)
  setwd(dir.workdir)

  ##### _ c. argument checkup ######
  if (length(delta.ref) >1){rlang::abort("delta.ref length should be one.")}

  ##### _ d. define output paths ######

  if(is.null(file.output)){
    file.output <- paste0("CI_fit_",
                          Sys.time() %>%
                            stringr::str_replace_all(pattern = " ", replacement = "_") %>%
                            stringr::str_replace_all(pattern = ":", replacement = "") %>%
                            stringr::str_replace_all(pattern = "-", replacement = "")
    )
  }

  ##### 2. import / format data #####
  ##### _ a. import observations for delta.ref######
  observations <- data.table::fread(file = file.observations, stringsAsFactors = T, data.table = F)
  names.obs <- c("BOX_ID", "delta.def", "delta.ref", "obs.delta", "obs.CI", "obs.CI.def", "obs.file")
  if(!all(names(observations) %in% names.obs)){
    rlang::abort(paste0("Headers in observations file do not contain expected names: ",
                        paste(names.obs, collapse = ", ")))
  }
  observations <- observations[observations$delta.ref == as.character(delta.ref), ]
  if (nrow(observations) == 0){
    rlang::abort(paste("observations file is empty for given delta.ref: " , delta.ref))
  }
  names(observations)[names(observations) %in% c("AV")] <- c("obs.delta")
  remove(file.observations)

  ##### _ b. import sweep_space sims ######
  # optional for later release: merging multiple sweeped spaces.
  # would require verification of compatibility
  # (same system, same sets of parameters, only different resolutions and ranges)
  # for (j in 1:length(dir.sweep_space_digest)){ # later release possible

  j <- 1
  dir.sweep_space_digest.loc <- dir.sweep_space_digest[j]
  if (!dir.exists(dir.sweep_space_digest.loc)) rlang::abort("dir.sweep_space_digest.loc not found.")

  if(stringr::str_ends(dir.sweep_space_digest.loc, pattern = "0_digest")){
    file.sweep_std <- normalizePath(paste0(dir.sweep_space_digest.loc, "/",
                                           list.files(dir.sweep_space_digest.loc,
                                                      pattern = "*_merged_results.RDS")))
    file.log <- normalizePath(paste0(dir.sweep_space_digest.loc, "/",
                                     list.files(dir.sweep_space_digest.loc,
                                                pattern = "*_merged_LOG.csv")))
    file.sweep_space <- normalizePath(paste0(dir.sweep_space_digest.loc, "/",
                                             list.files(dir.sweep_space_digest.loc,
                                                        pattern = "*_merged_param_space.RDS")))
  } else {
    rlang::abort(paste(dir.sweep_space_digest.loc, "directory does not exist."))
  }

  DF.loc <- readRDS(file = file.sweep_std)
  DF.loc[sapply(DF.loc, is.character)] <- lapply(DF.loc[sapply(DF.loc, is.character)], as.factor)
  sweeped_space <- readRDS(file = file.sweep_space)
  names(sweeped_space) <- paste0("swp.", names(sweeped_space))
  DF.loc <- cbind(DF.loc, sweeped_space)

  LOG_SERIES <- data.table::fread(file.log, nrows = 1)
  bx.sim.all <- unlist(stringr::str_split(LOG_SERIES[1,"BOXES_ID_list"], pattern = "_"))
  bx.sim.infinite <- unlist(stringr::str_split(LOG_SERIES[1,"INFINITE_BOXES_list"], pattern = "_"))
  bx.sim.finite <- bx.sim.all[!bx.sim.all %in% bx.sim.infinite]
  bx.sim.disconnected <- unlist(stringr::str_split(LOG_SERIES[1,"DISCONNECTED_BOXES"], pattern = "_"))
  bx.sim.connected_finite <- bx.sim.finite[!bx.sim.finite %in% bx.sim.disconnected]
  bx.sim <- list(all = bx.sim.all,
                 infinite = bx.sim.infinite,
                 finite = bx.sim.finite,
                 disconnected = bx.sim.disconnected,
                 connected.finite = bx.sim.connected_finite)
  remove(LOG_SERIES,
         bx.sim.all, bx.sim.infinite, bx.sim.finite,
         bx.sim.disconnected, bx.sim.connected_finite)

  DF.loc <- clear_subset(DF.loc)
  DF.loc.vert <- DF.loc %>%
    reshape2::melt(id = names(DF.loc)[!names(DF.loc) %in% bx.sim$all ],
                   variable.name = "BOX_ID",
                   value.name = "sim.delta")
  DF.loc.vert$delta.ref <- as.character("NaN")

  if (!is.null(delta.ref) & !is.na(delta.ref) & !is.nan(delta.ref)){

    if (!is.character(delta.ref)) rlang::abort("delta.ref should be a string.")

    NORM.DF.loc <- DF.loc
    NORM.DF.loc[, bx.sim$all] <- DF.loc[, bx.sim$all] - DF.loc[ , delta.ref]
    NORM.DF.loc.vert <- NORM.DF.loc %>%
      reshape2::melt(id = names(NORM.DF.loc)[!names(NORM.DF.loc) %in% bx.sim$all ],
                     variable.name = "BOX_ID",
                     value.name = "sim.delta")
    NORM.DF.loc.vert$delta.ref <- delta.ref
    DF.loc.vert <- dplyr::bind_rows(DF.loc.vert, NORM.DF.loc.vert)
    remove(NORM.DF.loc, NORM.DF.loc.vert)
  }

  # if (j == 1){
  DF <- DF.loc.vert
  # } else {
  #   DF <- dplyr::bind_rows(DF, DF.loc.vert)
  # }
  remove(j, DF.loc, DF.loc.vert, sweeped_space,
         file.log, file.sweep_space, file.sweep_std,
         dir.sweep_space_digest.loc)
  # }

  ##### _ d. keep existing BOXES & matching delta.ref ######
  DF <- clear_subset(DF[DF$BOX_ID %in% bx.sim$all & DF$delta.ref == delta.ref ,])
  quiet(gc())

  ##### _ c. run custom calculations ######
  if (!is.null(custom_expressions)){
    for (i in 1:length(custom_expressions)){
      DF <- custom.calculate(df = DF,
                             col_name = custom_expressions[i],
                             eval(parse(text=custom_expressions[i])))
    }
  }

  remove(custom.calculate, custom_expressions)
  # DF <- DF %>% dplyr::mutate(dplyr::across(tidyselect::where(is.character), factor))
  DF <- DF %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)


  ##### _ f. subset parameters ######
  names.parameter_subsets <- names(parameter_subsets)

  if (!is.null(parameter_subsets)){
    for (i in 1:length(parameter_subsets)){
      name.parameter_subsets.loc <- names.parameter_subsets[i]
      parameter_subsets.loc <- parameter_subsets[[i]]
      if (name.parameter_subsets.loc %in% names(DF)){
        DF$subset_param <- DF[ , name.parameter_subsets.loc]
        DF$subset_param.in_range <- FALSE
        colname_subset_param.in_range <- paste0("in.subset.", name.parameter_subsets.loc)
        names(DF)[names(DF) == "subset_param.in_range"] <- colname_subset_param.in_range
        if (is.character(parameter_subsets.loc)){
          if (nrow(DF[DF$subset_param %in% parameter_subsets.loc, ]) == 0){
            rlang::abort(paste0("Subset for ", name.parameter_subsets.loc, " is out of bound."))
          } else {
            DF[DF$subset_param %in% parameter_subsets.loc, colname_subset_param.in_range] <- TRUE
          }
        } else if (is.double(parameter_subsets.loc)){
          if (nrow(DF[DF$subset_param > min(parameter_subsets.loc) & DF$subset_param < max(parameter_subsets.loc),]) == 0){
            rlang::abort(paste0("Subset for ", name.parameter_subsets.loc, " is out of bound."))
          } else {
            DF[DF$subset_param >= min(parameter_subsets.loc) &DF$subset_param <= max(parameter_subsets.loc),
               colname_subset_param.in_range] <- TRUE
          }
        }
        DF <- DF[,!names(DF) %in% c("subset_param")]
      } else {
        rlang::abort(paste("The ", names(parameter_subsets.loc),
                           " parameter_subsets name was not found in current sweeped space."))
      }
    }
    remove(parameter_subsets.loc, name.parameter_subsets.loc,
           # names.parameter_subsets,
           colname_subset_param.in_range)
  }

  DF$in.all.subset_param <- FALSE
  in.all.subset_param.SERIES_RUN_ID <- DF %>%
    dplyr::filter(dplyr::if_all(dplyr::starts_with("in.subset."), ~ .x == TRUE)) %>%
    dplyr::select(SERIES_RUN_ID) %>%
    as.vector() %>%
    unlist() %>%
    as.character()

  if (nrow(DF[DF$SERIES_RUN_ID %in% in.all.subset_param.SERIES_RUN_ID, ]) == 0){
    rlang::abort("No intersection between parameter subsets within CI fitted runs. \n Redefine parameter_subsets.")
  } else {
    DF[DF$SERIES_RUN_ID %in% in.all.subset_param.SERIES_RUN_ID, "in.all.subset_param"] <- TRUE
  }

  remove(in.all.subset_param.SERIES_RUN_ID)
  quiet(gc())

  ##### _ g. merge sim and obs DFs ######
  if (is.nan(delta.ref)){
    DF$delta.ref <- "NaN"
    observations$delta.ref <- "NaN"
  }
  DF <- dplyr::full_join(DF, observations, by = c("delta.ref", "BOX_ID"))
  remove(delta.ref)

  ##### _ h. define in_out boxes to fit ######
  DF$in.boxes_to_fit <- FALSE
  DF[!(DF$BOX_ID %in% bx.excluded), "in.boxes_to_fit"] <- TRUE
  # DF %>% group_by(BOX_ID, in.boxes_to_fit) %>% count() %>%  arrange(in.boxes_to_fit) # check

  bx.targetted_initial <- bx.targetted <- DF[DF$in.boxes_to_fit == TRUE, "BOX_ID"] %>%
    unique() %>%
    as.character() %>%
    sort()

  bx.observed <- observations[!(is.na(observations$obs.delta) & is.na(observations$obs.CI)), "BOX_ID"] %>%
    unique() %>%
    as.character() %>%
    sort()

  bx.targetted_obs <- bx.targetted[bx.targetted %in% bx.observed]
  bx.targetted_no_obs <- bx.targetted[!bx.targetted %in% bx.observed]

  if (length(bx.targetted_no_obs != 0)){
    rlang::inform(paste0("\U2757 The following boxes are targetted for CI-fit but observation is missing. ", "\n",
                         "   They will be removed from targetted boxes.", "\n   ",
                         paste(bx.targetted_no_obs, collapse = ", ")))
    bx.targetted <- bx.targetted_obs
    DF[(DF$BOX_ID %in% bx.targetted_no_obs), "in.boxes_to_fit"] <- FALSE
  }

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

  ##### _ i. obs.CI fit ######
  DF$min.obs.delta <- DF$obs.delta-DF$obs.CI
  DF$max.obs.delta <- DF$obs.delta+DF$obs.CI
  DF <- DF %>% dplyr::mutate(in.obs.CI = sim.delta >= min.obs.delta & sim.delta <= max.obs.delta) %>% as.data.frame()
  SERIES_RUN_ID.in <- DF %>%
    dplyr::filter(in.obs.CI == TRUE) %>%
    dplyr::filter(in.boxes_to_fit == TRUE) %>%
    dplyr::group_by(SERIES_RUN_ID) %>%
    dplyr::summarise(CI_fitted.boxes.n = dplyr::n(),
                     CI_fitted.boxes.IDs = paste(sort(as.character(unique(BOX_ID))), collapse = ", "),
                     .groups = "drop_last") %>%
    as.data.frame()
  SERIES_RUN_ID.in <- SERIES_RUN_ID.in[order(SERIES_RUN_ID.in$CI_fitted.boxes.n, decreasing = T),]
  SERIES_RUN_ID.all <- DF %>% dplyr::count(SERIES_RUN_ID)
  CI_fits.box_combinations <- SERIES_RUN_ID.in %>% dplyr::count(CI_fitted.boxes.n, CI_fitted.boxes.IDs) %>% dplyr::arrange(-CI_fitted.boxes.n)
  CI_fits.box_combinations$index <- rownames(CI_fits.box_combinations)

  if (max(CI_fits.box_combinations$CI_fitted.boxes.n) == length(as.character(unique(DF[DF$in.boxes_to_fit == TRUE, "BOX_ID"])))){
    bx.fit$CI_fit.successful <-  sort(as.character(unique(DF[DF$in.boxes_to_fit == TRUE, "BOX_ID"])))
    report.CIfit <- paste0("CI-Fitted: ", paste(bx.fit$CI_fit.successful, collapse = ", "))
    SERIES_RUN_ID.in.all_boxes_in <-
      as.character(SERIES_RUN_ID.in[SERIES_RUN_ID.in$CI_fitted.boxes.n ==
                                      max(SERIES_RUN_ID.in$CI_fitted.boxes.n, na.rm = T),
                                    "SERIES_RUN_ID"])
    bx.CIfit_selected <- NULL
  } else {
    bx.CIfit_selected <-
      utils::select.list(choices= CI_fits.box_combinations$CI_fitted.boxes.IDs,
                         preselect = 1,
                         title = paste("\U2757 You intend to fit the following boxes: ",
                                       paste(bx.fit$targetted,
                                             collapse = ", "), "\n",
                                       "Only the combinations below simultaneously fall within observed confidence intervals.", "\n",
                                       "Run a new CI-fit with an updated list of boxes to be excluded from fit (excluded_boxes)", "\n",
                                       "or select the combination to be CI-fitted: ", sep = ""))
    bx.fit$CI_fit.successful <-  unlist(stringr::str_split(bx.CIfit_selected, pattern = ", "))
    SERIES_RUN_ID.in.all_boxes_in <-
      as.character(SERIES_RUN_ID.in[SERIES_RUN_ID.in$CI_fitted.boxes.IDs == bx.CIfit_selected, "SERIES_RUN_ID"])
    report.CIfit <- paste0("CI-Fitted: ", bx.CIfit_selected)
  }

  bx.fit$CI_fit.failed <- bx.fit$targetted[!bx.fit$targetted %in% bx.fit$CI_fit.successful]
  DF$in.all_fit_boxes.obs.CI <- FALSE
  DF[DF$SERIES_RUN_ID %in% SERIES_RUN_ID.in.all_boxes_in, "in.all_fit_boxes.obs.CI"] <- TRUE
  n_CI_fitted_runs <- length(SERIES_RUN_ID.in.all_boxes_in)

  gc()

  fit.counts <- NULL

  fit.counts$sims.all <- DF %>% dplyr::count(SERIES_RUN_ID) %>% nrow()
    # dplyr::select(SERIES_RUN_ID) %>%
    # unique() %>%
    # nrow()

  fit.counts$sims.in.param_subsets <- DF %>%
    dplyr::filter(in.all.subset_param == TRUE) %>%
    dplyr::count(SERIES_RUN_ID) %>% nrow()
    # dplyr::select(SERIES_RUN_ID) %>%
    # unique() %>%
    # nrow()

  fit.counts$sims.in.CIfit = DF %>%
    dplyr::filter(in.all_fit_boxes.obs.CI == TRUE) %>% dplyr::count(SERIES_RUN_ID) %>% nrow()
    # dplyr::select(SERIES_RUN_ID) %>%
    # unique() %>%
    # nrow()

  fit.counts$sims.in.CIfit.param_subsets = DF %>%
    dplyr::filter(in.all_fit_boxes.obs.CI == TRUE,
                  in.all.subset_param == TRUE) %>% dplyr::count(SERIES_RUN_ID) %>% nrow()
    # dplyr::select(SERIES_RUN_ID) %>%
    # unique() %>%
    # nrow()

  fit.counts$sim_delta.fitted_boxes = DF %>%
    dplyr::filter(in.all_fit_boxes.obs.CI == TRUE,
                  in.all.subset_param == TRUE,
                  BOX_ID %in% bx.fit$CI_fit.successful
    ) %>% dplyr::count() %>% as.numeric()

  # fit.counts <- list(sims.all =
  #                      DF %>%
  #                      dplyr::select(SERIES_RUN_ID) %>%
  #                      unique() %>%
  #                      nrow() ,
  #
  #                    sims.in.param_subsets = DF %>%
  #                      dplyr::filter(in.all.subset_param == TRUE) %>%
  #                      dplyr::select(SERIES_RUN_ID) %>%
  #                      unique() %>%
  #                      nrow() ,
  #
  #                    sims.in.CIfit = DF %>%
  #                      dplyr::filter(in.all_fit_boxes.obs.CI == TRUE) %>%
  #                      dplyr::select(SERIES_RUN_ID) %>%
  #                      unique() %>%
  #                      nrow() ,
  #
  #                    sims.in.CIfit.param_subsets = DF %>%
  #                      dplyr::filter(in.all_fit_boxes.obs.CI == TRUE,
  #                                    in.all.subset_param == TRUE) %>%
  #                      dplyr::select(SERIES_RUN_ID) %>%
  #                      unique() %>%
  #                      nrow() ,
  #
  #                    sim_delta.fitted_boxes = DF %>%
  #                      dplyr::filter(in.all_fit_boxes.obs.CI == TRUE,
  #                                    in.all.subset_param == TRUE,
  #                                    BOX_ID %in% bx.fit$CI_fit.successful
  #                      ) %>% dplyr::count() %>% as.numeric()
  # )

  remove(CI_fits.box_combinations, SERIES_RUN_ID.all, SERIES_RUN_ID.in, SERIES_RUN_ID.in.all_boxes_in)
  quiet(gc())

  #   DF %>%
  #     group_by(SERIES_RUN_ID, in.all_fit_boxes.obs.CI) %>%
  #     count() %>%
  #     group_by(in.all_fit_boxes.obs.CI) %>%
  #     count()

  ##### _ j. calculate SSR: sum of squared residuals ######
  # does this belong here really?  / shouldn't we only calculate SSR for CI-fitted? or on contrary all targetted_box?
  DF_fitness_stats <- DF[DF$BOX_ID %in% bx.fit$CI_fit.successful, ] %>%
    dplyr::group_by(SERIES_RUN_ID) %>%
    dplyr::summarise(SSR = sum((sim.delta-obs.delta)**2), .groups = "drop_last") %>%
    as.data.frame()

  DF <- dplyr::full_join(DF, DF_fitness_stats, by = "SERIES_RUN_ID")
  remove(DF_fitness_stats)
  quiet(gc())

  ##### _ k. read swp parameter names ######
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

  ############################################################ PROGRESS #####
  ##### 3. analyse fitted parameter frequencies #####
  ##### _ a. fitted quantitative parameters frequencies #####
  if (length(names.swp.ADSR) != 0){
    plot.freq_ADSR <- plot_freq_ADSR(DF = DF,
                                     names.swp.ADSR = names.swp.ADSR,
                                     parameter_subsets = parameter_subsets)
  }

  ##### _ b. plot flux series CI fit frequencies ####

  if (length(names.swp.flux_list_name) != 0){
    plot.freq_flux <- plot_freq_flux(DF = DF,
                                     parameter_subsets = parameter_subsets)
  }

  ##### _ c. plot coeff series CI fit frequencies ####
  if (length(names.swp.coeff_list_name) != 0){
    plot.freq_coeff <- plot_freq_coeff(DF = DF,
                                       parameter_subsets = parameter_subsets)
  }

  ##### 4. plot fitted simulations against observations ####

  plot.sim_obs <- plot_sim_obs(DF, bx.fit, fit.counts)
  quiet(gc())

  plot.sim_distrib <- plot_sim_distrib(DF, fit.counts, observations, bx.fit)
  quiet(gc())

  #### 5. analyse least squares surfaces ####
  if (print_LS_surfaces){
    #### _ a. plot qp least squares surfaces   ####
    if (length(names.swp.ADSR) != 0) plots.LS_surfaces_ADSR <- plot_LS_surfaces_ADSR(DF, names.swp.ADSR)
    #### _ b. plot surface lists least square  ####
    if (length(names.swp.lists) != 0)  plots.LS_surfaces_lists <- plot_LS_surfaces_lists(DF, names.swp.lists)
  }

  #### 6. Analyse structure of fitted quantitative parameters ####
  if(length(names.swp.ADSR)>0 & print_correlogram){
    # #### _ a. correlogram for least squares surfaces only MUTED ####
    # plot.correlogram.CI_fit.LS <- plot_correlogram_LS_surfaces(DF, names.swp.ADSR)
    #### _ b. correlogram over full CI-fit ####
    plot.correlogram.CI_fit.all <- plot_correlogram_all_CIfit(DF, names.swp.ADSR)
  }

  #### _ c. lda analysis ####
  if (print_lda) plot.lda <- plot_lda(DF, names.swp.ADSR)

  #### 7. final report #####
  n_digits <- 3

  #### _ a. prepare data report #####

  #### ___ i. report sim_obs ####
  report.sim_obs <- DF[DF$in.all_fit_boxes.obs.CI == "TRUE" &
                         DF$in.all.subset_param == TRUE, ] %>%
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

  report.sim_obs$fitted.boxes <- NA
  report.sim_obs[report.sim_obs$BOX_ID %in% bx.fit$CI_fit.successful, "fitted.boxes"] <- TRUE
  report.sim_obs[report.sim_obs$BOX_ID %in% bx.fit$CI_fit.failed, "fitted.boxes"] <- FALSE

  report.sim_obs$sim.2se <- dec_n(report.sim_obs$sim.2sd/sqrt(report.sim_obs$sim.n),n_digits)
  report.sim_obs$sim.2sd <- dec_n(report.sim_obs$sim.2sd,n_digits)

  #### ___ ii. report param distributions ####
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

  #### _ b. print / export #####
  #### ___ i. export data_report ####
  if (save_outputs & !dir.exists(dir.output)) dir.create(dir.output)
  if (save_outputs){
    writexl::write_xlsx(data.report, paste0(dir.output, "/", file.output, "_data_report.xlsx"))
  }

  if (export_fit_data){
    data.report$fitted_data <- cbind(fit_name = file.output,
                                     DF[DF$in.all_fit_boxes.obs.CI == "TRUE" &  DF$in.all.subset_param == TRUE, ])


    saveRDS(data.report,
            paste0(dir.output, "/", file.output, "_CI_fit_data.RDS"))
  }

  #### ___ ii. export figure_report ####
  suppressWarnings({
    suppressMessages({

      if (save_outputs){
        pdf(width = 21/2.54, height = 29.7/2.54,
            file = paste0(dir.output, "/", file.output, ".pdf"))
      }

      #### ____ p1 : sim_obs ####
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

      #### ____ p2 : frequency plots ####
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

      #### ____ p3 : simulation distribution plot ####
      page_title <- "Distributions of simulated isotope compositions"
      gridExtra::grid.arrange(top = grid::textGrob( page_title,
                                                    gp = grid::gpar(fontface = "bold", col = message_color)),
                              plot.sim_distrib
                              # gridExtra::arrangeGrob(plot.sim_distrib$all,
                              #             plot.sim_distrib$CI_fit, ncol = 1)
      )


      #### ____ p4a-b : qp surface plots / all and CI ####
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

      #### ____ p5 : CI-fit surface correlogram / DEACTIVATED ####
      # if (all(c("plot.correlogram.CI_fit.LS") %in% ls())){
      #   print(plot.correlogram.CI_fit.LS +
      #           ggplot2::labs(title = "Least square surfaces correlogram (all CI-fitted quantitative parameters)"))
      # }

      #### ____ p6a-b : correlogram / lda ####
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
        dev.off()
      }

    })
  })

  remove(DF)
  quiet(gc())

  if (!save_outputs){
    return(data.report)
  }

}
