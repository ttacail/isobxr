#  #_________________________________________________________________________80char
#' Simulate a scenario
#' @description  A function to compose an isotope box model scenario,
#' defined by a series of successive runs, \cr
#' each run inheriting from the final state conditions of the previous run. \cr
#' It is possible to force parameters at each run, namely:
#' \enumerate{
#' \item \strong{fluxes} \cr
#' (overwriting all or a subset of fluxes defined in \emph{0_ISOBXR_MASTER.xlsx} master file)
#' \item \strong{isotope fractionation coefficients} \cr
#' (overwriting all or a subset of coefficients defined in \emph{0_ISOBXR_MASTER.xlsx} master file)
#' \item \strong{box sizes} \cr
#' (overwriting all or a subset of box sizes defined in \emph{0_ISOBXR_MASTER.xlsx} master file)
#' \item \strong{rayleigh isotope distillation}
#' \item \strong{isotope composition of a source box at initial state}
#' }
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' @param SERIES_ID Name of the series the scenario run belongs to.
#' It determines the folder in which the output files will be stored inside workdir.
#' @param scenario_master_file Name of \strong{\emph{scenario excel master file}}.
#' @param isobxr_master_file Name of \strong{\emph{isobxr excel master file}}. \cr
#' Default is "0_ISOBXR_MASTER".
#' @param plot.hidden_boxes list of box names (BOX_ID) to hide in scenario plot.
#' @param plot.time_unit Time unit to use on plot if different from native time unit. \cr
#' Character string, to be selected among the following:\cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}\cr
#' Default is NULL.
#' @param export.single_run_digests If TRUE, exports full digest of each single run
#' of the scenario.
#' Default is FALSE.
#' @param export.data_as_csv_xlsx If TRUE, exports full scenario result data as csv and xlsx fo full
#' to scenario digest directory. \cr
#' Default is FALSE.
#' @param show.delta_plot If TRUE, prints delta and size time evolution plots in R. \cr
#' Default is TRUE.
#' @param save_outputs If TRUE, saves all run outputs to local working directory (workdir). \cr
#' By default, run outputs are stored in a temporary directory and erased if not saved. \cr
#' Default is FALSE.
#' @param inspect_inputs If TRUE, inspects and proof checks format of input taken from
#' \strong{\emph{isobxr excel master file}}. \cr
#' (Inspection run by \code{\link{read.isobxr_master}} function.) \cr
#' Default is TRUE.
#'
#' @return Delta values and box sizes as a function of time. \cr
#' sim.scenario outputs are saved to workdir if save_outputs = TRUE.
#'
#' @section sim.scenario outputs consist of:
#' \enumerate{
#' \item \strong{single run results} in SERIES directory: all single runs results as rds files
#' \item \strong{scenario digest} in scenario DIGEST directory (SERIES/DIGEST):
#' \enumerate{
#' \item \strong{isobxr master file archive} as xlsx
#' \item \strong{scenario master file archive} as xlsx
#' \item \strong{plot of delta and size vs. time} as pdf
#' \item \strong{scenario results data set} as rds, containing:
#' \enumerate{
#' \item \strong{delta_vs_t} data frame of delta as a function of time
#' \item \strong{size_vs_t} data frame of box sizes as a function of time
#' \item \strong{scenario_master} list containing all inputs from scenario master file
#' \item \strong{scenario_log} data frame of scenario specific LOG excerpt
#' \item \strong{isobxr_master} list containing all inputs from isobxr master file
#' \item \strong{paths} list of scenario specific paths
#' }
#' }
#' }
#' @export
#' @examples
#' \dontrun{
#' sim.scenario(workdir = "/Users/username/Documents/1_ABC_tutorial",
#'              SERIES_ID =  "1_source_change",
#'              scenario_master_file = "0_SCENARIO_source_change",
#'              isobxr_master_file = "0_ISOBXR_MASTER")
#'}
sim.scenario <- function(workdir,
                         SERIES_ID,
                         scenario_master_file,
                         isobxr_master_file = "0_ISOBXR_MASTER",
                         plot.hidden_boxes = NULL,
                         plot.time_unit = NULL,
                         export.single_run_digests = FALSE,
                         export.data_as_csv_xlsx = FALSE,
                         show.delta_plot = TRUE,
                         save_outputs = FALSE,
                         inspect_inputs = TRUE){

  # # 0. debug arguments
  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list = ls())
  # # rm(list = ls()[!ls() %in% c("sr_paths")])
  # gc()
  # devtools::load_all(".")
  #
  # isobxr_master_file = "0_ISOBXR_MASTER"
  # plot.hidden_boxes = NULL
  # plot.time_unit = NULL
  # export.single_run_digests = FALSE
  # export.data_as_csv_xlsx = FALSE
  # show.delta_plot = TRUE
  # save_outputs = FALSE
  # inspect_inputs = TRUE
  #
  # workdir = "/Users/username/Documents/1_ABC_tutorial"
  # SERIES_ID =  "1_source_change"
  # scenario_master_file = "0_SCENARIO_source_change"
  # isobxr_master_file = "0_ISOBXR_MASTER"

  # I. check arguments ####
  args <- c(as.list(environment()))
  rm(list=ls()[ls() != "args"])

  if (!is.logical(args$inspect_inputs)) rlang::abort("\"inspect_input\" argument should be logical.")

  if (args$inspect_inputs){
    args.allowed <- list(logical = c("export.single_run_digests",
                                     "export.data_as_csv_xlsx",
                                     "show.delta_plot",
                                     "save_outputs"))

    for (i in 1:length(args.allowed$logical)){
      if (!is.logical(eval(parse(text = paste0("args$", args.allowed$logical[i]))))){
        rlang::abort(paste0("\"", args.allowed$logical[i], "\" argument should be logical."))
      }
    }
  }


  # locally bind variables (fixing binding global variable issue)
  diff.Time <- Time <- path_outdir <- DELTA.t_max <- SIZE.t_max <- BOX_ID <- NULL

  # REMARKS
  # the FORCING_DELTA sheet :  when a delta value is forced at a stage it will be inherited in the next runs
  # FORCING_ALPHA sheet : not inherited from a run to another, back to previous value

  # II. initiate ####
  # _a. set workdir ####
  # __i. determine function mode (tuto/user) ####
  fun_mode <- using_extdata_tutorial_2(args$workdir, args$save_outputs, args$show.delta_plot)
  args$workdir <- fun_mode$workdir
  paths <- list(workdir = fun_mode$workdir)
  if (fun_mode$tuto_mode) args$isobxr_master_file <- "0_ISOBXR_MASTER"
  paths$isobxr_master_file <- args$isobxr_master_file
  paths$scenario_master_file <- args$scenario_master_file
  args$show.delta_plot <- fun_mode$plot_results
  args$save_outputs <- fun_mode$save_outputs

  # __ii. set workdir ####
  workdir_old <- getwd()
  on.exit(setwd(workdir_old), add = TRUE)
  setwd(paths$workdir)

  # Time_plot <- VAR <- VAR_TYPE <- NULL

  unlink(to_tmpdir(""), recursive = TRUE)
  on.exit(unlink(to_tmpdir(""), recursive = TRUE), add = TRUE)

  rlang::inform("________________________________________________________________________________")
  if (fun_mode$tuto_mode){
    rlang::inform(paste("\U2139 workdir: no workdir.
  You are using the tutorial mode (isobxr embedded tutorial files).
  The default outputs are limited and can't be exported.", sep = ""))
  } else {
    rlang::inform(paste("\U2139 workdir: ", getwd(), sep = ""))
  }

  args$hidden_runs <- c(1)

  args_run <- NULL
  if (args$export.single_run_digests){
    args_run$export.diagrams <- args_run$export.delta_plot <- args_run$export.data_as_csv_xlsx <- TRUE
  } else {
    args_run$export.diagrams <- args_run$export.delta_plot <- args_run$export.data_as_csv_xlsx <- FALSE
  }

  # III. Prepare arguments ####
  # _a. read scenario master ####

  master.sce <- read.scenario_master(workdir = args$workdir,
                                     scenario_master_file = args$scenario_master_file,
                                     isobxr_master_file = args$isobxr_master_file)

  # RUN_LIST <- as.data.frame(readxl::read_excel(COMPO_MASTER, "RUN_LIST"))
  # RAYLEIGH_LIST <- as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_RAYLEIGH"))
  # DELTA_FORCING <- as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_DELTA"))
  # FORCING_ALPHA <- as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_ALPHA"))
  # FORCING_SIZE <- as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_SIZE"))

  # if (nrow(master.sce$FORCING_DELTA) == 0) master.sce$FORCING_DELTA <- NULL
  # if (nrow(master.sce$FORCING_ALPHA) == 0) master.sce$FORCING_ALPHA <- NULL
  # if (nrow(master.sce$FORCING_SIZE) == 0) master.sce$FORCING_SIZE <- NULL

  # _b. define scenario series family ####
  paths$LOG_file <- "1_LOG.csv"
  n_zeros <- 3
  if (file.exists(paths$LOG_file)){
    file.copy(from = paths$LOG_file, to = to_tmpdir(paths$LOG_file))
    LOG <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T)
    LOG_COMPO <- LOG[LOG$COMPOSITE == TRUE, ]
    remove(LOG)
    # paths$COMPO_SERIES_FAMILY <- paste(names.output[names.output$func == "sim.scenario", "old.acronym"],
    #                                    as.character(args$SERIES_ID), sep = "_")
    paths$COMPO_SERIES_FAMILY <- paste(names.output[names.output$func == "sim.scenario", "acronym"],
                                       as.character(args$SERIES_ID), sep = "_")
    if (nrow(LOG_COMPO[LOG_COMPO$COMPO_SERIES_FAMILY == paths$COMPO_SERIES_FAMILY,]) == 0){
      paths$COMPO_SERIES_n <- 1
      # args$SERIES_ID <- paste(names.output[names.output$func == "sim.scenario", "old.acronym"],
      #                         as.character(args$SERIES_ID),
      #                         paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
      args$SERIES_ID <- paste(names.output[names.output$func == "sim.scenario", "acronym"],
                              as.character(args$SERIES_ID),
                              paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
    } else {
      paths$COMPO_SERIES_n <- max(LOG_COMPO[LOG_COMPO$COMPO_SERIES_FAMILY == paths$COMPO_SERIES_FAMILY, "COMPO_SERIES_n"])+1
      COMPO_SERIES_n_length <- length(unlist(strsplit(as.character(paths$COMPO_SERIES_n), "")))
      # args$SERIES_ID <- paste(names.output[names.output$func == "sim.scenario", "old.acronym"],
      #                         as.character(args$SERIES_ID),
      #                         paste(as.character(c(replicate(n_zeros-COMPO_SERIES_n_length,0),
      #                                              paths$COMPO_SERIES_n)), collapse = ""),
      #                         sep = "_")
      args$SERIES_ID <- paste(names.output[names.output$func == "sim.scenario", "acronym"],
                              as.character(args$SERIES_ID),
                              paste(as.character(c(replicate(n_zeros-COMPO_SERIES_n_length,0),
                                                   paths$COMPO_SERIES_n)), collapse = ""),
                              sep = "_")
    }
  } else {
    paths$COMPO_SERIES_n <- 1
    # paths$COMPO_SERIES_FAMILY <- paste(names.output[names.output$func == "sim.scenario", "old.acronym"],
    #                                    as.character(args$SERIES_ID), sep = "_")
    paths$COMPO_SERIES_FAMILY <- paste(names.output[names.output$func == "sim.scenario", "acronym"],
                                       as.character(args$SERIES_ID), sep = "_")
    # args$SERIES_ID <- paste(names.output[names.output$func == "sim.scenario", "old.acronym"],
    #                         as.character(args$SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
    args$SERIES_ID <- paste(names.output[names.output$func == "sim.scenario", "acronym"],
                            as.character(args$SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
  }

  paths$SERIES_ID <- args$SERIES_ID

  paths$outdir <- paste0("3_", paths$SERIES_ID, "/")

  # _c. read isobxr master constants ####
  isobxr_master <- read.isobxr_master(args$workdir,
                                      args$isobxr_master_file,
                                      inspect = T,
                                      export_rds = F)

  # IV. sim.single_run i = 1 ####
  i <- 1

  # _a. prepare args_run - defined for run 1 ####
  args_run$flux_list <- master.sce$RUN_SEQUENCE$flux_list[i] %>% as.character()
  args_run$coeff_list <- master.sce$RUN_SEQUENCE$coeff_list[i] %>% as.character()
  args_run$t_max <- master.sce$RUN_SEQUENCE$t_max[i]
  args_run$n_steps <- master.sce$RUN_SEQUENCE$n_steps[i]

  # _b. force delta.t0 ####
  if (i %in% master.sce$FORCING_DELTA$RUN_n){
    args_run$FORCING_DELTA.loc <- master.sce$FORCING_DELTA[master.sce$FORCING_DELTA$RUN_n == i,
                                                  c("BOX_ID", "DELTA.t0")] %>%
      clear_subset()
  } else {
    args_run$FORCING_DELTA.loc <- NULL
  }

  # _c. force size.t0 ####
  if (i %in% master.sce$FORCING_SIZE$RUN_n){
    args_run$FORCING_SIZE.loc <- master.sce$FORCING_SIZE[master.sce$FORCING_SIZE$RUN_n == i,
                                                  c("BOX_ID", "SIZE.t0")] %>%
      clear_subset()
  } else {
    args_run$FORCING_SIZE.loc <- NULL
  }

  # _d. force alphas ####
  if (i %in% master.sce$FORCING_ALPHA$RUN_n){
    args_run$FORCING_ALPHA.loc <- master.sce$FORCING_ALPHA[master.sce$FORCING_ALPHA$RUN_n == i,
                                                  c("FROM", "TO", "ALPHA")] %>%
      clear_subset()
  } else {
    args_run$FORCING_ALPHA.loc <- NULL
  }

  # _e. force rayleigh ####
  if (i %in% master.sce$FORCING_RAYLEIGH$RUN_n){
    args_run$FORCING_RAYLEIGH.loc <- master.sce$FORCING_RAYLEIGH[master.sce$FORCING_RAYLEIGH$RUN_n == i,
                                                        -which(names(master.sce$FORCING_RAYLEIGH) %in% c("RUN_n"))] %>%
      clear_subset()
  } else {
    args_run$FORCING_RAYLEIGH.loc <- NULL
  }

  # _f. run sim.single_run ####
  rlang::inform("________________________________________________________________________________")
  rlang::inform("\U0001f535 COMPUTING ")
  rlang::inform("\U2139 Running initial simulation: ")

  sim.single_run(workdir = args$workdir,
                 SERIES_ID = args$SERIES_ID,
                 flux_list = args_run$flux_list,
                 coeff_list = args_run$coeff_list,
                 t_max = args_run$t_max,
                 n_steps = args_run$n_steps,
                 # isobxr_master_file = args$isobxr_master_file,
                 isobxr_master = isobxr_master,
                 suppress_messages = FALSE,
                 export.diagrams = args_run$export.diagrams,
                 export.delta_plot = args_run$export.delta_plot,
                 export.data_as_csv_xlsx = args_run$export.data_as_csv_xlsx,
                 plot.time_unit = args$plot.time_unit,
                 show.delta_plot = FALSE,
                 inspect_inputs = TRUE,
                 save_outputs = FALSE,
                 return_data = FALSE,
                 FORCING_RAYLEIGH = args_run$FORCING_RAYLEIGH.loc,
                 FORCING_SIZE = args_run$FORCING_SIZE.loc,
                 FORCING_DELTA = args_run$FORCING_DELTA.loc,
                 FORCING_ALPHA = args_run$FORCING_ALPHA.loc,
                 COMPOSITE = TRUE,
                 COMPO_SERIES_n = paths$COMPO_SERIES_n,
                 COMPO_SERIES_FAMILY = paths$COMPO_SERIES_FAMILY,
                 EXPLORER = FALSE,
                 EXPLO_SERIES_n = NaN,
                 EXPLO_SERIES_FAMILY = NaN)

  rlang::inform("\U2139 Running scenario models: ")

  # V. sin.single_run i > 1 ####
  i <- i + 1

  if (length(master.sce$RUN_SEQUENCE$RUN_n) > 10) {long_scenario = TRUE} else {long_scenario = FALSE}
  if (long_scenario){
    pb_cpt <- utils::txtProgressBar(min = 2, max = length(master.sce$RUN_SEQUENCE$RUN_n), style = 3, width = 60)
    clock <- 1
  } else {
    calculation_gauge(i-1, length(master.sce$RUN_SEQUENCE$t_max)-1)
  }

  for (i in 2:length(master.sce$RUN_SEQUENCE$RUN_n)){

    # _a. prepare args_run - inherit from run i-1 ####
    args_run$flux_list <- master.sce$RUN_SEQUENCE$flux_list[i] %>% as.character()
    args_run$coeff_list <- master.sce$RUN_SEQUENCE$coeff_list[i] %>% as.character()
    args_run$t_max <- master.sce$RUN_SEQUENCE$t_max[i]
    args_run$n_steps <- master.sce$RUN_SEQUENCE$n_steps[i]

    LOG <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T)
    LOG_last <- LOG[nrow(LOG),]
    remove(LOG)

    args_run$SIZE.t0 <- readRDS(to_tmpdir(paste(LOG_last$path_outdir, ".rds", sep = "")))$outputs$final_state %>%
      dplyr::select(BOX_ID, SIZE.t_max) %>%
      dplyr::rename("SIZE.t0" = "SIZE.t_max") %>%
      clear_subset()

    args_run$DELTA.t0 <- readRDS(to_tmpdir(paste(LOG_last$path_outdir, ".rds", sep = "")))$outputs$final_state %>%
      dplyr::select(BOX_ID, DELTA.t_max) %>%
      dplyr::rename("DELTA.t0" = "DELTA.t_max") %>%
      clear_subset()

    # _b. force delta.t0 ####
    if (i %in% master.sce$FORCING_DELTA$RUN_n){
      args_run$FORCING_DELTA.loc <- master.sce$FORCING_DELTA[master.sce$FORCING_DELTA$RUN_n == i,
                                                             c("BOX_ID", "DELTA.t0")] %>%
        clear_subset()
      j <- 1
      for (j in 1:nrow(args_run$FORCING_DELTA.loc)){
        args_run$DELTA.t0[args_run$DELTA.t0$BOX_ID == as.character(args_run$FORCING_DELTA.loc[j,
                                                                                              "BOX_ID"]), "DELTA.t0"] <-
          args_run$FORCING_DELTA.loc[j, "DELTA.t0"]
        j <- j + 1
      }
    }

    # _c. force size.t0 ####
    if (i %in% master.sce$FORCING_SIZE$RUN_n){
      args_run$FORCING_SIZE.loc <- master.sce$FORCING_SIZE[master.sce$FORCING_SIZE$RUN_n == i,
                                                           c("BOX_ID", "SIZE.t0")] %>%
        clear_subset()
      j <- 1
      for (j in 1:nrow(args_run$FORCING_SIZE.loc)){
        args_run$SIZE.t0[args_run$SIZE.t0$BOX_ID == as.character(args_run$FORCING_SIZE.loc[j, "BOX_ID"]), "SIZE.t0"] <-
          args_run$FORCING_SIZE.loc[j, "SIZE.t0"]
        j <- j + 1
      }
    }

    # _d. force alphas ####
    if (i %in% master.sce$FORCING_ALPHA$RUN_n){
      args_run$FORCING_ALPHA.loc <- master.sce$FORCING_ALPHA[master.sce$FORCING_ALPHA$RUN_n == i,
                                                             c("FROM", "TO", "ALPHA")] %>%
        clear_subset()
    } else {
      args_run$FORCING_ALPHA.loc <- NULL
    }

    # _e. force rayleigh ####
    if (i %in% master.sce$FORCING_RAYLEIGH$RUN_n){
      args_run$FORCING_RAYLEIGH.loc <- master.sce$FORCING_RAYLEIGH[master.sce$FORCING_RAYLEIGH$RUN_n == i,
                                                                   -which(names(master.sce$FORCING_RAYLEIGH) %in% c("RUN_n"))] %>%
        clear_subset()
    } else {
      args_run$FORCING_RAYLEIGH.loc <- NULL
    }

    # _f. run sim.single_run ####
    sim.single_run(workdir = args$workdir,
                   SERIES_ID = args$SERIES_ID,
                   flux_list = args_run$flux_list,
                   coeff_list = args_run$coeff_list,
                   t_max = args_run$t_max,
                   n_steps = args_run$n_steps,
                   # isobxr_master_file = args$isobxr_master_file,
                   isobxr_master = isobxr_master,
                   suppress_messages = long_scenario,
                   export.diagrams = args_run$export.diagrams,
                   export.delta_plot = FALSE, # args_run$export.delta_plot,
                   export.data_as_csv_xlsx = args_run$export.data_as_csv_xlsx,
                   plot.time_unit = args$plot.time_unit,
                   show.delta_plot = FALSE,
                   inspect_inputs = isFALSE(long_scenario),
                   save_outputs = FALSE,
                   return_data = FALSE,
                   FORCING_RAYLEIGH = args_run$FORCING_RAYLEIGH.loc,
                   FORCING_SIZE = args_run$SIZE.t0,
                   FORCING_DELTA = args_run$DELTA.t0,
                   FORCING_ALPHA = args_run$FORCING_ALPHA.loc,
                   COMPOSITE = TRUE,
                   COMPO_SERIES_n = paths$COMPO_SERIES_n,
                   COMPO_SERIES_FAMILY = paths$COMPO_SERIES_FAMILY,
                   EXPLORER = FALSE,
                   EXPLO_SERIES_n = NaN,
                   EXPLO_SERIES_FAMILY = NaN)

    if (long_scenario){
      utils::setTxtProgressBar(pb_cpt, clock)
      clock <- clock + 1
    }
    else {
      calculation_gauge(i-1, length(master.sce$RUN_SEQUENCE$t_max)-1)
    }

    i <- i + 1
  }

  # VI. merge scenario outputs (evD and evS) ####
  # _a. read series log file ####
  LOG_SERIES <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T) %>%
    dplyr::filter(SERIES_ID == paths$SERIES_ID) %>%
    clear_subset()

  paths$paths_all_rds <- LOG_SERIES[, "path_outdir"] %>% as.character()

  # _b. merge evS and evD ####
  rlang::inform("\n ________________________________________________________________________________")
  rlang::inform("\U0001f535 PREPARING RESULTS")

  read_delta_vs_t_rds <- function(path_to_rds){
    readRDS(to_tmpdir(paste0(path_to_rds, ".rds"))) %>%
      magrittr::extract2("outputs") %>%
      magrittr::extract2("delta_vs_t") %>%
      dplyr::mutate(path_outdir = path_to_rds)
  }

  read_size_vs_t_rds <- function(path_to_rds){
    readRDS(to_tmpdir(paste0(path_to_rds, ".rds"))) %>%
      magrittr::extract2("outputs") %>%
      magrittr::extract2("size_vs_t") %>%
      dplyr::mutate(path_outdir = path_to_rds)
  }

  evD <- paths$paths_all_rds %>%
    purrr::map(read_delta_vs_t_rds) %>%
    purrr::reduce(dplyr::bind_rows)

  evS <- paths$paths_all_rds %>%
    purrr::map(read_size_vs_t_rds) %>%
    purrr::reduce(dplyr::bind_rows)

  evD <- evD %>%
    dplyr::group_by(path_outdir) %>%
    dplyr::mutate(diff.Time = Time - dplyr::lag(Time, default = 0)) %>%
    dplyr::ungroup(path_outdir) %>%
    dplyr::mutate(Time_COMPOSITE = cumsum(diff.Time)) %>%
    dplyr::select(!c(diff.Time)) %>%
    as.data.frame() %>%
    dplyr::full_join(LOG_SERIES[, c("RUN_n", "SERIES_RUN_ID", "path_outdir")],
                     by = "path_outdir") %>%
    dplyr::select(!path_outdir)
    # dplyr::filter(!(Time == 0 & RUN_n > 1))

  evS <- evS %>%
    dplyr::group_by(path_outdir) %>%
    dplyr::mutate(diff.Time = Time - dplyr::lag(Time, default = 0)) %>%
    dplyr::ungroup(path_outdir) %>%
    dplyr::mutate(Time_COMPOSITE = cumsum(diff.Time)) %>%
    dplyr::select(!c(diff.Time)) %>%
    as.data.frame() %>%
    dplyr::full_join(LOG_SERIES[, c("RUN_n", "SERIES_RUN_ID", "path_outdir")],
                     by = "path_outdir") %>%
    dplyr::select(!path_outdir)
    # dplyr::filter(!(Time == 0 & RUN_n > 1))

  # _c. edit csv meta and results ####
  rlang::inform("________________________________________________________________________________")
  rlang::inform("\U0001f535 WRITING OUTPUTS")

  # prepare paths
  # paths$digest_dir <- paste("3_", as.character(args$SERIES_ID), "/",
  #                           names.output[names.output$func == "sim.scenario", "old.digest_dir"],
  #                           sep = "")
  paths$digest_dir <- paste("3_", as.character(args$SERIES_ID), "/",
                            names.output[names.output$func == "sim.scenario", "digest_dir"],
                            sep = "")
  if (!dir.exists(to_tmpdir(paths$digest_dir))){dir.create(to_tmpdir(paths$digest_dir))}
  paths$digest_root <- paste(paths$digest_dir, "/", as.character(args$SERIES_ID), sep = "")
  paths$digest_log <- paste(paths$digest_root, "_meta_LOG.csv", sep = "")
  paths$digest_results <- paste(paths$digest_root, "_results.rds", sep = "")
  paths$digest_scenario_master <- paste(paths$digest_root, "_in_master_scenario.xlsx", sep = "")
  paths$digest_isobxr_master <- paste(paths$digest_root, "_in_master_isobxr.xlsx", sep = "")

  if(args$export.data_as_csv_xlsx){
    data.table::fwrite(LOG_SERIES,
                       file = to_tmpdir(paths$digest_log),
                       row.names = F, quote = F)
    data.table::fwrite(evD, file = paste(to_tmpdir(paths$digest_root), "_out_delta_vs_t.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evS, file = paste(to_tmpdir(paths$digest_root), "_out_size_vs_t.csv", sep = ""), row.names = F, quote = F)
  }

  # _d. edit master xlsx archives ####
  writexl::write_xlsx(master.sce,
                      to_tmpdir(paths$digest_scenario_master))

  writexl::write_xlsx(isobxr_master[!names(isobxr_master) %in% c("bx.groups")],
                      to_tmpdir(paths$digest_isobxr_master))

  # _e. edit rds results ####
  results <- list(delta_vs_t = evD,
                  size_vs_t = evS,
                  scenario_master = master.sce,
                  scenario_log = LOG_SERIES,
                  isobxr_master = isobxr_master,
                  paths = paths)

  saveRDS(object = results,
          file = to_tmpdir(paths$digest_results))

  # VII. plot scenario ####
  #### edit pdf of evD/evS multiplot
  if(!(fun_mode$tuto_mode)){
    pdf_path <- paste(paths$digest_root, "_plot_all_vs_t.pdf", sep = "")
    dev.new()
    pdf(to_tmpdir(pdf_path),
        width = 21/2.54, height = 29.7/2.54,
        pointsize = 1, useDingbats=FALSE)
    suppressWarnings(plot_scenario(workdir = to_tmpdir(""),
                                   scenario_dir_name = paths$outdir,
                                   shown_runs = NULL,
                                   time_unit = args$plot.time_unit,
                                   hidden_boxes = args$plot.hidden_boxes,
                                   return_as_print = TRUE,
                                   show.run_separations = isFALSE(long_scenario)))
    graphics.off()
  }

  # X. save_outputs ####
  if (isFALSE(long_scenario)){
    rlang::inform("________________________________________________________________________________")
    rlang::inform(message = paste("\U2139 Run outputs (stored in temporary directory).",
                                  sep = ""))
    fs::dir_tree(path = to_tmpdir(""), recurse = T)
  }

  rlang::inform("________________________________________________________________________________")
  if(!args$save_outputs){
    rlang::inform("\U2757 Results were not saved to working directory (set save_run_outputs = TRUE to save results).")
  } else if(args$save_outputs){
    R.utils::copyDirectory(to_tmpdir(""),
                           getwd(),
                           overwrite = T)
    rlang::inform("\U2705 Results were successfully saved to working directory.")
  }

  if(args$show.delta_plot){
    suppressWarnings(plot_scenario(workdir = to_tmpdir(""),
                                   scenario_dir_name = paths$outdir,
                                   shown_runs = NULL,
                                   time_unit = args$plot.time_unit,
                                   hidden_boxes = args$plot.hidden_boxes,
                                   return_as_print = TRUE,
                                   show.run_separations = isFALSE(long_scenario)))
  }
}
