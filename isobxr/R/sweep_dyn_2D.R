#  #_________________________________________________________________________80char
#' Sweep the space of two parameters during the response to a perturbation
#'
#' @description  A function to assess the influence of two parameters (varying
#' over a range of values) on dynamic evolution of a system in response to a given perturbation.
#'
#' @param workdir Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
#' of the dynamic sweep master file (e.g., \strong{\emph{0_EXPLO_DYN_MASTER.xlsx}}) \cr
#' and where output files will be stored if saved by user. \cr
#' (character string)
#' @param SERIES_ID Name of the sweep series belongs to. \cr
#' It determines the folder in which the output files will be stored inside workdir.\cr
#' (character string)
#' @param plot.time_unit Time unit to use on plot if different from native time unit. \cr
#' Character string, to be selected among the following:\cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}\cr
#' Default is NULL.
#' @param isobxr_master_file Name of \strong{\emph{isobxr excel master file}}. \cr
#' Default is "0_ISOBXR_MASTER".
#' @param sweep_master_file Name of \strong{\emph{sweep.dyn_2D excel master file}}.
#' (without file "xlsx" extension).
#' @param swept_param_1 Set of values of sweeping parameter 1. \cr
#' Formatted data frame, see vignette for further details.
#' @param swept_param_2 Set of values of sweeping parameter 2. \cr
#' Formatted data frame, see vignette for further details.
#' @param export.data_as_csv_xlsx If TRUE, exports full sweep result data as csv and xlsx fo full
#' to sweep digest directory. \cr
#' Default is TRUE.
#' @param show.delta_plot If TRUE, prints delta and size time evolution plots in R. \cr
#' Default is TRUE.
#' @param save_outputs If TRUE, saves all run outputs to local working directory (workdir). \cr
#' By default, run outputs are stored in a temporary directory and erased if not saved. \cr
#' Default is FALSE.
#' @param ask_confirmation  If TRUE, asks confirmation to run in interactive sessions. \cr
#' Default is TRUE.
#' @param keep_single_run_rds If TRUE, keeps single runs outputs (rds files in SERIES directory). \cr
#' Default is FALSE.
#'
#' @return Delta values and box sizes as a function of time in response to a perturbation,
#' in a 2D space of parameters
#' sweep.dyn_2D outputs are saved to workdir if save_outputs = TRUE.
#'
#' @section sweep.dyn_2D outputs consist of:
#' \enumerate{
#' \item \strong{single run results} in SERIES directory: all single runs results as rds files \cr
#' (keep_single_run_rds = TRUE)
#' \item \strong{sweep digest} in sweep DIGEST directory (SERIES/DIGEST):
#' \enumerate{
#' \item \strong{isobxr master file archive} as xlsx (export.data_as_csv_xlsx = TRUE)
#' \item \strong{sweep.dyn_2D master file archive} as xlsx (export.data_as_csv_xlsx = TRUE)
#' \item \strong{sweep.dyn_2D LOG excerpt} as csv (export.data_as_csv_xlsx = TRUE)
#' \item \strong{delta_size_vs_t} csv of delta and size vs time in 2D space (export.data_as_csv_xlsx = TRUE)
#' \item \strong{plot of delta and size vs. time in 2D space} as pdf
#' \item \strong{sweep.dyn_2D results data set} as rds, containing:
#' \enumerate{
#' \item \strong{delta_size_vs_t} data frame of delta and size as a function of time
#' \item \strong{sweeep_master} list containing all inputs from sweep master file
#' \item \strong{sweep_log} data frame of sweep specific LOG excerpt
#' \item \strong{isobxr_master} list containing all inputs from isobxr master file
#' \item \strong{paths} list of sweep specific paths
#' }
#' }
#' }
#'
#'
#' @export
#' @examples
#' \dontrun{
#' sweep.dyn_2D(workdir = "/Users/username/Documents/1_ABC_tutorial",
#'              SERIES_ID = "sweep_dyn_test",
#'              isobxr_master_file = "0_ISOBXR_MASTER",
#'              sweep_master_file = "0_SWEEP_DYN_demo",
#'              swept_param_1 = data.frame(FROM = c("A"),
#'                                         TO = c("C"),
#'                                         ALPHA_MIN = 0.999,
#'                                         ALPHA_MAX = 1,
#'                                         ALPHA_STEPS = 0.0005,
#'                                         EXPLO_TYPES = "EXPLO_1_ALPHA"),
#'              swept_param_2 = data.frame(BOX_ID = c("B"),
#'                                         SIZE_MIN = 2100,
#'                                         SIZE_MAX = 3000,
#'                                         SIZE_STEPS = 300,
#'                                         EXPLO_TYPES = "EXPLO_1_SIZE"),
#'              ask_confirmation = FALSE)
#'}
sweep.dyn_2D <- function(workdir,
                         SERIES_ID,
                         plot.time_unit = NULL,
                         isobxr_master_file = "0_ISOBXR_MASTER",
                         sweep_master_file,
                         swept_param_1,
                         swept_param_2,
                         export.data_as_csv_xlsx = TRUE,
                         show.delta_plot = TRUE,
                         save_outputs = FALSE,
                         ask_confirmation = TRUE,
                         keep_single_run_rds = FALSE
){

  # # 0. debug arguments
  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list = ls())
  # # rm(list = ls()[!ls() %in% c("sr_paths")])
  # gc()
  # devtools::load_all(".")
  #
  # list_of_fluxes <- c("INTAKE_004.000",
  #                     # "INTAKE_004.010",
  #                     "INTAKE_004.020",
  #                     # "INTAKE_004.030",
  #                     "INTAKE_004.040",
  #                     # "INTAKE_004.050",
  #                     "INTAKE_004.060",
  #                     # "INTAKE_004.070",
  #                     "INTAKE_004.080",
  #                     # "INTAKE_004.090",
  #                     "INTAKE_004.100",
  #                     # "INTAKE_004.110",
  #                     "INTAKE_004.120",
  #                     # "INTAKE_004.130",
  #                     "INTAKE_004.140"
  #                     # "INTAKE_004.150"
  # )
  #
  #
  # workdir = "/Users/sz18642/isobxr Gd 2023/3_human_dCa"
  # SERIES_ID = "sweep_dyn_bone_loss"
  # plot.time_unit = "yr"
  # isobxr_master_file = "0_ISOBXR_MASTER_Ca"
  # sweep_master_file = "0_SWEEP_DYN_bone_loss"
  # swept_param_1 = data.frame(VALUES_1 = rep("INTAKE_004.000", length(list_of_fluxes)),
  #                            VALUES_2 = list_of_fluxes,
  #                            EXPLO_TYPES = "EXPLO_n_FLUX_MATRICES")
  # swept_param_2 = data.frame(FROM = "ECF",
  #                            TO = "BONE",
  #                            ALPHA_MAX = 1,
  #                            ALPHA_MIN = .9994,
  #                            ALPHA_STEPS = .0003,
  #                            EXPLO_TYPES = "EXPLO_1_ALPHA")
  # export.data_as_csv_xlsx = TRUE
  # show.delta_plot = TRUE
  # save_outputs = TRUE
  # ask_confirmation = TRUE
  # keep_single_run_rds = FALSE

  # I. check arguments ####
  args <- c(as.list(environment()))
  rm(list=ls()[ls() != "args"])

  args.allowed <- list(logical = c("export.data_as_csv_xlsx",
                                   "show.delta_plot",
                                   "save_outputs",
                                   "ask_confirmation",
                                   "keep_single_run_rds"))

  for (i in 1:length(args.allowed$logical)){
    if (!is.logical(eval(parse(text = paste0("args$", args.allowed$logical[i]))))){
      rlang::abort(paste0("\"", args.allowed$logical[i], "\" argument should be logical."))
    }
  }

  # II. initiate ####

  # locally bind variables (fixing binding global variable issue)
  path_outdir <- VALUES <- EXPLO_TYPES <- VALUES_2 <-
    VALUES_1 <- TO <- FROM <- SIZE.t_max <-
    DELTA.t_max <- ALL_DESC <- BOX_ID <- FROM_TO <-
    # RUN_n <- old.acronym <- old.prefix <- func <-
    RUN_n <- acronym <- prefix <- func <-
    NUM_ANA <- SERIES_RUN_ID <- LEGEND_EXPLO_1 <-
    VAR_EXPLO_1 <- LEGEND_EXPLO_2 <- VAR_EXPLO_2 <-
    d0 <- m0 <- value <-  NULL

  # _a. set workdir ####
  # __i. determine function mode (tuto/user) ####
  fun_mode <- using_extdata_tutorial_2(args$workdir, args$save_outputs, args$show.delta_plot)
  args$workdir <- fun_mode$workdir
  paths <- list(workdir = fun_mode$workdir)
  if (fun_mode$tuto_mode) args$isobxr_master_file <- "0_ISOBXR_MASTER"
  paths$isobxr_master_file <- args$isobxr_master_file
  paths$sweep_master_file <- args$sweep_master_file
  args$show.delta_plot <- fun_mode$plot_results
  args$save_outputs <- fun_mode$save_outputs

  # paths$prefix <- names.output %>%
  #   dplyr::filter(func == "sweep.dyn_2D") %>%
  #   dplyr::pull(old.prefix)
  # paths$acronym <- names.output %>%
  #     dplyr::filter(func == "sweep.dyn_2D") %>%
  #     dplyr::pull(old.acronym)

  paths$prefix <- names.output %>%
    dplyr::filter(func == "sweep.dyn_2D") %>%
    dplyr::pull(prefix)
  paths$acronym <- names.output %>%
    dplyr::filter(func == "sweep.dyn_2D") %>%
    dplyr::pull(acronym)

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

  # III. Prepare arguments ####
  # _a. read sweep master ####

  master.sweep <- read.dyn_2D_master(workdir = args$workdir,
                                     dyn_2D_master_file = args$sweep_master_file,
                                     isobxr_master_file = args$isobxr_master_file)

  # RUN_LIST <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "RUN_LIST"))
  # RAYLEIGH_LIST <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_RAYLEIGH"))
  # DELTA_FORCING <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_DELTA"))
  # FORCING_ALPHA <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_ALPHA"))
  # FORCING_SIZE <-  as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_SIZE"))

  # # if (nrow(DELTA_FORCING) == 0) DELTA_FORCING <- NULL
  # # if (nrow(FORCING_ALPHA) == 0) FORCING_ALPHA <- NULL
  # # if (nrow(FORCING_SIZE) == 0) FORCING_SIZE <- NULL
  #
  # if (nrow(master.sweep$FORCING_DELTA) == 0) master.sweep$FORCING_DELTA <- NULL
  # if (nrow(master.sweep$FORCING_ALPHA) == 0) master.sweep$FORCING_ALPHA <- NULL
  # if (nrow(master.sweep$FORCING_SIZE) == 0) master.sweep$FORCING_SIZE <- NULL

  # t_lim_list <- as.numeric(RUN_LIST$t_lim_list)
  # nb_steps_list <- as.numeric(RUN_LIST$nb_step_list)
  # flux_list <- as.character(RUN_LIST$flux_list)
  # coeff_list <- as.character(RUN_LIST$coeff_list)

  # _b. define sweep series family ####
  paths$LOG_file <- "1_LOG.csv"
  n_zeros <- 4
  if (file.exists(paths$LOG_file) == TRUE){
    file.copy(from = paths$LOG_file, to = to_tmpdir(paths$LOG_file))
    LOG <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T)
    LOG_EXPLO <- LOG[LOG$EXPLORER == TRUE, ]
    remove(LOG)
    paths$EXPLO_SERIES_FAMILY <- paste(paths$acronym, as.character(args$SERIES_ID), sep = "_")
    if (nrow(LOG_EXPLO[LOG_EXPLO$EXPLO_SERIES_FAMILY == paths$EXPLO_SERIES_FAMILY,]) == 0){
      paths$EXPLO_SERIES_n <- 1
      args$SERIES_ID <- paste(paths$acronym, as.character(args$SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
    } else {
      paths$EXPLO_SERIES_n <- max(LOG_EXPLO[LOG_EXPLO$EXPLO_SERIES_FAMILY == paths$EXPLO_SERIES_FAMILY, "EXPLO_SERIES_n"])+1
      EXPLO_SERIES_n_length <- length(unlist(strsplit(as.character(paths$EXPLO_SERIES_n), "")))
      args$SERIES_ID <- paste(paths$acronym, as.character(args$SERIES_ID),
                              paste(as.character(c(replicate(n_zeros-EXPLO_SERIES_n_length,0),
                                                   paths$EXPLO_SERIES_n)),
                                    collapse = ""), sep = "_")
    }
  } else {
    paths$EXPLO_SERIES_n <- 1
    paths$EXPLO_SERIES_FAMILY <- paste(paths$acronym, as.character(args$SERIES_ID), sep = "_")
    args$SERIES_ID <- paste(paths$acronym, as.character(args$SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
  }

  paths$SERIES_ID <- args$SERIES_ID

  # _c. read isobxr master constants ####
  isobxr_master <- read.isobxr_master(args$workdir,
                                      args$isobxr_master_file,
                                      inspect = T,
                                      export_rds = F)

  # ISOPY_MASTER_file <- "0_ISOBXR_MASTER.xlsx"
  # CONSTANTS <- as.data.frame(readxl::read_excel(ISOPY_MASTER_file, "CONSTANTS"))


  # _d. BUILD EXPLO AXIS LISTS ####
  # __i. EXPLO_AXIS_1 ####
  par1 <- list(type = as.character(unique(args$swept_param_1$EXPLO_TYPES)))
  if (length(par1$type)!=1) rlang::abort("Parameter 1 should be a single type of parameter.")

  if (par1$type == "EXPLO_n_FLUX_MATRICES"){
    par1$range <- args$swept_param_1[,c("VALUES_1", "VALUES_2")]
    par1$leng <- nrow(par1$range)
    par1$forcing_list <- par1$range
  } else if (par1$type == "EXPLO_n_ALPHA_MATRICES"){
    par1$range <- args$swept_param_1[,c("VALUES_1", "VALUES_2")]
    par1$leng <- nrow(par1$range)
    par1$forcing_list <- par1$range
  } else if (par1$type == "EXPLO_1_ALPHA"){
    par1$range <- seq(args$swept_param_1$ALPHA_MIN, args$swept_param_1$ALPHA_MAX,
                      by = args$swept_param_1$ALPHA_STEPS)
    par1$leng <- length(par1$range)
    par1$forcing_list <- data.frame(FROM = args$swept_param_1$FROM,
                                    TO = args$swept_param_1$TO,
                                    ALPHA = par1$range)
    par1$forcing_list$FROM_TO <- as.factor(paste(par1$forcing_list$FROM,
                                                 par1$forcing_list$TO, sep = "_"))
  } else if (par1$type == "EXPLO_1_SIZE"){
    par1$range <- seq(args$swept_param_1$SIZE_MIN, args$swept_param_1$SIZE_MAX, by = args$swept_param_1$SIZE_STEPS)
    par1$leng <- length(par1$range)
    par1$forcing_list <- data.frame(BOX_ID = args$swept_param_1$BOX_ID,
                                    SIZE.t0 = par1$range)

  } else if (par1$type == "EXPLO_1_DELTA"){
    par1$range <- seq(args$swept_param_1$DELTA_MIN, args$swept_param_1$DELTA_MAX, by = args$swept_param_1$DELTA_STEPS)
    par1$leng <- length(par1$range)
    par1$forcing_list <- data.frame(BOX_ID = args$swept_param_1$BOX_ID,
                                    DELTA.t0 = par1$range)
  } else if (par1$type == "EXPLO_1_RAYLEIGH_ALPHA"){
    par1$range <- seq(args$swept_param_1$ALPHA_0_MIN, args$swept_param_1$ALPHA_0_MAX, by = args$swept_param_1$ALPHA_0_STEPS)
    par1$leng <- length(par1$range)
    par1$forcing_list <- data.frame(XFROM = args$swept_param_1$XFROM,
                                    XTO = args$swept_param_1$XTO,
                                    YFROM = args$swept_param_1$YFROM,
                                    YTO = args$swept_param_1$YTO,
                                    AFROM = args$swept_param_1$AFROM,
                                    ATO = args$swept_param_1$ATO,
                                    ALPHA_0 = par1$range)
    par1$forcing_list$ALL_DESC <- as.factor(paste(par1$forcing_list$XFROM,
                                                  par1$forcing_list$XTO,
                                                  par1$forcing_list$YFROM,
                                                  par1$forcing_list$YTO,
                                                  par1$forcing_list$AFROM,
                                                  par1$forcing_list$ATO,
                                                  sep = "_"))
  }

  # __ii. EXPLO_AXIS_2 ####
  par2 <- list(type = as.character(unique(args$swept_param_1$EXPLO_TYPES)))
  if (length(par2$type)!=1) rlang::abort("Parameter 2 should be a single type of parameter.")
  if (args$swept_param_2[1,"EXPLO_TYPES"] == "EXPLO_n_FLUX_MATRICES"){
    par2$range <- args$swept_param_2[,c("VALUES_1", "VALUES_2")]
    par2$leng <- nrow(par2$range)
    par2$type <- as.character(args$swept_param_2[1,"EXPLO_TYPES"])
    par2$forcing_list <- par2$range
  } else if (args$swept_param_2[1,"EXPLO_TYPES"] == "EXPLO_n_ALPHA_MATRICES"){
    par2$range <- args$swept_param_2[,c("VALUES_1", "VALUES_2")]
    par2$leng <- nrow(par2$range)
    par2$type <- as.character(args$swept_param_2[1,"EXPLO_TYPES"])
    par2$forcing_list <- par2$range
  } else if (args$swept_param_2[1,"EXPLO_TYPES"] == "EXPLO_1_ALPHA"){
    par2$range <- seq(args$swept_param_2$ALPHA_MIN, args$swept_param_2$ALPHA_MAX, by = args$swept_param_2$ALPHA_STEPS)
    par2$leng <- length(par2$range)
    par2$type <- as.character(args$swept_param_2[1,"EXPLO_TYPES"])
    par2$forcing_list <- data.frame(FROM = args$swept_param_2$FROM,
                                    TO = args$swept_param_2$TO,
                                    ALPHA = par2$range)
    par2$forcing_list$FROM_TO <- as.factor(paste(par2$forcing_list$FROM, par2$forcing_list$TO, sep = "_"))

  } else if (args$swept_param_2[1,"EXPLO_TYPES"] == "EXPLO_1_SIZE"){
    par2$range <- seq(args$swept_param_2$SIZE_MIN, args$swept_param_2$SIZE_MAX, by = args$swept_param_2$SIZE_STEPS)
    par2$leng <- length(par2$range)
    par2$type <- as.character(args$swept_param_2[1,"EXPLO_TYPES"])
    par2$forcing_list <- data.frame(BOX_ID = args$swept_param_2$BOX_ID,
                                    SIZE.t0 = par2$range)

  } else if (args$swept_param_2[1,"EXPLO_TYPES"] == "EXPLO_1_DELTA"){
    par2$range <- seq(args$swept_param_2$DELTA_MIN, args$swept_param_2$DELTA_MAX, by = args$swept_param_2$DELTA_STEPS)
    par2$leng <- length(par2$range)
    par2$type <- as.character(args$swept_param_2[1,"EXPLO_TYPES"])
    par2$forcing_list <- data.frame(BOX_ID = args$swept_param_2$BOX_ID,
                                    DELTA.t0 = par2$range)

  } else if (args$swept_param_2[1,"EXPLO_TYPES"] == "EXPLO_1_RAYLEIGH_ALPHA"){
    par2$range <- seq(args$swept_param_2$ALPHA_0_MIN, args$swept_param_2$ALPHA_0_MAX, by = args$swept_param_2$ALPHA_0_STEPS)
    par2$leng <- length(par2$range)
    par2$type <- as.character(args$swept_param_2[1,"EXPLO_TYPES"])
    par2$forcing_list <- data.frame(XFROM = args$swept_param_2$XFROM,
                                    XTO = args$swept_param_2$XTO,
                                    YFROM = args$swept_param_2$YFROM,
                                    YTO = args$swept_param_2$YTO,
                                    AFROM = args$swept_param_2$AFROM,
                                    ATO = args$swept_param_2$ATO,
                                    ALPHA_0 = par2$range)
    par2$forcing_list$ALL_DESC <- as.factor(paste(par2$forcing_list$XFROM,
                                                  par2$forcing_list$XTO,
                                                  par2$forcing_list$YFROM,
                                                  par2$forcing_list$YTO,
                                                  par2$forcing_list$AFROM,
                                                  par2$forcing_list$ATO,
                                                  sep = "_"))
  }

  # __iii. CALCULATE TOTAL NUMBER OF RUNS #----
  tot_run <- par1$leng * par2$leng

  STOP_GO <- FALSE
  rlang::inform("________________________________________________________________________________")
  if(all(interactive() & args$ask_confirmation)){
    if (.Platform$OS.type == "windows"){
      STOP_GO <- utils::askYesNo(paste("? This sweep requires *", as.character(tot_run), "* independent runs, do you wish to carry on? \n"), default = TRUE)
    } else {
      STOP_GO <- utils::askYesNo(cat("? This sweep requires *", as.character(tot_run), "* independent runs, do you wish to carry on? \n"), default = TRUE)
    }
  } else {
    rlang::inform(paste("\U2139 This sweep requires ", as.character(tot_run), " independent runs."))
    STOP_GO <- TRUE
  }

  STOP_GO <- TRUE

  # IV. SWEEP THE SPACE OF PARAMETERS ####
  if (isFALSE(STOP_GO)){
    rlang::abort("\U2757 You probably want to reduce the number of iterations in each EXPLO axis.")
  } else {
    rlang::inform("________________________________________________________________________________")
    rlang::inform("\U0001f535 COMPUTING SWEEP of RUN #1 & #2 ")
    pb_cpt <- utils::txtProgressBar(min = 1, max = tot_run, style = 3, width = 60)
    clock <- 1
    k <- 1

    for (k in 1:par1$leng){

      l <- 1

      for (l in 1:par2$leng){

        # _a. RUN 1/2, i in [1:n] ####
        i <- 1
        # __ FORCING FROM EXPLO_DYN_MASTER ####
        args_run <- NULL
        args_run$flux_list <- master.sweep$RUN_SEQUENCE$flux_list[i] %>% as.character()
        args_run$coeff_list <- master.sweep$RUN_SEQUENCE$coeff_list[i] %>% as.character()
        args_run$t_max <- master.sweep$RUN_SEQUENCE$t_max[i]
        args_run$n_steps <- master.sweep$RUN_SEQUENCE$n_steps[i]

        if (nrow(master.sweep$FORCING_DELTA) > 0 & i %in% master.sweep$FORCING_DELTA$RUN_n){
          args_run$FORCING_DELTA <- master.sweep$FORCING_DELTA[master.sweep$FORCING_DELTA$RUN_n == i,
                                                               c("BOX_ID", "DELTA.t0")]
        }

        if (nrow(master.sweep$FORCING_SIZE) > 0 & i %in% master.sweep$FORCING_SIZE$RUN_n){
          args_run$FORCING_SIZE <- master.sweep$FORCING_SIZE[master.sweep$FORCING_SIZE$RUN_n == i,
                                                             c("BOX_ID", "SIZE.t0")]
        }

        if (nrow(master.sweep$FORCING_ALPHA) > 0 & i %in% master.sweep$FORCING_ALPHA$RUN_n){
          args_run$FORCING_ALPHA <- master.sweep$FORCING_ALPHA[master.sweep$FORCING_ALPHA$RUN_n == i,
                                                               c("FROM", "TO", "ALPHA")]
        }

        if (nrow(master.sweep$FORCING_RAYLEIGH) > 0  & i %in% master.sweep$FORCING_RAYLEIGH$RUN_n){
          args_run$FORCING_RAYLEIGH <- master.sweep$FORCING_RAYLEIGH %>%
            dplyr::filter(RUN_n == i) %>%
            dplyr::select(!RUN_n) %>%
            clear_subset()

          args_run$FORCING_RAYLEIGH$ALL_DESC <-
            as.factor(paste(args_run$FORCING_RAYLEIGH$XFROM,
                            args_run$FORCING_RAYLEIGH$XTO,
                            args_run$FORCING_RAYLEIGH$YFROM,
                            args_run$FORCING_RAYLEIGH$YTO,
                            args_run$FORCING_RAYLEIGH$AFROM,
                            args_run$FORCING_RAYLEIGH$ATO,
                            sep = "_"))
        }

        # __ FORCING FROM EXPLO RANGES 1 AND 2 - STEP 1 (VALUES_1) #----
        # __ UPDATE ISOPYRUN ARGUMENTS WITH EXPLO RANGES TO EXPLORE # OVERWRITE EXPLO_DYN_MASTER FORCINGS IF NEEDED #----
        # __ UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_1 VALUE #----
        if (par1$type == "EXPLO_n_FLUX_MATRICES"){
          args_run$flux_list <- as.character(par1$forcing_list[k,"VALUES_1"])
        } else if (par1$type == "EXPLO_n_ALPHA_MATRICES"){
          args_run$coeff_list <- as.character(par1$forcing_list[k,"VALUES_1"])
        } else if (par1$type == "EXPLO_1_ALPHA"){
          if (is.null(args_run$FORCING_ALPHA)){
            args_run$FORCING_ALPHA <- par1$forcing_list[k,] %>% clear_subset()
          } else {
            args_run$FORCING_ALPHA$FROM_TO <- paste0(args_run$FORCING_ALPHA$FROM, "_", args_run$FORCING_ALPHA$TO)
            args_run$FORCING_ALPHA <- args_run$FORCING_ALPHA %>%
              dplyr::filter(!FROM_TO %in% as.character(par1$forcing_list[k,"FROM_TO"])) %>%
              dplyr::bind_rows(par1$forcing_list[k,])
          }
        } else if (par1$type == "EXPLO_1_SIZE"){
          if (is.null(args_run$FORCING_SIZE)){
            args_run$FORCING_SIZE <- par1$forcing_list[k,]
          } else {
            args_run$FORCING_SIZE <- args_run$FORCING_SIZE %>%
              dplyr::filter(!BOX_ID %in% as.character(par1$forcing_list[k,"BOX_ID"])) %>%
              dplyr::bind_rows(par1$forcing_list[k,])
          }
        } else if (par1$type == "EXPLO_1_DELTA"){
          if (is.null(args_run$FORCING_DELTA)){
            args_run$FORCING_DELTA <- par1$forcing_list[k,]
          } else {
            args_run$FORCING_DELTA <- args_run$FORCING_DELTA %>%
              dplyr::filter(!BOX_ID %in% as.character(par1$forcing_list[k,"BOX_ID"])) %>%
              dplyr::bind_rows(par1$forcing_list[k,])
          }
        } else if (par1$type == "EXPLO_1_RAYLEIGH_ALPHA"){
          if(is.null(args_run$FORCING_RAYLEIGH)){
            args_run$FORCING_RAYLEIGH <- clear_subset(par1$forcing_list[k,])
          }  else {
            args_run$FORCING_RAYLEIGH <- args_run$FORCING_RAYLEIGH %>%
              dplyr::filter(!ALL_DESC %in% as.character(par1$forcing_list[k,"ALL_DESC"])) %>%
              dplyr::bind_rows(par1$forcing_list[k,])
          }
        }

        # __ UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_2 VALUE #----
        if (par2$type == "EXPLO_n_FLUX_MATRICES"){
          args_run$flux_list <- as.character(par2$forcing_list[l,"VALUES_1"])
        } else if (par2$type == "EXPLO_n_ALPHA_MATRICES"){
          args_run$coeff_list <- as.character(par2$forcing_list[l,"VALUES_1"])
        } else if (par2$type == "EXPLO_1_ALPHA"){
          if (is.null(args_run$FORCING_ALPHA)){
            args_run$FORCING_ALPHA <- par2$forcing_list[l,] %>% clear_subset()
          } else {
            args_run$FORCING_ALPHA$FROM_TO <- paste0(args_run$FORCING_ALPHA$FROM, "_", args_run$FORCING_ALPHA$TO)
            args_run$FORCING_ALPHA <- args_run$FORCING_ALPHA %>%
              dplyr::filter(!FROM_TO %in% as.character(par2$forcing_list[l,"FROM_TO"])) %>%
              dplyr::bind_rows(par2$forcing_list[l,])
          }
        } else if (par2$type == "EXPLO_1_SIZE"){
          if (is.null(args_run$FORCING_SIZE)){
            args_run$FORCING_SIZE <- par2$forcing_list[l,]
          } else {
            args_run$FORCING_SIZE <- args_run$FORCING_SIZE %>%
              dplyr::filter(!BOX_ID %in% as.character(par2$forcing_list[l,"BOX_ID"])) %>%
              dplyr::bind_rows(par2$forcing_list[l,])
          }
        } else if (par2$type == "EXPLO_1_DELTA"){
          if (is.null(args_run$FORCING_DELTA)){
            args_run$FORCING_DELTA <- par2$forcing_list[l,]
          } else {
            args_run$FORCING_DELTA <- args_run$FORCING_DELTA %>%
              dplyr::filter(!BOX_ID %in% as.character(par2$forcing_list[l,"BOX_ID"])) %>%
              dplyr::bind_rows(par2$forcing_list[l,])
          }
        } else if (par2$type == "EXPLO_1_RAYLEIGH_ALPHA"){
          if(is.null(args_run$FORCING_RAYLEIGH)){
            args_run$FORCING_RAYLEIGH <- clear_subset(par2$forcing_list[l,])
          }  else {
            args_run$FORCING_RAYLEIGH <- args_run$FORCING_RAYLEIGH %>%
              dplyr::filter(!ALL_DESC %in% as.character(par2$forcing_list[l,"ALL_DESC"])) %>%
              dplyr::bind_rows(par2$forcing_list[l,])
          }
        }

        # __ RUN  #----
        quiet(
          sim.single_run(workdir = args$workdir,
                         SERIES_ID = args$SERIES_ID,
                         flux_list = args_run$flux_list,
                         coeff_list = args_run$coeff_list,
                         t_max = args_run$t_max,
                         n_steps = args_run$n_steps,
                         # isobxr_master_file = args$isobxr_master_file,
                         isobxr_master = isobxr_master,
                         suppress_messages = TRUE,
                         export.diagrams = FALSE,
                         export.delta_plot = FALSE,
                         export.data_as_csv_xlsx = FALSE,
                         plot.time_unit = args$plot.time_unit,
                         show.delta_plot = FALSE,
                         inspect_inputs = FALSE,
                         save_outputs = FALSE,
                         return_data = FALSE,
                         FORCING_RAYLEIGH = args_run$FORCING_RAYLEIGH,
                         FORCING_SIZE = args_run$FORCING_SIZE,
                         FORCING_DELTA = args_run$FORCING_DELTA,
                         FORCING_ALPHA = args_run$FORCING_ALPHA,
                         COMPOSITE = FALSE,
                         COMPO_SERIES_n = NaN,
                         COMPO_SERIES_FAMILY = NaN,
                         EXPLORER = TRUE,
                         EXPLO_SERIES_n = paths$EXPLO_SERIES_n,
                         EXPLO_SERIES_FAMILY = paths$EXPLO_SERIES_FAMILY)
        )

        # _b. RUN 2/2, i in [1:n] #----
        i <- 2
        # __ PREPARING INPUTS for ISOPY_RUN with EXPLO_MASTER as default and Taking final state of run 2/2 as initial #----
        args_run <- NULL
        args_run$flux_list <- master.sweep$RUN_SEQUENCE$flux_list[i] %>% as.character()
        args_run$coeff_list <- master.sweep$RUN_SEQUENCE$coeff_list[i] %>% as.character()
        args_run$t_max <- master.sweep$RUN_SEQUENCE$t_max[i]
        args_run$n_steps <- master.sweep$RUN_SEQUENCE$n_steps[i]

        LOG <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T)
        LOG_last <- LOG[nrow(LOG),]
        remove(LOG)

        # FORCING_DELTA from previous run
        args_run$FORCING_DELTA <- readRDS(to_tmpdir(paste(LOG_last$path_outdir, ".rds", sep = "")))$outputs$final_state %>%
          dplyr::select(BOX_ID, DELTA.t_max) %>%
          dplyr::rename("DELTA.t0" = "DELTA.t_max") %>%
          clear_subset()

        # FORCING_SIZE from previous run
        args_run$FORCING_SIZE <- readRDS(to_tmpdir(paste(LOG_last$path_outdir, ".rds", sep = "")))$outputs$final_state %>%
          dplyr::select(BOX_ID, SIZE.t_max) %>%
          dplyr::rename("SIZE.t0" = "SIZE.t_max") %>%
          clear_subset()

        # FORCING_DELTA from master.sweep
        if (nrow(master.sweep$FORCING_DELTA) > 0 & i %in% master.sweep$FORCING_DELTA$RUN_n){
          args_run$FORCING_DELTA <- args_run$FORCING_DELTA %>%
            dplyr::filter(!BOX_ID %in% (master.sweep$FORCING_DELTA[master.sweep$FORCING_DELTA$RUN_n == i,
                                                                   c("BOX_ID", "DELTA.t0")] %>%
                                          dplyr::pull(BOX_ID) %>% as.character())) %>%
            dplyr::bind_rows(master.sweep$FORCING_DELTA[master.sweep$FORCING_DELTA$RUN_n == i,
                                                        c("BOX_ID", "DELTA.t0")]) %>%
            dplyr::arrange(BOX_ID)
        }

        # FORCING_SIZE from master.sweep
        if (nrow(master.sweep$FORCING_SIZE) > 0 & i %in% master.sweep$FORCING_SIZE$RUN_n){
          args_run$FORCING_SIZE <- args_run$FORCING_SIZE %>%
            dplyr::filter(!BOX_ID %in% (master.sweep$FORCING_SIZE[master.sweep$FORCING_SIZE$RUN_n == i,
                                                                  c("BOX_ID", "SIZE.t0")] %>%
                                          dplyr::pull(BOX_ID) %>% as.character())) %>%
            dplyr::bind_rows(master.sweep$FORCING_SIZE[master.sweep$FORCING_SIZE$RUN_n == i,
                                                       c("BOX_ID", "SIZE.t0")]) %>%
            dplyr::arrange(BOX_ID)
        }

        # FORCING_ALPHA from master.sweep
        if (nrow(master.sweep$FORCING_ALPHA) > 0 & i %in% master.sweep$FORCING_ALPHA$RUN_n){
          args_run$FORCING_ALPHA <- master.sweep$FORCING_ALPHA[master.sweep$FORCING_ALPHA$RUN_n == i,
                                                               c("FROM", "TO", "ALPHA")] %>%
            dplyr::mutate(FROM_TO = paste0(FROM, "_", TO))
        }

        # FORCING_RAYLEIGH from master.sweep
        if (nrow(master.sweep$FORCING_RAYLEIGH) > 0  & i %in% master.sweep$FORCING_RAYLEIGH$RUN_n){
          args_run$FORCING_RAYLEIGH <- master.sweep$FORCING_RAYLEIGH %>%
            dplyr::filter(RUN_n == i) %>%
            dplyr::select(!RUN_n) %>%
            clear_subset()

          args_run$FORCING_RAYLEIGH$ALL_DESC <-
            as.factor(paste(args_run$FORCING_RAYLEIGH$XFROM,
                            args_run$FORCING_RAYLEIGH$XTO,
                            args_run$FORCING_RAYLEIGH$YFROM,
                            args_run$FORCING_RAYLEIGH$YTO,
                            args_run$FORCING_RAYLEIGH$AFROM,
                            args_run$FORCING_RAYLEIGH$ATO,
                            sep = "_"))
        }

        # __ FORCING FROM EXPLO RANGES 1 AND 2 - STEP 1 (VALUES_1) #----
        # __ UPDATE ISOPYRUN ARGUMENTS WITH EXPLO RANGES TO EXPLORE # OVERWRITE EXPLO_DYN_MASTER FORCINGS IF NEEDED #----
        # __ UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_1 VALUE #----
        if (par1$type == "EXPLO_n_FLUX_MATRICES"){
          args_run$flux_list <- as.character(par1$forcing_list[k,"VALUES_2"])
        } else if (par1$type == "EXPLO_n_ALPHA_MATRICES"){
          args_run$coeff_list <- as.character(par1$forcing_list[k,"VALUES_2"])
        } else if (par1$type == "EXPLO_1_ALPHA"){
          if (is.null(args_run$FORCING_ALPHA)){
            args_run$FORCING_ALPHA <- par1$forcing_list[k,] %>% clear_subset()
          } else {
            args_run$FORCING_ALPHA$FROM_TO <- paste0(args_run$FORCING_ALPHA$FROM, "_", args_run$FORCING_ALPHA$TO)
            args_run$FORCING_ALPHA <- args_run$FORCING_ALPHA %>%
              dplyr::filter(!FROM_TO %in% as.character(par1$forcing_list[k,"FROM_TO"])) %>%
              dplyr::bind_rows(par1$forcing_list[k,])
          }
        } else if (par1$type == "EXPLO_1_SIZE"){
          if (is.null(args_run$FORCING_SIZE)){
            args_run$FORCING_SIZE <- par1$forcing_list[k,]
          } else {
            args_run$FORCING_SIZE <- args_run$FORCING_SIZE %>%
              dplyr::filter(!BOX_ID %in% as.character(par1$forcing_list[k,"BOX_ID"])) %>%
              dplyr::bind_rows(par1$forcing_list[k,])
          }
        } else if (par1$type == "EXPLO_1_DELTA"){
          if (is.null(args_run$FORCING_DELTA)){
            args_run$FORCING_DELTA <- par1$forcing_list[k,]
          } else {
            args_run$FORCING_DELTA <- args_run$FORCING_DELTA %>%
              dplyr::filter(!BOX_ID %in% as.character(par1$forcing_list[k,"BOX_ID"])) %>%
              dplyr::bind_rows(par1$forcing_list[k,])
          }
        } else if (par1$type == "EXPLO_1_RAYLEIGH_ALPHA"){
          if(is.null(args_run$FORCING_RAYLEIGH)){
            args_run$FORCING_RAYLEIGH <- clear_subset(par1$forcing_list[k,])
          }  else {
            args_run$FORCING_RAYLEIGH <- args_run$FORCING_RAYLEIGH %>%
              dplyr::filter(!ALL_DESC %in% as.character(par1$forcing_list[k,"ALL_DESC"])) %>%
              dplyr::bind_rows(par1$forcing_list[k,])
          }
        }

        # __ UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_2 VALUE #----
        if (par2$type == "EXPLO_n_FLUX_MATRICES"){
          args_run$flux_list <- as.character(par2$forcing_list[l,"VALUES_2"])
        } else if (par2$type == "EXPLO_n_ALPHA_MATRICES"){
          args_run$coeff_list <- as.character(par2$forcing_list[l,"VALUES_2"])
        } else if (par2$type == "EXPLO_1_ALPHA"){
          if (is.null(args_run$FORCING_ALPHA)){
            args_run$FORCING_ALPHA <- par2$forcing_list[l,] %>% clear_subset()
          } else {
            args_run$FORCING_ALPHA$FROM_TO <- paste0(args_run$FORCING_ALPHA$FROM, "_", args_run$FORCING_ALPHA$TO)
            args_run$FORCING_ALPHA <- args_run$FORCING_ALPHA %>%
              dplyr::filter(!FROM_TO %in% as.character(par2$forcing_list[l,"FROM_TO"])) %>%
              dplyr::bind_rows(par2$forcing_list[l,])
          }
        } else if (par2$type == "EXPLO_1_SIZE"){
          if (is.null(args_run$FORCING_SIZE)){
            args_run$FORCING_SIZE <- par2$forcing_list[l,]
          } else {
            args_run$FORCING_SIZE <- args_run$FORCING_SIZE %>%
              dplyr::filter(!BOX_ID %in% as.character(par2$forcing_list[l,"BOX_ID"])) %>%
              dplyr::bind_rows(par2$forcing_list[l,])
          }
        } else if (par2$type == "EXPLO_1_DELTA"){
          if (is.null(args_run$FORCING_DELTA)){
            args_run$FORCING_DELTA <- par2$forcing_list[l,]
          } else {
            args_run$FORCING_DELTA <- args_run$FORCING_DELTA %>%
              dplyr::filter(!BOX_ID %in% as.character(par2$forcing_list[l,"BOX_ID"])) %>%
              dplyr::bind_rows(par2$forcing_list[l,])
          }
        } else if (par2$type == "EXPLO_1_RAYLEIGH_ALPHA"){
          if(is.null(args_run$FORCING_RAYLEIGH)){
            args_run$FORCING_RAYLEIGH <- clear_subset(par2$forcing_list[l,])
          }  else {
            args_run$FORCING_RAYLEIGH <- args_run$FORCING_RAYLEIGH %>%
              dplyr::filter(!ALL_DESC %in% as.character(par2$forcing_list[l,"ALL_DESC"])) %>%
              dplyr::bind_rows(par2$forcing_list[l,])
          }
        }

        # # __ FORCING FROM EXPLO RANGES 1 AND 2 - STEP 2 (VALUES_2) #----
        # # __ UPDATE ISOPYRUN ARGUMENTS WITH EXPLO RANGES TO EXPLORE # OVERWRITE EXPLO_MASTER FORCINGS IF NEEDED #----
        # # __ UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_1 VALUE #----
        # if (par1$type == "EXPLO_n_FLUX_MATRICES"){            ##### IF
        #   args_run$flux_list <- as.character(par1$forcing_list[k,"VALUES_2"])
        # } else if (par1$type == "EXPLO_n_ALPHA_MATRICES"){    ##### IF
        #   a <- as.character(par1$forcing_list[k,"VALUES_2"])
        # } else if (par1$type == "EXPLO_1_ALPHA"){             ##### IF
        #   if (is.null(FORCING_ALPHA_loc)){
        #     FORCING_ALPHA_loc <- clear_subset(par1$forcing_list[k,])
        #   } else if (par1$forcing_list[k,"FROM_TO"] %in% FORCING_ALPHA_loc$FROM_TO){
        #     FORCING_ALPHA_loc[FORCING_ALPHA_loc$FROM_TO == as.character(par1$forcing_list[k,"FROM_TO"]), "ALPHA"] <- par1$forcing_list[k,"ALPHA"]
        #   } else {
        #     FORCING_ALPHA_loc <- rbind(FORCING_ALPHA_loc, par1$forcing_list[k,])
        #   }
        # } else if (par1$type == "EXPLO_1_SIZE"){              ##### IF
        #   LOC_SIZE.t0[LOC_SIZE.t0$BOX_ID == as.character(par1$forcing_list[k, "BOX_ID"]), "SIZE.t0"] <- par1$forcing_list[k,"SIZE.t0"]
        # } else if (par1$type == "EXPLO_1_DELTA"){             ##### IF
        #   LOC_DELTA.t0[LOC_DELTA.t0$BOX_ID == as.character(par1$forcing_list[k, "BOX_ID"]), "DELTA.t0"] <- par1$forcing_list[k,"DELTA.t0"]
        # } else if (par1$type == "EXPLO_1_RAYLEIGH_ALPHA"){    ##### IF
        #   if(is.null(LOC_RAYLEIGH)){
        #     LOC_RAYLEIGH <- clear_subset(par1$forcing_list[k,])
        #   } else if (par1$forcing_list[k,"ALL_DESC"] %in% as.character(LOC_RAYLEIGH$ALL_DESC)){
        #     LOC_RAYLEIGH[LOC_RAYLEIGH$ALL_DESC == par1$forcing_list[k,"ALL_DESC"], "ALPHA_0"] <- par1$forcing_list[k,"ALPHA_0"]
        #   } else {
        #     LOC_RAYLEIGH <- rbind(LOC_RAYLEIGH, par1$forcing_list[k,])
        #   }
        # }
        #
        # # __ UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_2 VALUE #----
        # if (par2$type == "EXPLO_n_FLUX_MATRICES"){            ##### IF
        #   args_run$flux_list <- as.character(par2$forcing_list[l,"VALUES_2"])
        # } else if (par2$type == "EXPLO_n_ALPHA_MATRICES"){    ##### IF
        #   a <- as.character(par2$forcing_list[l,"VALUES_2"])
        # } else if (par2$type == "EXPLO_1_ALPHA"){             ##### IF
        #   if (is.null(FORCING_ALPHA_loc)){
        #     FORCING_ALPHA_loc <- clear_subset(par2$forcing_list[l,])
        #   } else if (par2$forcing_list[l,"FROM_TO"] %in% FORCING_ALPHA_loc$FROM_TO){
        #     FORCING_ALPHA_loc[FORCING_ALPHA_loc$FROM_TO == as.character(par2$forcing_list[l,"FROM_TO"]), "ALPHA"] <- par2$forcing_list[l,"ALPHA"]
        #   } else {
        #     FORCING_ALPHA_loc <- rbind(FORCING_ALPHA_loc, par2$forcing_list[l,])
        #   }
        # } else if (par2$type == "EXPLO_1_SIZE"){              ##### IF
        #   LOC_SIZE.t0[LOC_SIZE.t0$BOX_ID == as.character(par2$forcing_list[l, "BOX_ID"]), "SIZE.t0"] <- par2$forcing_list[l,"SIZE.t0"]
        # } else if (par2$type == "EXPLO_1_DELTA"){             ##### IF
        #   LOC_DELTA.t0[LOC_DELTA.t0$BOX_ID == as.character(par2$forcing_list[l, "BOX_ID"]), "DELTA.t0"] <- par2$forcing_list[l,"DELTA.t0"]
        # } else if (par2$type == "EXPLO_1_RAYLEIGH_ALPHA"){    ##### IF
        #   if(is.null(LOC_RAYLEIGH)){
        #     LOC_RAYLEIGH <- clear_subset(par2$forcing_list[l,])
        #   } else if (par2$forcing_list[l,"ALL_DESC"] %in% as.character(LOC_RAYLEIGH$ALL_DESC)){
        #     LOC_RAYLEIGH[LOC_RAYLEIGH$ALL_DESC == par2$forcing_list[l,"ALL_DESC"], "ALPHA_0"] <- par2$forcing_list[l,"ALPHA_0"]
        #   } else {
        #     LOC_RAYLEIGH <- rbind(LOC_RAYLEIGH, par2$forcing_list[l,])
        #   }
        # }

        # __ RUN #----
        # quiet(run_isobxr(workdir = LOC_workdir,
        #                  SERIES_ID = args$SERIES_ID,
        #                  flux_list_name = args_run$flux_list,
        #                  coeff_list_name = a,
        #                  t_lim = LOC_t_lim,
        #                  nb_steps = LOC_nb_steps,
        #                  time_units,
        #                  FORCING_RAYLEIGH <- LOC_RAYLEIGH,
        #                  FORCING_SIZE <- LOC_SIZE.t0,
        #                  FORCING_DELTA <- LOC_DELTA.t0,
        #                  FORCING_ALPHA <-  FORCING_ALPHA_loc,
        #                  COMPOSITE = FALSE,
        #                  COMPO_SERIES_n = NaN,
        #                  COMPO_SERIES_FAMILY = NaN,
        #                  EXPLORER = TRUE,
        #                  EXPLO_SERIES_n = paths$EXPLO_SERIES_n,
        #                  EXPLO_SERIES_FAMILY = paths$EXPLO_SERIES_FAMILY,
        #                  HIDE_PRINTS = TRUE,
        #                  to_DIGEST_DIAGRAMS = FALSE,
        #                  to_DIGEST_evD_PLOT = FALSE,
        #                  plot_results = FALSE))

        # __ RUN  #----
        quiet(
          sim.single_run(workdir = args$workdir,
                         SERIES_ID = args$SERIES_ID,
                         flux_list = args_run$flux_list,
                         coeff_list = args_run$coeff_list,
                         t_max = args_run$t_max,
                         n_steps = args_run$n_steps,
                         # isobxr_master_file = args$isobxr_master_file,
                         isobxr_master = isobxr_master,
                         suppress_messages = TRUE,
                         export.diagrams = FALSE,
                         export.delta_plot = FALSE,
                         export.data_as_csv_xlsx = FALSE,
                         plot.time_unit = args$plot.time_unit,
                         show.delta_plot = FALSE,
                         inspect_inputs = FALSE,
                         save_outputs = FALSE,
                         return_data = FALSE,
                         FORCING_RAYLEIGH = args_run$FORCING_RAYLEIGH,
                         FORCING_SIZE = args_run$FORCING_SIZE,
                         FORCING_DELTA = args_run$FORCING_DELTA,
                         FORCING_ALPHA = args_run$FORCING_ALPHA,
                         COMPOSITE = FALSE,
                         COMPO_SERIES_n = NaN,
                         COMPO_SERIES_FAMILY = NaN,
                         EXPLORER = TRUE,
                         EXPLO_SERIES_n = paths$EXPLO_SERIES_n,
                         EXPLO_SERIES_FAMILY = paths$EXPLO_SERIES_FAMILY)
        )

        # __ CLOCK #----
        utils::setTxtProgressBar(pb_cpt, clock)

        clock <- clock + 1
        l <- l + 1
      }
      k <- k + 1
    }
  }
  close(pb_cpt)

  # V. SUMMARY of EXPLOR SPACE #----
  if (par1$type %in% c("EXPLO_n_ALPHA_MATRICES", "EXPLO_n_FLUX_MATRICES")){
    par1$toEXPLOG <- args$swept_param_1 %>%
      dplyr::mutate(VALUES = paste(VALUES_1, VALUES_2, sep = ".")) %>%
      dplyr::select(EXPLO_TYPES, VALUES)
  } else {
    par1$toEXPLOG <- par1$forcing_list %>%
      dplyr::mutate(EXPLO_TYPES = as.factor(par1$type))
  }

  if (par2$type %in% c("EXPLO_n_ALPHA_MATRICES", "EXPLO_n_FLUX_MATRICES")){
    par2$toEXPLOG <- args$swept_param_2 %>%
      dplyr::mutate(VALUES = paste(VALUES_1, VALUES_2, sep = ".")) %>%
      dplyr::select(EXPLO_TYPES, VALUES)
  } else {
    par2$toEXPLOG <- par2$forcing_list %>%
      dplyr::mutate(EXPLO_TYPES = as.factor(par2$type))
  }

  names(par1$toEXPLOG) <- paste(names(par1$toEXPLOG), "_1", sep = "")
  names(par2$toEXPLOG) <- paste(names(par2$toEXPLOG), "_2", sep = "")

  k <- 1
  Run_n <- c(1,2)
  count <- 1
  for (k in 1:par1$leng){
    l <- 1
    for (l in 1:par2$leng){
      EXPLOG_loc <- cbind(par1$toEXPLOG[k,], par2$toEXPLOG[l,])
      EXPLOG_loc$EXPLO_SERIES_n <- paths$EXPLO_SERIES_n
      EXPLOG_loc$EXPLO_SERIES_FAMILY <- paths$EXPLO_SERIES_FAMILY
      EXPLOG_loc <- rbind(EXPLOG_loc, EXPLOG_loc)
      EXPLOG_loc$RUN_n <- Run_n
      if (count == 1){
        EXPLOG <- EXPLOG_loc
      } else {
        EXPLOG <- rbind(EXPLOG, EXPLOG_loc)
      }
      l <- l + 1
      Run_n <- Run_n + 2
      count <- count <- 1 + 1
    }
    k <- k + 1
  }
  remove(EXPLOG_loc)

  EXPLOG <- clear_subset(EXPLOG)

  if (par1$type %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
    EXPLOG$VAR_EXPLO_1 <- EXPLOG$VALUES_1
    EXPLOG$LEGEND_EXPLO_1 <- EXPLOG$EXPLO_TYPES_1
  } else if (par1$type == "EXPLO_1_SIZE"){
    EXPLOG$VAR_EXPLO_1 <- EXPLOG$SIZE.t0_1
    EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("SIZE_t0_", EXPLOG$BOX_ID_1, sep = ""))
  } else if (par1$type == "EXPLO_1_DELTA"){
    EXPLOG$VAR_EXPLO_1 <- EXPLOG$DELTA.t0_1
    EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("DELTA_t0_", EXPLOG$BOX_ID_1, sep = ""))
  } else if (par1$type == "EXPLO_1_ALPHA"){
    EXPLOG$VAR_EXPLO_1 <- EXPLOG$ALPHA_1
    EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("ALPHA_", EXPLOG$FROM_TO_1, sep = ""))
  } else if (par1$type == "EXPLO_1_RAYLEIGH_ALPHA"){
    EXPLOG$VAR_EXPLO_1 <- EXPLOG$ALPHA_0_1
    EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("ALPHA_", EXPLOG$ALL_DESC_1, sep = ""))
  }

  if (par2$type %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
    EXPLOG$VAR_EXPLO_2 <- EXPLOG$VALUES_2
    EXPLOG$LEGEND_EXPLO_2 <- EXPLOG$EXPLO_TYPES_2
  } else if (par2$type == "EXPLO_1_SIZE"){
    EXPLOG$VAR_EXPLO_2 <- EXPLOG$SIZE.t0_2
    EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("SIZE_t0_", EXPLOG$BOX_ID_2, sep = ""))
  } else if (par2$type == "EXPLO_1_DELTA"){
    EXPLOG$VAR_EXPLO_2 <- EXPLOG$DELTA.t0_2
    EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("DELTA_t0_", EXPLOG$BOX_ID_2, sep = ""))
  } else if (par2$type == "EXPLO_1_ALPHA"){
    EXPLOG$VAR_EXPLO_2 <- EXPLOG$ALPHA_2
    EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("ALPHA_", EXPLOG$FROM_TO_2, sep = ""))
  } else if (par2$type == "EXPLO_1_RAYLEIGH_ALPHA"){
    EXPLOG$VAR_EXPLO_2 <- EXPLOG$ALPHA_0_2
    EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("ALPHA_", EXPLOG$ALL_DESC_2, sep = ""))
  }

  EXPLOG <- clear_subset(EXPLOG)

  # LOAD/EDIT COMPOSITE SERIES LOG/OUT FILES and EDIT ANA evS ####
  # LOAD LOG/OUT FILES of CURRENT COMPO SERIES ####
  LOG_SERIES <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T) %>%
    dplyr::filter(SERIES_ID == args$SERIES_ID) %>%
    dplyr::full_join(EXPLOG, by = c("RUN_n", "EXPLO_SERIES_FAMILY", "EXPLO_SERIES_n")) %>%
    clear_subset()

  BOX_IDs <- (LOG_SERIES %>%
                head(1) %>%
                dplyr::pull(path_outdir) %>%
                as.character() %>%
                paste0(".rds") %>%
                to_tmpdir() %>%
                readRDS())$inputs$bx.groups$all

  # READ/BUILD/MERGE evS/D and evS/D final for ANA/NUM WHOLE COMPOSITE RUN ####
  rlang::inform("________________________________________________________________________________")
  rlang::inform("\U0001f535 PREPARING RESULTS")

  paths$paths_all_rds <- LOG_SERIES$path_outdir %>% as.character()

  # used for reading single outputs in sweep.final_nD prior to merging
  # to move outside - attention to LOG_SERIES and BOX_ID actually passed on
  read_single_run_outputs_for_DYN2D <- function(path_to_rds){

    # path_to_rds <- paths$paths_all_rds[1]

    header_sep <- "." ### change to . in the future

    metadata <- LOG_SERIES %>%
      dplyr::filter(path_outdir == path_to_rds ) %>%
      dplyr::select(NUM_ANA,
                    SERIES_RUN_ID,
                    RUN_n,
                    LEGEND_EXPLO_1,
                    VAR_EXPLO_1,
                    LEGEND_EXPLO_2,
                    VAR_EXPLO_2)

    metadata <- metadata %>% dplyr::bind_cols(
      path_to_rds %>%
        paste0(".rds") %>%
        to_tmpdir() %>%
        readRDS() %>%
        magrittr::extract2("inputs") %>%
        magrittr::extract2("INITIAL") %>%
        dplyr::rename("m0" = "SIZE.t0",
                      "d0" = "DELTA.t0") %>%
        tidyr::pivot_wider(names_from = BOX_ID,
                           values_from = c(d0, m0),
                           names_sep = header_sep) %>%
        as.data.frame())

    metadata.fluxes <- path_to_rds %>%
      paste0(".rds") %>%
      to_tmpdir() %>%
      readRDS() %>%
      magrittr::extract2("inputs") %>%
      magrittr::extract2("FLUXES") %>%
      tidyr::pivot_longer(cols = BOX_IDs) %>%
      dplyr::rename("FROM" = "BOX_ID",
                    "TO" = "name") %>%
      dplyr::filter(value != 0) %>%
      tidyr::unite(col = "FROM_TO", FROM, TO, sep = "_") %>%
      dplyr::mutate(FROM_TO = paste("f", FROM_TO, sep = header_sep)) %>%
      tidyr::pivot_wider(values_from = value,
                         names_from = FROM_TO) %>%
      as.data.frame()

    metadata.coeffs <- path_to_rds %>%
      paste0(".rds") %>%
      to_tmpdir() %>%
      readRDS() %>%
      magrittr::extract2("inputs") %>%
      magrittr::extract2("COEFFS") %>%
      tidyr::pivot_longer(cols = BOX_IDs) %>%
      dplyr::rename("FROM" = "BOX_ID",
                    "TO" = "name") %>%
      dplyr::filter(value != 1) %>%
      tidyr::unite(col = "FROM_TO", FROM, TO, sep = "_") %>%
      dplyr::mutate(FROM_TO = paste("a", FROM_TO, sep = header_sep)) %>%
      tidyr::pivot_wider(values_from = value,
                         names_from = FROM_TO) %>%
      as.data.frame()

    if (nrow(metadata.fluxes) > 0) metadata <- dplyr::bind_cols(metadata, metadata.fluxes)
    if (nrow(metadata.coeffs) > 0) metadata <- dplyr::bind_cols(metadata, metadata.coeffs)


    evDS <- dplyr::full_join(
      path_to_rds %>%
        paste0(".rds") %>%
        to_tmpdir() %>%
        readRDS() %>%
        magrittr::extract2("outputs") %>%
        magrittr::extract2("delta_vs_t") %>%
        dplyr::rename_with(
          ~ paste0("d.", .x, recycle0 = TRUE),
          !dplyr::starts_with("Time")
        ),
      path_to_rds %>%
        paste0(".rds") %>%
        to_tmpdir() %>%
        readRDS() %>%
        magrittr::extract2("outputs") %>%
        magrittr::extract2("size_vs_t") %>%
        dplyr::rename_with(
          ~ paste0("m.", .x, recycle0 = TRUE),
          !dplyr::starts_with("Time")
        ),
      by = "Time") %>%
      dplyr::bind_cols(metadata) %>%
      dplyr::mutate(path_outdir = path_to_rds)

    remove(metadata.coeffs, metadata.fluxes, metadata)
    return(evDS)
  }

  evDS <- paths$paths_all_rds %>%
    purrr::map(read_single_run_outputs_for_DYN2D) %>%
    purrr::reduce(dplyr::bind_rows)

  # EDIT CSV for WHOLE COMPOSITE RUN evS - evD - LOG - OUT ####
  rlang::inform("________________________________________________________________________________")
  rlang::inform("\U0001f535 WRITING DIGEST")

  # path_out_EXPLO <- paste("4_", as.character(args$SERIES_ID), "/", "0_DYN_DIGEST/", sep = "")
  paths$digest_dir <- paste("4_", as.character(args$SERIES_ID), "/",
                            names.output[names.output$func == "sweep.dyn_2D", "digest_dir"],
                            sep = "")
  # if (!dir.exists(to_tmpdir(path_out_EXPLO))){dir.create(to_tmpdir(path_out_EXPLO))}
  # path_out_EXPLO <- paste(path_out_EXPLO, args$SERIES_ID, sep = "")
  if (!dir.exists(to_tmpdir(paths$digest_dir))){dir.create(to_tmpdir(paths$digest_dir))}
  paths$digest_root <- paste(paths$digest_dir, "/", as.character(args$SERIES_ID), sep = "")
  paths$digest_log <- paste(paths$digest_root, "_meta_LOG.csv", sep = "")
  paths$digest_results <- paste(paths$digest_root, "_results.rds", sep = "")
  paths$digest_sweep_master <- paste(paths$digest_root, "_in_master_sweep.xlsx", sep = "")
  paths$digest_isobxr_master <- paste(paths$digest_root, "_in_master_isobxr.xlsx", sep = "")

  # data.table::fwrite(LOG_SERIES, file = paste(to_tmpdir(path_out_EXPLO), "_LOG.csv", sep = ""), row.names = F, quote = F)
  if(args$export.data_as_csv_xlsx){
    data.table::fwrite(LOG_SERIES,
                       file = to_tmpdir(paths$digest_log),
                       row.names = F, quote = F)
    data.table::fwrite(evDS, file = paste(to_tmpdir(paths$digest_root), "_out_delta_size_vs_t.csv", sep = ""), row.names = F, quote = F)
  }

  # _d. edit master xlsx archives ####
  writexl::write_xlsx(master.sweep,
                      to_tmpdir(paths$digest_sweep_master))

  writexl::write_xlsx(isobxr_master[!names(isobxr_master) %in% c("bx.groups")],
                      to_tmpdir(paths$digest_isobxr_master))

  # _e. edit rds results ####
  results <- list(delta_size_vs_t = evDS,
                  sweeep_master = master.sweep,
                  sweep_log = LOG_SERIES,
                  isobxr_master = isobxr_master,
                  paths = paths)

  saveRDS(object = results,
          file = to_tmpdir(paths$digest_results))

  # PLOT DEFAULT DELTAS FROM TEMPDIR ####

  if(args$show.delta_plot){
    plot.sweep <- plot_dyn_2D(workdir = to_tmpdir(""),
                              sweep_dir_name = paste0("4_", paths$SERIES_ID),
                              time_unit = NULL,
                              hidden_boxes = NULL,
                              return_as_print = FALSE,
                              free_y_scale = TRUE,
                              swap_sweep_params = FALSE)
  }

  # save_run_outputs or not ####

  #### edit pdf of evD/evS multiplot
  if(!(fun_mode$tuto_mode)){
    pdf_path <- paste(paths$digest_root, "_plot_all_vs_t.pdf", sep = "")
    dev.new()
    pdf(to_tmpdir(pdf_path),
        width = 21/2.54, height = 29.7/2.54,
        pointsize = 1, useDingbats=FALSE)
    suppressWarnings(print(plot.sweep$plot.delta_vs_t))
    suppressWarnings(print(plot.sweep$plot.size_vs_t))
    graphics.off()
  }

  # dispose of individual run outputs

  if (!args$keep_single_run_rds){

    unlink_rds <- function(x){
      unlink(to_tmpdir(x), recursive = TRUE)
    }

    unlinked <- paths$paths_all_rds %>%
      paste0(".rds") %>%
      purrr::map(unlink_rds)

    remove("unlinked")

  }

  rlang::inform("________________________________________________________________________________")
  rlang::inform(message = paste("\U2139 Run outputs (stored in temporary directory):", sep = ""))
  fs::dir_tree(path = to_tmpdir(""), recurse = T)
  rlang::inform("________________________________________________________________________________")

  if(!args$save_outputs){
    rlang::inform("\U2757 Results were not saved to working directory (set save_run_outputs = TRUE to save results).")
    rlang::inform("\U2139 You can explore the results with more parameters by using the plot_dyn_2D() function (requires saved outputs).")
  } else if(args$save_outputs){
    R.utils::copyDirectory(to_tmpdir(""),
                           getwd(),
                           overwrite = T)
    rlang::inform("\U2705 Results were successfully saved to working directory.")
    rlang::inform("\U2139 You can explore the results with more parameters by using the plot_dyn_2D() function.")
  }

  in_silence <- function(...)
  {
    mc <- match.call()[-1]
    a <- utils::capture.output(
      tryCatch(
        suppressMessages(suppressWarnings(
          eval(as.list(mc)[[1]])
        )), error = function(e) ""))
  }

  if(args$show.delta_plot){
    return(in_silence(plot.sweep$plot.delta_vs_t))
  }

}
