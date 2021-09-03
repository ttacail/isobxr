#  #_________________________________________________________________________80char
#' Sweep the space of two parameters at the final state of a run
#' @description  A function to assess the influence of two parameters (varying
#' over a range of values) on the final state of a given model.
#'
#' @param workdir Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
#' of the steady sweep master file (e.g., \strong{\emph{0_EXPLO_STEADY_MASTER.xlsx}}) \cr
#' and where output files will be stored if exported by user. \cr
#' (character string)
#'
#' @param SERIES_ID Name of the sweep series the run belongs to. \cr
#' It determines the folder in which the output files will be stored for this sweep run.\cr
#' A sweep run number is automatically linked to it,
#' and subsequent sweep runs can not overwrite a previous one.\cr
#' (character string)
#'
#' @param time_units Vector defining the initial time unit
#' (identical to unit used in fluxes), \cr
#' followed by the time unit used for the graphical output.\cr
#' Character string, to be selected  among the following:\cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}\cr
#' e.g.,  c("d", "yr") to convert days into years
#'
#' @param EXPLO_MASTER Name of the steady sweep master file (e.g., \strong{\emph{0_EXPLO_STEADY_MASTER.xlsx}}),
#' defining the steady sweep run scenario. \cr
#' (character string)
#'
#' @param EXPLO_AXIS_1 Set of values of sweeping parameter 1. \cr
#' See Vignette for further details.
#'
#' @param EXPLO_AXIS_2 Set of values of sweeping parameter 2. \cr
#' See Vignette for further details.
#'
#' @param to_STD_DIGEST_CSVs \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Exports all global csv outputs to \strong{\emph{0_STD_DIGEST}} folder (full and final evD and evS) if TRUE. \cr
#' Default is FALSE.
#'
#' @param plot_results \emph{OPTIONAL} \cr
#' Logical value. \cr
#' If TRUE, plots in R session the heatmaps of delta values of all system finite boxes in the 2D space of swept parameters. \cr
#' Default is TRUE.
#'
#' @param save_run_outputs  \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Allows saving all run outputs to working directory (workdir). \cr
#' By default, run outputs are stored in the temporary directory and are erased if not saved. \cr
#' Default is FALSE.
#'
#' @return Calculates the delta values and box sizes at final state of the sweeping of 2D space of parameters in all boxes.
#'
#' \code{\link{sweep_steady}} returns by default a heatmap plot of the isotope composition
#' of each finite box in the 2D space defined by the two swept parameters
#' (set plot_results = FALSE to mute the plots). \cr
#'
#' The graphical results of the sweep can be also interactively explored using the
#' \code{\link{shinobxr_app}} function in case user saves the outputs to the working
#' directory (save_run_outputs = TRUE).
#'
#' \code{\link{sweep_steady}} creates a series of isotope data and metadata,
#' all of which are stored in a temporary directory. \cr
#' The user can save all outputs described below to their working directory
#' by setting save_run_outputs = TRUE (default is FALSE). \cr
#'
#' \code{\link{sweep_steady}} creates and stores all outputs in a dedicated SERIES directory
#' with the following name structure: \cr
#' \strong{\emph{4_STD + SERIES_ID + YYY}}, where YYY is a sweep steady run number automatically
#' set between 001 and 999. \cr
#' No overwriting of previous sweep steady run runs is possible.
#'
#' \code{\link{sweep_steady}} base workflow:
#' \enumerate{
#' \item Calculates the number of single runs the sweeping will require depending on the swept parameters.
#' \item Asks the user confirmation to run \code{\link{sweep_steady}},
#' as the run calculation time depends on the number of successive sweeping runs.
#' \item Writes the set of inputs and outputs for the single initial run only with the following format: \cr
#' \strong{\emph{STD + SERIES_ID + YYY + 0001 + IN.Rda}} \cr
#' \strong{\emph{STD + SERIES_ID + YYY + 0001 + OUT.Rda}} \cr
#' (see \code{\link{run_isobxr}} documentation).
#'
#' \item Writes summarized results in the  \strong{\emph{0_STD_DIGEST}} folder:
#' \enumerate{
#' \item Archived LOG file of local sweep steady run. \cr
#' (file name structure:  \strong{\emph{STD + SERIES_ID + YYY + _LOG.csv}})
#' \item Archived sweep steady master file. \cr
#' (file name structure:  \strong{\emph{STD + SERIES_ID + YYY + _MASTER.xlsx}})
#' \item Dataset of temporal evolution of delta values (evD) in all boxes over the \emph{n} runs that constitute the sweep steady run. \cr
#' (file name structure: \strong{\emph{STD + SERIES_ID + YYY + evD.RDS}})
#' \item Dataset of temporal evolution of box sizes (evS, masses of X) in all boxes over the \emph{n} runs that constitute the sweep steady run. \cr
#' (file name structure: \strong{\emph{STD + SERIES_ID + YYY + evS.RDS}})
#' \item Dataset of final state of delta values (evD) in all boxes over the \emph{n} runs that constitute the sweep steady run. \cr
#' (file name structure: \strong{\emph{STD + SERIES_ID + YYY + evD_final.RDS}})
#' \item Dataset of final state of box sizes (evS, masses of X) in all boxes over the \emph{n} runs that constitute the sweep steady run. \cr
#' (file name structure: \strong{\emph{STD + SERIES_ID + YYY + evS_final.RDS}})
#' }
#' }
#'
#' @section Optional output:
#' \enumerate{
#' \item If to_STD_DIGEST_CSVs = TRUE \cr
#' In the \strong{\emph{0_STD_DIGEST}} folder,
#' edits csv versions of the sweep steady run datasets (full and final evD and evS) with the following name structures:
#' \enumerate{
#' \item \strong{\emph{STD + SERIES_ID + YYY + evD.csv}}
#' \item \strong{\emph{STD + SERIES_ID + YYY + evS.csv}}
#' \item \strong{\emph{STD + SERIES_ID + YYY + evD_final.csv}}
#' \item \strong{\emph{STD + SERIES_ID + YYY + evS_final.csv}}
#' }
#' }
#' For examples, see:
#' https://ttacail.github.io/isobxr_web/vgn_07_sweep_steady.html#4_Tutorial_example
#' @export
sweep_steady <- function(workdir,
                         SERIES_ID,
                         time_units,
                         EXPLO_MASTER,
                         EXPLO_AXIS_1,
                         EXPLO_AXIS_2,
                         to_STD_DIGEST_CSVs = FALSE,
                         plot_results = TRUE,
                         save_run_outputs = FALSE){
  # locally bind variables (fixing binding global variable issue)
  INITIAL_IN <- FLUXES_IN <- COEFFS_IN <- A_OUT <- N_OUT <- A_evD <- N_evD <- N_evS <- Time_plot <- Y <- Z <- NULL

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# set up extdata tutorial mode
  tuto_setup <- using_extdata_tutorial(workdir = workdir, save_run_outputs = save_run_outputs, plot_results = plot_results)
  tuto_mode <- as.logical(tuto_setup[1])
  workdir <- tuto_setup[2]
  save_run_outputs <- as.logical(tuto_setup[3])
  plot_results <- as.logical(tuto_setup[4])

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# clean up
  # Clear plots
  # if(!is.null(dev.list())) dev.off()
  # Clear console
  # cat("\014")
  # Clean workspace
  # rm(list=ls())
  unlink(to_tmpdir(""), recursive = TRUE)
  on.exit(unlink(to_tmpdir(""), recursive = TRUE), add = TRUE)

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIALIZE #----
  #************************************** SET WORKING DIRECTORY #----
  LOC_workdir <- workdir
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(LOC_workdir)

  rlang::inform("_______________________________________________________________________________")
  if (isTRUE(tuto_mode)){
    rlang::inform(paste("\U2139 workdir: no workdir. You are using the tutorial mode (isobxr embedded tutorial files)", sep = ""))
  } else {
    rlang::inform(paste("\U2139 workdir: ", getwd(), sep = ""))
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PREPARE ISOPYBOX ARGUMENTS FROM EXPLO_MASTER #----
  #************************************** DEFINE LOCAL FORCINGS AND CONSTANTS #----
  RUN_LIST <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "RUN_LIST"))
  RAYLEIGH_LIST <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_RAYLEIGH"))
  DELTA_FORCING <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_DELTA"))
  FORCING_ALPHA <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_ALPHA"))
  FORCING_SIZE <-  as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_SIZE"))

  if (nrow(DELTA_FORCING) == 0){
    DELTA_FORCING = NULL
  }

  if (nrow(FORCING_ALPHA) == 0){
    FORCING_ALPHA = NULL
  }

  if (nrow(FORCING_SIZE) == 0){
    FORCING_SIZE = NULL
  }

  t_lim_list <- as.numeric(RUN_LIST$t_lim_list)
  nb_steps_list <- as.numeric(RUN_LIST$nb_step_list)
  flux_list <- as.character(RUN_LIST$flux_list)
  coeff_list <- as.character(RUN_LIST$coeff_list)

  #************************************** DEFINE EXPLO SERIES FAMILY, EXPLO SERIES NUMBER, SERIES_ID #----
  dir_LOG <- "1_LOG.csv"

  n_zeros <- 4
  if (file.exists(dir_LOG) == TRUE){
    file.copy(from = dir_LOG, to = to_tmpdir(dir_LOG))
    LOG <- data.table::fread(to_tmpdir(dir_LOG), data.table = F, stringsAsFactors = T)
    LOG_EXPLO <- LOG[LOG$EXPLORER == TRUE, ]
    remove(LOG)
    EXPLO_SERIES_FAMILY <- paste("STD", as.character(SERIES_ID), sep = "_")
    if (nrow(LOG_EXPLO[LOG_EXPLO$EXPLO_SERIES_FAMILY == EXPLO_SERIES_FAMILY,]) == 0){
      EXPLO_SERIES_n <- 1
      SERIES_ID <- paste("STD", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
    } else {
      EXPLO_SERIES_n <- max(LOG_EXPLO[LOG_EXPLO$EXPLO_SERIES_FAMILY == EXPLO_SERIES_FAMILY, "EXPLO_SERIES_n"])+1
      EXPLO_SERIES_n_length <- length(unlist(strsplit(as.character(EXPLO_SERIES_n), "")))
      SERIES_ID <- paste("STD", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-EXPLO_SERIES_n_length,0),EXPLO_SERIES_n)), collapse = ""), sep = "_")
    }
  } else {
    EXPLO_SERIES_n <- 1
    EXPLO_SERIES_FAMILY <- paste("STD", as.character(SERIES_ID), sep = "_")
    SERIES_ID <- paste("STD", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
  }

  #************************************** READ CONSTANTS FROM ISOPY_MASTER_file #----
  ISOPY_MASTER_file <- "0_ISOBXR_MASTER.xlsx"
  CONSTANTS <- as.data.frame(readxl::read_excel(ISOPY_MASTER_file, "CONSTANTS"))

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIAL RUN 1/2 #----
  i <- 1
  if (is.null(DELTA_FORCING) == FALSE & i %in% DELTA_FORCING$COMPO_RUN_n){
    DELTA_FORCING_loc <- DELTA_FORCING[DELTA_FORCING$COMPO_RUN_n == i, c("BOXES_ID", "DELTA_INIT")]
  } else {
    DELTA_FORCING_loc = NULL
  }

  if (is.null(FORCING_SIZE) == FALSE & i %in% FORCING_SIZE$COMPO_RUN_n){
    FORCING_SIZE_loc <- FORCING_SIZE[FORCING_SIZE$COMPO_RUN_n == i, c("BOXES_ID", "SIZE_INIT")]
  } else {
    FORCING_SIZE_loc = NULL
  }

  i <- 1
  if (is.null(FORCING_ALPHA) == FALSE & i %in% FORCING_ALPHA$COMPO_RUN_n){
    FORCING_ALPHA_loc <- FORCING_ALPHA[FORCING_ALPHA$COMPO_RUN_n == i, c("FROM", "TO", "ALPHA")]
  } else {
    FORCING_ALPHA_loc = NULL
  }

  fx <- flux_list[i]
  a <- coeff_list[i]
  LOC_t_lim <- t_lim_list[i]
  LOC_nb_steps <- nb_steps_list[i]

  if (i %in% RAYLEIGH_LIST$COMPO_RUN_n){
    LOC_RAYLEIGH <- clear_subset(RAYLEIGH_LIST[RAYLEIGH_LIST$COMPO_RUN_n == i,-which(names(RAYLEIGH_LIST) %in% c("COMPO_RUN_n"))])
  } else {
    LOC_RAYLEIGH <- NULL
  }

  COMPOSITE <- TRUE
  rlang::inform("________________________________________________________________________________")
  rlang::inform("\U0001f535 COMPUTING RUN #1 (Initial relaxation) ")
  run_isobxr(workdir = LOC_workdir,
             SERIES_ID = SERIES_ID,
             flux_list_name = fx,
             coeff_list_name = a,
             t_lim = LOC_t_lim,
             nb_steps = LOC_nb_steps,
             time_units,
             FORCING_RAYLEIGH <- LOC_RAYLEIGH,
             FORCING_SIZE = FORCING_SIZE_loc,
             FORCING_DELTA = DELTA_FORCING_loc,
             FORCING_ALPHA = FORCING_ALPHA_loc,
             COMPOSITE = FALSE,
             COMPO_SERIES_n = NaN,
             COMPO_SERIES_FAMILY = NaN,
             EXPLORER = TRUE,
             EXPLO_SERIES_n = EXPLO_SERIES_n,
             EXPLO_SERIES_FAMILY = EXPLO_SERIES_FAMILY,
             HIDE_PRINTS = FALSE,
             to_DIGEST_DIAGRAMS = TRUE,
             to_DIGEST_evD_PLOT = TRUE,
             plot_results = FALSE)

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PREPARE SWEEP OF RUN 2/2 #----
  #************************************** PREPARE INPUTS for ISOPY_RUN with EXPLO_MASTER as default #----
  i <- 2
  fx <- flux_list[i]
  a <- coeff_list[i]
  LOC_t_lim <- t_lim_list[i]
  LOC_nb_steps <- nb_steps_list[i]

  if (i %in% RAYLEIGH_LIST$COMPO_RUN_n){
    LOC_RAYLEIGH <- clear_subset(RAYLEIGH_LIST[RAYLEIGH_LIST$COMPO_RUN_n == i,-which(names(RAYLEIGH_LIST) %in% c("COMPO_RUN_n"))])
    LOC_RAYLEIGH$ALL_DESC <- as.factor(paste(LOC_RAYLEIGH$XFROM, LOC_RAYLEIGH$XTO, LOC_RAYLEIGH$YFROM, LOC_RAYLEIGH$YTO, LOC_RAYLEIGH$AFROM, LOC_RAYLEIGH$ATO, sep = "_"))
  } else {
    LOC_RAYLEIGH <- NULL
  }

  LOG <- data.table::fread(to_tmpdir(dir_LOG), data.table = F, stringsAsFactors = T)
  LOG_last <- LOG[nrow(LOG),]
  remove(LOG)

  if (LOG_last$NUM_ANA == "ANA"){
    load(to_tmpdir(paste(LOG_last$path_outdir, "OUT.Rda", sep = "")))
    OUT_last_final <- A_OUT
    OUT_last_SIZE_FINAL <- OUT_last_final[, c("BOXES_ID", "SIZE_INIT")]
    OUT_last_DELTA_FINAL <- OUT_last_final[, c("BOXES_ID", "DELTA_FINAL")]
    names(OUT_last_DELTA_FINAL) <- c("BOXES_ID", "DELTA_INIT")
  } else {
    load(to_tmpdir(paste(LOG_last$path_outdir, "OUT.Rda", sep = "")))
    OUT_last_final <- N_OUT
    OUT_last_SIZE_FINAL <- OUT_last_final[, c("BOXES_ID", "SIZE_FINAL")]
    names(OUT_last_SIZE_FINAL) <- c("BOXES_ID", "SIZE_INIT")
    OUT_last_DELTA_FINAL <- OUT_last_final[, c("BOXES_ID", "DELTA_FINAL")]
    names(OUT_last_DELTA_FINAL) <- c("BOXES_ID", "DELTA_INIT")
  }

  LOC_SIZE_INIT <- OUT_last_SIZE_FINAL
  LOC_DELTA_INIT <- OUT_last_DELTA_FINAL
  LOC_SIZE_INIT <- clear_subset(LOC_SIZE_INIT)
  LOC_DELTA_INIT <- clear_subset(LOC_DELTA_INIT)

  #************************************** FORCE OVERWRITING DELTA_INIT #----
  if (is.null(DELTA_FORCING) == FALSE & i %in% DELTA_FORCING$COMPO_RUN_n){
    DELTA_FORCING_loc <- DELTA_FORCING[DELTA_FORCING$COMPO_RUN_n == i, c("BOXES_ID", "DELTA_INIT")]
    DELTA_FORCING_loc <- clear_subset(DELTA_FORCING_loc)
    j <- 1
    for (j in 1:nrow(DELTA_FORCING_loc)){
      LOC_DELTA_INIT[LOC_DELTA_INIT$BOXES_ID == as.character(DELTA_FORCING_loc[j, "BOXES_ID"]), "DELTA_INIT"] = DELTA_FORCING_loc[j, "DELTA_INIT"]
      j <- j + 1
    }
  }

  #************************************** FORCE OVERWRITING SIZE_INIT #----
  if (is.null(FORCING_SIZE) == FALSE & i %in% FORCING_SIZE$COMPO_RUN_n){
    FORCING_SIZE_loc <- FORCING_SIZE[FORCING_SIZE$COMPO_RUN_n == i, c("BOXES_ID", "SIZE_INIT")]
    FORCING_SIZE_loc <- clear_subset(FORCING_SIZE_loc)
    j <- 1
    for (j in 1:nrow(FORCING_SIZE_loc)){
      LOC_SIZE_INIT[LOC_SIZE_INIT$BOXES_ID == as.character(FORCING_SIZE_loc[j, "BOXES_ID"]), "SIZE_INIT"] = FORCING_SIZE_loc[j, "SIZE_INIT"]
      j <- j + 1
    }
  }

  #************************************** FORCE OVERWRITING ALPHA #----
  if (is.null(FORCING_ALPHA) == FALSE& i %in% FORCING_ALPHA$COMPO_RUN_n){
    FORCING_ALPHA_loc <- FORCING_ALPHA[FORCING_ALPHA$COMPO_RUN_n == i, c("FROM", "TO", "ALPHA")]
    FORCING_ALPHA_loc$FROM_TO <- paste(FORCING_ALPHA_loc$FROM, FORCING_ALPHA_loc$TO, sep = "_")
    FORCING_ALPHA_loc <- clear_subset(FORCING_ALPHA_loc)
  } else {
    FORCING_ALPHA_loc = NULL
  }

  LOC_DELTA_INIT_post_1st_run <- LOC_DELTA_INIT
  LOC_SIZE_INIT_post_1st_run <- LOC_SIZE_INIT
  FORCING_ALPHA_loc_post_1st_run <- FORCING_ALPHA_loc

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# EXPLO_RUN_LIST
  #************************************** EXPLO_AXIS_1 #----
  if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_n_FLUX_MATRICES"){
    EXPLO_AXIS_1_range <- as.character(EXPLO_AXIS_1[,"VALUES"])
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- EXPLO_AXIS_1_range
  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_n_ALPHA_MATRICES"){
    EXPLO_AXIS_1_range <- as.character(EXPLO_AXIS_1[,"VALUES"])
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- EXPLO_AXIS_1_range
  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_1_ALPHA"){
    EXPLO_AXIS_1_range <- seq(EXPLO_AXIS_1$ALPHA_MIN, EXPLO_AXIS_1$ALPHA_MAX, by = EXPLO_AXIS_1$ALPHA_STEPS)
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- data.frame(FROM = EXPLO_AXIS_1$FROM,
                                            TO = EXPLO_AXIS_1$TO,
                                            ALPHA = EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_forcing_list$FROM_TO <- as.factor(paste(EXPLO_AXIS_1_forcing_list$FROM, EXPLO_AXIS_1_forcing_list$TO, sep = "_"))

  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_1_SIZE"){
    EXPLO_AXIS_1_range <- seq(EXPLO_AXIS_1$SIZE_MIN, EXPLO_AXIS_1$SIZE_MAX, by = EXPLO_AXIS_1$SIZE_STEPS)
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- data.frame(BOXES_ID = EXPLO_AXIS_1$BOXES_ID,
                                            SIZE_INIT = EXPLO_AXIS_1_range)

  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_1_DELTA"){
    EXPLO_AXIS_1_range <- seq(EXPLO_AXIS_1$DELTA_MIN, EXPLO_AXIS_1$DELTA_MAX, by = EXPLO_AXIS_1$DELTA_STEPS)
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- data.frame(BOXES_ID = EXPLO_AXIS_1$BOXES_ID,
                                            DELTA_INIT = EXPLO_AXIS_1_range)

  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_1_RAYLEIGH_ALPHA"){
    EXPLO_AXIS_1_range <- seq(EXPLO_AXIS_1$ALPHA_0_MIN, EXPLO_AXIS_1$ALPHA_0_MAX, by = EXPLO_AXIS_1$ALPHA_0_STEPS)
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- data.frame(XFROM = EXPLO_AXIS_1$XFROM,
                                            XTO = EXPLO_AXIS_1$XTO,
                                            YFROM = EXPLO_AXIS_1$YFROM,
                                            YTO = EXPLO_AXIS_1$YTO,
                                            AFROM = EXPLO_AXIS_1$AFROM,
                                            ATO = EXPLO_AXIS_1$ATO,
                                            ALPHA_0 = EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_forcing_list$ALL_DESC <- as.factor(paste(EXPLO_AXIS_1_forcing_list$XFROM,
                                                          EXPLO_AXIS_1_forcing_list$XTO,
                                                          EXPLO_AXIS_1_forcing_list$YFROM,
                                                          EXPLO_AXIS_1_forcing_list$YTO,
                                                          EXPLO_AXIS_1_forcing_list$AFROM,
                                                          EXPLO_AXIS_1_forcing_list$ATO,
                                                          sep = "_"))
  }


  #************************************** EXPLO_AXIS_2 #----
  if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_n_FLUX_MATRICES"){
    EXPLO_AXIS_2_range <- as.character(EXPLO_AXIS_2[,"VALUES"])
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- EXPLO_AXIS_2_range
  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_n_ALPHA_MATRICES"){
    EXPLO_AXIS_2_range <- as.character(EXPLO_AXIS_2[,"VALUES"])
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- EXPLO_AXIS_2_range
  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_1_ALPHA"){
    EXPLO_AXIS_2_range <- seq(EXPLO_AXIS_2$ALPHA_MIN, EXPLO_AXIS_2$ALPHA_MAX, by = EXPLO_AXIS_2$ALPHA_STEPS)
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- data.frame(FROM = EXPLO_AXIS_2$FROM,
                                            TO = EXPLO_AXIS_2$TO,
                                            ALPHA = EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_forcing_list$FROM_TO <- as.factor(paste(EXPLO_AXIS_2_forcing_list$FROM, EXPLO_AXIS_2_forcing_list$TO, sep = "_"))

  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_1_SIZE"){
    EXPLO_AXIS_2_range <- seq(EXPLO_AXIS_2$SIZE_MIN, EXPLO_AXIS_2$SIZE_MAX, by = EXPLO_AXIS_2$SIZE_STEPS)
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- data.frame(BOXES_ID = EXPLO_AXIS_2$BOXES_ID,
                                            SIZE_INIT = EXPLO_AXIS_2_range)

  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_1_DELTA"){
    EXPLO_AXIS_2_range <- seq(EXPLO_AXIS_2$DELTA_MIN, EXPLO_AXIS_2$DELTA_MAX, by = EXPLO_AXIS_2$DELTA_STEPS)
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- data.frame(BOXES_ID = EXPLO_AXIS_2$BOXES_ID,
                                            DELTA_INIT = EXPLO_AXIS_2_range)

  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_1_RAYLEIGH_ALPHA"){
    EXPLO_AXIS_2_range <- seq(EXPLO_AXIS_2$ALPHA_0_MIN, EXPLO_AXIS_2$ALPHA_0_MAX, by = EXPLO_AXIS_2$ALPHA_0_STEPS)
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- data.frame(XFROM = EXPLO_AXIS_2$XFROM,
                                            XTO = EXPLO_AXIS_2$XTO,
                                            YFROM = EXPLO_AXIS_2$YFROM,
                                            YTO = EXPLO_AXIS_2$YTO,
                                            AFROM = EXPLO_AXIS_2$AFROM,
                                            ATO = EXPLO_AXIS_2$ATO,
                                            ALPHA_0 = EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_forcing_list$ALL_DESC <- as.factor(paste(EXPLO_AXIS_2_forcing_list$XFROM,
                                                          EXPLO_AXIS_2_forcing_list$XTO,
                                                          EXPLO_AXIS_2_forcing_list$YFROM,
                                                          EXPLO_AXIS_2_forcing_list$YTO,
                                                          EXPLO_AXIS_2_forcing_list$AFROM,
                                                          EXPLO_AXIS_2_forcing_list$ATO,
                                                          sep = "_"))
  }

  #************************************** CALCULATE TOTAL NUMBER OF RUNS #----
  tot_run <- EXPLO_AXIS_1_leng * EXPLO_AXIS_2_leng

  STOP_GO <- FALSE
  rlang::inform("________________________________________________________________________________")
  if(interactive()){
    if (.Platform$OS.type == "windows"){
      STOP_GO <- utils::askYesNo(paste("? This sweep requires *", as.character(tot_run), "* independent runs, do you wish to carry on?"), default = TRUE)
    } else {
      STOP_GO <- utils::askYesNo(cat("? This sweep requires *", as.character(tot_run), "* independent runs, do you wish to carry on? \n"), default = TRUE)
    }
  } else {
    rlang::inform(paste("\U2139 This sweep requires ", as.character(tot_run), " independent runs."))
    STOP_GO <- TRUE
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# SWEEP THE SPACE OF PARAMETERS #----
  if (STOP_GO == FALSE){
    rlang::abort("\U2757 You probably want to reduce the number of iterations in each EXPLO axis.")
  } else {
    rlang::inform("________________________________________________________________________________")
    rlang::inform("\U0001f535 COMPUTING SWEEP OF RUN #2")
    pb_cpt <- utils::txtProgressBar(min = 1, max = tot_run, style = 3, width = 50)
    #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# SWEEP RUN 2/2 in [1:n] #----
    clock <- 1
    k <- 1
    for (k in 1:EXPLO_AXIS_1_leng){
      l <- 1
      for (l in 1:EXPLO_AXIS_2_leng){
        LOC_DELTA_INIT <- LOC_DELTA_INIT_post_1st_run
        LOC_SIZE_INIT <- LOC_SIZE_INIT_post_1st_run
        FORCING_ALPHA_loc <- FORCING_ALPHA_loc_post_1st_run

        #************************************** UPDATE ISOPYRUN ARGUMENTS WITH EXPLO RANGES TO EXPLORE #----
        #************************************** OVERWRITE EXPLO_MASTER FORCINGS IF NEEDED #----
        #************************************** UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_1 VALUE #----
        if (EXPLO_AXIS_1_type == "EXPLO_n_FLUX_MATRICES"){            ##### IF
          fx <- EXPLO_AXIS_1_forcing_list[k]
        } else if (EXPLO_AXIS_1_type == "EXPLO_n_ALPHA_MATRICES"){    ##### IF
          a <- EXPLO_AXIS_1_forcing_list[k]
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_ALPHA"){             ##### IF
          if (is.null(FORCING_ALPHA_loc)){
            FORCING_ALPHA_loc <- clear_subset(EXPLO_AXIS_1_forcing_list[k,])
          } else if (EXPLO_AXIS_1_forcing_list[k,"FROM_TO"] %in% FORCING_ALPHA_loc$FROM_TO){
            FORCING_ALPHA_loc[FORCING_ALPHA_loc$FROM_TO == as.character(EXPLO_AXIS_1_forcing_list[k,"FROM_TO"]), "ALPHA"] <- EXPLO_AXIS_1_forcing_list[k,"ALPHA"]
          } else {
            FORCING_ALPHA_loc <- rbind(FORCING_ALPHA_loc, EXPLO_AXIS_1_forcing_list[k,])
          }
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_SIZE"){              ##### IF
          LOC_SIZE_INIT[LOC_SIZE_INIT$BOXES_ID == as.character(EXPLO_AXIS_1_forcing_list[k, "BOXES_ID"]), "SIZE_INIT"] <- EXPLO_AXIS_1_forcing_list[k,"SIZE_INIT"]
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_DELTA"){             ##### IF
          LOC_DELTA_INIT[LOC_DELTA_INIT$BOXES_ID == as.character(EXPLO_AXIS_1_forcing_list[k, "BOXES_ID"]), "DELTA_INIT"] <- EXPLO_AXIS_1_forcing_list[k,"DELTA_INIT"]
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_RAYLEIGH_ALPHA"){    ##### IF
          if(is.null(LOC_RAYLEIGH)){
            LOC_RAYLEIGH <- clear_subset(EXPLO_AXIS_1_forcing_list[k,])
          } else if (EXPLO_AXIS_1_forcing_list[k,"ALL_DESC"] %in% as.character(LOC_RAYLEIGH$ALL_DESC)){
            LOC_RAYLEIGH[LOC_RAYLEIGH$ALL_DESC == EXPLO_AXIS_1_forcing_list[k,"ALL_DESC"], "ALPHA_0"] <- EXPLO_AXIS_1_forcing_list[k,"ALPHA_0"]
          } else {
            LOC_RAYLEIGH <- rbind(LOC_RAYLEIGH, EXPLO_AXIS_1_forcing_list[k,])
          }
        }

        #************************************** UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_2 VALUE #----
        if (EXPLO_AXIS_2_type == "EXPLO_n_FLUX_MATRICES"){            ##### IF
          fx <- EXPLO_AXIS_2_forcing_list[l]
        } else if (EXPLO_AXIS_2_type == "EXPLO_n_ALPHA_MATRICES"){    ##### IF
          a <- EXPLO_AXIS_2_forcing_list[l]
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_ALPHA"){             ##### IF
          if (is.null(FORCING_ALPHA_loc)){
            FORCING_ALPHA_loc <- clear_subset(EXPLO_AXIS_2_forcing_list[l,])
          } else if (EXPLO_AXIS_2_forcing_list[l,"FROM_TO"] %in% FORCING_ALPHA_loc$FROM_TO){
            FORCING_ALPHA_loc[FORCING_ALPHA_loc$FROM_TO == as.character(EXPLO_AXIS_2_forcing_list[l,"FROM_TO"]), "ALPHA"] <- EXPLO_AXIS_2_forcing_list[l,"ALPHA"]
          } else {
            FORCING_ALPHA_loc <- rbind(FORCING_ALPHA_loc, EXPLO_AXIS_2_forcing_list[l,])
          }
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_SIZE"){              ##### IF
          LOC_SIZE_INIT[LOC_SIZE_INIT$BOXES_ID == as.character(EXPLO_AXIS_2_forcing_list[l, "BOXES_ID"]), "SIZE_INIT"] <- EXPLO_AXIS_2_forcing_list[l,"SIZE_INIT"]
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_DELTA"){             ##### IF
          LOC_DELTA_INIT[LOC_DELTA_INIT$BOXES_ID == as.character(EXPLO_AXIS_2_forcing_list[l, "BOXES_ID"]), "DELTA_INIT"] <- EXPLO_AXIS_2_forcing_list[l,"DELTA_INIT"]
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_RAYLEIGH_ALPHA"){    ##### IF
          if(is.null(LOC_RAYLEIGH)){
            LOC_RAYLEIGH <- clear_subset(EXPLO_AXIS_2_forcing_list[l,])
          } else if (EXPLO_AXIS_2_forcing_list[l,"ALL_DESC"] %in% as.character(LOC_RAYLEIGH$ALL_DESC)){
            LOC_RAYLEIGH[LOC_RAYLEIGH$ALL_DESC == EXPLO_AXIS_2_forcing_list[l,"ALL_DESC"], "ALPHA_0"] <- EXPLO_AXIS_2_forcing_list[l,"ALPHA_0"]
          } else {
            LOC_RAYLEIGH <- rbind(LOC_RAYLEIGH, EXPLO_AXIS_2_forcing_list[l,])
          }
        }

        #************************************** RUN #----
        quiet(run_isobxr(workdir = LOC_workdir,
                         SERIES_ID = SERIES_ID,
                         flux_list_name = fx,
                         coeff_list_name = a,
                         t_lim = LOC_t_lim,
                         nb_steps = LOC_nb_steps,
                         time_units,
                         FORCING_RAYLEIGH <- LOC_RAYLEIGH,
                         FORCING_SIZE <- LOC_SIZE_INIT,
                         FORCING_DELTA <- LOC_DELTA_INIT,
                         FORCING_ALPHA <-  FORCING_ALPHA_loc,
                         COMPOSITE = FALSE,
                         COMPO_SERIES_n = NaN,
                         COMPO_SERIES_FAMILY = NaN,
                         EXPLORER = TRUE,
                         EXPLO_SERIES_n = EXPLO_SERIES_n,
                         EXPLO_SERIES_FAMILY = EXPLO_SERIES_FAMILY,
                         HIDE_PRINTS = TRUE,
                         to_DIGEST_DIAGRAMS = FALSE,
                         to_DIGEST_evD_PLOT = FALSE,
                         plot_results = FALSE))
        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# CLOCK #----
        # calculation_gauge(clock, tot_run)
        utils::setTxtProgressBar(pb_cpt, clock)
        clock <- clock + 1
        l <- l + 1
      }
      k <- k + 1
    }
    close(pb_cpt)

    #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# SUMMARY of EXPLOR SPACE #----
    if (EXPLO_AXIS_1_type %in% c("EXPLO_n_ALPHA_MATRICES", "EXPLO_n_FLUX_MATRICES")){
      EXPLO_AXIS_1_toEXPLOG <- EXPLO_AXIS_1
    } else {
      EXPLO_AXIS_1_toEXPLOG <- EXPLO_AXIS_1_forcing_list
      EXPLO_AXIS_1_toEXPLOG$EXPLO_TYPES <- as.factor(EXPLO_AXIS_1_type)
    }

    if (EXPLO_AXIS_2_type %in% c("EXPLO_n_ALPHA_MATRICES", "EXPLO_n_FLUX_MATRICES")){
      EXPLO_AXIS_2_toEXPLOG <- EXPLO_AXIS_2
    } else {
      EXPLO_AXIS_2_toEXPLOG <- EXPLO_AXIS_2_forcing_list
      EXPLO_AXIS_2_toEXPLOG$EXPLO_TYPES <- as.factor(EXPLO_AXIS_2_type)
    }

    names(EXPLO_AXIS_1_toEXPLOG) <- paste(names(EXPLO_AXIS_1_toEXPLOG), "_1", sep = "")
    names(EXPLO_AXIS_2_toEXPLOG) <- paste(names(EXPLO_AXIS_2_toEXPLOG), "_2", sep = "")

    k <- 1
    Run_n <- 2
    for (k in 1:EXPLO_AXIS_1_leng){
      l <- 1
      for (l in 1:EXPLO_AXIS_2_leng){
        EXPLOG_loc <- cbind(EXPLO_AXIS_1_toEXPLOG[k,], EXPLO_AXIS_2_toEXPLOG[l,])
        EXPLOG_loc$EXPLO_SERIES_n <- EXPLO_SERIES_n
        EXPLOG_loc$EXPLO_SERIES_FAMILY <- EXPLO_SERIES_FAMILY
        EXPLOG_loc$RUN_n <- Run_n
        if (Run_n == 2){
          EXPLOG <- EXPLOG_loc
        } else {
          EXPLOG <- rbind(EXPLOG, EXPLOG_loc)
        }
        l <- l + 1
        Run_n <- Run_n + 1
      }
      k <- k + 1
    }

    new.row <- head(EXPLOG[NA,], 1)
    new.row$RUN_n <- 1
    new.row$EXPLO_SERIES_n <- EXPLO_SERIES_n
    new.row$EXPLO_SERIES_FAMILY <- EXPLO_SERIES_FAMILY
    EXPLOG <- rbind(new.row, EXPLOG)
    EXPLOG <- clear_subset(EXPLOG)

    if (EXPLO_AXIS_1_type %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$VALUES_1
      EXPLOG$LEGEND_EXPLO_1 <- EXPLOG$EXPLO_TYPES_1
    } else if (EXPLO_AXIS_1_type == "EXPLO_1_SIZE"){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$SIZE_INIT_1
      EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("SIZE_t0_", EXPLOG$BOXES_ID_1, sep = ""))
    } else if (EXPLO_AXIS_1_type == "EXPLO_1_DELTA"){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$DELTA_INIT_1
      EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("DELTA_t0_", EXPLOG$BOXES_ID_1, sep = ""))
    } else if (EXPLO_AXIS_1_type == "EXPLO_1_ALPHA"){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$ALPHA_1
      EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("ALPHA_", EXPLOG$FROM_TO_1, sep = ""))
    } else if (EXPLO_AXIS_1_type == "EXPLO_1_RAYLEIGH_ALPHA"){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$ALPHA_0_1
      EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("ALPHA_", EXPLOG$ALL_DESC_1, sep = ""))
    }

    if (EXPLO_AXIS_2_type %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$VALUES_2
      EXPLOG$LEGEND_EXPLO_2 <- EXPLOG$EXPLO_TYPES_2
    } else if (EXPLO_AXIS_2_type == "EXPLO_1_SIZE"){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$SIZE_INIT_2
      EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("SIZE_t0_", EXPLOG$BOXES_ID_2, sep = ""))
    } else if (EXPLO_AXIS_2_type == "EXPLO_1_DELTA"){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$DELTA_INIT_2
      EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("DELTA_t0_", EXPLOG$BOXES_ID_2, sep = ""))
    } else if (EXPLO_AXIS_2_type == "EXPLO_1_ALPHA"){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$ALPHA_2
      EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("ALPHA_", EXPLOG$FROM_TO_2, sep = ""))
    } else if (EXPLO_AXIS_2_type == "EXPLO_1_RAYLEIGH_ALPHA"){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$ALPHA_0_2
      EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("ALPHA_", EXPLOG$ALL_DESC_2, sep = ""))
    }

    #************************************** LOAD LOG/OUT FILES of CURRENT COMPO SERIES #----
    LOG <- data.table::fread(to_tmpdir(dir_LOG), data.table = F, stringsAsFactors = T)
    LOG_SERIES <- LOG[LOG$SERIES_ID == SERIES_ID,]
    remove(LOG)
    LOG_SERIES <- dplyr::full_join(LOG_SERIES, EXPLOG, by = "RUN_n")
    LOG_SERIES <- clear_subset(LOG_SERIES)
    SERIES_RUN_ID_1 <- LOG_SERIES[1, "SERIES_RUN_ID"]
    path_to_input_1 <- paste(LOG_SERIES[1, "path_outdir"], "IN.Rda", sep = "")
    load(to_tmpdir(path_to_input_1))
    BOXES_IDs <- as.character(INITIAL_IN$BOXES_ID)

    #************************************** READ/BUILD/MERGE evS/evD for ANA/NUM WHOLE COMPOSITE RUN #----
    rlang::inform("________________________________________________________________________________")
    rlang::inform("\U0001f535 PREPARING RESULTS")
    pb_prep <- utils::txtProgressBar(min = 1, max = tot_run+1, style = 3, width = 50)

    i <- 1
    for (i in 1:(tot_run+1)){
      SERIES_RUN_ID_i <- LOG_SERIES[i, "SERIES_RUN_ID"]
      RUN_n_i <- LOG_SERIES[i, "RUN_n"]
      path_outdir_i <- as.character(LOG_SERIES[i, "path_outdir"])
      path_to_input_i <- paste(path_outdir_i, "IN.Rda", sep = "")
      load(to_tmpdir(path_to_input_i))
      INIT_i <- INITIAL_IN
      SIZE_INIT_i <- INIT_i[,c("BOXES_ID", "SIZE_INIT")]
      DELTA_INIT_i <- INIT_i[,c("BOXES_ID", "DELTA_INIT")]
      FLUXES_i <- FLUXES_IN
      COEFFS_i <- COEFFS_IN
      if (i > 1){
        unlink(to_tmpdir(path_to_input_i), recursive = TRUE) ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option?
      }

      if (LOG_SERIES[i, "NUM_ANA"] == "ANA"){
        load(to_tmpdir(paste(as.character(LOG_SERIES[i, "path_outdir"]), "OUT.Rda", sep = "")))
        evD_i <- A_evD
        SIZE_INIT_i_hor <- as.data.frame(t(SIZE_INIT_i$SIZE_INIT))
        names(SIZE_INIT_i_hor) <- SIZE_INIT_i$BOXES_ID
        SIZE_INIT_i_hor$Time = NaN
        evS_i <- evD_i
        j <- 1
        for (j in 1:length(BOXES_IDs)){
          evS_i[,BOXES_IDs[j]] <- SIZE_INIT_i_hor[1, BOXES_IDs[j]]
          j <- j + 1
        }
        if (i > 1){ ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option?
          unlink(to_tmpdir(paste(as.character(LOG_SERIES[i, "path_outdir"]), "OUT.Rda", sep = "")), recursive = TRUE)
        }
      } else {
        if (LOG_SERIES[i, "NUM_ANA"] == "NUM"){
          load(to_tmpdir(paste(path_outdir_i, "OUT.Rda", sep = "")))
          evD_i <- N_evD
          evS_i <- N_evS
          if (i > 1){ ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option?
            unlink(to_tmpdir(paste(path_outdir_i, "OUT.Rda", sep = "")), recursive = TRUE)
          }
        }
      }

      FLUXES_i_colnames <- names(FLUXES_i)
      FLUXES_i_vert <- data.frame(VAR_TYPE = "FLUX",
                                  VARIABLE = NaN,
                                  VALUE = NaN)
      FLUXES_i_vert_loc <- FLUXES_i_vert
      j <- i
      for (j in 1:nrow(FLUXES_i)){
        k <- 1
        for (k in 1:(length(FLUXES_i)-1)){
          FLUXES_i_vert_loc$VALUE = FLUXES_i[j,k+1]
          FLUXES_i_vert_loc$VARIABLE = paste("f", FLUXES_i[j, "BOXES_ID"], FLUXES_i_colnames[k+1], sep = "_")
          FLUXES_i_vert <- rbind(FLUXES_i_vert, FLUXES_i_vert_loc)
        }
      }
      FLUXES_i_vert <- FLUXES_i_vert[2:nrow(FLUXES_i_vert),]

      COEFFS_i_colnames <- names(COEFFS_i)
      COEFFS_i_vert <- data.frame(VAR_TYPE = "COEFF",
                                  VARIABLE = NaN,
                                  VALUE = NaN)
      COEFFS_i_vert_loc <- COEFFS_i_vert
      j <- i
      for (j in 1:nrow(COEFFS_i)){
        k <- 1
        for (k in 1:(length(COEFFS_i)-1)){
          COEFFS_i_vert_loc$VALUE = COEFFS_i[j,k+1]
          COEFFS_i_vert_loc$VARIABLE = paste("a", COEFFS_i[j, "BOXES_ID"], COEFFS_i_colnames[k+1], sep = "_")
          COEFFS_i_vert <- rbind(COEFFS_i_vert, COEFFS_i_vert_loc)
        }
      }
      COEFFS_i_vert <- COEFFS_i_vert[2:nrow(COEFFS_i_vert),]

      SIZE_INIT_i_vert <- SIZE_INIT_i
      DELTA_INIT_i_vert <- DELTA_INIT_i
      SIZE_INIT_i_vert$VAR_TYPE <- "SIZE_INIT"
      DELTA_INIT_i_vert$VAR_TYPE <- "DELTA_INIT"
      SIZE_INIT_i_vert$VARIABLE <- paste("m0", SIZE_INIT_i_vert$BOXES_ID, sep = "_")
      DELTA_INIT_i_vert$VARIABLE <- paste("d0", DELTA_INIT_i_vert$BOXES_ID, sep = "_")
      SIZE_INIT_i_vert$VALUE <- SIZE_INIT_i_vert$SIZE_INIT
      DELTA_INIT_i_vert$VALUE <- DELTA_INIT_i_vert$DELTA_INIT
      SIZE_INIT_i_vert <- SIZE_INIT_i_vert[,c("VAR_TYPE", "VARIABLE", "VALUE")]
      DELTA_INIT_i_vert <- DELTA_INIT_i_vert[,c("VAR_TYPE", "VARIABLE", "VALUE")]

      meta_RUN_i <- rbind(SIZE_INIT_i_vert, DELTA_INIT_i_vert)
      meta_RUN_i <- rbind(meta_RUN_i, FLUXES_i_vert)
      meta_RUN_i <- rbind(meta_RUN_i, COEFFS_i_vert)
      meta_RUN_i_short <- meta_RUN_i[,c("VARIABLE", "VALUE")]
      meta_RUN_i_horiz <- as.data.frame(t(meta_RUN_i_short$VALUE))
      names(meta_RUN_i_horiz) <- meta_RUN_i$VARIABLE

      evD_i$SERIES_RUN_ID <- SERIES_RUN_ID_i
      evS_i$SERIES_RUN_ID <- SERIES_RUN_ID_i
      evD_i$RUN_n <- RUN_n_i
      evS_i$RUN_n <- RUN_n_i
      evD_i$LEGEND_EXPLO_1 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "LEGEND_EXPLO_1"]
      evD_i$VAR_EXPLO_1 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "VAR_EXPLO_1"]
      evD_i$LEGEND_EXPLO_2 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "LEGEND_EXPLO_2"]
      evD_i$VAR_EXPLO_2 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "VAR_EXPLO_2"]
      evS_i$LEGEND_EXPLO_1 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "LEGEND_EXPLO_1"]
      evS_i$VAR_EXPLO_1 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "VAR_EXPLO_1"]
      evS_i$LEGEND_EXPLO_2 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "LEGEND_EXPLO_2"]
      evS_i$VAR_EXPLO_2 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "VAR_EXPLO_2"]

      meta_RUN_i_evD_df <- as.data.frame(dplyr::slice(meta_RUN_i_horiz, rep(1:dplyr::n(), each = nrow(evD_i))))
      meta_RUN_i_evS_df <- as.data.frame(dplyr::slice(meta_RUN_i_horiz, rep(1:dplyr::n(), each = nrow(evS_i))))

      evD_i <- cbind(evD_i, meta_RUN_i_evD_df)
      evS_i <- cbind(evS_i, meta_RUN_i_evS_df)

      if (i == 1){
        evD <- evD_i
        evS <- evS_i
      } else {
        evD <- rbind(evD, evD_i[1:nrow(evD_i),])
        evS <- rbind(evS, evS_i[1:nrow(evS_i),])
      }
      utils::setTxtProgressBar(pb_prep, i)
      i <- i + 1
    }
    close(pb_prep)

    colnames_to_drop_check <- meta_RUN_i[meta_RUN_i$VAR_TYPE == "FLUX", "VARIABLE"]
    flux_cols_to_drop <- NULL
    i <- 1
    for (i in 1:length(colnames_to_drop_check)){
      if (sum(evD[, colnames_to_drop_check[i]]) == 0)
        flux_cols_to_drop <- c(flux_cols_to_drop, colnames_to_drop_check[i])
      i <- i + 1
    }

    colnames_to_drop_check <- meta_RUN_i[meta_RUN_i$VAR_TYPE == "COEFF", "VARIABLE"]
    alpha_cols_to_drop <- NULL
    i <- 1
    for (i in 1:length(colnames_to_drop_check)){
      if (sum(abs(evD[, colnames_to_drop_check[i]]-1)) == 0)
        alpha_cols_to_drop <- c(alpha_cols_to_drop, colnames_to_drop_check[i])
      i <- i + 1
    }

    evD <- evD[, -which(names(evD) %in% c(flux_cols_to_drop, alpha_cols_to_drop))]
    evS <- evS[, -which(names(evS) %in% c(flux_cols_to_drop, alpha_cols_to_drop))]

    evD_final <- evD[evD$Time == t_lim_list[2],]
    evS_final <- evS[evS$Time == t_lim_list[2],]

    #************************************** EDIT CSV for WHOLE COMPOSITE RUN evS - evD - LOG - OUT #----
    rlang::inform("________________________________________________________________________________")
    rlang::inform("\U0001f535 WRITING DIGEST")

    path_out_EXPLO <- paste("4_", as.character(SERIES_ID), "/", "0_STD_DIGEST/", sep = "")
    if (!dir.exists(to_tmpdir(path_out_EXPLO))){dir.create(to_tmpdir(path_out_EXPLO))}
    path_out_EXPLO <- paste(path_out_EXPLO, SERIES_ID, sep = "")

    data.table::fwrite(LOG_SERIES, file = paste(to_tmpdir(path_out_EXPLO), "_LOG.csv", sep = ""), row.names = F, quote = F)
    saveRDS(object = evD, file = paste(to_tmpdir(path_out_EXPLO), "_evD.RDS", sep = ""))
    saveRDS(object = evS, file = paste(to_tmpdir(path_out_EXPLO), "_evS.RDS", sep = ""))
    saveRDS(object = evD_final, file = paste(to_tmpdir(path_out_EXPLO), "_evD_final.RDS", sep = ""))
    saveRDS(object = evS_final, file = paste(to_tmpdir(path_out_EXPLO), "_evS_final.RDS", sep = ""))

    if (isTRUE(to_STD_DIGEST_CSVs)){
      data.table::fwrite(evD, file = paste(to_tmpdir(path_out_EXPLO), "_evD.csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(evS, file = paste(to_tmpdir(path_out_EXPLO), "_evS.csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(evD_final, file = paste(to_tmpdir(path_out_EXPLO), "_evD_final.csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(evS_final, file = paste(to_tmpdir(path_out_EXPLO), "_evS_final.csv", sep = ""), row.names = F, quote = F)
    }

    explo_master_excel_path <-  paste(to_tmpdir(path_out_EXPLO), "_MASTER.xlsx" , sep = "")
    writexl::write_xlsx(list(RUN_LIST = RUN_LIST,
                             FORCING_RAYLEIGH = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_RAYLEIGH")),
                             FORCING_SIZE = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_SIZE")),
                             FORCING_DELTA = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_DELTA")),
                             FORCING_ALPHA =  as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_ALPHA"))
    ),
    explo_master_excel_path)

  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PLOT DEFAULT MAPS FROM TEMPDIR #----
  heatmap_contour <- TRUE #DEFAULT not implemented
  if (isTRUE(plot_results)){
    SERIES_ID_plot <- LOG_SERIES[1,"SERIES_ID"]

    # define box lists
    BOXES <- unlist(stringr::str_split(LOG_SERIES[1,"BOXES_ID_list"], pattern = "_"))
    BOXES_INFINITE <- unlist(stringr::str_split(LOG_SERIES[1,"INFINITE_BOXES_list"], pattern = "_"))
    BOXES_DISCONNECTED <- unlist(stringr::str_split(LOG_SERIES[1,"DISCONNECTED_BOXES"], pattern = "_"))
    BOXES_FINITE <- BOXES[!BOXES %in% BOXES_INFINITE]
    BOXES_FINITE_CONNECTED <- BOXES_FINITE[!BOXES_FINITE %in% BOXES_DISCONNECTED]

    for (i in 1:length(BOXES_FINITE_CONNECTED)){
      loc_BOX <- BOXES_FINITE_CONNECTED[i] # check selected box exists
      X <- "VAR_EXPLO_1"
      Y <-  "VAR_EXPLO_2"

      # DROP RUN 1
      evD_final <- clear_subset(evD_final[evD_final$RUN_n != 1, ])

      # VERTICALIZE
      evD_final_vert <- DF_verticalizer(df_hor = evD_final, vert_col = BOXES)
      DF <- evD_final_vert

      # DEFINE TITLES
      if (levels(DF$LEGEND_EXPLO_1) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
        EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", levels(DF$VAR_EXPLO_1)[1], " to ", levels(DF$VAR_EXPLO_1)[length(levels(DF$VAR_EXPLO_1))], sep = ""), sep = "")
      } else {
        EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", paste(c(min(DF$VAR_EXPLO_1), max(DF$VAR_EXPLO_1)), collapse = " to "), collapse = ", "), sep = "")
      }

      if (levels(DF$LEGEND_EXPLO_2) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
        EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(levels(DF$VAR_EXPLO_2), collapse = ", "), sep = ""), sep = "")
      } else {
        EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(c(min(DF$VAR_EXPLO_2), max(DF$VAR_EXPLO_2)), collapse = " to "), collapse = ", "), sep = "")
      }

      EXPLO_subtitle_0 = paste(SERIES_ID_plot,
                               " (", min(LOG_SERIES$RUN_n),
                               "-", max(LOG_SERIES$RUN_n),
                               ") \n", "Hidden initial run: ",
                               paste(LOG_SERIES[1, "COEFF_FLUX"], collapse = " / "),
                               sep = "")

      EXPLO_subtitle <- paste(EXPLO_subtitle_0, "\n",
                              EXPLO_subtitle_1, "\n",
                              EXPLO_subtitle_2, sep = "")


      EXPLO_title <- paste("\u03B4", CONSTANTS$ELEMENT, " (", CONSTANTS$NUMERATOR, "/", CONSTANTS$DENOMINATOR,", \u2030", ") - [", loc_BOX, "]", sep = "")

      ################################################ PLOT VAR_EXPLO_2 vs VAR_EXPLO_1
      DF_map_1 <- DF

      X_ID <- as.character(evD_final_vert[1,"LEGEND_EXPLO_1"])
      X_ID_legend <- paste(as.character(evD_final_vert[1,"LEGEND_EXPLO_1"]), " (", as.character(evD_final_vert[1,X]), " to ",  as.character(evD_final_vert[nrow(evD_final_vert),X]), ")", sep ="")
      DF_map_1$X_ID <- X_ID
      DF_map_1$X <- DF_map_1[,X]

      Y_ID <- as.character(evD_final_vert[1,"LEGEND_EXPLO_2"])
      Y_ID_legend <- paste(as.character(evD_final_vert[1,"LEGEND_EXPLO_2"]), " (", as.character(evD_final_vert[1,Y]), " to ",  as.character(evD_final_vert[nrow(evD_final_vert),Y]), ")", sep ="")
      DF_map_1$Y_ID <- Y_ID
      DF_map_1$Y <- DF_map_1[,Y]

      remove(evD_final_vert)

      DF_map_1 <- DF_map_1[,c("X_ID", "X", "Y_ID", "Y", "VAR_TYPE", "VAR")]

      names(DF_map_1) <- c("X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")

      DF_map_1_loc <- DF_map_1[DF_map_1$Z_ID == loc_BOX,]

      map_1 <- ggplot2::ggplot(data = DF_map_1_loc, ggplot2::aes(x = as.numeric(X), y = as.numeric(Y), z = Z))+
        ggplot2::geom_raster(ggplot2::aes(fill = Z), hjust=0.5, vjust=0.5, interpolate = F)+
        ggplot2::scale_fill_gradientn(colors = rainbow(200))+
        ggplot2::theme_bw()+
        ggplot2::labs(y = Y_ID_legend,
                      x = X_ID_legend,
                      title = EXPLO_title,
                      subtitle = EXPLO_subtitle)

      if (isTRUE(heatmap_contour)){
        ampl <- (max(DF_map_1_loc$Z) - min(DF_map_1_loc$Z))
        n_bands <- 10
        bins_exact <- ampl/n_bands
        BINWIDTH_CONTOUR <- round(bins_exact, digits = -floor(log10(bins_exact)))
        if (BINWIDTH_CONTOUR >= 1e-3){
          breaks_loc <- metR::MakeBreaks(binwidth = as.numeric(BINWIDTH_CONTOUR))
          map_1 <- map_1 +
            metR::geom_contour_fill(ggplot2::aes(z = Z), alpha = 1, breaks = breaks_loc) +
            metR::geom_contour2(ggplot2::aes(z = Z), breaks = breaks_loc) +
            metR::geom_label_contour(ggplot2::aes(z = Z), breaks = breaks_loc, skip = 0)
        }
      }
      # print(map_1)
      # print(evD_plot)
      if (i == 1){
        plot_list <- list(map_1)
      } else {
        plot_list <- list(plot_list, map_1)
      }
    }
  }

  #----#----#----#----#----#----#----#----#----#---- save_run_outputs or not #----
  rlang::inform("________________________________________________________________________________")
  rlang::inform(message = paste("\U2139 Run outputs (stored in temporary directory):",
                                sep = ""))
  fs::dir_tree(path = to_tmpdir(""), recurse = T)
  rlang::inform("________________________________________________________________________________")
  if(isFALSE(save_run_outputs)){
    rlang::inform("\U2757 Results were not saved to working directory (set save_run_outputs = TRUE to save results).")
    rlang::inform("\U2139 You can explore the results with more parameters by using the shinobxr_app() function (requires saved outputs).")
  } else if(isTRUE(save_run_outputs)){
    R.utils::copyDirectory(to_tmpdir(""),
                           getwd(),
                           overwrite = T)
    rlang::inform("\U2705 Results were successfully saved to working directory.")
    rlang::inform("\U2139 You can explore the results with more parameters by using the shinobxr_app() function.")
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

  if(isTRUE(plot_results)){
    return(in_silence(plot_list))
  }
}

#  #_________________________________________________________________________80char
#' Sweep the space of two parameters during a dynamic run
#' @description  A function to assess the influence of two parameters (varying
#' over a range of values) on dynamic evolution of a given model in response to a given perturbation.
#'
#' @param workdir Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
#' of the dynamic sweep master file (e.g., \strong{\emph{0_EXPLO_DYN_MASTER.xlsx}}) \cr
#' and where output files will be stored if saved by user. \cr
#' (character string)
#'
#' @param SERIES_ID Name of the sweep series the run belongs to. \cr
#' It determines the folder in which the output files will be stored for this sweep run.\cr
#' A sweep run number is automatically linked to it,
#' and subsequent sweep runs can not overwrite a previous one.\cr
#' (character string)
#'
#' @param time_units Vector defining the initial time unit
#' (identical to unit used in fluxes), \cr
#' followed by the time unit used for the graphical output.\cr
#' Character string, to be selected  among the following:\cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}\cr
#' e.g.,  c("d", "yr") to convert days into years
#'
#' @param EXPLO_MASTER Name of the dynamic sweep master file (e.g., \strong{\emph{0_EXPLO_DYN_MASTER.xlsx}}),
#' defining the dynamic sweep run scenario. \cr
#' (character string)
#'
#' @param EXPLO_AXIS_1 Set of values of sweeping parameter 1. \cr
#' See Vignette for further details.
#'
#' @param EXPLO_AXIS_2 Set of values of sweeping parameter 2. \cr
#' See Vignette for further details.
#'
#' @param to_DYN_DIGEST_CSVs \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Exports all global csv outputs to \strong{\emph{0_DYN_DIGEST}} folder (full evD and evS) if TRUE. \cr
#' Default is FALSE.
#'
#' @param plot_results \emph{OPTIONAL} \cr
#' Logical value. \cr
#' If TRUE, plots in R session the evolution of deltas as a function of time with respect to parameters 1 and 2,
#' for all system finite boxes. \cr
#' Default is TRUE.
#'
#' @param save_run_outputs  \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Allows saving all run outputs to working directory (workdir). \cr
#' By default, run outputs are stored in the temporary directory and are erased if not saved. \cr
#' Default is FALSE.
#'
#' @return Calculates the delta values and box sizes at final state of the sweeping of 2D space of parameters in all boxes.
#'
#' \code{\link{sweep_dyn}} returns by default a plot showing time evolution of delta values of the isotope composition
#' of each finite box in the 2D space defined by the two swept parameters
#' (set plot_results = FALSE to mute the plots). \cr
#'
#' The graphical results of the sweep can be also interactively explored using the
#' \code{\link{shinobxr_app}} function in case user saves the outputs to the working
#' directory (save_run_outputs = TRUE).
#'
#' \code{\link{sweep_steady}} creates a series of isotope data and metadata,
#' all of which are stored in a temporary directory. \cr
#' The user can save all outputs described below to their working directory
#' by setting save_run_outputs = TRUE (default is FALSE). \cr
#'
#' \code{\link{sweep_dyn}} creates and stores all outputs in a dedicated SERIES directory
#' with the following name structure: \cr
#' \strong{\emph{4_DYN + SERIES_ID + YYY}}, where YYY is a sweep dynamic run number automatically set between 001 and 999. \cr
#' No overwriting of previous sweep dynamic run runs is possible.
#'
#' \code{\link{sweep_steady}} base workflow:
#' \enumerate{
#' \item Calculates the number of single runs the sweeping will require depending on the swept parameters.
#' \item Asks the user confirmation to run \code{\link{sweep_dyn}},
#' as the run calculation time depends on the number of successive sweeping runs.
#' \item Writes the set of inputs and outputs for all successive \emph{n} sweeping runs, numbered from to 1 to \emph{n} in an XXXX format,
#' with the following name formats: \cr
#' \strong{\emph{DYN + SERIES_ID + YYY + XXXX + IN.Rda}} \cr
#' \strong{\emph{DYN + SERIES_ID + YYY + XXXX + OUT.Rda}} \cr
#' (see \code{\link{run_isobxr}} documentation).
#'
#' \item Writes summarized results in the  \strong{\emph{0_DYN_DIGEST}} folder:
#' \enumerate{
#' \item Archived LOG file of local sweep dynamic run. \cr
#' (file name structure:  \strong{\emph{DYN + SERIES_ID + YYY + _LOG.csv}})
#' \item Archived sweep dynamic master file. \cr
#' (file name structure:  \strong{\emph{DYN + SERIES_ID + YYY + _MASTER.xlsx}})
#' \item Dataset of temporal evolution of delta values (evD) in all boxes over the \emph{n} runs that constitute the sweep dynamic run. \cr
#' (file name structure: \strong{\emph{DYN + SERIES_ID + YYY + evD.RDS}})
#' \item Dataset of temporal evolution of box sizes (evS, masses of X) in all boxes over the \emph{n} runs that constitute the sweep dynamic run. \cr
#' (file name structure: \strong{\emph{DYN + SERIES_ID + YYY + evS.RDS}})
#' }
#' }
#'
#' @section Optional output:
#' \enumerate{
#' \item If to_DYN_DIGEST_CSVs = TRUE \cr
#' In the \strong{\emph{0_DYN_DIGEST}} folder,
#' edits csv versions of the sweep dynamic run datasets (full evD and evS) with the following name structures:
#' \enumerate{
#' \item \strong{\emph{DYN + SERIES_ID + YYY + evD.csv}}
#' \item \strong{\emph{DYN + SERIES_ID + YYY + evS.csv}}
#' }
#' }
#' For examples, see
#' https://ttacail.github.io/isobxr_web/vgn_08_sweep_dyn.html#4_Tutorial_example
#'
#' @export
sweep_dyn <- function(workdir,
                      SERIES_ID,
                      time_units,
                      EXPLO_MASTER,
                      EXPLO_AXIS_1,
                      EXPLO_AXIS_2,
                      to_DYN_DIGEST_CSVs = FALSE,
                      plot_results = TRUE,
                      save_run_outputs = FALSE
                      ){

  # locally bind variables (fixing binding global variable issue)
  INITIAL_IN <- FLUXES_IN <- COEFFS_IN <- A_OUT <- N_OUT <- A_evD <- N_evD <- N_evS <- Time_plot <- Y <- Z <- NULL

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# set up extdata tutorial mode
  tuto_setup <- using_extdata_tutorial(workdir = workdir, save_run_outputs = save_run_outputs, plot_results = plot_results)
  tuto_mode <- as.logical(tuto_setup[1])
  workdir <- tuto_setup[2]
  save_run_outputs <- as.logical(tuto_setup[3])
  plot_results <- as.logical(tuto_setup[4])

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# clean up
  # Clear plots
  # if(!is.null(dev.list())) dev.off()
  # Clear console
  # cat("\014")
  # Clean workspace
  # rm(list=ls())
  unlink(to_tmpdir(""), recursive = TRUE)
  on.exit(unlink(to_tmpdir(""), recursive = TRUE), add = TRUE)

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIALIZE
  #************************************** SET WORKING DIRECTORY  #----
  LOC_workdir <- workdir
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(LOC_workdir)

  rlang::inform("_______________________________________________________________________________")
  if (isTRUE(tuto_mode)){
    rlang::inform(paste("\U2139 workdir: no workdir. You are using the tutorial mode (isobxr embedded tutorial files)", sep = ""))
  } else {
    rlang::inform(paste("\U2139 workdir: ", getwd(), sep = ""))
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PREPARING ISOPYBOX ARGUMENTS FROM EXPLO_MASTER  #----
  #************************************** DEFINE LOCAL FORCINGS AND CONSTANTS #----
  RUN_LIST <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "RUN_LIST"))
  RAYLEIGH_LIST <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_RAYLEIGH"))
  DELTA_FORCING <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_DELTA"))
  FORCING_ALPHA <- as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_ALPHA"))
  FORCING_SIZE <-  as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_SIZE"))

  if (nrow(DELTA_FORCING) == 0){
    DELTA_FORCING = NULL
  }

  if (nrow(FORCING_ALPHA) == 0){
    FORCING_ALPHA = NULL
  }

  if (nrow(FORCING_SIZE) == 0){
    FORCING_SIZE = NULL
  }

  t_lim_list <- as.numeric(RUN_LIST$t_lim_list)
  nb_steps_list <- as.numeric(RUN_LIST$nb_step_list)
  flux_list <- as.character(RUN_LIST$flux_list)
  coeff_list <- as.character(RUN_LIST$coeff_list)

  #************************************** DEFINE EXPLO SERIES FAMILY, EXPLO SERIES NUMBER, SERIES_ID #----
  dir_LOG <- "1_LOG.csv"
  n_zeros <- 4
  if (file.exists(dir_LOG) == TRUE){
    file.copy(from = dir_LOG, to = to_tmpdir(dir_LOG))
    LOG <- data.table::fread(to_tmpdir(dir_LOG), data.table = F, stringsAsFactors = T)
    LOG_EXPLO <- LOG[LOG$EXPLORER == TRUE, ]
    remove(LOG)
    EXPLO_SERIES_FAMILY <- paste("DYN", as.character(SERIES_ID), sep = "_")
    if (nrow(LOG_EXPLO[LOG_EXPLO$EXPLO_SERIES_FAMILY == EXPLO_SERIES_FAMILY,]) == 0){
      EXPLO_SERIES_n <- 1
      SERIES_ID <- paste("DYN", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
    } else {
      EXPLO_SERIES_n <- max(LOG_EXPLO[LOG_EXPLO$EXPLO_SERIES_FAMILY == EXPLO_SERIES_FAMILY, "EXPLO_SERIES_n"])+1
      EXPLO_SERIES_n_length <- length(unlist(strsplit(as.character(EXPLO_SERIES_n), "")))
      SERIES_ID <- paste("DYN", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-EXPLO_SERIES_n_length,0),EXPLO_SERIES_n)), collapse = ""), sep = "_")
    }
  } else {
    EXPLO_SERIES_n <- 1
    EXPLO_SERIES_FAMILY <- paste("DYN", as.character(SERIES_ID), sep = "_")
    SERIES_ID <- paste("DYN", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
  }

  #************************************** READ CONSTANTS FROM ISOPY_MASTER_file #----
  ISOPY_MASTER_file <- "0_ISOBXR_MASTER.xlsx"
  CONSTANTS <- as.data.frame(readxl::read_excel(ISOPY_MASTER_file, "CONSTANTS"))

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# BUILD EXPLO AXIS LISTS #----
  #**************************************  EXPLO_AXIS_1 #----
  if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_n_FLUX_MATRICES"){
    EXPLO_AXIS_1_range <- EXPLO_AXIS_1[,c("VALUES_1", "VALUES_2")]
    EXPLO_AXIS_1_leng <- nrow(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- EXPLO_AXIS_1_range
  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_n_ALPHA_MATRICES"){
    EXPLO_AXIS_1_range <- EXPLO_AXIS_1[,c("VALUES_1", "VALUES_2")]
    EXPLO_AXIS_1_leng <- nrow(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- EXPLO_AXIS_1_range
  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_1_ALPHA"){
    EXPLO_AXIS_1_range <- seq(EXPLO_AXIS_1$ALPHA_MIN, EXPLO_AXIS_1$ALPHA_MAX, by = EXPLO_AXIS_1$ALPHA_STEPS)
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- data.frame(FROM = EXPLO_AXIS_1$FROM,
                                            TO = EXPLO_AXIS_1$TO,
                                            ALPHA = EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_forcing_list$FROM_TO <- as.factor(paste(EXPLO_AXIS_1_forcing_list$FROM, EXPLO_AXIS_1_forcing_list$TO, sep = "_"))

  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_1_SIZE"){
    EXPLO_AXIS_1_range <- seq(EXPLO_AXIS_1$SIZE_MIN, EXPLO_AXIS_1$SIZE_MAX, by = EXPLO_AXIS_1$SIZE_STEPS)
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- data.frame(BOXES_ID = EXPLO_AXIS_1$BOXES_ID,
                                            SIZE_INIT = EXPLO_AXIS_1_range)

  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_1_DELTA"){
    EXPLO_AXIS_1_range <- seq(EXPLO_AXIS_1$DELTA_MIN, EXPLO_AXIS_1$DELTA_MAX, by = EXPLO_AXIS_1$DELTA_STEPS)
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- data.frame(BOXES_ID = EXPLO_AXIS_1$BOXES_ID,
                                            DELTA_INIT = EXPLO_AXIS_1_range)

  } else if (EXPLO_AXIS_1[1,"EXPLO_TYPES"] == "EXPLO_1_RAYLEIGH_ALPHA"){
    EXPLO_AXIS_1_range <- seq(EXPLO_AXIS_1$ALPHA_0_MIN, EXPLO_AXIS_1$ALPHA_0_MAX, by = EXPLO_AXIS_1$ALPHA_0_STEPS)
    EXPLO_AXIS_1_leng <- length(EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_type <- as.character(EXPLO_AXIS_1[1,"EXPLO_TYPES"])
    EXPLO_AXIS_1_forcing_list <- data.frame(XFROM = EXPLO_AXIS_1$XFROM,
                                            XTO = EXPLO_AXIS_1$XTO,
                                            YFROM = EXPLO_AXIS_1$YFROM,
                                            YTO = EXPLO_AXIS_1$YTO,
                                            AFROM = EXPLO_AXIS_1$AFROM,
                                            ATO = EXPLO_AXIS_1$ATO,
                                            ALPHA_0 = EXPLO_AXIS_1_range)
    EXPLO_AXIS_1_forcing_list$ALL_DESC <- as.factor(paste(EXPLO_AXIS_1_forcing_list$XFROM,
                                                          EXPLO_AXIS_1_forcing_list$XTO,
                                                          EXPLO_AXIS_1_forcing_list$YFROM,
                                                          EXPLO_AXIS_1_forcing_list$YTO,
                                                          EXPLO_AXIS_1_forcing_list$AFROM,
                                                          EXPLO_AXIS_1_forcing_list$ATO,
                                                          sep = "_"))
  }

  #**************************************  EXPLO_AXIS_2 #----
  if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_n_FLUX_MATRICES"){
    EXPLO_AXIS_2_range <- EXPLO_AXIS_2[,c("VALUES_1", "VALUES_2")]
    EXPLO_AXIS_2_leng <- nrow(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- EXPLO_AXIS_2_range
  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_n_ALPHA_MATRICES"){
    EXPLO_AXIS_2_range <- EXPLO_AXIS_2[,c("VALUES_1", "VALUES_2")]
    EXPLO_AXIS_2_leng <- nrow(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- EXPLO_AXIS_2_range
  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_1_ALPHA"){
    EXPLO_AXIS_2_range <- seq(EXPLO_AXIS_2$ALPHA_MIN, EXPLO_AXIS_2$ALPHA_MAX, by = EXPLO_AXIS_2$ALPHA_STEPS)
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- data.frame(FROM = EXPLO_AXIS_2$FROM,
                                            TO = EXPLO_AXIS_2$TO,
                                            ALPHA = EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_forcing_list$FROM_TO <- as.factor(paste(EXPLO_AXIS_2_forcing_list$FROM, EXPLO_AXIS_2_forcing_list$TO, sep = "_"))

  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_1_SIZE"){
    EXPLO_AXIS_2_range <- seq(EXPLO_AXIS_2$SIZE_MIN, EXPLO_AXIS_2$SIZE_MAX, by = EXPLO_AXIS_2$SIZE_STEPS)
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- data.frame(BOXES_ID = EXPLO_AXIS_2$BOXES_ID,
                                            SIZE_INIT = EXPLO_AXIS_2_range)

  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_1_DELTA"){
    EXPLO_AXIS_2_range <- seq(EXPLO_AXIS_2$DELTA_MIN, EXPLO_AXIS_2$DELTA_MAX, by = EXPLO_AXIS_2$DELTA_STEPS)
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- data.frame(BOXES_ID = EXPLO_AXIS_2$BOXES_ID,
                                            DELTA_INIT = EXPLO_AXIS_2_range)

  } else if (EXPLO_AXIS_2[1,"EXPLO_TYPES"] == "EXPLO_1_RAYLEIGH_ALPHA"){
    EXPLO_AXIS_2_range <- seq(EXPLO_AXIS_2$ALPHA_0_MIN, EXPLO_AXIS_2$ALPHA_0_MAX, by = EXPLO_AXIS_2$ALPHA_0_STEPS)
    EXPLO_AXIS_2_leng <- length(EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_type <- as.character(EXPLO_AXIS_2[1,"EXPLO_TYPES"])
    EXPLO_AXIS_2_forcing_list <- data.frame(XFROM = EXPLO_AXIS_2$XFROM,
                                            XTO = EXPLO_AXIS_2$XTO,
                                            YFROM = EXPLO_AXIS_2$YFROM,
                                            YTO = EXPLO_AXIS_2$YTO,
                                            AFROM = EXPLO_AXIS_2$AFROM,
                                            ATO = EXPLO_AXIS_2$ATO,
                                            ALPHA_0 = EXPLO_AXIS_2_range)
    EXPLO_AXIS_2_forcing_list$ALL_DESC <- as.factor(paste(EXPLO_AXIS_2_forcing_list$XFROM,
                                                          EXPLO_AXIS_2_forcing_list$XTO,
                                                          EXPLO_AXIS_2_forcing_list$YFROM,
                                                          EXPLO_AXIS_2_forcing_list$YTO,
                                                          EXPLO_AXIS_2_forcing_list$AFROM,
                                                          EXPLO_AXIS_2_forcing_list$ATO,
                                                          sep = "_"))
  }

  #**************************************  CALCULATE TOTAL NUMBER OF RUNS #----
  tot_run <- EXPLO_AXIS_1_leng * EXPLO_AXIS_2_leng

  STOP_GO <- FALSE
  rlang::inform("________________________________________________________________________________")
  if(interactive()){
    if (.Platform$OS.type == "windows"){
      STOP_GO <- utils::askYesNo(paste("? This sweep requires *", as.character(tot_run), "* independent runs, do you wish to carry on? \n"), default = TRUE)
    } else {
      STOP_GO <- utils::askYesNo(cat("? This sweep requires *", as.character(tot_run), "* independent runs, do you wish to carry on? \n"), default = TRUE)
    }
  } else {
    rlang::inform(paste("\U2139 This sweep requires ", as.character(tot_run), " independent runs."))
    STOP_GO <- TRUE
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# SWEEP THE SPACE OF PARAMETERS #----
  if (!isTRUE(STOP_GO)){
    rlang::abort("\U2757 You probably want to reduce the number of iterations in each EXPLO axis.")
  } else {
    rlang::inform("________________________________________________________________________________")
    rlang::inform("\U0001f535 COMPUTING SWEEP of RUN #1 & #2 ")
    pb_cpt <- utils::txtProgressBar(min = 1, max = tot_run, style = 3, width = 60)
    clock <- 1
    k <- 1
    for (k in 1:EXPLO_AXIS_1_leng){
      l <- 1
      for (l in 1:EXPLO_AXIS_2_leng){
        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# RUN 1/2, i in [1:n] #----
        i <- 1
        #************************************** FORCING FROM EXPLO_DYN_MASTER #----
        if (is.null(DELTA_FORCING) == FALSE & i %in% DELTA_FORCING$COMPO_RUN_n){
          DELTA_FORCING_loc <- DELTA_FORCING[DELTA_FORCING$COMPO_RUN_n == i, c("BOXES_ID", "DELTA_INIT")]
        } else {
          DELTA_FORCING_loc = NULL
        }

        if (is.null(FORCING_SIZE) == FALSE & i %in% FORCING_SIZE$COMPO_RUN_n){
          FORCING_SIZE_loc <- FORCING_SIZE[FORCING_SIZE$COMPO_RUN_n == i, c("BOXES_ID", "SIZE_INIT")]
        } else {
          FORCING_SIZE_loc = NULL
        }

        i <- 1
        if (is.null(FORCING_ALPHA) == FALSE & i %in% FORCING_ALPHA$COMPO_RUN_n){
          FORCING_ALPHA_loc <- FORCING_ALPHA[FORCING_ALPHA$COMPO_RUN_n == i, c("FROM", "TO", "ALPHA")]
        } else {
          FORCING_ALPHA_loc = NULL
        }

        fx <- flux_list[i]
        a <- coeff_list[i]
        LOC_t_lim <- t_lim_list[i]
        LOC_nb_steps <- nb_steps_list[i]

        if (i %in% RAYLEIGH_LIST$COMPO_RUN_n){
          LOC_RAYLEIGH <- clear_subset(RAYLEIGH_LIST[RAYLEIGH_LIST$COMPO_RUN_n == i,-which(names(RAYLEIGH_LIST) %in% c("COMPO_RUN_n"))])
          LOC_RAYLEIGH$ALL_DESC <- as.factor(paste(LOC_RAYLEIGH$XFROM, LOC_RAYLEIGH$XTO, LOC_RAYLEIGH$YFROM, LOC_RAYLEIGH$YTO, LOC_RAYLEIGH$AFROM, LOC_RAYLEIGH$ATO, sep = "_"))
        } else {
          LOC_RAYLEIGH <- NULL
        }

        #************************************** FORCING FROM EXPLO RANGES 1 AND 2 - STEP 1 (VALUES_1) #----
        #************************************** UPDATE ISOPYRUN ARGUMENTS WITH EXPLO RANGES TO EXPLORE # OVERWRITE EXPLO_DYN_MASTER FORCINGS IF NEEDED #----
        #************************************** UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_1 VALUE #----
        if (EXPLO_AXIS_1_type == "EXPLO_n_FLUX_MATRICES"){            ##### IF
          fx <- as.character(EXPLO_AXIS_1_forcing_list[k,"VALUES_1"])
        } else if (EXPLO_AXIS_1_type == "EXPLO_n_ALPHA_MATRICES"){    ##### IF
          a <- as.character(EXPLO_AXIS_1_forcing_list[k,"VALUES_1"])
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_ALPHA"){             ##### IF
          if (is.null(FORCING_ALPHA_loc)){
            FORCING_ALPHA_loc <- clear_subset(EXPLO_AXIS_1_forcing_list[k,])
          } else if (EXPLO_AXIS_1_forcing_list[k,"FROM_TO"] %in% FORCING_ALPHA_loc$FROM_TO){
            FORCING_ALPHA_loc[FORCING_ALPHA_loc$FROM_TO == as.character(EXPLO_AXIS_1_forcing_list[k,"FROM_TO"]), "ALPHA"] <- EXPLO_AXIS_1_forcing_list[k,"ALPHA"]
          } else {
            FORCING_ALPHA_loc <- rbind(FORCING_ALPHA_loc, EXPLO_AXIS_1_forcing_list[k,])
          }
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_SIZE"){              ##### IF
          if (is.null(FORCING_SIZE_loc)){
            FORCING_SIZE_loc <- EXPLO_AXIS_1_forcing_list[k,]
          } else if (EXPLO_AXIS_1_forcing_list[k,"BOXES_ID"] %in% FORCING_SIZE_loc$BOXES_ID){
            FORCING_SIZE_loc[FORCING_SIZE_loc$BOXES_ID == as.character(EXPLO_AXIS_1_forcing_list[k,"BOXES_ID"]), "SIZE_INIT"] <- EXPLO_AXIS_1_forcing_list[k, "SIZE_INIT"]
          } else {
            FORCING_SIZE_loc <- rbind(FORCING_SIZE_loc, EXPLO_AXIS_1_forcing_list[k,])
          }
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_DELTA"){             ##### IF
          if (is.null(DELTA_FORCING_loc)){
            DELTA_FORCING_loc <- EXPLO_AXIS_1_forcing_list[k,]
          } else if (EXPLO_AXIS_1_forcing_list[k,"BOXES_ID"] %in% DELTA_FORCING_loc$BOXES_ID){
            DELTA_FORCING_loc[DELTA_FORCING_loc$BOXES_ID == as.character(EXPLO_AXIS_1_forcing_list[k,"BOXES_ID"]), "DELTA_INIT"] <- EXPLO_AXIS_1_forcing_list[k, "DELTA_INIT"]
          } else {
            DELTA_FORCING_loc <- rbind(DELTA_FORCING_loc, EXPLO_AXIS_1_forcing_list[k,])
          }
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_RAYLEIGH_ALPHA"){    ##### IF
          if(is.null(LOC_RAYLEIGH)){
            LOC_RAYLEIGH <- clear_subset(EXPLO_AXIS_1_forcing_list[k,])
          } else if (EXPLO_AXIS_1_forcing_list[k,"ALL_DESC"] %in% as.character(LOC_RAYLEIGH$ALL_DESC)){
            LOC_RAYLEIGH[LOC_RAYLEIGH$ALL_DESC == EXPLO_AXIS_1_forcing_list[k,"ALL_DESC"], "ALPHA_0"] <- EXPLO_AXIS_1_forcing_list[k,"ALPHA_0"]
          } else {
            LOC_RAYLEIGH <- rbind(LOC_RAYLEIGH, EXPLO_AXIS_1_forcing_list[k,])
          }
        }

        #************************************** UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_2 VALUE #----
        if (EXPLO_AXIS_2_type == "EXPLO_n_FLUX_MATRICES"){            ##### IF
          fx <- as.character(EXPLO_AXIS_2_forcing_list[l,"VALUES_1"])
        } else if (EXPLO_AXIS_2_type == "EXPLO_n_ALPHA_MATRICES"){    ##### IF
          a <- as.character(EXPLO_AXIS_2_forcing_list[l,"VALUES_1"])
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_ALPHA"){             ##### IF
          if (is.null(FORCING_ALPHA_loc)){
            FORCING_ALPHA_loc <- clear_subset(EXPLO_AXIS_2_forcing_list[l,])
          } else if (EXPLO_AXIS_2_forcing_list[l,"FROM_TO"] %in% FORCING_ALPHA_loc$FROM_TO){
            FORCING_ALPHA_loc[FORCING_ALPHA_loc$FROM_TO == as.character(EXPLO_AXIS_2_forcing_list[l,"FROM_TO"]), "ALPHA"] <- EXPLO_AXIS_2_forcing_list[l,"ALPHA"]
          } else {
            FORCING_ALPHA_loc <- rbind(FORCING_ALPHA_loc, EXPLO_AXIS_2_forcing_list[l,])
          }
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_SIZE"){              ##### IF
          if (is.null(FORCING_SIZE_loc)){
            FORCING_SIZE_loc <- EXPLO_AXIS_2_forcing_list[k,]
          } else if (EXPLO_AXIS_2_forcing_list[k,"BOXES_ID"] %in% FORCING_SIZE_loc$BOXES_ID){
            FORCING_SIZE_loc[FORCING_SIZE_loc$BOXES_ID == as.character(EXPLO_AXIS_2_forcing_list[k,"BOXES_ID"]), "SIZE_INIT"] <- EXPLO_AXIS_2_forcing_list[k, "SIZE_INIT"]
          } else {
            FORCING_SIZE_loc <- rbind(FORCING_SIZE_loc, EXPLO_AXIS_2_forcing_list[k,])
          }
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_DELTA"){             ##### IF
          if (is.null(DELTA_FORCING_loc)){
            DELTA_FORCING_loc <- EXPLO_AXIS_2_forcing_list[k,]
          } else if (EXPLO_AXIS_2_forcing_list[k,"BOXES_ID"] %in% DELTA_FORCING_loc$BOXES_ID){
            DELTA_FORCING_loc[DELTA_FORCING_loc$BOXES_ID == as.character(EXPLO_AXIS_2_forcing_list[k,"BOXES_ID"]), "DELTA_INIT"] <- EXPLO_AXIS_2_forcing_list[k, "DELTA_INIT"]
          } else {
            DELTA_FORCING_loc <- rbind(DELTA_FORCING_loc, EXPLO_AXIS_2_forcing_list[k,])
          }
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_RAYLEIGH_ALPHA"){    ##### IF
          if(is.null(LOC_RAYLEIGH)){
            LOC_RAYLEIGH <- clear_subset(EXPLO_AXIS_2_forcing_list[l,])
          } else if (EXPLO_AXIS_2_forcing_list[l,"ALL_DESC"] %in% as.character(LOC_RAYLEIGH$ALL_DESC)){
            LOC_RAYLEIGH[LOC_RAYLEIGH$ALL_DESC == EXPLO_AXIS_2_forcing_list[l,"ALL_DESC"], "ALPHA_0"] <- EXPLO_AXIS_2_forcing_list[l,"ALPHA_0"]
          } else {
            LOC_RAYLEIGH <- rbind(LOC_RAYLEIGH, EXPLO_AXIS_2_forcing_list[l,])
          }
        }
        #************************************** RUN  #----
        quiet(run_isobxr(workdir = LOC_workdir,
                         SERIES_ID = SERIES_ID,
                         flux_list_name = fx,
                         coeff_list_name = a,
                         t_lim = LOC_t_lim,
                         nb_steps = LOC_nb_steps,
                         time_units,
                         FORCING_RAYLEIGH <- LOC_RAYLEIGH,
                         FORCING_SIZE = FORCING_SIZE_loc,
                         FORCING_DELTA = DELTA_FORCING_loc,
                         FORCING_ALPHA = FORCING_ALPHA_loc,
                         COMPOSITE = FALSE,
                         COMPO_SERIES_n = NaN,
                         COMPO_SERIES_FAMILY = NaN,
                         EXPLORER = TRUE,
                         EXPLO_SERIES_n = EXPLO_SERIES_n,
                         EXPLO_SERIES_FAMILY = EXPLO_SERIES_FAMILY,
                         HIDE_PRINTS = TRUE,
                         to_DIGEST_DIAGRAMS = FALSE,
                         to_DIGEST_evD_PLOT = FALSE,
                         plot_results = FALSE))

        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# RUN 2/2, i in [1:n] #----
        i <- 2
        #************************************** PREPARING INPUTS for ISOPY_RUN with EXPLO_MASTER as default and Taking final state of run 2/2 as initial #----
        fx <- flux_list[i]
        a <- coeff_list[i]
        LOC_t_lim <- t_lim_list[i]
        LOC_nb_steps <- nb_steps_list[i]

        if (i %in% RAYLEIGH_LIST$COMPO_RUN_n){
          LOC_RAYLEIGH <- clear_subset(RAYLEIGH_LIST[RAYLEIGH_LIST$COMPO_RUN_n == i,-which(names(RAYLEIGH_LIST) %in% c("COMPO_RUN_n"))])
          LOC_RAYLEIGH$ALL_DESC <- as.factor(paste(LOC_RAYLEIGH$XFROM, LOC_RAYLEIGH$XTO, LOC_RAYLEIGH$YFROM, LOC_RAYLEIGH$YTO, LOC_RAYLEIGH$AFROM, LOC_RAYLEIGH$ATO, sep = "_"))
        } else {
          LOC_RAYLEIGH <- NULL
        }
        LOG <- data.table::fread(to_tmpdir(dir_LOG), data.table = F, stringsAsFactors = T)
        LOG_last <- LOG[nrow(LOG),]
        remove(LOG)

        if (LOG_last$NUM_ANA == "ANA"){
          load(to_tmpdir(paste(LOG_last$path_outdir, "OUT.Rda", sep = "")))
          OUT_last_final <- A_OUT

          OUT_last_SIZE_FINAL <- OUT_last_final[, c("BOXES_ID", "SIZE_INIT")]
          OUT_last_DELTA_FINAL <- OUT_last_final[, c("BOXES_ID", "DELTA_FINAL")]
          names(OUT_last_DELTA_FINAL) <- c("BOXES_ID", "DELTA_INIT")
        } else {
          load(to_tmpdir(paste(LOG_last$path_outdir, "OUT.Rda", sep = "")))
          OUT_last_final <- N_OUT

          OUT_last_SIZE_FINAL <- OUT_last_final[, c("BOXES_ID", "SIZE_FINAL")]
          names(OUT_last_SIZE_FINAL) <- c("BOXES_ID", "SIZE_INIT")
          OUT_last_DELTA_FINAL <- OUT_last_final[, c("BOXES_ID", "DELTA_FINAL")]
          names(OUT_last_DELTA_FINAL) <- c("BOXES_ID", "DELTA_INIT")
        }
        LOC_SIZE_INIT <- OUT_last_SIZE_FINAL
        LOC_DELTA_INIT <- OUT_last_DELTA_FINAL
        LOC_SIZE_INIT <- clear_subset(LOC_SIZE_INIT)
        LOC_DELTA_INIT <- clear_subset(LOC_DELTA_INIT)

        #************************************** FORCE OVERWRITING DELTA_INIT FROM EXPLO_MASTER #----
        if (is.null(DELTA_FORCING) == FALSE & i %in% DELTA_FORCING$COMPO_RUN_n){
          DELTA_FORCING_loc <- DELTA_FORCING[DELTA_FORCING$COMPO_RUN_n == i, c("BOXES_ID", "DELTA_INIT")]
          DELTA_FORCING_loc <- clear_subset(DELTA_FORCING_loc)
          j <- 1
          for (j in 1:nrow(DELTA_FORCING_loc)){
            LOC_DELTA_INIT[LOC_DELTA_INIT$BOXES_ID == as.character(DELTA_FORCING_loc[j, "BOXES_ID"]), "DELTA_INIT"] = DELTA_FORCING_loc[j, "DELTA_INIT"]
            j <- j + 1
          }
        }

        #************************************** FORCE OVERWRITING SIZE_INIT FROM EXPLO_MASTER #----
        if (is.null(FORCING_SIZE) == FALSE & i %in% FORCING_SIZE$COMPO_RUN_n){
          FORCING_SIZE_loc <- FORCING_SIZE[FORCING_SIZE$COMPO_RUN_n == i, c("BOXES_ID", "SIZE_INIT")]
          FORCING_SIZE_loc <- clear_subset(FORCING_SIZE_loc)
          j <- 1
          for (j in 1:nrow(FORCING_SIZE_loc)){
            LOC_SIZE_INIT[LOC_SIZE_INIT$BOXES_ID == as.character(FORCING_SIZE_loc[j, "BOXES_ID"]), "SIZE_INIT"] = FORCING_SIZE_loc[j, "SIZE_INIT"]
            j <- j + 1
          }
        }

        #************************************** FORCE OVERWRITING ALPHA FROM EXPLO_MASTER #----
        if (is.null(FORCING_ALPHA) == FALSE& i %in% FORCING_ALPHA$COMPO_RUN_n){
          FORCING_ALPHA_loc <- FORCING_ALPHA[FORCING_ALPHA$COMPO_RUN_n == i, c("FROM", "TO", "ALPHA")]
          FORCING_ALPHA_loc$FROM_TO <- paste(FORCING_ALPHA_loc$FROM, FORCING_ALPHA_loc$TO, sep = "_")
          FORCING_ALPHA_loc <- clear_subset(FORCING_ALPHA_loc)
        } else {
          FORCING_ALPHA_loc = NULL
        }

        #************************************** FORCING FROM EXPLO RANGES 1 AND 2 - STEP 2 (VALUES_2) #----
        #************************************** UPDATE ISOPYRUN ARGUMENTS WITH EXPLO RANGES TO EXPLORE # OVERWRITE EXPLO_MASTER FORCINGS IF NEEDED #----
        #************************************** UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_1 VALUE #----
        if (EXPLO_AXIS_1_type == "EXPLO_n_FLUX_MATRICES"){            ##### IF
          fx <- as.character(EXPLO_AXIS_1_forcing_list[k,"VALUES_2"])
        } else if (EXPLO_AXIS_1_type == "EXPLO_n_ALPHA_MATRICES"){    ##### IF
          a <- as.character(EXPLO_AXIS_1_forcing_list[k,"VALUES_2"])
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_ALPHA"){             ##### IF
          if (is.null(FORCING_ALPHA_loc)){
            FORCING_ALPHA_loc <- clear_subset(EXPLO_AXIS_1_forcing_list[k,])
          } else if (EXPLO_AXIS_1_forcing_list[k,"FROM_TO"] %in% FORCING_ALPHA_loc$FROM_TO){
            FORCING_ALPHA_loc[FORCING_ALPHA_loc$FROM_TO == as.character(EXPLO_AXIS_1_forcing_list[k,"FROM_TO"]), "ALPHA"] <- EXPLO_AXIS_1_forcing_list[k,"ALPHA"]
          } else {
            FORCING_ALPHA_loc <- rbind(FORCING_ALPHA_loc, EXPLO_AXIS_1_forcing_list[k,])
          }
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_SIZE"){              ##### IF
          LOC_SIZE_INIT[LOC_SIZE_INIT$BOXES_ID == as.character(EXPLO_AXIS_1_forcing_list[k, "BOXES_ID"]), "SIZE_INIT"] <- EXPLO_AXIS_1_forcing_list[k,"SIZE_INIT"]
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_DELTA"){             ##### IF
          LOC_DELTA_INIT[LOC_DELTA_INIT$BOXES_ID == as.character(EXPLO_AXIS_1_forcing_list[k, "BOXES_ID"]), "DELTA_INIT"] <- EXPLO_AXIS_1_forcing_list[k,"DELTA_INIT"]
        } else if (EXPLO_AXIS_1_type == "EXPLO_1_RAYLEIGH_ALPHA"){    ##### IF
          if(is.null(LOC_RAYLEIGH)){
            LOC_RAYLEIGH <- clear_subset(EXPLO_AXIS_1_forcing_list[k,])
          } else if (EXPLO_AXIS_1_forcing_list[k,"ALL_DESC"] %in% as.character(LOC_RAYLEIGH$ALL_DESC)){
            LOC_RAYLEIGH[LOC_RAYLEIGH$ALL_DESC == EXPLO_AXIS_1_forcing_list[k,"ALL_DESC"], "ALPHA_0"] <- EXPLO_AXIS_1_forcing_list[k,"ALPHA_0"]
          } else {
            LOC_RAYLEIGH <- rbind(LOC_RAYLEIGH, EXPLO_AXIS_1_forcing_list[k,])
          }
        }

        #************************************** UPDATE ARGUMENTS WITH LOCAL EXPLO_AXIS_2 VALUE #----
        if (EXPLO_AXIS_2_type == "EXPLO_n_FLUX_MATRICES"){            ##### IF
          fx <- as.character(EXPLO_AXIS_2_forcing_list[l,"VALUES_2"])
        } else if (EXPLO_AXIS_2_type == "EXPLO_n_ALPHA_MATRICES"){    ##### IF
          a <- as.character(EXPLO_AXIS_2_forcing_list[l,"VALUES_2"])
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_ALPHA"){             ##### IF
          if (is.null(FORCING_ALPHA_loc)){
            FORCING_ALPHA_loc <- clear_subset(EXPLO_AXIS_2_forcing_list[l,])
          } else if (EXPLO_AXIS_2_forcing_list[l,"FROM_TO"] %in% FORCING_ALPHA_loc$FROM_TO){
            FORCING_ALPHA_loc[FORCING_ALPHA_loc$FROM_TO == as.character(EXPLO_AXIS_2_forcing_list[l,"FROM_TO"]), "ALPHA"] <- EXPLO_AXIS_2_forcing_list[l,"ALPHA"]
          } else {
            FORCING_ALPHA_loc <- rbind(FORCING_ALPHA_loc, EXPLO_AXIS_2_forcing_list[l,])
          }
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_SIZE"){              ##### IF
          LOC_SIZE_INIT[LOC_SIZE_INIT$BOXES_ID == as.character(EXPLO_AXIS_2_forcing_list[l, "BOXES_ID"]), "SIZE_INIT"] <- EXPLO_AXIS_2_forcing_list[l,"SIZE_INIT"]
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_DELTA"){             ##### IF
          LOC_DELTA_INIT[LOC_DELTA_INIT$BOXES_ID == as.character(EXPLO_AXIS_2_forcing_list[l, "BOXES_ID"]), "DELTA_INIT"] <- EXPLO_AXIS_2_forcing_list[l,"DELTA_INIT"]
        } else if (EXPLO_AXIS_2_type == "EXPLO_1_RAYLEIGH_ALPHA"){    ##### IF
          if(is.null(LOC_RAYLEIGH)){
            LOC_RAYLEIGH <- clear_subset(EXPLO_AXIS_2_forcing_list[l,])
          } else if (EXPLO_AXIS_2_forcing_list[l,"ALL_DESC"] %in% as.character(LOC_RAYLEIGH$ALL_DESC)){
            LOC_RAYLEIGH[LOC_RAYLEIGH$ALL_DESC == EXPLO_AXIS_2_forcing_list[l,"ALL_DESC"], "ALPHA_0"] <- EXPLO_AXIS_2_forcing_list[l,"ALPHA_0"]
          } else {
            LOC_RAYLEIGH <- rbind(LOC_RAYLEIGH, EXPLO_AXIS_2_forcing_list[l,])
          }
        }

        #************************************** RUN #----
        quiet(run_isobxr(workdir = LOC_workdir,
                         SERIES_ID = SERIES_ID,
                         flux_list_name = fx,
                         coeff_list_name = a,
                         t_lim = LOC_t_lim,
                         nb_steps = LOC_nb_steps,
                         time_units,
                         FORCING_RAYLEIGH <- LOC_RAYLEIGH,
                         FORCING_SIZE <- LOC_SIZE_INIT,
                         FORCING_DELTA <- LOC_DELTA_INIT,
                         FORCING_ALPHA <-  FORCING_ALPHA_loc,
                         COMPOSITE = FALSE,
                         COMPO_SERIES_n = NaN,
                         COMPO_SERIES_FAMILY = NaN,
                         EXPLORER = TRUE,
                         EXPLO_SERIES_n = EXPLO_SERIES_n,
                         EXPLO_SERIES_FAMILY = EXPLO_SERIES_FAMILY,
                         HIDE_PRINTS = TRUE,
                         to_DIGEST_DIAGRAMS = FALSE,
                         to_DIGEST_evD_PLOT = FALSE,
                         plot_results = FALSE))

        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# CLOCK #----
        utils::setTxtProgressBar(pb_cpt, clock)

        clock <- clock + 1
        l <- l + 1
      }
      k <- k + 1
    }
    close(pb_cpt)

    #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# SUMMARY of EXPLOR SPACE #----
    if (EXPLO_AXIS_1_type %in% c("EXPLO_n_ALPHA_MATRICES", "EXPLO_n_FLUX_MATRICES")){
      EXPLO_AXIS_1$VALUES <- paste(EXPLO_AXIS_1$VALUES_1, EXPLO_AXIS_1$VALUES_2, sep = ".")
      EXPLO_AXIS_1_toEXPLOG <- EXPLO_AXIS_1[, c("EXPLO_TYPES", "VALUES")]
    } else {
      EXPLO_AXIS_1_toEXPLOG <- EXPLO_AXIS_1_forcing_list
      EXPLO_AXIS_1_toEXPLOG$EXPLO_TYPES <- as.factor(EXPLO_AXIS_1_type)
    }

    if (EXPLO_AXIS_2_type %in% c("EXPLO_n_ALPHA_MATRICES", "EXPLO_n_FLUX_MATRICES")){
      EXPLO_AXIS_2$VALUES <- paste(EXPLO_AXIS_2$VALUES_1, EXPLO_AXIS_2$VALUES_2, sep = ".")
      EXPLO_AXIS_2_toEXPLOG <- EXPLO_AXIS_2[, c("EXPLO_TYPES", "VALUES")]
    } else {
      EXPLO_AXIS_2_toEXPLOG <- EXPLO_AXIS_2_forcing_list
      EXPLO_AXIS_2_toEXPLOG$EXPLO_TYPES <- as.factor(EXPLO_AXIS_2_type)
    }

    names(EXPLO_AXIS_1_toEXPLOG) <- paste(names(EXPLO_AXIS_1_toEXPLOG), "_1", sep = "")
    names(EXPLO_AXIS_2_toEXPLOG) <- paste(names(EXPLO_AXIS_2_toEXPLOG), "_2", sep = "")

    k <- 1
    Run_n <- c(1,2)
    count <- 1
    for (k in 1:EXPLO_AXIS_1_leng){
      l <- 1
      for (l in 1:EXPLO_AXIS_2_leng){
        EXPLOG_loc <- cbind(EXPLO_AXIS_1_toEXPLOG[k,], EXPLO_AXIS_2_toEXPLOG[l,])
        EXPLOG_loc$EXPLO_SERIES_n <- EXPLO_SERIES_n
        EXPLOG_loc$EXPLO_SERIES_FAMILY <- EXPLO_SERIES_FAMILY
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

    EXPLOG <- clear_subset(EXPLOG)

    if (EXPLO_AXIS_1_type %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$VALUES_1
      EXPLOG$LEGEND_EXPLO_1 <- EXPLOG$EXPLO_TYPES_1
    } else if (EXPLO_AXIS_1_type == "EXPLO_1_SIZE"){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$SIZE_INIT_1
      EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("SIZE_t0_", EXPLOG$BOXES_ID_1, sep = ""))
    } else if (EXPLO_AXIS_1_type == "EXPLO_1_DELTA"){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$DELTA_INIT_1
      EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("DELTA_t0_", EXPLOG$BOXES_ID_1, sep = ""))
    } else if (EXPLO_AXIS_1_type == "EXPLO_1_ALPHA"){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$ALPHA_1
      EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("ALPHA_", EXPLOG$FROM_TO_1, sep = ""))
    } else if (EXPLO_AXIS_1_type == "EXPLO_1_RAYLEIGH_ALPHA"){
      EXPLOG$VAR_EXPLO_1 <- EXPLOG$ALPHA_0_1
      EXPLOG$LEGEND_EXPLO_1 <- as.factor(paste("ALPHA_", EXPLOG$ALL_DESC_1, sep = ""))
    }

    if (EXPLO_AXIS_2_type %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$VALUES_2
      EXPLOG$LEGEND_EXPLO_2 <- EXPLOG$EXPLO_TYPES_2
    } else if (EXPLO_AXIS_2_type == "EXPLO_1_SIZE"){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$SIZE_INIT_2
      EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("SIZE_t0_", EXPLOG$BOXES_ID_2, sep = ""))
    } else if (EXPLO_AXIS_2_type == "EXPLO_1_DELTA"){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$DELTA_INIT_2
      EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("DELTA_t0_", EXPLOG$BOXES_ID_2, sep = ""))
    } else if (EXPLO_AXIS_2_type == "EXPLO_1_ALPHA"){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$ALPHA_2
      EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("ALPHA_", EXPLOG$FROM_TO_2, sep = ""))
    } else if (EXPLO_AXIS_2_type == "EXPLO_1_RAYLEIGH_ALPHA"){
      EXPLOG$VAR_EXPLO_2 <- EXPLOG$ALPHA_0_2
      EXPLOG$LEGEND_EXPLO_2 <- as.factor(paste("ALPHA_", EXPLOG$ALL_DESC_2, sep = ""))
    }

    #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# LOAD/EDIT COMPOSITE SERIES LOG/OUT FILES and EDIT ANA evS
    #************************************** LOAD LOG/OUT FILES of CURRENT COMPO SERIES #----
    LOG <- data.table::fread(to_tmpdir(dir_LOG), data.table = F, stringsAsFactors = T)
    LOG_SERIES <- LOG[LOG$SERIES_ID == SERIES_ID,]
    remove(LOG)
    LOG_SERIES <- dplyr::full_join(LOG_SERIES, EXPLOG, by = "RUN_n")
    LOG_SERIES <- clear_subset(LOG_SERIES)
    path_to_input_1 <- paste(LOG_SERIES[1, "path_outdir"], "IN.Rda", sep = "")
    load(to_tmpdir(path_to_input_1))
    BOXES_IDs <- as.character(INITIAL_IN$BOXES_ID)

    #************************************** READ/BUILD/MERGE evS/D and evS/D final for ANA/NUM WHOLE COMPOSITE RUN #----
    rlang::inform("________________________________________________________________________________")
    rlang::inform("\U0001f535 PREPARING RESULTS")
    pb_prep <- utils::txtProgressBar(min = 1, max = 2*tot_run, style = 3, width = 60)

    # calculation_gauge(0, (tot_run))

    i <- 2
    for (k in 1:(tot_run)){
      SERIES_RUN_ID_i <- LOG_SERIES[i, "SERIES_RUN_ID"]
      RUN_n_i <- LOG_SERIES[i, "RUN_n"]
      path_outdir_i <- as.character(LOG_SERIES[i, "path_outdir"])
      path_to_input_i <- paste(path_outdir_i, "IN.Rda", sep = "")
      load(to_tmpdir(path_to_input_i))
      INIT_i <- INITIAL_IN
      SIZE_INIT_i <- INIT_i[,c("BOXES_ID", "SIZE_INIT")]
      DELTA_INIT_i <- INIT_i[,c("BOXES_ID", "DELTA_INIT")]
      FLUXES_i <- FLUXES_IN
      COEFFS_i <- COEFFS_IN
      # if (i > 1){
      #   unlink(to_tmpdir(path_to_INPUT_i) ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option?
      # }

      if (LOG_SERIES[i, "NUM_ANA"] == "ANA"){
        load(to_tmpdir(paste(path_outdir_i, "OUT.Rda", sep = "")))
        evD_i <- A_evD
        SIZE_INIT_i_hor <- as.data.frame(t(SIZE_INIT_i$SIZE_INIT))
        names(SIZE_INIT_i_hor) <- SIZE_INIT_i$BOXES_ID
        SIZE_INIT_i_hor$Time = NaN
        evS_i <- evD_i
        j <- 1
        for (j in 1:length(BOXES_IDs)){
          evS_i[,BOXES_IDs[j]] <- SIZE_INIT_i_hor[1, BOXES_IDs[j]]
          j <- j + 1
        }
      } else {
        if (LOG_SERIES[i, "NUM_ANA"] == "NUM"){
          load(to_tmpdir(paste(path_outdir_i, "OUT.Rda", sep = "")))
          evD_i <- N_evD
          evS_i <- N_evS
        }
      }

      FLUXES_i_colnames <- names(FLUXES_i)
      FLUXES_i_vert <- data.frame(VAR_TYPE = "FLUX",
                                  VARIABLE = NaN,
                                  VALUE = NaN)
      FLUXES_i_vert_loc <- FLUXES_i_vert
      j <- i
      for (j in 1:nrow(FLUXES_i)){
        k <- 1
        for (k in 1:(length(FLUXES_i)-1)){
          FLUXES_i_vert_loc$VALUE = FLUXES_i[j,k+1]
          FLUXES_i_vert_loc$VARIABLE = paste("f", FLUXES_i[j, "BOXES_ID"], FLUXES_i_colnames[k+1], sep = "_")
          FLUXES_i_vert <- rbind(FLUXES_i_vert, FLUXES_i_vert_loc)
        }
      }
      FLUXES_i_vert <- FLUXES_i_vert[2:nrow(FLUXES_i_vert),]

      COEFFS_i_colnames <- names(COEFFS_i)
      COEFFS_i_vert <- data.frame(VAR_TYPE = "COEFF",
                                  VARIABLE = NaN,
                                  VALUE = NaN)
      COEFFS_i_vert_loc <- COEFFS_i_vert
      j <- i
      for (j in 1:nrow(COEFFS_i)){
        k <- 1
        for (k in 1:(length(COEFFS_i)-1)){
          COEFFS_i_vert_loc$VALUE = COEFFS_i[j,k+1]
          COEFFS_i_vert_loc$VARIABLE = paste("a", COEFFS_i[j, "BOXES_ID"], COEFFS_i_colnames[k+1], sep = "_")
          COEFFS_i_vert <- rbind(COEFFS_i_vert, COEFFS_i_vert_loc)
        }
      }
      COEFFS_i_vert <- COEFFS_i_vert[2:nrow(COEFFS_i_vert),]

      SIZE_INIT_i_vert <- SIZE_INIT_i
      DELTA_INIT_i_vert <- DELTA_INIT_i
      SIZE_INIT_i_vert$VAR_TYPE <- "SIZE_INIT"
      DELTA_INIT_i_vert$VAR_TYPE <- "DELTA_INIT"
      SIZE_INIT_i_vert$VARIABLE <- paste("m0", SIZE_INIT_i_vert$BOXES_ID, sep = "_")
      DELTA_INIT_i_vert$VARIABLE <- paste("d0", DELTA_INIT_i_vert$BOXES_ID, sep = "_")
      SIZE_INIT_i_vert$VALUE <- SIZE_INIT_i_vert$SIZE_INIT
      DELTA_INIT_i_vert$VALUE <- DELTA_INIT_i_vert$DELTA_INIT
      SIZE_INIT_i_vert <- SIZE_INIT_i_vert[,c("VAR_TYPE", "VARIABLE", "VALUE")]
      DELTA_INIT_i_vert <- DELTA_INIT_i_vert[,c("VAR_TYPE", "VARIABLE", "VALUE")]

      meta_RUN_i <- rbind(SIZE_INIT_i_vert, DELTA_INIT_i_vert)
      meta_RUN_i <- rbind(meta_RUN_i, FLUXES_i_vert)
      meta_RUN_i <- rbind(meta_RUN_i, COEFFS_i_vert)
      meta_RUN_i_short <- meta_RUN_i[,c("VARIABLE", "VALUE")]
      meta_RUN_i_horiz <- as.data.frame(t(meta_RUN_i_short$VALUE))
      names(meta_RUN_i_horiz) <- meta_RUN_i$VARIABLE

      evD_i$SERIES_RUN_ID <- SERIES_RUN_ID_i
      evS_i$SERIES_RUN_ID <- SERIES_RUN_ID_i
      evD_i$RUN_n <- RUN_n_i
      evS_i$RUN_n <- RUN_n_i
      evD_i$LEGEND_EXPLO_1 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "LEGEND_EXPLO_1"]
      evD_i$VAR_EXPLO_1 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "VAR_EXPLO_1"]
      evD_i$LEGEND_EXPLO_2 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "LEGEND_EXPLO_2"]
      evD_i$VAR_EXPLO_2 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "VAR_EXPLO_2"]
      evS_i$LEGEND_EXPLO_1 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "LEGEND_EXPLO_1"]
      evS_i$VAR_EXPLO_1 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "VAR_EXPLO_1"]
      evS_i$LEGEND_EXPLO_2 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "LEGEND_EXPLO_2"]
      evS_i$VAR_EXPLO_2 <- LOG_SERIES[LOG_SERIES$SERIES_RUN_ID == SERIES_RUN_ID_i, "VAR_EXPLO_2"]

      meta_RUN_i_evD_df <- as.data.frame(dplyr::slice(meta_RUN_i_horiz, rep(1:dplyr::n(), each = nrow(evD_i))))
      meta_RUN_i_evS_df <- as.data.frame(dplyr::slice(meta_RUN_i_horiz, rep(1:dplyr::n(), each = nrow(evS_i))))

      evD_i <- cbind(evD_i, meta_RUN_i_evD_df)
      evS_i <- cbind(evS_i, meta_RUN_i_evS_df)

      if (i == 2){
        evD <- evD_i
        evS <- evS_i
      } else {
        evD <- rbind(evD, evD_i[1:nrow(evD_i),])
        evS <- rbind(evS, evS_i[1:nrow(evS_i),])
      }
      utils::setTxtProgressBar(pb_prep, i)

      i <- i + 2
    }
    close(pb_prep)

    colnames_to_drop_check <- meta_RUN_i[meta_RUN_i$VAR_TYPE == "FLUX", "VARIABLE"]
    flux_cols_to_drop <- NULL
    i <- 1
    for (i in 1:length(colnames_to_drop_check)){
      if (sum(evD[, colnames_to_drop_check[i]]) == 0)
        flux_cols_to_drop <- c(flux_cols_to_drop, colnames_to_drop_check[i])
      i <- i + 1
    }

    colnames_to_drop_check <- meta_RUN_i[meta_RUN_i$VAR_TYPE == "COEFF", "VARIABLE"]
    alpha_cols_to_drop <- NULL
    i <- 1
    for (i in 1:length(colnames_to_drop_check)){
      if (sum(abs(evD[, colnames_to_drop_check[i]]-1)) == 0)
        alpha_cols_to_drop <- c(alpha_cols_to_drop, colnames_to_drop_check[i])
      i <- i + 1
    }

    evD <- evD[, -which(names(evD) %in% c(flux_cols_to_drop, alpha_cols_to_drop))]
    evS <- evS[, -which(names(evS) %in% c(flux_cols_to_drop, alpha_cols_to_drop))]

    # evD_final <- evD[evD$Time == t_lim_list[2],]
    # evS_final <- evS[evS$Time == t_lim_list[2],]

    #************************************** EDIT CSV for WHOLE COMPOSITE RUN evS - evD - LOG - OUT #----
    rlang::inform("________________________________________________________________________________")
    rlang::inform("\U0001f535 WRITING DIGEST")

    path_out_EXPLO <- paste("4_", as.character(SERIES_ID), "/", "0_DYN_DIGEST/", sep = "")
    if (!dir.exists(to_tmpdir(path_out_EXPLO))){dir.create(to_tmpdir(path_out_EXPLO))}
    path_out_EXPLO <- paste(path_out_EXPLO, SERIES_ID, sep = "")


    data.table::fwrite(LOG_SERIES, file = paste(to_tmpdir(path_out_EXPLO), "_LOG.csv", sep = ""), row.names = F, quote = F)

    saveRDS(object = evD, file = paste(to_tmpdir(path_out_EXPLO), "_evD.RDS", sep = ""))
    saveRDS(object = evS, file = paste(to_tmpdir(path_out_EXPLO), "_evS.RDS", sep = ""))
    # saveRDS(object = evD_final, file = paste(to_tmpdir(path_out_EXPLO), "_evD_final.RDS", sep = ""))
    # saveRDS(object = evS_final, file = paste(to_tmpdir(path_out_EXPLO), "_evS_final.RDS", sep = ""))

    if (isTRUE(to_DYN_DIGEST_CSVs)){
      data.table::fwrite(evD, file = paste(to_tmpdir(path_out_EXPLO), "_evD.csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(evS, file = paste(to_tmpdir(path_out_EXPLO), "_evS.csv", sep = ""), row.names = F, quote = F)
      # data.table::fwrite(evD_final, file = paste(to_tmpdir(path_out_EXPLO), "_evD_final.csv", sep = ""), row.names = F, quote = F)
      # data.table::fwrite(evS_final, file = paste(to_tmpdir(path_out_EXPLO), "_evS_final.csv", sep = ""), row.names = F, quote = F)
    }

    # explo_master_excel_path <-  paste(to_tmpdir(path_out_EXPLO), "_", EXPLO_MASTER, sep = "")
    explo_master_excel_path <-  paste(path_out_EXPLO, "_MASTER.xlsx", sep = "")
    writexl::write_xlsx(list(RUN_LIST = RUN_LIST,
                             FORCING_RAYLEIGH = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_RAYLEIGH")),
                             FORCING_SIZE = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_SIZE")),
                             FORCING_DELTA = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_DELTA")),
                             FORCING_ALPHA =  as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_ALPHA"))
    ),
    to_tmpdir(explo_master_excel_path))
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PLOT DEFAULT DELTAS FROM TEMPDIR #----

  if(isTRUE(plot_results)){
    # define plot function parameters
    SERIES_ID_plot <- LOG_SERIES[1,"SERIES_ID"]

    # define box lists
    BOXES <- unlist(stringr::str_split(LOG_SERIES[1,"BOXES_ID_list"], pattern = "_"))
    BOXES_INFINITE <- unlist(stringr::str_split(LOG_SERIES[1,"INFINITE_BOXES_list"], pattern = "_"))
    BOXES_DISCONNECTED <- unlist(stringr::str_split(LOG_SERIES[1,"DISCONNECTED_BOXES"], pattern = "_"))
    BOXES_FINITE <- BOXES[!BOXES %in% BOXES_INFINITE]
    BOXES_FINITE_CONNECTED <- BOXES_FINITE[!BOXES_FINITE %in% BOXES_DISCONNECTED]

    for (i in 1:length(BOXES_FINITE_CONNECTED)){
      loc_BOX <- BOXES_FINITE_CONNECTED[i]
      Z_ID <- paste("\u03B4", CONSTANTS$ELEMENT, " [", loc_BOX, "]", sep = "")
      evD$Z_ID <- Z_ID
      evD$Z <- evD[, loc_BOX]
      X_ID <- as.character(evD[1,"LEGEND_EXPLO_1"])
      evD$X_ID <- X_ID
      evD$X <- evD[,"VAR_EXPLO_1"]
      Y_ID <- as.character(evD[1,"LEGEND_EXPLO_2"])
      evD$Y_ID <- Y_ID
      evD$Y <- evD[,"VAR_EXPLO_2"]
      DF <- evD

      #### DEFINE TITLES
      EXPLO_subtitle_0 = paste(SERIES_ID_plot,
                               " (", min(LOG_SERIES$RUN_n),
                               "-", max(LOG_SERIES$RUN_n),
                               ") \n", "Hidden initial run: ",
                               paste(LOG_SERIES[1, "COEFF_FLUX"], collapse = " / "),
                               sep = "")

      if (levels(DF$LEGEND_EXPLO_1) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
        EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", levels(DF$VAR_EXPLO_1)[1], " to ", levels(DF$VAR_EXPLO_1)[length(levels(DF$VAR_EXPLO_1))], sep = ""), sep = "")
      } else {
        EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", paste(c(min(DF$VAR_EXPLO_1), max(DF$VAR_EXPLO_1)), collapse = " to "), collapse = ", "), sep = "")
      }

      if (levels(DF$LEGEND_EXPLO_2) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
        EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(levels(DF$VAR_EXPLO_2), collapse = ", "), sep = ""), sep = "")
      } else {
        EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(c(min(DF$VAR_EXPLO_2), max(DF$VAR_EXPLO_2)), collapse = " to "), collapse = ", "), sep = "")
      }

      EXPLO_subtitle <- paste(EXPLO_subtitle_0, "\n",
                              EXPLO_subtitle_1, "\n",
                              EXPLO_subtitle_2, sep = "")

      EXPLO_title <- paste("\u03B4", CONSTANTS$ELEMENT, " (", CONSTANTS$NUMERATOR, "/", CONSTANTS$DENOMINATOR,", \u2030", ") - [", loc_BOX, "]", sep = "")

      #### PREPARE DF
      DF <- DF[, c("Time", "X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")]
      initial_time_unit <- time_units[1]
      display_time_unit <- time_units[2]
      DF <- time_converter(dataframe <- DF,
                           time_colname <- "Time",
                           conv_timecolname <- "Time_plot",
                           former_unit <- initial_time_unit, # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"
                           new_unit <- display_time_unit)
      X.labs <- paste(DF[1,"X_ID"], ": ", as.character(sort(unique(DF[,"X"]))), sep = "")
      names(X.labs) <- as.character(sort(unique(DF[,"X"])))

      ### PLOT
      evD_plot <- ggplot2::ggplot(data = DF,  ggplot2::aes(x = Time_plot, y = Z, color = as.numeric(Y), group = as.numeric(Y)))+
        ggplot2::geom_line()+
        ggplot2::scale_color_gradientn(name = DF[1,"Y_ID"], colors = rainbow(100))+
        ggplot2::facet_grid(. ~ X, labeller = ggplot2::labeller(X = X.labs))+
        ggplot2::theme_bw()+
        ggplot2::theme(strip.text = ggplot2::element_text(size = 12, colour = "white", face = "bold"),
                       strip.background = ggplot2::element_rect(fill = "black"))+
        ggplot2::labs(x = paste("Time (", time_units[2], ")", sep = ""),
                      y = DF[1,"Z_ID"],
                      title = EXPLO_title,
                      subtitle = EXPLO_subtitle)

      # print(evD_plot)
      if (i == 1){
        plot_list <- list(evD_plot)
      } else {
        plot_list <- list(plot_list, evD_plot)
      }
    }
  }

  #----#----#----#----#----#----#----#----#----#---- save_run_outputs or not #----
  rlang::inform("________________________________________________________________________________")
  rlang::inform(message = paste("\U2139 Run outputs (stored in temporary directory):",
                                sep = ""))
  fs::dir_tree(path = to_tmpdir(""), recurse = T)
  rlang::inform("________________________________________________________________________________")
  if(isFALSE(save_run_outputs)){
    rlang::inform("\U2757 Results were not saved to working directory (set save_run_outputs = TRUE to save results).")
    rlang::inform("\U2139 You can explore the results with more parameters by using the shinobxr_app() function (requires saved outputs).")
  } else if(isTRUE(save_run_outputs)){
    R.utils::copyDirectory(to_tmpdir(""),
                           getwd(),
                           overwrite = T)
    rlang::inform("\U2705 Results were successfully saved to working directory.")
    rlang::inform("\U2139 You can explore the results with more parameters by using the shinobxr_app() function.")
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

  if(isTRUE(plot_results)){
    return(in_silence(plot_list))
  }
}
