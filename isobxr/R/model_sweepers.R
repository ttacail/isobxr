usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("metR", min_version = TRUE)
usethis::use_package("beepr", min_version = TRUE)
# usethis::use_package("mefa", min_version = TRUE)
usethis::use_package("iotools", min_version = TRUE)
usethis::use_package("data.table", min_version = TRUE)
usethis::use_package("svDialogs", min_version = TRUE)
usethis::use_package("plyr", min_version = TRUE)



#  #_________________________________________________________________________80char
#' Sweep the space of two parameters at the final state of a run
#' @description  A function to assess the influence of two parameters (varying
#' over a range of values) on the final state of a given model.
#' @param workdir Working directory in which the general master file (0_ISOBXR_MASTER.xlsx)
#' and steady sweep master file (e.g., 0_EXPLO_STEADY_MASTER.xlsx)
#' are found and where the output files will be stored.
#' \cr (string of characters)
#' @param SERIES_ID Name of the sweep series the run belongs to, that will determine
#' the folder in which the output files of this sweep run will be stored.
#' A sweep run number in YYY format will be automatically linked to it and no run will
#' overwrite a previous sweep run.
#' \cr (string of characters)
#' @param time_units Vector defining the initial time units (identical to time unit
#' used for fluxes) followed by the time unit used for the graphic output.
#' \cr (vector of two strings of characters, eg. c("days", "years"))
#' @param EXPLO_MASTER Name of the steady sweep master file (e.g., 0_EXPLO_STEADY_MASTER.xlsx),
#' defining the steady sweep run scenario.
#' \cr (string of characters)
#' @param EXPLO_AXIS_1 Set of values of sweeping parameter 1
#' @param EXPLO_AXIS_2 Set of values of sweeping parameter 2
#' @return If non existing, the fonction creates and stores all outputs
#' in a SERIES directory located in working directory, automatically numbered.
#' \cr Directory name structure: 4_STD + SERIES_ID + YYY, YYY being an automically set steady sweep run number between 001 and 999.
#' \cr No overwriting of previous steady sweep runs is possible.
#' \enumerate{
#' \item Calculates the number of runs the sweeping will require depending on the sweeped parameters.
#' The function then asks the user confirmation to run sweep_steady,
#' as the run calculation time depends on the number of successive sweeping runs.
#' \item Creates the set of inputs and outputs for each successive \emph{n} runs, numbered from to 1 to \emph{n} in an XXXX format (see \code{\link{run_isobxr}} documentation).
#' \cr Single run plots normally edited by \code{\link{run_isobxr}} are here not edited.
#' \cr Named with the following format: STD + SERIES_ID + YYY + XXXX
#' \item Archives the steady sweep master file.
#' \cr (file name structure: 0_STD + SERIES_ID + YYY + EXPLO_STEADY_MASTER.xlsx)
#' \item Archives the local LOG for the given steady sweep run.
#' \cr (file name structure: 0_STD + SERIES_ID + YYY + LOG.csv)
#' \item Stores the evolution of delta values with time in all boxes over the \emph{n} runs that constitute the steady sweep run.
#' \cr (file name structure: 0_STD + SERIES_ID + YYY + evD.csv)
#' \item Stores the evolution of box sizes (masses of element X) with time in all boxes over the \emph{n} runs that constitute the steady sweep run
#' \cr (file name structure: 0_STD + SERIES_ID + YYY + evS.csv)
#' \item Stores the final state of delta values with time in all boxes over the \emph{n} runs that constitute the steady sweep run.
#' \cr (file name structure: 0_STD + SERIES_ID + YYY + evD_final.csv)
#' \item Stores the final state of box sizes (masses of element X) with time in all boxes over the \emph{n} runs that constitute the steady sweep run
#' \cr (file name structure: 0_STD + SERIES_ID + YYY + evS_final.csv)
#' }
#' @export
sweep_steady <- function(workdir,
                         SERIES_ID,
                         time_units,
                         EXPLO_MASTER,
                         EXPLO_AXIS_1,
                         EXPLO_AXIS_2){

  # Clear plots
  if(!is.null(dev.list())) dev.off()
  # Clear console
  cat("\014")
  # Clean workspace
  rm(list=ls())

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIALIZE #----
  #************************************** SET WORKING DIRECTORY #----
  LOC_workdir <- workdir
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(LOC_workdir)

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
    LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
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
    EXPLO_SERIES_FAMILY <- SERIES_ID
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
             PLOT_DIAGRAMS = TRUE,
             PLOT_evD = TRUE)

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

  LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
  LOG_last <- LOG[nrow(LOG),]
  remove(LOG)

  if (LOG_last$NUM_ANA == "ANA"){
    path_to_OUT_last_final <- paste(LOG_last$path_outdir, "OUT/", LOG_last$SERIES_RUN_ID, "_A_1_OUT.csv", sep = "")
    OUT_last_final <- iotools::read.csv.raw(path_to_OUT_last_final)
    OUT_last_SIZE_FINAL <- OUT_last_final[, c("BOXES_ID", "SIZE_INIT")]
    OUT_last_DELTA_FINAL <- OUT_last_final[, c("BOXES_ID", "DELTA_FINAL")]
    names(OUT_last_DELTA_FINAL) <- c("BOXES_ID", "DELTA_INIT")
  } else {
    path_to_OUT_last_final <- paste(LOG_last$path_outdir, "OUT/", LOG_last$SERIES_RUN_ID, "_N_1_OUT.csv", sep = "")
    OUT_last_final <- iotools::read.csv.raw(path_to_OUT_last_final)
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
  STOP_GO <- svDialogs::dlgInput(message = cat("\n The EXPLO run will require ***", as.character(tot_run), "*** iterations. Do you wish to carry on ? \n"), default = "Yes", gui = .GUI)
  STOP_GO <- STOP_GO$res

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# SWEEP THE SPACE OF PARAMETERS #----
  if (STOP_GO != "Yes"){
    cat("\n *** You probably want to reduce the number of iterations in each EXPLO axis. *** \n")
  } else {
    cat("\n *** COMPUTE *** \n ")
    calculation_gauge(0, tot_run)
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
        run_isobxr(workdir = LOC_workdir,
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
                   PLOT_DIAGRAMS = FALSE,
                   PLOT_evD = FALSE)
        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# CLOCK #----
        calculation_gauge(clock, tot_run)
        clock <- clock + 1
        l <- l + 1
      }
      k <- k + 1
    }

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
    LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
    LOG_SERIES <- LOG[LOG$SERIES_ID == SERIES_ID,]
    remove(LOG)
    LOG_SERIES <- plyr::join(LOG_SERIES, EXPLOG, by = "RUN_n") ##### pck plyr (retired) or dplyr (active) ?
    LOG_SERIES <- clear_subset(LOG_SERIES)
    SERIES_RUN_ID_1 <- LOG_SERIES[1, "SERIES_RUN_ID"]
    path_to_input_1 <- paste(LOG_SERIES[1, "path_outdir"], "INPUT.xlsx", sep = "")
    BOXES_IDs <- as.data.frame(readxl::read_excel(path_to_input_1, "INITIAL"))[,c("BOXES_ID")]

    #************************************** READ/BUILD/MERGE evS/evD for ANA/NUM WHOLE COMPOSITE RUN #----
    cat("\n *** PREPARE RESULTS *** \n \n ")
    calculation_gauge(0, (tot_run+1))

    i <- 1
    for (i in 1:(tot_run+1)){
      SERIES_RUN_ID_i <- LOG_SERIES[i, "SERIES_RUN_ID"]
      RUN_n_i <- LOG_SERIES[i, "RUN_n"]
      path_outdir_i <- as.character(LOG_SERIES[i, "path_outdir"])
      path_to_INPUT_i <- paste(path_outdir_i, "INPUT.xlsx", sep = "")
      INIT_i <- as.data.frame(readxl::read_excel(path_to_INPUT_i, "INITIAL"))
      SIZE_INIT_i <- INIT_i[,c("BOXES_ID", "SIZE_INIT")]
      DELTA_INIT_i <- INIT_i[,c("BOXES_ID", "DELTA_INIT")]
      FLUXES_i <- as.data.frame(readxl::read_excel(path_to_INPUT_i, "FLUXES"))
      COEFFS_i <- as.data.frame(readxl::read_excel(path_to_INPUT_i, "COEFFS"))
      if (i > 1){
        unlink(path_to_INPUT_i) ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option ?
      }

      if (LOG_SERIES[i, "NUM_ANA"] == "ANA"){
        OUT_address <- paste(as.character(LOG_SERIES[i, "path_outdir"]), "OUT/", as.character(LOG_SERIES[i, "SERIES_RUN_ID"]), "_A_1_OUT.csv" , sep = "")
        ODE_SOLNs_address <- paste(as.character(LOG_SERIES[i, "path_outdir"]), "OUT/", as.character(LOG_SERIES[i, "SERIES_RUN_ID"]), "_A_2_ODE_SOLNs.csv" , sep = "")
        evD_address <- paste(as.character(LOG_SERIES[i, "path_outdir"]), "OUT/", as.character(LOG_SERIES[i, "SERIES_RUN_ID"]), "_A_3_evD.csv" , sep = "")
        evD_i <- data.table::fread(evD_address, data.table = F, stringsAsFactors = T)
        SIZE_INIT_i_hor <- as.data.frame(t(SIZE_INIT_i$SIZE_INIT))
        names(SIZE_INIT_i_hor) <- SIZE_INIT_i$BOXES_ID
        SIZE_INIT_i_hor$Time = NaN
        evS_i <- evD_i
        j <- 1
        for (j in 1:length(BOXES_IDs)){
          evS_i[,BOXES_IDs[j]] <- SIZE_INIT_i_hor[1, BOXES_IDs[j]]
          j <- j + 1
        }
        if (i > 1){ ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option ?
          unlink(c(evD_address, OUT_address, ODE_SOLNs_address, paste(path_outdir_i, "OUT/", sep = "")), recursive = T)
        }
      } else {
        if (LOG_SERIES[i, "NUM_ANA"] == "NUM"){
          OUT_address <- paste(path_outdir_i, "OUT/", SERIES_RUN_ID_i, "_N_1_OUT.csv" , sep = "")
          evD_address <- paste(path_outdir_i, "OUT/", SERIES_RUN_ID_i, "_N_3_evD.csv" , sep = "")
          evD_i <- data.table::fread(evD_address, data.table = F, stringsAsFactors = T, sep = ",")
          evS_address <- paste(path_outdir_i, "OUT/", SERIES_RUN_ID_i, "_N_2_evS.csv" , sep = "")
          evS_i <- data.table::fread(evS_address, data.table = F, stringsAsFactors = T, sep = ",")
          if (i > 1){ ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option ?
            unlink(c(evD_address, evS_address, OUT_address, paste(path_outdir_i, "OUT/", sep = "")), recursive = T)
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

      evD_i <- cbind(evD_i, rep(meta_RUN_i_horiz, each = nrow(evD_i)))
      evS_i <- cbind(evS_i, rep(meta_RUN_i_horiz, each = nrow(evS_i)))

      if (i == 1){
        evD <- evD_i
        evS <- evS_i
      } else {
        evD <- rbind(evD, evD_i[1:nrow(evD_i),])
        evS <- rbind(evS, evS_i[1:nrow(evS_i),])
      }
      calculation_gauge(i, (tot_run+1))
      i <- i + 1
    }

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
    cat("\n *** WRITE OUTPUTS *** \n \n ")

    path_out_EXPLO <- paste("4_", as.character(SERIES_ID), "/", "0_", SERIES_ID, sep = "")
    data.table::fwrite(LOG_SERIES, file = paste(path_out_EXPLO, "_LOG.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evD, file = paste(path_out_EXPLO, "_evD.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evS, file = paste(path_out_EXPLO, "_evS.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evD_final, file = paste(path_out_EXPLO, "_evD_final.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evS_final, file = paste(path_out_EXPLO, "_evS_final.csv", sep = ""), row.names = F, quote = F)
    explo_master_excel_path <-  paste(path_out_EXPLO, EXPLO_MASTER, sep = "")

    writexl::write_xlsx(list(RUN_LIST = RUN_LIST,
                             FORCING_RAYLEIGH = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_RAYLEIGH")),
                             FORCING_SIZE = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_SIZE")),
                             FORCING_DELTA = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_DELTA")),
                             FORCING_ALPHA =  as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_ALPHA"))
    ),
    explo_master_excel_path)

  }
  beepr::beep(sound = 10)
}


#  #_________________________________________________________________________80char
#' Sweep the space of two parameters during a dynamic run
#' @description  A function to assess the influence of two parameters (varying
#' over a range of values) on dynamic evolution of given model.
#' @param workdir Working directory in which the general master file (0_ISOBXR_MASTER.xlsx)
#' and dynamic sweep master file (e.g., 0_EXPLO_DYN_MASTER.xlsx)
#' are found and where the output files will be stored.
#' \cr (string of characters)
#' @param SERIES_ID Name of the sweep series the run belongs to, that will determine
#' the folder in which the output files of this sweep run will be stored.
#' A sweep run number in YYY format will be automatically linked to it and no run will
#' overwrite a previous sweep run.
#' \cr (string of characters)
#' @param time_units Vector defining the initial time units (identical to time unit
#' used for fluxes) followed by the time unit used for the graphic output.
#' \cr (vector of two strings of characters, eg. c("days", "years"))
#' @param EXPLO_MASTER Name of the dynamic sweep master file (e.g., 0_EXPLO_DYN_MASTER.xlsx),
#' defining the dynamic sweep run scenario.
#' \cr (string of characters)
#' @param EXPLO_AXIS_1 Set of values of sweeping parameter 1
#' @param EXPLO_AXIS_2 Set of values of sweeping parameter 2
#' @return If non existing, the fonction creates and stores all outputs
#' in a SERIES directory located in working directory, automatically numbered.
#' \cr Directory name structure: 4_DYN + SERIES_ID + YYY, YYY being an automically set dynamic sweep run number between 001 and 999.
#' \cr No overwriting of previous dynamic sweep runs is possible.
#' \enumerate{
#' \item Calculates the number of runs the sweeping will require depending on the sweeped parameters.
#' The function then asks the user confirmation to run sweep_dyn,
#' as the run calculation time depends on the number of successive sweeping runs.
#' \item Creates the set of inputs and outputs for each successive \emph{n} runs, numbered from to 1 to \emph{n} in an XXXX format (see \code{\link{run_isobxr}} documentation).
#' \cr Single run plots normally edited by \code{\link{run_isobxr}} are here not edited.
#' \cr Named with the following format: DYN + SERIES_ID + YYY + XXXX
#' \item Archives the dynamic sweep master file.
#' \cr (file name structure: 0_DYN + SERIES_ID + YYY + 0_EXPLO_DYN_MASTER.xlsx)
#' \item Archives the local LOG for the given dynamic sweep run.
#' \cr (file name structure: 0_DYN + SERIES_ID + YYY + LOG.csv)
#' \item Stores the evolution of delta values with time in all boxes over the \emph{n} runs that constitute the dynamic sweep run.
#' \cr (file name structure: 0_DYN + SERIES_ID + YYY + evD.csv)
#' \item Stores the evolution of box sizes (masses of element X) with time in all boxes over the \emph{n} runs that constitute the dynamic sweep run
#' \cr (file name structure: 0_DYN + SERIES_ID + YYY + evS.csv)
#' \item Stores the final state of delta values with time in all boxes over the \emph{n} runs that constitute the dynamic sweep run.
#' \cr (file name structure: 0_DYN + SERIES_ID + YYY + evD_final.csv)
#' \item Stores the final state of box sizes (masses of element X) with time in all boxes over the \emph{n} runs that constitute the dynamic sweep run
#' \cr (file name structure: 0_DYN + SERIES_ID + YYY + evS_final.csv)
#' }
#' @export
sweep_dyn <- function(workdir,
                      SERIES_ID,
                      time_units,
                      EXPLO_MASTER,
                      EXPLO_AXIS_1,
                      EXPLO_AXIS_2){

  # Clear plots
  if(!is.null(dev.list())) dev.off()
  # Clear console
  cat("\014")
  # Clean workspace
  rm(list=ls())

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIALIZE
  #************************************** SET WORKING DIRECTORY  #----
  LOC_workdir <- workdir
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(LOC_workdir)

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PREPARE ISOPYBOX ARGUMENTS FROM EXPLO_MASTER  #----
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
    LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
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
    EXPLO_SERIES_FAMILY <- SERIES_ID
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
  STOP_GO <- svDialogs::dlgInput(message = cat("\n The EXPLO run will require ***", as.character(tot_run), "*** iterations. Do you wish to carry on ? \n"), default = "Yes", gui = .GUI)
  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# SWEEP THE SPACE OF PARAMETERS #----
  if (STOP_GO$res != "Yes"){
    cat("\n *** You probably want to reduce the number of iterations in each EXPLO axis. *** \n")
  } else {
    cat("\n *** COMPUTING *** \n ")
    calculation_gauge(0, tot_run)
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
                   HIDE_PRINTS = TRUE,
                   PLOT_DIAGRAMS = FALSE,
                   PLOT_evD = FALSE
        )

        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# RUN 2/2, i in [1:n] #----
        i <- 2
        #************************************** PREPARE INPUTS for ISOPY_RUN with EXPLO_MASTER as default and Taking final state of run 2/2 as initial #----
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
        LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
        LOG_last <- LOG[nrow(LOG),]
        remove(LOG)

        if (LOG_last$NUM_ANA == "ANA"){
          path_to_OUT_last_final <- paste(LOG_last$path_outdir, "OUT/", LOG_last$SERIES_RUN_ID, "_A_1_OUT.csv", sep = "")
          OUT_last_final <- iotools::read.csv.raw(path_to_OUT_last_final)
          OUT_last_SIZE_FINAL <- OUT_last_final[, c("BOXES_ID", "SIZE_INIT")]
          OUT_last_DELTA_FINAL <- OUT_last_final[, c("BOXES_ID", "DELTA_FINAL")]
          names(OUT_last_DELTA_FINAL) <- c("BOXES_ID", "DELTA_INIT")
        } else {
          path_to_OUT_last_final <- paste(LOG_last$path_outdir, "OUT/", LOG_last$SERIES_RUN_ID, "_N_1_OUT.csv", sep = "")
          OUT_last_final <- iotools::read.csv.raw(path_to_OUT_last_final)
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
        run_isobxr(workdir = LOC_workdir,
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
                   PLOT_DIAGRAMS = FALSE,
                   PLOT_evD = FALSE)
        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# CLOCK #----
        calculation_gauge(clock, tot_run)
        clock <- clock + 1
        l <- l + 1
      }
      k <- k + 1
    }

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
    LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
    LOG_SERIES <- LOG[LOG$SERIES_ID == SERIES_ID,]
    remove(LOG)
    LOG_SERIES <- plyr::join(LOG_SERIES, EXPLOG, by = "RUN_n") # plyr (retired) or dplyr (active) ?
    LOG_SERIES <- clear_subset(LOG_SERIES)
    path_to_input_1 <- paste(LOG_SERIES[1, "path_outdir"], "INPUT.xlsx", sep = "")
    BOXES_IDs <- as.data.frame(readxl::read_excel(path_to_input_1, "INITIAL"))[,c("BOXES_ID")]

    #************************************** READ/BUILD/MERGE evS/D and evS/D final for ANA/NUM WHOLE COMPOSITE RUN #----
    cat("\n *** PREPARE RESULTS *** \n \n ")
    calculation_gauge(0, (tot_run))

    i <- 2
    for (k in 1:(tot_run)){
      SERIES_RUN_ID_i <- LOG_SERIES[i, "SERIES_RUN_ID"]
      RUN_n_i <- LOG_SERIES[i, "RUN_n"]
      path_outdir_i <- as.character(LOG_SERIES[i, "path_outdir"])
      path_to_INPUT_i <- paste(path_outdir_i, "INPUT.xlsx", sep = "")
      INIT_i <- as.data.frame(readxl::read_excel(path_to_INPUT_i, "INITIAL"))
      SIZE_INIT_i <- INIT_i[,c("BOXES_ID", "SIZE_INIT")]
      DELTA_INIT_i <- INIT_i[,c("BOXES_ID", "DELTA_INIT")]
      FLUXES_i <- as.data.frame(readxl::read_excel(path_to_INPUT_i, "FLUXES"))
      COEFFS_i <- as.data.frame(readxl::read_excel(path_to_INPUT_i, "COEFFS"))
      # if (i > 1){
      #   unlink(path_to_INPUT_i) ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option ?
      # }

      if (LOG_SERIES[i, "NUM_ANA"] == "ANA"){
        OUT_address <- paste(path_outdir_i, "OUT/", as.character(SERIES_RUN_ID_i), "_A_1_OUT.csv" , sep = "")
        ODE_SOLNs_address <- paste(path_outdir_i, "OUT/", as.character(SERIES_RUN_ID_i), "_A_2_ODE_SOLNs.csv" , sep = "")
        evD_address <- paste(path_outdir_i, "OUT/", as.character(SERIES_RUN_ID_i), "_A_3_evD.csv" , sep = "")
        evD_i <- data.table::fread(evD_address, data.table = F, stringsAsFactors = T)
        SIZE_INIT_i_hor <- as.data.frame(t(SIZE_INIT_i$SIZE_INIT))
        names(SIZE_INIT_i_hor) <- SIZE_INIT_i$BOXES_ID
        SIZE_INIT_i_hor$Time = NaN
        evS_i <- evD_i
        j <- 1
        for (j in 1:length(BOXES_IDs)){
          evS_i[,BOXES_IDs[j]] <- SIZE_INIT_i_hor[1, BOXES_IDs[j]]
          j <- j + 1
        }
        # if (i > 1){ ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option ?
        #   unlink(c(evD_address, OUT_address, ODE_SOLNs_address, paste(path_outdir_i, "OUT/", sep = "")), recursive = T)
        # }
      } else {
        if (LOG_SERIES[i, "NUM_ANA"] == "NUM"){
          OUT_address <- paste(path_outdir_i, "OUT/", SERIES_RUN_ID_i, "_N_1_OUT.csv" , sep = "")
          evD_address <- paste(path_outdir_i, "OUT/", SERIES_RUN_ID_i, "_N_3_evD.csv" , sep = "")
          evD_i <- data.table::fread(evD_address, data.table = F, stringsAsFactors = T, sep = ",")
          evS_address <- paste(path_outdir_i, "OUT/", SERIES_RUN_ID_i, "_N_2_evS.csv" , sep = "")
          evS_i <- data.table::fread(evS_address, data.table = F, stringsAsFactors = T, sep = ",")
          # if (i > 1){ ## delete all run_isobxr outputs after building summary except RUN #1 ### make it an option ?
          #   unlink(c(evD_address, evS_address, OUT_address, paste(path_outdir_i, "OUT/", sep = "")), recursive = T)
          # }
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

      evD_i <- cbind(evD_i, rep(meta_RUN_i_horiz, each = nrow(evD_i)))
      evS_i <- cbind(evS_i, rep(meta_RUN_i_horiz, each = nrow(evS_i)))

      if (i == 2){
        evD <- evD_i
        evS <- evS_i
      } else {
        evD <- rbind(evD, evD_i[1:nrow(evD_i),])
        evS <- rbind(evS, evS_i[1:nrow(evS_i),])
      }
      calculation_gauge(i, (2*tot_run))
      i <- i + 2
    }

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
    cat("\n *** WRITE OUTPUTS *** \n \n ")

    path_out_EXPLO <- paste("4_", as.character(SERIES_ID), "/", "0_", SERIES_ID, sep = "")
    data.table::fwrite(LOG_SERIES, file = paste(path_out_EXPLO, "_LOG.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evD, file = paste(path_out_EXPLO, "_evD.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evS, file = paste(path_out_EXPLO, "_evS.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evD_final, file = paste(path_out_EXPLO, "_evD_final.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evS_final, file = paste(path_out_EXPLO, "_evS_final.csv", sep = ""), row.names = F, quote = F)
    explo_master_excel_path <-  paste(path_out_EXPLO, EXPLO_MASTER, sep = "")

    writexl::write_xlsx(list(RUN_LIST = RUN_LIST,
                             FORCING_RAYLEIGH = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_RAYLEIGH")),
                             FORCING_SIZE = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_SIZE")),
                             FORCING_DELTA = as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_DELTA")),
                             FORCING_ALPHA =  as.data.frame(readxl::read_excel(EXPLO_MASTER, "FORCING_ALPHA"))
    ),
    explo_master_excel_path)
  }
  beepr::beep(sound = 10)
}
