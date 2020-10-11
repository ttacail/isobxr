#' @importFrom grDevices dev.list dev.new dev.off graphics.off pdf rainbow
NULL

#' @importFrom utils head read.csv
NULL

usethis::use_package("qgraph", min_version = TRUE)
usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("writexl", min_version = TRUE)
usethis::use_package("openxlsx", min_version = TRUE)
usethis::use_package("stringr", min_version = TRUE)
usethis::use_package("plyr", min_version = TRUE)
usethis::use_package("dplyr", min_version = TRUE)
usethis::use_package("data.table", min_version = TRUE)
usethis::use_package("writexl", min_version = TRUE)
usethis::use_package("ggplot2", min_version = TRUE)
usethis::use_package("ggrepel", min_version = TRUE)

#  #_________________________________________________________________________80char
#' Run isobxr stable isotope box model
#' @description  A function to run the isobxr stable isotope box model,
#' assessing the design of the model and automatically running \code{\link{num_slvr}}
#' or \code{\link{ana_slvr}} depending on the conditions.
#' @param workdir Working directory in which the master file (0_ISOBXR_MASTER.xlsx)
#' is found and where the output files will be stored.
#' \cr (string of characters)
#' @param SERIES_ID Name of the model series the run belongs to, that will determine
#' the folder in which the output files will be stored.
#' \cr (string of characters)
#' @param flux_list_name Name of the list of fluxes to be used for the run as
#' defined in a single column of the FLUXES sheet of the 0_ISOBXR_MASTER.xlsx file.
#' \cr (string of characters)
#' @param coeff_list_name Name of the list of fractionation coefficients to be used
#' for the run as defined in a single column of the COEFFS sheet of the
#' 0_ISOBXR_MASTER.xlsx file.
#' \cr (string of characters)
#' @param t_lim Run duration, given in the same time units as the fluxes.
#' \cr (integer)
#' @param nb_steps Number of steps that will define the resolution of the run.
#' \cr (integer)
#' @param time_units Vector defining the initial time units (identical to time unit
#' used for fluxes) followed by the time unit used for the graphic output.
#' \cr (vector of two strings of characters, eg. c("days", "years"))
#' @param FORCING_RAYLEIGH \emph{OPTIONAL} Dataframe describing the forcing on a Rayleigh
#' distillation driven fractionation coefficient as a function of flux intensities
#' and a fundamental fractionation coefficient (Dataframe formating details in
#' read me).
#' \cr Default is NULL.
#' @param FORCING_SIZE \emph{OPTIONAL} Dataframe describing the forcing on one or several box
#' sizes (mass of element X). The newly defined sizes for the given set of boxes
#' shown in dataframe will overwrite the sizes as defined in 0_ISOBXR_MASTER.xlsx file.
#' \cr Default is NULL.
#' @param FORCING_DELTA \emph{OPTIONAL} Dataframe describing the forcing on one or several boxes
#' initial isotope composition described as delta (Dataframe formating details in read me).
#' The newly defined delta values for the given set of boxes shown in dataframe will
#' overwrite the delta values as defined in ISOBXR_MASTER file.
#' \cr Default is NULL.
#' @param FORCING_ALPHA \emph{OPTIONAL} Dataframe describing the forcing on one or several
#' fractionation coefficients linked to fluxes from one reservoir to another
#' (Dataframe formating details in read me). The newly defined alpha values
#' for the given set of boxes shown in dataframe will overwrite the alpha values
#' as defined in ISOBXR_MASTER file.
#' \cr Default is NULL.
#' @param COMPOSITE \emph{NOT TO BE USED IN SINGLE RUN} Logical value automatically defined in \code{\link{compose_isobxr}}.
#' \cr Default is FALSE.
#' @param COMPO_SERIES_n \emph{NOT TO BE USED IN SINGLE RUN} Iteration of the composite run for the given series it belongs to,
#' automatically defined in \code{\link{compose_isobxr}}.
#' \cr Default is NaN.
#' @param COMPO_SERIES_FAMILY \emph{NOT TO BE USED IN SINGLE RUN} Composite run series family, automatically defined in
#' \code{\link{compose_isobxr}}.
#' \cr Default is NaN.
#' @param EXPLORER \emph{NOT TO BE USED IN SINGLE RUN} Logical value automatically defined in \code{\link{sweep_steady}}
#' or \code{\link{sweep_dyn}}.
#' \cr Default is FALSE.
#' @param EXPLO_SERIES_n \emph{NOT TO BE USED IN SINGLE RUN} Iteration of the sweep run for the given series it belongs to,
#' automatically defined in \code{\link{sweep_steady}} or \code{\link{sweep_dyn}}.
#' \cr Default is NaN.
#' @param EXPLO_SERIES_FAMILY \emph{NOT TO BE USED IN SINGLE RUN} Sweep run series family, automatically defined in
#' \code{\link{sweep_steady}} or \code{\link{sweep_dyn}}.
#' \cr Default is NaN.
#' @param HIDE_PRINTS \emph{OPTIONAL} Logical value determining whether to run outputs details in R console.
#' This parameter will not hide the warnings regarding the automatic update of
#' the run duration in case of the emptying of a box.
#' \cr Default is FALSE.
#' @param PLOT_DIAGRAMS \emph{OPTIONAL} Logical value to edit pdf of box model diagram or not.
#' \cr Default is TRUE.
#' @param PLOT_evD \emph{OPTIONAL} Logical value to edit pdf of delta time evolution plot or not.
#' \cr Default is TRUE.
#'
#' @return If function is run independently and if the directory is non existing,
#' the fonction creates and stores all outputs
#' in a SERIES directory located in working directory.
#' \cr Directory name structure: 2_RUN + SERIES_ID
#'
#' \enumerate{
#' \item Automatically sets a XXXX run number between 0001 and 9999. The outputs do not overwrite possible identical previously performed runs.
#'
#' \item Creates an INPUT file with all run conditions, located in SERIES directory.
#' \cr (file name structure: SERIES_ID + XXXX + _INPUT.xlsx)
#'
#' \item Edits a Box model diagram of flux (DIAGf pdf) of element X (mass per time unit)
#' between all boxes. (Optional, depends on parameter PLOT_DIAGRAM), located in SERIES directory.
#' \cr (file name structure: SERIES_ID + XXXX + DIAGf + _flux_list_name.pdf)
#'
#' \item Edits a Box model diagram of isotope fractionation coefficients (alphas, DIAGa pdf)
#' between all boxes (optional, depends on parameter PLOT_DIAGRAM), located in SERIES directory.
#' \cr (file name structure: SERIES_ID + XXXX + DIAGf + _flux_list_name.pdf)
#'
#' \item Creates a subdirectory for numerical outputs.
#' \cr (directory name structure: SERIES_ID + XXXX + _OUT)
#'
#' \enumerate{
#' \item Stores \code{\link{num_slvr}} or \code{\link{ana_slvr}} outputs in subdirectory.
#'
#' \item Edits a pdf plot of the time dependent evolution of delta values, with a logarithmic x axis time scale (optional).
#' \cr (file name structure: SERIES_ID + XXXX + _plot_evD.pdf)
#' }
#'
#' \item Creates or updates the general log file, located in general working directory.
#' \cr (file name: 1_LOG.csv)
#' }
#' @seealso Documentation on \code{\link{num_slvr}} or \code{\link{ana_slvr}} functions.
#' @export
run_isobxr <- function(workdir, SERIES_ID, flux_list_name, coeff_list_name, t_lim, nb_steps, time_units, FORCING_RAYLEIGH = NULL, FORCING_SIZE = NULL, FORCING_DELTA = NULL, FORCING_ALPHA = NULL, COMPOSITE = FALSE, COMPO_SERIES_n = NaN, COMPO_SERIES_FAMILY = NaN, EXPLORER = FALSE, EXPLO_SERIES_n = NaN, EXPLO_SERIES_FAMILY = NaN, HIDE_PRINTS = FALSE, PLOT_DIAGRAMS = TRUE, PLOT_evD = FALSE){
  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIALIZE
  #************************************** SET WORKING DIRECTORY and DEFINE ISOPY_MASTER file #----
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(workdir)
  ISOPY_MASTER_file <- "0_ISOBXR_MASTER.xlsx"

  Time <- VAR <- VAR_TYPE <- NULL

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# READ and PREPARE INPUTS
  #************************************** READ MODEL MASTER FILE #----
  #### CREATE CONSTS DF
  CONSTANTS <- as.data.frame(readxl::read_excel(ISOPY_MASTER_file, "CONSTANTS"))
  CONSTS <- data.frame(CONSTS_ID = c("Element", "Numerator","Denominator", "Ratio_Standard", "time", "n_steps"),
                       CONSTS = c(CONSTANTS$ELEMENT, CONSTANTS$NUMERATOR, CONSTANTS$DENOMINATOR, CONSTANTS$RATIO_STANDARD, t_lim, nb_steps))
  CONSTS$CONSTS_ID <- as.character(CONSTS$CONSTS_ID)
  CONSTS$CONSTS <- as.character(CONSTS$CONSTS)

  #### IMPORT BOXES master, FLUXES master and COEFF master FROM MASTER EXCEL FILE
  BOXES_master <- as.data.frame(readxl::read_excel(ISOPY_MASTER_file, "BOXES"))
  FLUXES_master <- as.data.frame(readxl::read_excel(ISOPY_MASTER_file, "FLUXES"))
  COEFFS_master <- as.data.frame(readxl::read_excel(ISOPY_MASTER_file, "COEFFS"))

  #### TURN TO FACTOR FACTOR COLUMNS / non numeric
  list_factor_COLS <- c("BOXES_ID", "INFINITE", "FROM", "TO", "ID", "STATUS")

  list_factor_COLS_loc <- colnames(BOXES_master[, colnames(BOXES_master) %in% list_factor_COLS])
  i <- 1
  for (i in 1:length(list_factor_COLS_loc)){
    BOXES_master[,list_factor_COLS_loc[i]] <- as.factor(BOXES_master[,list_factor_COLS_loc[i]])
    i <- i + 1
  }

  list_factor_COLS_loc <- colnames(FLUXES_master[, colnames(FLUXES_master) %in% list_factor_COLS])
  i <- 1
  for (i in 1:length(list_factor_COLS_loc)){
    FLUXES_master[,list_factor_COLS_loc[i]] <- as.factor(FLUXES_master[,list_factor_COLS_loc[i]])
    i <- i + 1
  }

  list_factor_COLS_loc <- colnames(COEFFS_master[, colnames(COEFFS_master) %in% list_factor_COLS])
  i <- 1
  for (i in 1:length(list_factor_COLS_loc)){
    COEFFS_master[,list_factor_COLS_loc[i]] <- as.factor(COEFFS_master[,list_factor_COLS_loc[i]])
    i <- i + 1
  }

  #### Extract list of infinite boxes
  if (length(BOXES_master[BOXES_master$INFINITE == "INFINITE", "BOXES_ID"]) != 0){
    INFINITE_BOXES <- as.character(BOXES_master[BOXES_master$INFINITE == "INFINITE", "BOXES_ID"])
  } else {
    INFINITE_BOXES <- NaN
  }

  #************************************** EXTRACT AND PREPARE LOCAL RUN INPUTS #----
  #### EXTRACT LIST OF COEFF and FLUXES LIST names
  list_COEFFS_master <- colnames(COEFFS_master[, -which(colnames(COEFFS_master) %in% c("FROM", "TO"))])
  list_FLUXES_master <- colnames(FLUXES_master[, -which(colnames(FLUXES_master) %in% c("FROM", "TO", "ID", "STATUT"))])
  list_BOXES_master <- as.character(BOXES_master$BOXES_ID)
  n_BOXES <- length(list_BOXES_master)

  #### EXTRACT LOCAL FLUX / ALPHA LISTS
  flux_list_loc <- FLUXES_master[FLUXES_master$STATUS == "FLUX", c("FROM", "TO", "STATUS", flux_list_name)]
  coeff_list_loc <- COEFFS_master[, c("FROM", "TO", coeff_list_name)]

  #### SET INITIAL - SIZES and DELTAs
  SIZE_INITIAL <- FLUXES_master[FLUXES_master$STATUS == "SIZE", c("ID", flux_list_name)]
  names(SIZE_INITIAL)[names(SIZE_INITIAL) == 'ID'] <- 'BOXES_ID'
  names(SIZE_INITIAL)[names(SIZE_INITIAL) == flux_list_name] <- 'SIZE_INIT'
  SIZE_INITIAL <- clear_subset(SIZE_INITIAL)
  INITIAL <- data.frame(BOXES_ID = list_BOXES_master)
  INITIAL <- merge(INITIAL, SIZE_INITIAL, by = "BOXES_ID", all = T, sort = F)
  INITIAL[is.na(INITIAL)] = 0

  if (is.null(FORCING_SIZE) == FALSE){     # SET SIZE_INIT
    i <- 1
    for (i in 1:nrow(FORCING_SIZE)){
      INITIAL[INITIAL$BOXES_ID == as.character(FORCING_SIZE[i,"BOXES_ID"]), "SIZE_INIT"] <- FORCING_SIZE[i,"SIZE_INIT"]
      i <- i + 1
    }
  }

  if (is.null(FORCING_DELTA) == FALSE){    # SET DELTA_INIT
    INITIAL <- plyr::join(INITIAL, FORCING_DELTA, by = "BOXES_ID")
    INITIAL[is.na(INITIAL)] = 0
  } else {
    INITIAL$DELTA_INIT <- 0
  }

  #### FORMAT FLUXES AND COEFFS LISTS as MATRICES
  FLUXES_loc <- as.data.frame(matrix(data = 0, nrow = n_BOXES, ncol = n_BOXES+1))
  colnames(FLUXES_loc) <- c("BOXES_ID", list_BOXES_master)
  FLUXES_loc$BOXES_ID <- as.factor(list_BOXES_master)

  COEFFS_loc <- as.data.frame(matrix(data = 1, nrow = n_BOXES, ncol = n_BOXES+1))
  colnames(COEFFS_loc) <- c("BOXES_ID", list_BOXES_master)
  COEFFS_loc$BOXES_ID <- as.factor(list_BOXES_master)

  i <- 1
  for (i in 1:nrow(flux_list_loc)){
    FROM_loc <- as.character(flux_list_loc[i, "FROM"])
    TO_loc <- as.character(flux_list_loc[i, "TO"])
    FLUXES_loc[FLUXES_loc$BOXES_ID == FROM_loc, TO_loc] <- flux_list_loc[i, flux_list_name]
    i <-  i + 1
  }

  i <- 1
  for (i in 1:nrow(coeff_list_loc)){
    FROM_loc <- as.character(coeff_list_loc[i, "FROM"])
    TO_loc <- as.character(coeff_list_loc[i, "TO"])
    COEFFS_loc[COEFFS_loc$BOXES_ID == FROM_loc, TO_loc] <- coeff_list_loc[i, coeff_list_name]
    i <-  i + 1
  }

  #### FORCING ALPHA
  if (is.null(FORCING_ALPHA) == FALSE){
    i <- 1
    for (i in 1:nrow(FORCING_ALPHA)){
      COEFFS_loc[COEFFS_loc$BOXES_ID == as.character(FORCING_ALPHA[i,"FROM"]), as.character(FORCING_ALPHA[i,"TO"])] <- FORCING_ALPHA[i, "ALPHA"] # 1-log(ALPHA_loc)
      i <- i + 1
    }
  }

  #### FORCING RAYLEIGH DISTILLATION RESULTING FRACTIONATION, CALCULATOR
  if (is.null(FORCING_RAYLEIGH) == FALSE){
    i <- 1
    for (i in 1:nrow(FORCING_RAYLEIGH)){
      FRAC_EXCR <- flux_list_loc[flux_list_loc$FROM == as.character(FORCING_RAYLEIGH[i,"XFROM"]) & flux_list_loc$TO == as.character(FORCING_RAYLEIGH[i,"XTO"]) , flux_list_name] /
        flux_list_loc[flux_list_loc$FROM == as.character(FORCING_RAYLEIGH[i,"YFROM"]) & flux_list_loc$TO == as.character(FORCING_RAYLEIGH[i,"YTO"]) , flux_list_name] # FRACTIONAL EXCRETION RATE
      ALPHA_loc <- exp((1000*(FORCING_RAYLEIGH[i, "ALPHA_0"]-1)*log(FRAC_EXCR))/1000)
      COEFFS_loc[COEFFS_loc$BOXES_ID == as.character(FORCING_RAYLEIGH[i,"AFROM"]), as.character(FORCING_RAYLEIGH[i,"ATO"])] <- ALPHA_loc # 1-log(ALPHA_loc)
    }
  }

  #### STORE CONSTS - FLUXES - COEFFS - INITIAL FOR INPUT EXCEL FILE EDITION
  row.names(FLUXES_loc) <- as.character(0:(nrow(FLUXES_loc)-1))
  row.names(COEFFS_loc) <- as.character(0:(nrow(COEFFS_loc)-1))

  CONSTS_trad <- CONSTS
  INITIAL_trad <- INITIAL
  FLUXES_trad <- FLUXES_loc
  COEFFS_trad <- COEFFS_loc

  #### STORE COEFFS - FLUXES FOR PLOT
  COEFFS <- COEFFS_loc
  FLUXES <- FLUXES_loc

  row.names(INITIAL) <- INITIAL$BOXES_ID
  row.names(FLUXES) <- FLUXES$BOXES_ID
  row.names(COEFFS) <- COEFFS$BOXES_ID

  #************************************** CHECKING FLUX BALANCE in each FINITE BOX / ROUTE TO NUM vs. ANA #----
  #### REMIND USER WHAT ARE THE INFINITE BOXES

  if (HIDE_PRINTS == F){
    if (is.na(INFINITE_BOXES[1]) == F){
      cat( paste("\n < The INFINITE boxes are ", paste(INFINITE_BOXES, collapse = ", "), " >", sep = ""))
    } else {
      cat( paste("\n < All boxes are FINITE >", sep = ""))
    }
  }

  #### CACLULATE FLUX BALANCE and RESIDENCE TIME for EACH BOX
  BOXES_ID <- as.character(FLUXES$BOXES_ID)

  INITIAL$FLUX_IN <- NaN
  INITIAL$FLUX_OUT <- NaN
  INITIAL$FLUX_BALANCE <- NaN
  INITIAL$RES_TIME <- NaN

  i <- 1
  for (i in 1:nrow(FLUXES)){
    INITIAL[BOXES_ID[i], "FLUX_IN"] <- sum(FLUXES[,BOXES_ID[i]])
    INITIAL[BOXES_ID[i], "FLUX_OUT"] <- sum(FLUXES[BOXES_ID[i],BOXES_ID])
    i <- i + 1
  }

  INITIAL$FLUX_BALANCE <- INITIAL$FLUX_IN-INITIAL$FLUX_OUT
  INITIAL$RES_TIME <- INITIAL$SIZE_INIT/INITIAL$FLUX_OUT #### !!!! WARNING !!!! DEFINITION FOR A BOX WITH BALANCED IN/OUT FLUXES

  #### IDENTIFY AND EDIT WARNINGS FOR UNBALANCED FINITE BOXES
  #### AND AUTOMATICALLY ROUTE TO EITHER ANALYTICAL SOLVER (ISOPYBOX_ANA) or NUMERICAL SOLVER (ISOPYBOX_NUM)

  if (is.na(INFINITE_BOXES[1]) == F){
    FINITE_BOXES <- INITIAL[-which(INITIAL$BOXES_ID %in% INFINITE_BOXES), "BOXES_ID"]
  } else {
    FINITE_BOXES <- INITIAL[, "BOXES_ID"]
  }

  INITIAL$t_lim_run <- NaN
  INITIAL[,"t_lim_run"] <- - INITIAL[,"SIZE_INIT"]/INITIAL[,"FLUX_BALANCE"]
  NUM_ANA = "ANA"
  UNBAL_FINITE_BOXES <- NaN

  i <- 1
  for (i in 1:nrow(INITIAL)){
    if (INITIAL[i,"BOXES_ID"] %in% FINITE_BOXES & INITIAL[i,"FLUX_BALANCE"] != 0){
      NUM_ANA = "NUM"
      if (HIDE_PRINTS == F){
        cat( paste("\n < ", INITIAL[i,"BOXES_ID"]," IN-OUT BALANCE ", sep = ""))
      }
      UNBAL_FINITE_BOXES <- c(UNBAL_FINITE_BOXES, as.character(INITIAL[i,"BOXES_ID"]))
      if (HIDE_PRINTS == F){
        if (INITIAL[i, "FLUX_BALANCE"] < 0){
          cat( paste("is neg (max run: ", - INITIAL[i,"SIZE_INIT"]/INITIAL[i,"FLUX_BALANCE"], " t units) > ", sep = ""))
        } else {
          cat(paste("is pos > ", sep = ""))
        }
      }
    }
    i <- i + 1
  }

  if (HIDE_PRINTS == F){
    if (NUM_ANA == "NUM"){
      cat( paste("\n < Running num_slvr (UNBALANCED FINITE BOXES) > \n", sep = ""))
    } else {
      cat( paste("\n < Running ana_slvr (BALANCED FINITE BOXES) >  \n", sep = ""))
    }
  }


  #### BUILD BOX_META to XLSX with ALL RESIDENCE TIME AND BALANCE INFORMATION
  BOX_META_to_xls <- INITIAL
  BOX_META_to_xls <- merge(INITIAL, BOXES_master, sort = F)

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PRE-RUN OUTPUTS
  #************************************** define coeff_list_name_outdir #----
  coeff_list_name_outdir <- coeff_list_name

  if (is.null(FORCING_ALPHA)){
    coeff_list_name_outdir <- coeff_list_name_outdir
  } else {
    coeff_list_name_outdir <- paste(coeff_list_name, "_mod", sep = "")
  }

  #************************************** UPDATE COEFF ID IF RAYLEIGH and PREPARE RAYLEIGH LOG INFORMATION #----
  if (is.null(FORCING_RAYLEIGH)){
    rayleigh_to_LOG <- NaN
  } else {
    FORCING_RAYLEIGH$ALPHA_ID <- NaN
    FORCING_RAYLEIGH$ALPHA_ID <- paste(FORCING_RAYLEIGH$AFROM, FORCING_RAYLEIGH$ATO, sep = "t")
    i  = 1
    for (i in 1:nrow(FORCING_RAYLEIGH)){
      if (i == 1){
        coeff_list_name_outdir <- paste(coeff_list_name, "_mod", sep = "")
        rayleigh_to_LOG <- paste(FORCING_RAYLEIGH[i, "XFROM"], "", FORCING_RAYLEIGH[i, "XTO"], "_", FORCING_RAYLEIGH[i, "YFROM"], "", FORCING_RAYLEIGH[i, "YTO"], "_", FORCING_RAYLEIGH[i, "AFROM"], "t", FORCING_RAYLEIGH[i, "ATO"],  "_", as.character(FORCING_RAYLEIGH[i, "ALPHA_0"]), sep = "")
      } else {
        rayleigh_to_LOG_loc <- paste(FORCING_RAYLEIGH[i, "XFROM"], "", FORCING_RAYLEIGH[i, "XTO"], "_", FORCING_RAYLEIGH[i, "YFROM"], "", FORCING_RAYLEIGH[i, "YTO"], "_", FORCING_RAYLEIGH[i, "AFROM"], "t", FORCING_RAYLEIGH[i, "ATO"],  "_", as.character(FORCING_RAYLEIGH[i, "ALPHA_0"]), sep = "")
        rayleigh_to_LOG <- paste(rayleigh_to_LOG, rayleigh_to_LOG_loc, sep = "__")
      }
      i <- i + 1
    }
  }

  #************************************** INITIATE LOG FILE DATA FRAME #----
  dir_LOG <- "1_LOG.csv"

  LOG_loc <- data.frame(RUN_n = NaN,
                        RUN_ID = NaN,
                        SERIES_RUN_ID = NaN,
                        RUN_STATUS = "WAITING",
                        SERIES_ID = c(SERIES_ID),
                        DATE_TIME = c(chartr(old = "-: ", new = "___", Sys.time())),
                        COEFF_FLUX = c(paste(coeff_list_name_outdir , "__", flux_list_name, sep = "")),
                        FLUX_MASTER = c(flux_list_name),
                        COEFF_MASTER = c(coeff_list_name),
                        COEFF_RUN = c(coeff_list_name_outdir),
                        NUM_ANA = NUM_ANA,
                        T_LIM = t_lim,
                        N_STEPS = nb_steps,
                        BOXES_ID_n = length(BOX_META_to_xls$BOXES_ID),
                        BOXES_ID_list = c(stringr::str_c(as.character(BOX_META_to_xls$BOXES_ID), collapse = "_")),
                        INFINITE_BOXES_list = c(stringr::str_c(INFINITE_BOXES, collapse = "_")),
                        DISCONNECTED_BOXES = NaN,
                        UNBAL_FINITE_BOXES = NaN,
                        SIZE_INIT = c(stringr::str_c(as.character(BOX_META_to_xls[, "SIZE_INIT"]), collapse = "_")),
                        DELTA_INIT = c(stringr::str_c(as.character(BOX_META_to_xls[, "DELTA_INIT"]), collapse = "_")),
                        FORCING_RAYLEIGH = rayleigh_to_LOG,
                        FORCING_SIZE = NaN,
                        FORCING_DELTA = NaN,
                        FORCING_ALPHA = NaN,
                        COMPOSITE = FALSE,
                        COMPO_SERIES_FAMILY = NaN,
                        COMPO_SERIES_n = NaN,
                        EXPLORER = FALSE,
                        EXPLO_SERIES_FAMILY = NaN,
                        EXPLO_SERIES_n = NaN,
                        path_outdir = NaN)

  if (length(UNBAL_FINITE_BOXES) > 1){
    LOG_loc$UNBAL_FINITE_BOXES <- c(stringr::str_c(as.character(UNBAL_FINITE_BOXES[2:length(UNBAL_FINITE_BOXES)]), collapse = "_"))
  }

  if (COMPOSITE == TRUE){
    LOG_loc$COMPOSITE <- TRUE
    LOG_loc$COMPO_SERIES_n <- COMPO_SERIES_n
    LOG_loc$COMPO_SERIES_FAMILY <- COMPO_SERIES_FAMILY
  }

  if (EXPLORER == TRUE){
    LOG_loc$EXPLORER <- TRUE
    LOG_loc$EXPLO_SERIES_n <- EXPLO_SERIES_n
    LOG_loc$EXPLO_SERIES_FAMILY <- EXPLO_SERIES_FAMILY
  }

  if (is.null(FORCING_DELTA) == F){
    LOG_loc$FORCING_DELTA = paste(as.character(FORCING_DELTA$BOXES_ID), as.character(FORCING_DELTA$DELTA_INIT), collapse = "_", sep = "_")
  }

  if (is.null(FORCING_SIZE) == F){
    LOG_loc$FORCING_SIZE = paste(as.character(FORCING_SIZE$BOXES_ID), as.character(FORCING_SIZE$SIZE_INIT), collapse = "_", sep = "_")
  }

  if (is.null(FORCING_ALPHA) == F){
    LOG_loc$FORCING_ALPHA = paste(as.character(FORCING_ALPHA$FROM), as.character(FORCING_ALPHA$TO), as.character(FORCING_ALPHA$ALPHA), collapse = "_", sep = "_")
  }


  #************************************** DEFINE SERIES OUTDIR #----
  if (COMPOSITE == T){
    outdir <- paste("3_", SERIES_ID, sep = "")
  } else if (EXPLORER == T){
    outdir <- paste("4_", SERIES_ID, sep = "")
  } else {
    outdir <- paste("2_RUN_", SERIES_ID, sep = "")
  }


  #### Edit/Create outdir folder and check slash in outdir
  check_slash <- unlist(strsplit(outdir, ""))
  if (check_slash[length(check_slash)] != "/"){
    outdir <- paste(outdir, "/", sep = "")
  }

  if (dir.exists(outdir) == FALSE){
    dir.create(outdir)
  }

  #************************************** DEFINE RUN OUTDIR and RUN ID #----
  #### DEFINE RUN number in the list of a given series, RUN_ID, SERIES_RUN_ID
  n_zeros <- 4
  if (file.exists(dir_LOG) == FALSE){
    LOG_loc$RUN_n <- 1
    LOG_loc$RUN_ID <- paste(c(as.character(replicate(n_zeros-length(unlist(strsplit(as.character(LOG_loc$RUN_n), ""))),0)), as.character(LOG_loc$RUN_n)), collapse = "")
    LOG_loc$SERIES_RUN_ID <- paste(SERIES_ID, LOG_loc$RUN_ID, sep = "_")
  } else {
    LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
    if (SERIES_ID %in% levels(LOG$SERIES_ID)){
      LOG_loc$RUN_n <- max(LOG[LOG$SERIES_ID == SERIES_ID, "RUN_n"])+1
    } else {
      LOG_loc$RUN_n <- 1
    }
    remove(LOG)
    LOG_loc$RUN_ID <- paste(c(as.character(replicate(n_zeros-length(unlist(strsplit(as.character(LOG_loc$RUN_n), ""))),0)), as.character(LOG_loc$RUN_n)), collapse = "")
    LOG_loc$SERIES_RUN_ID <- paste(SERIES_ID, LOG_loc$RUN_ID, sep = "_")
  }

  #### CREATE A FILE WITH RUN ID FOR EACH RUN placed in SERIES FOLDER
  folder_outdir <- paste(outdir, SERIES_ID, "_", as.character(LOG_loc$RUN_ID), "_", sep = "")
  LOG_loc$path_outdir = folder_outdir


  #************************************** UPDATE TOTAL RUN TIME DEPENDING ON MOST UNBALANCED BOX (incl. "INFINITE") #----
  if (length(BOX_META_to_xls[BOX_META_to_xls$t_lim_run > 0, "t_lim_run"]) > 0){
    MIN_POS_t_lim_run <- min(BOX_META_to_xls[BOX_META_to_xls$t_lim_run > 0, "t_lim_run"])
    MIN_POS_t_lim_run_BOX <- as.character(BOX_META_to_xls[BOX_META_to_xls$t_lim_run == MIN_POS_t_lim_run, "BOXES_ID"])
  } else {
    MIN_POS_t_lim_run <- t_lim
  }

  if (t_lim > MIN_POS_t_lim_run){
    CONSTS[CONSTS$CONSTS_ID == "time", "CONSTS"] <- as.character(MIN_POS_t_lim_run)
    cat( paste("*** UPDATED TOTAL RUN TIME *** < Total run time has been changed from ", as.character(t_lim), " to ", as.character(MIN_POS_t_lim_run), " (limiting box: ", MIN_POS_t_lim_run_BOX, ") > \n " , sep = ""))
    LOG_loc$T_LIM <- MIN_POS_t_lim_run
    CONSTS_trad <- CONSTS
  }

  #************************************** EDIT EXCEL INPUT FILE FOR CURRENT RUN #----
  trad_excel_path <-  paste(folder_outdir, "INPUT.xlsx", sep = "")

  writexl::write_xlsx(list(CONSTS = CONSTS_trad,
                           INITIAL = INITIAL_trad,
                           FLUXES = FLUXES_trad,
                           COEFFS = COEFFS_trad,
                           BOX_META = BOX_META_to_xls),
                      trad_excel_path)

  #************************************** NETWORK DIAGRAM OUTPUT #----
  #### REMOVE ISOLATED BOXES FOR NETWORK DIAGRAM

  BOXES_network_drop <- INITIAL[INITIAL$FLUX_IN == 0 & INITIAL$FLUX_OUT == 0, "BOXES_ID"]

  if (length(BOXES_network_drop) > 0){
    LOG_loc$DISCONNECTED_BOXES <- c(stringr::str_c(as.character(BOXES_network_drop), collapse = "_"))
  }

  if (length(BOXES_network_drop) > 0){
    INITIAL_short <- INITIAL[-which(INITIAL$BOXES_ID %in% BOXES_network_drop),]

    FLUXES_short <- FLUXES[, -which(names(FLUXES) %in% BOXES_network_drop)]
    FLUXES_short <- FLUXES_short[-which(row.names(FLUXES_short) %in% BOXES_network_drop),]

    COEFFS_short <- COEFFS[, -which(names(COEFFS) %in% BOXES_network_drop)]
    COEFFS_short <- COEFFS_short[-which(row.names(COEFFS_short) %in% BOXES_network_drop),]
  } else {
    INITIAL_short <- INITIAL
    FLUXES_short <- FLUXES
    COEFFS_short <- COEFFS
  }

  #### PREP MATRICES FOR NETWORK DIAGRAMS
  FLUXES_adj <- FLUXES_short
  FLUXES_adj <- FLUXES_adj[ , !(names(FLUXES_adj) %in% "BOXES_ID")]
  COEFFS_adj <- COEFFS_short
  COEFFS_adj <- COEFFS_adj[ , !(names(COEFFS_adj) %in% "BOXES_ID")]
  COEFFS_adj <- 1000*log(COEFFS_adj)

  BOX_groups <- INITIAL_short$GROUPS

  if (length(BOXES_network_drop) > 0){
    BOXES_master_loc <- BOXES_master[-which(BOXES_master$BOXES_ID %in% BOXES_network_drop), c("BOXES_ID", "INFINITE", "LAYOUT_X", "LAYOUT_Y")]
  } else {
    BOXES_master_loc <- BOXES_master[, c("BOXES_ID", "INFINITE", "LAYOUT_X", "LAYOUT_Y")]
  }

  names(BOXES_master_loc) <- c("BOXES_ID", "GROUP", "X_COORD", "Y_COORD")
  row.names(BOXES_master_loc) <- BOXES_master_loc$BOXES_ID

  FLUXES_adj_box_order <- data.frame(BOXES_ID = row.names(FLUXES_adj),
                                     ORDER = 1:nrow(FLUXES_adj))

  INITIAL_short_loc <- merge(BOXES_master_loc, INITIAL_short, by = "BOXES_ID", sort = F)
  INITIAL_short_loc <- merge(INITIAL_short_loc, FLUXES_adj_box_order, by = "BOXES_ID", sort = F)
  INITIAL_short_loc <- clear_subset(INITIAL_short_loc)
  INITIAL_short_loc <- INITIAL_short_loc[order(INITIAL_short_loc$ORDER),]
  INITIAL_short_loc <- clear_subset(INITIAL_short_loc)

  matrix_layout <- as.matrix(INITIAL_short_loc[,c("X_COORD", "Y_COORD")])

  #### NETWORK DIAGRAM TITLES
  NET_FLUXES_title <- paste("Flux config: " , flux_list_name, sep = "")

  if (is.null(FORCING_RAYLEIGH)){
    NET_COEFFS_title <- paste("Coeffs config: " , coeff_list_name, sep = "")
  } else {
    FORCING_RAYLEIGH$ALPHA_ID <- NaN
    FORCING_RAYLEIGH$ALPHA_ID <- paste(FORCING_RAYLEIGH$AFROM, FORCING_RAYLEIGH$ATO, sep = "_to_")
    i  = 1
    RLG_alpha_list <- NULL
    for (i in 1:nrow(FORCING_RAYLEIGH)){
      RLG_alpha_list <- paste(RLG_alpha_list, FORCING_RAYLEIGH[i, "ALPHA_ID"], sep =  " - ")
      i <- i + 1
    }
    NET_COEFFS_title <- paste("Coeffs config: " , coeff_list_name, " // Rayleigh forcing: ", LOG_loc$FORCING_RAYLEIGH, sep = "")
  }

  if (is.null(FORCING_ALPHA)){
    NET_COEFFS_title <- NET_COEFFS_title
  } else {
    NET_COEFFS_title <- paste(NET_COEFFS_title, " // Alpha forcing: ", LOG_loc$FORCING_ALPHA, sep = "")
  }

  if (PLOT_DIAGRAMS == T){
    #### EDIT NETWORK DIAGRAM PDF
    pdf_path <- paste(folder_outdir, "DIAGf_", flux_list_name, ".pdf", sep = "")
    pdf(pdf_path, width = 3, height = 3, pointsize = 1, useDingbats=FALSE, encoding="MacRoman")
    NET_FLUXES <- qgraph::qgraph(FLUXES_adj,
                                 title = NET_FLUXES_title,
                                 layout = matrix_layout,
                                 edge.labels = T,
                                 edge.label.color = "black",
                                 shape = "square",
                                 fade = F,
                                 groups = BOXES_master_loc$GROUP,
                                 color = rainbow(length(levels(BOXES_master_loc$GROUP)), s = 0.25),
                                 legend = F,
                                 edge.color = "black",
                                 edge.label.cex = 2.5,
                                 edge.label.margin = 0.02,
                                 asize = 8,
                                 vsize = 16*exp(-nrow(BOXES_master_loc)/80)+1)
    dev.off()

    pdf_path <- paste(folder_outdir, "DIAGa_", coeff_list_name_outdir, ".pdf", sep = "")
    pdf(pdf_path, width = 3, height = 3, pointsize = 1, useDingbats=FALSE, encoding="MacRoman")
    NET_COEFFS <- qgraph::qgraph(COEFFS_adj,
                                 title = NET_COEFFS_title,
                                 title.cex = 0.25,
                                 layout = matrix_layout,
                                 edge.labels = T,
                                 edge.label.color = "black",
                                 shape = "square",
                                 fade = F,
                                 groups = BOXES_master_loc$GROUP,
                                 color = rainbow(length(levels(BOXES_master_loc$GROUP)), s = 0.25),
                                 legend = F,
                                 edge.color = "brown4",
                                 edge.label.cex = 2.5,
                                 edge.label.margin = 0.02,
                                 asize = 8,
                                 vsize = 16*exp(-nrow(BOXES_master_loc)/80)+1)
    dev.off()
  }

  #************************************** UPDATE LOG DATA FRAME, EDIT CSV #----
  if (file.exists(dir_LOG) == FALSE){
    data.table::fwrite(LOG_loc, file = dir_LOG, row.names = F, quote = F)
  } else {
    data.table::fwrite(LOG_loc, file = dir_LOG, row.names = F, quote = F, append = T)
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# RUN ISOBOXr #----
  input_path <- paste(LOG_loc$path_outdir, "INPUT.xlsx", sep = "")
  if (LOG_loc$NUM_ANA == "ANA"){
    ana_slvr(input_path)
  } else {
    if (LOG_loc$NUM_ANA == "NUM"){
      num_slvr(input_path)
    }
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# POST-RUN OUTPUTS
  #************************************** EXTRACT or COMPUTE TIME DEPENDENT EVOLUTION OF DELTA VALUES #----
  if (LOG_loc$NUM_ANA == "ANA"){ #### OFFLINE CALCULATION of EV D for ANA using EigenVec/Vals/Coeffs
    evD <- read.csv(paste(folder_outdir, "OUT/", as.character(LOG_loc$SERIES_RUN_ID), "_A_3_evD.csv", sep = ""), sep = ",")
    t_lim <- LOG_loc$T_LIM
    nb_steps <- LOG_loc$N_STEPS
    ratio_standard <- CONSTANTS$RATIO_STANDARD
    BOXES_IDs <- names(evD[,-which(names(evD) %in% c("Time"))])

  } else {
    if (LOG_loc$NUM_ANA == "NUM"){ #### READ ISOPYBOX_NUM evD already Computed
      ISO_OUT_loc <- read.csv(paste(folder_outdir, "OUT/", as.character(LOG_loc$SERIES_RUN_ID), "_N_3_evD.csv", sep = ""), sep = ",")
      evD <- ISO_OUT_loc
      t_lim = LOG_loc$T_LIM
      nb_steps = LOG_loc$N_STEPS
      ratio_standard <- CONSTANTS$RATIO_STANDARD
      BOXES_IDs = names(ISO_OUT_loc[,-which(names(ISO_OUT_loc) %in% c("Time"))])
    }
  }

  #************************************** CREATE evD PLOTS #----
  ##### LOCAL PLOT ARGUMENTS
  initial_time_unit <- time_units[1]
  display_time_unit <- time_units[2]

  #### VERTICALIZE evD
  evD_vert <- DF_verticalizer(df_hor = evD, vert_col = BOXES_IDs)

  #### CHANGE TIME UNITS FROM FLUX IMPOSED UNIT TO WANTED DISPLAY UNIT
  if (display_time_unit != initial_time_unit & initial_time_unit == "days"){
    if (display_time_unit == "hours"){
      evD_vert$Time <- evD_vert$Time*24
    } else {
      if (display_time_unit == "minutes"){
        evD_vert$Time <- evD_vert$Time*24*60
      } else {
        if (display_time_unit == "years"){
          evD_vert$Time <- evD_vert$Time/365
        } else {
          display_time_unit = initial_time_unit
        }
      }
    }
  }

  #### CREATE PLOT (OPTION TO HIDE)
  if (PLOT_evD == TRUE){
    Ymin <- round(min(evD_vert$VAR), 0)-1
    Ymax <- round(max(evD_vert$VAR), 0)+1
    Ymin_zoom <- min(evD_vert$VAR)
    Ymax_zoom <- max(evD_vert$VAR)
    Ybin <- 0.25
    Xmin <- evD_vert[2,"Time"]
    Xmax <- max(evD_vert$Time) + 0.5*max(evD_vert$Time)

    if (is.na(INFINITE_BOXES[1]) == F){
      evD_vert <- evD_vert[-which(evD_vert$VAR_TYPE %in% c(INFINITE_BOXES)),]
    }

    if (length(BOXES_network_drop) > 0){
      evD_vert <- evD_vert[-which(evD_vert$VAR_TYPE %in% c(as.character(BOXES_network_drop))),]
    }

    evD_vert <- evD_vert[2:nrow(evD_vert),]
    evD_vert <- clear_subset(evD_vert)

    evD_initial <- evD_vert[evD_vert$Time == min(evD_vert$Time),]

    evD_final <- evD_vert[evD_vert$Time == max(evD_vert$Time),]

    evD_plot <- ggplot2::ggplot(data = evD_vert, ggplot2::aes(x = Time, y = VAR, color = VAR_TYPE))+
      ggplot2::geom_line(cex = 1)+
      ggplot2::theme_bw()+
      ggplot2::scale_y_continuous(limits=c(Ymin, Ymax), breaks=seq(Ymin, Ymax, by = Ybin), labels = dec_2)+
      ggplot2::coord_cartesian(ylim = c(Ymin_zoom, Ymax_zoom), xlim = c(Xmin, Xmax))+
      ggplot2::labs(y = paste("d", CONSTANTS$NUMERATOR, "/", CONSTANTS$DENOMINATOR, CONSTANTS$ELEMENT, sep = ""),
           x = paste("Time in", display_time_unit, sep = " "),
           title = paste(SERIES_ID, "-", LOG_loc$RUN_ID, ": ", LOG_loc$COEFF_FLUX, ", ", LOG_loc$NUM_ANA, sep = ""),
           subtitle = NET_COEFFS_title)+
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size=7))+
      ggrepel::geom_text_repel(data = evD_final, ggplot2::aes(label = paste(VAR_TYPE, " (", dec_2(VAR), ")", sep = ""), color = VAR_TYPE), nudge_x = 0.05*max(evD_vert$Time),  hjust = 0)+
      ggplot2::scale_x_log10()

    #### EXPORT PLOT
    pdf_path <- paste(folder_outdir, "OUT/", SERIES_ID, "_", as.character(LOG_loc$RUN_ID), "_plot_evD.pdf", sep = "")
    dev.new()
    pdf(pdf_path, width = 10, height = 7, pointsize = 1, useDingbats=FALSE, encoding="MacRoman")
    suppressWarnings(print(evD_plot))
    graphics.off()
  }
}
