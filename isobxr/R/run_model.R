#' @importFrom grDevices dev.list dev.new dev.off graphics.off pdf rainbow
NULL

#' @importFrom utils head read.csv
NULL

#  #_________________________________________________________________________80char
#' Run isobxr stable isotope box model
#' @description  A function to run the isobxr stable isotope box model,
#' assessing the design of the model and automatically running \code{\link{num_slvr}}
#' or \code{\link{ana_slvr}} depending on the conditions.
#' @param workdir Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file \cr
#' and where output files will be stored if exported by user. \cr
#' (character string)
#' @param SERIES_ID Name of the model series the run belongs to. \cr
#' It determines the folder in which the output files will be stored.\cr
#' (character string)
#' @param flux_list_name Name of the list of fluxes and initial box sizes to be used for the run, \cr
#' calling (by its header name) a single column of the \strong{FLUXES} sheet
#' of the \strong{\emph{0_ISOBXR_MASTER.xlsx}} file. \cr
#' (character string)
#' @param coeff_list_name Name of the list of fractionation coefficients to be used for the run, \cr
#' calling (by its header name) a single column of the \strong{COEFFS} sheet
#' of the \strong{\emph{0_ISOBXR_MASTER.xlsx}} file. \cr
#' (character string)
#' @param t_lim Run duration, given in the same time units as the fluxes. \cr
#' (integer)
#' @param nb_steps Number of calculation steps. \cr
#' It determines the resolution of the run. \cr
#' (integer)
#' @param time_units Vector defining the initial time unit
#' (identical to unit used in fluxes), \cr
#' followed by the time unit used for the graphical output.\cr
#' Character string, to be selected  among the following:\cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}\cr
#' e.g.,  c("d", "yr") to convert days into years
#' @param FORCING_RAYLEIGH \emph{OPTIONAL} \cr
#' Dataframe describing the forcing on a fractionation coefficient
#' by a Rayleigh isotope distillation, \cr
#' as a function of flux intensities and a fundamental fractionation coefficient. \cr
#' Dataframe formatting details are in isobxr vignette. \cr
#' Default is NULL.
#' @param FORCING_SIZE \emph{OPTIONAL} \cr
#' Dataframe describing the forcing on one or several box sizes (mass of element X). \cr
#' The newly defined sizes for the given set of boxes
#' overwrite their sizes as previously defined in \strong{\emph{0_ISOBXR_MASTER.xlsx}} file. \cr
#' Dataframe formatting details are in isobxr vignette. \cr
#' Default is NULL.
#' @param FORCING_DELTA \emph{OPTIONAL} \cr
#' Dataframe describing the forcing on one or several boxes
#' initial isotope composition expressed as delta values. \cr
#' The newly defined delta values for the given set of boxes
#' overwrite the delta values as previously defined in \strong{\emph{0_ISOBXR_MASTER.xlsx}} file. \cr
#' Dataframe formatting details are in isobxr vignette. \cr
#' Default is NULL.
#' @param FORCING_ALPHA \emph{OPTIONAL} \cr
#' Dataframe describing the forcing on one or several
#' fractionation coefficients from one reservoir to another. \cr
#' The newly defined alpha values for the given set of boxes
#' overwrite the alpha values as previously defined in \strong{\emph{0_ISOBXR_MASTER.xlsx}} file. \cr
#' Dataframe formatting details are in isobxr vignette. \cr
#' Default is NULL.
#' @param COMPOSITE \emph{NOT TO BE USED IN SINGLE RUN}  \cr
#' Logical value automatically defined in \code{\link{compose_isobxr}}. \cr
#' Default is FALSE.
#' @param COMPO_SERIES_n \emph{NOT TO BE USED IN SINGLE RUN}  \cr
#' Iteration of the composite run for the given series it belongs to,
#' automatically defined in \code{\link{compose_isobxr}}.  \cr
#' Default is NaN.
#' @param COMPO_SERIES_FAMILY \emph{NOT TO BE USED IN SINGLE RUN} \cr
#' Composite run series family, automatically defined in
#' \code{\link{compose_isobxr}}. \cr
#' Default is NaN.
#' @param EXPLORER \emph{NOT TO BE USED IN SINGLE RUN} \cr
#' Logical value automatically defined in \code{\link{sweep_steady}}
#' or \code{\link{sweep_dyn}}. \cr
#' Default is FALSE.
#' @param EXPLO_SERIES_n \emph{NOT TO BE USED IN SINGLE RUN}  \cr
#' Iteration of the sweep run for the given series it belongs to,
#' automatically defined in \code{\link{sweep_steady}} or \code{\link{sweep_dyn}}. \cr
#' Default is NaN.
#' @param EXPLO_SERIES_FAMILY \emph{NOT TO BE USED IN SINGLE RUN}  \cr
#' Sweep run series family, automatically defined in
#' \code{\link{sweep_steady}} or \code{\link{sweep_dyn}}. \cr
#' Default is NaN.
#' @param HIDE_PRINTS \emph{OPTIONAL}  \cr
#' Logical value. \cr
#' Prints outputs details in R console if TRUE. \cr
#' This parameter does not hide the warnings regarding the automatic update of
#' the run duration in case of the emptying of a box. \cr
#' Default is FALSE.
#' @param to_DIGEST_DIAGRAMS \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Edits pdf of box model diagram in RUN DIGEST folder if TRUE. \cr
#' Default is TRUE.
#' @param to_DIGEST_evD_PLOT \emph{OPTIONAL} \cr
#' Logical value.  \cr
#' Edits pdf of delta time evolution plot in RUN DIGEST folder if TRUE.\cr
#' Default is TRUE.
#' @param to_DIGEST_CSV_XLS \emph{OPTIONAL}\cr
#' Logical value.  \cr
#' Edits xlsx version of the Rda input file (ending with _IN.xlsx) and all \code{\link{ana_slvr}}
#' or \code{\link{num_slvr}} CSV output files in RUN DIGEST folder if TRUE. \cr
#' Default is FALSE.
#' @param evD_PLOT_time_as_log \emph{OPTIONAL}\cr
#' Logical value.  \cr
#' Print evD plot with log10 time scale as x-axis. \cr
#' Default is TRUE.
#' @param plot_results \emph{OPTIONAL} \cr
#' Logical value. \cr
#' If TRUE, plots in R session the single model run evolution of delta values and box sizes for all boxes. \cr
#' Default is TRUE.
#' @param save_run_outputs  \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Allows saving all run outputs to working directory (workdir). \cr
#' By default, run outputs are stored in the temporary directory and are erased if not exported. \cr
#' Default is FALSE.
#'
#' @return Calculates the time evolution of delta values and box sizes in all boxes.
#'
#' \code{\link{run_isobxr}} returns by default a plot showing time evolution of delta values and box sizes
#' for all boxes (set plot_results = FALSE to mute the plots). \cr
#'
#' \code{\link{run_isobxr}} creates a series of isotope data and metadata,
#' all of which are stored in a temporary directory. \cr
#' The user can save all outputs described below to their working directory by setting save_run_outputs = TRUE (default is FALSE). \cr
#'
#' If \code{\link{run_isobxr}} is run independently,
#' it creates and stores all outputs in a \emph{SERIES} folder,
#' with the following name structure: \cr
#' \strong{\emph{2_RUN + SERIES_ID}}
#'
#' \code{\link{run_isobxr}} base workflow:
#' \enumerate{
#' \item Automatically sets a XXXX run number between 0001 and 9999. \cr
#' The outputs do not overwrite possible identical previously performed runs.
#'
#' \item Stores all run commands in a file with the Rda format. \cr
#' This file stores all commands used as arguments for solver function
#' (\code{\link{ana_slvr}} or \code{\link{num_slvr}}). \cr
#' (file name structure: \strong{\emph{SERIES_ID + XXXX + _IN.Rda}}) \cr
#' This encompasses the following: \cr
#' \enumerate{
#' \item \strong{CONSTS_IN}: Dataframe documenting the constants (Element, isotopes, reference ratio)
#' \item \strong{INITIAL_IN}:  Dataframe documenting the initial conditions of all box sizes (incl. empty boxes) \cr
#' and all initial delta values.
#' \item \strong{FLUXES_IN}: Dataframe documenting the mass fluxes of element X \cr
#' between all boxes (structured as a square matrix)
#' \item \strong{COEFFS_IN}: Dataframe documenting the isotopic fractionation coefficients \cr
#' between all boxes (structured as a square matrix)
#' \item \strong{BOX_META_IN}: Dataframe documenting box metadata \cr
#' (Box names, initial X masses and delta values, total inward and outward X fluxes in each box, \cr
#' flux balance for each box, X residence time for all balanced boxes, max run time before total emptying of each box, \cr
#' box infinite/finite status, system diagram coordinates)
#' }
#'
#' \item Stores all outputs in a file with the Rda format. \cr
#' This file stores all data produced by the function. \cr
#' (file name structure: \strong{\emph{SERIES_ID + XXXX + _OUT.Rda}})
#'
#' \item Updates the general log file. \cr
#' (file name: \strong{\emph{1_LOG.csv}})
#' }
#'
#' @section Optional outputs, stored in \emph{DIGEST} folder:
#'
#' A \emph{DIGEST} folder is created to store all optional outputs of the \code{\link{run_isobxr}} function. \cr
#' The \emph{DIGEST} folder is created in the \emph{SERIES} folder with the following name structure: \cr
#' \strong{\emph{2_RUN + SERIES_ID / SERIES_ID + XXXX + DIGEST}}
#'
#' \enumerate{
#' \item If to_DIGEST_CSV_XLS = TRUE, \cr
#' creates an INPUT file in the xlsx format stored in \emph{DIGEST} folder, \cr
#' containing all run conditions and parameters. \cr
#' (file name structure: \strong{\emph{in_0_INPUTS + SERIES_ID + XXXX + .xlsx}}) \cr
#'
#' \item If to_DIGEST_CSV_XLS = TRUE, \cr
#' stores csv versions of the \code{\link{num_slvr}} or \code{\link{ana_slvr}} outputs in \emph{DIGEST} folder. \cr
#' See \code{\link{num_slvr}} or \code{\link{ana_slvr}} documentation for further details.
#'
#' \item If to_DIGEST_DIAGRAMS = TRUE, \cr
#' edits a Box model diagram of flux (DIAG_FLUX pdf) of element X (mass per time unit)
#' between all boxes. \cr
#' (file name structure: \strong{\emph{in_1_DIAG_FLUX + SERIES_ID + XXXX + .pdf}})
#'
#' \item If to_DIGEST_DIAGRAMS = TRUE, \cr
#' edits a Box model diagram of isotope fractionation coefficients (DIAG_COEFF pdf)
#' between all boxes. \cr
#' (file name structure: \strong{\emph{in_2_DIAG_COEFF + SERIES_ID + XXXX + .pdf}})
#'
#' \item If to_DIGEST_evD_PLOT = TRUE, \cr
#' edits a pdf plot of the time dependent evolution of delta values
#' together with the evolution of the box sizes (masses of element X). \cr
#' The time x-axis is a logarithmic scale by default but can be set
#' to linear scale with the evD_PLOT_time_as_log parameter, if set to FALSE.
#' (file name structure: \strong{\emph{out_0_PLOT_evD + SERIES_ID + XXXX + .pdf}})
#'
#' }
#' @seealso Documentation on \code{\link{num_slvr}} or \code{\link{ana_slvr}} functions.
#' @examples
#' # Example 1. {ABC}, closed, balanced
#' # for more information see tutorial at
#' # https://ttacail.github.io/isobxr_web/vgn_04_Run_isobxr_tutorial.html#22_Run_the_model
#'
#' # This is an example using the tutorial files embedded in package data
#' # It can be run as such.
#' run_isobxr(workdir = "/Users/username/Documents/1_ABC_tutorial",
#'            SERIES_ID = "ABC_closed_balanced", # series ID of the set of runs
#'            flux_list_name = "Fx1_ABC_closed_bal", # which flux list from FLUXES sheet
#'            coeff_list_name = "a1", # which coefficients list from COEFFS sheet
#'            t_lim = 2500, # how long do I want to run
#'            nb_steps = 250, # how many steps over this run duration
#'            time_units = c("d", "yr"), # run time units (days), plot time units (years)
#'            to_DIGEST_evD_PLOT = FALSE, # do not export plots as pdf files
#'            to_DIGEST_CSV_XLS = FALSE, # do not export datasets as csv and xlsx files
#'            to_DIGEST_DIAGRAMS = FALSE) # do not export diagrams as pdf files
#' @export
run_isobxr <- function(workdir, SERIES_ID, flux_list_name, coeff_list_name, t_lim, nb_steps, time_units,
                       FORCING_RAYLEIGH = NULL, FORCING_SIZE = NULL, FORCING_DELTA = NULL, FORCING_ALPHA = NULL,
                       COMPOSITE = FALSE, COMPO_SERIES_n = NaN, COMPO_SERIES_FAMILY = NaN,
                       EXPLORER = FALSE, EXPLO_SERIES_n = NaN, EXPLO_SERIES_FAMILY = NaN,
                       HIDE_PRINTS = FALSE, to_DIGEST_DIAGRAMS = TRUE, to_DIGEST_evD_PLOT = TRUE, to_DIGEST_CSV_XLS = FALSE,
                       evD_PLOT_time_as_log = TRUE, plot_results = TRUE,
                       save_run_outputs = FALSE){

  # locally bind variables (fixing binding global variable issue)
  output_list <- A_OUT <- N_evS <- A_evD <- N_evD <- NULL

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# set up extdata tutorial mode
  tuto_setup <- using_extdata_tutorial(workdir = workdir, save_run_outputs = save_run_outputs, plot_results = plot_results)
  tuto_mode <- as.logical(tuto_setup[1])
  workdir <- tuto_setup[2]
  if(isFALSE(COMPOSITE) & isFALSE(EXPLORER)){
    save_run_outputs <- as.logical(tuto_setup[3])
    plot_results <- as.logical(tuto_setup[4])
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIALIZE
  #************************************** SET WORKING DIRECTORY and DEFINE ISOPY_MASTER file #----
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(workdir)
  ISOPY_MASTER_file <- "0_ISOBXR_MASTER.xlsx"

  if(isFALSE(COMPOSITE) & isFALSE(EXPLORER)){
    unlink(to_tmpdir(""), recursive = TRUE)
    on.exit(unlink(to_tmpdir(""), recursive = TRUE), add = TRUE)
    rlang::inform("________________________________________________________________________________")
    if (isTRUE(tuto_mode)){
      rlang::inform(paste("\U2139 workdir: no workdir.
  You are using the tutorial mode (isobxr embedded tutorial files).
  The default outputs are limited and can't be exported.", sep = ""))
    } else {
      rlang::inform(paste("\U2139 workdir: ", getwd(), sep = ""))
    }
    rlang::inform("________________________________________________________________________________")
  }

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


  # WARNING # warning for n > 2 inf bxes / for connected bxes only
  if ((all(!is.na(INFINITE_BOXES))) & length(INFINITE_BOXES) > 2){
    # rlang::warn(paste("The modelling of open systems with isobxr best works with no more than 2 infinite boxes. \n",
    #                   "You defined more than 2 infinite boxes: [",paste(INFINITE_BOXES, collapse = ", "), "] \n",
    #                   "The numerical outputs will be accurrate. ",
    #                   "This type of design is however currently not supported by the plot editing shiny app.", sep = ""))
    rlang::inform(paste("\U2757 The modelling of open systems with isobxr best works with no more than 2 infinite boxes. \n",
                        "   You defined more than 2 infinite boxes: [",paste(INFINITE_BOXES, collapse = ", "), "] \n",
                        "   The numerical outputs will be accurrate. \n",
                        "   This type of design is however currently not supported by the plot editing shiny app.", sep = ""))
  }

  #************************************** EXTRACT AND PREPARE LOCAL RUN INPUTS #----
  #### EXTRACT LIST OF COEFF and FLUXES LIST names
  list_COEFFS_master <- colnames(COEFFS_master[, -which(colnames(COEFFS_master) %in% c("FROM", "TO"))])
  list_FLUXES_master <- colnames(FLUXES_master[, -which(colnames(FLUXES_master) %in% c("FROM", "TO", "ID", "STATUT"))])
  list_BOXES_master <- as.character(BOXES_master$BOXES_ID)
  all_flux_coeff_levels <- c(levels(FLUXES_master$FROM),
                             levels(FLUXES_master$TO),
                             levels(FLUXES_master$ID),
                             levels(COEFFS_master$FROM),
                             levels(COEFFS_master$TO))
  list_all_boxes <- c(list_BOXES_master, all_flux_coeff_levels)
  n_BOXES <- length(list_BOXES_master)

  # ERROR # CHECK BOXES NAMES ARE SPECIAL CHARACTER FREE
  misnamed_boxes <- list_all_boxes[stringr::str_detect(list_all_boxes, pattern = "[ !@#$%^&*()_+}{\';|:/.,?><}]")]
  if (length(misnamed_boxes) > 0){
    rlang::abort(paste("Box names (defined in ", as.character(ISOPY_MASTER_file), ") must not include any special characters. \n",
                       "The following box names do not match the required format: \n",
                       "[", paste(misnamed_boxes, collapse = ", "), "]",
                       sep = ""))
  }
  remove(misnamed_boxes)

  #### EXTRACT LOCAL FLUX / ALPHA LISTS
  flux_list_loc <- FLUXES_master[FLUXES_master$STATUS == "FLUX", c("FROM", "TO", "STATUS", flux_list_name)]
  coeff_list_loc <- COEFFS_master[, c("FROM", "TO", coeff_list_name)]

  # ERROR # CHECK BOX NAMES CALLED IN FLUX AND COEFF LISTS are ALL DEFINED IN BOX LIST MASTER
  if (!(all(all_flux_coeff_levels %in% c(list_BOXES_master, "NaN", NaN)))){
    rlang::abort(paste("Boxes called in flux and coefficient lists must be defined in the list of boxes ",
               "(in ", as.character(ISOPY_MASTER_file), "). \n",
               "The following boxes are not defined in the list of boxes: \n",
               "[", paste(all_flux_coeff_levels[!(all_flux_coeff_levels %in% c(list_BOXES_master, "NaN", NaN))], collapse = ", "), "]",
               sep = ""))
  }
  remove(all_flux_coeff_levels)

  #### SET INITIAL - SIZES and DELTAs
  SIZE_INITIAL <- FLUXES_master[FLUXES_master$STATUS == "SIZE", c("ID", flux_list_name)]
  names(SIZE_INITIAL)[names(SIZE_INITIAL) == 'ID'] <- 'BOXES_ID'
  names(SIZE_INITIAL)[names(SIZE_INITIAL) == flux_list_name] <- 'SIZE_INIT'
  SIZE_INITIAL <- clear_subset(SIZE_INITIAL)

  # CHECK ALL BOXES DEFINED IN BOXES SHEET ARE HAVE DEFINED SIZES
  if (isFALSE(identical(stringr::str_sort(as.character(SIZE_INITIAL$BOXES_ID)),
                        stringr::str_sort(list_BOXES_master)))){
    rlang::abort("The list of boxes with defined sizes (FLUXES sheet) does not match the list of boxes (BOXES sheet).
                 Please fix this error in the 0_ISOBXR_MASTER.xlsx")
  }

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
    INITIAL <- dplyr::full_join(INITIAL, FORCING_DELTA, by = "BOXES_ID")
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

  #### REMIND USER WHAT ARE THE INFINITE BOXES

  if (HIDE_PRINTS == F){
    if (is.na(INFINITE_BOXES[1]) == F){
      CONNECTED_INFINITE_BOXES <- INITIAL[INITIAL$FLUX_BALANCE != 0 & INITIAL$BOXES_ID %in% INFINITE_BOXES, "BOXES_ID"]
      if (length(CONNECTED_INFINITE_BOXES) != 0){
        rlang::inform(message = paste("\U2013 The INFINITE boxes are: ", paste(CONNECTED_INFINITE_BOXES,  collapse = ", "), sep = ""))
      }  else {
        rlang::inform(message = paste("\U2013 All boxes are FINITE", sep = ""))
      }
    } else {
      rlang::inform(message = paste("\U2013 All boxes are FINITE", sep = ""))
    }
  }

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
      UNBAL_FINITE_BOXES <- c(UNBAL_FINITE_BOXES, as.character(INITIAL[i,"BOXES_ID"]))
      if (HIDE_PRINTS == F){
        if (INITIAL[i, "FLUX_BALANCE"] < 0){
          rlang::inform(message = paste("\U2013 ", INITIAL[i,"BOXES_ID"]," IN-OUT BALANCE ",
                                        "is negative (max run: ", - INITIAL[i,"SIZE_INIT"]/INITIAL[i,"FLUX_BALANCE"], " t units)", sep = ""))
        } else {
          rlang::inform(message = paste("\U2013 ", INITIAL[i,"BOXES_ID"]," IN-OUT BALANCE ",
                                        "is positive", sep = ""))
        }
      }
    }
    i <- i + 1
  }

  if (HIDE_PRINTS == F){
    if (NUM_ANA == "NUM"){
      rlang::inform(message = paste("\U2013 Running num_slvr (unbalanced finite boxes)", sep = ""))
    } else {
      rlang::inform(message = paste("\U2013 Running ana_slvr (balanced finite boxes)", sep = ""))
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
                        SERIES_ID = SERIES_ID,
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

  # if (dir.exists(outdir) == FALSE){
  #   dir.create(outdir)
  # }

  if (dir.exists(to_tmpdir(outdir)) == FALSE){
    dir.create(to_tmpdir(outdir))
  }

  output_list <- c(output_list, outdir)

  #************************************** DEFINE RUN OUTDIR and RUN ID #----
  ### DEFINE RUN number in the list of a given series, RUN_ID, SERIES_RUN_ID
  n_zeros <- 4
  if (!file.exists(dir_LOG)){
    if (!file.exists(to_tmpdir(dir_LOG))){
      LOG_loc$RUN_n <- 1
    } else {
      LOG <- data.table::fread(to_tmpdir(dir_LOG), data.table = F, stringsAsFactors = T)
      if (SERIES_ID %in% levels(LOG$SERIES_ID)){
        LOG_loc$RUN_n <- max(LOG[LOG$SERIES_ID == SERIES_ID, "RUN_n"])+1
      } else {
        LOG_loc$RUN_n <- 1
      }
      remove(LOG)
    }

  } else {
    if(isFALSE(COMPOSITE) & isFALSE(EXPLORER)){
      LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
      file.copy(from = dir_LOG, to = to_tmpdir(dir_LOG))
      if (SERIES_ID %in% levels(LOG$SERIES_ID)){
        LOG_loc$RUN_n <- max(LOG[LOG$SERIES_ID == SERIES_ID, "RUN_n"])+1
      } else {
        LOG_loc$RUN_n <- 1
      }
      remove(LOG)
    } else {
      if (!file.exists(to_tmpdir(dir_LOG))){
        LOG_loc$RUN_n <- 1
      } else {
        LOG <- data.table::fread(to_tmpdir(dir_LOG), data.table = F, stringsAsFactors = T)
        if (SERIES_ID %in% levels(LOG$SERIES_ID)){
          LOG_loc$RUN_n <- max(LOG[LOG$SERIES_ID == SERIES_ID, "RUN_n"])+1
        } else {
          LOG_loc$RUN_n <- 1
        }
        remove(LOG)
      }
    }
  }
  LOG_loc$RUN_ID <- paste(c(as.character(replicate(n_zeros-length(unlist(strsplit(as.character(LOG_loc$RUN_n), ""))),0)), as.character(LOG_loc$RUN_n)), collapse = "")
  LOG_loc$SERIES_RUN_ID <- paste(SERIES_ID, LOG_loc$RUN_ID, sep = "_")

  #### CREATE A FILE WITH RUN ID FOR EACH RUN placed in SERIES FOLDER
  folder_outdir <- paste(outdir, SERIES_ID, "_", as.character(LOG_loc$RUN_ID), "_", sep = "")
  SERIES_ID_RUN_ID <- paste(SERIES_ID, "_", as.character(LOG_loc$RUN_ID), sep = "")
  LOG_loc$path_outdir = folder_outdir

  # CREATE folder OUTPUT directory for xlsx, csv, and pdf ouputs if necessary
  if (any(isTRUE(to_DIGEST_CSV_XLS), isTRUE(to_DIGEST_DIAGRAMS), isTRUE(to_DIGEST_evD_PLOT))){
    if (!dir.exists(to_tmpdir(paste(folder_outdir, "DIGEST/", sep = "")))){
      dir.create(to_tmpdir(paste(folder_outdir, "DIGEST/", sep = "")))
    }
  }

  output_list <- c(output_list, folder_outdir)

  #************************************** UPDATE TOTAL RUN TIME DEPENDING ON MOST UNBALANCED BOX (incl. "INFINITE") #----
  if (length(BOX_META_to_xls[BOX_META_to_xls$t_lim_run > 0, "t_lim_run"]) > 0){
    MIN_POS_t_lim_run <- min(BOX_META_to_xls[BOX_META_to_xls$t_lim_run > 0, "t_lim_run"])
    MIN_POS_t_lim_run_BOX <- as.character(BOX_META_to_xls[BOX_META_to_xls$t_lim_run == MIN_POS_t_lim_run, "BOXES_ID"])
  } else {
    MIN_POS_t_lim_run <- t_lim
  }

  if (t_lim > MIN_POS_t_lim_run){
    CONSTS[CONSTS$CONSTS_ID == "time", "CONSTS"] <- as.character(MIN_POS_t_lim_run)
    # rlang::warn(message = paste("Updated total run duration. Total run time has been changed from ",
    #                             as.character(t_lim), " to ", as.character(MIN_POS_t_lim_run),
    #                             " (limiting box: ", MIN_POS_t_lim_run_BOX, ")" , sep = ""))
    rlang::inform(message = paste("\U2757 ",
                                "Updated total run duration. Total run time has been changed from ",
                                as.character(t_lim), " to ", as.character(MIN_POS_t_lim_run),
                                " (limiting box: ", MIN_POS_t_lim_run_BOX, ")" , sep = ""))
    LOG_loc$T_LIM <- MIN_POS_t_lim_run
    CONSTS_trad <- CONSTS
  }

  #************************************** EDIT INPUT FILE FOR CURRENT RUN #----

  # EXPORTED AS XLSX OPTIONAL
  if (isTRUE(to_DIGEST_CSV_XLS)){
    trad_excel_path <-  paste(folder_outdir, "DIGEST/", "in_0_INPUTS_", SERIES_ID_RUN_ID, ".xlsx", sep = "")
    writexl::write_xlsx(list(CONSTS = CONSTS_trad,
                             INITIAL = INITIAL_trad,
                             FLUXES = FLUXES_trad,
                             COEFFS = COEFFS_trad,
                             BOX_META = BOX_META_to_xls),
                        to_tmpdir(trad_excel_path))

    output_list <- c(output_list, trad_excel_path)
  }


  # SAVE AS ...IN.Rda
  CONSTS_IN <- CONSTS_trad
  INITIAL_IN <- INITIAL_trad
  FLUXES_IN <- FLUXES_trad
  COEFFS_IN <- COEFFS_trad
  BOX_META_IN <- BOX_META_to_xls

  trad_rda_path <-  paste(folder_outdir, "IN.Rda", sep = "")
  save(CONSTS_IN,
       INITIAL_IN,
       FLUXES_IN,
       COEFFS_IN,
       BOX_META_IN,
       file = to_tmpdir(trad_rda_path))
  output_list <- c(output_list, trad_rda_path)

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

  #### EDIT NETWORK DIAGRAM PDF
  if (to_DIGEST_DIAGRAMS == T & isFALSE(tuto_mode)){
    pdf_path <- paste(folder_outdir, "DIGEST/", "in_1_DIAG_FLUX_", SERIES_ID_RUN_ID, ".pdf", sep = "")
    output_list <- c(output_list, pdf_path)
    pdf(to_tmpdir(pdf_path), width = 3, height = 3, pointsize = 1, useDingbats=FALSE)
    plot_diagram(input = FLUXES_adj, title = NET_FLUXES_title, matrix_layout = matrix_layout, BOXES_master_loc = BOXES_master_loc, COEFF_FLUX = "FLUX")
    dev.off()

    pdf_path <- paste(folder_outdir, "DIGEST/", "in_2_DIAG_COEFF_", SERIES_ID_RUN_ID, ".pdf", sep = "")
    output_list <- c(output_list, pdf_path)
    pdf(to_tmpdir(pdf_path), width = 3, height = 3, pointsize = 1, useDingbats=FALSE)
    plot_diagram(input = COEFFS_adj, title = NET_COEFFS_title, matrix_layout = matrix_layout, BOXES_master_loc = BOXES_master_loc, COEFF_FLUX = "COEFF")
    dev.off()
  }

  #************************************** UPDATE LOG DATA FRAME, EDIT CSV #----
  # if (!file.exists(to_tmpdir(dir_LOG))){
  #   data.table::fwrite(LOG_loc, file = to_tmpdir(dir_LOG), row.names = F, quote = F)
  # } else {
  #   data.table::fwrite(LOG_loc, file = to_tmpdir(dir_LOG), row.names = F, quote = F, append = T)
  # }
  data.table::fwrite(LOG_loc, file = to_tmpdir(dir_LOG), row.names = F, quote = F, append = T)
  output_list <- c(output_list, dir_LOG)

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# RUN ISOBOXr #----
  input_path <- paste(folder_outdir, "IN.Rda", sep = "")

  if (isTRUE(to_DIGEST_CSV_XLS)){
    to_DIGEST_csv = TRUE
  } else {
    to_DIGEST_csv = FALSE
  }

  if (LOG_loc$NUM_ANA == "ANA"){
    ana_slvr(to_tmpdir(input_path), to_DIGEST_csv = to_DIGEST_csv, save_run_outputs = TRUE)
  } else {
    if (LOG_loc$NUM_ANA == "NUM"){
      num_slvr(to_tmpdir(input_path), to_DIGEST_csv = to_DIGEST_csv, save_run_outputs = TRUE)
    }
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# POST-RUN OUTPUTS

  if (to_DIGEST_evD_PLOT == TRUE | plot_results == TRUE){

    load(paste(to_tmpdir(folder_outdir), "OUT.Rda", sep = ""))

    #************************************** EXTRACT or COMPUTE TIME DEPENDENT EVOLUTION OF DELTA VALUES #----
    if (LOG_loc$NUM_ANA == "ANA"){ #### OFFLINE CALCULATION of EV D for ANA using EigenVec/Vals/Coeffs
      evD <- A_evD
      t_lim <- LOG_loc$T_LIM
      nb_steps <- LOG_loc$N_STEPS
      ratio_standard <- CONSTANTS$RATIO_STANDARD
      BOXES_IDs <- names(evD[,-which(names(evD) %in% c("Time"))])
      S <- as.data.frame(t(A_OUT[, c("SIZE_INIT")]))
      names(S) <- as.character(A_OUT$BOXES_ID)
      evS <- dplyr::bind_rows(replicate(nrow(evD), S, simplify = FALSE))
      evS$Time <- evD$Time
      evS <- evS[,c("Time", names(S))]
    } else {
      if (LOG_loc$NUM_ANA == "NUM"){ #### READ ISOPYBOX_NUM evD already Computed
        evD <- N_evD
        evS <- N_evS
        t_lim = LOG_loc$T_LIM
        nb_steps = LOG_loc$N_STEPS
        ratio_standard <- CONSTANTS$RATIO_STANDARD
        BOXES_IDs = names(evD[,-which(names(evD) %in% c("Time"))])
      }
    }

    #************************************** CREATE evD PLOTS #----
    ##### LOCAL PLOT ARGUMENTS
    initial_time_unit <- time_units[1]
    display_time_unit <- time_units[2]

    #### VERTICALIZE evD
    evD_vert <- DF_verticalizer(df_hor = evD, vert_col = BOXES_IDs)
    evD_vert <- time_converter(dataframe = evD_vert, time_colname = "Time",
                               conv_timecolname = "Time_conv",
                               former_unit = initial_time_unit,
                               new_unit = display_time_unit)
    evD_vert$Time <- evD_vert$Time_conv

    evS_vert <- DF_verticalizer(df_hor = evS, vert_col = BOXES_IDs)
    evS_vert <- time_converter(dataframe = evS_vert, time_colname = "Time",
                               conv_timecolname = "Time_conv",
                               former_unit = initial_time_unit,
                               new_unit = display_time_unit)
    evS_vert$Time <- evS_vert$Time_conv

    Ymin <- round(min(evD_vert$VAR), 0)-1
    Ymax <- round(max(evD_vert$VAR), 0)+1
    Ymin_zoom <- min(evD_vert$VAR)
    Ymax_zoom <- max(evD_vert$VAR)
    # Ybin <- 0.25
    Ybin <- signif((Ymax-Ymin)/10, digits = 1) # automatic definition of Ybin
    Xmin <- evD_vert[2,"Time"]
    Xmax <- max(evD_vert$Time) + 0.5*max(evD_vert$Time)

    # if (is.na(INFINITE_BOXES[1]) == F){
    #   evD_vert <- evD_vert[-which(evD_vert$VAR_TYPE %in% c(INFINITE_BOXES)),]
    # }

    BOXES_network_drop <- as.character(BOXES_network_drop)
    # BOXES_network_drop <- BOXES_network_drop[!BOXES_network_drop %in% INFINITE_BOXES]

    if (length(BOXES_network_drop) > 0){
      evD_vert <- evD_vert[-which(evD_vert$VAR_TYPE %in% c(BOXES_network_drop)),]
      evS_vert <- evS_vert[-which(evS_vert$VAR_TYPE %in% c(BOXES_network_drop)),]
    }

    # evD_vert <- clear_subset(evD_vert[2:nrow(evD_vert),])
    evD_initial <- evD_vert[evD_vert$Time == min(evD_vert$Time),]
    evD_final <- evD_vert[evD_vert$Time == max(evD_vert$Time),]

    # evS_vert <- clear_subset(evS_vert[2:nrow(evS_vert),])
    evS_initial <- evS_vert[evS_vert$Time == min(evS_vert$Time),]
    evS_final <- evS_vert[evS_vert$Time == max(evS_vert$Time),]

    evD_plot <- ggplot2::ggplot(data = evD_vert, ggplot2::aes(x = Time, y = VAR, color = VAR_TYPE))+
      ggplot2::geom_line(cex = 1)+
      ggplot2::theme_bw()+
      ggplot2::scale_y_continuous(limits=c(Ymin, Ymax), breaks=seq(Ymin, Ymax, by = Ybin), labels = dec_2)+
      ggplot2::coord_cartesian(ylim = c(Ymin_zoom, Ymax_zoom), xlim = c(Xmin, Xmax))+
      ggplot2::labs(y = paste("d", CONSTANTS$NUMERATOR, "/", CONSTANTS$DENOMINATOR, CONSTANTS$ELEMENT, sep = ""),
                    x = paste("Time in", display_time_unit, sep = " "),
                    title = paste(SERIES_ID, "-", LOG_loc$RUN_ID, ": ", LOG_loc$COEFF_FLUX, ", ", LOG_loc$NUM_ANA, sep = ""),
                    subtitle = NET_COEFFS_title)+
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size=7),
                     legend.position = "None")+
      ggrepel::geom_text_repel(data = evD_final, ggplot2::aes(label = paste(VAR_TYPE, " (", dec_2(VAR), ")", sep = ""), color = VAR_TYPE), nudge_x = 0.05*max(evD_vert$Time),  hjust = 0)

    if (evD_PLOT_time_as_log == T){
      evD_plot <- evD_plot + ggplot2::scale_x_log10()
    }

    evS_plot <- ggplot2::ggplot(data = evS_vert, ggplot2::aes(x = Time, y = VAR, color = VAR_TYPE))+
      ggplot2::geom_line(cex = 1)+
      ggplot2::theme_bw()+
      ggplot2::facet_wrap(VAR_TYPE~., scales = "free_y")+
      # ggplot2::scale_y_continuous(limits=c(Ymin, Ymax), breaks=seq(Ymin, Ymax, by = Ybin), labels = dec_2)+
      # ggplot2::coord_cartesian(ylim = c(Ymin_zoom, Ymax_zoom), xlim = c(Xmin, Xmax))+
      ggplot2::labs(y = paste("mass of ", CONSTANTS$ELEMENT, sep = ""),
                    x = paste("Time in", display_time_unit, sep = " "),
                    title = paste("Evolution of box sizes (mass of ", CONSTANTS$ELEMENT, ")", sep = ""))+
      ggplot2::theme(plot.subtitle = ggplot2::element_text(size=7),
                     legend.position = "None")
    # ggrepel::geom_text_repel(data = evS_final, ggplot2::aes(label = paste(VAR_TYPE, " (", ttboxR::dec_0(VAR), ")", sep = ""), color = VAR_TYPE), nudge_x = 0.05*max(evS_vert$Time),  hjust = 0)+


    if (evD_PLOT_time_as_log == T){
      evS_plot <- evS_plot + ggplot2::scale_x_log10()
    }
  }

  #### EXPORT PLOT
  if (isTRUE(to_DIGEST_evD_PLOT) & isFALSE(tuto_mode)){
    pdf_path <- paste(folder_outdir, "DIGEST/", "out_0_PLOT_evD_", SERIES_ID_RUN_ID, ".pdf", sep = "")
    output_list <- c(output_list, pdf_path)
    dev.new()
    pdf(to_tmpdir(pdf_path), width = 10, height = 10, pointsize = 1, useDingbats=FALSE)
    suppressWarnings(multiplot(evD_plot, evS_plot, cols = 1))
    graphics.off()
  }
  # ##### PRINT PLOTs
  # if (isTRUE(plot_results)){
  #   suppressWarnings(multiplot(evD_plot, evS_plot, cols = 1))
  # }

  #----#----#----#----#----#----#----#----#----#---- save_run_outputs or not #----
  if(isFALSE(COMPOSITE) & isFALSE(EXPLORER)){
    rlang::inform("________________________________________________________________________________")
    rlang::inform(message = paste("\U2139 Run outputs (stored in temporary directory):",
                                  sep = ""))
    fs::dir_tree(path = to_tmpdir(""), recurse = T)
    rlang::inform("________________________________________________________________________________")
    if(isFALSE(save_run_outputs)){
      rlang::inform("\U2757 Results were not exported to working directory (set save_run_outputs = TRUE to save results).")
    } else if(isTRUE(save_run_outputs)){
      R.utils::copyDirectory(to_tmpdir(""),
                             getwd(),
                             overwrite = T)
      rlang::inform("\U2705 Results were successfully exported to working directory.")
    }
  }

  ##### PRINT PLOTs
  if (isTRUE(plot_results)){
    return(suppressWarnings(multiplot(evD_plot, evS_plot, cols = 1)))
  }
}
