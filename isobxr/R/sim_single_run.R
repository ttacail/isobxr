#' @importFrom grDevices dev.list dev.new dev.off graphics.off pdf rainbow
NULL

#' @importFrom utils head read.csv
NULL

#  #_________________________________________________________________________80char
#' Run a single isotope box-model simulation
#' @description A function to run the isobxr stable isotope box model,
#' assessing the design of the model and automatically running \code{\link{solve_numerically}}
#' or \code{\link{solve_analytically}} depending on system design.
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' (character string)
#' @param SERIES_ID Name of the series the run belongs to. \cr
#' It determines the folder in which the output files will be stored inside workdir.\cr
#' (character string)
#' @param flux_list Name of the list of fluxes and initial box sizes to be used for the run, \cr
#' calling (by its header name) a single column of the \strong{FLUXES} sheet
#' of the \strong{\emph{isobxr excel master file}}. \cr
#' (character string)
#' @param coeff_list Name of the list of fractionation coefficients to be used for the run, \cr
#' calling (by its header name) a single column of the \strong{COEFFS} sheet
#' of the \strong{\emph{isobxr excel master file}}. \cr
#' (character string)
#' @param t_max Run duration, given in the same time units as unit declared in \strong{CONSTANTS} \cr
#' spreadsheet of \strong{\emph{isobxr excel master file}} in the \strong{TIME_UNIT} column.
#' (integer)
#' @param n_steps Number of calculation steps. \cr
#' It determines the resolution of the run. \cr
#' (integer)
#' @param isobxr_master_file Name of \strong{\emph{isobxr excel master file}}. \cr
#' Default is "0_ISOBXR_MASTER".
#' @param suppress_messages If TRUE, hides all information and warning messages regarding run. \cr
#' Default is FALSE.
#' @param export.diagrams If TRUE, exports box-model flux and fractionation diagrams as pdf. \cr
#' Default is FALSE.
#' @param export.delta_plot If TRUE, exports delta and size time evolution plots of the evolution
#' of the system, as pdf. \cr
#' Default is FALSE.
#' @param export.data_as_csv_xlsx If TRUE, exports all results and run conditions as csv and xlsx files, \cr
#' to DIGEST directory. \cr
#' Default is FALSE.
#' @param plot.time_as_log10 If TRUE, uses logarithmic time scale in plot. \cr
#' Default is FALSE.
#' @param plot.time_unit Time unit to use on plot if different from native time unit. \cr
#' Character string, to be selected among the following:\cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}\cr
#' Default is NULL.
#' @param show.delta_plot If TRUE, prints delta and size time evolution plots in R.
#' Default is TRUE.
#' @param inspect_inputs If TRUE, inspects and proof checks format of input taken from
#' \strong{\emph{isobxr excel master file}}. \cr
#' (Inspection run by \code{\link{read.isobxr_master}} function.) \cr
#' Default is TRUE.
#' @param save_outputs If TRUE, saves all run outputs to local working directory (workdir). \cr
#' By default, run outputs are stored in a temporary directory and erased if not saved. \cr
#' Default is FALSE.
#' @param return_data If TRUE, returns all data (inputs and outputs) as a list. \cr
#' Default is FALSE.
#' @param solver Determines what solver to used: "analytical" or "numerical". \cr
#' Default is "auto" for automatic selection of adapted solver.
#' Note that this option returns warnings or prevents user to run when solver wished is
#' not adapted to system solution.
#' @param n_zeros_RUN_IDs Number of figures used in iteration of RUNs of a given series (SERIES_ID). \cr
#' Default is 4: the run IDs of a given series range between 0001 and 9999.
#' @param FORCING_RAYLEIGH \emph{OPTIONAL} \cr
#' Dataframe describing the forcing on a fractionation coefficient
#' by a Rayleigh isotope distillation, \cr
#' as a function of flux intensities and a fundamental fractionation coefficient. \cr
#' Dataframe formatting details are in isobxr vignette. \cr
#' Default is NULL.
#' @param FORCING_SIZE \emph{OPTIONAL} \cr
#' Dataframe describing the forcing on one or several box sizes (mass of element X). \cr
#' The newly defined sizes for the given set of boxes
#' overwrite their sizes as previously defined in \strong{\emph{isobxr excel master file}}. \cr
#' Dataframe formatting details are in isobxr vignette. \cr
#' Default is NULL.
#' @param FORCING_DELTA \emph{OPTIONAL} \cr
#' Dataframe describing the forcing on one or several boxes
#' initial isotope composition expressed as delta values. \cr
#' The newly defined delta values for the given set of boxes
#' overwrite the delta values as previously defined in \strong{\emph{isobxr excel master file}}. \cr
#' Dataframe formatting details are in isobxr vignette. \cr
#' Default is NULL.
#' @param FORCING_ALPHA \emph{OPTIONAL} \cr
#' Dataframe describing the forcing on one or several
#' fractionation coefficients from one reservoir to another. \cr
#' The newly defined alpha values for the given set of boxes
#' overwrite the alpha values as previously defined in \strong{\emph{isobxr excel master file}}. \cr
#' Dataframe formatting details are in isobxr vignette. \cr
#' Default is NULL.
#' @param COMPOSITE \emph{NOT TO BE USED IN SINGLE RUN}  \cr
#' Logical value automatically defined in \code{\link{sim.scenario}}. \cr
#' Default is FALSE.
#' @param COMPO_SERIES_n \emph{NOT TO BE USED IN SINGLE RUN}  \cr
#' Iteration of the composite run for the given series it belongs to,
#' automatically defined in \code{\link{sim.scenario}}.  \cr
#' Default is NaN.
#' @param COMPO_SERIES_FAMILY \emph{NOT TO BE USED IN SINGLE RUN} \cr
#' Composite run series family, automatically defined in
#' \code{\link{sim.scenario}}. \cr
#' Default is NaN.
#' @param EXPLORER \emph{NOT TO BE USED IN SINGLE RUN} \cr
#' Logical value automatically defined in \code{\link{sweep.final_nD}}
#' or \code{\link{sweep.dyn_2D}}. \cr
#' Default is FALSE.
#' @param EXPLO_SERIES_n \emph{NOT TO BE USED IN SINGLE RUN}  \cr
#' Iteration of the sweep run for the given series it belongs to,
#' automatically defined in \code{\link{sweep.final_nD}} or \code{\link{sweep.dyn_2D}}. \cr
#' Default is NaN.
#' @param EXPLO_SERIES_FAMILY \emph{NOT TO BE USED IN SINGLE RUN}  \cr
#' Sweep run series family, automatically defined in
#' \code{\link{sweep.final_nD}} or \code{\link{sweep.dyn_2D}}. \cr
#' Default is NaN.
#' @param isobxr_master isobxr_master list of input dataframes formatted by
#' \code{\link{read.isobxr_master}} \cr
#' Overwrites isobxr_master_file.
#' Default is NULL.
#' @param diagram_pdf.widh_height Vector of width and height in inches of the pdf diagrams.
#'
#' @return A results data set as a list containing the following components:
#' \enumerate{
#' \item \strong{inputs} input data:
#' \enumerate{
#' \item \strong{CONSTS} data frame of all run specific constants.
#' \item \strong{INITIAL} data frame of delta and sizes at t = 0 in all boxes.
#' \item \strong{FLUXES} data frame of all fluxes intensities (row ID: FROM / col ID: TO)
#' \item \strong{COEFFS} data frame of all fractionation coefficient values (row ID: FROM / col ID: TO)
#' \item \strong{BOX_META} data frame of box specific metadata (e.g., flux balance, residence times, layout position )
#' \item \strong{bx.groups} list of box names grouped by relevant categories (e.g., disconnected boxes, infinite boxes)
#' \item \strong{LOG} data frame of run specific LOG excerpt.
#' }
#' \item \strong{outputs} output data:
#' \enumerate{
#' \item \strong{solver}
#' \item \strong{final_state}
#' \item \strong{delta_vs_t}
#' \item \strong{size_vs_t}
#' \item \strong{for analytical solutions}
#' \enumerate{
#' \item \strong{diffeq_solutions} solutions of differential equations, \cr
#' including relaxation times, eigenvalues, constants, eigenvectors
#' }
#' }
#' \item \strong{paths} list of run specific paths
#'
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' sim.single_run(workdir = "/Users/username/Documents/1_ABC_tutorial",
#'                SERIES_ID = "1_ABC_balanced_closed",
#'                flux_list = "Fx1_ABC_bal",
#'                coeff_list = "a1",
#'                t_max = 2500,
#'                n_steps = 2500)
#'}
sim.single_run <-
  function(workdir,
           SERIES_ID,
           flux_list,
           coeff_list,
           t_max,
           n_steps,
           isobxr_master_file = "0_ISOBXR_MASTER",
           suppress_messages = FALSE,
           export.diagrams = FALSE,
           export.delta_plot = FALSE,
           export.data_as_csv_xlsx = FALSE,
           plot.time_as_log10 = TRUE,
           plot.time_unit = NULL,
           show.delta_plot = TRUE,
           inspect_inputs = TRUE,
           save_outputs = FALSE,
           return_data = FALSE,
           solver = "auto",
           n_zeros_RUN_IDs = 4,
           FORCING_RAYLEIGH = NULL,
           FORCING_SIZE = NULL,
           FORCING_DELTA = NULL,
           FORCING_ALPHA = NULL,
           COMPOSITE = FALSE,
           COMPO_SERIES_n = NaN,
           COMPO_SERIES_FAMILY = NaN,
           EXPLORER = FALSE,
           EXPLO_SERIES_n = NaN,
           EXPLO_SERIES_FAMILY = NaN,
           isobxr_master = NULL,
           diagram_pdf.widh_height = NULL) {

  # ########################################################
  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list = ls())
  # gc()
  # devtools::load_all(".")
  # ########################################################
  # isobxr_master_file = "0_ISOBXR_MASTER"
  # suppress_messages = FALSE
  # export.diagrams = FALSE
  # export.delta_plot = FALSE
  # export.data_as_csv_xlsx = FALSE
  # plot.time_as_log10 = TRUE
  # plot.time_unit = NULL
  # show.delta_plot = TRUE
  # inspect_inputs = TRUE
  # save_outputs = FALSE
  # return_data = FALSE
  # solver = "auto"
  # n_zeros_RUN_IDs = 4
  # FORCING_RAYLEIGH = NULL
  # FORCING_SIZE = NULL
  # FORCING_DELTA = NULL
  # FORCING_ALPHA = NULL
  # COMPOSITE = FALSE
  # COMPO_SERIES_n = NaN
  # COMPO_SERIES_FAMILY = NaN
  # EXPLORER = FALSE
  # EXPLO_SERIES_n = NaN
  # EXPLO_SERIES_FAMILY = NaN
  # ########################################################
  #
  # workdir_ABCD <- paste("/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/",
  #                       "1_ABCD_dev", sep = "")
  # workdir = workdir_ABCD
  # SERIES_ID = "3_ABC_balanced_open"
  # flux_list = "Fx6_ABC_open_bal"
  # coeff_list = "a0"
  # t_max = 10000
  # n_steps = 1000
  # plot.time_unit = "d"
  # FORCING_DELTA = data.frame(BOX_ID = c("SINK"),
  #                            DELTA.t0 = c(-1))
  # export.delta_plot = T
  # plot.time_as_log10 = T
  # export.diagrams = T
  # export.data_as_csv_xlsx = T
  # save_outputs = T
  # return_data = T
  #
  ########################################################
  # I. check arguments ####
  args <- c(as.list(environment()))
  rm(list=ls()[ls() != "args"])

  if (!is.logical(args$inspect_inputs)) rlang::abort("\"inspect_input\" argument should be logical.")

  if (args$inspect_inputs){
    args.allowed <- list(solver = c("auto", "analytical", "numerical"),
                         logical = c("suppress_messages",
                                     "export.diagrams",
                                     "export.delta_plot",
                                     "export.data_as_csv_xlsx",
                                     "plot.time_as_log10",
                                     "show.delta_plot",
                                     "save_outputs",
                                     "return_data",
                                     "COMPOSITE",
                                     "EXPLORER"))

    for (i in 1:length(args.allowed$logical)){
      if (!is.logical(eval(parse(text = paste0("args$", args.allowed$logical[i]))))){
        rlang::abort(paste0("\"", args.allowed$logical[i], "\" argument should be logical."))
      }
    }

    if (!args$solver %in% args.allowed$solver){
      rlang::abort(paste0("The solver argument can only take one of the following value: ",
                          paste(as.character(args.allowed$solver), collapse = ", ")))
    }
  }

  Y_COORD <- X_COORD <- ORDER <- BOX_ID <- prefix <- func <- NULL

  # II. initiate ####
  # _a. set workdir ####
  # __i. determine function mode (tuto/user) ####
  fun_mode <- using_extdata_tutorial_2(args$workdir, args$save_outputs, args$show.delta_plot)
  paths <- list(workdir = fun_mode$workdir)
  if (fun_mode$tuto_mode) args$isobxr_master_file <- "0_ISOBXR_MASTER.xlsx"
  paths$isobxr_master_file <- args$isobxr_master_file
  # args$show.delta_plot <- fun_mode$plot_results
  args$save_outputs <- fun_mode$save_outputs

  # __ii. set workdir ####
  workdir_old <- getwd()
  on.exit(setwd(workdir_old), add = TRUE)
  setwd(paths$workdir)

  if(!args$COMPOSITE & !args$EXPLORER){
    unlink(to_tmpdir(""), recursive = TRUE)
    on.exit(unlink(to_tmpdir(""), recursive = TRUE), add = TRUE)
    if(!args$suppress_messages){
      rlang::inform("________________________________________________________________________________")
      if (fun_mode$tuto_mode){
        rlang::inform(paste("\U2139 workdir: no workdir.
  You are using the tutorial mode (isobxr embedded tutorial files).
  The default outputs are limited and won't be saved.", sep = ""))
      } else {
        rlang::inform(paste("\U2139 workdir: ", getwd(), sep = ""))
      }
      rlang::inform("________________________________________________________________________________")
    }
  }

  # # _b. locally bind variables ####
  # # (fixing binding global variable issue)
  # Time <- VAR <- VAR_TYPE <- NULL

  # III. read inputs, prepare inputs ####
  # _a. read isobxr master file ####
  if (is.null(args$isobxr_master)){
    if (!args$COMPOSITE & !args$EXPLORER){
      master <- read.isobxr_master(workdir = args$workdir,
                                   isobxr_master_file = args$isobxr_master_file,
                                   inspect = args$inspect_inputs,
                                   export_rds = F)
    } else {
      master <- readRDS(paste0(args$workdir, "/", args$isobxr_master_file, ".rds"))
    }
  } else {
    master <- args$isobxr_master
  }

  master$CONSTANTS$t_max <- args$t_max
  master$CONSTANTS$n_steps <- args$n_steps

  bx.groups <- master$bx.groups

  # _b. check list names ####
  if (args$inspect_inputs){
    if(!args$flux_list %in% names(master$FLUXES)){
      rlang::abort(paste0("Cannot find \"", args$flux_list, "\" in fluxes lists defined in FLUXES." , "\n",
                          "Found in master file: \n ",
                          paste(names(master$FLUXES)[!names(master$FLUXES) %in% c("FROM", "TO", "SIZE_or_FLUX", "BOX_ID")],
                                collapse = "\n")))
    }

    if(!args$coeff_list %in% names(master$COEFFS)){
      rlang::abort(paste0("Cannot find \"", args$coeff_list, "\" in fractionation coefficients lists defined in COEFFS.", "\n",
                          "Found in master file: \n",
                          paste(names(master$COEFFS)[!names(master$COEFFS) %in% c("FROM", "TO", "SIZE_or_FLUX", "BOX_ID")],
                                collapse = "\n")))
    }
  }

  # _c. set INITIAL state ####
  INITIAL <- master$FLUXES[master$FLUXES$SIZE_or_FLUX == "SIZE", c("BOX_ID", args$flux_list)] %>%
    clear_subset() %>%
    dplyr::arrange(BOX_ID)

  names(INITIAL) <- c("BOX_ID", "SIZE.t0")

  # __i. set SIZE.t0 ####
  if (!is.null(args$FORCING_SIZE)){
    if (args$inspect_inputs) check_FORCINGS("SIZE", args$FORCING_SIZE, master)
    INITIAL <- INITIAL %>%
      dplyr::filter(! BOX_ID %in% args$FORCING_SIZE$BOX_ID) %>%
      dplyr::bind_rows(args$FORCING_SIZE) %>%
      dplyr::arrange(BOX_ID)
  }

  # __ii. set DELTA.t0 ####
  if (!is.null(args$FORCING_DELTA)){
    if (args$inspect_inputs) check_FORCINGS("DELTA", args$FORCING_DELTA, master)
    INITIAL <- dplyr::full_join(INITIAL, args$FORCING_DELTA, by = "BOX_ID")
    INITIAL[is.na(INITIAL)] = 0
  } else {
    INITIAL$DELTA.t0 <- 0
  }

  # _d. edit matrices ####
  # __i. edit flux matrix ####
  FLUXES.loc <- master$FLUXES[master$FLUXES$SIZE_or_FLUX == "FLUX",
                              c("FROM", "TO", args$flux_list)]
  FLUXES.matrix <- as.data.frame(matrix(data = 0,
                                        nrow = bx.groups$all.n,
                                        ncol = bx.groups$all.n+1))
  colnames(FLUXES.matrix) <- c("BOX_ID", bx.groups$all)
  FLUXES.matrix$BOX_ID <- as.factor(bx.groups$all)

  for (i in 1:nrow(FLUXES.loc)){
    FROM_loc <- as.character(FLUXES.loc[i, "FROM"])
    TO_loc <- as.character(FLUXES.loc[i, "TO"])
    FLUXES.matrix[FLUXES.matrix$BOX_ID == FROM_loc, TO_loc] <- FLUXES.loc[i, args$flux_list]
  }

  # __ii. edit coeff matrix ####
  COEFFS.loc <- master$COEFFS[, c("FROM", "TO", args$coeff_list)]
  names(COEFFS.loc) <- c("FROM", "TO", args$coeff_list)
  COEFFS.matrix <- as.data.frame(matrix(data = 1,
                                        nrow = bx.groups$all.n,
                                        ncol = bx.groups$all.n+1))
  colnames(COEFFS.matrix) <- c("BOX_ID", bx.groups$all)
  COEFFS.matrix$BOX_ID <- as.factor(bx.groups$all)

  for (i in 1:nrow(COEFFS.loc)){
    FROM_loc <- as.character(COEFFS.loc[i, "FROM"])
    TO_loc <- as.character(COEFFS.loc[i, "TO"])
    COEFFS.matrix[COEFFS.matrix$BOX_ID == FROM_loc, TO_loc] <- COEFFS.loc[i, args$coeff_list]
  }

  # __iii. force alpha ####
  if (!is.null(args$FORCING_ALPHA)){
    if (args$inspect_inputs) check_FORCINGS("ALPHA", args$FORCING_ALPHA, master)
    for (i in 1:nrow(args$FORCING_ALPHA)){
      COEFFS.matrix[COEFFS.matrix$BOX_ID == as.character(args$FORCING_ALPHA[i,"FROM"]),
                    as.character(args$FORCING_ALPHA[i,"TO"])] <- args$FORCING_ALPHA[i, "ALPHA"]
    }
  }

  # __iv. force rayleigh ####
  # FORCING RAYLEIGH DISTILLATION RESULTING FRACTIONATION, CALCULATOR
  if (!is.null(args$FORCING_RAYLEIGH)){
    if(args$inspect_inputs) check_FORCINGS(type = "RAYLEIGH", FORCING =  args$FORCING_RAYLEIGH, isobxr_master = master)
    for (i in 1:nrow(args$FORCING_RAYLEIGH)){
      FRAC_EXCR <- FLUXES.loc[FLUXES.loc$FROM == as.character(args$FORCING_RAYLEIGH[i,"XFROM"]) &
                                FLUXES.loc$TO == as.character(args$FORCING_RAYLEIGH[i,"XTO"]) , args$flux_list] /
        FLUXES.loc[FLUXES.loc$FROM == as.character(args$FORCING_RAYLEIGH[i,"YFROM"]) &
                     FLUXES.loc$TO == as.character(args$FORCING_RAYLEIGH[i,"YTO"]) , args$flux_list] # FRACTIONAL EXCRETION RATE
      ALPHA_loc <- exp((1000*(args$FORCING_RAYLEIGH[i, "ALPHA_0"]-1)*log(FRAC_EXCR))/1000)
      COEFFS.matrix[COEFFS.matrix$BOX_ID == as.character(args$FORCING_RAYLEIGH[i,"AFROM"]),
                    as.character(args$FORCING_RAYLEIGH[i,"ATO"])] <- ALPHA_loc
    }
    remove(FRAC_EXCR, ALPHA_loc)
  }

  # _e. archive run conditions ####
  # __i. for input excel file ####
  row.names(FLUXES.matrix) <- as.character(0:(nrow(FLUXES.matrix)-1))
  row.names(COEFFS.matrix) <- as.character(0:(nrow(COEFFS.matrix)-1))

  CONSTS_IN <- master$CONSTANTS
  INITIAL_IN <- INITIAL
  FLUXES_IN <- FLUXES.matrix
  COEFFS_IN <- COEFFS.matrix

  # __ii. for plot ####
  #### STORE COEFFS - FLUXES FOR PLOT
  FLUXES <- FLUXES.matrix # .matrix now
  COEFFS <- COEFFS.matrix # .matrix now

  row.names(INITIAL) <- INITIAL$BOX_ID
  row.names(FLUXES) <- FLUXES$BOX_ID # .matrix now
  row.names(COEFFS) <- COEFFS$BOX_ID# .matrix now

  # IV. Analyze cycle structure ####

  # _a. compute balance and res. times ####
  for (i in 1:nrow(FLUXES)){
    INITIAL[bx.groups$all[i], "FLUX_IN"] <- sum(FLUXES[,bx.groups$all[i]])
    INITIAL[bx.groups$all[i], "FLUX_OUT"] <- sum(FLUXES[bx.groups$all[i], bx.groups$all])
  }
  INITIAL$FLUX_BALANCE <- INITIAL$FLUX_IN-INITIAL$FLUX_OUT
  INITIAL$RES_TIME <- INITIAL$SIZE.t0/INITIAL$FLUX_OUT #### !!!! WARNING !!!! DEFINITION FOR A BOX WITH BALANCED IN/OUT FLUXES

  # _b. identify disconnected boxes ####
  bx.groups$disconnected <- INITIAL[INITIAL$FLUX_IN == 0 & INITIAL$FLUX_OUT == 0, "BOX_ID"] %>% as.character()

  # _c. identify infinite boxes ####
  # __i. determine infinite boxes ####
  bx.groups$infinite <- master$BOXES[master$BOXES$INFINITE == "INFINITE", "BOX_ID"] %>%
    as.character()

  # __ii. warn if > 2 inf. bx  ####
  if (args$inspect_inputs){
    if(length(bx.groups$infinite) > 2){
      rlang::inform(paste("\U2757 The modelling of open systems with isobxr best works with no more than 2 infinite boxes. \n",
                          "   You defined more than 2 infinite boxes: [",paste(bx.groups$infinite, collapse = ", "), "] \n",
                          "   The numerical outputs will be accurrate. \n",
                          "   This type of design is however currently not supported by the plot editing shiny app.", sep = ""))
    }
  }

  # __iii. inform user of inf. boxes ####
  if (args$suppress_messages == F){
    if (length(bx.groups$infinite) > 0){
      bx.groups$infinite.connected <- INITIAL[(INITIAL$FLUX_OUT != 0 | INITIAL$FLUX_IN != 0) &
                                                INITIAL$BOX_ID %in% bx.groups$infinite, "BOX_ID"] %>% as.character()
      if (length(bx.groups$infinite.connected) != 0){
        rlang::inform(message = paste("\U2013 The INFINITE boxes are: ", paste(bx.groups$infinite.connected,
                                                                               collapse = ", "), sep = ""))
      }  else {
        rlang::inform(message = paste("\U2013 All boxes are FINITE", sep = ""))
      }
    } else {
      rlang::inform(message = paste("\U2013 All boxes are FINITE", sep = ""))
    }
  }

  # _d. identify finite boxes ####
  if (length(bx.groups$infinite) > 0){
    bx.groups$finite <- INITIAL[-which(INITIAL$BOX_ID %in% bx.groups$infinite), "BOX_ID"] %>% as.character()
  } else {
    bx.groups$finite <- INITIAL[, "BOX_ID"] %>% as.character()
  }

  # _e. analyze system balance ####
  # __i. identify unbalanced boxes ####
  INITIAL[,"t_max_run"] <- - INITIAL[,"SIZE.t0"]/INITIAL[,"FLUX_BALANCE"]
  NUM_ANA = "ANA"
  solver.picked <- "analytical solution"
  bx.groups$finite.unbalanced <- NULL #UNBAL_FINITE_BOXES

  for (i in 1:nrow(INITIAL)){
    if (INITIAL[i,"BOX_ID"] %in% bx.groups$finite & INITIAL[i,"FLUX_BALANCE"] != 0){
      NUM_ANA = "NUM"
      solver.picked <- "numerical solution"
      bx.groups$finite.unbalanced <- c(bx.groups$finite.unbalanced, as.character(INITIAL[i,"BOX_ID"]))
      if (args$suppress_messages == F){
        if (INITIAL[i, "FLUX_BALANCE"] < 0){
          rlang::inform(message = paste("\U2013 ", INITIAL[i,"BOX_ID"]," IN-OUT BALANCE ",
                                        "is negative (max run: ", - INITIAL[i,"SIZE.t0"]/INITIAL[i,"FLUX_BALANCE"], " t units)", sep = ""))
        } else {
          rlang::inform(message = paste("\U2013 ", INITIAL[i,"BOX_ID"]," IN-OUT BALANCE ",
                                        "is positive", sep = ""))
        }
      }
    }
  }

  # __ii. determine recommended solver ####
  if (args$suppress_messages == F){
    if (NUM_ANA == "NUM"){
      rlang::inform(message = paste("\U2013 Running solve_numerically (unbalanced finite boxes)", sep = ""))
    } else {
      rlang::inform(message = paste("\U2013 Running solve_analytically (balanced finite boxes)", sep = ""))
    }
  }

  # __iii. update run duration if required ####
  # depending on most unblalanced box (incl. "INFINITE")
  # build BOX_META_IN
  BOX_META_IN <-  dplyr::full_join(INITIAL, master$BOXES, by = "BOX_ID") %>%
    dplyr::arrange(BOX_ID)

  if (length(BOX_META_IN[BOX_META_IN$t_max_run > 0, "t_max_run"]) > 0){
    MIN_POS_t_max_run <- min(BOX_META_IN[BOX_META_IN$t_max_run > 0, "t_max_run"])
    MIN_POS_t_max_run_BOX <- as.character(BOX_META_IN[BOX_META_IN$t_max_run == MIN_POS_t_max_run, "BOX_ID"])
  } else {
    MIN_POS_t_max_run <- args$t_max
  }

  if (args$t_max > MIN_POS_t_max_run){
    rlang::inform(message = paste("\U2757 ",
                                  "Updated total run duration. Total run time has been changed from ",
                                  as.character(args$t_max), " to ", as.character(MIN_POS_t_max_run),
                                  " (limiting box: ", MIN_POS_t_max_run_BOX, ")" , sep = ""))
    args$t_max <- MIN_POS_t_max_run
    CONSTS_IN$t_max <- master$CONSTANTS$t_max <- MIN_POS_t_max_run
  }

  # V. Edit pre-run outputs ####
  # _a. define coeff_list_desc ####
  coeff_list_desc <- args$coeff_list
  if (!is.null(args$FORCING_ALPHA)){
    coeff_list_desc <- paste(args$coeff_list, "_mod", sep = "")
  }

  # _b. prepare documentation if Rayleigh ####
  if (is.null(args$FORCING_RAYLEIGH)){
    rayleigh_to_LOG <- NaN
  } else {
    args$FORCING_RAYLEIGH$ALPHA_ID <- NaN
    args$FORCING_RAYLEIGH$ALPHA_ID <- paste(args$FORCING_RAYLEIGH$AFROM, args$FORCING_RAYLEIGH$ATO, sep = "t")
    for (i in 1:nrow(args$FORCING_RAYLEIGH)){
      if (i == 1){
        coeff_list_desc <- paste(args$coeff_list, "_mod", sep = "")
        rayleigh_to_LOG <- paste(args$FORCING_RAYLEIGH[i, "XFROM"], "",
                                 args$FORCING_RAYLEIGH[i, "XTO"], "_",
                                 args$FORCING_RAYLEIGH[i, "YFROM"], "",
                                 args$FORCING_RAYLEIGH[i, "YTO"], "_",
                                 args$FORCING_RAYLEIGH[i, "AFROM"], "t",
                                 args$FORCING_RAYLEIGH[i, "ATO"],  "_",
                                 as.character(args$FORCING_RAYLEIGH[i, "ALPHA_0"]), sep = "")
      } else {
        rayleigh_to_LOG_loc <- paste(args$FORCING_RAYLEIGH[i, "XFROM"], "",
                                     args$FORCING_RAYLEIGH[i, "XTO"], "_",
                                     args$FORCING_RAYLEIGH[i, "YFROM"], "",
                                     args$FORCING_RAYLEIGH[i, "YTO"], "_",
                                     args$FORCING_RAYLEIGH[i, "AFROM"], "t",
                                     args$FORCING_RAYLEIGH[i, "ATO"],  "_",
                                     as.character(args$FORCING_RAYLEIGH[i, "ALPHA_0"]), sep = "")
        rayleigh_to_LOG <- paste(rayleigh_to_LOG, rayleigh_to_LOG_loc, sep = "__")
      }
    }
  }

  # _c. initiate LOG file ####
  paths$LOG_file <- "1_LOG.csv"

  LOG_loc <- data.frame(RUN_n = NaN,
                        RUN_ID = NaN,
                        SERIES_RUN_ID = NaN,
                        SERIES_ID = args$SERIES_ID,
                        DATE_TIME = c(chartr(old = "-: ", new = "___", Sys.time())),
                        COEFF_FLUX = c(paste(coeff_list_desc , "__", args$flux_list, sep = "")),
                        FLUX_MASTER = c(args$flux_list),
                        COEFF_MASTER = c(args$coeff_list),
                        COEFF_RUN = c(coeff_list_desc),
                        NUM_ANA = NUM_ANA,
                        T_MAX = args$t_max,
                        N_STEPS = args$n_steps,
                        BOXES_ID_n = bx.groups$all.n,
                        BOXES_ID_list = c(stringr::str_c(bx.groups$all, collapse = "_")),
                        INFINITE_BOXES_list =
                          if(length(bx.groups$infinite) > 0){
                            paste(bx.groups$infinite, collapse = "_")
                          } else { NaN },
                        DISCONNECTED_BOXES =
                          if(length(bx.groups$disconnected) > 0){
                            paste(bx.groups$disconnected, collapse = "_")
                          } else { NaN },
                        UNBAL_FINITE_BOXES =
                          if(length(bx.groups$finite.unbalanced) > 0){
                            paste(bx.groups$finite.unbalanced, collapse = "_")
                          } else { NaN },
                        SIZE.t0 = c(stringr::str_c(as.character(BOX_META_IN[, "SIZE.t0"]), collapse = "_")),
                        DELTA.t0 = c(stringr::str_c(as.character(BOX_META_IN[, "DELTA.t0"]), collapse = "_")),
                        FORCING_RAYLEIGH = rayleigh_to_LOG,
                        FORCING_SIZE = NaN,
                        FORCING_DELTA = NaN,
                        FORCING_ALPHA = NaN,
                        COMPOSITE = args$COMPOSITE,
                        COMPO_SERIES_FAMILY = args$COMPO_SERIES_FAMILY,
                        COMPO_SERIES_n = args$COMPO_SERIES_n,
                        EXPLORER = args$EXPLORER,
                        EXPLO_SERIES_FAMILY = args$EXPLO_SERIES_FAMILY,
                        EXPLO_SERIES_n = args$EXPLO_SERIES_n,
                        path_outdir = NaN)

  if (!is.null(args$FORCING_DELTA)){
    LOG_loc$FORCING_DELTA <-
      paste(as.character(args$FORCING_DELTA$BOX_ID),
            as.character(args$FORCING_DELTA$DELTA.t0),
            collapse = "_", sep = "_")
  }

  if (!is.null(args$FORCING_SIZE)){
    LOG_loc$FORCING_SIZE <-
      paste(as.character(args$FORCING_SIZE$BOX_ID),
            as.character(args$FORCING_SIZE$SIZE.t0), collapse = "_", sep = "_")
  }

  if (!is.null(args$FORCING_ALPHA)){
    LOG_loc$FORCING_ALPHA <-
      paste(as.character(args$FORCING_ALPHA$FROM),
            as.character(args$FORCING_ALPHA$TO),
            as.character(args$FORCING_ALPHA$ALPHA), collapse = "_", sep = "_")
  }

  # _d. define series outdir ####
  paths$SERIES_ID <- args$SERIES_ID

  if (args$COMPOSITE){
    paths$outdir <- paste("3_", args$SERIES_ID, sep = "")
  } else if (args$EXPLORER){
    paths$outdir <- paste("4_", args$SERIES_ID, sep = "")
  } else {
    # paths$outdir <- paste(names.output %>% dplyr::filter(func == "sim.single_run") %>% dplyr::pull(old.prefix),
    #                       "_", args$SERIES_ID, sep = "")
    paths$outdir <- paste(names.output %>% dplyr::filter(func == "sim.single_run") %>% dplyr::pull(prefix),
                          "_", args$SERIES_ID, sep = "")
  }

  # Edit/Create outdir folder and check slash in outdir
  check_slash <- unlist(strsplit(paths$outdir, ""))
  if (check_slash[length(check_slash)] != "/"){
    paths$outdir <- paste(paths$outdir, "/", sep = "")
  }
  remove(check_slash)

  if (!dir.exists(to_tmpdir(paths$outdir))){
    dir.create(to_tmpdir(paths$outdir))
  }

  # _e. define run outdir and ID ####
  # DEFINE RUN number in the list of a given series, RUN_ID, SERIES_RUN_ID
  n_zeros <- args$n_zeros_RUN_IDs
  if (!file.exists(paths$LOG_file)){
    if (!file.exists(to_tmpdir(paths$LOG_file))){
      LOG_loc$RUN_n <- 1
    } else {
      LOG <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T)
      if (args$SERIES_ID %in% levels(LOG$SERIES_ID)){
        LOG_loc$RUN_n <- max(LOG[LOG$SERIES_ID == args$SERIES_ID, "RUN_n"])+1
      } else {
        LOG_loc$RUN_n <- 1
      }
      remove(LOG)
    }

  } else {
    if(!args$COMPOSITE & !args$EXPLORER){
      LOG <- data.table::fread(paths$LOG_file, data.table = F, stringsAsFactors = T)
      file.copy(from = paths$LOG_file, to = to_tmpdir(paths$LOG_file))
      if (args$SERIES_ID %in% levels(LOG$SERIES_ID)){
        LOG_loc$RUN_n <- max(LOG[LOG$SERIES_ID == args$SERIES_ID, "RUN_n"])+1
      } else {
        LOG_loc$RUN_n <- 1
      }
      remove(LOG)
    } else {
      if (!file.exists(to_tmpdir(paths$LOG_file))){
        LOG_loc$RUN_n <- 1
      } else {
        LOG <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T)
        if (args$SERIES_ID %in% levels(LOG$SERIES_ID)){
          LOG_loc$RUN_n <- max(LOG[LOG$SERIES_ID == args$SERIES_ID, "RUN_n"])+1
        } else {
          LOG_loc$RUN_n <- 1
        }
        remove(LOG)
      }
    }
  }
  LOG_loc$RUN_ID <- paste(c(as.character(replicate(n_zeros-length(unlist(strsplit(as.character(LOG_loc$RUN_n), ""))),0)),
                            as.character(LOG_loc$RUN_n)), collapse = "")
  SERIES_ID_RUN_ID <- paste(args$SERIES_ID, "_", as.character(LOG_loc$RUN_ID), sep = "")
  LOG_loc$SERIES_RUN_ID <- SERIES_ID_RUN_ID

  # _f. create RUN path in SERIES dir ####
  paths$SERIES_RUN_ID <- SERIES_ID_RUN_ID
  paths$outdir_root <- paste(paths$outdir, SERIES_ID_RUN_ID, sep = "")
  paths$digest_dir <- paste(paths$outdir_root, "_DIGEST/", sep = "")
  LOG_loc$path_outdir <- paths$outdir_root

  # _g. create RUN DIGEST dir in SERIES dir  ####
  if (any(args$export.data_as_csv_xlsx, args$export.diagrams, args$export.delta_plot)){
    if (!dir.exists(to_tmpdir(paths$digest_dir))){
      dir.create(to_tmpdir(paths$digest_dir))
    }
  }

  # _h. prepare input file #----
  IN <- list(CONSTS = CONSTS_IN,
             INITIAL = INITIAL_IN,
             FLUXES = FLUXES_IN,
             COEFFS = COEFFS_IN,
             BOX_META = BOX_META_IN)

  # __i. export as .xlsx ####
  if (args$export.data_as_csv_xlsx){
    paths$input_xl_file <-  paste(paths$digest_dir, "in_0_INPUTS_", SERIES_ID_RUN_ID, ".xlsx", sep = "")
    writexl::write_xlsx(IN, to_tmpdir(paths$input_xl_file))
  }

  IN$bx.groups = bx.groups

  # __ii. export as IN.rda ####
  paths$output_rds_file <-  paste(paths$outdir_root, ".rds", sep = "")

  # saveRDS(IN, file = to_tmpdir(paths$output_rds_file))

  remove(CONSTS_IN, INITIAL_IN, FLUXES_IN, COEFFS_IN, BOX_META_IN)

  # __iii. edit flux/coeff titles ####
  if (args$export.delta_plot | args$show.delta_plot | args$export.diagrams){
    FLUXES_title <- paste("Flux config    : " , args$flux_list, sep = "")
    COEFFS_title <- paste("Coeffs config : " , args$coeff_list, sep = "")
    if (!is.null(args$FORCING_RAYLEIGH)){
      COEFFS_title <- paste(COEFFS_title,
                            " // Rayleigh forcing: ", LOG_loc$FORCING_RAYLEIGH, sep = "")
    }

    if (!is.null(args$FORCING_ALPHA)){
      COEFFS_title <- paste(COEFFS_title, " // Alpha forcing: ", LOG_loc$FORCING_ALPHA, sep = "")
    }
  }

  # _i. update LOG csv ####
  data.table::fwrite(LOG_loc,
                     file = to_tmpdir(paths$LOG_file), row.names = F, quote = F, append = T)

  IN$LOG <- LOG_loc

  # _h. create plot path ####
  if (args$export.delta_plot & !fun_mode$tuto_mode){
    paths$plot_file <- paste(paths$digest_dir, "out_0_PLOT_evD_", SERIES_ID_RUN_ID, ".pdf", sep = "")
  }

  # VI. edit network diagrams ####
  if (args$export.diagrams & !fun_mode$tuto_mode){
    # _a. remove disconnected boxes ####
    if (length(bx.groups$disconnected) > 0){
      # LOG_loc$DISCONNECTED_BOXES <- c(stringr::str_c(bx.groups$disconnected), collapse = "_")
      LOG_loc$DISCONNECTED_BOXES <- paste(bx.groups$disconnected, collapse = "_")
      INITIAL <- INITIAL[!(INITIAL$BOX_ID %in% bx.groups$disconnected),]
      FLUXES <- FLUXES[, !(names(FLUXES) %in% bx.groups$disconnected)]
      FLUXES <- FLUXES[!(row.names(FLUXES) %in% bx.groups$disconnected),]
      COEFFS <- COEFFS[, -which(names(COEFFS) %in% bx.groups$disconnected)]
      COEFFS <- COEFFS[-which(row.names(COEFFS) %in% bx.groups$disconnected),]
    }

    # _b. prepare matrices ####
    FLUXES_adj <- FLUXES[ , !(names(FLUXES) %in% "BOX_ID")]
    COEFFS_adj <- COEFFS[ , !(names(COEFFS) %in% "BOX_ID")]
    COEFFS_adj <- 1000*log(COEFFS_adj)

    if (length(bx.groups$disconnected) > 0){
      BOXES_master_loc <- master$BOXES[-which(master$BOXES$BOX_ID %in% bx.groups$disconnected),
                                       c("BOX_ID", "INFINITE", "LAYOUT_X", "LAYOUT_Y")]
    } else {
      BOXES_master_loc <- master$BOXES[, c("BOX_ID", "INFINITE", "LAYOUT_X", "LAYOUT_Y")]
    }

    names(BOXES_master_loc) <- c("BOX_ID", "GROUP", "X_COORD", "Y_COORD")
    row.names(BOXES_master_loc) <- BOXES_master_loc$BOX_ID

    FLUXES_adj_box_order <- data.frame(BOX_ID = row.names(FLUXES_adj),
                                       ORDER = 1:nrow(FLUXES_adj))

    matrix_layout <- BOXES_master_loc %>%
      dplyr::full_join(INITIAL, by = "BOX_ID") %>%
      dplyr::full_join(FLUXES_adj_box_order, by = "BOX_ID") %>%
      dplyr::arrange(ORDER) %>%
      clear_subset() %>%
      dplyr::select(X_COORD, Y_COORD) %>%
      as.matrix()

    # _d. edit diagrams as pdf ####
    if (is.null(args$diagram_pdf.widh_height)){
      diag.width_height <- c(3, 3)
    } else {
      diag.width_height <- args$diagram_pdf.widh_height
    }
    paths$diag_flux_file <- paste(paths$digest_dir, "in_1_DIAG_FLUX_", SERIES_ID_RUN_ID, ".pdf", sep = "")
    pdf(to_tmpdir(paths$diag_flux_file), width = diag.width_height[1], height = diag.width_height[2], pointsize = 1, useDingbats=FALSE)
    suppressWarnings(
      plot_diagram(input = FLUXES_adj,
                   title = FLUXES_title,
                   matrix_layout = matrix_layout,
                   BOXES_master_loc = BOXES_master_loc,
                   COEFF_FLUX = "FLUX")
    )
    dev.off()

    paths$diag_coeff_file <- paste(paths$digest_dir, "in_2_DIAG_COEFF_", SERIES_ID_RUN_ID, ".pdf", sep = "")
    pdf(to_tmpdir(paths$diag_coeff_file), width = diag.width_height[1], height = diag.width_height[2], pointsize = 1, useDingbats=FALSE)
    suppressWarnings(
      plot_diagram(input = COEFFS_adj,
                   title = COEFFS_title,
                   matrix_layout = matrix_layout,
                   BOXES_master_loc = BOXES_master_loc,
                   COEFF_FLUX = "COEFF")
    )
    dev.off()
  }

  # VII. run model ####
  if (args$export.data_as_csv_xlsx){to_DIGEST_csv = TRUE} else {to_DIGEST_csv = FALSE}

  if (LOG_loc$NUM_ANA == "NUM") {
    solver.suited <- "numerical"
  } else {
    solver.suited <- "analytical"
  }

  if (args$export.delta_plot | args$show.delta_plot | args$return_data){
    return_results <- TRUE
  } else {
    return_results <- FALSE
  }

  # results <- solve_numerically(IN = IN, paths = paths, to_DIGEST_csv = to_DIGEST_csv, return_results = return_results)

  if (LOG_loc$NUM_ANA == "ANA" & args$solver %in% c("auto", "analytical")){
    results <- solve_analytically(IN = IN, paths = paths, to_DIGEST_csv = to_DIGEST_csv, return_results = return_results)
  } else if (LOG_loc$NUM_ANA == "NUM" & args$solver %in% c("auto", "numerical")){
    results <- solve_numerically(IN = IN, paths = paths, to_DIGEST_csv = to_DIGEST_csv, return_results = return_results)
  } else {
    if (args$solver == "analytical"){
      rlang::abort(paste0("An analytical solution is required by user or sweeping function.",
                          "\n ", " The system has unbalanced fluxes:",
                          " only the numerical solver can be used."))
    } else if (args$solver == "numerical"){
      rlang::abort(paste0("The user requires a numerical solution but sim.single_run recommends the use of the analytical solver.",
                          "\n ", " The current version of isobxr does not allow forcing the type of solver",
                          "\n ", " because the accuracy of numerical solutions strongly depends on temporal resolution."))
    }
  }

  # VIII. edit plots ####
  # _a. plot ####
  # _b. export plot as pdf ####
  if (args$export.delta_plot & !fun_mode$tuto_mode){
    dev.new()
    pdf(to_tmpdir(paths$plot_file),
        width = 21/2.54, height = 29.7/2.54,
        pointsize = 1, useDingbats=FALSE)
    suppressWarnings(plot_single_run(workdir = to_tmpdir(""),
                                     RUN_ID = paths$SERIES_RUN_ID,
                                     time_as_log10 = args$plot.time_as_log10,
                                     time_unit = args$plot.time_unit,
                                     hidden_boxes = NULL,
                                     return_as_print = TRUE))
    graphics.off()
  }

  # XIX. final outputs ####
  # _a. save outputs ####
  if(!args$COMPOSITE & !args$EXPLORER){
    if(!args$suppress_messages){
      rlang::inform("________________________________________________________________________________")
      rlang::inform(message = paste("\U2139 Run outputs (stored in temporary directory):",
                                    sep = ""))
      fs::dir_tree(path = to_tmpdir(""), recurse = T)
      rlang::inform("________________________________________________________________________________")
    }
    if(fun_mode$save_outputs){
      R.utils::copyDirectory(to_tmpdir(""),
                             getwd(),
                             overwrite = T)
      if(!args$suppress_messages){
        rlang::inform("\U2705 Results were successfully exported to working directory.")
      }
    } else {
      if(!args$suppress_messages){
        rlang::inform("\U2757 Results were not exported to working directory (set save_outputs = TRUE to save results).")
      }
    }
  }

  # _b. print plots on console ####
  if (args$show.delta_plot){
    suppressWarnings(plot_single_run(workdir = to_tmpdir(""),
                                     RUN_ID = paths$SERIES_RUN_ID,
                                     time_as_log10 = args$plot.time_as_log10,
                                     time_unit = args$plot.time_unit,
                                     hidden_boxes = NULL,
                                     return_as_print = TRUE))
  }

  if(args$return_data){
    return(list(inputs = IN, outputs = results, paths = paths))
  }
}
