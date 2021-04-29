usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("beepr", min_version = TRUE)
usethis::use_package("data.table", min_version = TRUE)
usethis::use_package("ggplot2", min_version = TRUE)
usethis::use_package("ggrepel", min_version = TRUE)
usethis::use_package("rlang", min_version = TRUE)

#  #_________________________________________________________________________80char
#' Compose a stable isotope box model scenario
#' @description  A function to compose an isobxr box model scenario,
#' defined by a series of \emph{n} successive runs, \cr
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
#' @param workdir Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
#' of the composite master file (e.g., \strong{\emph{0_COMPO_MASTER.xlsx}}) \cr
#' and where output files will be stored. \cr
#' (character string)
#' @param SERIES_ID Name of the composite model series the run belongs to. \cr
#' It determines the folder in which the output files will be stored for this composite run.\cr
#' A composite run number is automatically linked to it,
#' subsequent runs can not overwrite a previous composite run.\cr
#' (character string)
#' @param time_units Vector defining the initial time unit
#' (identical to unit used in fluxes), \cr
#' followed by the time unit used for the graphical output.\cr
#' Character string, to be selected  amongst the following:\cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}\cr
#' e.g.,  c("d", "yr") to convert days into years
#' @param COMPO_MASTER Name of the composite master file (e.g., \strong{\emph{0_COMPO_MASTER.xlsx}}),
#' defining the composite run scenario. \cr
#' (character string)
#' @param plot_HIDE_BOXES_delta  \emph{OPTIONAL} \cr
#' Vector of character strings, \cr
#' defining the names of the boxes to hide in the plot of the delta values as a function of time, edited as a pdf.
#' \cr (e.g., c("BOX_A", "BOX_C"))
#' \cr Default is NULL (no box hidden).
#' @param plot_HIDE_BOXES_size  \emph{OPTIONAL} \cr
#' Vector of character strings, \cr
#' defining the names of the boxes to hide in the plot of the box sizes (masses of X) as a function of time, edited as a pdf.
#' \cr (e.g., c("BOX_A", "BOX_C"))
#' \cr Default is NULL (no box hidden).
#' @param EACH_RUN_DIGEST \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Edits full digests for each model run
#' (all optional outputs of \code{\link{run_isobxr}} function) if TRUE. \cr
#' Default is FALSE.
#' @param to_CPS_DIGEST_CSVs \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Exports all global csv outputs to \strong{\emph{0_CPS_DIGEST}} folder (full evD and full evS) if TRUE. \cr
#' Default is FALSE.
#'
#' @return Creates and stores all outputs in a dedicated composite SERIES directory located in working directory,
#' with the following name structure: \cr
#' \strong{\emph{3_CPS + SERIES_ID + YYY}}, where YYY is a composite scenario number automically set between 001 and 999. \cr
#' No overwriting of previous composite runs is possible.
#'
#' \enumerate{
#' \item Creates the set of inputs and outputs for all successive \emph{n} runs,\cr
#' numbered from to 1 to \emph{n} in an XXXX format with the following format: \cr
#' \strong{\emph{CPS + SERIES_ID + YYY + XXXX + IN.Rda}} \cr
#' \strong{\emph{CPS + SERIES_ID + YYY + XXXX + OUT.Rda}} \cr
#' (see \code{\link{run_isobxr}} documentation)
#'
#' \item Writes summarized results in the  \strong{\emph{0_CPS_DIGEST}} folder:
#' \enumerate{
#' \item LOG file of local composite run. \cr
#' (file name structure:  \strong{\emph{CPS + SERIES_ID + YYY + _LOG.csv}})
#' \item Composite master file. \cr
#' (file name structure:  \strong{\emph{CPS + SERIES_ID + YYY + _MASTER.xlsx}})
#' \item Dataset of temporal evolution of delta values (evD) in all boxes over the \emph{n} runs that constitute the composite run scenario \cr
#' (file name structure: \strong{\emph{CPS + SERIES_ID + YYY + evD.RDS}})
#' \item Dataset of temporal evolution of box sizes (evS, masses of X) in all boxes over the \emph{n} runs that constitute the composite run scenario \cr
#' (file name structure: \strong{\emph{CPS + SERIES_ID + YYY + evS.RDS}})
#' \item All-in-one plot of the evolution of delta values + sizes in all non hidden boxes. \cr
#' (file name structure: \strong{\emph{CPS + SERIES_ID + YYY + p_evDS.pdf}})
#' \item Multiple plots of the evolution of delta values in all non hidden boxes. \cr
#' (file name structure: \strong{\emph{CPS + SERIES_ID + YYY + pf_evD.pdf}})
#' \item Multiple plots of the evolution of box sizes in all non hidden boxes. \cr
#' (file name structure: \strong{\emph{CPS + SERIES_ID + YYY + pf_evS.pdf}})
#' }
#' }
#' @section Optional outputs:
#' \enumerate{
#' \item If EACH_RUN_DIGEST = TRUE \cr
#' Creates and fills \strong{\emph{DIGEST}} folder for each run of the composite scenario with
#' all optional outputs of \code{\link{run_isobxr}} function. \cr
#' (folder name structure: \strong{\emph{CPS + SERIES_ID + YYY + XXXX + DIGEST}})
#' \item If to_CPS_DIGEST_CSVs = TRUE \cr
#' In the \strong{\emph{0_CPS_DIGEST}} folder,
#' edits csv versions of the whole-composite scenario evD and evS datasets. \cr
#' (file names structures: \strong{\emph{CPS + SERIES_ID + YYY + evD.csv}}
#' and \strong{\emph{CPS + SERIES_ID + YYY + evS.csv}})
#' }
#' @seealso Documentation on \code{\link{run_isobxr}}
#' @export
compose_isobxr <- function(workdir,
                           SERIES_ID,
                           time_units, # WARNING, so far it only works from "days" (inherited by the units of the fluxes values) to "minutes", "hours", "days", "hours"
                           COMPO_MASTER, # excel file
                           plot_HIDE_BOXES_delta = NULL,
                           plot_HIDE_BOXES_size = NULL,
                           EACH_RUN_DIGEST = FALSE,
                           to_CPS_DIGEST_CSVs = FALSE){

  # locally bind variables (fixing binding global variable issue)
  INITIAL_IN <- FLUXES_IN <- COEFFS_IN <- A_OUT <- N_OUT <- A_evD <- N_evD <- N_evS <- NULL

  # REMARKS
  # the FORCING_DELTA sheet :  when a delta value is forced at a stage it will be inherited in the next runs
  # FORCING_ALPHA sheet : not inherited from a run to another, back to previous value

  # #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# CLEAR
  # Clear plots
  if(!is.null(dev.list())) dev.off()
  # Clear console
  # cat("\014")
  # Clean workspace
  # rm(list=ls())
  Time_plot <- VAR <- VAR_TYPE <- NULL

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIALIZE
  #************************************** SET WORKING DIRECTORY #----
  LOC_workdir <- workdir
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(LOC_workdir)

  plot_HIDE_RUNs_n <- c(1)

  if (isFALSE(EACH_RUN_DIGEST)){
    to_DIGEST_DIAGRAMS = FALSE
    to_DIGEST_evD_PLOT = FALSE
    to_DIGEST_CSV_XLS = FALSE
  } else {
    to_DIGEST_DIAGRAMS = TRUE
    to_DIGEST_evD_PLOT = TRUE
    to_DIGEST_CSV_XLS = TRUE
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PREPARE ISOPYBOX ARGUMENTS
  #************************************** DEFINE LOCAL FORCINGs and CONSTANTS from COMPO_MASTER #----
  RUN_LIST <- as.data.frame(readxl::read_excel(COMPO_MASTER, "RUN_LIST"))
  RAYLEIGH_LIST <- as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_RAYLEIGH"))
  DELTA_FORCING <- as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_DELTA"))
  FORCING_ALPHA <- as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_ALPHA"))
  FORCING_SIZE <- as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_SIZE"))

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

  #************************************** DEFINE COMPOSITE SERIES FAMILY, COMPO SERIES NUMBER, SERIES_ID #----
  dir_LOG <- "1_LOG.csv"
  n_zeros <- 3
  if (file.exists(dir_LOG) == TRUE){
    LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
    LOG_COMPO <- LOG[LOG$COMPOSITE == TRUE, ]
    remove(LOG)
    COMPO_SERIES_FAMILY <- paste("CPS", as.character(SERIES_ID), sep = "_")
    if (nrow(LOG_COMPO[LOG_COMPO$COMPO_SERIES_FAMILY == COMPO_SERIES_FAMILY,]) == 0){
      COMPO_SERIES_n <- 1
      SERIES_ID <- paste("CPS", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
    } else {
      COMPO_SERIES_n <- max(LOG_COMPO[LOG_COMPO$COMPO_SERIES_FAMILY == COMPO_SERIES_FAMILY, "COMPO_SERIES_n"])+1
      COMPO_SERIES_n_length <- length(unlist(strsplit(as.character(COMPO_SERIES_n), "")))
      SERIES_ID <- paste("CPS", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-COMPO_SERIES_n_length,0),COMPO_SERIES_n)), collapse = ""), sep = "_")
    }
  } else {
    COMPO_SERIES_n <- 1
    COMPO_SERIES_FAMILY <- paste("CPS", as.character(SERIES_ID), sep = "_")
    SERIES_ID <- paste("CPS", as.character(SERIES_ID), paste(as.character(c(replicate(n_zeros-1,0),1)), collapse = ""), sep = "_")
  }

  #************************************** READ CONSTANTS FROM ISOPY_MASTER #----
  ISOPY_MASTER_file <- "0_ISOBXR_MASTER.xlsx"
  CONSTANTS <- as.data.frame(readxl::read_excel(ISOPY_MASTER_file, "CONSTANTS"))

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIAL ISOPYBOX RUN (i = 1) #----
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
  rlang::inform("* COMPUTING *")

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
             COMPOSITE = COMPOSITE,
             COMPO_SERIES_n = COMPO_SERIES_n,
             COMPO_SERIES_FAMILY = COMPO_SERIES_FAMILY,
             EXPLORER = FALSE,
             EXPLO_SERIES_n = NaN,
             EXPLO_SERIES_FAMILY = NaN,
             HIDE_PRINTS = FALSE,
             to_DIGEST_DIAGRAMS = to_DIGEST_DIAGRAMS,
             to_DIGEST_evD_PLOT = to_DIGEST_evD_PLOT,
             to_DIGEST_CSV_XLS = to_DIGEST_CSV_XLS)

  calculation_gauge(0, length(t_lim_list))
  calculation_gauge(i, length(t_lim_list))

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# INITIAL ISOPYBOX RUN (i > 1) #----
  i <- i + 1
  # i
  for (i in 2:length(t_lim_list)){

    #### PREPARE INPUTS for ISOPY_RUN / INHERIT RUN i-1
    fx <- flux_list[i]
    a <- coeff_list[i]
    LOC_t_lim <- t_lim_list[i]
    LOC_nb_steps <- nb_steps_list[i]

    if (i %in% RAYLEIGH_LIST$COMPO_RUN_n){
      LOC_RAYLEIGH <- clear_subset(RAYLEIGH_LIST[RAYLEIGH_LIST$COMPO_RUN_n == i,-which(names(RAYLEIGH_LIST) %in% c("COMPO_RUN_n"))])
    } else {
      LOC_RAYLEIGH <- NULL
    }

    LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
    LOG_last <- LOG[nrow(LOG),]
    remove(LOG)

    if (LOG_last$NUM_ANA == "ANA"){
      load(paste(LOG_last$path_outdir, "OUT.Rda", sep = ""))
      OUT_last_final <- A_OUT
      OUT_last_SIZE_FINAL <- OUT_last_final[, c("BOXES_ID", "SIZE_INIT")]
      OUT_last_DELTA_FINAL <- OUT_last_final[, c("BOXES_ID", "DELTA_FINAL")]
      names(OUT_last_DELTA_FINAL) <- c("BOXES_ID", "DELTA_INIT")

    } else {
      load(paste(LOG_last$path_outdir, "OUT.Rda", sep = ""))
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

    #### FORCE OVERWRITING DELTA_INIT
    if (is.null(DELTA_FORCING) == FALSE& i %in% DELTA_FORCING$COMPO_RUN_n){
      DELTA_FORCING_loc <- DELTA_FORCING[DELTA_FORCING$COMPO_RUN_n == i, c("BOXES_ID", "DELTA_INIT")]
      DELTA_FORCING_loc <- clear_subset(DELTA_FORCING_loc)
      j <- 1
      for (j in 1:nrow(DELTA_FORCING_loc)){
        LOC_DELTA_INIT[LOC_DELTA_INIT$BOXES_ID == as.character(DELTA_FORCING_loc[j, "BOXES_ID"]), "DELTA_INIT"] = DELTA_FORCING_loc[j, "DELTA_INIT"]
        j <- j + 1
      }
    }

    #### FORCE OVERWRITING SIZE_INIT
    if (is.null(FORCING_SIZE) == FALSE& i %in% FORCING_SIZE$COMPO_RUN_n){
      FORCING_SIZE_loc <- FORCING_SIZE[FORCING_SIZE$COMPO_RUN_n == i, c("BOXES_ID", "SIZE_INIT")]
      FORCING_SIZE_loc <- clear_subset(FORCING_SIZE_loc)
      j <- 1
      for (j in 1:nrow(FORCING_SIZE_loc)){
        LOC_SIZE_INIT[LOC_SIZE_INIT$BOXES_ID == as.character(FORCING_SIZE_loc[j, "BOXES_ID"]), "SIZE_INIT"] = FORCING_SIZE_loc[j, "SIZE_INIT"]
        j <- j + 1
      }
    }

    #### FORCE OVERWRITING ALPHa
    if (is.null(FORCING_ALPHA) == FALSE& i %in% FORCING_ALPHA$COMPO_RUN_n){
      FORCING_ALPHA_loc <- FORCING_ALPHA[FORCING_ALPHA$COMPO_RUN_n == i, c("FROM", "TO", "ALPHA")]
      FORCING_ALPHA_loc <- clear_subset(FORCING_ALPHA_loc)
    } else {
      FORCING_ALPHA_loc = NULL
    }

    #### RUN ISOPYRUN
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
               FORCING_ALPHA = FORCING_ALPHA_loc,
               COMPOSITE = COMPOSITE,
               COMPO_SERIES_n = COMPO_SERIES_n,
               COMPO_SERIES_FAMILY = COMPO_SERIES_FAMILY,
               EXPLORER = FALSE,
               EXPLO_SERIES_n = NaN,
               EXPLO_SERIES_FAMILY = NaN,
               HIDE_PRINTS = TRUE,
               to_DIGEST_DIAGRAMS = to_DIGEST_DIAGRAMS,
               to_DIGEST_evD_PLOT = to_DIGEST_evD_PLOT,
               to_DIGEST_CSV_XLS = to_DIGEST_CSV_XLS)
    calculation_gauge(i, length(t_lim_list))
    i <- i + 1
  }

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# LOAD/EDIT COMPOSITE SERIES LOG/OUT FILES and EDIT ANA evS
  #************************************** LOAD LOG/OUT FILES of CURRENT COMPO SERIES #----
  LOG <- data.table::fread(dir_LOG, data.table = F, stringsAsFactors = T)
  LOG_SERIES <- LOG[LOG$SERIES_ID == SERIES_ID,]
  remove(LOG)
  LOG_SERIES <- clear_subset(LOG_SERIES)
  SERIES_RUN_ID_1 <- LOG_SERIES[1, "SERIES_RUN_ID"]
  path_to_input_1 <- paste(LOG_SERIES[1, "path_outdir"], "IN.Rda", sep = "")
  load(path_to_input_1)
  BOXES_IDs <- as.character(INITIAL_IN$BOXES_ID)

  #************************************** READ/BUILD/MERGE evS/evD for ANA/NUM WHOLE COMPOSITE RUN #----
  i <- 1

  rlang::inform("* PREPARE RESULTS *")
  calculation_gauge(0, length(t_lim_list))

  for (i in 1:length(t_lim_list)){
    SERIES_RUN_ID_i <- LOG_SERIES[i, "SERIES_RUN_ID"]
    RUN_n_i <- LOG_SERIES[i, "RUN_n"]
    path_outdir_i <- as.character(LOG_SERIES[i, "path_outdir"])
    path_to_INPUT_i <- paste(path_outdir_i, "IN.Rda", sep = "")
    load(path_to_INPUT_i)
    SIZE_INIT_i <- INITIAL_IN[,c("BOXES_ID", "SIZE_INIT")]
    DELTA_INIT_i <- INITIAL_IN[,c("BOXES_ID", "DELTA_INIT")]
    FLUXES_i <- FLUXES_IN
    COEFFS_i <- COEFFS_IN

    if (LOG_SERIES[i, "NUM_ANA"] == "ANA"){
      load(paste(path_outdir_i, "OUT.Rda", sep = ""))
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
        load(paste(path_outdir_i, "OUT.Rda", sep = ""))
        evD_i <- N_evD
        evS_i <- N_evS
      }
    }

    evD_i$Time_COMPOSITE <- evD_i$Time
    evS_i$Time_COMPOSITE <- evS_i$Time
    evD_i$SERIES_RUN_ID <- SERIES_RUN_ID_i
    evS_i$SERIES_RUN_ID <- SERIES_RUN_ID_i
    evD_i$RUN_n <- RUN_n_i
    evS_i$RUN_n <- RUN_n_i

    if (i == 1){
      evD <- evD_i
      evS <- evS_i
    } else {
      evD_i$Time_COMPOSITE <- evD_i$Time + max(evD$Time_COMPOSITE)
      evS_i$Time_COMPOSITE <- evS_i$Time + max(evD$Time_COMPOSITE)
      evD <- rbind(evD, evD_i[1:nrow(evD_i),])
      evS <- rbind(evS, evS_i[1:nrow(evS_i),])
    }
    calculation_gauge(i, length(t_lim_list))
    i <- i + 1

  }

  #************************************** EDIT CSV for WHOLE COMPOSITE RUN evS - evD - LOG - OUT #----
  rlang::inform("* WRITE OUTPUTS *")

  path_out_COMPO <- paste("3_", as.character(SERIES_ID), "/", "0_CPS_DIGEST/", sep = "")
  if (!dir.exists(path_out_COMPO)){dir.create(path_out_COMPO)}
  path_out_COMPO <- paste(path_out_COMPO, as.character(SERIES_ID), sep = "")

  data.table::fwrite(LOG_SERIES, file = paste(path_out_COMPO, "_LOG.csv", sep = ""), row.names = F, quote = F)
  saveRDS(object = evD, file = paste(path_out_COMPO, "_evD.RDS", sep = ""))
  saveRDS(object = evS, file = paste(path_out_COMPO, "_evS.RDS", sep = ""))

  if (isTRUE(to_CPS_DIGEST_CSVs)){
    data.table::fwrite(evD, file = paste(path_out_COMPO, "_evD.csv", sep = ""), row.names = F, quote = F)
    data.table::fwrite(evS, file = paste(path_out_COMPO, "_evS.csv", sep = ""), row.names = F, quote = F)
  }


  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PLOT COMPOSITE RUN
  #************************************** PLOT evD #----
  #### subset evD for plot (hide the first plot_HIDE_RUNs_n runs)
  evD <- evD[-which(evD$RUN_n %in% plot_HIDE_RUNs_n), ]

  #### reset time for plot (Time_plot)
  evD$Time_plot <- evD$Time_COMPOSITE - min(evD$Time_COMPOSITE)
  evD <- clear_subset(evD)

  #### reset time units
  initial_time_unit <- time_units[1]
  display_time_unit <- time_units[2]

  # if (display_time_unit != initial_time_unit & initial_time_unit == "days"){
  #   if (display_time_unit == "hours"){
  #     evD$Time_plot <- evD$Time_plot*24
  #   } else {
  #     if (display_time_unit == "minutes"){
  #       evD$Time_plot <- evD$Time_plot*24*60
  #     } else {
  #       if (display_time_unit == "years"){
  #         evD$Time_plot <- evD$Time_plot/365
  #       } else {
  #         display_time_unit = initial_time_unit
  #       }
  #     }
  #   }
  # }

  evD <- time_converter(dataframe = evD, time_colname = "Time_plot",
                             conv_timecolname = "Time_plot_conv",
                             former_unit = initial_time_unit,
                             new_unit = display_time_unit)

  evD$Time_plot <- evD$Time_plot_conv

  #### extract composite sub-runs (zones)
  k <- 1
  SERIES_RUN_ID_plot <- levels(evD$SERIES_RUN_ID)
  for (k in 1:length(SERIES_RUN_ID_plot)){4
    min_time_loc <- min(evD[evD$SERIES_RUN_ID == SERIES_RUN_ID_plot[k], "Time"])
    if (k == 1){
      evD_zones <- evD[evD$SERIES_RUN_ID == SERIES_RUN_ID_plot[k] & evD$Time == min_time_loc, ]
    } else {
      evD_zones <- rbind(evD_zones, evD[evD$SERIES_RUN_ID == SERIES_RUN_ID_plot[k] & evD$Time == min_time_loc, ])
    }
    k <- k + 1
  }

  #### verticalize evD
  evD_zones_vert <- DF_verticalizer(df_hor = evD_zones, vert_col = BOXES_IDs)
  evD_vert <- DF_verticalizer(df_hor = evD, vert_col = BOXES_IDs)


  display_evD_plot = TRUE

  #### hide unwanted boxes for delta plot
  if (! is.null(plot_HIDE_BOXES_delta)){
    evD_zones_vert <- evD_zones_vert[-which(evD_zones_vert$VAR_TYPE %in% plot_HIDE_BOXES_delta), ]
    evD_vert <- evD_vert[-which(evD_vert$VAR_TYPE %in% plot_HIDE_BOXES_delta), ]
  }
  evD_zones_vert <- clear_subset(evD_zones_vert)
  evD_vert <- clear_subset(evD_vert)

  #### set limits of plot
  Ymin <- round(min(evD_vert$VAR), 0)-1
  Ymax <- round(max(evD_vert$VAR), 0)+1
  Ymin_zoom <- min(evD_vert$VAR)
  Ymax_zoom <- max(evD_vert$VAR)
  # Ybin <- 0.25
  Ybin <- signif((Ymax-Ymin)/10, digits = 1) # automatic definition of Ybin
  Xmax <- max(evD_vert$Time_plot) + 0.1*max(evD_vert$Time_plot)

  #### extract t0 and t_final delta values
  evD_initial <- evD_vert[evD_vert$Time_plot == min(evD_vert$Time_plot),]
  evD_final <- evD_vert[evD_vert$Time_plot == max(evD_vert$Time_plot),]

  #### plot delta evol
  evD_plot <- ggplot2::ggplot(data = evD_vert, ggplot2::aes(x = Time_plot, y = VAR, color = VAR_TYPE))+
    ggplot2::geom_line(cex = 1)+
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(limits=c(Ymin, Ymax), breaks=seq(Ymin, Ymax, by = Ybin), labels = dec_2)+
    ggplot2::coord_cartesian(ylim = c(Ymin_zoom, Ymax_zoom + 0.05*Ymax_zoom), xlim = c(0.1, Xmax))+
    ggplot2::labs(y = paste("d", CONSTANTS$NUMERATOR, "/", CONSTANTS$DENOMINATOR, CONSTANTS$ELEMENT, sep = ""),
                  x = paste("Time in", display_time_unit, sep = " "),
                  title = paste(SERIES_ID, " (", min(LOG_SERIES$RUN_n), "-", max(LOG_SERIES$RUN_n), ") - Initial hidden: ", paste(LOG_SERIES[plot_HIDE_RUNs_n, "COEFF_FLUX"], collapse = " / "), sep = ""))+
    ggrepel::geom_text_repel(data = evD_final, ggplot2::aes(label = paste(VAR_TYPE, " (", dec_2(VAR), ")",  sep = ""), color = VAR_TYPE), nudge_x = 0.05*max(evD_vert$Time_plot),  hjust = 0)

  #### annotate zones
  k <- 1
  for (k in 1:nrow(evD_zones)){
    evD_plot <- evD_plot +
      ggplot2::geom_vline(xintercept = evD_zones[k, "Time_plot"] ,  linetype = 2) +
      ggplot2::annotate(geom = "text", x = evD_zones[k, "Time_plot"], y = Ymax_zoom + 0.05*Ymax_zoom, label = LOG_SERIES[plot_HIDE_RUNs_n+k, "COEFF_FLUX"], hjust = 0)
    k <- k + 1
  }

  #### plot evD facets
  evD_plot_facet <- ggplot2::ggplot(data = evD_vert, ggplot2::aes(x = Time_plot, y = VAR, color = VAR_TYPE))+
    ggplot2::geom_line(cex = 1)+
    ggplot2::facet_wrap(. ~ VAR_TYPE)+
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(limits=c(Ymin, Ymax), breaks=seq(Ymin, Ymax, by = Ybin), labels = dec_2)+
    ggplot2::coord_cartesian(ylim = c(Ymin_zoom, Ymax_zoom + 0.05*Ymax_zoom), xlim = c(0.1, Xmax))+
    ggplot2::labs(y = paste("d", CONSTANTS$NUMERATOR, "/", CONSTANTS$DENOMINATOR, CONSTANTS$ELEMENT, sep = ""),
                  x = paste("Time in", display_time_unit, sep = " "),
                  title = paste(SERIES_ID, " (", min(LOG_SERIES$RUN_n), "-", max(LOG_SERIES$RUN_n), ") - Initial hidden: ", paste(LOG_SERIES[plot_HIDE_RUNs_n, "COEFF_FLUX"], collapse = " / "), sep = ""))+
    ggrepel::geom_text_repel(data = evD_final, ggplot2::aes(label = paste("(", dec_2(VAR), ")",  sep = ""), color = VAR_TYPE), nudge_x = 0.05*max(evD_vert$Time_plot),  hjust = 0) +
    ggrepel::geom_text_repel(data = evD_zones_vert, ggplot2::aes(label = paste("(", dec_2(VAR), ")",  sep = ""), color = VAR_TYPE), nudge_y = 0.1*max(evD_vert$VAR), nudge_x = 0.05*max(evD_vert$Time_plot))

  #### annotate zones on evD facets
  k <- 1
  for (k in 1:nrow(evD_zones)){
    evD_plot_facet <- evD_plot_facet +
      ggplot2::geom_vline(xintercept = evD_zones[k, "Time_plot"] ,  linetype = 2)
    k <- k + 1
  }

  #### export evD facets pdf
  pdf_path <- paste(path_out_COMPO, "_pf_evD.pdf", sep = "")
  pdf(pdf_path, width = 15, height = 10, pointsize = 1, useDingbats = FALSE)
  print(evD_plot_facet)
  dev.off()

  #************************************** PLOT evS #----
  display_evS_plot = TRUE

  #### subset evD for plot (hide the first plot_HIDE_RUNs_n runs)
  evS <- evS[-which(evS$RUN_n %in% plot_HIDE_RUNs_n), ]

  #### reset time for plot (Time_plot)
  evS$Time_plot <- evS$Time_COMPOSITE - min(evS$Time_COMPOSITE)
  evS <- clear_subset(evS)

  #### reset time units
  initial_time_unit <- time_units[1]
  display_time_unit <- time_units[2]

  # if (display_time_unit != initial_time_unit & initial_time_unit == "days"){
  #   if (display_time_unit == "hours"){
  #     evS$Time_plot <- evS$Time_plot*24
  #   } else {
  #     if (display_time_unit == "minutes"){
  #       evS$Time_plot <- evS$Time_plot*24*60
  #     } else {
  #       if (display_time_unit == "years"){
  #         evS$Time_plot <- evS$Time_plot/365
  #       } else {
  #         display_time_unit = initial_time_unit
  #       }
  #     }
  #   }
  # }

  evS <- time_converter(dataframe = evS, time_colname = "Time_plot",
                        conv_timecolname = "Time_plot_conv",
                        former_unit = initial_time_unit,
                        new_unit = display_time_unit)

  evS$Time_plot <- evS$Time_plot_conv

  #### extract composite sub-runs (zones)
  k <- 1
  SERIES_RUN_ID_plot <- levels(evS$SERIES_RUN_ID)
  for (k in 1:length(SERIES_RUN_ID_plot)){4
    min_time_loc <- min(evS[evS$SERIES_RUN_ID == SERIES_RUN_ID_plot[k], "Time"])
    if (k == 1){
      evS_zones <- evS[evS$SERIES_RUN_ID == SERIES_RUN_ID_plot[k] & evS$Time == min_time_loc, ]
    } else {
      evS_zones <- rbind(evS_zones, evS[evS$SERIES_RUN_ID == SERIES_RUN_ID_plot[k] & evS$Time == min_time_loc, ])
    }
    k <- k + 1
  }

  #### verticalize evS
  evS_zones_vert <- DF_verticalizer(df_hor = evS, vert_col = BOXES_IDs)
  evS_vert <- DF_verticalizer(df_hor = evS, vert_col = BOXES_IDs)

  #### hide unwanted boxes for delta plot
  if (! is.null(plot_HIDE_BOXES_size)){
    evS_zones_vert <- evS_zones_vert[-which(evS_zones_vert$VAR_TYPE %in% plot_HIDE_BOXES_size), ]
    evS_vert <- evS_vert[-which(evS_vert$VAR_TYPE %in% plot_HIDE_BOXES_size), ]
  }

  evS_zones_vert <- clear_subset(evS_zones_vert)
  evS_vert <- clear_subset(evS_vert)

  #### set limits of plot
  Ymin <- round(min(evS_vert$VAR), 0)-1
  Ymax <- round(max(evS_vert$VAR), 0)+1
  Ymin_zoom <- min(evS_vert$VAR)
  Ymax_zoom <- max(evS_vert$VAR)
  Ybin <- 0.25
  Xmax <- max(evS_vert$Time_plot) + 0.1*max(evS_vert$Time_plot)

  #### extract t0 and t_final delta values
  evS_initial <- evS_vert[evS_vert$Time_plot == min(evS_vert$Time_plot),]
  evS_final <- evS_vert[evS_vert$Time_plot == max(evS_vert$Time_plot),]

  #### plot evS
  evS_plot <- ggplot2::ggplot(data = evS_vert, ggplot2::aes(x = Time_plot, y = VAR, color = VAR_TYPE))+
    ggplot2::geom_line(cex = 1)+
    ggplot2::theme_bw()+
    ggplot2::scale_y_log10()+
    ggplot2::coord_cartesian(xlim = c(0.1, Xmax))+
    ggplot2::labs(y = paste("mass of ", CONSTANTS$ELEMENT, sep = ""),
                  x = paste("Time in", display_time_unit, sep = " "),
                  title = paste(SERIES_ID, " (", min(LOG_SERIES$RUN_n), "-", max(LOG_SERIES$RUN_n), ") - Initial hidden: ", paste(LOG_SERIES[plot_HIDE_RUNs_n, "COEFF_FLUX"], collapse = " / "), sep = ""))+
    ggrepel::geom_text_repel(data = evS_final, ggplot2::aes(label = paste(VAR_TYPE, sep = ""), color = VAR_TYPE), nudge_x = 0.05*max(evS_vert$Time_plot),  hjust = 0)

  #### annotate zones on evS
  k <- 1
  for (k in 1:nrow(evS_zones)){
    evS_plot <- evS_plot +
      ggplot2::geom_vline(xintercept = evS_zones[k, "Time_plot"] ,  linetype = 2) +
      ggplot2::annotate(geom = "text", x = evS_zones[k, "Time_plot"] , y = Ymax_zoom + 0.75*Ymax_zoom, label = LOG_SERIES[plot_HIDE_RUNs_n+k, "COEFF_FLUX"], hjust = 0)
    k <- k + 1
  }

  #### edit pdf of evD/evS multiplot
  pdf_path <- paste(path_out_COMPO, "_p_evDS.pdf", sep = "")
  dev.new()
  pdf(pdf_path, width = 15, height = 15, pointsize = 1, useDingbats=FALSE)
  multiplot(evD_plot, evS_plot, cols = 1)
  graphics.off()

  #### plot evS facets
  evS_plot_facet <- ggplot2::ggplot(data = evS_vert, ggplot2::aes(x = Time_plot, y = VAR, color = VAR_TYPE))+
    ggplot2::geom_line(cex = 1)+
    ggplot2::theme_bw()+
    ggplot2::facet_wrap(. ~ VAR_TYPE, scales = "free_y")+
    ggplot2::coord_cartesian(xlim = c(0.1, Xmax))+
    ggplot2::labs(y = paste("mass of ", CONSTANTS$ELEMENT, sep = ""),
                  x = paste("Time in", display_time_unit, sep = " "),
                  title = paste(SERIES_ID, " (", min(LOG_SERIES$RUN_n), "-", max(LOG_SERIES$RUN_n), ") - Initial hidden: ", paste(LOG_SERIES[plot_HIDE_RUNs_n, "COEFF_FLUX"], collapse = " / "), sep = ""))+
    ggrepel::geom_text_repel(data = evS_final, ggplot2::aes(label = paste(VAR_TYPE, sep = ""), color = VAR_TYPE), nudge_x = 0.05*max(evS_vert$Time_plot),  hjust = 0)

  #### annotate zones on evS facets
  k <- 1
  for (k in 1:nrow(evS_zones)){
    evS_plot_facet <- evS_plot_facet +
      ggplot2::geom_vline(xintercept = evS_zones[k, "Time_plot"] ,  linetype = 2)
    # annotate(geom = "text", x = evS_zones[k, "Time_plot"] , y = Ymax_zoom + 0.75*Ymax_zoom, label = LOG_SERIES[plot_HIDE_RUNs_n+k, "COEFF_FLUX"], hjust = 0)
    k <- k + 1
  }

  #### export evS facets pdf
  pdf_path <- paste(path_out_COMPO, "_pf_evS.pdf", sep = "")
  dev.new()
  pdf(pdf_path, width = 15, height = 10, pointsize = 1, useDingbats = FALSE)
  suppressWarnings(print(evS_plot_facet))
  graphics.off()

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# EDIT LOCAL COMPO MASTER XLSX #----
  # compo_master_excel_path <-  paste(path_out_COMPO, "_COMPO_MASTER.xlsx", sep = "")
  compo_master_excel_path <-  paste(path_out_COMPO, "_MASTER.xlsx", sep = "")

  writexl::write_xlsx(list(RUN_LIST = RUN_LIST,
                           FORCING_RAYLEIGH = as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_RAYLEIGH")),
                           FORCING_SIZE = as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_SIZE")),
                           FORCING_DELTA = as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_DELTA")),
                           FORCING_ALPHA =  as.data.frame(readxl::read_excel(COMPO_MASTER, "FORCING_ALPHA"))),
                      compo_master_excel_path)
  beepr::beep(sound = 10)
}
