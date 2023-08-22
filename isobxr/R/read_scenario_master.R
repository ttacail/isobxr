#  #_________________________________________________________________________80char
#' Read and inspect scenario master files
#' @description A function to read and inspect the scenario master files
#' and obtain a master formatted list.
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' (character string)
#'
#' @param scenario_master_file  Name of \strong{\emph{scenario excel master file}}.
#' (without file "xlsx" extension).
#'
#' @param isobxr_master_file Name of \strong{\emph{isobxr excel master file}}.
#' (without file "xlsx" extension).
#' Default is "0_ISOBXR_MASTER".
#'
#' @return List of formatted scenario master inputs.
#'
#' @export
#' @examples
#' read.scenario_master(workdir = "/Users/username/Documents/1_ABC_tutorial",
#'                      scenario_master_file = "0_SCENARIO_source_change",
#'                      isobxr_master_file = "0_ISOBXR_MASTER")
#'
read.scenario_master <- function(workdir,
                                 scenario_master_file,
                                 isobxr_master_file = "0_ISOBXR_MASTER"){
  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list=ls())
  # gc()
  # devtools::load_all(".")
  #
  # workdir_root <- "/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/"
  # workdir <- paste(workdir_root, "1_ABCD_dev", sep = "")
  # scenario_master_file <- "0_COMPO_MASTER_balance_change"
  # isobxr_master_file <- "0_ISOBXR_MASTER"

  # 0. tuto mode ####
  tuto_setup <- using_extdata_tutorial_2(workdir = workdir,
                                         save_outputs = FALSE,
                                         plot_results = FALSE)

  workdir <- tuto_setup$workdir
  if (tuto_setup$tuto_mode) isobxr_master_file <- "0_ISOBXR_MASTER.xlsx"

  # 1. read and check isobxr master ####
  isobxr_master <- read.isobxr_master(workdir, isobxr_master_file, inspect = T, export_rds = F)

  # 1. check arguments ####
  if(!dir.exists(workdir)) rlang::abort("Workdir does not exist.")
  path.file <- paste0(workdir, "/", scenario_master_file, ".xlsx")
  if(!file.exists(path.file)) rlang::abort("Scenario master file not found.")

  # 2. import ####
  master.sce <- NULL

  master.sce$RUN_SEQUENCE <- readxl::read_excel(path.file, "RUN_SEQUENCE") %>%
    as.data.frame()

  master.sce$FORCING_RAYLEIGH <- readxl::read_excel(path.file, "FORCING_RAYLEIGH") %>%
    as.data.frame()

  master.sce$FORCING_SIZE <- readxl::read_excel(path.file, "FORCING_SIZE") %>%
    as.data.frame()

  master.sce$FORCING_DELTA <- readxl::read_excel(path.file, "FORCING_DELTA") %>%
    as.data.frame()

  master.sce$FORCING_ALPHA <- readxl::read_excel(path.file, "FORCING_ALPHA") %>%
    as.data.frame()

  # 3. to factor if character ####
  # master.sce$RUN_SEQUENCE <- master.sce$RUN_SEQUENCE %>% dplyr::mutate(across(where(is.character), as.factor)) %>% dplyr::arrange(RUN_n)
  master.sce$RUN_SEQUENCE <- master.sce$RUN_SEQUENCE %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)
  # master.sce$FORCING_RAYLEIGH <- master.sce$FORCING_RAYLEIGH %>% dplyr::mutate(across(where(is.character), as.factor)) %>% dplyr::arrange(RUN_n)
  master.sce$FORCING_RAYLEIGH <- master.sce$FORCING_RAYLEIGH %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)
  # master.sce$FORCING_SIZE <- master.sce$FORCING_SIZE %>% dplyr::mutate(across(where(is.character), as.factor)) %>% dplyr::arrange(RUN_n)
  master.sce$FORCING_SIZE <- master.sce$FORCING_SIZE %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)
  # master.sce$FORCING_DELTA <- master.sce$FORCING_DELTA %>% dplyr::mutate(across(where(is.character), as.factor)) %>% dplyr::arrange(RUN_n)
  master.sce$FORCING_DELTA <- master.sce$FORCING_DELTA %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)
  # master.sce$FORCING_ALPHA <- master.sce$FORCING_ALPHA %>% dplyr::mutate(across(where(is.character), as.factor)) %>% dplyr::arrange(RUN_n)
  master.sce$FORCING_ALPHA <- master.sce$FORCING_ALPHA %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)

  # 6. inspect ####
  # _i. check master.sce colnames ####
  for (i in 1:length(master.sce)){
    loc.df <- names(master.sce)[i]
    if(!all(names(master.sce[[loc.df]]) == master.names$scenario[[loc.df]])){
      rlang::abort(paste("Column names of the", loc.df, "spreadsheet should be:",
                         paste(master.names$scenario[[loc.df]], collapse = ", ")))
    }
  }

  # _ii. check RUN_SEQUENCE spreadsheet ####
  if(!master.sce$RUN_SEQUENCE$RUN_n %>% is.numeric()){
    rlang::abort("RUN_n in RUN_SEQUENCE spreadsheet are not numeric.")
  }

  if(!(min(master.sce$RUN_SEQUENCE$RUN_n) == 1)){
    rlang::abort("RUN_n in RUN_SEQUENCE spreadsheet does not start with 1.")
  }

  if(!(all(master.sce$RUN_SEQUENCE$RUN_n == seq(1, nrow(master.sce$RUN_SEQUENCE))))){
    rlang::abort("RUN_n in RUN_SEQUENCE spreadsheet are not successive integers.")
  }

  if(!(master.sce$RUN_SEQUENCE$t_max %>% is.numeric())){
    rlang::abort("t_max in RUN_SEQUENCE spreadsheet are not numeric.")
  }

  if(!(master.sce$RUN_SEQUENCE$n_steps %>% is.numeric())){
    rlang::abort("n_steps in RUN_SEQUENCE spreadsheet are not numeric.")
  }

  if(!(all(master.sce$RUN_SEQUENCE$flux_list %in% names(isobxr_master$FLUXES)[!names(isobxr_master$FLUXES) %in% master.names$scenario$FLUXES]))){
    rlang::abort("All flux_list values in RUN_SEQUENCE spreadsheet are not defined in isobxr master file FLUXES spreadsheet.")
  }

  if(!(all(master.sce$RUN_SEQUENCE$coeff_list %in% names(isobxr_master$COEFFS)[!names(isobxr_master$COEFFS) %in%  master.names$scenario$COEFFS]))){
    rlang::abort("All coeff_list values in RUN_SEQUENCE spreadsheet are not defined in isobxr master file COEFFS spreadsheet.")
  }

  # _iii. check FORCING_RAYLEIGH ####
  if (nrow(master.sce$FORCING_RAYLEIGH) > 0){
    if(!all(master.sce$FORCING_RAYLEIGH$RUN_n %in% master.sce$RUN_SEQUENCE$RUN_n)){
      rlang::abort("All RUN_n in FORCING_RAYLEIGH spreadsheet are not defined in RUN_SEQUENCE spreadsheet.")
    }
    for (i in 1:length(master.sce$FORCING_RAYLEIGH$RUN_n)){
      RUN_n <- sort(unique(master.sce$FORCING_RAYLEIGH$RUN_n))[i]
      check_FORCINGS(type = "RAYLEIGH",
                      FORCING = master.sce$FORCING_RAYLEIGH[master.sce$FORCING_RAYLEIGH$RUN_n %in% RUN_n,
                                                            names.forcing$FORCING_RAYLEIGH],
                      isobxr_master = isobxr_master )
    }
  }

  # _iv. check FORCING_SIZE ####
  if (nrow(master.sce$FORCING_SIZE) > 0){
    if(!all(master.sce$FORCING_SIZE$RUN_n %in% master.sce$RUN_SEQUENCE$RUN_n)){
      rlang::abort("All RUN_n in FORCING_SIZE spreadsheet are not defined in RUN_SEQUENCE spreadsheet.")
    }
    for (i in 1:length(master.sce$FORCING_SIZE$RUN_n)){
      RUN_n <- sort(unique(master.sce$FORCING_SIZE$RUN_n))[i]
      check_FORCINGS(type = "SIZE",
                      FORCING = master.sce$FORCING_SIZE[master.sce$FORCING_SIZE$RUN_n %in% RUN_n,
                                                            names.forcing$FORCING_SIZE],
                      isobxr_master = isobxr_master )
    }
  }

  # _iv. check FORCING_DELTA ####
  if (nrow(master.sce$FORCING_DELTA) > 0){
    if(!all(master.sce$FORCING_DELTA$RUN_n %in% master.sce$RUN_SEQUENCE$RUN_n)){
      rlang::abort("All RUN_n in FORCING_DELTA spreadsheet are not defined in RUN_SEQUENCE spreadsheet.")
    }
    for (i in 1:length(master.sce$FORCING_DELTA$RUN_n)){
      RUN_n <- sort(unique(master.sce$FORCING_DELTA$RUN_n))[i]
      check_FORCINGS(type = "DELTA",
                      FORCING = master.sce$FORCING_DELTA[master.sce$FORCING_DELTA$RUN_n %in% RUN_n,
                                                        names.forcing$FORCING_DELTA],
                      isobxr_master = isobxr_master )
    }
  }

  # _iv. check FORCING_ALPHA ####
  if (nrow(master.sce$FORCING_ALPHA) > 0){
    if(!all(master.sce$FORCING_ALPHA$RUN_n %in% master.sce$RUN_SEQUENCE$RUN_n)){
      rlang::abort("All RUN_n in FORCING_ALPHA spreadsheet are not defined in RUN_SEQUENCE spreadsheet.")
    }
    for (i in 1:length(master.sce$FORCING_ALPHA$RUN_n)){
      RUN_n <- sort(unique(master.sce$FORCING_ALPHA$RUN_n))[i]
      check_FORCINGS(type = "ALPHA",
                      FORCING = master.sce$FORCING_ALPHA[master.sce$FORCING_ALPHA$RUN_n %in% RUN_n,
                                                         names.forcing$FORCING_ALPHA],
                      isobxr_master = isobxr_master )
    }
  }

  return(master.sce)
}

