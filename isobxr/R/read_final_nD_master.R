#  #_________________________________________________________________________80char
#' Read and inspect sweep.final_nD master files
#' @description  A function to read and inspect the sweep.final_nD master files
#' and obtain a master formatted list.
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' (character string)
#' @param final_nD_master_file Name of \strong{\emph{sweep.final_nD excel master file}}.
#' (without file "xlsx" extension).
#' @param isobxr_master_file Name of \strong{\emph{isobxr excel master file}}.
#' (without file "xlsx" extension).
#' Default is "0_ISOBXR_MASTER".
#'
#' @return List of formatted sweep.final_nD master inputs
#'
#' @details List contains:
#' \enumerate{
#' \item \strong{param_space} data frame of shuffled all combinations of all parameters values.
#' \item \strong{sweep.DEFAULT} data frame of default run conditions \cr
#' (flux and coeff lists, t_max, chunk size)
#' \item \strong{sweep_lists_ids} list of names of swept parameters
#' }
#'
#' @export
#' @examples
#' read.final_nD_master(workdir = "/Users/username/Documents/1_ABC_tutorial",
#'                      final_nD_master_file = "0_SWEEP_FINnD_demo",
#'                      isobxr_master_file = "0_ISOBXR_MASTER")
#'
read.final_nD_master <- function(workdir,
                                 final_nD_master_file,
                                 isobxr_master_file){

  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list=ls())
  # gc()
  # devtools::load_all(".")
  #
  # workdir_root <- "/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/"
  # workdir <- paste(workdir_root, "1_ABCD_dev", sep = "")
  # final_nD_master_file <- "0_SWEEP_FINAL_nD_1"
  # isobxr_master_file <- "0_ISOBXR_MASTER"

  # 0. tuto mode ####
  tuto_setup <- using_extdata_tutorial_2(workdir = workdir,
                                         save_outputs = FALSE,
                                         plot_results = FALSE)

  workdir <- tuto_setup$workdir
  if (tuto_setup$tuto_mode) isobxr_master_file <- "0_ISOBXR_MASTER.xlsx"

  # 1. read and check isobxr master ####
  isobxr_master <- read.isobxr_master(workdir, isobxr_master_file, inspect = T, export_rds = F)

  # 2. check arguments ####
  if(!dir.exists(workdir)) rlang::abort("Workdir does not exist.")
  if (stringr::str_ends(final_nD_master_file, pattern = ".xlsx")){
    path.file <- paste0(workdir, "/", final_nD_master_file)
  } else {
    path.file <- paste0(workdir, "/", final_nD_master_file, ".xlsx")
  }
  if(!file.exists(path.file)) rlang::abort("sweep.final_nD master file not found.")

  # 2. import ####
  master.FINnD <- NULL

  master.FINnD$sweep.DEFAULT <- readxl::read_excel(path.file, "DEFAULT_RUN") %>%
    as.data.frame()

  master.FINnD$sweep.LISTS <- readxl::read_excel(path.file, "SWEEP_LISTS") %>%
    as.data.frame()

  master.FINnD$sweep.DELTA <- readxl::read_excel(path.file, "SWEEP_DELTA") %>%
    as.data.frame()

  master.FINnD$sweep.SIZE <- readxl::read_excel(path.file, "SWEEP_SIZE") %>%
    as.data.frame()

  master.FINnD$sweep.ALPHA <- readxl::read_excel(path.file, "SWEEP_ALPHA") %>%
    as.data.frame()

  master.FINnD$sweep.RAYLEIGH <- readxl::read_excel(path.file, "SWEEP_RAYLEIGH") %>%
    as.data.frame()

  # 3. to factor if character ####
  # master.FINnD$sweep.DEFAULT <- master.FINnD$sweep.DEFAULT %>% dplyr::mutate(across(where(is.character), as.factor))
  # master.FINnD$sweep.LISTS <- master.FINnD$sweep.LISTS %>% dplyr::mutate(across(where(is.character), as.factor))
  # master.FINnD$sweep.DELTA <- master.FINnD$sweep.DELTA %>% dplyr::mutate(across(where(is.character), as.factor))
  # master.FINnD$sweep.SIZE <- master.FINnD$sweep.SIZE %>% dplyr::mutate(across(where(is.character), as.factor))
  # master.FINnD$sweep.ALPHA <- master.FINnD$sweep.ALPHA %>% dplyr::mutate(across(where(is.character), as.factor))
  # master.FINnD$sweep.RAYLEIGH <- master.FINnD$sweep.RAYLEIGH %>% dplyr::mutate(across(where(is.character), as.factor))

  sweep_lists_ids <- NULL
  exp_list <- NULL

  # check
  if (nrow(master.FINnD$sweep.DEFAULT) != 1){
    rlang::abort("Default run conditions defined in DEFAULT_RUN spreadsheet should be exactly one row.")
  }

  # 4. extract flux_list, coeff_list #####
  if (nrow(master.FINnD$sweep.LISTS) == 0){
    master.FINnD$sweep.flux_list = NULL
    master.FINnD$sweep.coeff_list = NULL
  } else if(!all(names(master.FINnD$sweep.LISTS) == master.names$final_nD$SWEEP_LIST)){
    rlang::abort(paste0("SWEEP_LISTs headers should be ", paste(master.names$final_nD$SWEEP_LIST, collapse = " and "), sep = ""))
  } else {
    master.FINnD$sweep.flux_list <- master.FINnD$sweep.LISTS$flux_list
    master.FINnD$sweep.coeff_list <- master.FINnD$sweep.LISTS$coeff_list
    if(is.null(master.FINnD$sweep.flux_list) | all(is.na(master.FINnD$sweep.flux_list))){
      master.FINnD$sweep.flux_list <- NULL
    } else {
      master.FINnD$sweep.flux_list <- master.FINnD$sweep.flux_list[!is.na(master.FINnD$sweep.flux_list)]
      sweep_lists_ids <- c(sweep_lists_ids, "flux_list_name")
      exp_list <- list(master.FINnD$sweep.flux_list)
    }
    if(is.null(master.FINnD$sweep.coeff_list)| all(is.na(master.FINnD$sweep.coeff_list))){
      master.FINnD$sweep.coeff_list <- NULL
    } else {
      master.FINnD$sweep.coeff_list <- master.FINnD$sweep.coeff_list[!is.na(master.FINnD$sweep.coeff_list)]
      sweep_lists_ids <- c(sweep_lists_ids, "coeff_list_name")
      exp_list <- append(exp_list, list(master.FINnD$sweep.coeff_list))
    }
  }

  # 5. extract alpha ####
  sweep.LOC <- master.FINnD$sweep.ALPHA
  if (nrow(sweep.LOC) == 0){
    sweep.LOC =  NULL
  } else {
    sweep.LOC$A.FROM_TO <- paste0("A." , sweep.LOC$FROM, "_", sweep.LOC$TO)
    for (i in 1:nrow(sweep.LOC)){
      sweep.LOC.loc.id <- sweep.LOC[i,"A.FROM_TO"]
      sweep.LOC.loc <- seq(min(sweep.LOC[i,c("min.ALPHA", "max.ALPHA")]),
                           max(sweep.LOC[i,c("min.ALPHA", "max.ALPHA")]),
                           length.out = sweep.LOC[i,"nsteps.ALPHA"])
      sweep_lists_ids <- c(sweep_lists_ids, sweep.LOC.loc.id)
      exp_list <- append(exp_list, list(sweep.LOC.loc))
    }
  }

  # 6. extract delta ####
  sweep.LOC <- master.FINnD$sweep.DELTA
  if (nrow(sweep.LOC) == 0){
    sweep.LOC =  NULL
  } else {
    sweep.LOC$BOX_ID <- paste0("D." , sweep.LOC$BOX_ID)
    for (i in 1:nrow(sweep.LOC)){
      sweep.LOC.loc.id <- sweep.LOC[i,"BOX_ID"]
      sweep.LOC.loc <- seq(min(sweep.LOC[i,c("min.DELTA.t0", "max.DELTA.t0")]),
                           max(sweep.LOC[i,c("min.DELTA.t0", "max.DELTA.t0")]),
                           length.out = sweep.LOC[i,"nsteps.DELTA.t0"])
      sweep_lists_ids <- c(sweep_lists_ids, sweep.LOC.loc.id)
      exp_list <- append(exp_list, list(sweep.LOC.loc))
    }
  }

  # 7. extract size ####
  sweep.LOC <- master.FINnD$sweep.SIZE
  if (nrow(sweep.LOC) == 0){
    sweep.LOC =  NULL
  } else {
    sweep.LOC$BOX_ID <- paste0("S." , sweep.LOC$BOX_ID)
    for (i in 1:nrow(sweep.LOC)){
      sweep.LOC.loc.id <- sweep.LOC[i,"BOX_ID"]
      sweep.LOC.loc <- seq(min(sweep.LOC[i,c("min.SIZE.t0", "max.SIZE.t0")]),
                           max(sweep.LOC[i,c("min.SIZE.t0", "max.SIZE.t0")]),
                           length.out = sweep.LOC[i,"nsteps.SIZE.t0"])
      sweep_lists_ids <- c(sweep_lists_ids, sweep.LOC.loc.id)
      exp_list <- append(exp_list, list(sweep.LOC.loc))
    }
  }

  # 8. extract Rayleigh #####
  sweep.LOC <- master.FINnD$sweep.RAYLEIGH
  if (nrow(sweep.LOC) == 0){
    sweep.LOC =  NULL
  } else {
    sweep.LOC$R.XFROM_TO.YFROM_TO.AFROM_TO <- paste0("R",
                                                     ".", sweep.LOC$XFROM, "_", sweep.LOC$XTO,
                                                     ".", sweep.LOC$YFROM, "_", sweep.LOC$YTO,
                                                     ".", sweep.LOC$AFROM, "_", sweep.LOC$ATO)
    for (i in 1:nrow(sweep.LOC)){
      sweep.LOC.loc.id <- sweep.LOC[i,"R.XFROM_TO.YFROM_TO.AFROM_TO"]
      sweep.LOC.loc <- seq(min(sweep.LOC[i,c("min.ALPHA_0", "max.ALPHA_0")]),
                           max(sweep.LOC[i,c("min.ALPHA_0", "max.ALPHA_0")]),
                           length.out = sweep.LOC[i,"nsteps.ALPHA_0"])
      sweep_lists_ids <- c(sweep_lists_ids, sweep.LOC.loc.id)
      exp_list <- append(exp_list, list(sweep.LOC.loc))
    }
  }

  # 9. define sweep space ####
  sweep.space <- expand.grid(exp_list)
  names(sweep.space) <- sweep_lists_ids

  # 10. shuffle sweep_space
  sweep.space <- sweep.space[sample(1:nrow(sweep.space)), ]
  rownames(sweep.space) <- seq(1,nrow(sweep.space),1)
  return(list(param_space = sweep.space,
              sweep.DEFAULT = master.FINnD$sweep.DEFAULT,
              sweep_lists_ids = sweep_lists_ids))
}
