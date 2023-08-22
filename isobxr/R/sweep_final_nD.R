#_________________________________________________________________________80char
#' Sweep the space of n parameters at the final state of a system
#'
#' @description  A function to assess the influence of n parameters (varying
#' over a range of values) on the final state of a system.
#'
#' @param workdir Working directory of \strong{\emph{0_ISOBXR_MASTER.xlsx}} master file, \cr
#' of the dynamic sweep master file (e.g., \strong{\emph{0_EXPLO_DYN_MASTER.xlsx}}) \cr
#' and where output files will be stored if saved by user. \cr
#' (character string)
#' @param sweep_master_file Name of \strong{\emph{sweep.final_nD excel master file}}.
#' (without file "xlsx" extension).
#' @param sweep_dir_to_complete Name of directory of previously halted sweep.final_nD
#' run that the user wishes to continue. Starts with "4_FINnD". \cr
#' Default is NULL.
#' @param export.data_as_csv_xlsx If TRUE, exports chunk sweep result data as csv and xlsx fo full
#' to chunk digest directory. \cr
#' Default is FALSE.
#' @param isobxr_master_file Name of \strong{\emph{isobxr excel master file}}. \cr
#' Default is "0_ISOBXR_MASTER".
#' @param save_outputs If TRUE, saves all run outputs to local working directory (workdir). \cr
#' By default, run outputs are stored in a temporary directory and erased if not saved.
#' Default is FALSE.
#'
#' @return Delta values and box sizes at final state of the system, in the n-dimensions space of parameters.
#' sweep.final_nD outputs are saved to workdir if save_outputs = TRUE.
#'
#' @section sweep.final_nD outputs:
#' \enumerate{
#' \item \strong{digest sweep.final_nD outputs} full sweep.final_nD outputs are stored in a digest directory directly in workdir:
#' \enumerate{
#' \item \strong{isobxr master file archive} as xlsx
#' \item \strong{global chunk_log} as csv
#' \item \strong{global chunk_log} as rds
#' \item \strong{sweep.final_nD master file archive} as xlsx
#' \item \strong{sweep.final_nD merged chunks LOG excerpt} as csv
#' \item \strong{sweep.final_nD merged param_space}, current progress, as rds (space of swept parameters)
#' \item \strong{sweep.final_nD merged results}, current progress, as rds
#' \enumerate{
#' \item data frame containing delta and size at final state across whole n-dimensions space of parameters
#' \item \strong{sweep.final_nD param_space} as planned, as rds
#' \item \strong{sweep.final_nD sweep_default} data frame of default run conditions, as rds
#' \item \strong{sweep.final_nD sweep progress plot} as pdf, follows merging of chunks
#' }
#' }
#' \item \strong{chunks sweep.final_nD outputs} chunk directories are edited in case total number.
#'  of runs exceeds chunk size defined in sweep.final_nD master file. They are removed after full sweep.
#' }
#'
#' @export
sweep.final_nD <- function(workdir,
                           sweep_master_file, # formerly EXPLO_MASTER
                           sweep_dir_to_complete = NULL, # formerly dir.space_digest.to_complete / corresponds to the digest dir.
                           export.data_as_csv_xlsx = FALSE, # formerly to_STD_DIGEST_CSVs
                           isobxr_master_file = "0_ISOBXR_MASTER",
                           save_outputs = FALSE # formerly save_run_outputs
){

  # # # 0. debug arguments ####
  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list = ls())
  # gc()
  # devtools::load_all(".")
  #
  # # workdir <- "/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/1_ABCD_dev"
  # # sweep_dir_to_complete = NULL
  # # export.data_as_csv_xlsx = FALSE
  # # sweep_master_file <- "0_SWEEP_FINAL_nD_1"
  # # isobxr_master_file <- "0_ISOBXR_MASTER"
  # workdir = "/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/1_ABCD_dev"
  # sweep_dir_to_complete = NULL
  # export.data_as_csv_xlsx = FALSE
  # sweep_master_file = "0_SWEEP_FINnD_demo"
  # isobxr_master_file = "0_ISOBXR_MASTER"
  # save_outputs = TRUE

  # I. check arguments ####
  args <- c(as.list(environment()))
  rm(list=ls()[ls() != "args"])

  args.allowed <- list(logical = c("export.data_as_csv_xlsx"))

  for (i in 1:length(args.allowed$logical)){
    if (!is.logical(eval(parse(text = paste0("args$", args.allowed$logical[i]))))){
      rlang::abort(paste0("\"", args.allowed$logical[i], "\" argument should be logical."))
    }
  }

  # ############################################################ DEV ONLY
  # dyn2d_results <-
  #   readRDS("/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/1_ABCD_dev/4_DYN_sweep_dyn_test_0001/0_DYN_DIGEST/DYN_sweep_dyn_test_0001_results.rds")
  #
  # chunk_evD_final <- readRDS("/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/obs vs sim_3/4_FINnD_0_SWEEP_n_d_1_001_001/SPACE_0_SWEEP_n_d_1_001_001_evD_final.rds")
  #
  # ############################################################ DEV ONLY


  # II. Initiate ####

  # _a. locally bind variables ####
  # evD_final <- chunk_n <- n_zeros <- old.acronym <- old.prefix <- func <- NULL
  evD_final <- chunk_n <- n_zeros <- acronym <- prefix <- func <- NULL

  # _b. set workdir ####
  # __i. determine function mode (tuto/user) ####
  fun_mode <- using_extdata_tutorial_2(workdir = args$workdir,
                                      save_outputs = args$save_outputs,
                                      plot_results = args$show.delta_plot)
  args$workdir <- fun_mode$workdir
  paths <- list(workdir = fun_mode$workdir)
  if (fun_mode$tuto_mode) args$isobxr_master_file <- "0_ISOBXR_MASTER"
  paths$isobxr_master_file <- args$isobxr_master_file
  paths$sweep_master_file <- args$sweep_master_file <- stringr::str_remove_all(args$sweep_master_file, pattern = ".xlsx")
  args$show.delta_plot <- fun_mode$plot_results
  args$save_outputs <- fun_mode$save_outputs

  # paths$prefix <- names.output %>%
  #   dplyr::filter(func == "sweep.final_nD") %>%
  #   dplyr::pull(old.prefix)
  #
  # paths$acronym <- names.output %>%
  #   dplyr::filter(func == "sweep.final_nD") %>%
  #   dplyr::pull(old.acronym)

  paths$prefix <- names.output %>%
    dplyr::filter(func == "sweep.final_nD") %>%
    dplyr::pull(prefix)

  paths$acronym <- names.output %>%
    dplyr::filter(func == "sweep.final_nD") %>%
    dplyr::pull(acronym)


  # __ii. set workdir ####
  workdir_old <- getwd()
  on.exit(setwd(workdir_old), add = TRUE)
  setwd(paths$workdir)

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

  # _c. import isobxr master ####
  master.isobxr <-
    read.isobxr_master(
      workdir = args$workdir,
      isobxr_master_file = args$isobxr_master_file,
      inspect = T,
      export_rds = F
    )

  # _c. check matching sweep_space series ####
  paths$LOG_file <- "1_LOG.csv"
  n_zeros_SERIES <- 3 # optional - defines number of series per sweep family - here max 999 chunks

  if (file.exists(paths$LOG_file)){
    # LOG ####
    # file.copy(from = paths$LOG_file, to = to_tmpdir(paths$LOG_file))
    # LOG <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T)
    newLOG <- FALSE
    LOG <- data.table::fread(paths$LOG_file, data.table = F, stringsAsFactors = T)
    LOG_EXPLO <- LOG[LOG$EXPLORER == TRUE, ]
    remove(LOG)
    if (nrow(LOG_EXPLO) > 0){
      previous.space_sweep.SERIES <- as.character(unique(LOG_EXPLO$EXPLO_SERIES_FAMILY))
      previous.space_sweep.FAMILIES <- data.frame(space_sweep.FAMILY = "NaN", space_sweep.FAMILY.n = NaN)
      for (i in 1:length(previous.space_sweep.SERIES)){
        space_sweep.SERIES <- previous.space_sweep.SERIES[i]
        split.space_sweep.SERIES <- stringr::str_split(space_sweep.SERIES, pattern = "_")[[1]]
        if(split.space_sweep.SERIES[1] == paths$acronym){
          space_sweep.FAMILY <- paste(split.space_sweep.SERIES[1:(length(split.space_sweep.SERIES)-1)], collapse = "_")
          previous.space_sweep.FAMILIES.loc <- data.frame(space_sweep.FAMILY = "NaN", space_sweep.FAMILY.n = NaN)
          previous.space_sweep.FAMILIES.loc$space_sweep.FAMILY <- space_sweep.FAMILY
          previous.space_sweep.FAMILIES.loc$space_sweep.FAMILY.n <- as.numeric(split.space_sweep.SERIES[length(split.space_sweep.SERIES)])
          previous.space_sweep.FAMILIES <- dplyr::bind_rows(previous.space_sweep.FAMILIES, previous.space_sweep.FAMILIES.loc)
          remove(previous.space_sweep.FAMILIES.loc)
        }
      }
      previous.space_sweep.FAMILIES <- clear_subset(previous.space_sweep.FAMILIES[-1,])
      previous.space_sweep.FAMILIES <- as.data.frame(unclass(previous.space_sweep.FAMILIES), stringsAsFactors = TRUE)
      paths$current.space_sweep.FAMILY <- paste0(paths$acronym, "_", stringr::str_remove_all(args$sweep_master_file, pattern = ".xlsx"))

      if (paths$current.space_sweep.FAMILY %in% previous.space_sweep.FAMILIES$space_sweep.FAMILY){
        rlang::inform(paste0("A sweep_space run was previously initiated using the same SWEEP_SPACE_MASTER file (xlsx)."))
        space_sweep.FAMILY.n.loc <- max(previous.space_sweep.FAMILIES[previous.space_sweep.FAMILIES ==
                                                                        paths$current.space_sweep.FAMILY, "space_sweep.FAMILY.n"])+1
        paths$current.space_sweep.SERIES <- paste0(paths$current.space_sweep.FAMILY, "_",
                                                   sprintf(paste0("%0", n_zeros_SERIES, "d"), space_sweep.FAMILY.n.loc))
      } else {
        paths$current.space_sweep.SERIES <- paste0(paths$current.space_sweep.FAMILY, "_", sprintf(paste0("%0", n_zeros_SERIES, "d"), 1))
      }
    } else {
      paths$current.space_sweep.FAMILY <- paste0(paths$acronym, "_", stringr::str_remove_all(args$sweep_master_file, pattern = ".xlsx"))
      paths$current.space_sweep.SERIES <- paste0(paths$current.space_sweep.FAMILY, "_", sprintf(paste0("%0", n_zeros_SERIES, "d"), 1))
    }
      # remove(split.space_sweep.SERIES, space_sweep.SERIES, previous.space_sweep.SERIES, previous.space_sweep.FAMILIES, LOG_EXPLO)
  } else {
    newLOG <- TRUE
    paths$current.space_sweep.FAMILY <- paste0(paths$acronym, "_", stringr::str_remove_all(args$sweep_master_file, pattern = ".xlsx"))
    paths$current.space_sweep.SERIES <- paste0(paths$current.space_sweep.FAMILY, "_", sprintf(paste0("%0", n_zeros_SERIES, "d"), 1))
  }

  # _d. define digest, FAMILY and SERIES directories ####
  n_zeros_CHUNKS <- 3

  if (is.null(args$sweep_dir_to_complete)){
    paths$digest_dir <- paste0("4_", paths$current.space_sweep.SERIES, "_",
                               paste(as.character(rep(0,n_zeros_CHUNKS)), collapse = ""),
                               "_digest")
  } else {
    paths$digest_dir <- args$sweep_dir_to_complete
    if(!dir.exists(paths$digest_dir)){
      rlang::abort("sweep_dir_to_complete not found")
    }

    if(stringr::str_starts(args$sweep_dir_to_complete, pattern = paste0(paths$prefix,  "_")) &
       stringr::str_ends(args$sweep_dir_to_complete, pattern =
                         paste0("_", paste(as.character(rep(0, n_zeros_CHUNKS)), collapse = ""),"_digest"))){

      paths$current.space_sweep.SERIES <- stringr::str_remove(args$sweep_dir_to_complete, pattern = paste0(paths$prefix,  "_"))
      paths$current.space_sweep.SERIES <- stringr::str_remove(paths$current.space_sweep.SERIES,
                                                              pattern = paste0("_", paste(as.character(rep(0,n_zeros_CHUNKS)), collapse = ""),
                                                                               "_digest"))
      paths$current.space_sweep.SERIES <- paste0(paths$acronym,  "_", paths$current.space_sweep.SERIES)
      paths$current.space_sweep.FAMILY <- stringr::str_split(paths$current.space_sweep.SERIES, pattern = "_")[[1]]
      paths$current.space_sweep.FAMILY <- paste(paths$current.space_sweep.FAMILY[1:(length(paths$current.space_sweep.FAMILY)-1)],
                                                collapse = "_")

    } else {
      rlang::abort(paste0("sweep_dir_to_complete is not fitting expected format: \n ", paths$prefix, "_", "NameOfSweepFinalnDMasterFile",
                          "_", paste(rep("X", n_zeros), collapse = ""), "_", paste(rep("0", n_zeros_CHUNKS), collapse = ""), "_",
                          "digest")
      )
    }
  }

  # _e. define param_space and paths$digest_dir content #####

  # tempdir? ####
  if (!dir.exists(paths$digest_dir)){
    first_attempt <- TRUE
    master.sweep <- read.final_nD_master(workdir = paths$workdir,
                                         final_nD_master_file = args$sweep_master_file,
                                         isobxr_master_file = args$isobxr_master_file)
    dir.create(to_tmpdir(paths$digest_dir))
    file.copy(from = paste0(args$sweep_master_file, ".xlsx"),
              to = to_tmpdir(paste0(paths$digest_dir, "/", paths$current.space_sweep.SERIES, "_MASTER.xlsx")),
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE)
    file.copy(from = paste0(args$isobxr_master_file, ".xlsx"),
              to = to_tmpdir(paste0(paths$digest_dir, "/",
                                    paste0(args$isobxr_master_file, ".xlsx"))),
              overwrite = TRUE, recursive = FALSE,
              copy.mode = TRUE)
    saveRDS(object = master.sweep$param_space,
            file = to_tmpdir(paste0(paths$digest_dir, "/",
                                    paths$current.space_sweep.SERIES, "_param_space.RDS")))
    saveRDS(object = master.sweep$sweep.DEFAULT,
            file = to_tmpdir(paste0(paths$digest_dir, "/",
                                    paths$current.space_sweep.SERIES, "_sweep_default.RDS")))
  } else if (dir.exists(paths$digest_dir) & is.null(args$sweep_dir_to_complete)) {
    rlang::abort(paste0("Digest directory exists but no LOG was found and directory for sweep_space run continuation is not declared.",
                        " \n Advised to delete digest directory."))
  }

  if (!is.null(args$sweep_dir_to_complete)){
    first_attempt <- FALSE
    master.sweep <- NULL
    master.sweep$param_space <- readRDS(paste0(paths$digest_dir, "/",
                                               paths$current.space_sweep.SERIES, "_param_space.RDS"))
    master.sweep$sweep.DEFAULT <- readRDS(paste0(paths$digest_dir, "/",
                                                 paths$current.space_sweep.SERIES, "_sweep_default.RDS"))
    master.sweep$sweep_lists_ids <- read.final_nD_master(workdir = paths$workdir,
                                                         final_nD_master_file = paste0(paths$digest_dir, "/",
                                                                                       paths$current.space_sweep.SERIES, "_MASTER.xlsx"),
                                                         isobxr_master_file = args$isobxr_master_file)[[3]]
    paths$digest_isobxr_master <- paste0(paths$digest_dir, "/", paste0(args$isobxr_master_file, ".xlsx"))
  } else {
    paths$digest_isobxr_master <- paste0(args$isobxr_master_file, ".xlsx")
  }

  # _f. extract DEFAULT parameters #####
  if(nrow(master.sweep$sweep.DEFAULT) == 1){
    flux_list_name = master.sweep$sweep.DEFAULT[1,"flux_list"]
    coeff_list_name = master.sweep$sweep.DEFAULT[1,"coeff_list"]
    t_max = master.sweep$sweep.DEFAULT[1,"t_max"]
    swp.chunk.size = master.sweep$sweep.DEFAULT[1,"runs_per_chunk"]
  } else {
    rlang::abort("Default run conditions defined in DEFAULT_RUN spreadsheet should be exactly one row (sweep.final_nD master file).")
  }

  # _g determine master.sweep$param_space chunks completed and due #----
  tot_run <- nrow(master.sweep$param_space)
  master.sweep$param_space$chunk_n <- rep(1:tot_run, each=ceiling(swp.chunk.size), length.out=tot_run)
  master.sweep$param_space.chunks <- split(master.sweep$param_space, f = master.sweep$param_space$chunk_n)

  if(first_attempt){
    chunk_LOG <- data.frame(chunk_n = unique(master.sweep$param_space$chunk_n),
                            chunk_ID = paste0(paths$current.space_sweep.SERIES, "_",
                                              sprintf(paste0("%0", n_zeros_CHUNKS, "d"), unique(master.sweep$param_space$chunk_n))),
                            n_runs = dplyr::count(master.sweep$param_space, chunk_n)[,"n"],
                            elapsed.all = NaN,
                            elapsed.sweep = NaN,
                            elapsed.prepare = NaN,
                            elapsed.all.per_run = NaN,
                            elapsed.sweep.per_run = NaN,
                            elapsed.prepare.per_run = NaN)

    chunk_LOG$chunk_status <- as.factor("due") # or done
    levels(chunk_LOG$chunk_status) <- c(levels(chunk_LOG$chunk_status), "complete")

    saveRDS(chunk_LOG,
            file = to_tmpdir(paste0(paths$digest_dir, "/", paths$current.space_sweep.SERIES, "_chunk_log.RDS")))
    data.table::fwrite(chunk_LOG,
                       file = to_tmpdir(paste0(paths$digest_dir, "/", paths$current.space_sweep.SERIES, "_chunk_log.csv")))

  } else {
    chunk_LOG <- readRDS(paste0(paths$digest_dir, "/", paths$current.space_sweep.SERIES, "_chunk_log.RDS"))
  }

  chunk_LOG.due <- chunk_LOG[chunk_LOG$chunk_status == "due",]
  n_chunks <- nrow(chunk_LOG)
  n_chunks.due <- nrow(chunk_LOG.due)
  n_chunks.complete <- nrow(chunk_LOG[chunk_LOG$chunk_status == "complete",])

  if (n_chunks > 1 & fun_mode$tuto_mode){
    rlang::abort("In tutorial mode, sweep.final_nD can only be used for a single chunk sweeps.")
  }

  if (n_chunks.due == 0){
    rlang::abort("All chunks already completed.")
  } else if (n_chunks.complete != 0){
    rlang::inform(paste0(" You are about to continue calculations of a previous sweep_space run.",
                         "\n Chunks remaining: ", as.character(n_chunks.due), "/", as.character(n_chunks)))
  }

  # _h. ask user about total run number #----
  param_space.chunk_LOG <- dplyr::right_join(master.sweep$param_space, chunk_LOG, by = "chunk_n")
  tot_run <- nrow(param_space.chunk_LOG[param_space.chunk_LOG$chunk_status == "due", ])

  STOP_GO <- FALSE
  rlang::inform("________________________________________________________________________________")
  if(interactive()){
    if (.Platform$OS.type == "windows"){
      STOP_GO <- utils::askYesNo(paste("? This sweep requires *", as.character(tot_run),
                                       "* independent runs, do you wish to carry on?"), default = TRUE)
    } else {
      STOP_GO <- utils::askYesNo(cat("? This sweep requires *", as.character(tot_run),
                                     "* independent runs, do you wish to carry on? \n"), default = TRUE)
    }
  } else {
    rlang::inform(paste("\U2139 This full sweep requires ", as.character(tot_run), " independent runs."))
    STOP_GO <- TRUE
  }

  if (STOP_GO == FALSE){
    unlink(paths$digest_dir, recursive = TRUE)
    rlang::abort("\U2757 You probably want to reduce the number of iterations in each parameter.")
  }

  l <- 1

  # _i. merge upon exit ####
  on.exit({
    gc()
    if (n_chunks.complete >= 1){
      if(args$save_outputs){
        merge_FINnD_chunks(workdir = args$workdir,
                           FINnD_digest_dir.to_merge = paths$digest_dir,
                           save_outputs = args$save_outputs)
      }
    }
  }, add = TRUE, after = TRUE)

  # III. Run due chunks ####
  for (l in 1:n_chunks.due){

    gc()

    # _a. prepare local chunk ####

    # tempdir? ####
    # unlink(to_tmpdir(""), recursive = TRUE)
    # on.exit(unlink(to_tmpdir(""), recursive = TRUE), add = TRUE)

    chunk.loc <- chunk_LOG.due[l,]
    rlang::inform("________________________________________________________________________________")
    rlang::inform(paste0("\U2139 Calculating chunk ", chunk.loc[ , "chunk_n"], "/", n_chunks, " (", chunk.loc[,"n_runs"], " runs)"))
    tictoc::tic(paste0("All chunk ", chunk.loc[ , "chunk_n"]))
    param_space.loc <- master.sweep$param_space.chunks[[chunk.loc[,"chunk_n"]]]
    tot_run.loc <- nrow(param_space.loc)

    SERIES_ID <- paste0(paths$current.space_sweep.SERIES,"_",
                        sprintf(paste0("%0", n_zeros_CHUNKS, "d"), chunk.loc[ , "chunk_n"]))


    rlang::inform("\U0001f535 SWEEPING")
    tictoc::tic("chunk sweep:")
    pb_cpt <- utils::txtProgressBar(min = 0, max = tot_run.loc, style = 3, width = 50)
    clock <- 1
    j <- 1

    if (chunk.loc[ , "chunk_n"] == 1){
      file.copy(from = paths$LOG_file, to = to_tmpdir(paths$LOG_file))
    } else {
      data.table::fread(paths$LOG_file, data.table = F, stringsAsFactors = T) %>%
        utils::tail(1) %>%
        data.table::fwrite(file = to_tmpdir(paths$LOG_file))
    }

    ############################## START CHUNK LOOP ######
    # _b. chunk sweep for loop #####
    for (j in 1:nrow(param_space.loc)){

      # __i. prepare single run ####

      FORCING_RAYLEIGH <- FORCING_SIZE <- FORCING_DELTA <- FORCING_ALPHA <- NULL

      for (i in 1:length(master.sweep$sweep_lists_ids)){
        name_loc <- master.sweep$sweep_lists_ids[i]

        if(stringr::str_starts(name_loc, pattern = "A.")){
          name_loc_vector <- stringr::str_split(stringr::str_remove(name_loc, pattern = "A."), pattern = "_")[[1]]
          FORCING_ALPHA_loc <-
            data.frame(FROM = name_loc_vector[1],
                       TO = name_loc_vector[2],
                       ALPHA = param_space.loc[j, name_loc ],
                       FROM_TO = paste(name_loc_vector, collapse = "_"))

          if(is.null(FORCING_ALPHA)){
            FORCING_ALPHA <- FORCING_ALPHA_loc
          } else {
            FORCING_ALPHA <- dplyr::bind_rows(FORCING_ALPHA, FORCING_ALPHA_loc)
          }
        }

        if(stringr::str_starts(name_loc, pattern = "D.")){
          name_loc_vector <- stringr::str_split(stringr::str_remove(name_loc, pattern = "D."), pattern = "_")[[1]]
          FORCING_DELTA_loc <-
            data.frame(BOX_ID = name_loc_vector[1],
                       DELTA.t0 = param_space.loc[j, name_loc ])

          if(is.null(FORCING_DELTA)){
            FORCING_DELTA <- FORCING_DELTA_loc
          } else {
            FORCING_DELTA <- dplyr::bind_rows(FORCING_DELTA, FORCING_DELTA_loc)
          }
        }

        if(stringr::str_starts(name_loc, pattern = "S.")){
          name_loc_vector <- stringr::str_split(stringr::str_remove(name_loc, pattern = "S."), pattern = "_")[[1]]
          FORCING_SIZE_loc <-
            data.frame(BOX_ID = name_loc_vector[1],
                       SIZE.t0 = param_space.loc[j, name_loc ])

          if(is.null(FORCING_DELTA)){
            FORCING_SIZE <- FORCING_SIZE_loc
          } else {
            FORCING_SIZE <- dplyr::bind_rows(FORCING_SIZE, FORCING_SIZE_loc)
          }
        }

        if(stringr::str_starts(name_loc, pattern = "R.")){
          box_fluxes_loc <- strsplit(x = stringr::str_remove(name_loc, pattern = "R."),
                                     split = ".",
                                     fixed = T)[[1]]

          FORCING_RAYLEIGH_loc <- data.frame(XFROM = unlist(strsplit(box_fluxes_loc[1], "_", fixed = T))[1],
                                             XTO = unlist(strsplit(box_fluxes_loc[1], "_", fixed = T))[2],
                                             YFROM = unlist(strsplit(box_fluxes_loc[2], "_", fixed = T))[1],
                                             YTO = unlist(strsplit(box_fluxes_loc[2], "_", fixed = T))[2],
                                             AFROM = unlist(strsplit(box_fluxes_loc[3], "_", fixed = T))[1],
                                             ATO = unlist(strsplit(box_fluxes_loc[3], "_", fixed = T))[2],
                                             ALPHA_0 = param_space.loc[j, name_loc ])

          if(is.null(FORCING_ALPHA)){
            FORCING_RAYLEIGH <- FORCING_RAYLEIGH_loc
          } else {
            FORCING_RAYLEIGH <- dplyr::bind_rows(FORCING_RAYLEIGH, FORCING_RAYLEIGH_loc)
          }
        }

        if(name_loc == "flux_list_name"){
          flux_list_name <- as.character(param_space.loc[j, name_loc ])
        }

        if(name_loc == "coeff_list_name"){
          coeff_list_name <- as.character(param_space.loc[j, name_loc ])
        }
      }

      # __ii. run sim.single_run ####
      sim.single_run(
        # workdir = args$workdir,
        workdir = to_tmpdir(""),
        SERIES_ID = SERIES_ID,
        flux_list = flux_list_name,
        coeff_list = coeff_list_name,
        t_max = t_max,
        n_steps = 1,
        # isobxr_master_file = args$isobxr_master_file,
        isobxr_master = master.isobxr,
        suppress_messages = T,
        export.diagrams = F,
        export.delta_plot = F,
        export.data_as_csv_xlsx = F,
        plot.time_as_log10 = F,
        plot.time_unit = NULL,
        show.delta_plot = F,
        inspect_inputs = F,
        save_outputs = F,
        return_data = F,
        solver = "analytical",
        n_zeros_RUN_IDs = 5,
        FORCING_RAYLEIGH = FORCING_RAYLEIGH,
        FORCING_SIZE = FORCING_SIZE,
        FORCING_DELTA = FORCING_DELTA,
        FORCING_ALPHA = FORCING_ALPHA,
        COMPOSITE = FALSE,
        COMPO_SERIES_n = NaN,
        COMPO_SERIES_FAMILY = NaN,
        EXPLORER = TRUE,
        EXPLO_SERIES_n = chunk.loc[, "chunk_n"],
        EXPLO_SERIES_FAMILY = paths$current.space_sweep.SERIES
      )

      # calculation_gauge(clock, tot_run)
      utils::setTxtProgressBar(pb_cpt, clock)
      clock <- clock + 1
    }
    ############################## END CHUNK LOOP ######

    elapsed.sweep <- tictoc::toc(quiet = T)
    close(pb_cpt)

    # _c. read and merge all single run outputs from tempdir - local chunk ####
    # LOG ####
    if (newLOG){
      LOG_SERIES <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T)
    } else {
      LOG_SERIES <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T) %>% dplyr::slice(-1)
    }
    LOG_SERIES <- LOG_SERIES[LOG_SERIES$SERIES_ID == SERIES_ID, ]

    # LOG_SERIES <- data.table::fread(to_tmpdir(paths$LOG_file), data.table = F, stringsAsFactors = T) %>% dplyr::filter(SERIES_ID == SERIES_ID)

    rlang::inform("\U0001f535 PREPARING RESULTS")
    tictoc::tic("chunk result preparation:")

    # paths$paths_all_rds <- LOG_SERIES$path_outdir %>% as.character()
    #
    # # used for reading single outputs from a single chunk in sweep.final_nD prior to merging
    # # TO MOVE OUTSIDE #
    # read_single_run_output_for_FINnD <- function(path_to_rds){
    #   header_sep <- "." ### change to . in the future
    #
    #   run_rds <- path_to_rds %>%
    #     paste0(".rds") %>%
    #     to_tmpdir() %>%
    #     readRDS()
    #
    #   run_rds %>%
    #     .$outputs %>%
    #     .$delta_vs_t %>%
    #     tail(1) %>%
    #     dplyr::bind_cols(run_rds %>%
    #                        .$inputs %>%
    #                        .$LOG %>%
    #                        dplyr::select(SERIES_RUN_ID, RUN_n)) %>%
    #     dplyr::bind_cols(run_rds %>%
    #                        magrittr::extract2("inputs") %>%
    #                        magrittr::extract2("INITIAL") %>%
    #                        dplyr::rename("m0" = "SIZE.t0",
    #                                      "d0" = "DELTA.t0") %>%
    #                        tidyr::pivot_wider(names_from = BOX_ID,
    #                                           values_from = c(d0, m0),
    #                                           names_sep = header_sep) %>%
    #                        as.data.frame()
    #     ) %>%
    #     dplyr::bind_cols(run_rds %>%
    #                        magrittr::extract2("inputs") %>%
    #                        magrittr::extract2("FLUXES") %>%
    #                        tidyr::pivot_longer(cols = stringr::str_split(LOG_SERIES$BOXES_ID_list[1], pattern = "_") %>% unlist()) %>%
    #                        dplyr::rename("FROM" = "BOX_ID",
    #                                      "TO" = "name") %>%
    #                        dplyr::filter(value != 0) %>%
    #                        tidyr::unite(col = "FROM_TO", FROM, TO, sep = "_") %>%
    #                        dplyr::mutate(FROM_TO = paste("f", FROM_TO, sep = header_sep)) %>%
    #                        tidyr::pivot_wider(values_from = value,
    #                                           names_from = FROM_TO) %>%
    #                        as.data.frame()) %>%
    #     dplyr::bind_cols(run_rds %>%
    #                        magrittr::extract2("inputs") %>%
    #                        magrittr::extract2("COEFFS") %>%
    #                        tidyr::pivot_longer(cols = stringr::str_split(LOG_SERIES$BOXES_ID_list[1], pattern = "_") %>% unlist()) %>%
    #                        dplyr::rename("FROM" = "BOX_ID",
    #                                      "TO" = "name") %>%
    #                        dplyr::filter(value != 1) %>%
    #                        tidyr::unite(col = "FROM_TO", FROM, TO, sep = "_") %>%
    #                        dplyr::mutate(FROM_TO = paste("a", FROM_TO, sep = header_sep)) %>%
    #                        tidyr::pivot_wider(values_from = value,
    #                                           names_from = FROM_TO) %>%
    #                        as.data.frame())
    # }
    #
    # pb_cpt <- utils::txtProgressBar(min = 0, max = tot_run.loc, style = 3, width = 50)
    # clock <- 1
    #
    # for (i in 1:length(paths$paths_all_rds)){
    #   if (i == 1){
    #     FINnD.results.chunk <- read_single_run_output_for_FINnD(paths$paths_all_rds[i])
    #   } else {
    #     FINnD.results.chunk <- dplyr::bind_rows(FINnD.results.chunk, read_single_run_output_for_FINnD(paths$paths_all_rds[i]))
    #   }
    #   utils::setTxtProgressBar(pb_cpt, clock)
    #   clock <- clock + 1
    # }
    #
    # # FINnD.results.chunk <- paths$paths_all_rds %>%
    # #   purrr::map(read_single_run_output_for_FINnD) %>%
    # #   purrr::reduce(dplyr::bind_rows)
    #
    # elapsed.prepare <- tictoc::toc(quiet = T)
    # close(pb_cpt)

    pb_prep <- utils::txtProgressBar(min = 0, max = tot_run.loc, style = 3, width = 50)

    i <- 1
    for (i in 1:(tot_run.loc)){
      SERIES_RUN_ID_i <- LOG_SERIES[i, "SERIES_RUN_ID"]
      RUN_n_i <- LOG_SERIES[i, "RUN_n"]
      path_outdir_i <- as.character(LOG_SERIES[i, "path_outdir"])

      run_rds <- path_outdir_i %>%
        paste0(".rds") %>%
        to_tmpdir() %>%
        readRDS()

      SIZE_INIT_i <- run_rds$inputs$INITIAL[,c("BOX_ID", "SIZE.t0")]
      DELTA_INIT_i <- run_rds$inputs$INITIAL[,c("BOX_ID", "DELTA.t0")]
      FLUXES_i <- run_rds$inputs$FLUXES
      COEFFS_i <- run_rds$inputs$COEFFS
      unlink(to_tmpdir(path_outdir_i %>% paste0(".rds")), recursive = TRUE)
      evD_i <- run_rds$outputs$delta_vs_t %>% utils::tail(1)

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
          FLUXES_i_vert_loc$VARIABLE = paste("f", paste0(FLUXES_i[j, "BOX_ID"], "_", FLUXES_i_colnames[k+1]), sep = ".")
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
          COEFFS_i_vert_loc$VARIABLE = paste("a", paste0(COEFFS_i[j, "BOX_ID"], "_", COEFFS_i_colnames[k+1]), sep = ".")
          COEFFS_i_vert <- rbind(COEFFS_i_vert, COEFFS_i_vert_loc)
        }
      }
      COEFFS_i_vert <- COEFFS_i_vert[2:nrow(COEFFS_i_vert),]

      SIZE_INIT_i_vert <- SIZE_INIT_i
      DELTA_INIT_i_vert <- DELTA_INIT_i
      SIZE_INIT_i_vert$VAR_TYPE <- "SIZE.t0"
      DELTA_INIT_i_vert$VAR_TYPE <- "DELTA.t0"
      SIZE_INIT_i_vert$VARIABLE <- paste("m0", paste(SIZE_INIT_i_vert$BOX_ID, sep = "_"), sep = ".")
      DELTA_INIT_i_vert$VARIABLE <- paste("d0", paste(DELTA_INIT_i_vert$BOX_ID, sep = "_"), sep = ".")
      SIZE_INIT_i_vert$VALUE <- SIZE_INIT_i_vert$SIZE.t0
      DELTA_INIT_i_vert$VALUE <- DELTA_INIT_i_vert$DELTA.t0
      SIZE_INIT_i_vert <- SIZE_INIT_i_vert[,c("VAR_TYPE", "VARIABLE", "VALUE")]
      DELTA_INIT_i_vert <- DELTA_INIT_i_vert[,c("VAR_TYPE", "VARIABLE", "VALUE")]

      meta_RUN_i <- rbind(SIZE_INIT_i_vert, DELTA_INIT_i_vert)
      meta_RUN_i <- rbind(meta_RUN_i, FLUXES_i_vert)
      meta_RUN_i <- rbind(meta_RUN_i, COEFFS_i_vert)
      meta_RUN_i_short <- meta_RUN_i[,c("VARIABLE", "VALUE")]
      meta_RUN_i_horiz <- as.data.frame(t(meta_RUN_i_short$VALUE))
      names(meta_RUN_i_horiz) <- meta_RUN_i$VARIABLE

      evD_i$SERIES_RUN_ID <- SERIES_RUN_ID_i
      evD_i$RUN_n <- RUN_n_i

      meta_RUN_i_evD_df <- as.data.frame(dplyr::slice(meta_RUN_i_horiz, rep(1:dplyr::n(), each = nrow(evD_i))))

      evD_i <- cbind(evD_i, meta_RUN_i_evD_df)

      if (i == 1){
        evD <- evD_i
      } else {
        evD <- rbind(evD, evD_i[1:nrow(evD_i),])
      }
      utils::setTxtProgressBar(pb_prep, i)
      i <- i + 1
    }

    FINnD.results.chunk <- evD
    remove(evD)

    elapsed.prepare <- tictoc::toc(quiet = T)
    close(pb_prep)

    colnames_to_drop_check <- names(FINnD.results.chunk)[names(FINnD.results.chunk) %>% stringr::str_starts(pattern = "f.")]
    if (length(colnames_to_drop_check) > 0){
      flux_cols_to_drop <- NULL
      i <- 1
      for (i in 1:length(colnames_to_drop_check)){
        if (sum(abs(FINnD.results.chunk[, colnames_to_drop_check[i]]-1)) == 0){
          FINnD.results.chunk <- FINnD.results.chunk[, -which(names(FINnD.results.chunk) %in% c(colnames_to_drop_check[i]))]
        }
        i <- i + 1
      }
    }

    colnames_to_drop_check <- names(FINnD.results.chunk)[names(FINnD.results.chunk) %>% stringr::str_starts(pattern = "a.")]
    alpha_cols_to_drop <- NULL
    if (length(colnames_to_drop_check) > 0){
      i <- 1
      for (i in 1:length(colnames_to_drop_check)){
        if (sum(abs(FINnD.results.chunk[, colnames_to_drop_check[i]]-1)) == 0){
          FINnD.results.chunk <- FINnD.results.chunk[, -which(names(FINnD.results.chunk) %in% c(colnames_to_drop_check[i]))]
        }
        i <- i + 1
      }
    }

    # _d. edit chunk output : RDS and CSV ####

    paths$current.chunk_dir <- paste("4_", as.character(SERIES_ID), "/", sep = "")

    if (!dir.exists(to_tmpdir(paths$current.chunk_dir))){dir.create(to_tmpdir(paths$current.chunk_dir))}

    paths$current.chunk.digest_root <- paste(paths$current.chunk_dir, SERIES_ID, sep = "")

    if (file.exists(paths$LOG_file)){
      # LOG ####
      LOG <- data.table::fread(paths$LOG_file, data.table = F, stringsAsFactors = T)
      LOG <- dplyr::bind_rows(LOG, LOG_SERIES)
    } else {
      LOG <- LOG_SERIES
    }

    # LOG ####
    data.table::fwrite(LOG_SERIES, file = paste(to_tmpdir(paths$current.chunk.digest_root), "_chunk_LOG.csv", sep = ""), row.names = F, quote = F)
    saveRDS(object = FINnD.results.chunk, file = paste(to_tmpdir(paths$current.chunk.digest_root), "_chunk_results.RDS", sep = ""))
    saveRDS(object = param_space.loc, file = paste(to_tmpdir(paths$current.chunk.digest_root), "_chunk_param_space.RDS", sep = ""))

    if (args$export.data_as_csv_xlsx){
      data.table::fwrite(evD_final, file = paste(to_tmpdir(paths$current.chunk.digest_root), "_chunk_results.csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(param_space.loc, file = paste(to_tmpdir(paths$current.chunk.digest_root), "_chunk_param_space.csv", sep = ""), row.names = F, quote = F)
    }

    # _e. unlink single output rds files from tempdir ####
    unlink_rds <- function(x){
      unlink(to_tmpdir(x), recursive = TRUE)
    }

    unlinked <- paths$paths_all_rds %>%
      paste0(".rds") %>%
      purrr::map(unlink_rds)

    remove("unlinked")

    # VI. save outputs ####

    rlang::inform("________________________________________________________________________________")
    rlang::inform(message = paste("\U2139 Run outputs (stored in temporary directory):", sep = ""))
    fs::dir_tree(path = to_tmpdir(""), recurse = T)
    rlang::inform("________________________________________________________________________________")

    if(!args$save_outputs){
      rlang::inform("\U2757 Results were not saved to working directory (set save_outputs = TRUE to save results).")
    } else if(args$save_outputs){
      R.utils::copyDirectory(to_tmpdir(""), getwd(), overwrite = T)
      # LOG ####
      data.table::fwrite(LOG, file = "1_LOG.csv", row.names = F, quote = F)
      rlang::inform("\U2705 Results were successfully saved to working directory.")
      elapsed.all <- tictoc::toc(quiet = T)
      chunk_LOG[chunk_LOG$chunk_n == chunk.loc$chunk_n, "chunk_status"] <- as.factor("complete")
      chunk_LOG[chunk_LOG$chunk_n == chunk.loc$chunk_n, "elapsed.all"] <- as.numeric((elapsed.all$toc-elapsed.all$tic[[1]]))
      chunk_LOG[chunk_LOG$chunk_n == chunk.loc$chunk_n, "elapsed.sweep"] <- as.numeric((elapsed.sweep$toc-elapsed.sweep$tic[[1]]))
      chunk_LOG[chunk_LOG$chunk_n == chunk.loc$chunk_n, "elapsed.prepare"] <- as.numeric((elapsed.prepare$toc-elapsed.prepare$tic[[1]]))
      chunk_LOG$elapsed.all.per_run <- chunk_LOG$elapsed.all/chunk_LOG$n_runs
      chunk_LOG$elapsed.sweep.per_run <- chunk_LOG$elapsed.sweep/chunk_LOG$n_runs
      chunk_LOG$elapsed.prepare.per_run <- chunk_LOG$elapsed.prepare/chunk_LOG$n_runs
      saveRDS(chunk_LOG, file = paste0(paths$digest_dir, "/", paths$current.space_sweep.SERIES, "_chunk_log.RDS"))
      # LOG ####
      data.table::fwrite(chunk_LOG, file = paste0(paths$digest_dir, "/", paths$current.space_sweep.SERIES, "_chunk_log.csv"))
      print(chunk_LOG[chunk_LOG$chunk_status == "complete",
                      (names(chunk_LOG) %in%
                         c("chunk_n",
                           "chunk_ID",
                           "n_runs",
                           "elapsed.all",
                           "elapsed.all.per_run",
                           "chunk_status"
                         ))])
    }

    n_chunks.complete <- n_chunks.complete + 1
  }

}
