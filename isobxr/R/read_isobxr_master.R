#  #_________________________________________________________________________80char
#' Read and inspect isobxr master files
#'
#' @description  A function to read and inspect the isobxr master files
#' and obtain a master formatted list.
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' (character string)
#'
#' @param isobxr_master_file Name of \strong{\emph{isobxr excel master file}}.
#' (without file "xlsx" extension).
#' Default is "0_ISOBXR_MASTER".
#'
#' @param inspect If TRUE, checks all inputs from isobxr master file for format
#' and structure errors. \cr
#' Default is TRUE.
#'
#' @param export_rds If TRUE, exports rds version of isobxr master file
#' to working directory. \cr
#' Default is FALSE.
#'
#' @return A formatted list of data frames containing constants, box, fluxes and
#' fractionation coefficients descriptions.
#'
#' @export
#' @examples
#' read.isobxr_master(workdir = "/Users/username/Documents/1_ABC_tutorial",
#'                    isobxr_master_file = "0_ISOBXR_MASTER",
#'                    export_rds = FALSE,
#'                    inspect = TRUE)
#'
read.isobxr_master <-
  function(workdir,
           isobxr_master_file = "0_ISOBXR_MASTER",
           inspect = TRUE,
           export_rds = FALSE
  ){

    FROM_TO <- value <- TO <- FROM <- SIZE_or_FLUX <- BOX_ID <- NULL

    # workdir = "/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/1_ABCD_dev"
    # isobxr_master_file = "0_ISOBXR_MASTER"
    # export_rds = F
    # inspect = T

    # 0. tuto mode ####
    tuto_setup <- using_extdata_tutorial_2(workdir = workdir,
                                           save_outputs = export_rds,
                                           plot_results = F)

    workdir <- tuto_setup$workdir
    export_rds <- tuto_setup$save_outputs
    if (tuto_setup$tuto_mode) isobxr_master_file <- "0_ISOBXR_MASTER.xlsx"

    # 1. check arguments ####
    if (stringr::str_ends(isobxr_master_file, pattern = ".xlsx")){
      isobxr_master_file <- stringr::str_remove(isobxr_master_file, pattern = ".xlsx")
    }

    if (inspect){
      if(!dir.exists(workdir)) rlang::abort("Workdir does not exist.")
      path.file <- paste0(workdir, "/", isobxr_master_file, ".xlsx")
      if(!file.exists(path.file)) rlang::abort("Isobxr master file not found.")
    } else{
      path.file <- paste0(workdir, "/", isobxr_master_file, ".xlsx")
    }

    # 2. import ####
    master.constants <- readxl::read_excel(path.file, "CONSTANTS") %>%
      as.data.frame()

    master.boxes <- readxl::read_excel(path.file, "BOXES") %>%
      as.data.frame()

    master.fluxes <- readxl::read_excel(path.file, "FLUXES") %>%
      as.data.frame()

    master.coeffs <- readxl::read_excel(path.file, "COEFFS") %>%
      as.data.frame()

    # 3. to factor if character ####
    # master.boxes <- master.boxes %>% dplyr::mutate(across(where(is.character), as.factor))
    master.boxes <- master.boxes %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)
    # master.fluxes <- master.fluxes %>% dplyr::mutate(across(where(is.character), as.factor))
    master.fluxes <- master.fluxes %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)
    # master.coeffs <- master.coeffs %>% dplyr::mutate(across(where(is.character), as.factor))
    master.coeffs <- master.coeffs %>% unclass() %>% as.data.frame(stringsAsFactors = TRUE)

    # 4. prepare output ####
    master <- list(CONSTANTS = master.constants,
                   BOXES = master.boxes,
                   FLUXES = master.fluxes,
                   COEFFS = master.coeffs)

    # 5. inspect called boxes ####
    bx.groups <- NULL
    bx.groups$all <-  master$BOXES %>%
      dplyr::pull(BOX_ID) %>%
      as.character() %>%
      sort()

    bx.groups$all.n <- length(bx.groups$all)

    # 6. inspect ####
    if (inspect){
      # _i. check master df ####
      # CONSTANTS col names
      if(!all(names(master.constants) == master.names$isobxr$constants.colnames)){
        rlang::abort(paste("Column names of the CONSTANTS spreadsheet should be:",
                           paste(master.names$isobxr$constants.colnames, collapse = ", ")))
      }

      # BOXES col names
      if(!all(names(master.boxes) == master.names$isobxr$boxes.colnames)){
        rlang::abort(paste("Column names of the BOXES spreadsheet should be:",
                           paste(master.names$isobxr$boxes.colnames, collapse = ", ")))
      }

      # FLUXES col names
      if(!all(master.names$isobxr$fluxes.colnames %in% names(master.fluxes))){
        rlang::abort(paste("Column names of the FLUXES spreadsheet should include:",
                           paste(master.names$isobxr$fluxes.colnames, collapse = ", ")))
      }

      # COEFFS col names
      if(!all(master.names$isobxr$coeffs.colnames %in% names(master.coeffs))){
        rlang::abort(paste("Column names of the COEFFS spreadsheet should include:",
                           paste(master.names$isobxr$coeffs.colnames, collapse = ", ")))
      }

      # _ii. inspect constants ####
      if (inspect){
        if(is.na(master.constants$RATIO_STANDARD)){
          rlang::abort("Reference ratio should be numeric.")
        }

        if(!master.constants$MASS_UNIT %in% master.names$isobxr$mass_units){
          rlang::abort(paste("Unknown mass unit. Allowed values are: \n",
                             paste(master.names$isobxr$mass_units, collapse = ", ")))
        }

        if(!master.constants$TIME_UNIT %in% master.names$isobxr$time_units){
          rlang::abort(paste("Unknown time unit. Allowed values are: \n",
                             paste(master.names$isobxr$time_units, collapse = ", ")))
        }
      }

      # _iii. check for box duplicates ####
      dup.boxes <- bx.groups$all[duplicated(bx.groups$all)]
      if(length(dup.boxes) > 0){
        rlang::abort(paste("Following boxes defined more than once in BOXES: \n",
                           paste(dup.boxes, collapse = ", ")))
      }

      # _iv. box names ####
      name_error <- bx.groups$all[stringr::str_detect(bx.groups$all, pattern = "[ !@#$%^&*()+}{\';|:/.,?><}]")]
      if (length(name_error) > 0){
        rlang::abort(paste("Box names must not include any special characters (\"_\" excepted) \n",
                           "The following box names do not match the required format: \n",
                           "[", paste(name_error, collapse = ", "), "]",
                           sep = ""))
      }

      # _v. boxes called in FLUXES ####

      # all boxes called in FLUXES FROM/TO should be defined in BOXES
      bx.groups$called_in_FLUXES <- master$FLUXES %>%
        dplyr::filter(SIZE_or_FLUX == "FLUX") %>%
        dplyr::select(FROM, TO) %>%
        tidyr::pivot_longer(cols = c("FROM", "TO")) %>%
        as.data.frame() %>%
        dplyr::select(value) %>%
        unique() %>%
        dplyr::pull(value) %>%
        as.character()

      if(!all(bx.groups$called_in_FLUXES %in% bx.groups$all)){
        rlang::abort(paste("The following boxes called in FLUXES are not defined in the list of boxes: \n",
                           "[", paste(bx.groups$called_in_FLUXES[!bx.groups$called_in_FLUXES %in% bx.groups$all], collapse = ", "), "]",
                           sep = ""))
      }

      # boxes FROM/TO pairs should be unique
      dup.FLUXES.FROM_TO <- master$FLUXES %>%
        dplyr::filter(SIZE_or_FLUX == "FLUX") %>%
        dplyr::select(FROM, TO) %>%
        tidyr::unite(FROM, TO, col = "FROM_TO", sep = " to ") %>%
        dplyr::filter(duplicated(FROM_TO)) %>%
        dplyr::pull(FROM_TO) %>%
        as.character()

      if(length(dup.FLUXES.FROM_TO) > 0){
        rlang::abort(paste("The following FROM-TO pairs defined more than once in FLUXES: \n",
                           paste(dup.FLUXES.FROM_TO, collapse = ", ")))
      }

      # FROM and TO should be distinct
      id_pairs.FLUXES.FROM_TO <-
        master$FLUXES[(as.character(master$FLUXES$FROM) == as.character(master$FLUXES$TO)), ] %>%
        dplyr::filter(SIZE_or_FLUX == "FLUX") %>%
        dplyr::select(FROM, TO) %>%
        tidyr::unite(FROM, TO, col = "FROM_TO", sep = " to ") %>%
        dplyr::pull(FROM_TO) %>%
        as.character()

      if(length(id_pairs.FLUXES.FROM_TO) > 0){
        rlang::abort(paste("The following fluxes have identical FROM and TO defined in FLUXES: \n",
                           paste(id_pairs.FLUXES.FROM_TO, collapse = ", ")))
      }

      # _vi. boxes called in SIZES ####
      bx.groups$called_in_SIZES <- master$FLUXES %>%
        dplyr::filter(SIZE_or_FLUX == "SIZE") %>%
        dplyr::select(BOX_ID) %>%
        tidyr::pivot_longer(cols = c("BOX_ID")) %>%
        as.data.frame() %>%
        dplyr::select(value) %>%
        unique() %>%
        dplyr::pull(value) %>%
        as.character()

      # all boxes called in FLUXES SIZES (ID) should be defined in BOXES
      if(!all(bx.groups$called_in_SIZES %in% bx.groups$all)){
        rlang::abort(paste("The following boxes called in SIZES are not defined in the list of boxes: \n",
                           "[", paste(bx.groups$called_in_SIZES[!bx.groups$called_in_SIZES %in% bx.groups$all], collapse = ", "), "]",
                           sep = ""))
      }

      # all boxes defined in BOXES should have a defined SIZE
      if(!all(bx.groups$all %in% bx.groups$called_in_SIZES)){
        rlang::abort(paste("The following boxes defined in BOXES have no defined sizes in FLUXES spreadsheet: \n",
                           "[", paste(bx.groups$all[!bx.groups$all %in% bx.groups$called_in_SIZES], collapse = ", "), "]",
                           sep = ""))
      }

      # boxes SIZES should be unique
      dup.SIZES <-
        master$FLUXES %>%
        dplyr::select(BOX_ID) %>%
        dplyr::filter(duplicated(BOX_ID)) %>%
        dplyr::filter(BOX_ID != "NaN") %>%
        dplyr::pull(BOX_ID) %>%
        as.character()

      if(length(dup.SIZES) > 0){
        rlang::abort(paste("Following boxes defined more than once in SIZES: \n",
                           paste(dup.SIZES, collapse = ", ")))
      }

      # _vii. boxes called in COEFFS ####
      bx.groups$called_in_COEFFS <- master$COEFFS %>%
        dplyr::filter(FROM != "NaN" | TO != "NaN") %>%
        dplyr::select(FROM, TO) %>%
        tidyr::pivot_longer(cols = c("FROM", "TO")) %>%
        as.data.frame() %>%
        dplyr::select(value) %>%
        unique() %>%
        dplyr::pull(value) %>%
        as.character()

      # all boxes called in COEFFS (FROM/TO) should be defined in BOXES
      if(!all(bx.groups$called_in_COEFFS %in% bx.groups$all)){
        rlang::abort(paste("The following boxes called in COEFFS are not defined in the list of boxes: \n",
                           "[", paste(bx.groups$called_in_COEFFS[!bx.groups$called_in_COEFFS %in% bx.groups$all], collapse = ", "), "]",
                           sep = ""))
      }

      # boxes FROM/TO should be unique
      dup.COEFFS.FROM_TO <- master$COEFFS %>%
        dplyr::select(FROM, TO) %>%
        tidyr::unite(FROM, TO, col = "FROM_TO", sep = " to ") %>%
        dplyr::filter(duplicated(FROM_TO) & FROM_TO != "NaN to NaN") %>%
        dplyr::pull(FROM_TO) %>%
        as.character()

      if(length(dup.COEFFS.FROM_TO) > 0){
        rlang::abort(paste("Following FROM-TO pairs defined more than once in COEFFS: \n",
                           paste(dup.COEFFS.FROM_TO, collapse = ", ")))
      }


      # FROM and TO should be distinct
      id_pairs.COEFFS.FROM_TO <-
        master$COEFFS[(as.character(master$COEFFS$FROM) == as.character(master$COEFFS$TO)), ] %>%
        dplyr::select(FROM, TO) %>%
        tidyr::unite(FROM, TO, col = "FROM_TO", sep = " to ") %>%
        dplyr::pull(FROM_TO) %>%
        as.character()

      if(length(id_pairs.COEFFS.FROM_TO) > 0){
        rlang::abort(paste("The following fluxes have identical FROM and TO defined in COEFFS: \n",
                           paste(id_pairs.COEFFS.FROM_TO, collapse = ", ")))
      }
    }

    if (export_rds){
      saveRDS(object = c(master, list(bx.groups = bx.groups)),
              file = paste0(workdir, "/", isobxr_master_file, ".rds")
      )
    }

    return(c(master, list(bx.groups = bx.groups)))
  }

