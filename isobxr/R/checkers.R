#  #_________________________________________________________________________80char
#' check_FORCINGS
#' @description  A function to check the formatting data frames used as forcing inputs \cr
#' in several functions (e.g., sim.single_run)
#' @param type Type of forcing. As character. \cr
#' One value among: "RAYLEIGH", "ALPHA", "SIZE", "DELTA"
#' @param FORCING Forcing data frame.
#' @param isobxr_master Isobxr master as a list as returned from read_isobxr_master
#' @return aborts run if a mistake is found
#' @keywords internal
check_FORCINGS <- function(type,
                           FORCING,
                           isobxr_master) {

  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list=ls())
  # gc()
  # devtools::load_all(".")
  #
  # workdir_root <- "/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/"
  # workdir <- paste(workdir_root, "1_ABCD_dev", sep = "")
  # isobxr_master_file <- "0_ISOBXR_MASTER"
  # isobxr_master <- read_isobxr_master_(workdir, isobxr_master_file, inspect = T, export_rds = F)
  #
  # FORCING_RAYLEIGH <-
  #   data.frame(XFROM = c("B"), # Define the B>C flux at numerator
  #              XTO = c("C"),
  #              YFROM = c("A"), # Define the A>B flux at denominator
  #              YTO = c("B"),
  #              AFROM = c("B"), # Define the resulting fractionation coefficient
  #              ATO = c("C"),
  #              ALPHA_0 = c(1.00003) )
  #
  # FORCING_SIZE <- data.frame(BOX_ID = c("C", "C"), SIZE.t0 = c(3000, 3000))
  #
  # FORCING_DELTA <- data.frame(BOX_ID = c("A", "D"), DELTA.t0 = c(-1,1))
  #
  # FORCING_ALPHA <- data.frame(FROM = c("A", "A"), TO = c("B", "A"), ALPHA = c(1.02,0.999))
  #
  # type <- "SIZE"
  # FORCING <- FORCING_SIZE

  # 1. check type exists ####
  ALPHA_0 <- SIZE.t0 <-  DELTA.t0<-  ALPHA<- FROM<-  TO<- value <- FROM_TO <- NULL
  allowed.types <- c("RAYLEIGH", "ALPHA", "SIZE", "DELTA")
  if(!type %in% allowed.types) rlang::abort("type not found.")

  # 2. RAYLEIGH ####
  if (type == "RAYLEIGH"){
    if(!all(names(FORCING) == names.forcing$FORCING_RAYLEIGH)){
      rlang::abort(paste("Column names of FORCING_RAYLEIGH dataframe should be: ", "\n",
                         paste(names.forcing$FORCING_RAYLEIGH, collapse = ", ")
      ))
    }

    if(!all({FORCING %>%
        dplyr::select(!ALPHA_0) %>%
        unlist() %>%
        as.vector() %>%
        unique()} %in% isobxr_master$BOXES$BOX_ID)){
      rlang::abort("Box names called in FORCING_RAYLEIGH do not match boxes from isobxr master.")
    }

    if(!FORCING %>%
       dplyr::pull(ALPHA_0) %>%
       is.numeric()){
      rlang::abort("ALPHA_0 in FORCING_RAYLEIGH should be numeric.")
    }

    if(any(duplicated(paste(FORCING$AFROM, FORCING$ATO, sep = "_")))){
      rlang::abort(paste0("In FORCING_RAYLEIGH, for a given run, fractionations shouldn't be attached more than once to a given flux. \n",
                          "  FROM and TO should not have duplicated pairs."
      ))
    }

  }

  # 3. SIZE ####
  if (type == "SIZE"){
    if(!all(names(FORCING) == names.forcing$FORCING_SIZE)){
      rlang::abort(paste("Column names of FORCING_SIZE dataframe should be: ", "\n",
                         paste(names.forcing$FORCING_SIZE, collapse = ", ")
      ))
    }

    if(!all(FORCING$BOX_ID %in% isobxr_master$BOXES$BOX_ID)){
      rlang::abort("Box names called in FORCING_SIZE do not match boxes from isobxr master.")
    }

    if(any(duplicated(FORCING$BOX_ID))){
      rlang::abort("In FORCING_SIZE, for a given run, boxes can only be defined once.")
    }

    if(!FORCING %>%
       dplyr::pull(SIZE.t0) %>%
       is.numeric()){
      rlang::abort("SIZE.t0 in FORCING_SIZE should be numeric.")
    }


  }


  # 4. DELTA ####
  if (type == "DELTA"){
    if(!all(names(FORCING) == names.forcing$FORCING_DELTA)){
      rlang::abort(paste("Column names of FORCING_DELTA dataframe should be: ", "\n",
                         paste(names.forcing$FORCING_DELTA, collapse = ", ")
      ))
    }

    if(!all(FORCING$BOX_ID %in% isobxr_master$BOXES$BOX_ID)){
      rlang::abort("Box names called in FORCING_DELTA do not match boxes from isobxr master.")
    }

    if(any(duplicated(FORCING$BOX_ID))){
      rlang::abort("In FORCING_DELTA, for a given run, boxes can only be defined once.")
    }

    if(!FORCING %>%
       dplyr::pull(DELTA.t0) %>%
       is.numeric()){
      rlang::abort("DELTA.t0 in FORCING_SIZE should be numeric.")
    }
  }

  # 5. ALPHA ####
  if (type == "ALPHA"){
    if(!all(names(FORCING) == names.forcing$FORCING_ALPHA)){
      rlang::abort(paste("Column names of FORCING_ALPHA dataframe should be: ", "\n",
                         paste(names.forcing$FORCING_ALPHA, collapse = ", ")
      ))
    }

    if(!FORCING %>%
       dplyr::pull(ALPHA) %>%
       is.numeric()){
      rlang::abort("ALPHA in FORCING_ALPHA should be numeric.")
    }

    called_BOXES <- FORCING %>%
      dplyr::filter(FROM != "NaN" | TO != "NaN") %>%
      dplyr::select(FROM, TO) %>%
      tidyr::pivot_longer(cols = c("FROM", "TO")) %>%
      as.data.frame() %>%
      dplyr::select(value) %>%
      unique() %>%
      dplyr::pull(value) %>%
      as.character()

    if(!all(called_BOXES %in% isobxr_master$BOXES$BOX_ID)){
      rlang::abort(paste("In FORCING_ALPHA, the following boxes called are not defined in the list of boxes: \n",
                         "[", paste(called_BOXES[!called_BOXES %in% isobxr_master$BOXES$BOX_ID], collapse = ", "), "]",
                         sep = ""))
    }

    dup.COEFFS.FROM_TO <- FORCING %>%
      dplyr::select(FROM, TO) %>%
      tidyr::unite(FROM, TO, col = "FROM_TO", sep = " to ") %>%
      dplyr::filter(duplicated(FROM_TO) & FROM_TO != "NaN to NaN") %>%
      dplyr::pull(FROM_TO) %>%
      as.character()

    if(length(dup.COEFFS.FROM_TO) > 0){
      rlang::abort(paste("In FORCING_ALPHA, for a given run, FROM-TO pairs shouldn't be defined more than once, \n",
                         "The following FROM-TO pairs were defined more than once: \n",
                         paste(dup.COEFFS.FROM_TO, collapse = ", ")))
    }

    id_pairs.COEFFS.FROM_TO <-
      FORCING[(as.character(FORCING$FROM) == as.character(FORCING$TO)), ] %>%
      dplyr::select(FROM, TO) %>%
      tidyr::unite(FROM, TO, col = "FROM_TO", sep = " to ") %>%
      dplyr::pull(FROM_TO) %>%
      as.character()

    if(length(id_pairs.COEFFS.FROM_TO) > 0){
      rlang::abort(paste("In FORCING_ALPHA, the following fluxes have identical FROM and TO: \n",
                         paste(id_pairs.COEFFS.FROM_TO, collapse = ", ")))
    }
  }
}
