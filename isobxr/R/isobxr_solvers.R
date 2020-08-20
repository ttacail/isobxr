# library(devtools)
# library(usthis)


usethis::use_package("stringr", min_version = TRUE)
usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("plyr", min_version = TRUE)
usethis::use_package("data.table", min_version = TRUE)
usethis::use_package("deSolve", min_version = TRUE)
#  #_________________________________________________________________________80char
#' Numerically solve stable isotope box models
#' @description  A numerical solver of the system of ordinary differential
#' equations (ODES) of stable isotope ratios of element X in all boxes of a system.
#' The numerical solver uses the ode function of the deSolve package
#' to integrate the stable isotopes ratios in each box. It allows the
#' calculation of the evolution of stable isotope ratio even in the case of
#' unbalanced outward and inward fluxes of element X in a given box
#' resulting in the accumulation or loss of element X.
#' @param input_path path to the INPUT file containing all commands for the run.
#' \cr (file name structure: RUN name + _INPUT.xlsx)
#' @return The function returns the numerically determined evolution of stable
#' isotope compositions and mass of element X in all boxes over the run duration as
#' specified in INPUT file. The function outputs are as follows:
#' \enumerate{
#' \item OUT data file storing initial and final size and delta values in all boxes.
#' \cr (file name structure: RUN name + _N_1_OUT.csv)
#' \item evS data file storing the evolution with time of the sizes (masses of element X) of all boxes.
#' \cr (file name structure: RUN name + _N_2_evS.csv)
#' \item evD data file storing the evolution with time of the delta values in all boxes.
#' \cr (file name structure: RUN name + _N_3_evD.csv)
#' }
num_slvr <- function(input_path){
  ############################## IDENTIFY PREFIX in INPUT FILENAME #####################
  occ1 <- stringr::str_locate(input_path, "INPUT.xlsx")[[1]]
  if (is.na(occ1[1]) == "FALSE"){
    namefile = input_path
    if (.Platform$OS.type == "unix"){
      if (stringr::str_detect(input_path, "/") == TRUE){
        occ2 = max(stringr::str_locate_all(input_path, "/")[[1]][,1]) # for Unix
      } else {
        occ2 = 0
      }
    }
    if (.Platform$OS.type == "nt"){
      if (stringr::str_detect(input_path, "\\") == TRUE){
        occ2 = max(stringr::str_locate_all(input_path, "\\")[[1]][,1]) # for Windows
      }
      else {
        occ2 = 0
      }
    }

    prefix = stringr::str_sub(namefile, start = occ2+1, end = occ1-1)
    if (occ2 != 0){
      outdir = stringr::str_sub(namefile, start = 0, end = occ2-1)
    } else {
      outdir = NaN
    }
  } else {
    stop('\nWrong file or check file name\n')
  }

  ############################## DEFINE outdir #####################
  if (is.na(outdir)){
    cwd = getwd()
    run_dir = paste(prefix, "OUT", sep = "")
    if (.Platform$OS.type == "unix"){
      if (dir.exists(paste(cwd, "/", run_dir, "/", sep = "")) == FALSE){
        dir.create(paste(cwd, "/", run_dir, "/", sep = ""))
      }
      outdir = paste(cwd, "/", run_dir, "/", sep = "")
    }
    if (.Platform$OS.type == "nt"){
      dir.create(paste(cwd, "\\", run_dir, sep = ""), showWarnings = FALSE)
      outdir = paste(cwd, "\\", run_dir, "\\", sep = "")
    }
  } else {
    cwd = outdir
    run_dir = paste(prefix, "OUT", sep = "")
    if (.Platform$OS.type == "unix"){
      if (dir.exists(paste(cwd, "/", run_dir, "/", sep = "")) == FALSE){
        dir.create(paste(cwd, "/", run_dir, "/", sep = ""))
      }
      outdir = paste(cwd, "/", run_dir, "/", sep = "")
    }
    if (.Platform$OS.type == "nt"){
      dir.create(paste(cwd, "\\", run_dir, sep = ""), showWarnings = FALSE)
      outdir = paste(cwd, "\\", run_dir, "\\", sep = "")
    }
  }

  ############################## CONSTANTS ###################################
  consts_f = as.data.frame(readxl::read_excel(namefile, "CONSTS"))
  ratio_standard = as.numeric(consts_f[consts_f$CONSTS_ID == "Ratio_Standard", "CONSTS"])
  time_max = as.integer(as.numeric(consts_f[consts_f$CONSTS_ID == "time", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  nb_steps = as.integer(as.numeric(consts_f[consts_f$CONSTS_ID == "n_steps", "CONSTS"]))
  time = seq(0, time_max, length = nb_steps)

  ############################## INITIAL ###################################
  initial_f = as.data.frame(readxl::read_excel(namefile, "INITIAL"))
  boxes_id = as.character(initial_f$BOXES_ID)
  boxes_nb = length(boxes_id)
  boxes_size = as.numeric(initial_f$SIZE_INIT)
  delta = as.numeric(initial_f$DELTA_INIT)

  ############################## FLUXES ###################################
  fluxes_f = as.data.frame(readxl::read_excel(namefile, "FLUXES"))
  fluxes <- as.matrix(fluxes_f[,2:(boxes_nb+1)]) # potentially +1 if .xlsx with mac...
  colnames(fluxes) <- NULL

  ############################## FRACTIONATION COEFFICIENTS ("ALPHAs") ###################################
  coeffs_f = as.data.frame(readxl::read_excel(namefile, "COEFFS"))
  coeffs <- as.matrix(coeffs_f[,2:(boxes_nb+1)]) # potentially +1 if .xlsx with mac...
  colnames(coeffs) <- NULL

  ############################## DEFINE EVOL RATIO ##############################
  evol_ratio <- function(t, ratio, parms){
    with(as.list(c(ratio, parms)), {
      fluxes <- parms$fluxes
      coeffs <- parms$coeffs
      boxes_size <- parms$boxes_size

      rationew = rep(0,length(ratio))

      # Element mass evolution
      bsizenew = rep(0, length(boxes_size))
      for (ii in 1:length(bsizenew)){
        outflux = 0
        influx = 0
        for (jj in 1:length(bsizenew)){
          outflux  = outflux + fluxes[ii,jj]
          influx = influx + fluxes[jj,ii]
        }
        bsizenew[ii] = (influx - outflux)*t + boxes_size[ii]
      }

      # ratio evolution
      for (ii in 1:length(ratio)){
        outflux = 0
        influx = 0
        outflux_bsize = 0
        influx_bsize = 0
        for (jj in 1:length(ratio)){
          outflux = outflux + fluxes[ii,jj] / bsizenew[ii] * coeffs[ii,jj] * ratio[ii] - fluxes[ii,jj] / bsizenew[ii] * ratio[ii]
          influx = influx + fluxes[jj,ii] / bsizenew[ii] * coeffs[jj,ii] * ratio[jj] - fluxes[jj,ii] / bsizenew[ii] * ratio[ii]
          outflux_bsize = outflux_bsize + fluxes[ii,jj]
          influx_bsize = influx_bsize + fluxes[jj,ii]
        }
        rationew[ii] <- influx - outflux
        bsizenew[ii] <- influx_bsize - outflux_bsize
      }

      return(list(c(rationew)))
    })
  }

  ############################## COMPUTE EVOL ##############################
  Ratio = ((delta/1e3 + 1e0)*ratio_standard)
  parms = list(fluxes = fluxes, coeffs = coeffs, boxes_size = boxes_size)
  Ratio <- deSolve::ode(y = Ratio, parms = parms, times = time, func =  evol_ratio)
  Delta = ((Ratio / ratio_standard) - 1.0) * 1000

  ############################## WRITING DELTA EVOLUTION #####################
  Delta_as_df <- as.data.frame(Delta)
  Delta_as_df$time <- time
  colnames(Delta_as_df) <- c("Time", boxes_id)
  data.table::fwrite(Delta_as_df, file = paste(outdir, prefix, "N_3_evD.csv", sep = ""), row.names = F, quote = F, sep = ",")

  ############################## WRITING SIZE EVOLUTION #####################
  Boxes_size = matrix(0, nrow = length(time), ncol = length(boxes_size)+1)
  Boxes_size[,1] <- time
  for (tt in 1:length(time)){
    for (ii in 1:boxes_nb){
      outflux = 0
      influx = 0
      for (jj in 1:boxes_nb){
        outflux  = outflux + fluxes[ii,jj]
        influx = influx + fluxes[jj,ii]
      }
      Boxes_size[tt,ii+1] <- (influx - outflux)*time[tt] + boxes_size[ii]
    }
  }
  Boxes_size_as_df <- as.data.frame(Boxes_size)
  colnames(Boxes_size_as_df) <- c("Time", boxes_id)
  data.table::fwrite(Boxes_size_as_df, file = paste(outdir, prefix, "N_2_evS.csv", sep = ""), row.names = F, quote = F, sep = ",")

  ############################## WRITING FINAL STATE (IN NUM OUT csv) #####################
  df <- initial_f
  colnames(df) <- c("BOXES_ID", "SIZE_FINAL", "DELTA_FINAL")
  df$SIZE_FINAL <- Boxes_size[length(Boxes_size[,1]),1:length(boxes_id)+1]
  df$DELTA_FINAL <- Delta[length(Delta[,1]),1:length(boxes_id)+1]
  df <- plyr::join(initial_f, df, by = "BOXES_ID")
  data.table::fwrite(df, file = paste(outdir, prefix, "N_1_OUT.csv", sep = ""), row.names = F, quote = F, sep = ",")
}

#  #_________________________________________________________________________80char
#' Analytically solve stable isotope box models
#' @description  An analytical solver of the system of ordinary differential
#' equations (ODES) of stable isotope ratios of element X in all boxes.
#' The analytical solver finds the eigenvalues and eigenvectors of the ODES.
#' Given the initial conditions as specified in INPUT.xlsx file, it determines the
#' set of analytical solutions that describes the evolution of isotope ratios
#' in each box over time.
#' @param input_path path to the INPUT file containing all commands for the run.
#' \cr (file name structure: RUN name + _INPUT.xlsx)
#' @return The function returns the analytically determined evolution of stable
#' isotope compositions in all boxes over the run duration as specified in
#' INPUT file. The function outputs are as follows:
#' \enumerate{
#' \item OUT data file with initial and final size and delta values in each boxes.
#' \cr (file name structure: RUN name + _A_1_OUT.csv)
#' \item ODE_SOLNs data file summarizing outputs of the analytical solutions of the ODES
#' (eigenvalues, eigenvectors, relaxation times, constants according to initial conditions).
#' \cr (file name structure: RUN name + _A_2_ODE_SOLNs.csv)
#' \item evD data file of the evolution with time of the delta values in each boxes.
#' \cr (file name structure: RUN name + _A_3_evD.csv)
#' }
ana_slvr <- function(input_path){
  ############################## IDENTIFY PREFIX in INPUT FILENAME #####################
  occ1 <- stringr::str_locate(input_path, "INPUT.xlsx")[[1]]
  if (is.na(occ1[1]) == "FALSE"){
    namefile = input_path
    if (.Platform$OS.type == "unix"){
      if (stringr::str_detect(input_path, "/") == TRUE){
        occ2 = max(stringr::str_locate_all(input_path, "/")[[1]][,1]) # for Unix
      } else {
        occ2 = 0
      }
    }
    if (.Platform$OS.type == "nt"){
      if (stringr::str_detect(input_path, "\\") == TRUE){
        occ2 = max(stringr::str_locate_all(input_path, "\\")[[1]][,1]) # for Windows
      }
      else {
        occ2 = 0
      }
    }

    prefix = stringr::str_sub(namefile, start = occ2+1, end = occ1-1)
    if (occ2 != 0){
      outdir = stringr::str_sub(namefile, start = 0, end = occ2-1)
    } else {
      outdir = NaN
    }
  } else {
    stop('\nWrong file or check file name\n')
  }

  ############################## DEFINE outdir #####################
  if (is.na(outdir)){
    cwd = getwd()
    run_dir = paste(prefix, "OUT", sep = "")
    if (.Platform$OS.type == "unix"){
      if (dir.exists(paste(cwd, "/", run_dir, "/", sep = "")) == FALSE){
        dir.create(paste(cwd, "/", run_dir, "/", sep = ""))
      }
      outdir = paste(cwd, "/", run_dir, "/", sep = "")
    }
    if (.Platform$OS.type == "nt"){
      dir.create(paste(cwd, "\\", run_dir, sep = ""), showWarnings = FALSE)
      outdir = paste(cwd, "\\", run_dir, "\\", sep = "")
    }
  } else {
    cwd = outdir
    run_dir = paste(prefix, "OUT", sep = "")
    if (.Platform$OS.type == "unix"){
      if (dir.exists(paste(cwd, "/", run_dir, "/", sep = "")) == FALSE){
        dir.create(paste(cwd, "/", run_dir, "/", sep = ""))
      }
      outdir = paste(cwd, "/", run_dir, "/", sep = "")
    }
    if (.Platform$OS.type == "nt"){
      dir.create(paste(cwd, "\\", run_dir, sep = ""), showWarnings = FALSE)
      outdir = paste(cwd, "\\", run_dir, "\\", sep = "")
    }
  }

  ############################## CONSTANTS ###################################
  consts_f = as.data.frame(readxl::read_excel(namefile, "CONSTS"))
  ratio_standard = as.numeric(consts_f[consts_f$CONSTS_ID == "Ratio_Standard", "CONSTS"])
  time_max = as.integer(as.numeric(consts_f[consts_f$CONSTS_ID == "time", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  nb_steps = as.integer(as.numeric(consts_f[consts_f$CONSTS_ID == "n_steps", "CONSTS"]))
  time = seq(0, time_max, length = nb_steps)


  ############################## INITIAL ###################################
  initial_f = as.data.frame(readxl::read_excel(namefile, "INITIAL"))
  boxes_id = as.character(initial_f$BOXES_ID)
  boxes_nb = length(boxes_id)
  boxes_size = as.numeric(initial_f$SIZE_INIT)
  delta = as.numeric(initial_f$DELTA_INIT)

  ############################## FLUXES ###################################
  fluxes_f = as.data.frame(readxl::read_excel(namefile, "FLUXES"))
  fluxes <- as.matrix(fluxes_f[,2:(boxes_nb+1)]) # maybe +1 if .xlsx with mac...
  colnames(fluxes) <- NULL

  ############################## FRACTIONATION COEFFICIENTS ("ALPHAs") ###################################
  coeffs_f = as.data.frame(readxl::read_excel(namefile, "COEFFS"))
  coeffs <- as.matrix(coeffs_f[,2:(boxes_nb+1)]) # maybe +1 if .xlsx with mac...
  colnames(coeffs) <- NULL

  ############################## MATRIX ODE SYSTEM ###################################
  VecM <- boxes_size
  MatJ <- fluxes
  MatD <- coeffs

  InitSystem <- function(VecM,MatJ,MatD){
    MatM = matrix(VecM, nrow = length(VecM), ncol=length(VecM), byrow = FALSE)
    MatSystem = array(0, dim(MatJ))
    MatSystem = t(MatJ)*t(MatD)/MatM
    MatAux = MatJ/MatM-MatJ/MatM*MatD-t(MatJ)/MatM
    for (i in 1:as.integer(length(MatSystem[,1]))){
      MatSystem[i,i] = sum(MatAux[i,])
      i <- i + 1
    }
    return(MatSystem)
  }

  ############################## SOLVING SYSTEM ##############################
  MatSystem=InitSystem(VecM,MatJ,MatD)
  eigenval <- eigen(MatSystem)$values
  eigenval_as_df <- as.data.frame(eigenval)
  colnames(eigenval_as_df) <- "Eigenvalues"
  eigenvec <- eigen(MatSystem)$vectors
  eigenvec_as_df = as.data.frame(eigenvec)
  colnames(eigenvec_as_df) <- rep(0:(length(colnames(eigenvec_as_df))-1), 1)
  ratio_standard_init = ratio_standard*((0.001*delta)+1)

  A = eigenvec
  B = ratio_standard_init*rep(1,length(eigenval))
  C = solve(A, B )
  C_as_df <- as.data.frame(C)
  colnames(C_as_df) <- "COEFFs"

  ############################## CALCULATE DELTA FINAL ###############################
  R = (C*exp(eigenval*time_max)) %*% t(eigenvec)
  d = ((R/ratio_standard)-1)*1000
  d_as_df = as.data.frame(t(d))
  colnames(d_as_df) <- "DELTA_FINAL"

  ############################## EXPORT DELTA FINAL, EIGENVALUES, EQUATION COEFFICIENTS #######
  results <- initial_f
  results$SIZE_FINAL <- results$SIZE_INIT
  results <- cbind(results, d_as_df)
  data.table::fwrite(results, file = paste(outdir, prefix, "A_1_OUT.csv", sep = ""), row.names = F, quote = F)

  ############################## EXPORT ODE SOLUTIONS #################################
  eigenval_as_df_inverse <- -1/eigenval_as_df
  colnames(eigenval_as_df_inverse) <- "relax_times"
  ODE_SOLNs <- cbind(eigenval_as_df, eigenval_as_df_inverse)
  colnames(C_as_df) <- "Constants"
  ODE_SOLNs <- cbind(ODE_SOLNs, C_as_df)
  colnames(eigenvec_as_df) <- paste("EigenVec_", rep(1:(length(colnames(eigenvec_as_df))), 1), sep = "")
  ODE_SOLNs <- cbind(ODE_SOLNs, eigenvec_as_df)
  data.table::fwrite(ODE_SOLNs, file = paste(outdir, prefix, "A_2_ODE_SOLNs.csv", sep = ""), row.names = F, quote = F)

  ############################## CALCULATE AND WRITE evD with solutions from ODE EigenVec/Vals/Constants#################################
  i <- 1
  dt <- time_max/(nb_steps)
  t_loc <- 0
  for (i in 1:(nb_steps+1)){
    d_t_loc <- ANA_delta_t_Calculator(t_loc, ODE_SOLNs$Constants, ODE_SOLNs$Eigenvalues, eigenvec_as_df, boxes_id, ratio_standard)
    if (i == 1){
      d_t_all <- d_t_loc
    } else {
      d_t_all <- rbind(d_t_all, d_t_loc)
    }
    t_loc <- t_loc + dt
    i <- i + 1
  }

  data.table::fwrite(d_t_all, file = paste(outdir, prefix, "A_3_evD.csv", sep = ""), row.names = F, quote = F)
}