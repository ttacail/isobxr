#  #_________________________________________________________________________80char
#' Numerically solve stable isotope box models
#' @description A numerical solver of the system of ordinary differential equations (ODES),
#' describing the evolution of stable isotope ratios in all boxes of a system. \cr
#' Not intended for direct use although possible. \cr
#' The numerical solver uses the ode function of the deSolve package
#' to integrate the stable isotopes ratios over time in each box. It allows the
#' calculation of the evolution of stable isotope ratio even in the case of
#' unbalanced outward and inward fluxes of element X in a given box
#' resulting in the accumulation or loss of element X.
#' @param input_path path to the INPUT file containing all commands for the run \cr
#' (character string, file name structure: \strong{\emph{RUN name + _IN.Rda}})
#' @param to_DIGEST_csv Edits csv outputs to the RUN DIGEST folder \cr
#' (logical, default is FALSE)
#'
#' @param save_run_outputs  \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Allows saving all run outputs to directory containing INPUT file. \cr
#' By default, run outputs are stored in a temporary directory and are erased if not exported. \cr
#' Default is FALSE.
#'
#' @return The function returns the numerically determined evolution of stable
#' isotope compositions and mass of element X in all boxes over the run duration as
#' specified in INPUT file. \cr  \cr
#' By default (unless save_run_outputs = TRUE), run outputs are stored in the temporary directory and are not exported. \cr \cr
#' The outputs of the run are stored in a Rda output file
#' with the following file name structure:  \cr
#' \strong{\emph{RUN name + _OUT.Rda}}
#' @section Optional csv outputs to the DIGEST folder are as follows:
#' \enumerate{
#' \item OUT data file storing initial and final size and delta values in all boxes.\cr
#' (file name structure: \strong{\emph{out_1_N_OUT + RUN name + .csv}})
#' \item evS data file storing the evolution with time of the sizes (masses of element X) of all boxes.\cr
#' (file name structure:  \strong{\emph{out_2_N_evS + RUN name + .csv}})
#' \item evD data file storing the evolution with time of the delta values in all boxes.\cr
#' (file name structure:  \strong{\emph{out_3_N_evD + RUN name + .csv}})
#' }
#' @export
num_slvr <- function(input_path,
                     to_DIGEST_csv = FALSE,
                     save_run_outputs = FALSE
                     ){
  # locally bind variables (fixing binding global variable issue)
  CONSTS_IN <- INITIAL_IN <- FLUXES_IN <- COEFFS_IN <- NULL

  ############################## IDENTIFY PREFIX in INPUT FILENAME #####################
  if (stringr::str_detect(input_path, "IN.Rda")){
    input_path <- normalizePath(input_path, winslash = "/")
    namefile <- input_path
    prefix <- stringr::str_remove(basename(input_path), pattern = "_IN.Rda")
    cwd <- dirname(input_path)
  } else {
    rlang::abort('num_slvr \n Wrong file or file name. Path should end with ** IN.Rda **')
  }

  ############################## DEFINE outdir #####################
  run_dir <- paste(prefix, "_DIGEST", sep = "")
  outdir <- paste(cwd, "/", run_dir, "/", sep = "")
  if (isTRUE(save_run_outputs)){
    tmpoutdir <- paste(tempdir(), "/", run_dir, "/", sep = "")
  }

  ############################## LOAD IN.Rda ###################################
  load(namefile)

  ############################## CONSTANTS ###################################
  consts_f <- CONSTS_IN
  ratio_standard = as.numeric(consts_f[consts_f$CONSTS_ID == "Ratio_Standard", "CONSTS"])
  # # issue with as.integer used for time max (and nb_steps: ) as such does not accept values higher than 2147483647
  # time_max = as.integer(as.numeric(consts_f[consts_f$CONSTS_ID == "time", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  # nb_steps = as.integer(as.numeric(consts_f[consts_f$CONSTS_ID == "n_steps", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  # to avoid this issue, as.integer is replaced with ceiling (round to next unity)
  time_max = ceiling(as.numeric(consts_f[consts_f$CONSTS_ID == "time", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  nb_steps = ceiling(as.numeric(consts_f[consts_f$CONSTS_ID == "n_steps", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  time = seq(0, time_max, length = nb_steps)

  ############################## INITIAL ###################################
  initial_f <- INITIAL_IN
  boxes_id = as.character(initial_f$BOXES_ID)
  boxes_nb = length(boxes_id)
  boxes_size = as.numeric(initial_f$SIZE_INIT)
  delta = as.numeric(initial_f$DELTA_INIT)

  ############################## FLUXES ###################################
  fluxes_f <- FLUXES_IN
  fluxes <- as.matrix(fluxes_f[,2:(boxes_nb+1)])
  colnames(fluxes) <- NULL

  ############################## FRACTIONATION COEFFICIENTS ("ALPHAs") ###################################
  coeffs_f <- COEFFS_IN
  coeffs <- as.matrix(coeffs_f[,2:(boxes_nb+1)])
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

  ############################## WRITING FINAL STATE (IN NUM OUT csv) #####################
  df <- initial_f
  colnames(df) <- c("BOXES_ID", "SIZE_FINAL", "DELTA_FINAL")
  df$SIZE_FINAL <- Boxes_size[length(Boxes_size[,1]),1:length(boxes_id)+1]
  df$DELTA_FINAL <- Delta[length(Delta[,1]),1:length(boxes_id)+1]
  df <- dplyr::full_join(initial_f, df, by = "BOXES_ID")

  ############################## save outputs ##############################
  if(isTRUE(save_run_outputs)){
    if (isTRUE(to_DIGEST_csv)){
      if (!dir.exists(outdir)){dir.create(outdir)}
      data.table::fwrite(Delta_as_df, file = paste(outdir,  "out_3_N_evD_", prefix,".csv", sep = ""), row.names = F, quote = F, sep = ",")
      data.table::fwrite(Boxes_size_as_df, file = paste(outdir, "out_2_N_evS_", prefix,".csv", sep = ""), row.names = F, quote = F, sep = ",")
      data.table::fwrite(df, file = paste(outdir, "out_1_N_OUT_", prefix,".csv", sep = ""), row.names = F, quote = F, sep = ",")
    }
    N_evD <- Delta_as_df
    N_evS <- Boxes_size_as_df
    N_OUT <- df
    save(N_OUT, N_evD, N_evS, file = paste(cwd, "/", prefix, "_OUT.Rda", sep = ""))
  } else if (isFALSE(save_run_outputs)){
    if (isTRUE(to_DIGEST_csv)){
      if (!dir.exists(tmpoutdir)){dir.create(tmpoutdir)}
      data.table::fwrite(Delta_as_df, file = paste(tmpoutdir,  "out_3_N_evD_", prefix,".csv", sep = ""), row.names = F, quote = F, sep = ",")
      data.table::fwrite(Boxes_size_as_df, file = paste(tmpoutdir, "out_2_N_evS_", prefix,".csv", sep = ""), row.names = F, quote = F, sep = ",")
      data.table::fwrite(df, file = paste(tmpoutdir, "out_1_N_OUT_", prefix,".csv", sep = ""), row.names = F, quote = F, sep = ",")
    }
    N_evD <- Delta_as_df
    N_evS <- Boxes_size_as_df
    N_OUT <- df
    save(N_OUT, N_evD, N_evS, file = paste(tempdir(), "/", prefix, "_OUT.Rda", sep = ""))
  }
}

#  #_________________________________________________________________________80char
#' Analytically solve stable isotope box models
#' @description  An analytical solver of the system of ordinary differential
#' equations (ODES) of stable isotope ratios of element X in all boxes. \cr
#' Not intended for direct use although possible. \cr
#' The analytical solver finds the eigenvalues and eigenvectors of the ODES. \cr
#' Given the initial conditions as specified in IN.Rda file, it determines the
#' set of analytical solutions that describes the evolution of isotope ratios
#' in each box over time.
#' @param input_path path to the INPUT file containing all commands for the run \cr
#' (character string, file name structure: \strong{\emph{RUN name + _IN.Rda}})
#' @param to_DIGEST_csv edit csv outputs or not (logical) to the RUN DIGEST folder \cr
#' (logical, default is FALSE)
#' @param save_run_outputs  \emph{OPTIONAL} \cr
#' Logical value. \cr
#' Allows saving all run outputs to directory containing INPUT file. \cr
#' By default, run outputs are stored in a temporary directory and are erased if not exported. \cr
#' Default is FALSE.
#'
#' @return The function returns the analytically determined evolution of stable
#' isotope compositions in all boxes over the run duration as specified in INPUT file. \cr \cr
#' By default (unless save_run_outputs = TRUE), run outputs are stored in the temporary directory and are not exported. \cr \cr
#' The outputs of the run are stored in a Rda output file
#' with the following file name structure: \cr
#' \strong{\emph{RUN name + _OUT.Rda}}
#' @section Optional csv outputs to the DIGEST folder are as follows:
#' \enumerate{
#' \item OUT data file with initial and final size and delta values in all boxes. \cr
#' (file name structure: \strong{\emph{out_1_A_OUT + RUN name + .csv}})
#' \item ODE_SOLNs data file summarizing outputs of the analytical solutions of the ODES  \cr
#' (eigenvalues, eigenvectors, relaxation times, constants according to initial conditions).  \cr
#' (file name structure: \strong{\emph{out_2_A_ODE_SOLNs + RUN name + .csv}})
#' \item evD data file of the evolution with time of the delta values in all boxes.  \cr
#' (file name structure: \strong{\emph{out_3_A_evD + RUN name + .csv}})
#' }
#' @export
ana_slvr <- function(input_path,
                     to_DIGEST_csv = FALSE,
                     save_run_outputs = FALSE){
  # locally bind variables (fixing binding global variable issue)
  CONSTS_IN <- INITIAL_IN <- FLUXES_IN <- COEFFS_IN <- NULL

  ############################## IDENTIFY PREFIX in INPUT FILENAME #####################
  if (stringr::str_detect(input_path, "IN.Rda")){
    input_path <- normalizePath(input_path, winslash = "/")
    namefile <- input_path
    prefix <- stringr::str_remove(basename(input_path), pattern = "_IN.Rda")
    cwd <- dirname(input_path)
  } else {
    rlang::abort('ana_slvr \n Wrong file or file name. Path should end with ** IN.Rda **')
  }

  ############################## DEFINE outdir #####################
  run_dir <- paste(prefix, "_DIGEST", sep = "")
  outdir <- paste(cwd, "/", run_dir, "/", sep = "")
  if (isTRUE(save_run_outputs)){
    tmpoutdir <- paste(tempdir(), "/", run_dir, "/", sep = "")
  }

  ############################## LOAD IN.Rda ###################################
  load(namefile)

  ############################## CONSTANTS ###################################
  consts_f <- CONSTS_IN
  ratio_standard = as.numeric(consts_f[consts_f$CONSTS_ID == "Ratio_Standard", "CONSTS"])
  # # issue with as.integer used for time max (and nb_steps: ) as such does not accept values higher than 2147483647
  # time_max = as.integer(as.numeric(consts_f[consts_f$CONSTS_ID == "time", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  # nb_steps = as.integer(as.numeric(consts_f[consts_f$CONSTS_ID == "n_steps", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  # to avoid this issue, as.integer is replaced with ceiling (round to next unity)
  time_max = ceiling(as.numeric(consts_f[consts_f$CONSTS_ID == "time", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  nb_steps = ceiling(as.numeric(consts_f[consts_f$CONSTS_ID == "n_steps", "CONSTS"])) ##### as.integer PREVENTS RUN DURATIONS SHORTER THAN 1 TIME UNIT
  time = seq(0, time_max, length = nb_steps)

  ############################## INITIAL ###################################
  initial_f <- INITIAL_IN
  boxes_id = as.character(initial_f$BOXES_ID)
  boxes_nb = length(boxes_id)
  boxes_size = as.numeric(initial_f$SIZE_INIT)
  delta = as.numeric(initial_f$DELTA_INIT)

  ############################## FLUXES ###################################
  fluxes_f <- FLUXES_IN
  fluxes <- as.matrix(fluxes_f[,2:(boxes_nb+1)])
  colnames(fluxes) <- NULL

  ############################## FRACTIONATION COEFFICIENTS ("ALPHAs") ###################################
  coeffs_f <- COEFFS_IN
  coeffs <- as.matrix(coeffs_f[,2:(boxes_nb+1)])
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


  ############################## EXPORT ODE SOLUTIONS #################################
  eigenval_as_df_inverse <- -1/eigenval_as_df
  colnames(eigenval_as_df_inverse) <- "relax_times"
  ODE_SOLNs <- cbind(eigenval_as_df, eigenval_as_df_inverse)
  colnames(C_as_df) <- "Constants"
  ODE_SOLNs <- cbind(ODE_SOLNs, C_as_df)
  colnames(eigenvec_as_df) <- paste("EigenVec_", rep(1:(length(colnames(eigenvec_as_df))), 1), sep = "")
  ODE_SOLNs <- cbind(ODE_SOLNs, eigenvec_as_df)


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

  ############################## save outputs ##############################
  if(isTRUE(save_run_outputs)){
    if (isTRUE(to_DIGEST_csv)){
      if (!dir.exists(outdir)){dir.create(outdir)}
      data.table::fwrite(results, file = paste(outdir, "out_1_A_OUT_", prefix, ".csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(ODE_SOLNs, file = paste(outdir, "out_2_A_ODE_SOLNs_", prefix, ".csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(d_t_all, file = paste(outdir, "out_3_A_evD_", prefix, ".csv", sep = ""), row.names = F, quote = F)
    }

    A_evD <- d_t_all
    A_ODE_SOLNs <- ODE_SOLNs
    A_OUT <- results
    save(A_OUT, A_evD, A_ODE_SOLNs, file = paste(cwd, "/", prefix, "_OUT.Rda", sep = ""))
  } else if (isFALSE(save_run_outputs)){
    if (isTRUE(to_DIGEST_csv)){
      if (!dir.exists(tmpoutdir)){dir.create(tmpoutdir)}
      data.table::fwrite(results, file = paste(tmpoutdir, "out_1_A_OUT_", prefix, ".csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(ODE_SOLNs, file = paste(tmpoutdir, "out_2_A_ODE_SOLNs_", prefix, ".csv", sep = ""), row.names = F, quote = F)
      data.table::fwrite(d_t_all, file = paste(tmpoutdir, "out_3_A_evD_", prefix, ".csv", sep = ""), row.names = F, quote = F)
    }
    A_evD <- d_t_all
    A_ODE_SOLNs <- ODE_SOLNs
    A_OUT <- results
    save(A_OUT, A_evD, A_ODE_SOLNs, file = paste(tempdir(), "/", prefix, "_OUT.Rda", sep = ""))
  }
}
