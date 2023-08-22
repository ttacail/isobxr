#  #_________________________________________________________________________80char
#' Numerically solve stable isotope box models
#' @description A numerical solver of the system of ordinary differential equations (ODES), \cr
#' describing the evolution of stable isotope ratios in all boxes of a system. \cr
#' Not intended for manual use. \cr
#' The numerical solver uses the \strong{\emph{deSolve::ode}} function
#' to integrate the stable isotopes ratios over time in each box. It allows the
#' calculation of the evolution of stable isotope ratio in the case of
#' unbalanced outward and inward fluxes of element X in a given box
#' resulting in the accumulation or loss of element X.
#' @param IN input data, edited by \code{\link{sim.single_run}} (list of dataframes)
#' @param paths paths object edited by \code{\link{sim.single_run}} (list of characters)
#' @param to_DIGEST_csv if TRUE, edits csv outputs to DIGEST directory \cr
#' Default is FALSE.
#' @param return_results if TRUE, results returned as a list of R objects. \cr
#' Default is FALSE.
#'
#' @return Numerically determined evolution of stable isotope compositions \cr
#' and masses of element X in all boxes over the run duration as specified in INPUT file.
#' \cr  \cr
#' Run outputs are stored in a temporary directory and not exported by \code{\link{solve_numerically}}.
#' \cr \cr
#' The outputs of the run are stored in the rds output file in the SERIES directory
#' with the following file name structure:  \cr
#' \strong{\emph{SERIES_ID + RUN_n.rds}}
#'
#' @section Optional csv outputs to DIGEST directory are as follows:
#' \enumerate{
#' \item OUT data file storing initial and final size and delta values in all boxes.\cr
#' (file name structure: \strong{\emph{out_1_N_OUT + RUN name + .csv}})
#' \item evS data file storing the evolution with time of the sizes (masses of element X) of all boxes.\cr
#' (file name structure:  \strong{\emph{out_2_N_evS + RUN name + .csv}})
#' \item evD data file storing the evolution with time of the delta values in all boxes.\cr
#' (file name structure:  \strong{\emph{out_3_N_evD + RUN name + .csv}})
#' }
#' @export
solve_numerically <- function(IN,
                              paths,
                              to_DIGEST_csv = FALSE,
                              return_results = FALSE){
  rm(list=ls()[!ls() %in% c("IN", "paths", "to_DIGEST_csv", "return_results")])

  # CONSTANTS ####
  time <- seq(0, IN$CONSTS$t_max, length = IN$CONSTS$n_steps+1)

  # INITIAL ####
  boxes_id <- as.character(IN$INITIAL$BOX_ID)
  boxes_nb <- length(boxes_id)
  boxes_size <- as.numeric(IN$INITIAL$SIZE.t0)
  delta <- as.numeric(IN$INITIAL$DELTA.t0)

  # FLUXES ####
  fluxes <- as.matrix(IN$FLUXES[,2:(boxes_nb+1)])
  colnames(fluxes) <- NULL

  # FRACTIONATION COEFFICIENTS ("ALPHAs ####
  coeffs <- as.matrix(IN$COEFFS[,2:(boxes_nb+1)])
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
  Ratio <- ((delta/1e3 + 1e0)*IN$CONSTS$RATIO_STANDARD)
  parms <- list(fluxes = fluxes, coeffs = coeffs, boxes_size = boxes_size)
  Ratio <- deSolve::ode(y = Ratio, parms = parms, times = time, func =  evol_ratio)
  Delta <- ((Ratio / IN$CONSTS$RATIO_STANDARD) - 1.0) * 1000

  ############################## WRITING DELTA EVOLUTION #####################
  N_evD <- as.data.frame(Delta)
  N_evD$time <- time
  colnames(N_evD) <- c("Time", boxes_id)

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
  N_evS <- as.data.frame(Boxes_size)
  colnames(N_evS) <- c("Time", boxes_id)

  ############################## WRITING FINAL STATE (IN NUM OUT csv) #####################
  N_OUT <- IN$INITIAL
  N_OUT$SIZE.t_max <- Boxes_size[length(Boxes_size[,1]),1:length(boxes_id)+1]
  N_OUT <- dplyr::full_join(N_OUT,
                            N_evD[nrow(N_evD), boxes_id] %>%
                              clear_subset() %>%
                              t() %>%
                              as.data.frame() %>%
                              dplyr::mutate(BOX_ID = boxes_id, .before = "V1") %>%
                              dplyr::rename("DELTA.t_max" = "V1"),
                            by = "BOX_ID")
  # save csv ####
  if (to_DIGEST_csv){

    if (!dir.exists(to_tmpdir(paths$digest_dir))) dir.create(to_tmpdir(paths$digest_dir))

    data.table::fwrite(N_evD,
                       file = paste(to_tmpdir(paths$digest_dir), "out_3_N_evD_", paths$SERIES_RUN_ID, ".csv", sep = ""),
                       row.names = F, quote = F)

    data.table::fwrite(N_evS,
                       file = paste(to_tmpdir(paths$digest_dir), "out_2_N_evS_", paths$SERIES_RUN_ID, ".csv", sep = ""),
                       row.names = F, quote = F)

    data.table::fwrite(N_OUT,
                       file = paste(to_tmpdir(paths$digest_dir), "out_1_N_OUT_", paths$SERIES_RUN_ID, ".csv", sep = ""),
                       row.names = F, quote = F)
  }

  # save RDS/RDA ####
  outputs <- list(solver = "numerical",
                  final_state = N_OUT,
                  delta_vs_t = N_evD,
                  size_vs_t = N_evS)

  saveRDS(list(inputs = IN,
               outputs = outputs,
               paths = paths),
          file = paste(to_tmpdir(paths$outdir), "/", paths$SERIES_RUN_ID, ".rds", sep = ""))

  if (return_results){
    return(outputs)
  } else {
    return(NULL)
  }
}
