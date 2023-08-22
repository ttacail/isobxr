#  #_________________________________________________________________________80char
#' Analytically solve stable isotope box models
#' @description  An analytical solver of the system of ordinary differential \cr
#' equations (ODES) of stable isotope ratios of element X in all boxes. \cr
#' Not intended for manual use. \cr
#' The analytical solver finds the eigenvalues and eigenvectors of the ODES. \cr
#' It determines the set of analytical solutions that describes
#' the evolution of isotope ratios in each box over time.
#' @param IN input data, edited by \code{\link{sim.single_run}} (list of dataframes)
#' @param paths paths object edited by \code{\link{sim.single_run}} (list of characters)
#' @param to_DIGEST_csv if TRUE, edits csv outputs to DIGEST directory \cr
#' Default is FALSE.
#' @param return_results if TRUE, results returned as a list of R objects. \cr
#' Default is FALSE.
#'
#' @return Analytically determined evolution of stable isotope compositions
#' in all boxes over the run duration as specified in INPUT file. \cr \cr
#'
#' Run outputs are stored in a temporary directory and not exported by \code{\link{solve_numerically}}.
#' \cr \cr
#' The outputs of the run are stored in the rds output file in the SERIES directory
#' with the following file name structure:  \cr
#' \strong{\emph{SERIES_ID + RUN_n.rds}}
#'
#' @section Optional csv outputs to the DIGEST folder are as follows:
#' \enumerate{
#' \item OUT data file with initial and final size and delta values in all boxes. \cr
#' (file name structure: \strong{\emph{out_1_A_OUT + SERIES_ID + RUN_n + .csv}})
#' \item ODE_SOLNs data file summarizing outputs of the analytical solutions of the ODES  \cr
#' (eigenvalues, eigenvectors, relaxation times, constants according to initial conditions).  \cr
#' (file name structure: \strong{\emph{out_2_A_ODE_SOLNs + SERIES_ID + RUN_n + .csv}})
#' \item evD data file of the evolution with time of the delta values in all boxes.  \cr
#' (file name structure: \strong{\emph{out_3_A_evD + SERIES_ID + RUN_n + .csv}})
#' }
#' @export
solve_analytically <- function(IN,
                               paths,
                               to_DIGEST_csv = FALSE,
                               return_results = FALSE){

  rm(list=ls()[!ls() %in% c("IN", "paths", "to_DIGEST_csv", "return_results")])

  # time series ####
  # issue with as.integer used for time max (and nb_steps: )
  # as such does not accept values higher than 2147483647
  # to avoid this issue, as.integer is replaced with ceiling (round to next unity)
  time <- seq(0, IN$CONSTS$t_max, length = IN$CONSTS$n_steps+1)

  # INITIAL ####
  boxes_id <- as.character(IN$INITIAL$BOX_ID)
  boxes_nb <- length(boxes_id)
  boxes_size <- as.numeric(IN$INITIAL$SIZE.t0)
  delta <- as.numeric(IN$INITIAL$DELTA.t0)

  # FLUXES ####
  fluxes <- as.matrix(IN$FLUXES[,2:(boxes_nb+1)])
  colnames(fluxes) <- NULL

  # FRACTIONATION COEFFICIENTS = ALPHAs ####
  coeffs <- as.matrix(IN$COEFFS[,2:(boxes_nb+1)])
  colnames(coeffs) <- NULL

  # MATRIX ODE SYSTEM ####
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

 # SOLVING SYSTEM ####
  MatSystem=InitSystem(VecM,MatJ,MatD)
  eigenval <- eigen(MatSystem)$values
  eigenval_as_df <- as.data.frame(eigenval)
  colnames(eigenval_as_df) <- "Eigenvalues"
  eigenvec <- eigen(MatSystem)$vectors
  eigenvec_as_df = as.data.frame(eigenvec)
  colnames(eigenvec_as_df) <- rep(0:(length(colnames(eigenvec_as_df))-1), 1)
  ratio_standard_init = IN$CONSTS$RATIO_STANDARD*((0.001*delta)+1)

  A = eigenvec
  B = ratio_standard_init*rep(1,length(eigenval))
  C = solve(A, B )
  C_as_df <- as.data.frame(C)
  colnames(C_as_df) <- "COEFFs"

  # CALCULATE DELTA FINAL ####
  R = (C*exp(eigenval*IN$CONSTS$t_max)) %*% t(eigenvec)
  d = ((R/IN$CONSTS$RATIO_STANDARD)-1)*1000
  d_as_df = as.data.frame(t(d))
  colnames(d_as_df) <- "DELTA.t_max"

  # EXPORT DELTA FINAL, EIGENVALUES, EQUATION COEFFICIENTS ####
  A_OUT <- IN$INITIAL
  A_OUT$SIZE.t_max <- A_OUT$SIZE.t0
  A_OUT <- cbind(A_OUT, d_as_df)

 # EXPORT ODE SOLUTIONS ####i
  eigenval_as_df_inverse <- -1/eigenval_as_df
  colnames(eigenval_as_df_inverse) <- "relax_times"
  A_ODE_SOLNs <- cbind(eigenval_as_df, eigenval_as_df_inverse)
  colnames(C_as_df) <- "Constants"
  A_ODE_SOLNs <- cbind(A_ODE_SOLNs, C_as_df)
  colnames(eigenvec_as_df) <- paste("EigenVec_", rep(1:(length(colnames(eigenvec_as_df))), 1), sep = "")
  A_ODE_SOLNs <- cbind(A_ODE_SOLNs, eigenvec_as_df)

  # CALCULATE AND WRITE evD with solutions from ODE EigenVec/Vals/Constants ####
  loc.ANA_delta_t_Calculator <- function(x) {
    ANA_delta_t_Calculator(t = x,
                           ODE_Constants = A_ODE_SOLNs$Constants,
                           ODE_Eigenvalues = A_ODE_SOLNs$Eigenvalues,
                           ODE_Eigenvectors = eigenvec_as_df,
                           BOXES_IDs = boxes_id,
                           ratio_standard = IN$CONSTS$RATIO_STANDARD)
  }

  A_evD <-  sapply(time,
                   function(x) loc.ANA_delta_t_Calculator(x),
                   simplify = T) %>%
    t() %>% as.data.frame() %>%
    tidyr::unnest(cols = c("Time", dplyr::all_of(boxes_id))) %>% as.data.frame()

  # prepare size vs t ####
  S <- as.data.frame(t(A_OUT[, c("SIZE.t0")]))
  names(S) <- as.character(A_OUT$BOX_ID)
  A_evS <- dplyr::bind_rows(replicate(nrow(A_evD), S, simplify = FALSE))
  A_evS$Time <- A_evD$Time
  A_evS <- A_evS[,c("Time", names(S))]

  # save csv ####
  if (to_DIGEST_csv){
    if (!dir.exists(to_tmpdir(paths$digest_dir))) dir.create(to_tmpdir(paths$digest_dir))

    data.table::fwrite(A_OUT,
                       file = paste(to_tmpdir(paths$digest_dir), "out_1_A_OUT_",
                                    paths$SERIES_RUN_ID, ".csv", sep = ""),
                       row.names = F, quote = F)

    data.table::fwrite(A_ODE_SOLNs,
                       file = paste(to_tmpdir(paths$digest_dir), "out_2_A_ODE_SOLNs_",
                                    paths$SERIES_RUN_ID, ".csv", sep = ""),
                       row.names = F, quote = F)

    data.table::fwrite(A_evD,
                       file = paste(to_tmpdir(paths$digest_dir), "out_3_A_evD_",
                                    paths$SERIES_RUN_ID, ".csv", sep = ""),
                       row.names = F, quote = F)
  }

  # save RDA/RDS ####
  outputs <- list(solver = "analytical",
                  final_state = A_OUT,
                  delta_vs_t = A_evD,
                  size_vs_t = A_evS,
                  diffeq_solutions = A_ODE_SOLNs )

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
