#  #_________________________________________________________________________80char
#' plot sim.single_run outputs
#' @description  A function to plot delta and size vs time from the sim.single_run outputs
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' (character string)
#' @param RUN_ID ID of the run (formerly SERIES_RUN_ID).
#' Corresponds to the name of the .rds file storing the results of the run.
#' @param time_as_log10 If TRUE, uses logarithmic time scale in plot. \cr
#' Default is TRUE.
#' @param time_unit Time unit to use on plot if different from native time unit. \cr
#' Character string, to be selected among the following: \cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr} \cr
#' Default is NULL.
#' @param hidden_boxes List of boxes to hide from plots, as a vector of character strings. \cr
#' For instance c("SOURCE", "SINK").
#' @param return_as_print If TRUE, prints delta and size vs. time plots in a single page figure on R. \cr
#' If FALSE, returns separately delta and size vs. time plots as list of editable R objects. \cr
#' Default is TRUE.
#' @return A set of plots showing the evolution of delta and sizes with time.
#' @export
plot_single_run <- function(workdir,
                            RUN_ID,
                            time_as_log10 = TRUE,
                            time_unit = NULL,
                            hidden_boxes = NULL,
                            return_as_print = TRUE){
  # ########################################################
  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list = ls())
  # gc()
  # devtools::load_all(".")
  # ########################################################
  # workdir = "/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/1_ABCD_dev"
  # RUN_ID = "1_ABC_balanced_closed_0001"
  # time_as_log10 = TRUE
  # time_unit = NULL
  # hidden_boxes = NULL

  VAR_TYPE <- VAR <- Time <- NULL

  # I. check arguments ####
  # _a. workdir
  if(!dir.exists(workdir)){
    rlang::abort("workdir not found.")
  }

  workdir_old <- getwd()
  on.exit(setwd(workdir_old), add = TRUE)
  setwd(workdir)

  # _b. RUN_ID ####
  names.output.loc <- names.output[names.output$func == "sim.single_run", ] %>% clear_subset()
  # prefix <- names.output.loc$old.prefix
  prefix <- names.output.loc$prefix

  if (stringr::str_ends(RUN_ID, pattern = "/")){
    RUN_ID <- stringr::str_sub(RUN_ID, end = -2)
  }

  # _c. read scenario_results ####
  # path.run_results <- paste0(paste0(names.output.loc$old.prefix, "_",
  #                                   gsub("_\\d+$", "", RUN_ID)),
  #                            "/",
  #                            paste0(RUN_ID, ".rds"))

  path.run_results <- paste0(paste0(names.output.loc$prefix, "_",
                                    gsub("_\\d+$", "", RUN_ID)),
                             "/",
                             paste0(RUN_ID, ".rds"))

  if (!file.exists(path.run_results)){
    rlang::abort(paste("run results file not found at the following adress:", "\n",
                       path.run_results))
  }

  results <- readRDS(path.run_results)

  # _a. extract evD/eVS ####
  evD <- results$outputs$delta_vs_t
  evS <- results$outputs$size_vs_t

  # _b. build evD plot  ####
  # __i. verticalize evD/evS ####
  evD_vert <- DF_verticalizer(df_hor = evD, vert_col = results$outputs$final_state$BOX_ID %>% as.character())
  evS_vert <- DF_verticalizer(df_hor = evS, vert_col = results$outputs$final_state$BOX_ID %>% as.character())

  # __ii. convert time units ####
  initial_time_unit <- results$inputs$CONSTS$TIME_UNIT
  if (!is.null(time_unit)){
    display_time_unit <- time_unit
  }  else {
    display_time_unit <- initial_time_unit
  }

  evD_vert <- time_converter(dataframe = evD_vert, time_colname = "Time",
                             conv_timecolname = "Time_conv",
                             former_unit = initial_time_unit,
                             new_unit = display_time_unit)
  evD_vert$Time <- evD_vert$Time_conv
  evS_vert <- time_converter(dataframe = evS_vert, time_colname = "Time",
                             conv_timecolname = "Time_conv",
                             former_unit = initial_time_unit,
                             new_unit = display_time_unit)
  evS_vert$Time <- evS_vert$Time_conv

  # __iii. define plot args ####
  Ymin <- round(min(evD_vert$VAR), 0)-1
  Ymax <- round(max(evD_vert$VAR), 0)+1
  Ymin_zoom <- min(evD_vert$VAR)
  Ymax_zoom <- max(evD_vert$VAR)
  # Ybin <- 0.25
  Ybin <- signif((Ymax-Ymin)/10, digits = 1) # automatic definition of Ybin

  if(time_as_log10) {
    Xmin <- evD_vert[2,"Time"]/2
    Xmax <- max(evD_vert$Time) + 0.5*max(evD_vert$Time)
  } else {
    Xmin <- evD_vert[1,"Time"]
    Xmax <- max(evD_vert$Time) + 0.1*max(evD_vert$Time)
  }

  # __iv. drop boxes ####
  BOXES_drop <- results$inputs$bx.groups$disconnected
  BOXES_drop <- unique(BOXES_drop, hidden_boxes)

  if (length(BOXES_drop) > 0){
    evD_vert <- evD_vert[-which(evD_vert$VAR_TYPE %in% BOXES_drop),]
    evS_vert <- evS_vert[-which(evS_vert$VAR_TYPE %in% BOXES_drop),]
  }

  evD_initial <- evD_vert[evD_vert$Time == min(evD_vert$Time),]
  evD_final <- evD_vert[evD_vert$Time == max(evD_vert$Time),]

  evS_initial <- evS_vert[evS_vert$Time == min(evS_vert$Time),]
  evS_final <- evS_vert[evS_vert$Time == max(evS_vert$Time),]


  FLUXES_title <- paste("Flux config    : " , results$inputs$LOG$FLUX_MASTER, sep = "")
  COEFFS_title <- paste("Coeffs config : " , results$inputs$LOG$COEFF_RUN, sep = "")
  if (!is.nan(results$inputs$LOG$FORCING_RAYLEIGH)){
    COEFFS_title <- paste(COEFFS_title,
                          " // Rayleigh forcing: ", results$inputs$LOG$FORCING_RAYLEIGH, sep = "")
  }

  if (!is.nan(results$inputs$LOG$FORCING_ALPHA)){
    COEFFS_title <- paste(COEFFS_title, " // Alpha forcing: ", results$inputs$LOG$FORCING_ALPHA, sep = "")
  }


  # _c. plot evD ####
  evD_plot <- ggplot2::ggplot(data = evD_vert, ggplot2::aes(x = Time, y = VAR, color = VAR_TYPE))+
    ggplot2::geom_line(cex = 1)+
    ggplot2::theme_bw()+
    ggplot2::scale_y_continuous(limits=c(Ymin, Ymax), breaks=seq(Ymin, Ymax, by = Ybin), labels = dec_2)+
    ggplot2::coord_cartesian(ylim = c(Ymin_zoom, Ymax_zoom), xlim = c(Xmin, Xmax))+
    ggplot2::labs(y =  paste("d", results$inputs$CONSTS$NUMERATOR, "/",
                             results$inputs$CONSTS$DENOMINATOR, results$inputs$CONSTS$ELEMENT, " (permil)", sep = ""),
                  x = paste("Time (", display_time_unit, ")", sep = ""),
                  title = paste(results$paths$SERIES_RUN_ID, " (", results$outputs$solver, ")", sep = ""),
                  subtitle = paste(FLUXES_title, COEFFS_title, sep = "\n"))+
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8),
                   legend.position = "None")+
    ggrepel::geom_text_repel(data = evD_final, segment.linetype = 2,
                             ggplot2::aes(label = paste(VAR_TYPE, " (", dec_2(VAR), ")", sep = ""), color = VAR_TYPE),
                             nudge_x = 0.05*max(evD_vert$Time),  hjust = 0)

  if (time_as_log10){
    suppressWarnings(evD_plot <- evD_plot + ggplot2::scale_x_log10())
  }

  # _d. plot evS ####
  evS_plot.ncols <- 4

  evS_plot <- ggplot2::ggplot(data = evS_vert, ggplot2::aes(x = Time, y = VAR, color = VAR_TYPE))+
    ggplot2::geom_line(cex = 1)+
    ggplot2::theme_bw()+
    ggplot2::facet_wrap(VAR_TYPE~., scales = "free_y", ncol = evS_plot.ncols)+
    ggplot2::labs(y = paste(results$inputs$CONSTS$ELEMENT, " (", results$inputs$CONSTS$MASS_UNIT, ")", sep = ""),
                  x = paste("Time (", display_time_unit, ")", sep = ""),
                  title = paste(results$inputs$CONSTS$ELEMENT, " mass vs. time", sep = ""))+
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 8),
                   legend.position = "None")

  evS_plot.nrows <- ceiling(length(unique(as.character(evS_vert$VAR_TYPE)))/evS_plot.ncols)

  if (time_as_log10){
    suppressWarnings(evS_plot <- evS_plot + ggplot2::scale_x_log10())
  }

  if (return_as_print){
    gridExtra::grid.arrange(
      gridExtra::arrangeGrob(
        evD_plot,
        evS_plot,
        ncol = 1,
        heights = c(1, evS_plot.nrows*.5)))
  } else {
    return(list(plot.delta_vs_t = evD_plot,
                plot.size_vs_t = evS_plot))
  }
}
