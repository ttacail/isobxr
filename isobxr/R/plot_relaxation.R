#  #_________________________________________________________________________80char
#' plot relaxation
#' @description  A function to plot the relaxation of isotope ratios in a system,
#' including characteristic times
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' (character string)
#' @param flux_list Name of the list of fluxes and initial box sizes to be used for the run, \cr
#' calling (by its header name) a single column of the \strong{FLUXES} sheet
#' of the \strong{\emph{isobxr excel master file}}. \cr
#' (character string)
#' @param coeff_list Name of the list of fractionation coefficients to be used for the run, \cr
#' calling (by its header name) a single column of the \strong{COEFFS} sheet
#' of the \strong{\emph{isobxr excel master file}}. \cr
#' (character string)
#' @param spiked_boxes Vector of box names ("BOX_ID") to be spiked. \cr
#' If several boxes are listed, initial spike will be evenly distributed from 0 to spike.max_delta value.
#' @param spike.max_delta Value of the maximum spike isotope composition \cr
#' (in permil on the delta scale). \cr
#' Default is 100 permil.
#' @param n_steps Number of calculation steps. Determines the resolution of the run. \cr
#' Default is 10000.
#' @param hidden_boxes Vector of boxes to hide from plots. \cr
#' For instance c("SOURCE", "SINK"). \cr
#' Default is NULL.
#' @param show.residence_time If TRUE, displays box-specific residence times on plot. \cr
#' Default is FALSE.
#' @param show.facets If TRUE, displays results in box-specific facets. \cr
#' Default is FALSE.
#' @param time_landmarks Vector of time landmarks to display on x-axis (numerical values).
#' @param time_as_log10 If TRUE, uses logarithmic time scale in plot. \cr
#' Default is TRUE.
#' @param isobxr_master_file Name of \strong{\emph{isobxr excel master file}}. \cr
#' Default is "0_ISOBXR_MASTER".
#' @param time.resolution_cut Time below which resolution is increased. Default is NULL.
#' @return A plots showing the evolution of the isotopic ratios in the system until full relaxation, \cr
#' defined as the maximum relaxation time multiplied by 10.
#'
#' @export
#' @examples
#' \dontrun{
#' plot_relaxation(workdir = "/Users/username/Documents/1_ABC_tutorial",
#'                 flux_list = "Fx6_ABC_open_bal",
#'                 coeff_list = "a0",
#'                 n_steps = 1000,
#'                 spiked_boxes = c("SOURCE"))
#'}
plot_relaxation <- function(
    workdir,
    flux_list,
    coeff_list,
    spiked_boxes,
    spike.max_delta = 100,
    n_steps = 10000,
    hidden_boxes = NULL,
    show.residence_time = TRUE,
    show.facets = FALSE,
    time_landmarks = NULL,
    time_as_log10 = TRUE,
    isobxr_master_file = "0_ISOBXR_MASTER",
    time.resolution_cut = NULL){

  # spike.max_delta = 100
  # n_steps = 10000
  # hidden_boxes = NULL
  # show.residence_time = TRUE
  # show.facets = FALSE
  # time_landmarks = NULL
  # time_as_log10 = TRUE
  #
  # workdir = paste("/Users/sz18642/OneDrive - University of Bristol/5_isobxr/dev_ongoing/1_isobxr_V2/",
  #                 "1_ABCD_dev", sep = "")
  # flux_list = "Fx6_ABC_open_bal_1"
  # coeff_list = "a0"
  # # n_steps = 1e4,
  # # time_landmarks = c(5*1189),
  # spike.max_delta = 1
  # spiked_boxes = c("SOURCE", "A",  "B", "C")
  # hidden_boxes =  c("SINK", "D")

  label <- y <-
    VAR_TYPE <- VAR <- Time <- ormag <-
    RES_TIME <- INFINITE <- BOX_ID <- NULL

  # 0. tuto mode ####
  tuto_setup <- using_extdata_tutorial_2(workdir = workdir,
                                         save_outputs = FALSE,
                                         plot_results = FALSE)

  workdir <- tuto_setup$workdir

  if (tuto_setup$tuto_mode) isobxr_master_file <- "0_ISOBXR_MASTER.xlsx"

  # I. check arguments ####
  # _a. workdir ####
  if(!dir.exists(workdir)){
    rlang::abort("workdir not found.")
  }

  workdir_old <- getwd()
  on.exit(setwd(workdir_old), add = TRUE)
  setwd(workdir)

  # _b. local arguments ####
  time_unit <- NULL
  SERIES_ID <- "Relaxation"

  if(length(spiked_boxes) > 1){
    DELTA.spike <- rev(seq.int(0,spike.max_delta,
                               ((spike.max_delta - 0)/(length(spiked_boxes) - 1))))
  } else (
    DELTA.spike <- spike.max_delta
  )

  # II. Initial run for constants calculations ####
  # _a. run ####

  run.solve <- quiet(sim.single_run(workdir = workdir,
                                    SERIES_ID = SERIES_ID,
                                    flux_list = flux_list,
                                    coeff_list = coeff_list,
                                    t_max = 10,
                                    n_steps = 10,
                                    FORCING_DELTA = data.frame(BOX_ID = spiked_boxes,
                                                               DELTA.t0 = DELTA.spike),
                                    export.delta_plot = F,
                                    plot.time_as_log10 = T,
                                    export.diagrams = F,
                                    export.data_as_csv_xlsx = F,
                                    isobxr_master_file = isobxr_master_file,
                                    save_outputs = F,
                                    show.delta_plot = F,
                                    suppress_messages = T,
                                    return_data = T,
                                    solver = "analytical"))

  # _b. extract constants ####
  # __i. time units ####
  initial_time_unit <- run.solve$inputs$CONSTS$TIME_UNIT
  if(is.null(time_unit)){
    display_time_unit <- initial_time_unit
  } else {
    display_time_unit <- time_unit
  }

  # __ii. residence times ####
  min_restime_INF <- run.solve$inputs$BOX_META %>%
    dplyr::select(BOX_ID, INFINITE, RES_TIME) %>%
    dplyr::filter(INFINITE == "INFINITE") %>%
    dplyr::pull(RES_TIME)
  if(length(min_restime_INF) > 0){
    min_restime_INF <- min(min_restime_INF)
  } else {
    min_restime_INF <- NULL
  }

  restime_FIN <- run.solve$inputs$BOX_META %>%
    dplyr::select(BOX_ID, INFINITE, RES_TIME) %>%
    dplyr::filter(INFINITE == "FINITE") %>%
    dplyr::filter(! BOX_ID %in% run.solve$inputs$bx.groups$disconnected)

  # __iii. relaxation times ####
  relax_times <- run.solve$outputs$diffeq_solutions$relax_times
  relax_times <- relax_times[relax_times < min_restime_INF/10 & relax_times > 0] %>%
    as.data.frame() %>%
    dplyr::rename("relax_times" = ".")

  relax_times <- relax_times %>%
    dplyr::mutate(ormag = log10(relax_times)) %>%
    dplyr::mutate(ormag = ifelse(ormag < 0, floor(ormag), ceiling(ormag))) %>%
    dplyr::mutate(label = ifelse(ormag < 0, dec_n(relax_times, -ormag+1), dec_n(relax_times, 0)))

  max.relax_times <- max(relax_times$relax_times)

  # III. relaxation runs ####
  # _i. short run ####

  if (!is.null(time.resolution_cut)) {
    t_max.short <- time.resolution_cut
  } else {
    t_max.short <- 50*min(relax_times$relax_times)
  }

  run.short <- quiet(sim.single_run(workdir = workdir,
                                    SERIES_ID = SERIES_ID,
                                    flux_list = flux_list,
                                    coeff_list = coeff_list,
                                    t_max = t_max.short,
                                    n_steps = n_steps/10,
                                    FORCING_DELTA = data.frame(BOX_ID = spiked_boxes,
                                                               DELTA.t0 = DELTA.spike),
                                    export.delta_plot = F,
                                    plot.time_as_log10 = T,
                                    export.diagrams = F,
                                    export.data_as_csv_xlsx = F,
                                    isobxr_master_file =  isobxr_master_file,
                                    save_outputs = F,
                                    show.delta_plot = F,
                                    suppress_messages = T,
                                    return_data = T,
                                    solver = "analytical"))

  # _ii. long run ####
  run.long <- quiet(sim.single_run(workdir = workdir,
                                   SERIES_ID = SERIES_ID,
                                   flux_list = flux_list,
                                   coeff_list = coeff_list,
                                   t_max = 10*(max.relax_times),
                                   n_steps = n_steps-(n_steps/10),
                                   FORCING_DELTA = data.frame(BOX_ID = spiked_boxes,
                                                              DELTA.t0 = DELTA.spike),
                                   export.delta_plot = F,
                                   plot.time_as_log10 = T,
                                   export.diagrams = F,
                                   export.data_as_csv_xlsx = F,
                                   isobxr_master_file =  isobxr_master_file,
                                   save_outputs = F,
                                   show.delta_plot = F,
                                   suppress_messages = T,
                                   return_data = T,
                                   solver = "analytical"))

  # _iii. prepare data set ####
  evD <- run.short$outputs$delta_vs_t %>%
    dplyr::bind_rows(run.long$outputs$delta_vs_t %>%
                dplyr::filter(Time > max(run.short$outputs$delta_vs_t$Time)))

  evD_vert <- DF_verticalizer(df_hor = evD,
                              vert_col = run.long$inputs$bx.groups$all)
  if(!is.null(hidden_boxes)){
    evD_vert <- evD_vert[!evD_vert$VAR_TYPE %in% hidden_boxes, ]
  }

  evD_vert <- time_converter(dataframe = evD_vert,
                             time_colname = "Time",
                             conv_timecolname = "Time_conv",
                             former_unit = initial_time_unit,
                             new_unit = display_time_unit)

  evD_vert$Time <- evD_vert$Time_conv

  # IV. plot ####
  # _i. prepare plot ####
  Ymin <- round(min(evD_vert$VAR), 0)
  Ymax <- round(max(evD_vert$VAR), 0)
  Yrange <- Ymax - Ymin

  if(log10(Yrange) <= 0){
    digits <- abs(floor(log10(Yrange)))
  } else {
    digits <- 0
  }

  Ybin <- signif((Ymax-Ymin)/5, digits = 1) # automatic definition of Ybin
  relax_times$y <- Ymax

  evD_initial <- evD_vert[evD_vert$Time == min(evD_vert$Time),]
  evD_final <- evD_vert[evD_vert$Time == max(evD_vert$Time),]

  time.min_non_zero <- evD_vert %>%
    dplyr::filter(Time != 0) %>%
    dplyr::pull(Time) %>%
    min()

  evD_vert <- evD_vert %>%
    # dplyr::filter(VAR_TYPE == "C") %>%
    dplyr::mutate(Time = ifelse(Time == 0, time.min_non_zero / 100, Time))

  # _ii. bulk plot ####
  evD_plot <- ggplot2::ggplot(data = evD_vert, ggplot2::aes(x = Time, y = VAR, color = VAR_TYPE))+
    ggplot2::theme_linedraw()+
    ggplot2::labs(# y = "Equilibration progress (%)",
      y = paste("d", run.solve$inputs$CONSTS$NUMERATOR, "/",
                run.solve$inputs$CONSTS$DENOMINATOR,
                run.solve$inputs$CONSTS$ELEMENT,
                " (permil)",
                sep = ""),
      x = paste0("Time (", display_time_unit, ")"),
      caption = paste0("Gray annotated times correspond to relaxation times.",
                       "\n",
                       "Color coded annotated times are residence times for each finite box.",
                       "\n",
                       "Red annotated times are manual landmarks."),
      title = paste0("Relaxation plot", " - Flux list: ", flux_list),
      subtitle = paste0("(", run.solve$outputs$solver, " solver)")
      # subtitle = NET_COEFFS_title
    )+
    ggplot2::geom_hline(yintercept = Ymax, linetype = 2, size = 0.5, color = "gray75")+
    ggplot2::geom_hline(yintercept = Ymin, linetype = 2, size = 0.5, color = "gray75")+
    ggplot2::theme(plot.subtitle = ggplot2::element_text(size=7),
                   panel.grid = ggplot2::element_blank(),
                   legend.position = "right",
                   strip.text = ggplot2::element_text(face = "bold", size = 12),
                   plot.caption = ggplot2::element_text(hjust = 0)
    )+
    ggplot2::scale_y_continuous(limits=c(Ymin-Ybin, Ymax+Ybin), breaks=seq(Ymin-Ybin, Ymax+Ybin, by = Ybin)) +
    ggplot2::geom_segment(ggplot2::aes(x = relax_times, xend = relax_times,
                              y = Ymin, yend = Ymax),
                          data = relax_times,
                          color = "gray40",
                          linetype = 3,
                          size = 0.25) +
    ggrepel::geom_text_repel(data = relax_times,
                             ggplot2::aes(x = relax_times,
                                          y = y,
                                          label = label),
                             color = "gray40",
                             nudge_y = 0.05*Yrange,
                             hjust = 0.5
    ) +
    ggplot2::guides(color = ggplot2::guide_legend("BOX ID"))

  # _iii. option: time landmarks ####
  if(!is.null(time_landmarks)){
    time_landmarks <- as.data.frame(time_landmarks)
    time_landmarks$y <- 100
    time_landmarks <- time_landmarks %>%
      dplyr::mutate(ormag = log10(time_landmarks)) %>%
      dplyr::mutate(ormag = ifelse(ormag < 0, floor(ormag), ceiling(ormag))) %>%
      dplyr::mutate(label = ifelse(ormag < 0, dec_n(time_landmarks, -ormag+1), dec_n(time_landmarks, 0)))
    evD_plot <- evD_plot +
      ggplot2::geom_segment(ggplot2::aes(x = time_landmarks,
                                xend = time_landmarks,
                                y = Ymin,
                                yend = Ymax),
                            data = time_landmarks,
                            color = "darkred",
                            linetype = 2,
                            size = 0.5) +
      ggrepel::geom_text_repel(data = time_landmarks,
                               ggplot2::aes(x = time_landmarks,
                                            y = Ymin,
                                            label = label),
                               color = "darkred",
                               nudge_y = -0.05*Yrange,
                               hjust = 0.5
      )
  }

  # _iv. option: residence times ####
  if(show.residence_time){
    restime_FIN$y <- 100
    restime_FIN <- restime_FIN %>%
      dplyr::mutate(ormag = log10(RES_TIME)) %>%
      dplyr::mutate(ormag = ifelse(ormag < 0, floor(ormag), ceiling(ormag))) %>%
      dplyr::mutate(label = ifelse(ormag < 0, dec_n(RES_TIME, -ormag+1), dec_n(RES_TIME, 0)))
    evD_plot <- evD_plot +
      ggplot2::geom_segment(ggplot2::aes(x = RES_TIME,
                                xend = RES_TIME,
                                y = Ymin,
                                yend = Ymax,
                                color = BOX_ID),
                            data = restime_FIN,
                            linetype = 2,
                            size = 0.5) +
      ggrepel::geom_text_repel(data = restime_FIN,
                               ggplot2::aes(x = RES_TIME,
                                            y = Ymin,
                                            color = BOX_ID,
                                            label = paste0(BOX_ID, ": ", label)),
                               nudge_y = -0.05*Yrange,
                               hjust = 0.5
      )
  }

  # _v. plot curves ####
  evD_plot <- evD_plot + ggplot2::geom_line(cex = .75)

  # _vi. option: log10 x scale ####
  if(time_as_log10){
    evD_plot <- evD_plot +
      ggplot2::scale_x_log10(breaks = sort(c(10**(seq(from = -20, to = 20, by = 1)))),
                             limits = c(1e-20, 1e+20))
  }

  evD_plot <- evD_plot +
    ggplot2::coord_cartesian(xlim = c(min(relax_times$relax_times)*.01,
                                      max(relax_times$relax_times)*10),
                             ylim = c(Ymin - 0.05*Yrange, Ymax + 0.05*Yrange))

  # _vii. option: show facets ####
  if(show.facets){
    evD_plot <- evD_plot +
      ggplot2::facet_wrap(VAR_TYPE ~ . )
  }

  # evD_plot

  # IV. return plot ####
  return(evD_plot)
}
