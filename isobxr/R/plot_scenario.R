#  #_________________________________________________________________________80char
#' plot sim.scenario outputs
#' @description A function to plot delta and size vs time from the sim.scenario runs and to include observations along simulations.
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' (character string)
#' @param scenario_dir_name name of sim.scenario SERIES directory (starts with 3_SCEN)
#' @param shown_runs Vector of successive run numbers (RUN_n) to be displayed (e.g., 1:5). \cr
#' Default is run 2 to last run (hides initial relaxation run)
#' @param time_unit Time unit to use on plot if different from native time unit. \cr
#' Character string, to be selected among the following: \cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr} \cr
#' Default is NULL.
#' @param hidden_boxes List of boxes to hide from plots, as a vector of character strings. \cr
#' For instance c("SOURCE", "SINK").
#' @param return_as_print If TRUE, prints delta and size vs. time plots in a single page figure on R. \cr
#' If FALSE, returns separately delta and size vs. time plots as list of two editable R objects.
#' @param show.facets If TRUE, shows delta vs. time as faceted by BOX. \cr
#' Default is FALSE.
#' @param show.run_separations If TRUE, shows limits between subruns. \cr
#' Default is TRUE.
#' @param observations_file Name of the csv file containing observations (without csv extension). \cr
#' Observation csv file should contain the following columns: \cr
#' \enumerate{
#' \item \strong{GROUP}: observation subset group label if relevant
#' \item \strong{BOX_ID}: BOX ID (e.g., A, OCEAN...) as defined in isobxr master file.
#' \item \strong{delta.def} definition of delta value, e.g., d18O
#' \item \strong{obs.delta} average observed delta numerical value
#' \item \strong{obs.CI} confidence interval of delta value
#' \item \strong{obs.CI.def} definition of confidence interval, e.g., 95% ci
#' \item \strong{obs.counts} number of observations corresponding to average delta
#' \item \strong{Time} time of observation in scenario timeline, in display time units
#' }
#' Default is NULL.
#' @param observations_groups vector of observations groups to include in plot, \cr
#' from GROUP column in observation csv file. \cr
#' Default is NULL.
#' @return A set of plots showing the evolution of delta and sizes with time.
#' @export
plot_scenario <- function(workdir,
                          scenario_dir_name,
                          shown_runs = NULL,
                          time_unit = NULL,
                          hidden_boxes = NULL,
                          return_as_print = TRUE,
                          show.facets = FALSE,
                          show.run_separations = TRUE,
                          observations_file = NULL,
                          observations_groups = NULL) {

  # # 0. debug arguments ####
  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list = ls())
  # # rm(list = ls()[!ls() %in% c("sr_paths")])
  # gc()
  # devtools::load_all(".")
  #
  # shown_runs = NULL
  # time_unit = NULL
  # hidden_boxes = NULL
  # return_as_print = TRUE
  # show.facets = FALSE
  # show.run_separations = TRUE
  # observations_file = NULL
  # observations_groups = NULL
  #
  # workdir = "/Users/sz18642/isobxr Gd 2023/2_rat_dZn"
  # scenario_dir_name = "3_SCEN_2_supplier_to_lucerne_001"
  # show.facets = T
  # show.run_separations = F
  #
  # observations_file = "all_obs_av_by_group"
  # observations_groups = c("supplier", "lucerne")

  obs.CI <- obs.delta <-
    Time <- GROUP <- VAR_TYPE <- VAR <-
    Time_plot <- Time_COMPOSITE <- RUN_n <- NULL

  # I. check arguments ####
  # _a. workdir
  if(!dir.exists(workdir)){
    rlang::abort("workdir not found.")
  }

  workdir_old <- getwd()
  on.exit(setwd(workdir_old), add = TRUE)
  setwd(workdir)

  # _b. scenario_dir_name ####
  names.output.loc <-
    names.output[names.output$func == "sim.scenario",] %>%
    clear_subset()

  # prefix <- names.output.loc$old.prefix
  prefix <- names.output.loc$prefix

  if (stringr::str_ends(scenario_dir_name, pattern = "/")){
    scenario_dir_name <- stringr::str_sub(scenario_dir_name, end = -2)
  }

  if(!scenario_dir_name %>% stringr::str_starts(pattern = prefix)){
    rlang::abort(paste0("scenario_dir_name should start with ", prefix))
  }

  if(!scenario_dir_name %in% list.files(workdir)){
    rlang::abort("scenario_dir_name not found.")
  }

  # _c. read scenario_results ####
  # path.scenario_results <- paste0(scenario_dir_name, "/",
  #                                 names.output.loc$old.digest_dir,
  #                                 "/",
  #                                 paste0(names.output.loc$old.acronym, "_",
  #                                        stringr::str_remove(scenario_dir_name, pattern = paste0(prefix, "_")),
  #                                        "_results.rds"))

  path.scenario_results <- paste0(scenario_dir_name, "/",
                                  names.output.loc$digest_dir,
                                  "/",
                                  paste0(names.output.loc$acronym, "_",
                                         stringr::str_remove(scenario_dir_name, pattern = paste0(prefix, "_")),
                                         "_results.rds"))

  if (!file.exists(path.scenario_results)){
    rlang::abort(paste("scenario results file not found at the following adress:", "\n",
                       path.scenario_results))
  }

  results <- readRDS(path.scenario_results)


  # _c. read observations ###
  if (!is.null(observations_file)) {
    observations <- data.table::fread(file = paste0(observations_file, ".csv"))
  } else {
    observations <- NULL
  }

  # _d. shown_runs ####
  if(is.null(shown_runs)){
    shown_runs <- results$scenario_master$RUN_SEQUENCE$RUN_n
    shown_runs <- shown_runs[2:length(shown_runs)]
  } else {
    if (!all(abs(diff(shown_runs)) == 1)){
      rlang::abort("shown_runs should be a vector of sequential integers")
    }

    if (!any(shown_runs %in% results$scenario_master$RUN_SEQUENCE$RUN_n)) {
      rlang::abort(
        paste0(
          "shown_runs should share at least one run number with the scenario.",
          "\n",
          "You asked plot_scenario to show runs number ",
          paste(shown_runs, collapse = ", "),
          "\n",
          "The current scenario entails the runs number ",
          paste(results$scenario_master$RUN_SEQUENCE$RUN_n,
                collapse = ", ")
        )
      )
    }

    if (max(shown_runs) > max(results$scenario_master$RUN_SEQUENCE$RUN_n)){
      shown_runs <- min(shown_runs):max(results$scenario_master$RUN_SEQUENCE$RUN_n)
    }
  }



  # II. plot ####
  # _a. plot delta ####
  # __i. prepare evD ####
  evD <- results$delta_vs_t %>%
    dplyr::filter(RUN_n %in% shown_runs) %>%
    dplyr::mutate(Time_plot = Time_COMPOSITE - min(Time_COMPOSITE) ) %>%
    clear_subset()

  # reset time units
  initial_time_unit <- results$isobxr_master$CONSTANTS$TIME_UNIT

  if(is.null(time_unit)){
    display_time_unit <- initial_time_unit
  } else {
    display_time_unit <- time_unit
  }

  evD <- time_converter(dataframe = evD,
                        time_colname = "Time_plot",
                        conv_timecolname = "Time_plot_conv",
                        former_unit = initial_time_unit,
                        new_unit = display_time_unit)

  evD$Time_plot <- evD$Time_plot_conv

  # extract composite sub-runs (zones)
  k <- 1
  SERIES_RUN_ID_plot <- levels(evD$SERIES_RUN_ID)
  for (k in 1:length(SERIES_RUN_ID_plot)){4
    min_time_loc <- min(evD[evD$SERIES_RUN_ID == SERIES_RUN_ID_plot[k], "Time"])
    if (k == 1){
      evD_zones <- evD[evD$SERIES_RUN_ID == SERIES_RUN_ID_plot[k] & evD$Time == min_time_loc, ]
    } else {
      evD_zones <- rbind(evD_zones, evD[evD$SERIES_RUN_ID == SERIES_RUN_ID_plot[k] & evD$Time == min_time_loc, ])
    }
    k <- k + 1
  }

  # verticalize evD
  evD_zones_vert <- DF_verticalizer(df_hor = evD_zones, vert_col = results$isobxr_master$bx.groups$all)
  evD_vert <- DF_verticalizer(df_hor = evD, vert_col = results$isobxr_master$bx.groups$all)

  # hide unwanted boxes for delta plot
  if (!is.null(hidden_boxes)){
    evD_zones_vert <- evD_zones_vert[!(evD_zones_vert$VAR_TYPE %in% hidden_boxes), ] %>% clear_subset()
    evD_vert <- evD_vert[!(evD_vert$VAR_TYPE %in% hidden_boxes), ] %>% clear_subset()
  }

  # __ii. prepare plot arguments  ####
  #### set limits of plot
  Ymin <- round(min(evD_vert$VAR), 0)-1
  Ymax <- round(max(evD_vert$VAR), 0)+1
  Ymin_zoom <- min(evD_vert$VAR)
  Ymax_zoom <- max(evD_vert$VAR)
  # Ybin <- 0.25
  Ybin <- signif((Ymax-Ymin)/10, digits = 1) # automatic definition of Ybin
  Xmax <- max(evD_vert$Time_plot) + 0.1*max(evD_vert$Time_plot)

  #### extract t0 and t_final delta values
  evD_initial <- evD_vert[evD_vert$Time_plot == min(evD_vert$Time_plot),]
  evD_final <- evD_vert[evD_vert$Time_plot == max(evD_vert$Time_plot),]

  # __iii. plot evD vs T / all ####
  evD_plot <- ggplot2::ggplot(data = evD_vert, ggplot2::aes(x = Time_plot, y = VAR, color = VAR_TYPE))

  if (show.run_separations){
    for (k in 1:nrow(evD_zones)){
      evD_plot <- evD_plot +
        ggplot2::geom_vline(xintercept = evD_zones[k, "Time_plot"] ,  linetype = 2, size = 0.25)+
        ggplot2::annotate(geom = "text", x = evD_zones[k, "Time_plot"], y = Ymax_zoom + 0.05*Ymax_zoom,
                          label = evD_zones[k, "RUN_n"], hjust = -0.5)
    }
  }

  evD_plot <- evD_plot +
    ggplot2::geom_line(cex = 1)+
    ggplot2::theme_linedraw()+
    ggplot2::scale_y_continuous(limits=c(Ymin, Ymax), breaks=seq(Ymin, Ymax, by = Ybin), labels = dec_2)+
    ggplot2::labs(y = paste("d", results$isobxr_master$CONSTANTS$NUMERATOR, "/",
                            results$isobxr_master$CONSTANTS$DENOMINATOR,
                            results$isobxr_master$CONSTANTS$ELEMENT,
                            " (permil)",
                            sep = ""),
                  x = paste0("Time (", display_time_unit, ")"),
                  title = paste(results$paths$SERIES_ID, sep = "")
                  )+
    ggrepel::geom_text_repel(data = evD_final, ggplot2::aes(label = VAR_TYPE,
                                                            color = VAR_TYPE),
                             nudge_x = 0.05 * max(evD_vert$Time_plot),  hjust = 0)+
    ggplot2::theme(legend.position = "None",
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", size = 12),
                   plot.caption = ggplot2::element_text(hjust = 0)
    )

  if (!is.null(observations)) show.facets <- TRUE

  if (show.facets){
    # ___1. delta in facets ####
    evD_plot <- evD_plot + ggplot2::facet_wrap(VAR_TYPE ~ . , scales = "free_y")

    if(!is.null(observations)){
      # ___2. include observations ####
      if (!is.null(observations_groups)){
        observations <- observations %>% dplyr::filter(GROUP %in% observations_groups)
      }

      observations$VAR_TYPE <- observations$BOX_ID

      if (!is.null(hidden_boxes)){
        observations <- observations %>% dplyr::filter(!VAR_TYPE %in% hidden_boxes)
      }

      evD_plot <- evD_plot +
        ggplot2::geom_point(inherit.aes = F,
                            data = observations,
                            ggplot2::aes(x = Time,
                                         y = obs.delta,
                                         shape = GROUP),
                            size = 3) +

        ggplot2::geom_errorbar(inherit.aes = F,
                               data = observations,
                               ggplot2::aes(x = Time,
                                            y = obs.delta,
                                            ymin = obs.delta - obs.CI,
                                            ymax = obs.delta + obs.CI),
                               size = .25,
                               width = 0)

      Ymin_zoom <- min(Ymin_zoom, observations$obs.delta, na.rm = T)
      Ymax_zoom <- max(Ymax_zoom, observations$obs.delta, na.rm = T)
    }
  }

  evD_plot <- evD_plot +
    ggplot2::coord_cartesian(ylim = c(Ymin_zoom, Ymax_zoom + 0.05*Ymax_zoom), xlim = c(0.1, Xmax))

  # _b. plot sizes ####
  # __i. prepare evD ####
  #### subset evD for plot (hide the first args$hidden_runs runs)
  evS <- results$size_vs_t %>%
    dplyr::filter(RUN_n %in% shown_runs) %>%
    dplyr::mutate(Time_plot = Time_COMPOSITE - min(Time_COMPOSITE) ) %>%
    clear_subset()

  #### reset time units
  evS <- time_converter(dataframe = evS, time_colname = "Time_plot",
                        conv_timecolname = "Time_plot_conv",
                        former_unit = initial_time_unit,
                        new_unit = display_time_unit)

  evS$Time_plot <- evS$Time_plot_conv

  #### extract composite sub-runs (zones)
  SERIES_RUN_ID_plot <- levels(evS$SERIES_RUN_ID)
  for (k in 1:length(SERIES_RUN_ID_plot)){4
    min_time_loc <- min(evS[evS$SERIES_RUN_ID == SERIES_RUN_ID_plot[k], "Time"])
    if (k == 1){
      evS_zones <- evS[evS$SERIES_RUN_ID == SERIES_RUN_ID_plot[k] & evS$Time == min_time_loc, ]
    } else {
      evS_zones <- rbind(evS_zones, evS[evS$SERIES_RUN_ID == SERIES_RUN_ID_plot[k] & evS$Time == min_time_loc, ])
    }
  }

  #### verticalize evS
  evS_zones_vert <- DF_verticalizer(df_hor = evS, vert_col = results$isobxr_master$bx.groups$all)
  evS_vert <- DF_verticalizer(df_hor = evS, vert_col = results$isobxr_master$bx.groups$all)

  # hide unwanted boxes for delta plot
  if (!is.null(hidden_boxes)){
    evS_zones_vert <- evS_zones_vert[!(evS_zones_vert$VAR_TYPE %in% hidden_boxes), ] %>% clear_subset()
    evS_vert <- evS_vert[!(evS_vert$VAR_TYPE %in% hidden_boxes), ] %>% clear_subset()
  }

  # __ii. prepare plot arguments ####
  #### set limits of plot
  Ymin <- round(min(evS_vert$VAR), 0)-1
  Ymax <- round(max(evS_vert$VAR), 0)+1
  Ymin_zoom <- min(evS_vert$VAR)
  Ymax_zoom <- max(evS_vert$VAR)
  Ybin <- 0.25
  Xmax <- max(evS_vert$Time_plot) + 0.1*max(evS_vert$Time_plot)

  #### extract t0 and t_final delta values
  evS_initial <- evS_vert[evS_vert$Time_plot == min(evS_vert$Time_plot),]
  evS_final <- evS_vert[evS_vert$Time_plot == max(evS_vert$Time_plot),]

  # __iii. plot evS vs T / in facets ####
  evS_plot_facet <- ggplot2::ggplot(data = evS_vert, ggplot2::aes(x = Time_plot, y = VAR, color = VAR_TYPE))

  if (show.run_separations){
    for (k in 1:nrow(evS_zones)){
      evS_plot_facet <- evS_plot_facet +
        ggplot2::geom_vline(xintercept = evS_zones[k, "Time_plot"] ,  linetype = 2, size = 0.25)
    }
  }

  evS_plot.ncols <- 3
  evS_plot.nrows <- ceiling(length(unique(as.character(evS_vert$VAR_TYPE)))/evS_plot.ncols)

  evS_plot_facet <- evS_plot_facet +
    ggplot2::geom_line(cex = 1)+
    ggplot2::theme_linedraw()+
    ggplot2::facet_wrap(. ~ VAR_TYPE, scales = "free_y", ncol = evS_plot.ncols)+
    ggplot2::coord_cartesian(xlim = c(0.1, Xmax))+
    ggplot2::labs(y = paste0(results$isobxr_master$CONSTANTS$ELEMENT,
                               " (",results$isobxr_master$CONSTANTS$MASS_UNIT,")"),
                  x = paste0("Time (", display_time_unit, ")"),
                  title = paste(results$isobxr_master$CONSTANTS$ELEMENT, "mass vs. time"),
                  caption = paste("Hidden runs (RUN_n): ",
                                  paste(results$scenario_master$RUN_SEQUENCE$RUN_n[!results$scenario_master$RUN_SEQUENCE$RUN_n %in% shown_runs], collapse = ", ")))+
    ggplot2::theme(legend.position = "None",
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", size = 12),
                   plot.caption = ggplot2::element_text(hjust = 0)
                   )

  if (return_as_print){
    if (!show.facets) {
    gridExtra::grid.arrange(
      gridExtra::arrangeGrob(
        evD_plot,
        evS_plot_facet,
        ncol = 1,
        heights = c(1,evS_plot.nrows*.5)))
    } else {
      gridExtra::grid.arrange(
        gridExtra::arrangeGrob(
          evD_plot,
          evS_plot_facet,
          ncol = 1,
          heights = c(1,1)))
    }
  } else {
    return(list(plot.delta_vs_t = evD_plot,
                plot.size_vs_t = evS_plot_facet))
  }
}
