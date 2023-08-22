#  #_________________________________________________________________________80char
#' plot sweep.dyn_2D outputs
#' @description  A function to plot delta vs time from the sweep.dyn_2D runs
#'
#' @param workdir Working directory of \strong{\emph{isobxr excel master file}}
#' and where output files will be stored if exported by user.
#' (character string)
#' @param sweep_dir_name Full name of sweep.dyn_2D SERIES directory (character string)
#' @param time_unit Time unit to use on plot if different from native time unit. \cr
#' Character string, to be selected among the following: \cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr} \cr
#' Default is NULL.
#' @param time_range Time range to zoom on as vector of two values, such as: c(0,100) \cr
#' Values in displayed time units. \cr
#' Default is NULL.
#' @param hidden_boxes List of boxes to hide from plots, as a vector of character strings. \cr
#' For instance c("SOURCE", "SINK"). \cr
#' Default is NULL.
#' @param return_as_print If TRUE, prints delta and size vs. time plots in a single page figure on R. \cr
#' If FALSE, returns separately delta and size vs. time plots as list of editable R objects. \cr
#' Default is TRUE.
#' @param free_y_scale If TRUE, frees Y axis scale. \cr
#' Default is TRUE.
#' @param swap_sweep_params If TRUE, swaps the sweep parameter 1 and 2 representations
#' from color to facet scales. \cr
#' Default is FALSE.
#' @param show.delta_drift If TRUE, displays drift of delta values from t0.
#' @param time_as_log10 If TRUE, uses logarithmic time scale in plot. \cr
#' Default is TRUE.
#' @return A set of plots showing the evolution of delta and sizes with time.
#'
#' @export
plot_dyn_2D <- function(workdir,
                        sweep_dir_name,
                        time_unit = NULL,
                        time_range = NULL,
                        hidden_boxes = NULL,
                        return_as_print = TRUE,
                        free_y_scale = TRUE,
                        swap_sweep_params = FALSE,
                        show.delta_drift = FALSE,
                        time_as_log10 = TRUE){

  # ########################################################
  # setwd("/Users/sz18642/isobxr/isobxr")
  # rm(list = ls())
  # gc()
  # devtools::load_all(".")
  # #######################################################
  # time_unit = NULL
  # time_range = NULL
  # hidden_boxes = NULL
  # return_as_print = TRUE
  # free_y_scale = TRUE
  # swap_sweep_params = FALSE
  #
  # workdir = "/Users/sz18642/isobxr Gd 2023/3_human_dCa"
  # sweep_dir_name = "4_DYN_sweep_dyn_bone_loss_0001"
  # return_as_print = T
  # swap_sweep_params = F
  # free_y_scale = T
  # hidden_boxes =  c("WASTE", "SKIN", "MILK", "KDN", "FET", "DIET", "GIT", "ST", "FEC")

  value.t0 <-value <-BOX_ID <-VAR_EXPLO_2 <-VAR_EXPLO_1 <-Time_plot <-Time <-name <-RUN_n <- NULL

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
    names.output[names.output$func == "sweep.dyn_2D",] %>%
    clear_subset()

  # prefix <- names.output.loc$old.prefix
  prefix <- names.output.loc$prefix

  if (stringr::str_ends(sweep_dir_name, pattern = "/")){
    sweep_dir_name <- stringr::str_sub(sweep_dir_name, end = -2)
  }

  if(!sweep_dir_name %>% stringr::str_starts(pattern = prefix)){
    rlang::abort(paste0("sweep_dir_name should start with ", prefix))
  }

  if(!sweep_dir_name %in% list.files(workdir)){
    rlang::abort("sweep_dir_name not found.")
  }

  # _c. read scenario_results ####
  # path.sweep_results <- paste0(sweep_dir_name, "/",
  #                                 names.output.loc$old.digest_dir,
  #                                 "/",
  #                                 paste0(names.output.loc$old.acronym, "_",
  #                                        stringr::str_remove(sweep_dir_name, pattern = paste0(prefix, "_")),
  #                                        "_results.rds"))

  path.sweep_results <- paste0(sweep_dir_name, "/",
                               names.output.loc$digest_dir,
                               "/",
                               paste0(names.output.loc$acronym, "_",
                                      stringr::str_remove(sweep_dir_name, pattern = paste0(prefix, "_")),
                                      "_results.rds"))

  if (!file.exists(path.sweep_results)){
    rlang::abort(paste("sweep results file not found at the following adress:", "\n",
                       path.sweep_results))
  }

  results <- readRDS(path.sweep_results)

  # II. plot ####
  # _a. plot delta ####
  # __i. prepare evD ####

  d.bx <- paste0("d.", results$isobxr_master$bx.groups$all)
  m.bx <- paste0("m.", results$isobxr_master$bx.groups$all)
  bx <- results$isobxr_master$bx.groups$all

  evDS <- results$delta_size_vs_t %>%
    dplyr::filter( (RUN_n %% 2) == 0) %>%
    clear_subset()

  # reset time units ####
  initial_time_unit <- results$isobxr_master$CONSTANTS$TIME_UNIT

  if(is.null(time_unit)){
    display_time_unit <- initial_time_unit
  } else {
    display_time_unit <- time_unit
  }

  evDS <- time_converter(dataframe = evDS,
                        time_colname = "Time",
                        conv_timecolname = "Time_plot",
                        former_unit = initial_time_unit,
                        new_unit = display_time_unit)

  # verticalize evDS ####
  evDS_vert <- evDS %>%
    tidyr::pivot_longer(cols = c(d.bx, m.bx)) %>%
    as.data.frame() %>%
    dplyr::arrange(name, RUN_n, Time) %>%
    clear_subset() %>%
    tidyr::separate(name, sep = "\\.", into = c("VAR_TYPE", "BOX_ID"))

  # hide unwanted boxes for delta plot ####
  if (!is.null(hidden_boxes)){
    evDS_vert <- evDS_vert[!(evDS_vert$BOX_ID %in% hidden_boxes), ] %>% clear_subset()
  }

  # DEFINE TITLES ####
  EXPLO_caption = paste(" Hidden equilibration run",
                        paste0("Flux-coeff config.: ", results$sweep_log[1,"COEFF_FLUX"]),
                        sep = " \n ")

  if (levels(as.factor(evDS_vert$LEGEND_EXPLO_1)) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
    EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(as.factor(evDS_vert$LEGEND_EXPLO_1)), ": ", paste(c(min(levels(as.factor(evDS_vert$VAR_EXPLO_1))),
                                                                                                                     max(levels(as.factor(evDS_vert$VAR_EXPLO_1)))),
                                                                                                                   collapse = " to "), collapse = ", "), sep = "")
  } else {
    EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(evDS_vert$LEGEND_EXPLO_1), ": ", paste(c(min(evDS_vert$VAR_EXPLO_1), max(evDS_vert$VAR_EXPLO_1)), collapse = " to "), collapse = ", "), sep = "")
  }

  if (levels(as.factor(evDS_vert$LEGEND_EXPLO_2)) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
    EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(as.factor(evDS_vert$LEGEND_EXPLO_2)), ": ", paste(c(min(levels(as.factor(evDS_vert$VAR_EXPLO_2))),
                                                                                                                     max(levels(as.factor(evDS_vert$VAR_EXPLO_2)))),
                                                                                                                   collapse = " to "), collapse = ", "), sep = "")
  } else {
    EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(evDS_vert$LEGEND_EXPLO_2), ": ", paste(c(min(evDS_vert$VAR_EXPLO_2), max(evDS_vert$VAR_EXPLO_2)), collapse = " to "), collapse = ", "), sep = "")
  }

  EXPLO_subtitle <- paste(EXPLO_subtitle_1, "\n",
                          EXPLO_subtitle_2, sep = "")

  # free scale ####
  if (free_y_scale){
    Y.scales <- "free_y"
  } else {
    Y.scales <- "fixed"
  }

  # time_range ####
  if (!is.null(time_range)){
    if(length(time_range) != 2){ rlang::abort("time_range should be a vector with min and max time to plot (in displayed units).") }
    if (min(time_range) >= max(evDS_vert$Time_plot)) rlang::abort("time_range min should be lower than max time (in displayed units).")
    if (max(time_range) <= min(evDS_vert$Time_plot)) rlang::abort("time_range max should be lower than min time (in displayed units).")
    evDS_vert <- evDS_vert %>%
      dplyr::filter(Time_plot >= min(time_range) & Time_plot <= max(time_range))
  }

  # plot delta values ####
  data_loc <- evDS_vert[evDS_vert$VAR_TYPE == "d",]

  EXPLO_title <- paste(sweep_dir_name, " - ",
                       "\u03B4", results$isobxr_master$CONSTANTS$ELEMENT,
                       " (", results$isobxr_master$CONSTANTS$NUMERATOR, "/",
                       results$isobxr_master$CONSTANTS$DENOMINATOR,", \u2030", ") ", sep = "")

  if (show.delta_drift){
    data_loc <- data_loc %>% dplyr::full_join(
      data_loc %>%
        dplyr::select(VAR_EXPLO_1, VAR_EXPLO_2, BOX_ID, Time_plot, value) %>%
        dplyr::filter(Time_plot == 0) %>%
        dplyr::select(-Time_plot) %>%
        dplyr::rename(value.t0 = "value"),
      by = c("VAR_EXPLO_1", "VAR_EXPLO_2", "BOX_ID")) %>%
      dplyr::mutate(value = value-value.t0) %>%
      dplyr::select(-value.t0)

    delta_type <- "Drift from t0 of d"
  } else {
    delta_type <- "d"
  }


  if (swap_sweep_params) {

    VAR_EXPLO_2.labs <- paste(data_loc[1,"LEGEND_EXPLO_2"], "\n", as.character(sort(unique(data_loc[,"VAR_EXPLO_2"]))), sep = "")
    names(VAR_EXPLO_2.labs) <- as.character(sort(unique(data_loc[,"VAR_EXPLO_2"])))

    if (data_loc[1,"LEGEND_EXPLO_2"] %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
      strip.text.x.size <- 7
    } else {
      strip.text.x.size <- 12
    }

    plot.delta_vs_t <- ggplot2::ggplot(data = data_loc,
                    ggplot2::aes(x = Time_plot,
                                 y = value,
                                 color = (VAR_EXPLO_1),
                                 group = (VAR_EXPLO_1)))+
      ggplot2::geom_line()+
      ggplot2::facet_grid(BOX_ID ~ VAR_EXPLO_2, scales = Y.scales, labeller = ggplot2::labeller(VAR_EXPLO_2 = VAR_EXPLO_2.labs))+
      ggplot2::theme_bw()+
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = strip.text.x.size, colour = "white", face = "bold"),
                     strip.text.y = ggplot2::element_text(size = 12, colour = "white", face = "bold"),
                     strip.background = ggplot2::element_rect(fill = "black"),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(hjust = 0),
                     plot.title = ggplot2::element_text(face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 8))+
      ggplot2::labs(x = paste("Time (", display_time_unit, ")", sep = ""),
                    y = paste(delta_type, results$isobxr_master$CONSTANTS$NUMERATOR, "/",
                              results$isobxr_master$CONSTANTS$DENOMINATOR,
                              results$isobxr_master$CONSTANTS$ELEMENT,
                              " (permil)",
                              sep = ""),
                    title = EXPLO_title,
                    subtitle = EXPLO_subtitle,
                    caption = EXPLO_caption,
                    color = data_loc[1,"LEGEND_EXPLO_2"]
                    )


    if (is.numeric(data_loc$VAR_EXPLO_1)){
      plot.delta_vs_t <- plot.delta_vs_t +
        ggplot2::scale_color_gradientn(name = data_loc[1,"LEGEND_EXPLO_1"], colors = rainbow(100)) +
        ggplot2::theme(legend.position = "right")
    } else {
      plot.delta_vs_t <- plot.delta_vs_t +
        ggplot2::theme(legend.position = "bottom")
    }

  } else if (!swap_sweep_params){

    VAR_EXPLO_1.labs <- paste(data_loc[1,"LEGEND_EXPLO_1"], "\n", as.character(sort(unique(data_loc[,"VAR_EXPLO_1"]))), sep = "")
    names(VAR_EXPLO_1.labs) <- as.character(sort(unique(data_loc[,"VAR_EXPLO_1"])))

    if (data_loc[1,"LEGEND_EXPLO_1"] %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
      strip.text.x.size <- 7
    } else {
      strip.text.x.size <- 12
    }

    plot.delta_vs_t <- ggplot2::ggplot(data = data_loc,
                    ggplot2::aes(x = Time_plot,
                                 y = value,
                                 color = VAR_EXPLO_2,
                                 group = VAR_EXPLO_2))+
      ggplot2::geom_line()+

      # ggplot2::facet_grid(. ~ LEGEND_EXPLO_1, labeller = ggplot2::labeller(X = X.labs))+
      ggplot2::facet_grid(BOX_ID ~ VAR_EXPLO_1, scales = Y.scales, labeller = ggplot2::labeller(VAR_EXPLO_1 = VAR_EXPLO_1.labs))+
      ggplot2::theme_bw()+
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = strip.text.x.size, colour = "white", face = "bold"),
                     strip.text.y = ggplot2::element_text(size = 12, colour = "white", face = "bold"),
                     strip.background = ggplot2::element_rect(fill = "black"),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(hjust = 0),
                     plot.title = ggplot2::element_text(face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 8))+
      ggplot2::labs(x = paste("Time (", display_time_unit, ")", sep = ""),
                    y = paste(delta_type, results$isobxr_master$CONSTANTS$NUMERATOR, "/",
                              results$isobxr_master$CONSTANTS$DENOMINATOR,
                              results$isobxr_master$CONSTANTS$ELEMENT,
                              " (permil)", sep = ""),
                    title = EXPLO_title,
                    subtitle = EXPLO_subtitle,
                    caption = EXPLO_caption,
                    color = data_loc[1,"LEGEND_EXPLO_2"]
      )

    if (is.numeric(data_loc$VAR_EXPLO_2)){
      plot.delta_vs_t <- plot.delta_vs_t +
        ggplot2::scale_color_gradientn(name = data_loc[1,"LEGEND_EXPLO_2"], colors = rainbow(100)) +
        ggplot2::theme(legend.position = "right")
    } else {
      plot.delta_vs_t <- plot.delta_vs_t +
        ggplot2::theme(legend.position = "bottom",
                       legend.direction = "vertical")
    }

  }

  # plot size values ####
  data_loc <- evDS_vert[evDS_vert$VAR_TYPE == "m",]

  EXPLO_title <- paste(sweep_dir_name, " - ",
                       "Mass of ", results$isobxr_master$CONSTANTS$ELEMENT, " (", results$isobxr_master$CONSTANTS$MASS_UNIT, ")",
                       sep = "")

  if (swap_sweep_params) {
    VAR_EXPLO_2.labs <- paste(data_loc[1,"LEGEND_EXPLO_2"], "\n", as.character(sort(unique(data_loc[,"VAR_EXPLO_2"]))), sep = "")
    names(VAR_EXPLO_2.labs) <- as.character(sort(unique(data_loc[,"VAR_EXPLO_2"])))

    if (data_loc[1,"LEGEND_EXPLO_2"] %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
      strip.text.x.size <- 7
    } else {
      strip.text.x.size <- 12
    }

    plot.size_vs_t <- ggplot2::ggplot(data = data_loc,
                                       ggplot2::aes(x = Time_plot,
                                                    y = value,
                                                    color = (VAR_EXPLO_1),
                                                    group = (VAR_EXPLO_1)))+
      ggplot2::geom_line()+
      ggplot2::facet_grid(BOX_ID ~ VAR_EXPLO_2, scales = Y.scales, labeller = ggplot2::labeller(VAR_EXPLO_2 = VAR_EXPLO_2.labs))+
      ggplot2::theme_bw()+
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = strip.text.x.size, colour = "white", face = "bold"),
                     strip.text.y = ggplot2::element_text(size = 12, colour = "white", face = "bold"),
                     strip.background = ggplot2::element_rect(fill = "black"),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(hjust = 0),
                     plot.title = ggplot2::element_text(face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 8))+
      ggplot2::labs(x = paste("Time (", display_time_unit, ")", sep = ""),
                    y = paste("mass of ",
                              results$isobxr_master$CONSTANTS$ELEMENT,
                              " (", results$isobxr_master$CONSTANTS$MASS_UNIT, ")",
                              sep = ""),
                    title = EXPLO_title,
                    subtitle = EXPLO_subtitle,
                    caption = EXPLO_caption,
                    color = data_loc[1,"LEGEND_EXPLO_2"]
      )


    if (is.numeric(data_loc$VAR_EXPLO_1)){
      plot.size_vs_t <- plot.size_vs_t +
        ggplot2::scale_color_gradientn(name = data_loc[1,"LEGEND_EXPLO_1"], colors = rainbow(100)) +
        ggplot2::theme(legend.position = "right")
    } else {
      plot.size_vs_t <- plot.size_vs_t +
        ggplot2::theme(legend.position = "bottom")
    }

  } else if (!swap_sweep_params){

    VAR_EXPLO_1.labs <- paste(data_loc[1,"LEGEND_EXPLO_1"], "\n", as.character(sort(unique(data_loc[,"VAR_EXPLO_1"]))), sep = "")
    names(VAR_EXPLO_1.labs) <- as.character(sort(unique(data_loc[,"VAR_EXPLO_1"])))

    if (data_loc[1,"LEGEND_EXPLO_1"] %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
      strip.text.x.size <- 7
    } else {
      strip.text.x.size <- 12
    }

    plot.size_vs_t <- ggplot2::ggplot(data = data_loc,
                                       ggplot2::aes(x = Time_plot,
                                                    y = value,
                                                    color = VAR_EXPLO_2,
                                                    group = VAR_EXPLO_2))+
      ggplot2::geom_line()+
      # ggplot2::facet_grid(. ~ LEGEND_EXPLO_1, labeller = ggplot2::labeller(X = X.labs))+
      ggplot2::facet_grid(BOX_ID ~ VAR_EXPLO_1, scales = Y.scales, labeller = ggplot2::labeller(VAR_EXPLO_1 = VAR_EXPLO_1.labs))+
      ggplot2::theme_bw()+
      ggplot2::theme(strip.text.x = ggplot2::element_text(size = strip.text.x.size, colour = "white", face = "bold"),
                     strip.text.y = ggplot2::element_text(size = 12, colour = "white", face = "bold"),
                     strip.background = ggplot2::element_rect(fill = "black"),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.caption = ggplot2::element_text(hjust = 0),
                     plot.title = ggplot2::element_text(face = "bold"),
                     plot.subtitle = ggplot2::element_text(size = 8))+
      ggplot2::labs(x = paste("Time (", display_time_unit, ")", sep = ""),
                    y = paste("mass of ",
                              results$isobxr_master$CONSTANTS$ELEMENT,
                              " (", results$isobxr_master$CONSTANTS$MASS_UNIT, ")",
                              sep = ""),
                    title = EXPLO_title,
                    subtitle = EXPLO_subtitle,
                    caption = EXPLO_caption,
                    color = data_loc[1,"LEGEND_EXPLO_2"]
      )

    if (is.numeric(data_loc$VAR_EXPLO_2)){
      plot.size_vs_t <- plot.size_vs_t +
        ggplot2::scale_color_gradientn(name = data_loc[1,"LEGEND_EXPLO_2"], colors = rainbow(100)) +
        ggplot2::theme(legend.position = "right")
    } else {
      plot.size_vs_t <- plot.size_vs_t +
        ggplot2::theme(legend.position = "bottom",
                       legend.direction = "vertical")
    }

  }

  if (time_as_log10) {
    plot.delta_vs_t <- plot.delta_vs_t + ggplot2::scale_x_log10()
    plot.size_vs_t <- plot.size_vs_t + ggplot2::scale_x_log10()
  }

  if (return_as_print){
    print(plot.delta_vs_t)
  } else {
    return(list(plot.delta_vs_t = plot.delta_vs_t,
                plot.size_vs_t = plot.size_vs_t))
  }

}
