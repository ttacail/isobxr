#  #_________________________________________________________________________80char
#' plot_freq_ADSR
#'
#' @description  Plot frequency profiles by fitted parameter of ADSR types
#'
#' @param DF fit.final_space data frame
#' @param names.swp.ADSR names of swept ADSR parameters (alpha, delta, size, rayleigh, custom expressions), as vector
#' @param parameter_subsets parameter subsets passed from fit.final_space
#'
#' @return A frequency plot of fitted parameter values.
#'
#' @keywords internal
plot_freq_ADSR <- function(DF, names.swp.ADSR, parameter_subsets){

  . <- in.all.subset_param <- n <- swp.ADSR.value <- swp.ADSR.name <-
    label <- swp.center <- swp.2se <- swp.mean <- swp.50 <- swp.75 <-
    swp.25 <- swp.95 <- swp.05 <- freq.in <- swp.2sd <-
    NULL

  names.parameter_subsets <- names(parameter_subsets)

  ##### ___ i. qp freq calculation / parameter within CI & within parameter subsets ######
  if (!is.null(names.parameter_subsets) & any(names.parameter_subsets %in% names.swp.ADSR)){

    DF.swp.ADSR <- DF %>%
      dplyr::group_by( dplyr::across(names.swp.ADSR)) %>%
      dplyr::select(dplyr::all_of(names.swp.ADSR)) %>%
      as.data.frame() %>%
      reshape2::melt(id.vars = NULL,
                     variable.name = "swp.ADSR.name",
                     value.name = "swp.ADSR.value") %>%
      dplyr::group_by(swp.ADSR.name) %>%
      dplyr::summarise(swp.ADSR.value = unique(swp.ADSR.value), .groups = "drop_last") %>%
      as.data.frame()

    swp.ADSR.subset_range <- as.data.frame(parameter_subsets[names(parameter_subsets) %in% names.swp.ADSR])
    DF.swp.ADSR$in.parameter_subsets <- TRUE

    for (i in 1:ncol(swp.ADSR.subset_range)){
      swp.ADSR_loc <- names(swp.ADSR.subset_range)[i]
      DF.swp.ADSR[DF.swp.ADSR$swp.ADSR.name == swp.ADSR_loc &
                    DF.swp.ADSR$swp.ADSR.value < min(swp.ADSR.subset_range[,swp.ADSR_loc])
                  , "in.parameter_subsets"] <- FALSE
      DF.swp.ADSR[DF.swp.ADSR$swp.ADSR.name == swp.ADSR_loc &
                    DF.swp.ADSR$swp.ADSR.value > max(swp.ADSR.subset_range[,swp.ADSR_loc])
                  , "in.parameter_subsets"] <- FALSE
    }
  }

  # remove(swp.ADSR_loc)

  molten.DF.CI_fitted <- reshape2::melt(DF[DF$in.all_fit_boxes.obs.CI == TRUE &
                                             DF$in.all.subset_param == TRUE,
                                           c(names.swp.ADSR)],
                                        id.vars = NULL,
                                        variable.name = "swp.ADSR.name",
                                        value.name = "swp.ADSR.value")

  freq.molten.DF.CI_fitted <- molten.DF.CI_fitted %>%
    dplyr::group_by(swp.ADSR.name, swp.ADSR.value) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(freq = 100*n/sum(n)) %>%
    as.data.frame()

  molten.DF <- reshape2::melt(DF[, c(names.swp.ADSR, "in.all.subset_param")],
                              id.vars = "in.all.subset_param",
                              variable.name = "swp.ADSR.name",
                              value.name = "swp.ADSR.value")

  freq.molten.DF <- molten.DF %>%
    dplyr::group_by(swp.ADSR.name, swp.ADSR.value) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    dplyr::mutate(freq = 100*n/sum(n)) %>%
    as.data.frame()

  freq.molten <- dplyr::full_join(freq.molten.DF, freq.molten.DF.CI_fitted,
                                  by = c("swp.ADSR.name", "swp.ADSR.value"),
                                  suffix = c(".all", ".in"))

  freq.molten$freq.in.vs.all <- 100*freq.molten$n.in/freq.molten$n.all

  cropped_param_values <- molten.DF %>%
    dplyr::filter(swp.ADSR.name %in% names(parameter_subsets)) %>%
    dplyr::group_by(swp.ADSR.name, swp.ADSR.value, in.all.subset_param) %>%
    dplyr::count() %>% as.data.frame()

  cropped_param_values.TRUE <- cropped_param_values %>%
    dplyr::filter(in.all.subset_param == TRUE) %>%
    dplyr::select(swp.ADSR.name, swp.ADSR.value, in.all.subset_param)

  cropped_param_values <- cropped_param_values %>%
    dplyr::select(swp.ADSR.name, swp.ADSR.value) %>%
    dplyr::distinct()

  cropped_param_values <- dplyr::full_join(cropped_param_values, cropped_param_values.TRUE,
                                           by = c("swp.ADSR.name", "swp.ADSR.value") )

  cropped_param_values <- cropped_param_values %>% replace(is.na(.), FALSE)

  freq.molten <- freq.molten %>% replace(is.na(.), 0)
  freq.molten <- dplyr::full_join(freq.molten, cropped_param_values, by = c("swp.ADSR.name", "swp.ADSR.value"))
  freq.molten <- freq.molten %>% replace(is.na(.), TRUE)

  param.min_max <- freq.molten %>%
    dplyr::group_by(swp.ADSR.name) %>%
    dplyr::summarise(full_range.min = min(swp.ADSR.value),
              full_range.max = max(swp.ADSR.value), .groups = "drop_last") %>%
    as.data.frame()

  stats.molten.DF.CI_fitted <- molten.DF.CI_fitted %>%
    dplyr::group_by(swp.ADSR.name) %>%
    dplyr::summarise(n = dplyr::n(),
              swp.min = min(swp.ADSR.value, na.rm = T),
              swp.max = max(swp.ADSR.value, na.rm = T),
              swp.05 = stats::quantile(swp.ADSR.value, .05),
              swp.25 = stats::quantile(swp.ADSR.value, .25),
              swp.50 = stats::quantile(swp.ADSR.value, .5),
              swp.75 = stats::quantile(swp.ADSR.value, .75),
              swp.95 = stats::quantile(swp.ADSR.value, .95),
              swp.mean = mean(swp.ADSR.value, na.rm = T),
              swp.2sd = 2*stats::sd(swp.ADSR.value, na.rm = T),
              swp.2se = swp.2sd/sqrt(n) , .groups = "drop_last") %>%
    as.data.frame()

  center.swp.param <- molten.DF %>%
    dplyr::group_by(swp.ADSR.name) %>%
    dplyr::summarise(swp.center = min(swp.ADSR.value, na.rm = T)+.5*(max(swp.ADSR.value, na.rm = T)-min(swp.ADSR.value, na.rm = T)),
              .groups = "drop_last") %>%
    as.data.frame()

  stats.molten.DF.CI_fitted <- dplyr::full_join(stats.molten.DF.CI_fitted, center.swp.param, by = "swp.ADSR.name")

  stats.molten.DF.CI_fitted <- dplyr::full_join(stats.molten.DF.CI_fitted, param.min_max, by = "swp.ADSR.name")

  stats.molten.DF.CI_fitted$mag_order <-
    round(-log10(stats.molten.DF.CI_fitted$full_range.max-stats.molten.DF.CI_fitted$full_range.min))

  stats.molten.DF.CI_fitted[stats.molten.DF.CI_fitted$mag_order < 0, "mag_order"] <- 0
  stats.molten.DF.CI_fitted[stats.molten.DF.CI_fitted$mag_order == Inf, "mag_order"] <- 0

  stats.molten.DF.CI_fitted$label <- paste0(dec_n(stats.molten.DF.CI_fitted$swp.mean,
                                                  stats.molten.DF.CI_fitted$mag_order+2),
                                            " \u00B1 ",
                                            dec_n(.5*stats.molten.DF.CI_fitted$swp.2sd,
                                                  stats.molten.DF.CI_fitted$mag_order+2),
                                            " (",
                                            dec_n(stats.molten.DF.CI_fitted$swp.2se,
                                                  stats.molten.DF.CI_fitted$mag_order+2),
                                            ")"
  )

  max.freq.molten <- freq.molten %>%
    dplyr::group_by(swp.ADSR.name) %>%
    dplyr::filter(freq.in == max(freq.in)) %>%
    dplyr::arrange (swp.ADSR.name, swp.ADSR.value, freq.in) %>%
    as.data.frame()

  stats.molten.DF.CI_fitted.max <-
    dplyr::full_join(stats.molten.DF.CI_fitted,
                     max.freq.molten[, c("swp.ADSR.name", "swp.ADSR.value", "freq.in")],
                     by = "swp.ADSR.name")

  stats.molten.DF.CI_fitted.max$freq_max.label <-
    paste0(dec_n(stats.molten.DF.CI_fitted.max$swp.ADSR.value,
                 stats.molten.DF.CI_fitted.max$mag_order+2))

  remove(molten.DF, center.swp.param, cropped_param_values,
         cropped_param_values.TRUE, freq.molten.DF.CI_fitted, freq.molten.DF,
         max.freq.molten, param.min_max)
  quiet(gc())


  ##### ___ ii. plot qp CI fit frequencies ####
  boxplot_y <- -5
  mean_y <- -11
  annotation_y <- -20
  lower_border_y <- -20

  plot.freq_ADSR <- ggplot2::ggplot(freq.molten,
                                    ggplot2::aes(x = swp.ADSR.value,
                                                 y = freq.in,
                                                 color = swp.ADSR.name,
                                                 fill = swp.ADSR.name))+
    ggplot2::geom_hline(linetype = 1, size = .25,
                        yintercept = 0)+
    ggplot2::geom_rect(ggplot2::aes(xmin = -Inf,
                                    xmax = Inf,
                                    ymin = -Inf,
                                    ymax = 0),
                       fill = "gray95",
                       color =NA)+
    # ggplot2::geom_point(size = 1, alpha = 1)+
    # ggplot2::geom_area(data =  freq.molten[freq.molten$in.all.subset_param == TRUE, ],
    #           alpha = .25)+
    ggplot2::geom_linerange(ggplot2::aes(ymin = 0,
                                         ymax = freq.in),
                            size = 2,
                            linetype = 1)+
    ggplot2::theme_linedraw()+
    ggplot2::theme(legend.position = "None",
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.caption = ggplot2::element_text(hjust = 0),
          strip.text = ggplot2::element_text(face="bold"),
          panel.grid = ggplot2::element_blank()
          # plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          # plot.caption.position =  "plot"
    )+
    ggplot2::facet_wrap(. ~ swp.ADSR.name,
               scales = "free_x",
               ncol = round(1.1*length(levels(freq.molten$swp.ADSR.name))/2)
    )+
    ggplot2::labs(title = "Frequency distributions of CI fitted quantitative parameter ",
         y = "frequence of runs within observed CIs of all boxes (\u0025)",
         x = "sweeped parameter values",
         caption = paste(#"Proportion of fits:", dec_n(fraction_of_fitting_sim, 1), "\u0025",
           "Upper part: Frequence curves with parameter value of maximum frequence.",
           " \n", "Lower part: (up) Horizontal distribution boxplot (5, 25, 50, 75, 95\u0025) with median (IQR); ",
           "(mid) Horizontal mean with 1sd and 2se error bar; ",
           "\n", "(down) Value of mean \u00B1 1sd (2se).",
           "\n", "Crosses mark values standing outside a parameter subset.",
           sep = "")
    )+
    ggplot2::geom_errorbarh(data = stats.molten.DF.CI_fitted, # boxplot / 2SD / 2SE
                   inherit.aes = F,
                   ggplot2::aes(y = boxplot_y,
                       xmin = swp.05,
                       xmax = swp.95,
                       color = swp.ADSR.name),
                   size = 1,
                   alpha = 1,
                   height = 0)+
    ggplot2::geom_errorbarh(data = stats.molten.DF.CI_fitted,
                   inherit.aes = F,
                   ggplot2::aes(y = boxplot_y,
                       xmin = swp.25,
                       xmax = swp.75,
                       color = swp.ADSR.name),
                   size = 3,
                   alpha = 1,
                   height = 0)+
    ggplot2::geom_point(data = stats.molten.DF.CI_fitted,
               ggplot2::aes(y = boxplot_y,
                   x = swp.50),
               color = "black",
               size = 2,
               shape = 24)+
    ggplot2::geom_errorbarh(data = stats.molten.DF.CI_fitted, # mean / 2SD / 2SE
                   inherit.aes = F,
                   ggplot2::aes(y = mean_y,
                       xmin = swp.mean-.5*swp.2sd,
                       xmax = swp.mean+.5*swp.2sd,
                       color = swp.ADSR.name),
                   size = 1,
                   alpha = 1,
                   height = 0)+
    ggplot2::geom_errorbarh(data = stats.molten.DF.CI_fitted,
                   inherit.aes = F,
                   ggplot2::aes(y = mean_y,
                       xmin = swp.mean-swp.2se,
                       xmax = swp.mean+swp.2se,
                       color = swp.ADSR.name),
                   size = 3,
                   alpha = 1,
                   height = 0)+
    ggplot2::geom_point(data = stats.molten.DF.CI_fitted,
               ggplot2::aes(y = mean_y,
                   x = swp.mean),
               color = "black",
               size = 2,
               shape = 25)+
    ggplot2::geom_text(data = stats.molten.DF.CI_fitted,
              ggplot2::aes(x = swp.center,
                  y = annotation_y,
                  hjust = 0.5,
                  vjust = 0,
                  label = label),
              size = 3)+
    ggplot2::scale_y_continuous(limits=c(lower_border_y, 100), breaks=seq(0, 100, by = 10))+
    ggplot2::coord_cartesian(ylim = c(lower_border_y, 1.0*max(freq.molten$freq.in)))
  # ggrepel::geom_label_repel(data = stats.molten.DF.CI_fitted.max,
  #                           fill = "white",
  #                           nudge_y = 15,
  #                           vjust = 0,
  #                           direction = c("y"),
  #                           # hjust = 0,
  #                           size = 2.5,
  #                           angle = 45,
  #                           segment.linetype = 3,
  #                           label.size = NA,
  #                           ggplot2::aes(x = swp.ADSR.value,
  #                               y = freq.in,
  #                               label = freq_max.label))

  if (exists("DF.swp.ADSR")){
    plot.freq_ADSR <- plot.freq_ADSR +
      ggplot2::geom_point(inherit.aes = F,
                 data = DF.swp.ADSR[DF.swp.ADSR$in.parameter_subsets == FALSE, ],
                 ggplot2::aes(x = swp.ADSR.value,
                     y = 0),
                 shape = 4,
                 size = 2,
                 color = "black" )
  }

  freq.molten.export <-
    freq.molten[,c("swp.ADSR.name", "in.all.subset_param", "swp.ADSR.value", "freq.in", "n.all", "n.in")]

  names(freq.molten.export) <- c("Quantitative parameter", "Manually subset", "Parameter value",
                                 "Frequence in CI-fit (\u0025)", "n all simulations", "n CI-fitted simulations")

  stats.report <- stats.molten.DF.CI_fitted

  return(list(plot = plot.freq_ADSR,
              stats = stats.molten.DF.CI_fitted,
              freqs = stats.report))
}

#  #_________________________________________________________________________80char
#' plot_freq_flux
#'
#' @description  Plot frequency profiles by fitted flux lists
#'
#' @param DF fit.final_space data frame
#' @param parameter_subsets  fitted flux lists subsets passed from fit.final_space
#'
#' @return A frequency plot of fitted parameter values.
#'
#' @keywords internal
plot_freq_flux <- function(DF, parameter_subsets){

  in.subset.swp.flux_list_name <-
    freq.in <- . <- n <- in.all_fit_boxes.obs.CI <-
    in.all.subset_param <- swp.flux_list_name <-
    NULL

  names.parameter_subsets <- names(parameter_subsets)

  DF.flux_counts.all <- DF %>%
    dplyr::group_by(swp.flux_list_name) %>%
    dplyr::summarise(n.all = dplyr::n(), .groups = 'drop_last') %>%
    as.data.frame()

  DF.flux_counts.in_out <- DF %>%
    dplyr::filter(in.all.subset_param == TRUE) %>%
    dplyr::group_by(swp.flux_list_name, in.all_fit_boxes.obs.CI) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    as.data.frame()

  DF.flux_freq.in <- DF.flux_counts.in_out[DF.flux_counts.in_out$in.all_fit_boxes.obs.CI == TRUE, ] %>%
    dplyr::mutate(freq.in = 100*n/sum(n)) %>%
    as.data.frame()

  DF.flux_freq.out <- DF.flux_counts.in_out[DF.flux_counts.in_out$in.all_fit_boxes.obs.CI == FALSE, ] %>%
    dplyr::mutate(freq.out = 100*n/sum(n)) %>%
    as.data.frame()

  DF.flux_freq <- dplyr::full_join(DF.flux_freq.in, DF.flux_freq.out,
                                   by = "swp.flux_list_name",
                                   suffix = c(".in", ".out"))

  DF.flux_freq <- dplyr::full_join(DF.flux_counts.all, DF.flux_freq,
                                   by = "swp.flux_list_name",
                                   suffix = c(".all", ""))

  DF.flux_freq <- DF.flux_freq %>% replace(is.na(.), 0)

  plot.freq_flux <- ggplot2::ggplot(DF.flux_freq,
                           ggplot2::aes(x = swp.flux_list_name,
                               y = freq.in,
                               label= swp.flux_list_name))+
    ggplot2::geom_col(size = 1, alpha =.25, fill = "red")+
    ggplot2::theme_linedraw()+
    ggplot2::theme(legend.position = "None",
          # axis.title.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot",
          plot.caption = ggplot2::element_text(hjust = 0)
    )+
    ggplot2::geom_text(ggplot2::aes(x = swp.flux_list_name, y = 0), hjust = 0, size = 2)+
    ggplot2::labs(title = "Frequency distribution of CI fitted flux lists",
         y = "frequence of fit within all CIs (\u0025)",
         x = "flux list name")+
    ggplot2::coord_flip()


  if (!is.null(names.parameter_subsets) & "swp.flux_list_name" %in% names.parameter_subsets){
    DF.flux_counts.parameter_subsets <- DF %>%
      dplyr::group_by(swp.flux_list_name, in.subset.swp.flux_list_name) %>%
      dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
      as.data.frame()
    if (nrow(DF.flux_counts.parameter_subsets[DF.flux_counts.parameter_subsets$in.subset.swp.flux_list_name == FALSE, ]) > 0){
      plot.freq_flux <- plot.freq_flux +
        ggplot2::geom_point(inherit.aes = F,
                   data = DF.flux_counts.parameter_subsets[DF.flux_counts.parameter_subsets$in.subset.swp.flux_list_name == FALSE, ],
                   ggplot2::aes(x = swp.flux_list_name,
                       y = -2), shape = 4,
                   size = 2, color = "black") +
        ggplot2::labs(caption =
               "Crosses mark values standing outside a parameter subset.")
    }
  }

  DF.flux_freq.export <- DF.flux_freq[, c("swp.flux_list_name", "freq.in", "n.all", "n.in")]
  names(DF.flux_freq.export) <- c("Flux list name", "Frequence in CI-fit (\u0025)", "n all simulations", "n CI-fitted simulations" )

  return(list(plot = plot.freq_flux,
              freqs = DF.flux_freq.export))
}

#  #_________________________________________________________________________80char
#' plot_freq_coeff
#'
#' @description  Plot frequency profiles by fitted coeff lists
#'
#' @param DF fit.final_space data frame
#' @param parameter_subsets  fitted coeff lists subsets passed from fit.final_space
#'
#' @return A frequency plot of fitted parameter values.
#'
#' @keywords internal
plot_freq_coeff <- function(DF, parameter_subsets){

  in.subset.swp.coeff_list_name <-
    freq.in <- . <- n <- in.all_fit_boxes.obs.CI <-
    in.all.subset_param <- swp.coeff_list_name <- NULL

  names.parameter_subsets <- names(parameter_subsets)

  DF.coeff_counts.all <- DF %>%
    dplyr::group_by(swp.coeff_list_name) %>%
    dplyr::summarise(n.all = dplyr::n(), .groups = 'drop_last') %>%
    as.data.frame()

  DF.coeff_counts.in_out <- DF %>%
    dplyr::filter(in.all.subset_param == TRUE) %>%
    dplyr::group_by(swp.coeff_list_name, in.all_fit_boxes.obs.CI) %>%
    dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
    as.data.frame()

  DF.coeff_freq.in <- DF.coeff_counts.in_out[DF.coeff_counts.in_out$in.all_fit_boxes.obs.CI == TRUE, ] %>%
    dplyr::mutate(freq.in = 100*n/sum(n)) %>%
    as.data.frame()

  DF.coeff_freq.out <- DF.coeff_counts.in_out[DF.coeff_counts.in_out$in.all_fit_boxes.obs.CI == FALSE, ] %>%
    dplyr::mutate(freq.out = 100*n/sum(n)) %>%
    as.data.frame()

  DF.coeff_freq <- dplyr::full_join(DF.coeff_freq.in, DF.coeff_freq.out,
                                    by = "swp.coeff_list_name",
                                    suffix = c(".in", ".out"))

  DF.coeff_freq <- dplyr::full_join(DF.coeff_counts.all, DF.coeff_freq,
                                    by = "swp.coeff_list_name",
                                    suffix = c(".all", ""))

  DF.coeff_freq <- DF.coeff_freq %>% replace(is.na(.), 0)

  plot.freq_coeff <- ggplot2::ggplot(DF.coeff_freq,
                            ggplot2::aes(x = swp.coeff_list_name,
                                y = freq.in,
                                label= swp.coeff_list_name))+
    ggplot2::geom_col(size = 1, alpha =.25, fill = "red")+
    ggplot2::theme_linedraw()+
    ggplot2::theme(legend.position = "None",
          # axis.title.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot"
    )+
    ggplot2::geom_text(ggplot2::aes(x = swp.coeff_list_name, y = 0), hjust = 0)+
    ggplot2::labs(title = "Frequency distribution of CI fitted coeff. lists",
         y = "frequence of fit within all CIs (\u0025)",
         x = "coeff list name")+
    ggplot2::coord_flip()

  if (!is.null(names.parameter_subsets) & "swp.coeff_list_name" %in% names.parameter_subsets){
    DF.coeff_counts.parameter_subsets <- DF %>%
      dplyr::group_by(swp.coeff_list_name, in.subset.swp.coeff_list_name) %>%
      dplyr::summarise(n = dplyr::n(), .groups = 'drop_last') %>%
      as.data.frame()
    if (nrow(DF.coeff_counts.parameter_subsets[DF.coeff_counts.parameter_subsets$in.subset.swp.coeff_list_name == FALSE, ]) > 0){
      plot.freq_coeff <- plot.freq_coeff +
        ggplot2::geom_point(inherit.aes = F,
                   data = DF.coeff_counts.parameter_subsets[DF.coeff_counts.parameter_subsets$in.subset.swp.coeff_list_name == FALSE, ],
                   ggplot2::aes(x = swp.coeff_list_name,
                       y = -2), shape = 4,
                   size = 2, color = "black") +
        ggplot2::labs(caption =
               "Crosses mark values standing outside a parameter subset.")
    }
  }

  DF.coeff_freq.export <- DF.coeff_freq[, c("swp.coeff_list_name", "freq.in", "n.all", "n.in")]
  names(DF.coeff_freq.export) <- c("Coeff list name", "Frequence in CI-fit (\u0025)", "n all simulations", "n CI-fitted simulations" )

  return(list(plot = plot.freq_coeff,
              freqs = DF.coeff_freq))
}

#  #_________________________________________________________________________80char
#' plot_sim_obs
#'
#' @description  Plot simulations against observations
#'
#' @param DF fit.final_space data frame
#' @param bx.fit list of boxes fitted and unfitted groups and subgroups passed from fit.final_space
#' @param fit.counts counts of simulations by groups passed from fit.final_space
#'
#' @return A plot of fitted simulated delta values against observations
#'
#' @keywords internal
plot_sim_obs <- function(DF, bx.fit, fit.counts){

  X.CI <- Y.75 <- Y.25 <-
    Y.50 <- Y.max <- Y.min <-
    ymax <- ymin <- x <-
    Y <- X <- sim.delta <- obs.CI <- obs.delta <-
    in.boxes_to_fit <- BOX_ID <- n <-
    SERIES_RUN_ID <- in.all.subset_param <- in.all_fit_boxes.obs.CI <- NULL

  run_fit_proportions <- DF %>%
    dplyr::group_by(in.all_fit_boxes.obs.CI, in.all.subset_param, SERIES_RUN_ID) %>%
    dplyr::count() %>%
    as.data.frame() %>%
    dplyr::count(in.all_fit_boxes.obs.CI, in.all.subset_param) %>%
    dplyr::summarise(in.all_fit_boxes.obs.CI = in.all_fit_boxes.obs.CI,
              in.all.subset_param = in.all.subset_param,
              n = n,
              freq = 100*n/sum(n), .groups = "drop_last")

  frac_of_runs_in_CI_parameter_subsets <-
    run_fit_proportions[run_fit_proportions$in.all_fit_boxes.obs.CI == TRUE &
                          run_fit_proportions$in.all.subset_param == TRUE, "freq"]
  frac_of_runs_in_CI <- sum(run_fit_proportions[run_fit_proportions$in.all_fit_boxes.obs.CI == TRUE, "freq"])
  tot_runs <- sum(run_fit_proportions[ , "n"])
  remove(run_fit_proportions)

  sim_obs.mean <- DF[DF$in.all_fit_boxes.obs.CI == "TRUE" &
                       DF$in.all.subset_param == TRUE, ] %>%
    dplyr::group_by(BOX_ID, in.boxes_to_fit) %>%
    dplyr::summarise(X = mean(obs.delta),
              X.CI = mean(obs.CI),
              Y = mean(sim.delta),
              Y.05 = stats::quantile(sim.delta, .05),
              Y.25 = stats::quantile(sim.delta, .25),
              Y.50 = stats::quantile(sim.delta, .50),
              Y.75 = stats::quantile(sim.delta, .75),
              Y.95 = stats::quantile(sim.delta, .95),
              Y.min = min(sim.delta, na.rm = T),
              Y.max = max(sim.delta, na.rm = T), .groups = "drop_last") %>%
    as.data.frame()

  sim_obs.mean$X.provided <- FALSE
  sim_obs.mean[!is.nan(sim_obs.mean$X), "X.provided"] <- TRUE

  nudge_y.range <- abs(max(DF$sim.delta)-min(DF$sim.delta))
  nudge_x.range <- abs(max(DF$obs.delta, na.rm = T)-min(DF$obs.delta, na.rm = T))

  # box_lists <- sim_obs.mean %>%
  #   dplyr::group_by(in.boxes_to_fit, X.provided) %>%
  #   dplyr::summarise(boxes = paste0(sort(as.character(BOX_ID)), collapse = ", "), .groups = "drop_last") %>%
  #   as.data.frame()
  # box_lists$label <- NaN
  # box_lists[box_lists$in.boxes_to_fit == TRUE & box_lists$X.provided == TRUE, "label"] <- "Targetted: "
  # box_lists[box_lists$in.boxes_to_fit == FALSE & box_lists$X.provided == TRUE, "label"] <- "Excluded: "
  # box_lists[box_lists$X.provided == FALSE, "label"] <- "Missing obs.: "
  # box_lists_caption <- paste0(paste(paste(box_lists$label, box_lists$boxes, sep = ""),
  #                                   collapse = " \n"), "\n", report.CIfit)

  # box_lists_caption <-
  if (all(bx.fit$targetted == bx.fit$targetted_initial)){
    list_box_lists_caption <- paste0("Targets : ", paste(bx.fit$targetted, collapse = " - "))
  } else {
    list_box_lists_caption <- paste0("Initial targets : ", paste(bx.fit$targetted_initial, collapse = " - "))

    if (length(bx.fit$targetted_no_obs) > 0) {
      list_box_lists_caption <- c(list_box_lists_caption,
                                  paste0("Missing obs : ", paste(bx.fit$targetted_no_obs, collapse = " - ")))
    }

    if (length(bx.fit$targetted) > 0) {
      list_box_lists_caption <- c(list_box_lists_caption,
                                  paste0("Targets : ", paste(bx.fit$targetted, collapse = " - ")))
    }

  }

  if (length(bx.fit$not_targetted) > 0) {
    list_box_lists_caption <- c(list_box_lists_caption,
                                paste0("Not targets : ", paste(bx.fit$not_targetted, collapse = " - ")))
  }

  if (length(bx.fit$CI_fit.successful) > 0) {
    list_box_lists_caption <- c(list_box_lists_caption,
                                paste0("Successful fit : ", paste(bx.fit$CI_fit.successful, collapse = " - ")))
  }

  if (length(bx.fit$CI_fit.failed) > 0) {
    list_box_lists_caption <- c(list_box_lists_caption,
                                paste0("No fit : ", paste(bx.fit$CI_fit.failed, collapse = " - ")))
  }

  box_lists_caption <- paste(list_box_lists_caption, collapse = "\n")

  # lm reports
  lm_report <- corr_stats(data_lm = DF[DF$in.all_fit_boxes.obs.CI == TRUE &
                                         DF$in.boxes_to_fit == TRUE &
                                         DF$in.all.subset_param == TRUE, ],
                          X = "obs.delta",
                          Y = "sim.delta") %>% as.list()

  lm_report.text <- paste0("LM (2se): ", " y = ",
                           dec_n(lm_report$pear.slope, 3), " ( \u00B1 ", dec_n(lm_report$pear.slope.2se, 3),
                           " ) * x + ",
                           dec_n(lm_report$pear.yint, 3), " ( \u00B1 ", dec_n(lm_report$pear.yint.2se, 3),
                           " ) ", "\n",
                           "Pear. R2 = ", dec_n(lm_report$pear.R2, 3), " ", lm_report$pear.signif, "; ",
                           "Spear. rho = ", dec_n(lm_report$spear.Rho, 3), " ", lm_report$spear.signif, "\n",
                           "n = ", dec_n(lm_report$n, 0)
  )

  lm_report.text.x = min(sim_obs.mean$X-sim_obs.mean$X.CI, na.rm=T)
  lm_report.text.y = max(sim_obs.mean$Y.max)

  linerange <- clear_subset(DF[DF$BOX_ID %in% bx.fit$observed, ]) %>%
    dplyr::group_by(BOX_ID) %>%
    dplyr::summarise(x = unique(obs.delta),
              ymin = min(sim.delta, na.rm = T),
              ymax = max(sim.delta, na.rm = T),
              .groups = "drop_last") %>%
    as.data.frame()

  plot.sim_obs <-
    ggplot2::ggplot(data = sim_obs.mean,
           ggplot2::aes(x = X,
               y = Y
           ))+
    ggplot2::geom_abline(slope = 1, linetype = 2)+
    ggplot2::geom_abline(intercept = lm_report$pear.yint,
                slope = lm_report$pear.slope,
                linetype = 1,
                color = "blue")+
    ggplot2::scale_shape_manual(values = c(23, 21))+
    ggplot2::geom_linerange(data = linerange,
                   inherit.aes = F,
                   ggplot2::aes(x = x,
                       y = ymin,
                       ymin = ymin,
                       ymax = ymax,
                       color = BOX_ID),
                   linetype = 2,
                   size = 0.25, alpha = 0.5) +
    # ggplot2::geom_jitter(data = DF[DF$in.all_fit_boxes.obs.CI == "TRUE" &
    #                         DF$in.all.subset_param == TRUE &
    #                         DF$BOX_ID %in% bx.fit$observed, ], shape = 43, size = 0.2,
    #             ggplot2::aes(x = obs.delta, y = sim.delta, color = BOX_ID), na.rm = T,
    #             width =  (max(sim_obs.mean$X + sim_obs.mean$X.CI, na.rm = T) - min(sim_obs.mean$X - sim_obs.mean$X.CI, na.rm = T))/100 )+
    # ggplot2::scale_alpha_continuous(range = c(0.1,.5))+
    # stat_bin_2d(inherit.aes = F,
    #             data = clear_subset(DF[DF$BOX_ID %in% bx.fit$observed, ]),
    #             ggplot2::aes(x = obs.delta, y = sim.delta, alpha = ..ndensity.., fill = BOX_ID),
    #             bins = 200, na.rm = T)+
    ggplot2::geom_errorbar(data = clear_subset(sim_obs.mean[sim_obs.mean$BOX_ID %in% bx.fit$observed, ]),
                  inherit.aes = F,
                  ggplot2::aes(x = X, ymin = Y.min, ymax = Y.max, color = BOX_ID),
                  size = 1,
                  width = 0)+

    ggplot2::geom_errorbar(data = clear_subset(sim_obs.mean[sim_obs.mean$BOX_ID %in% bx.fit$observed, ]),
                  inherit.aes = F,
                  ggplot2::aes(x = X, y = Y.50, ymin = Y.25, ymax = Y.75, color = BOX_ID),
                  size = 3,
                  width = 0)+

    ggplot2::geom_errorbarh(data = clear_subset(sim_obs.mean[sim_obs.mean$BOX_ID %in% bx.fit$observed, ]),
                   inherit.aes = F,
                   ggplot2::aes( y = Y.50, xmin = X - X.CI, xmax = X + X.CI, color = BOX_ID),
                   size = 1, na.rm = T,
                   height = 0)+
    ggplot2::geom_point(data = clear_subset(sim_obs.mean[sim_obs.mean$BOX_ID %in% bx.fit$observed, ]),
               inherit.aes = F,
               ggplot2::aes(y = Y.50, x = X, fill = BOX_ID, shape = as.factor(in.boxes_to_fit) ),
               color = "black",
               size = 3, na.rm = T )+
    ggplot2::theme_linedraw()+
    ggplot2::labs(title = "Simulation vs. Observation",
         subtitle = box_lists_caption,
         caption = paste(fit.counts$sims.all, " simulations - ",
                         fit.counts$sims.in.CIfit.param_subsets, " fitted simulations -  ",
                         fit.counts$sim_delta.fitted_boxes, " fitted delta values", "\n",
                         "Symbols show the medians of fitted boxes (circles) and non fitted boxes (diamonds)", "\n",
                         "Horizontal bars: confidence interval on observations.", "\n",
                         "Dashed light vertical bars: full extant of simulations.","\n",
                         "Solid thin and thick vertical bars: Full and interquartile (25-75\u0025) range of predicted compositions.","\n",
                         "Lines: black dashed line is identity; blue solid line is linear regression of all CI-fitted sim-obs pairs.",
                         sep = ""),
         x = expression(paste("Observed ", delta^{"i/j"},"X" )),
         y = expression(paste("Simulated ", delta^{"i/j"},"X" )))+
    ggplot2::theme(legend.position = "None",
          plot.caption = ggplot2::element_text(hjust = 0),
          panel.grid = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_text(size = 7)
    ) +
    ggplot2::annotate(geom = "text", x = lm_report.text.x, y = lm_report.text.y,
             label = lm_report.text, hjust = 0, vjust = 1)+
    ggrepel::geom_label_repel(data = clear_subset(sim_obs.mean[sim_obs.mean$BOX_ID %in% bx.fit$observed, ]),
                              # inherit.aes = F,
                              ggplot2::aes(x = X - X.CI, y = Y.50, label = BOX_ID, color = BOX_ID),
                              nudge_y = 0,
                              fill = "white",
                              segment.color = "gray75",
                              nudge_x = -0.09*nudge_x.range)+
    ggplot2::scale_x_continuous(labels = dec_2)+
    ggplot2::scale_y_continuous(labels = dec_2)+
    ggplot2::coord_equal(ylim = c(min(sim_obs.mean$Y.min), max(sim_obs.mean$Y.max)))

  return(plot.sim_obs)
}

#  #_________________________________________________________________________80char
#' plot_sim_distrib
#'
#' @description  Plot distributions of all and fitted simulations
#'
#' @param DF fit.final_space data frame
#' @param fit.counts counts of simulations by groups passed from fit.final_space
#' @param observations data frame of observations
#' @param bx.fit list of boxes fitted and unfitted groups and subgroups passed from fit.final_space
#'
#' @return A plot of distributions of all and fitted simulations
#'
#' @keywords internal
plot_sim_distrib <- function(DF, fit.counts, observations, bx.fit){
  obs.CI <- obs.delta <- sim.delta.mean <- sim.delta <- BOX_ID <- NULL

  # box_lists_caption <-
  if (all(bx.fit$targetted == bx.fit$targetted_initial)){
    list_box_lists_caption <- paste0("Targets : ", paste(bx.fit$targetted, collapse = " - "))
  } else {
    list_box_lists_caption <- paste0("Initial targets : ", paste(bx.fit$targetted_initial, collapse = " - "))

    if (length(bx.fit$targetted_no_obs) > 0) {
      list_box_lists_caption <- c(list_box_lists_caption,
                                  paste0("Missing obs : ", paste(bx.fit$targetted_no_obs, collapse = " - ")))
    }

    if (length(bx.fit$targetted) > 0) {
      list_box_lists_caption <- c(list_box_lists_caption,
                                  paste0("Targets : ", paste(bx.fit$targetted, collapse = " - ")))
    }

  }

  if (length(bx.fit$not_targetted) > 0) {
    list_box_lists_caption <- c(list_box_lists_caption,
                                paste0("Not targets : ", paste(bx.fit$not_targetted, collapse = " - ")))
  }

  if (length(bx.fit$CI_fit.successful) > 0) {
    list_box_lists_caption <- c(list_box_lists_caption,
                                paste0("Successful fit : ", paste(bx.fit$CI_fit.successful, collapse = " - ")))
  }

  if (length(bx.fit$CI_fit.failed) > 0) {
    list_box_lists_caption <- c(list_box_lists_caption,
                                paste0("No fit : ", paste(bx.fit$CI_fit.failed, collapse = " - ")))
  }

  box_lists_caption <- paste(list_box_lists_caption, collapse = "\n")

  BOX_ID_facet_labels <- DF[DF$in.all_fit_boxes.obs.CI == "TRUE" & DF$in.all.subset_param == TRUE, ] %>%
    dplyr::group_by(BOX_ID) %>%
    dplyr::summarise(sim.delta.mean = mean(sim.delta)) %>%
    dplyr::arrange(sim.delta.mean) %>%
    as.data.frame() %>%
    dplyr::select(BOX_ID) %>%
    as.vector()

  BOX_ID_facet_labels <- BOX_ID_facet_labels$BOX_ID %>%
    as.character()

  fit.limits <- DF[DF$in.all_fit_boxes.obs.CI == "TRUE" & DF$in.all.subset_param == TRUE, ] %>%
    dplyr::summarise(sim.delta.min = min(sim.delta),
              sim.delta.max = max(sim.delta))

  all_sims.limits <- DF %>%
    dplyr::summarise(sim.delta.min = min(sim.delta),
              sim.delta.max = max(sim.delta))

  plot.sim_distrib <- ggplot2::ggplot(DF,
                             ggplot2::aes(x = stats::reorder(BOX_ID, obs.delta),
                                 y = sim.delta,
                                 groups = BOX_ID,
                                 # y = ..ndensity..,
                                 # y = ..prop..,
                                 # fill = BOX_ID
                                 # height = stat(density)
                             )) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "gray75", size = 0.5)+
    ggplot2::theme_linedraw()+
    ggplot2::geom_violin(alpha = 1,
                fill = "gray90",
                draw_quantiles = c(0.5),
                scale = "width",
                color = "black",
                size = 0.25,
                trim = T,
                adjust = .5) +
    # ggplot2::geom_jitter(data = DF[DF$in.all_fit_boxes.obs.CI == "TRUE" & DF$in.all.subset_param == TRUE, ],
    #             size = .01, color = "blue", alpha = .25,
    #             shape = 46, width = 0.1) +
    ggplot2::geom_violin(data = DF[DF$in.all_fit_boxes.obs.CI == "TRUE" & DF$in.all.subset_param == TRUE, ],
                # ggplot2::aes(fill = BOX_ID),
                fill = "blue",
                # alpha = 0.5,
                draw_quantiles = c(0.5),
                scale = "width",
                size = .25,
                color = "blue",
                alpha = 0.15,
                trim = T, adjust = .5)+
    ggplot2::geom_errorbar(data = observations, na.rm = T,
                  ggplot2::aes(x = BOX_ID, y = obs.delta, ymin = obs.delta - obs.CI, ymax = obs.delta + obs.CI),
                  color = "red", width = 0, size = .5 )+
    ggplot2::geom_point(data = observations, na.rm = T,
               ggplot2::aes(x = BOX_ID, y = obs.delta), size = 2,
               shape = 23,
               color = "black",
               fill = "red")+
    ggplot2::labs(title = "Distributions of simulated isotope compositions",
         subtitle = box_lists_caption,
         caption = paste(fit.counts$sims.all, " simulations - ",
                         fit.counts$sims.in.CIfit.param_subsets, " fitted simulations -  ", "\n",
                         "Gray shaded violins show the density distributions of all simulated compositions.", "\n",
                         "Blue shaded violins show the density distributions of all CI-fitted predicted compositions.", "\n",
                         "Medians are shown as horizontal bars in violin distributions.", "\n",
                         "Jittered points in background correspond to all simulated compositions predicted by CI-fitting.", "\n",
                         "Red diamonds and error bars show the average and confidence intervals of observed compositions.",
                         sep = ""),
         x = expression(paste("Observed ", delta^{"i/j"},"X" )))+
    ggplot2::scale_y_continuous(labels = dec_2)+
    ggplot2::theme(legend.position = "None",
          axis.title.x = ggplot2::element_blank(),
          plot.caption = ggplot2::element_text(hjust = 0),
          panel.grid = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_text(size = 7) )+
    ggplot2::coord_fixed(ratio = 2)

  return(plot.sim_distrib)
}

#  #_________________________________________________________________________80char
#' plot_LS_surfaces_ADSR
#'
#' @description  Plot surfaces of summed squared residuals.
#'
#' @param DF fit.final_space data frame
#' @param names.swp.ADSR names of swept ADSR parameters (alpha, delta, size, rayleigh, custom expressions), as vector
#'
#' @return A plot of surfaces of summed squared residuals for each ADSR parameter.
#'
#' @keywords internal
plot_LS_surfaces_ADSR <- function(DF, names.swp.ADSR){

  swp.ADSR.name <-
    swp.ADSR.value <-
    SSR <-
    quantile <-
    Y.0 <-
    Y.20 <-
    Y.40 <-
    Y.60 <-
    Y.80 <-
    Y.100 <-
    Y.min <-
    Y.max <-
    Y.1 <-
    Y.2 <-
    Y.3 <-
    Y.4 <-
    Y.5 <-
    Y.6 <-
    Y.7 <-
    Y.8 <-
    Y.9 <-
    Y.10 <-
    Y.30 <-
    Y.50 <-
    Y.70 <-
    Y.90 <- NULL

  #### ___ i. with CI fit & parameter_subsets ####
  DF.melt.CI <- reshape2::melt(DF[DF$in.all.subset_param == TRUE & DF$in.all_fit_boxes.obs.CI == TRUE, ],
                               id.vars = names(DF)[!(names(DF) %in% names.swp.ADSR)],
                               variable.name = "swp.ADSR.name",
                               value.name = "swp.ADSR.value")

  DF.melt.CI.envelope <- DF.melt.CI %>%
    dplyr::group_by(swp.ADSR.name,
             swp.ADSR.value) %>%
    dplyr::summarise(Y.min = min(SSR),
              Y.max = max(SSR),
              Y.0 = stats::quantile(SSR, 0),
              Y.10 = stats::quantile(SSR, 0.1),
              Y.20 = stats::quantile(SSR, 0.20),
              Y.30 = stats::quantile(SSR, 0.30),
              Y.40 = stats::quantile(SSR, 0.40),
              Y.50 = stats::quantile(SSR, 0.50),
              Y.60 = stats::quantile(SSR, 0.60),
              Y.70 = stats::quantile(SSR, 0.70),
              Y.80 = stats::quantile(SSR, 0.80),
              Y.90 = stats::quantile(SSR, 0.90),
              Y.100 = stats::quantile(SSR, 1),
              .groups = 'drop_last') %>%
    as.data.frame()

  lt_loc <- 1
  size_loc <- 0.25

  ribbon.y.min <- .75*10**((log10(min(DF.melt.CI$SSR))))

  plot.surface_qp_CI <- ggplot2::ggplot(DF.melt.CI.envelope, mapping = ggplot2::aes(x = swp.ADSR.value, y = Y.0))+
    ggplot2::geom_point(DF.melt.CI, mapping = ggplot2::aes(x = swp.ADSR.value,
                                         y = SSR),
               inherit.aes = F,
               shape = 18,
               alpha = .1,
               cex = 0.5)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.0),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.10),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.20),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.30),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.40),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.50),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.60),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.70),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.80),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.90),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.100),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::facet_wrap(swp.ADSR.name ~ . , scales = "free_x",
               ncol = round(1.1*length(levels(DF.melt.CI.envelope$swp.ADSR.name))/2))+
    ggplot2::theme_linedraw()+
    ggplot2::scale_y_log10()+
    ggplot2::geom_ribbon(inherit.aes = F,
                data = DF.melt.CI.envelope,
                ggplot2::aes(x = swp.ADSR.value,
                    ymin = ribbon.y.min,
                    ymax = Y.min),
                alpha = .2,
                fill = "chartreuse3")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank(),
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot",
          plot.caption =  ggplot2::element_text(hjust = 0))+
    ggplot2::labs(title = "Fit residuals of CI fitted simulations",
         caption =
           paste0(" Fit method: summed squares of residuals.",
                  " \n ", "Green lines show percentiles of distributions, from 0 to 100\u0025."
           ))

  # plot.surface_qp_CI

  #### ___ ii. without CI fit & parameter_subsets ####

  DF.melt.envelope <- reshape2::melt(DF,
                                     id.vars = names(DF)[!(names(DF) %in% names.swp.ADSR)],
                                     variable.name = "swp.ADSR.name",
                                     value.name = "swp.ADSR.value") %>%
    dplyr::group_by(swp.ADSR.name,
             swp.ADSR.value) %>%
    dplyr::summarise(Y.min = min(SSR),
              Y.max = max(SSR),
              Y.0 = stats::quantile(SSR, 0),
              Y.1 = stats::quantile(SSR, 0.01),
              Y.2 = stats::quantile(SSR, 0.02),
              Y.3 = stats::quantile(SSR, 0.03),
              Y.4 = stats::quantile(SSR, 0.04),
              Y.5 = stats::quantile(SSR, 0.05),
              Y.6 = stats::quantile(SSR, 0.06),
              Y.7 = stats::quantile(SSR, 0.07),
              Y.8 = stats::quantile(SSR, 0.08),
              Y.9 = stats::quantile(SSR, 0.09),
              Y.10 = stats::quantile(SSR, 0.1),
              Y.20 = stats::quantile(SSR, 0.20),
              Y.30 = stats::quantile(SSR, 0.30),
              Y.40 = stats::quantile(SSR, 0.40),
              Y.50 = stats::quantile(SSR, 0.50),
              Y.60 = stats::quantile(SSR, 0.60),
              Y.70 = stats::quantile(SSR, 0.70),
              Y.80 = stats::quantile(SSR, 0.80),
              Y.90 = stats::quantile(SSR, 0.90),
              Y.100 = stats::quantile(SSR, 1),
              .groups = 'drop_last') %>%
    as.data.frame()

  quiet(gc())

  lt_loc <- 1
  size_loc <- 0.25

  ribbon.y.min <- .75*10**((log10(min(DF$SSR))))

  plot.surface_qp_all <- ggplot2::ggplot(DF.melt.envelope,
                                ggplot2::aes(x = swp.ADSR.value,
                                    y = Y.min))+
    ggplot2::geom_linerange(data = DF.melt.envelope,
                   ggplot2::aes(x= swp.ADSR.value,
                       ymin =  Y.min,
                       ymax =  Y.max),
                   inherit.aes = F, alpha = .1)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.0),   #0
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.1),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.2),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.3),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.4),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.5),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.6),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.7),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.8),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.9),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.10),    #10
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.20),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.30),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.40),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.50),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.60),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.70),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.80),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.90),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(x = swp.ADSR.value, y = Y.100),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::facet_wrap(swp.ADSR.name ~ . , scales = "free_x")+
    ggplot2::theme_linedraw()+
    ggplot2::scale_y_log10()+
    ggplot2::geom_point(data = DF.melt.CI, inherit.aes = F,
               ggplot2::aes(x = swp.ADSR.value,
                   y = SSR),
               shape = 18,
               alpha = 1,
               color = "red",
               cex = 0.5)+
    ggplot2::geom_ribbon(inherit.aes = F,
                data = DF.melt.envelope,
                ggplot2::aes(x = swp.ADSR.value,
                    ymin = ribbon.y.min,
                    ymax = Y.min),
                alpha = .2,
                fill = "chartreuse3")+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid = ggplot2::element_blank(),
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot",
          plot.caption =  ggplot2::element_text(hjust = 0))+
    ggplot2::labs(title = "Fit residuals of all simulations",
         caption =
           paste0(" Fit method: summed squares of residuals.",
                  " \n ", "Green lines show percentiles of distributions, from 0 to 100\u0025.",
                  " \n", " Red dots corresponds to the runs fitted within observed CI and parameter subsets."
           ))

  # plot.surface_qp_all
  # remove(DF.melt, DF.melt.CI, DF.melt.CI.envelope, DF.melt.envelope, lt_loc, size_loc )

  return(list(plot.all = plot.surface_qp_all,
              plot.CI = plot.surface_qp_CI))
}

#  #_________________________________________________________________________80char
#' plot_LS_surfaces_lists
#'
#' @description  Plot surfaces of summed squared residuals.
#'
#' @param DF fit.final_space data frame
#' @param names.swp.lists names of swept list parameters (flux and coeff lists), as vector
#'
#' @return A plot of surfaces of summed squared residuals for flux lists and coeff lists.
#'
#' @keywords internal
plot_LS_surfaces_lists <- function(DF, names.swp.lists){

  names.swp.list.name <-
    names.swp.list.ID.start_text <-
    names.swp.list.ID.label <-
    SSR <-
    quantile <-
    Y.min <-
    Y.max <-
    Y.0 <-
    Y.20 <-
    Y.40 <-
    Y.60 <-
    Y.80 <-
    Y.100 <-
    Y.1 <-
    Y.2 <-
    Y.3 <-
    Y.4 <-
    Y.5 <-
    Y.6 <-
    Y.7 <-
    Y.8 <-
    Y.9 <-
    Y.10 <-
    Y.30 <-
    Y.50 <-
    Y.70 <-
    Y.90 <- NULL

  #### ___ i. with CI fit & param subset ####
  DF.melt.CI <- reshape2::melt(DF[DF$in.all.subset_param == TRUE & DF$in.all_fit_boxes.obs.CI == TRUE, ],
                               id.vars = names(DF)[!(names(DF) %in% names.swp.lists)],
                               variable.name = "names.swp.list.name",
                               value.name = "names.swp.list.ID")

  # relabel if lists are numbered
  names.swp.list.ID <- DF.melt.CI$names.swp.list.ID
  names.swp.list.ID.end_number <- stringr::str_extract(names.swp.list.ID, "(\\d+$)")
  if (!any(is.na(names.swp.list.ID.end_number))){
    DF.melt.CI$names.swp.list.ID.end_number <- as.numeric(names.swp.list.ID.end_number)
    DF.melt.CI$names.swp.list.ID.start_text <- gsub("^\\d+|\\d+$", "", names.swp.list.ID)
    DF.melt.CI$names.swp.list.ID.label <- as.factor(DF.melt.CI$names.swp.list.ID.end_number)
    x_axis_labels <- DF.melt.CI %>%
      dplyr::group_by(names.swp.list.name,
               names.swp.list.ID.start_text) %>%
      dplyr::summarise(n.list = paste(sort(unique(names.swp.list.ID.end_number)), collapse = ", "), .groups = "drop_last") %>%
      as.data.frame()
    x_axis_labels <- paste(x_axis_labels$names.swp.list.name, ":", x_axis_labels$names.swp.list.ID.start_text, "(", x_axis_labels$n.list, ")")
  } else {
    DF.melt.CI$names.swp.list.ID.label <- DF.melt.CI$names.swp.list.ID
    x_axis_labels <- "See x axis"
  }

  DF.melt.CI.envelope <- DF.melt.CI %>%
    dplyr::group_by(names.swp.list.name,
             names.swp.list.ID.label) %>%
    dplyr::summarise(Y.min = min(SSR),
              Y.max = max(SSR),
              Y.0 = stats::quantile(SSR, 0),
              Y.1 = stats::quantile(SSR, 0.01),
              Y.2 = stats::quantile(SSR, 0.02),
              Y.3 = stats::quantile(SSR, 0.03),
              Y.4 = stats::quantile(SSR, 0.04),
              Y.5 = stats::quantile(SSR, 0.05),
              Y.6 = stats::quantile(SSR, 0.06),
              Y.7 = stats::quantile(SSR, 0.07),
              Y.8 = stats::quantile(SSR, 0.08),
              Y.9 = stats::quantile(SSR, 0.09),
              Y.10 = stats::quantile(SSR, 0.1),
              Y.20 = stats::quantile(SSR, 0.20),
              Y.30 = stats::quantile(SSR, 0.30),
              Y.40 = stats::quantile(SSR, 0.40),
              Y.50 = stats::quantile(SSR, 0.50),
              Y.60 = stats::quantile(SSR, 0.60),
              Y.70 = stats::quantile(SSR, 0.70),
              Y.80 = stats::quantile(SSR, 0.80),
              Y.90 = stats::quantile(SSR, 0.90),
              Y.100 = stats::quantile(SSR, 1),
              .groups = 'drop_last') %>%
    as.data.frame()

  lt_loc <- 1
  size_loc <- 0.25

  plot.surface_lists_CI <-
    ggplot2::ggplot(DF.melt.CI.envelope,
           ggplot2::aes(x = (names.swp.list.ID.label),
               y = Y.min))+
    ggplot2::geom_linerange(data = DF.melt.CI.envelope,
                   ggplot2::aes(x= (names.swp.list.ID.label),
                       ymin =  Y.min,
                       ymax =  Y.max),
                   inherit.aes = F, alpha = .1)+
    ggplot2::facet_wrap(names.swp.list.name ~ . , scales = "free_x")+
    ggplot2::theme_linedraw()+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.0),   #0
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.1),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.2),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.3),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.4),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.5),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.6),
  #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
  # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.7),
  #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
  # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.8),
  #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
  # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.9),
  #           color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
  # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.10),    #10
  #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
  ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.20),
            color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.30),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.40),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.50),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.60),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.70),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.80),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    # ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.90),
    #           color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.CI.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.100),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_segment(inherit.aes = F,
                 data = DF.melt.CI.envelope,
                 ggplot2::aes(x = names.swp.list.ID.label,
                     xend = names.swp.list.ID.label,
                     y = .1*min(Y.min),
                     yend = Y.min),
                 size = 5,
                 color = "darkgreen",
                 alpha = .7) +
    # ggplot2::scale_y_log10()+``
    ggplot2::scale_y_log10()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot",
          plot.caption =  ggplot2::element_text(hjust = 0),
          panel.grid = ggplot2::element_blank())+
    ggplot2::labs(title = "Fit residuals of CI fitted simulations",
         x = "list labels",
         caption =
           paste0(" Fit method: summed squares of residuals.",
                  "\n ", "Green lines show percentiles of distributions, from 0 to 100\u0025.",
                  "\n ", "x axis labels:",
                  "\n ", paste(x_axis_labels, collapse = "\n ")
           ))

  #### ___ ii. without CI fit & param subset ####

  DF.melt <- reshape2::melt(DF,
                            id.vars = names(DF)[!(names(DF) %in% names.swp.lists)],
                            variable.name = "names.swp.list.name",
                            value.name = "names.swp.list.ID")

  # relabel if lists are numbered
  names.swp.list.ID <- DF.melt$names.swp.list.ID
  names.swp.list.ID.end_number <- stringr::str_extract(names.swp.list.ID, "(\\d+$)")
  if (!any(is.na(names.swp.list.ID.end_number))){
    DF.melt$names.swp.list.ID.end_number <- as.numeric(names.swp.list.ID.end_number)
    DF.melt$names.swp.list.ID.start_text <- gsub("^\\d+|\\d+$", "", names.swp.list.ID)
    DF.melt$names.swp.list.ID.label <- as.factor(DF.melt$names.swp.list.ID.end_number)
    x_axis_labels <- DF.melt %>%
      dplyr::group_by(names.swp.list.name,
               names.swp.list.ID.start_text) %>%
      dplyr::summarise(n.list = paste(sort(unique(names.swp.list.ID.end_number)), collapse = ", "), .groups = "drop_last") %>%
      as.data.frame()
    x_axis_labels <- paste(x_axis_labels$names.swp.list.name, ":", x_axis_labels$names.swp.list.ID.start_text, "(", x_axis_labels$n.list, ")")
  } else {
    DF.melt$names.swp.list.ID.label <- DF.melt$names.swp.list.ID
    x_axis_labels <- "See x axis"
  }

  DF.melt.envelope <- DF.melt %>%
    dplyr::group_by(names.swp.list.name,
             names.swp.list.ID.label) %>%
    dplyr::summarise(Y.min = min(SSR),
              Y.max = max(SSR),
              Y.0 = stats::quantile(SSR, 0),
              Y.1 = stats::quantile(SSR, 0.01),
              Y.2 = stats::quantile(SSR, 0.02),
              Y.3 = stats::quantile(SSR, 0.03),
              Y.4 = stats::quantile(SSR, 0.04),
              Y.5 = stats::quantile(SSR, 0.05),
              Y.6 = stats::quantile(SSR, 0.06),
              Y.7 = stats::quantile(SSR, 0.07),
              Y.8 = stats::quantile(SSR, 0.08),
              Y.9 = stats::quantile(SSR, 0.09),
              Y.10 = stats::quantile(SSR, 0.1),
              Y.20 = stats::quantile(SSR, 0.20),
              Y.30 = stats::quantile(SSR, 0.30),
              Y.40 = stats::quantile(SSR, 0.40),
              Y.50 = stats::quantile(SSR, 0.50),
              Y.60 = stats::quantile(SSR, 0.60),
              Y.70 = stats::quantile(SSR, 0.70),
              Y.80 = stats::quantile(SSR, 0.80),
              Y.90 = stats::quantile(SSR, 0.90),
              Y.100 = stats::quantile(SSR, 1),
              .groups = 'drop_last') %>%
    as.data.frame()

  lt_loc <- 1
  size_loc <- 0.25

  plot.surface_lists_all <-
    ggplot2::ggplot(DF.melt.envelope,
           ggplot2::aes(x = names.swp.list.ID.label,
               y = Y.min))+
    ggplot2::geom_linerange(data = DF.melt.envelope,
                   ggplot2::aes(x= names.swp.list.ID.label,
                       ymin =  Y.min,
                       ymax =  Y.max),
                   inherit.aes = F, alpha = .1)+
    ggplot2::facet_wrap(names.swp.list.name ~ . , scales = "free_x")+
    ggplot2::theme_linedraw()+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.0),   #0
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.1),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.2),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.3),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.4),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.5),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.6),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.7),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.8),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.9),
              color = "chartreuse4", linetype = lt_loc , size = size_loc/2)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.10),    #10
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.20),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.30),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.40),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.50),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.60),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.70),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.80),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.90),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_line(data = DF.melt.envelope, ggplot2::aes(group = 1, x = as.factor(names.swp.list.ID.label), y = Y.100),
              color = "chartreuse4", linetype = lt_loc , size = size_loc)+
    ggplot2::geom_segment(inherit.aes = F,
                 data = DF.melt.envelope,
                 ggplot2::aes(x = names.swp.list.ID.label,
                     xend = names.swp.list.ID.label,
                     y = .1*min(Y.min),
                     yend = Y.min),
                 size = 5,
                 color = "darkgreen",
                 alpha = .7) +
    ggplot2::geom_point(data = DF.melt.CI,
               inherit.aes = F,
               ggplot2::aes(x = names.swp.list.ID.label,
                   y = SSR),
               shape = 18,
               alpha = 1,
               color = "red",
               cex = 0.5)+
    # ggplot2::scale_y_log10()+``
    ggplot2::scale_y_log10()+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
          plot.caption.position =  "plot",
          plot.caption =  ggplot2::element_text(hjust = 0),
          panel.grid = ggplot2::element_blank())+
    ggplot2::labs(title = "Fit residuals of all simulations",
         x = "list labels",
         caption =
           paste0(" Fit method: summed squares of residuals.",
                  "\n ", "Green lines show percentiles of distributions, from 0 to 100\u0025.",
                  "\n ", "Red dots corresponds to the runs fitted within observed CI and parameter subsets.",
                  "\n ", "x axis labels:",
                  "\n ", paste(x_axis_labels, collapse = "\n ")
           ))

  return(list(plot.surface_lists_all = plot.surface_lists_all,
              plot.surface_lists_CI = plot.surface_lists_CI))
}

#  #_________________________________________________________________________80char
#' plot_correlogram_all_CIfit
#'
#' @description  Plot correlogram of CI fitted ADSR parameter values
#'
#' @param DF fit.final_space data frame
#' @param names.swp.ADSR names of swept ADSR parameters (alpha, delta, size, rayleigh, custom expressions), as vector
#'
#' @return A correlogram of CI fitted ADSR parameter values
#'
#' @keywords internal
plot_correlogram_all_CIfit <- function(DF, names.swp.ADSR){

  X.ID <-
    Y.ID <-
    spear.rho <-
    spear.p <-
    spear.p.signif <-
    label.swp.ADSR <- NULL

  # prepare labels for diagonal of correlograms
  ADSR_starts_with <- c(paste("swp", c("A", "D", "S", "R"), sep = "."), "custom")
  decomposed.names.swp.ADSR <- data.frame(names.swp.ADSR = names.swp.ADSR,
                                          type.swp.ADSR = NaN, def.swp.ADSR = NaN)
  decomposed.names.swp.ADSR$def.swp.ADSR <-
    stringr::str_remove_all(decomposed.names.swp.ADSR$names.swp.ADSR,
                            pattern = paste(paste0(ADSR_starts_with, "."), collapse = "|"))
  for (j in 1:nrow(decomposed.names.swp.ADSR)){
    decomposed.names.swp.ADSR[j, "type.swp.ADSR"] <-
      stringr::str_remove(decomposed.names.swp.ADSR[j, "names.swp.ADSR"],
                          pattern = paste0(".", decomposed.names.swp.ADSR[j, "def.swp.ADSR"]))
  }
  decomposed.names.swp.ADSR$label.swp.ADSR <-
    paste0(decomposed.names.swp.ADSR$type.swp.ADSR, " \n ", decomposed.names.swp.ADSR$def.swp.ADSR)
  decomposed.names.swp.ADSR$X.ID <- decomposed.names.swp.ADSR$names.swp.ADSR
  decomposed.names.swp.ADSR$Y.ID <- decomposed.names.swp.ADSR$names.swp.ADSR
  decomposed.names.swp.ADSR$label.swp.ADSR <-
    stringr::str_replace_all(decomposed.names.swp.ADSR$label.swp.ADSR,
                             pattern =  "/", replacement = "\n / \n")

  #### ___ i. prepare data: standardize (center, scale) ####
  all.ADRS <- DF[DF$in.all_fit_boxes.obs.CI == TRUE &
                   DF$in.all.subset_param == TRUE, c("SSR", names.swp.ADSR)]

  standardized.all.ADRS <- all.ADRS %>%
    scale(scale = TRUE, center = TRUE) %>%
    as.data.frame() %>%
    dplyr::select_if(~!all(is.na(.)))

  my_data <- standardized.all.ADRS

  if (ncol(my_data) >= 3){
    # XID_facet_order <- unique(pairs$V1)
    # YID_facet_order <- unique(pairs$V2)
    ID_facet_order <- sort(names(my_data), decreasing = T)
    ID_facet_order <- c(ID_facet_order[!ID_facet_order %in% c("SSR")], "SSR")
    pairs <- as.data.frame(t(utils::combn(ID_facet_order,2)))

    for (k1 in 1:nrow(pairs)){
      X <- pairs[k1, 1]
      Y <- pairs[k1,2]
      if (k1 == 1){
        my_data_pairs <- my_data[, c(X,Y)]
        my_data_pairs$X.ID <- X
        my_data_pairs$Y.ID <- Y
        names(my_data_pairs) <- c("X", "Y", "X.ID", "Y.ID")
        my_data_pairs <- my_data_pairs[,c("X.ID","X", "Y.ID", "Y")]
      } else {
        my_data_pairs.loc <- my_data[, c(X,Y)]
        my_data_pairs.loc$X.ID <- X
        my_data_pairs.loc$Y.ID <- Y
        names(my_data_pairs.loc) <- c("X", "Y", "X.ID", "Y.ID")
        my_data_pairs.loc <- my_data_pairs.loc[,c("X.ID","X", "Y.ID", "Y")]
        my_data_pairs <- dplyr::bind_rows(my_data_pairs, my_data_pairs.loc)
      }
    }

    my_data_pairs$X.ID <- as.factor(my_data_pairs$X.ID)
    my_data_pairs$Y.ID <- as.factor(my_data_pairs$Y.ID)

    ## in centered normalized parameter values
    summary.my_data_pairs <- my_data_pairs %>%
      dplyr::group_by(X.ID, Y.ID) %>%
      dplyr::summarise(n = dplyr::n(),
                spear.rho = stats::cor(X, Y, method = "spearman"),
                spear.p = stats::cor.test(X, Y, method = "spearman", na.rm = T, exact = F)$p.value,
                pear.r = stats::cor(X, Y, method = "pearson"),
                pear.p = stats::cor.test(X, Y, method = "pearson")$p.value, .groups = "drop_last"
      ) %>%
      as.data.frame()

    summary.my_data_pairs[, c("spear.p.signif", "pear.p.signif")] <-
      significance_pval_df(summary.my_data_pairs[, c("spear.p", "pear.p")])
    my_data_pairs <- dplyr::right_join(my_data_pairs, summary.my_data_pairs, by = c("X.ID", "Y.ID"))
    # my_data_pairs$X.ID = factor(my_data_pairs$X.ID,  levels = XID_facet_order)
    # my_data_pairs$Y.ID = factor(my_data_pairs$Y.ID, levels = YID_facet_order)
    XY_labels <- decomposed.names.swp.ADSR[,c("X.ID", "Y.ID", "label.swp.ADSR")]
    XY_labels[nrow(XY_labels)+1, ] <- "SSR"
    names(summary.my_data_pairs)[names(summary.my_data_pairs) %in% c("X.ID", "Y.ID")] <- c("Y.ID", "X.ID")

    Y.max <- ceiling(max(abs(my_data_pairs$Y)))
    X.max <- ceiling(max(abs(my_data_pairs$X)))

    XY_labels <- XY_labels[XY_labels$X.ID %in% unique(c(as.character(unique(my_data_pairs$X.ID)),
                                                        as.character(unique(my_data_pairs$Y.ID)))), ]

    #### ___ ii. plot correlations qp within CI fitted & param subset ####
    distinct.my_data_pairs <- my_data_pairs %>% dplyr::distinct()

    plot.correlogram.CI_fit <- ggplot2::ggplot(data = my_data_pairs,
                                      ggplot2::aes(x = X,
                                          y = Y
                                      ))+
      ggplot2::geom_point(inherit.aes = F,
                 data = summary.my_data_pairs,
                 shape = 15,
                 ggplot2::aes(x = 0,
                     y = 0,
                     color = abs(spear.rho),
                     size = abs(spear.rho),
                     alpha = (-log10(spear.p)/max(-log10(my_data_pairs$spear.p[my_data_pairs$spear.p!=0])))))+
      ggplot2::scale_size_continuous(range = c(1,18))+
      ggplot2::scale_colour_gradient(low = "white", high = "orange")+
      ggplot2::scale_fill_gradient(low = "white", high = "black")+
      # ggplot2::geom_point(data = distinct.my_data_pairs,
      #            shape = 21,
      #            stroke = 0,
      #            color = "black",
      #            fill = "black"
      # )+
      ggplot2::geom_bin2d()+
      ggplot2::stat_smooth(method = "loess", se = F, formula = "y ~ x",
                  size = .5,
                  ggplot2::aes(alpha =   abs(spear.rho))
      )+
      ggplot2::labs(title = "Full correlogram",
           subtitle = "for all CI-fitted quantitative parameters and SSR (sums of squared residuals)",
           caption =
             paste("Statistics are Spearman absolute rho (size and color of central square) and p-value significance.",
                   " \n", "Blue line is smooth loess method.",
                   sep = ""
             )
      )+
      # egg::theme_article()+
      ggplot2::theme_linedraw()+
      ggplot2::theme(legend.position = "None",
            strip.text.x = ggplot2::element_blank(),
            strip.text.y = ggplot2::element_blank(),
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            plot.caption = ggplot2::element_text(hjust = 0),
            panel.grid.minor = ggplot2::element_blank()
      )+
      ggplot2::facet_grid( factor(Y.ID, levels = ID_facet_order) ~
                    factor(X.ID, levels = ID_facet_order))+
      ggplot2::geom_text(data = summary.my_data_pairs,
                ggplot2::aes(x = 0,
                    y = .7*Y.max,
                    label = spear.p.signif),
                hjust   = 0.5,
                vjust   = 0)+
      ggplot2::geom_text(data = summary.my_data_pairs,
                ggplot2::aes(x = 0,
                    y = -.8*Y.max,
                    label = paste("|rho| =", dec_n(abs(spear.rho), 3))),
                size = 2,
                hjust   = 0.5,
                vjust   = 0.5)+
      ggplot2::scale_y_continuous(limits = c(-Y.max,Y.max))+
      ggplot2::scale_x_continuous(limits = c(-X.max,X.max))+
      ggplot2::geom_text(inherit.aes = F,
                data = XY_labels,
                ggplot2::aes(x = 0,
                    y = 0,
                    label = label.swp.ADSR),
                vjust = 0.5,
                size = 3)
    # plot.correlogram.CI_fit
    return(plot.correlogram.CI_fit)
  }
}

#  #_________________________________________________________________________80char
#' plot_lda
#'
#' @description  Plot linear discriminant analysis by quartiles of summed squared residuals for all ADSR parameters
#'
#' @param DF fit.final_space data frame
#' @param names.swp.ADSR names of swept ADSR parameters (alpha, delta, size, rayleigh, custom expressions), as vector
#'
#' @return A linear discriminant analysis by quartiles of summed squared residuals for all ADSR parameters
#'
#' @keywords internal
plot_lda <- function(DF, names.swp.ADSR){

  ..level.. <- LD2 <- LD1 <- SSR <- NULL

  all.ADRS <- DF[DF$in.all_fit_boxes.obs.CI == TRUE &
                   DF$in.all.subset_param == TRUE, c("SSR", names.swp.ADSR)]

  standardized.all.ADRS <- all.ADRS %>%
    scale(center =  TRUE, scale = TRUE) %>%
    as.data.frame() %>%
    dplyr::select_if(~!all(is.na(.)))

  if (ncol(standardized.all.ADRS) >= 4){
    dat <- standardized.all.ADRS
    names(dat) <- stringr::str_replace_all(names(dat), pattern = "/", replacement = "_")
    dat_num_vars <- names(dat)
    lda_dat_num_vars <- dat_num_vars[!dat_num_vars %in% "SSR"]
    dat <- dat %>% dplyr::mutate(cut=cut(include.lowest = T,
                                  SSR, breaks = as.vector(stats::quantile(dat$SSR)),
                                  labels = c("SSR 0-25\u0025","SSR 25-50\u0025","SSR 50-75\u0025", "SSR 75-100\u0025")))
    lda_dat_num_vars <- dat_num_vars[!dat_num_vars %in% "SSR"]
    linear <- MASS::lda(cut ~ ., dat[, c(lda_dat_num_vars, "cut")])
    Proportion_of_trace <- linear$svd^2/sum(linear$svd^2)
    lda_plot <- cbind(dat[, c(dat_num_vars, "cut")], stats::predict(linear, dat[, c(dat_num_vars, "cut")])$x) # define data to plot
    col<- grDevices::colorRampPalette(c("chartreuse2", "brown1"))(4)
    cols <- c("SSR 0-25\u0025" = col[1], "SSR 25-50\u0025" = col[2], "SSR 50-75\u0025" = col[3], "SSR 75-100\u0025" = col[4])
    # circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    #   r = diameter / 2
    #   tt <- seq(0,2*pi,length.out = npoints)
    #   xx <- center[1] + r * cos(tt)
    #   yy <- center[2] + r * sin(tt)
    #   return(data.frame(x = xx, y = yy))
    # }
    # circ <- circleFun(c(0,0), 2*max(abs(linear$scaling[,1:2])) ,npoints = 500)

    if (nrow(lda_plot) <= 1000 ){
      plot.lda <- ggplot2::ggplot(lda_plot, ggplot2::aes(LD1, LD2, color = cut)) +
        ggplot2::geom_point()
    } else {
      plot.lda <-
        ggplot2::ggplot(lda_plot, ggplot2::aes(LD1, LD2, color = cut)) +
        ggplot2::stat_density_2d(color = NA,
                        geom = "polygon", ggplot2::aes(alpha = ..level.., fill = cut))
    }

    plot.lda <- plot.lda +
      ggplot2::stat_ellipse(geom="polygon",
                   ggplot2::aes(fill = cut),
                   # fill = NA,
                   alpha = 0.2,
                   show.legend = FALSE,
                   level = .5)+
      # egg::theme_article()+
      ggplot2::theme_classic()+
      # ggforce::geom_mark_hull(alpha = 0.1,
      #                         expand = .001,
      #                         linetype = 2,
      #                         radius = 0,
      #                         concavity = 10,
      #                         con.cap = 1)+
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::scale_fill_manual(values = cols) +
      ggplot2::geom_segment(data = as.data.frame(linear$scaling),
                   ggplot2::aes(x = 0, xend = LD1, y = 0, yend = LD2),
                   arrow = ggplot2::arrow(length = ggplot2::unit(0.025, "npc"), type = "open"),
                   lwd = 1, color = "black",
                   inherit.aes = F) +
      ggrepel::geom_label_repel(data = as.data.frame(linear$scaling),
                                ggplot2::aes(x = LD1*1.3, y =  LD2*1.3,
                                    label = lda_dat_num_vars),
                                # check_overlap = F,
                                force = 1,
                                size = 3,
                                label.size = NA,
                                fill = NA,
                                inherit.aes = F)+
      ggplot2::labs(x = paste0("LD 1 (", dec_n(100*Proportion_of_trace[1], 2), "\u0025)"),
           y = paste0("LD 2 (", dec_n(100*Proportion_of_trace[2], 2), "\u0025)"),
           title = "Linear discriminant analysis of quantitative parameters for least squares best fits",
           caption =
             paste0("SSR categories correspond to inter-quantiles groups of the sum of squared residuals (SSR).", "\n",
                    "Ellipses show 50\u0025 distribution."))+
      ggplot2::theme(legend.position = "bottom",
            plot.caption = ggplot2::element_text(hjust = 0)
      )

    plot.lda

  } else {
    rlang::inform("\U2139 Not enough continuous parameters for Linear Discriminant Analysis.")
  }
}
