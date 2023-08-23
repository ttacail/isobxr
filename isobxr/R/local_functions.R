# clear a subset
#' Clear a subset
#' @description Takes a previously subsetted dataframe, clears its deleted levels and resets its row indexes.
#' @param dataset Previously subsetted dataframe.
#' @return Cleared dataframe.
#' @keywords internal
clear_subset <- function(dataset){
  dataset <- droplevels(dataset)
  rownames(dataset) <- NULL
  return(dataset)}

#' Print a number with 0 decimal figures
#' @description Takes a numerical value and returns a string of the rounded value, with 0 decimal figures.
#' @param x Numerical value
#' @return A character string with 0 decimal figures.
#' @keywords internal
dec_0 <- function(x) sprintf("%.0f", x)

#' Print a number with 1 decimal figures
#' @description Takes a numerical value and returns a print with 1 decimal figures.
#' @param x Numerical value
#' @return A character string with 1 decimal figure.
#' @keywords internal
dec_1 <- function(x) sprintf("%.1f", x)

#' Print a number with 2 decimal figures
#' @description Takes a numerical value and returns a print with 2 decimal figures.
#' @param x Numerical value
#' @return A character string with 2 decimal figures.
#' @keywords internal
dec_2 <- function(x) sprintf("%.2f", x)

#' Print a number with 3 decimal figures
#' @description Takes a numerical value and returns a print with 3 decimal figures.
#' @param x Numerical value
#' @return A character string with 3 decimal figures.
#' @keywords internal
dec_3 <- function(x) sprintf("%.3f", x)

#' Print a number with 4 decimal figures
#' @description Takes a numerical value and returns a print with 4 decimal figures.
#' @param x Numerical value
#' @return A character string with 4 decimal figures.
#' @keywords internal
dec_4 <- function(x) sprintf("%.4f", x)

#' Print a number with n decimal figures
#' @description Takes a numerical value and returns a print with n decimal figures.
#' @param x Numerical value
#' @param n number of decimals
#' @return A character string with 4 decimal figures.
#' @keywords internal
dec_n <- function(x,n){
  sprintf(paste0("%.", n, "f"), x)
}

#' Verticalizes a dataframe
#' @description Takes a dataframe with a set of different columns containing numerical values to be verticalized,
#' returns a vertical dataframe with all variables in a single column called "VAR" together with a "VAR_TYPE" column
#' defining the type of variable for the given row, named after the column name found in horizontal dataframe.
#' @param df_hor Horizontal dataframe
#' @param vert_col Vector of the names of the columns of numerical variables to be verticalized.
#' @return A vertical dataframe containing the variables to be verticalized (column "VAR", numeric)
#' and the name of the variable (column "VAR_TYPE", character strings).
#' @keywords internal
DF_verticalizer <- function(df_hor,      # horizontal dataframe
                            vert_col     # vector of column names of numerical variables to be verticalized
){
  hor_col <- names(df_hor)[-which(names(df_hor) %in% vert_col)]
  i <- 1
  for (i in 1:length(vert_col)){
    df_vert_loc <- df_hor[, c(hor_col, vert_col[i])]
    df_vert_loc$VAR_TYPE <- vert_col[i]
    names(df_vert_loc) <- c(hor_col, "VAR", "VAR_TYPE")
    if (i == 1){
      df_vert <- df_vert_loc
    } else {
      df_vert <- rbind(df_vert,df_vert_loc)
    }
    i <- i + 1
  }
  return(df_vert)
}

#' Collate multiple ggplot object into a grid format
#' @description Takes a list of ggplot objects and returns a single object with a grid of the ggplot objects.
#' \cr ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#' \cr If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' multiplot(a, b, c, layout = matrix(c(1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,3), nrow=1, byrow=TRUE))
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @param ... initiation of plot list (ggplot objects)
#' @param plotlist the list of plots
#' @param file file
#' @param cols number of columns for the facetting
#' @param layout user defined matrix layout (numeric matrix). default is NULL.
#' @return A plot composed of multiple subplots.
#' @keywords internal
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}

#' Gauge of calculation
#' @description Prints a gauge of the calculation progress
#' @param i Calculating step number
#' @param len Total calculation steps
#' @return A print summarizing the progress of calculation
#' @keywords internal
calculation_gauge <- function(i, len){
  stars <- paste(rep("*", round(i*50/len, 0)), collapse = "")
  straights <- paste(rep("-", 50-round(i*50/len, 0)), collapse = "")
  perc <- paste(" (", round(i*100/len,0), "% of n = ", as.character(len), ")", sep = "")
  print(paste(stars, straights, perc, sep = ""), quote = F)
}

#' Delete rows of a dataframe containing NaN values in a given column
#' @description Delete rows of a dataframe containing NaN values in a given column
#' @param dataframe dataframe
#' @param by_col Name of column from which NaN values should be removed. Character string.
#' @param resetrows Logical value to reset the row numbering or not.
#' @return Subset of dataframe without the rows containing NaN values in column by_col
#' @keywords internal
del_NaN_rows <- function(dataframe, by_col, resetrows){ # not used in app, could be unexported
  dataframe <- subset(dataframe,!(is.na(dataframe[by_col])))
  if(resetrows == TRUE){rownames(dataframe) <- NULL}
  return(dataframe)
}

#' edits a full dataframe of significance symbols from a dataframe of p-values
#' @description edits a full dataframe of significance symbols from a dataframe of p-values
#' \cr Symbols as asterisks denote p-values lower than .0001, .001, .01, .05. ns refers to non significant (higher than .5).
#' @param x a data frame column of p-values or a pvalues as double (alone or in vector).
#' @return p-values symbols as dataframe or as string.
#' @keywords internal
significance_pval_df <- function(x){
  symbols <- c("****", "***", "**", "*", "ns")
  cut_points <- c(0, 0.0001, 0.001, 0.01, 0.05, 1)
  if(is.data.frame(x)) {
    x_symbols <- x %>%
      dplyr::mutate(dplyr::across(dplyr::everything(),
                                  ~ symbols[base::findInterval(., cut_points)]))
  }
  if(is.double(x)) x_symbols <- symbols[findInterval(x, cut_points)]
  return(x_symbols)
}

#' corr_stats
#' @description edits dataframe report of linear regression and correlation tests (Pearson and Spearman)
#' @param data_lm a data frame with X and Y data
#' @param X name of X variable column (character)
#' @param Y name of Y variable column (character)
#' @param as_character reports statistics formatted as characters. Default is FALSE.
#' @param n number of digits to display in correlation coefficient/rho. Default is 3. Only used if as_character = T.
#' @param m number of digits to display in regression equation parameters (slope, y-intercept). Default is 4. Only used if as_character = T.
#' @return report of regression and correlation tests
#' @keywords internal
corr_stats <- function (data_lm, X, Y, as_character = F,  n = 3, m = 4){
  if(!is.data.frame(data_lm)) stop("data_lm should be a dataframe")
  if(!is.logical(as_character)) stop("as_character should be a logical")
  data_lm$X <- data_lm[, X]
  data_lm$Y <- data_lm[, Y]
  pearson_all <- stats::cor.test(data_lm$X, data_lm$Y, method = "pearson")
  pearson_regeq <- base::summary(stats::lm(Y ~ X, data = data_lm))
  spearman_all <- stats::cor.test(data_lm$X, data_lm$Y, method = "spearman", exact = F)
  if (isTRUE(as_character)){
    lm_df <- data.frame(X = X,
                        Y = Y,
                        n = base::nrow(data_lm),
                        pear.R2 = dec_n(pearson_all$estimate^2, n),
                        pear.pval = base::formatC(pearson_all$p.value, digits = 1, format = "e"),
                        pear.signif = significance_pval_df(pearson_all$p.value),
                        pear.yint = dec_n(pearson_regeq$coefficients[1,1], m),
                        pear.yint.2se = dec_n(2*pearson_regeq$coefficients[1,2], m),
                        pear.slope = dec_n(pearson_regeq$coefficients[2,1], m),
                        pear.slope.2se = dec_n(2*pearson_regeq$coefficients[2,2], m),
                        spear.Rho = dec_n(spearman_all$estimate, n),
                        spear.pval = base::formatC(spearman_all$p.value, digits = 1, format = "e"),
                        spear.signif = significance_pval_df(spearman_all$p.value))
  } else {
    lm_df <- data.frame(X = X,
                        Y = Y,
                        n = base::nrow(data_lm),
                        pear.R2 = pearson_all$estimate^2,
                        pear.pval = pearson_all$p.value,
                        pear.signif = significance_pval_df(pearson_all$p.value),
                        pear.yint = pearson_regeq$coefficients[1,1],
                        pear.yint.2se = 2*pearson_regeq$coefficients[1,2],
                        pear.slope = pearson_regeq$coefficients[2,1],
                        pear.slope.2se = 2*pearson_regeq$coefficients[2,2],
                        spear.Rho = spearman_all$estimate,
                        spear.pval = spearman_all$p.value,
                        spear.signif = significance_pval_df(spearman_all$p.value))
  }

  return(lm_df)
}

#' Calculate delta values at t time with ODE solutions from ana_slvr
#' @description Calculate the delta values at t time using the ODE analytical solutions of the isotopic box model.
#' @param t Time at which the delta values are to be calculated (numeric)
#' @param ODE_Constants Constants as determined by the analytical solver for the system of \cr
#' ordinary differential equations (single column dataframe).
#' @param ODE_Eigenvalues Eigenvalues as determined by the analytical solver for the system of \cr
#' ordinary differential equations (single column dataframe).
#' @param ODE_Eigenvectors Eigenvectors as determined by the analytical solver for the system of \cr
#' ordinary differential equations (multiple columns dataframe).
#' @param BOXES_IDs Vector of character strings with the names of the boxes \cr
#' in the same order as used in ana_slvr.
#' @param ratio_standard Isotope ratio of the reference material used to calculate the delta values.
#' @return Dataframe of the delta values in all boxes at t time.
#' @keywords internal
ANA_delta_t_Calculator <- function(t, ODE_Constants, ODE_Eigenvalues, ODE_Eigenvectors, BOXES_IDs, ratio_standard){
  R_t_loc <- ((ODE_Constants*exp(ODE_Eigenvalues*t)))%*%t(ODE_Eigenvectors)
  d_t_loc <- ((R_t_loc/ratio_standard)-1)*1000
  colnames(d_t_loc) <- BOXES_IDs
  d_t_loc <- as.data.frame(d_t_loc)
  d_t_loc$Time <- t
  d_t_loc <- d_t_loc[,c("Time", BOXES_IDs)]
  return(d_t_loc)
}

#' Convert time units in a dataframe column
#' @description Convert the time units in plots
#' @param dataframe dataframe for which a column with numerical time values should be converted.
#' @param time_colname name of column with time (numerical) values to be converted to a different time unit. \cr
#' (character string)
#' @param conv_timecolname name of the column after time units conversion.  \cr
#' Can be identical to time_colname. \cr
#' (character string)
#' @param former_unit former time unit. Character string among the following: \cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}
#' @param new_unit new time unit. Character string among the following: \cr
#' \emph{micros, ms, s, min, h, d, wk, mo, yr, kyr, Myr, Gyr}
#' @return a dataframe with values converted to new time unit.
#' @keywords internal
time_converter <- function(dataframe,
                           time_colname,
                           conv_timecolname,
                           former_unit, # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"
                           new_unit){   # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"

  # time_conversions from sysdata.rda
  if (!(former_unit %in% time_conversions$UNIT)){
    rlang::abort(paste("Native time unit should be among the following: ", paste(time_conversions$UNIT, collapse = ", ")))
  }

  if (!(new_unit %in% time_conversions$UNIT)){
    rlang::abort(paste("Conversion time unit should be among the following: ", paste(time_conversions$UNIT, collapse = ", ")))
  }

  dataframe$conv_timecolname <- dataframe[,time_colname]*time_conversions[time_conversions$UNIT == new_unit, former_unit]
  names(dataframe)[names(dataframe) == "conv_timecolname"] <- conv_timecolname
  return(dataframe)
}

#' Preventing any console prints (for sweepers only)
#' @description Preventing any console prints (for sweepers only)
#' @param x function to quiet
#' @return No return value, called for side effects
#' @keywords internal
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' address_to_tmpdir
#' @description makes path to a isobxr_temp_data directory located in temporary directory
#' @param dir_or_file directory or file to address in temporary directory
#' @return name of subdirectory or file
#' @keywords internal
to_tmpdir <- function(dir_or_file){
  tmp_path <- paste(tempdir(), "isobxr_temp_data", sep = "/")
  if (dir.exists(tmp_path) == FALSE){
    dir.create(tmp_path)
  }
  if(nchar(dir_or_file) != 0){
    tmp_path <- paste(tmp_path, dir_or_file, sep = "/")
  }
  return(tmp_path)
}

#' plot_diagram
#' @description plot diagrams for fluxes and coefficients using the qgraph network function
#' @param input matrix of fluxes or coefficients
#' @param title title of diagram
#' @param matrix_layout matrix of X,Y layout
#' @param BOXES_master_loc BOXES_master_loc
#' @param COEFF_FLUX "COEFF" or "FLUX"
#' @return diagrams for console or pdf edition (not used in shinobxr)
#' @keywords internal
plot_diagram <- function(input, title, matrix_layout, BOXES_master_loc, COEFF_FLUX){
  if(COEFF_FLUX == "COEFF"){edge_color = "brown4"} else if (COEFF_FLUX == "FLUX"){edge_color = "black"}
  qgraph::qgraph(input = input,
                 title = title,
                 layout = matrix_layout,
                 edge.labels = T,
                 edge.label.color = "black",
                 shape = "square",
                 fade = F,
                 groups = BOXES_master_loc$GROUP,
                 color = rainbow(length(levels(BOXES_master_loc$GROUP)), s = 0.25),
                 legend = F,
                 edge.color = edge_color,
                 edge.label.cex = 2.89*exp(-nrow(BOXES_master_loc)/19),
                 edge.label.margin = 0.02,
                 asize = 9*exp(-nrow(BOXES_master_loc)/20)+2,
                 curve = 0.88*exp(-nrow(BOXES_master_loc)/17.54),
                 curveAll = F,
                 vsize = 14*exp(-nrow(BOXES_master_loc)/80)+1)
}

#' using_extdata_tutorial V1
#' @description identifies workdir value referring to use of extdata tutorial files, returns correct workdir value, \cr
#' prevents saving outputs locally (save_run_outputs = FALSE) \cr
#' forces display of default graphical output to R session (plot_results = TRUE)
#' @param workdir working directory value referring to extdata tutorial files, several options among the following: \cr
#' c("/Users/username/Documents/1_ABC_tutorial",
#' "use_isobxr_demonstration_files",
#' system.file("extdata", package = "isobxr"),
#' system.file("extdata", "0_ISOBXR_MASTER.xlsx", package = "isobxr"))
#' @return a vector with a boolean stating whether this is a tutorial mode, a workdir value allowing reading of extdata tutorial files, save_run_outputs = FALSE, plot_results = TRUE
#' @keywords internal
using_extdata_tutorial <- function(workdir, save_run_outputs, plot_results){
  workdir_to_extdata <- c("/Users/username/Documents/1_ABC_tutorial",
                          "use_isobxr_demonstration_files",
                          system.file("extdata", package = "isobxr"),
                          system.file("extdata", "0_ISOBXR_MASTER.xlsx", package = "isobxr"))
  if (workdir %in% workdir_to_extdata){
    workdir <- system.file("extdata", package = "isobxr")
    save_run_outputs = FALSE
    plot_results = TRUE
    tuto_mode = TRUE
    # rlang::inform("You are using the external data embedded in the isobxr package for the tutorial.
    # This will allow you to run the demonstration models but won't allow to save the outputs to your local working directory.
    # In order to be able to do so, download and save the tutorial files to your working directory.")
  } else {
    tuto_mode = FALSE
  }
  return(c(tuto_mode, workdir, save_run_outputs, plot_results))
}

#' using_extdata_tutorial_2
#' @description identifies workdir value referring to use of extdata tutorial files, returns correct workdir value, \cr
#' prevents saving outputs locally (save_run_outputs = FALSE) \cr
#' forces display of default graphical output to R session (plot_results = TRUE)
#' @param workdir working directory value referring to extdata tutorial files, several options among the following: \cr
#' c("/Users/username/Documents/1_ABC_tutorial",
#' "use_isobxr_demonstration_files",
#' system.file("extdata", package = "isobxr"),
#' system.file("extdata", "0_ISOBXR_MASTER.xlsx", package = "isobxr"))
#' @return a list with a boolean stating whether this is a tutorial mode, a workdir value allowing reading of extdata tutorial files, save_run_outputs = FALSE, plot_results = TRUE
#' @keywords internal
using_extdata_tutorial_2 <- function(workdir, save_outputs, plot_results){

  workdir_to_extdata <- c("/Users/username/Documents/1_ABC_tutorial",
                          "use_isobxr_demonstration_files",
                          "demo",
                          system.file("extdata", package = "isobxr"),
                          system.file("extdata", "0_ISOBXR_MASTER.xlsx", package = "isobxr"))

  if (workdir %in% workdir_to_extdata){
    workdir <- system.file("extdata", package = "isobxr")
    save_outputs = FALSE
    plot_results = TRUE
    tuto_mode = TRUE
    # rlang::inform("You are using the external data embedded in the isobxr package for the tutorial.
    # This will allow you to run the demonstration models but won't allow to save the outputs to your local working directory.
    # In order to be able to do so, download and save the tutorial files to your working directory.")
  } else {
    tuto_mode = FALSE
  }

  return(list(tuto_mode = tuto_mode,
              workdir = workdir,
              save_outputs = save_outputs,
              plot_results = plot_results))
}

#' tempdir_tree
#' @description display tree of the temporary isobxr directory
#' @return a print of the directory tree
#' @keywords internal
tempdir_tree <- function(){
  fs::dir_tree(paste0(tempdir(""), "/isobxr_temp_data"))
}
