# clear a subset
#' Clear a subset
#' @description Takes a previously subsetted dataframe, clears its deleted levels and resets its row indexes.
#' @param dataset Previously subsetted dataframe.
#' @return Cleared dataframe.
#' @export
#' @keywords internal
#' @examples
#' clear_subset(iris[iris$Species == "setosa" & iris$Sepal.Length > 5 ,])
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
#' @export
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

#' Verticalizes a dataframe
#' @description Takes a dataframe with a set of different columns containing numerical values to be verticalized,
#' returns a vertical dataframe with all variables in a single column called "VAR" together with a "VAR_TYPE" column
#' defining the type of variable for the given row, named after the column name found in horizontal dataframe.
#' @param df_hor Horizontal dataframe
#' @param vert_col Vector of the names of the columns of numerical variables to be verticalized.
#' @return A vertical dataframe containing the variables to be verticalized (column "VAR", numeric)
#' and the name of the variable (column "VAR_TYPE", character strings).
#' @keywords internal
#' @export
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
#' @export
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
#' @export
#' @keywords internal
#' @examples
#' ex_df <- data.frame(letters = c("A", "B", "C", "D"),  numbers = c(1,2,3,NaN))
#' ex_df
#' del_NaN_rows(ex_df, c("numbers"), TRUE)
del_NaN_rows <- function(dataframe, by_col, resetrows){ # not used in app, could be unexported
  dataframe <- subset(dataframe,!(is.na(dataframe[by_col])))
  if(resetrows == TRUE){rownames(dataframe) <- NULL}
  return(dataframe)
}

#' Calculate delta values at t time with ODE solutions from \code{\link{ana_slvr}}
#' @description Calculate the delta values at t time using the ODE analytical solutions of the isotopic box model.
#' @param t Time at which the delta values are to be calculated (numeric)
#' @param ODE_Constants Constants as determined by the analytical solver for the system of \cr
#' ordinary differential equations (single column dataframe).
#' @param ODE_Eigenvalues Eigenvalues as determined by the analytical solver for the system of \cr
#' ordinary differential equations (single column dataframe).
#' @param ODE_Eigenvectors Eigenvectors as determined by the analytical solver for the system of \cr
#' ordinary differential equations (multiple columns dataframe).
#' @param BOXES_IDs Vector of character strings with the names of the boxes \cr
#' in the same order as used in \code{\link{ana_slvr}}.
#' @param ratio_standard Isotope ratio of the reference material used to calculate the delta values.
#' @return Dataframe of the delta values in all boxes at t time.
#' @keywords internal
#' @seealso \code{\link{ana_slvr}}
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
#' @export
#' @examples
#' ex_df <- data.frame(observation_num = c(1,2,3,4), time_d = c(100, 365, 1000, 3650))
#' time_converter(ex_df, "time_d", "time_yr", "d", "yr")
time_converter <- function(dataframe,
                           time_colname,
                           conv_timecolname,
                           former_unit, # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"
                           new_unit){   # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"

  time_converting_table <- data.frame(UNIT = c("micros", "ms", "s", "min", "h", "d", "wk", "mo", "yr", "kyr", "Myr", "Gyr"),
                                      micros = c(1, 0.001, 0.000001, 1.66666666666667E-08, 2.77777777777778E-10, 1.15740740740741E-11, 1.65343915343915E-12, 3.79477838506674E-13, 3.1688087804657E-14, 3.1688087804657E-17, 3.1688087804657E-20, 3.1688087804657E-23),
                                      ms = c(1000, 1, 0.000001, 1.66666666666667E-08, 2.77777777777778E-10, 1.15740740740741E-11, 1.65343915343915E-12, 3.79477838506674E-13, 3.1688087804657E-14, 3.1688087804657E-17, 3.1688087804657E-20, 3.1688087804657E-23),
                                      s = c(1000000, 1000000, 1, 0.0166666666666667, 0.000277777777777778, 1.15740740740741E-05, 1.65343915343915E-06, 3.79477838506674E-07, 3.1688087804657E-08, 3.1688087804657E-11, 3.1688087804657E-14, 3.1688087804657E-17),
                                      min = c(60000000, 60000000, 60, 1, 0.0166666666666667, 0.000694444444444444, 9.92063492063492E-05, 2.27686703104004E-05, 1.90128526827942E-06, 1.90128526827942E-09, 1.90128526827942E-12, 1.90128526827942E-15),
                                      h = c(3600000000, 3600000000, 3600, 60, 1, 0.0416666666666667, 0.00595238095238095, 0.00136612021862403, 0.000114077116096765, 1.14077116096765E-07, 1.14077116096765E-10, 1.14077116096765E-13),
                                      d = c(86400000000, 86400000000, 86400, 1440, 24, 1, 0.142857142857143, 0.0327868852469766, 0.00273785078632237, 2.73785078632237E-06, 2.73785078632237E-09, 2.73785078632237E-12),
                                      wk = c(604800000000, 604800000000, 604800, 10080, 168, 7, 1, 0.229508196728836, 0.0191649555042566, 1.91649555042566E-05, 1.91649555042566E-08, 1.91649555042566E-11),
                                      mo = c(2635199999913.6, 2635199999913.6, 2635199.9999136, 43919.99999856, 731.999999976, 30.499999999, 4.357142857, 1, 0.0835044489800944, 8.35044489800944E-05, 8.35044489800944E-08, 8.35044489800944E-11),
                                        yr = c(31557600009333.3, 31557600009333.3, 31557600.0093333, 525960.000155555, 8766.00000259259, 365.250000108025, 52.1785714440035, 11.97540984, 1, 0.001, 0.000001, 0.000000001),
                                      kyr = c(31557600009333300, 31557600009333300, 31557600009.3333, 525960000.155555, 8766000.00259259, 365250.000108025, 52178.5714440035, 11975.40984, 1000, 1, 0.001, 0.000001),
                                      Myr = c(31557600009333300000, 31557600009333300000, 31557600009333.3, 525960000155.555, 8766000002.59259, 365250000.108025, 52178571.4440035, 11975409.84, 1000000, 1000, 1, 0.001),
                                      Gyr = c(3.15576000093333E+22, 3.15576000093333E+22, 31557600009333300, 525960000155555, 8766000002592.59, 365250000108.025, 52178571444.0035, 11975409840, 1000000000, 1000000, 1000, 1))

  if (!(former_unit %in% time_converting_table$UNIT)){
    rlang::abort(paste("Native time unit should be among the following: ", paste(time_converting_table$UNIT, collapse = ", ")))
  }

  if (!(new_unit %in% time_converting_table$UNIT)){
    rlang::abort(paste("Conversion time unit should be among the following: ", paste(time_converting_table$UNIT, collapse = ", ")))
  }

  dataframe$conv_timecolname <- dataframe[,time_colname]*time_converting_table[time_converting_table$UNIT == new_unit, former_unit]
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

#' using_extdata_tutorial
#' @description identifies workdir value refering to use of extdata tutorial files, returns correct workdir value, \cr
#' prevents saving outputs locally (save_run_outputs = FALSE) \cr
#' forces display of default graphical output to R session (plot_results = TRUE)
#' @param workdir working directory value refering to extdata tutorial files, several options among the following: \cr
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
