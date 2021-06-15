usethis::use_package("shiny", min_version = TRUE)
usethis::use_package("shinyFiles", min_version = TRUE)
usethis::use_package("shinythemes", min_version = TRUE)
usethis::use_package("shinyjs", min_version = TRUE)
usethis::use_package("DT", min_version = TRUE)
usethis::use_package("data.table", min_version = TRUE)
usethis::use_package("ggplot2", min_version = TRUE)
usethis::use_package("readxl", min_version = TRUE)
usethis::use_package("ggrepel", min_version = TRUE)
usethis::use_package("metR", min_version = TRUE)
usethis::use_package("qgraph", min_version = TRUE)

#************************************** LOCAL FUNCTIONS
#' Call isobxr plot shiny app
#' @description A function to call the isobxr plot shiny app to interactively plot outputs from \code{\link{compose_isobxr}},
#' \code{\link{sweep_steady}} and \code{\link{sweep_dyn}}. \cr
#' The function takes no arguments but requires the definition of a working directory where all SERIES directory are stored. \cr
#' This working directory needs to be defined as a character string and stored in a variable called workdir.
#' For instance: \cr
#' workdir = "User/isobxr_working_directory"
#' @export
runShinyPlots <- function() {
  appDir <- system.file("shiny-examples", "plot_isobxr", package = "isobxr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `isobxr`.", call. = FALSE)
  }
  suppressWarnings(
    shiny::runApp(appDir,
                  display.mode = "normal",
                  launch.browser = TRUE)
  )
}
