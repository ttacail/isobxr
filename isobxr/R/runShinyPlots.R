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
#' @description A function to call the isobxr plot shiny app
runShinyPlots <- function() {
  appDir <- system.file("shiny-examples", "plot_isobxr", package = "isobxr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
