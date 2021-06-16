#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

LOC_workdir <- workdir

#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#
#----#----#----#---- UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI - UI -
#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#

ui <- fluidPage(tags$style("
              body {
    -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
    zoom: 0.9; /* Other non-webkit browsers */
    zoom: 90%; /* Webkit browsers */
}
              "),


                # SET UI THEME
                theme = shinythemes::shinytheme("darkly"),

                # add close window option
                shinyjs::useShinyjs(debug = FALSE),
                shinyjs::extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }", functions = c("closeWindow")),

                # HEADER
                fluidRow(
                  # with directory button
                  column(width = 2,
                         shinyFiles::shinyDirButton("dir", "Select Series directory", "Upload"), # ALL
                         em(textOutput(outputId = "SeriesType")) # ALL
                  ),
                  # APP title
                  column(width = 8,
                         titlePanel(h1("Shinobxr: isobxr plot editor", align = "center"), windowTitle = "shinobxr")),
                  # Close button
                  column(width = 2,
                         align = "right",
                         actionButton("close", "Quit app")
                  )
                ),

                hr(),

                # TABS
                tabsetPanel(id = "tabs",
                            # PLOT TAB
                            tabPanel("Plots", value = 1,
                                     conditionalPanel(condition = 'output.identified_series == "YES"',
                                                      plotOutput("PLOT", height = 600),
                                                      actionButton("downloadPLOT", "Download pdf")
                                     )
                            ),
                            # CPS summary TAB
                            tabPanel("Composite model summary", value = 2,
                                     fluidRow(
                                       h3("Summary table of composite scenario", align = 'center'),
                                       br(),
                                       column(width = 12, align = "center",
                                              DT::dataTableOutput("summary_table",  width = 1200)
                                       )),
                                     hr(),
                                     fluidRow(
                                       h3("Model diagrams of fluxes and isotope fractionation coefficients", align = 'center'),
                                       br(),
                                       uiOutput("DIAG_RUN_loc"), # CPS,
                                       br(),
                                       column(width = 6, plotOutput("DIAGRAM_F", height = 400, width = 400), align = "center"),
                                       column(width = 6, plotOutput("DIAGRAM_A", height = 400, width = 400), align = "center")),
                                     hr(),
                                     fluidRow(
                                       br(),
                                       column(width = 12, align = "center",
                                              conditionalPanel(condition = 'output.CPS_PLOT_REPORT_yn == "YES"',
                                                               h3("Flux intensities and masses of element X as a function of time (varying values only)", align = "center"),
                                                               plotOutput("CPS_PLOT_REPORT", width = 1200),
                                                               actionButton("download_CPS_REPORT_PLOT", "Download pdf")),
                                              conditionalPanel(condition = 'output.CPS_PLOT_REPORT_yn == "NO"',
                                                               h3("This composite scenario does not involve any changing box sizes and/or flux intensities.
                                                           There is thus no plot of flux and masses as a function of time.", style = "color:grey"))
                                       )
                                     )
                            )
                ),


                hr(),

                # OPTIONS BELOW
                fluidRow(
                  column(width = 3,
                         uiOutput("HIDE_BOX_CPS"), # CPS
                         uiOutput("SHOW_RUNS"),    # CPS
                         uiOutput("PICK_BOX_STD_DYN"),          # STD DYN
                         uiOutput("PICK_BOX_STD_DYN_SUB"),      # STD DYN
                         uiOutput("DISPLAY_DRIFT"),             # DYN
                         uiOutput("TIME_ZOOM")                  # DYN
                  ),
                  column(width = 6,
                         fluidRow(
                           column(width = 3,
                                  uiOutput("PICK_X"),                     # DYN or STD
                                  uiOutput("PICK_Y"),                     # DYN or STD
                                  uiOutput("TIME_UNIT_in"),               # DYN or CPS
                                  uiOutput("HIDE_VALUES_VAR_EXPLO_1")    # DYN

                           ),
                           column(width = 3,
                                  uiOutput("PICK_X_norm"),                # DYN or STD,
                                  uiOutput("PICK_Y_norm"),                # DYN or STD,
                                  uiOutput("TIME_UNIT_out"),              # DYN or CPS,
                                  uiOutput("HIDE_VALUES_VAR_EXPLO_2")    # DYN
                           )
                         )
                  ),
                  column(width = 3,
                         uiOutput("DISPLAY_CONTOUR"),            # STD or DYN
                         uiOutput("BINWIDTH_CONTOUR"),            # STD
                         uiOutput("TIME_MAP_DYN")
                  )
                ),

                hr(),

                # INTRODUCTION
                fluidRow(
                  br(),
                  h3("Welcome to the isobxr plot editor for composite and sweep runs"),
                  p("This app will allow you to interactively display and export the output plots of the",
                    strong("isobxr"),
                    "package."),
                  p("Here, you need to select the composite or sweep output directory you want to plot. It's name should to start with one of these prefixes: "),
                  strong("[3_CPS]"), em("   for composite_isobxr runs"), br(),
                  strong("[4_STD]"), em("   for sweep_steady runs"), br(),
                  strong("[4_DYN]"), em("   for sweep_dyn runs"), br(),
                  br(),
                  br(),

                  p("Selecting a",
                    strong("composite run", style = "color:grey"),
                    "output will allow you to plot the evolution of isotope compositions of one or several boxes of your choice.
                                     You will also have the possility to zoom in the time frame of your choice.
                                     By default, the first (relaxation) run is not shown."),
                  br(),
                  br(),
                  p("Selecting a",
                    strong("sweep steady run", style = "color:grey"),
                    "output will allow you to map the isotope composition of a given box at the final state of the sweep_steady run,
                                     in the 2D-space of the two sweeped parameters (left plot). You can also chose a custom space of parameters (or ratios of numerical parameters)
                                     that might be dependent with the sweep parameters (right plot). Finally, you can fine tune a series of parameters (map the difference between isotope compositions of two distinct boxes, add contours to the maps, ...)."),
                  br(),
                  br(),

                  p("Selecting a",
                    strong("sweep dynamic run", style = "color:grey"),
                    "output will allow you to plot the isotope composition of a given box at in reaction to a perturbation (with time),
                                     in the 2D-space of the two sweeped parameters (left plot).
                                     This is either shown in raw values or as the drift from the initial value.
                                     You can also see (on the right side) the map of isotope compositions of this box in the 2D space of sweep parameters."),
                  br(),
                  br(),

                  p("You can use the ",
                    strong("[Download pdf]", style = "color:grey"),
                    "button to export the graph with the display parameters you like. This pdf will be exported direclty to the selected output directory, and it's name will start with",
                    strong("[00_]"), "."),
                  br(),
                  br(),

                  em("NOTE: This app does not allow to plot single runs (starting with '2_RUN').", style = "color:grey"),
                  br(),
                  br()
                )
                ,

                hr(),

                # ours
                fluidRow(
                  p("This shiny app was developed for the",
                    a("isobxr", href = "https://github.com/ttacail/isobxr"),
                    "package by Théo Tacail (",
                    a("ttacail/github.io", href = "https://ttacail.github.io/"),
                    ").", br(),
                    "This app is offline, locally run on your computer. It therefore does not share any of your data with online servers.",
                    style = "color:grey")
                )
)

#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#
#----#----#----#---- SERVER - SERVER - SERVER - SERVER - SERVER - SERVER - SERVER - SERVER - SERVER - SERVER - SERVER -
#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#
server <- function(input, output) {

  observeEvent(input$close, {
    shinyjs::js$closeWindow()
    stopApp()
  })

  #************************************** DEFINE SERIES DIRECTORY
  output$CurrentDir <- renderText(expr = paste("Current series: ", SERIES_RUN_FILE_ID(), sep = ""))

  output$SeriesType <- renderText({
    if (is.null(SERIES_TYPE())){
      shiny::hideTab(inputId = "tabs", target = "2", session = getDefaultReactiveDomain())
      expr = "No Series folder identified yet."
    } else {
      expr = paste("Series Type: ", paste(SERIES_TYPE(), collapse = ", "), sep = "")
    }
  })

  shinyFiles::shinyDirChoose(
    input,
    'dir',
    roots = c(home = normalizePath(LOC_workdir, winslash = "/")),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )

  global <- reactiveValues(datapath = normalizePath(getwd(), winslash = "/"))

  dir <- reactive(input$dir)

  output$dir <- renderText({
    global$datapath
    print(global$datapath)
  })

  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   # file.path(normalizePath(LOC_workdir, winslash = "/"), paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                   file.path(normalizePath(LOC_workdir, winslash = "/"), paste(unlist(dir()$path[-1]), collapse = "/"))
               })

  SERIES_RUN_dir <- reactive({global$datapath})

  SERIES_RUN_FILE_ID <- reactive({ strsplit(SERIES_RUN_dir(),split = "/")[[1]][length(strsplit(SERIES_RUN_dir(),split = "/")[[1]])] })

  #************************************** DEFINE SERIES TYPE
  SERIES_RUN_FILE_ID_decomp <- reactive({ strsplit(SERIES_RUN_FILE_ID(), split = "_")[[1]] })

  output$identified_series <- reactive({
    if (is.null(SERIES_TYPE())){
      identified_series <- "NO"
      return("NO")
    } else {
      identified_series <- "YES"
      return("YES")
    }
  })

  outputOptions(output, "identified_series", suspendWhenHidden = FALSE)

  SERIES_TYPE <- reactive({
    if (SERIES_RUN_FILE_ID_decomp()[1] == "2"){
      SERIES_TYPE_loc = "SINGLE_RUN"
    } else if (SERIES_RUN_FILE_ID_decomp()[1] == "3"){
      SERIES_TYPE_loc = "CPS"
    } else if (SERIES_RUN_FILE_ID_decomp()[1] == "4"){
      SERIES_TYPE_loc = "SWEEP"
      if (SERIES_RUN_FILE_ID_decomp()[2] == "DYN"){
        SWEEP_TYPE = "DYN"
      } else if (SERIES_RUN_FILE_ID_decomp()[2] == "STD"){
        SWEEP_TYPE = "STD"
      }
    }
    if (exists("SWEEP_TYPE")){
      SERIES_TYPE <- c(SERIES_TYPE_loc, SWEEP_TYPE)
      return(SERIES_TYPE)
    } else if(exists("SERIES_TYPE_loc")){
      SERIES_TYPE <- SERIES_TYPE_loc
      return(SERIES_TYPE)
    } else {
      return(NULL)
    }
  })

  # SHOW/HIDE TABS of COMPL INFORMATION
  observeEvent(eventExpr = {SERIES_TYPE()},
               handlerExpr = {
                 if (SERIES_TYPE()[1] == "CPS"){
                   shiny::showTab(inputId = "tabs", target = "2", session = getDefaultReactiveDomain())
                 } else {
                   shiny::hideTab(inputId = "tabs", target = "2", session = getDefaultReactiveDomain())
                 }
               })

  #************************************** LOAD and PREPARE DATA
  SERIES_RUN_ID <- reactive({
    if (is.null(SERIES_TYPE())){
      return(NULL)
    }
    if (SERIES_TYPE()[1] == "CPS"){
      paste(SERIES_RUN_FILE_ID_decomp()[!SERIES_RUN_FILE_ID_decomp() %in% c("3", "CPS")], collapse = "_")
    } else if (SERIES_TYPE()[1] == "SWEEP"){
      if (SERIES_TYPE()[2] == "STD"){
        paste(SERIES_RUN_FILE_ID_decomp()[!SERIES_RUN_FILE_ID_decomp() %in% c("4", "STD")], collapse = "_")
      } else if (SERIES_TYPE()[2] == "DYN"){
        paste(SERIES_RUN_FILE_ID_decomp()[!SERIES_RUN_FILE_ID_decomp() %in% c("4", "DYN")], collapse = "_")
      }
    } else {
      return(NULL)
    }
  })

  LOG_SERIES <- reactive({
    if (is.null(SERIES_TYPE())){
      return(NULL)
    }
    if (SERIES_TYPE()[1] == "CPS"){
      dir_LOG_SERIES <- paste(SERIES_RUN_dir(), "/0_CPS_DIGEST/", "CPS_", SERIES_RUN_ID(), "_LOG.csv", sep = "")
      data.table::fread(dir_LOG_SERIES, data.table = F, stringsAsFactors = T)
    } else if (SERIES_TYPE()[1] == "SWEEP"){
      if (SERIES_TYPE()[2] == "STD"){
        dir_LOG_SERIES <- paste(SERIES_RUN_dir(), "/0_STD_DIGEST/", "STD_", SERIES_RUN_ID(), "_LOG.csv", sep = "")
        data.table::fread(dir_LOG_SERIES, data.table = F, stringsAsFactors = T)
      } else if (SERIES_TYPE()[2] == "DYN"){
        dir_LOG_SERIES <- paste(SERIES_RUN_dir(), "/0_DYN_DIGEST/", "DYN_", SERIES_RUN_ID(), "_LOG.csv", sep = "")
        data.table::fread(dir_LOG_SERIES, data.table = F, stringsAsFactors = T)
      }
    } else {
      return(NULL)
    }

  })

  evD <- reactive({
    if (is.null(SERIES_TYPE())){
      return(NULL)
    }
    if (SERIES_TYPE()[1] == "CPS"){
      dir_evD_loc <- paste(SERIES_RUN_dir(), "/0_CPS_DIGEST/", "CPS_", SERIES_RUN_ID(), "_evD.RDS", sep = "")
      # data.table::fread(dir_evD_loc, data.table = F, stringsAsFactors = T)
      # readRDS(dir_evD_loc)
      dplyr::mutate_if(readRDS(dir_evD_loc), is.character, as.factor)
    } else if (SERIES_TYPE()[1] == "SWEEP"){
      if (SERIES_TYPE()[2] == "STD"){
        return(NULL)
        # dir_evD_loc <- paste(SERIES_RUN_dir(), "/", "0_STD_", SERIES_RUN_ID(), "_evD.csv", sep = "")
        # data.table::fread(dir_evD_loc, data.table = F, stringsAsFactors = T)
      } else if (SERIES_TYPE()[2] == "DYN"){
        dir_evD_loc <- paste(SERIES_RUN_dir(), "/0_DYN_DIGEST/", "DYN_", SERIES_RUN_ID(), "_evD.RDS", sep = "")
        # data.table::fread(dir_evD_loc, data.table = F, stringsAsFactors = T)
        dplyr::mutate_if(readRDS(dir_evD_loc), is.character, as.factor)
      }
    } else {
      return(NULL)
    }
  })

  evD_final <- reactive({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD"){
      dir_evD_final_loc <- paste(SERIES_RUN_dir(), "/0_STD_DIGEST/", "STD_", SERIES_RUN_ID(), "_evD_final.RDS", sep = "")
      # data.table::fread(dir_evD_final_loc, data.table = F, stringsAsFactors = T)
      # readRDS(dir_evD_final_loc)
      dplyr::mutate_if(readRDS(dir_evD_final_loc), is.character, as.factor)
      # print("evD_final loaded")
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
      # return(NULL)
      dir_evD_final_loc <- paste(SERIES_RUN_dir(), "/0_DYN_DIGEST/", "DYN_", SERIES_RUN_ID(), "_evD_final.RDS", sep = "")
      # data.table::fread(dir_evD_final_loc, data.table = F, stringsAsFactors = T)
      # readRDS(dir_evD_final_loc)
      dplyr::mutate_if(readRDS(dir_evD_final_loc), is.character, as.factor)
    } else {
      return(NULL)
    }
  })

  evS <- reactive({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "CPS"){
      dir_evS_loc <- paste(SERIES_RUN_dir(), "/0_CPS_DIGEST/", "CPS_", SERIES_RUN_ID(), "_evS.RDS", sep = "")
      # data.table::fread(dir_evS_loc, data.table = F, stringsAsFactors = T)
      # readRDS(dir_evS_loc)
      dplyr::mutate_if(readRDS(dir_evS_loc), is.character, as.factor)
    } else {
      return(NULL)
    }
  })

  ALL_BOXES <- reactive({
    strsplit(as.character(LOG_SERIES()$BOXES_ID_list[1]), split = "_")[[1]]
  })

  DISCONNECTED_BOXES <- reactive({
    DISCONNECTED_BOXES <- strsplit(as.character(LOG_SERIES()$DISCONNECTED_BOXES[1]),split = "_")[[1]]
    # if(is.null(DISCONNECTED_BOXES) == FALSE | length(DISCONNECTED_BOXES) != 0 | !is.na(DISCONNECTED_BOXES)){
    #   DISCONNECTED_BOXES <- DISCONNECTED_BOXES
    # } else {
    #   DISCONNECTED_BOXES <- NULL
    # }
    DISCONNECTED_BOXES
  })

  output$HIDE_BOX_CPS <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "CPS"){
      selectInput(inputId = "HIDE_BOX_CPS",
                  label =  "Select boxes to hide",
                  multiple = T,
                  choices = ALL_BOXES()[!ALL_BOXES() %in% DISCONNECTED_BOXES()]
      )
    } else {
      return(NULL)
    }
  })

  output$SHOW_RUNS <- renderUI({
    if (is.null(SERIES_TYPE())){
      return(NULL)
    } else if (SERIES_TYPE()[1] == "CPS") {
      sliderInput(inputId = "SHOW_RUNS",
                  label = "Select the runs to show",
                  value = c(2,max(LOG_SERIES()$RUN_n)),
                  min = 1,
                  max = max(LOG_SERIES()$RUN_n),
                  step = 1)

    } else {
      return(NULL)
    }
  })

  output$PICK_BOX_STD_DYN <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD" | SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN" ){
      selectInput(inputId = "PICK_BOX_STD_DYN",
                  label =  "Box \u03B4",
                  multiple = F,
                  choices = ALL_BOXES()[!ALL_BOXES() %in% DISCONNECTED_BOXES()],
                  selected = ALL_BOXES()[!ALL_BOXES() %in% DISCONNECTED_BOXES()][1])
    } else {
      return(NULL)
    }
  })

  output$PICK_BOX_STD_DYN_SUB <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD" | SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN" ){
      selectInput(inputId = "PICK_BOX_STD_DYN_SUB",
                  label =  "Subtract box \u03B4",
                  multiple = F,
                  choices = c("0", ALL_BOXES()[!ALL_BOXES() %in% DISCONNECTED_BOXES()]),
                  selected = "0")
    } else {
      return(NULL)
    }
  })

  output$TIME_UNIT_in <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN" | SERIES_TYPE()[1] == "CPS"){
      all_time_units <- c("micros", "ms", "s", "min", "h", "d", "wk", "mo", "yr", "kyr", "Myr", "Gyr")
      selectInput(inputId = "TIME_UNIT_in",
                  label = "t unit in",
                  choices = all_time_units,
                  multiple = F,
                  selected = "d")
    } else {
      return(NULL)
    }
  })

  output$TIME_UNIT_out <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN" | SERIES_TYPE()[1] == "CPS"){
      all_time_units <- c("micros", "ms", "s", "min", "h", "d", "wk", "mo", "yr", "kyr", "Myr", "Gyr")
      selectInput(inputId = "TIME_UNIT_out",
                  label = "t unit out",
                  choices = all_time_units,
                  multiple = F,
                  selected = "d")
    } else {
      return(NULL)
    }
  })

  output$PICK_X <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD" | SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN" ){
      if (SERIES_TYPE()[2] == "DYN"){
        colnames <- names(evD())
        colnames <- colnames[!colnames %in% c(ALL_BOXES(), "Time", "SERIES_RUN_ID", "RUN_n", "LEGEND_EXPLO_1", "LEGEND_EXPLO_2")]
      }
      if (SERIES_TYPE()[2] == "STD"){
        colnames <- names(evD_final())
        colnames <- colnames[!colnames %in% c(ALL_BOXES(), "Time", "SERIES_RUN_ID", "RUN_n", "LEGEND_EXPLO_1", "LEGEND_EXPLO_2")]
        if (LOG_SERIES()[2,"EXPLO_TYPES_1"] == "EXPLO_1_RAYLEIGH_ALPHA"){
          Rayleigh_description <- strsplit(as.character(LOG_SERIES()[2,"LEGEND_EXPLO_1"]), split = "_")[[1]]
          Rayleigh_description_len <- length(Rayleigh_description)
          Rayleigh_apparent_alpha_ID <- paste("a", Rayleigh_description[Rayleigh_description_len-1], Rayleigh_description[Rayleigh_description_len], sep = "_")
          colnames <- colnames[!colnames %in% c(Rayleigh_apparent_alpha_ID)]
        }
        if (LOG_SERIES()[2,"EXPLO_TYPES_2"] == "EXPLO_1_RAYLEIGH_ALPHA"){
          Rayleigh_description <- strsplit(as.character(LOG_SERIES()[2,"LEGEND_EXPLO_2"]), split = "_")[[1]]
          Rayleigh_description_len <- length(Rayleigh_description)
          Rayleigh_apparent_alpha_ID <- paste("a", Rayleigh_description[Rayleigh_description_len-1],Rayleigh_description[Rayleigh_description_len], sep = "_")
          colnames <- colnames[!colnames %in% c(Rayleigh_apparent_alpha_ID)]
        }
      }
      selectInput(inputId = "PICK_X",
                  label = "X numerator",
                  choices = c(colnames),
                  selected = c(colnames)[1],# colnames[length(colnames)],
                  multiple = F)
    } else {
      return(NULL)
    }
  })

  output$PICK_Y <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD" | SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN" ){
      if (SERIES_TYPE()[2] == "DYN"){
        colnames <- names(evD())
        colnames <- colnames[!colnames %in% c(ALL_BOXES(), "Time", "SERIES_RUN_ID", "RUN_n", "LEGEND_EXPLO_1", "LEGEND_EXPLO_2")]
      }

      if (SERIES_TYPE()[2] == "STD"){
        colnames <- names(evD_final())
        colnames <- colnames[!colnames %in% c(ALL_BOXES(), "Time", "SERIES_RUN_ID", "RUN_n", "LEGEND_EXPLO_1", "LEGEND_EXPLO_2")]
        if (LOG_SERIES()[2,"EXPLO_TYPES_1"] == "EXPLO_1_RAYLEIGH_ALPHA"){
          Rayleigh_description <- strsplit(as.character(LOG_SERIES()[2,"LEGEND_EXPLO_1"]), split = "_")[[1]]
          Rayleigh_description_len <- length(Rayleigh_description)
          Rayleigh_apparent_alpha_ID <- paste("a", Rayleigh_description[Rayleigh_description_len-1], Rayleigh_description[Rayleigh_description_len], sep = "_")
          colnames <- colnames[!colnames %in% c(Rayleigh_apparent_alpha_ID)]
        }
        if (LOG_SERIES()[2,"EXPLO_TYPES_2"] == "EXPLO_1_RAYLEIGH_ALPHA"){
          Rayleigh_description <- strsplit(as.character(LOG_SERIES()[2,"LEGEND_EXPLO_2"]), split = "_")[[1]]
          Rayleigh_description_len <- length(Rayleigh_description)
          Rayleigh_apparent_alpha_ID <- paste("a", Rayleigh_description[Rayleigh_description_len-1],Rayleigh_description[Rayleigh_description_len], sep = "_")
          colnames <- colnames[!colnames %in% c(Rayleigh_apparent_alpha_ID)]
        }
      }
      selectInput(inputId = "PICK_Y",
                  label = "Y numerator",
                  choices = c(colnames),
                  selected = c(colnames)[2], # colnames[length(colnames)-1],
                  multiple = F)
    } else {
      return(NULL)
    }
  })

  output$PICK_X_norm <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD" | SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN" ){
      if (SERIES_TYPE()[2] == "DYN"){
        colnames <- c("1",names(evD()))
        colnames <- colnames[!colnames %in% c(ALL_BOXES(), "Time", "SERIES_RUN_ID", "RUN_n", "LEGEND_EXPLO_1", "LEGEND_EXPLO_2")]
      }

      if (SERIES_TYPE()[2] == "STD"){
        colnames <- c("1",names(evD_final()))
        colnames <- colnames[!colnames %in% c(ALL_BOXES(), "Time", "SERIES_RUN_ID", "RUN_n", "LEGEND_EXPLO_1", "LEGEND_EXPLO_2")]

        if (LOG_SERIES()[2,"EXPLO_TYPES_1"] == "EXPLO_1_RAYLEIGH_ALPHA"){
          Rayleigh_description <- strsplit(as.character(LOG_SERIES()[2,"LEGEND_EXPLO_1"]), split = "_")[[1]]
          Rayleigh_description_len <- length(Rayleigh_description)
          Rayleigh_apparent_alpha_ID <- paste("a", Rayleigh_description[Rayleigh_description_len-1], Rayleigh_description[Rayleigh_description_len], sep = "_")
          colnames <- colnames[!colnames %in% c(Rayleigh_apparent_alpha_ID)]
        }
        if (LOG_SERIES()[2,"EXPLO_TYPES_2"] == "EXPLO_1_RAYLEIGH_ALPHA"){
          Rayleigh_description <- strsplit(as.character(LOG_SERIES()[2,"LEGEND_EXPLO_2"]), split = "_")[[1]]
          Rayleigh_description_len <- length(Rayleigh_description)
          Rayleigh_apparent_alpha_ID <- paste("a", Rayleigh_description[Rayleigh_description_len-1],Rayleigh_description[Rayleigh_description_len], sep = "_")
          colnames <- colnames[!colnames %in% c(Rayleigh_apparent_alpha_ID)]
        }
      }
      selectInput(inputId = "PICK_X_norm",
                  label = "divided by:",
                  choices = c(colnames),
                  selected = c(colnames)[1],
                  multiple = F)
    } else {
      return(NULL)
    }
  })

  output$PICK_Y_norm <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD" | SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN" ){
      if (SERIES_TYPE()[2] == "DYN"){
        colnames <- c("1",names(evD()))
        colnames <- colnames[!colnames %in% c(ALL_BOXES(), "Time", "SERIES_RUN_ID", "RUN_n", "LEGEND_EXPLO_1", "LEGEND_EXPLO_2")]
      }

      if (SERIES_TYPE()[2] == "STD"){
        colnames <- c("1",names(evD_final()))
        colnames <- colnames[!colnames %in% c(ALL_BOXES(), "Time", "SERIES_RUN_ID", "RUN_n", "LEGEND_EXPLO_1", "LEGEND_EXPLO_2")]
        if (LOG_SERIES()[2,"EXPLO_TYPES_1"] == "EXPLO_1_RAYLEIGH_ALPHA"){
          Rayleigh_description <- strsplit(as.character(LOG_SERIES()[2,"LEGEND_EXPLO_1"]), split = "_")[[1]]
          Rayleigh_description_len <- length(Rayleigh_description)
          Rayleigh_apparent_alpha_ID <- paste("a", Rayleigh_description[Rayleigh_description_len-1], Rayleigh_description[Rayleigh_description_len], sep = "_")
          colnames <- colnames[!colnames %in% c(Rayleigh_apparent_alpha_ID)]
        }
        if (LOG_SERIES()[2,"EXPLO_TYPES_2"] == "EXPLO_1_RAYLEIGH_ALPHA"){
          Rayleigh_description <- strsplit(as.character(LOG_SERIES()[2,"LEGEND_EXPLO_2"]), split = "_")[[1]]
          Rayleigh_description_len <- length(Rayleigh_description)
          Rayleigh_apparent_alpha_ID <- paste("a", Rayleigh_description[Rayleigh_description_len-1],Rayleigh_description[Rayleigh_description_len], sep = "_")
          colnames <- colnames[!colnames %in% c(Rayleigh_apparent_alpha_ID)]
        }
      }
      selectInput(inputId = "PICK_Y_norm",
                  label = "divided by:",
                  choices = c(colnames),
                  selected = c(colnames)[1],
                  multiple = F)
    } else {
      return(NULL)
    }
  })

  output$DIAG_RUN_loc <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "CPS"){
      choices_loc <- min(sort(unique(LOG_SERIES()$RUN_n)))
      sliderInput(inputId = "DIAG_RUN_loc",
                  label = "Display Run #",
                  min = min(LOG_SERIES()$RUN_n),
                  max = max(LOG_SERIES()$RUN_n),
                  step = 1,
                  value = 2)
    } else {
      return(NULL)
    }
  })

  output$DISPLAY_CONTOUR <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD" | SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
      checkboxInput(inputId = "DISPLAY_CONTOUR", label = "Display contour", value = F)
    } else {
      return(NULL)
    }
  })

  output$DISPLAY_DRIFT <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
      checkboxInput(inputId = "DISPLAY_DRIFT", label = "Display drift", value = F)
    } else {
      return(NULL)
    }
  })

  output$BINWIDTH_CONTOUR <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD" | SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
      textInput(inputId = "BINWIDTH_CONTOUR", label = "Contour width", value = "0.1")
    } else {
      return(NULL)
    }
  })

  output$HIDE_VALUES_VAR_EXPLO_1 <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
      choices_var_explo_1 <-  as.character(sort(unique(evD()[,"VAR_EXPLO_1"])))
      selectInput(inputId = "HIDE_VALUES_VAR_EXPLO_1",
                  label = "Hide SWEEP #1 values: ",
                  multiple = T,
                  choices = choices_var_explo_1)
    } else {
      return(NULL)
    }
  })

  output$HIDE_VALUES_VAR_EXPLO_2 <- renderUI({
    if (is.null(SERIES_TYPE())) {
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
      choices_var_explo_1 <- as.character(sort(unique(evD()[,"VAR_EXPLO_2"])))
      selectInput(inputId = "HIDE_VALUES_VAR_EXPLO_2",
                  label = "Hide SWEEP #2 values: ",
                  multiple = T,
                  choices = choices_var_explo_1)
    } else {
      return(NULL)
    }
  })

  output$TIME_ZOOM <- renderUI({
    if (is.null(SERIES_TYPE())){
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN") {
      sliderInput(inputId = "TIME_ZOOM",
                  label = "Time range (%)",
                  value = c(0,100),
                  min = 0,
                  max = 100,
                  step = 1)
    } else {
      return(NULL)
    }
  })

  output$TIME_MAP_DYN <- renderUI({
    if (is.null(SERIES_TYPE())){
      return(NULL)
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN") {
      sliderInput(inputId = "TIME_MAP_DYN",
                  label = "Time at mapping (%)",
                  value = 100,
                  min = 0,
                  max = 100,
                  step = 1)
    } else {
      return(NULL)
    }
  })

  CONSTANTS <- reactive({
    dir_INPUT_init <- paste(SERIES_RUN_dir(), "/", as.character(LOG_SERIES()[1,"SERIES_RUN_ID"]), "_IN.Rda", sep = "")
    # as.data.frame(readxl::read_excel(dir_INPUT_init, "CONSTS"))
    load(dir_INPUT_init)
    CONSTS_IN
  })

  ELEMENT <- reactive({
    as.character(CONSTANTS()[CONSTANTS()$CONSTS_ID == "Element", "CONSTS"])
  })

  ISO_NUMERATOR <-  reactive({
    as.numeric(CONSTANTS()[CONSTANTS()$CONSTS_ID == "Numerator", "CONSTS"])
  })

  ISO_DENOMINATOR <- reactive({
    as.numeric(CONSTANTS()[CONSTANTS()$CONSTS_ID == "Denominator", "CONSTS"])
  })

  #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# PLOT functions #----

  PLOT_CPS <- reactive({
    if (is.null(SERIES_TYPE())){
      return()
    } else if (SERIES_TYPE()[1] == "CPS"){
      #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# COMPOSITE PLOT

      if (input$SHOW_RUNS[1] == 1 & input$SHOW_RUNS[2] == max(LOG_SERIES()$RUN_n)){
        plot_HIDE_RUNs_n <- NULL
      } else {
        all_runs <- seq(1,max(LOG_SERIES()$RUN_n), by = 1)
        show_runs <- seq(input$SHOW_RUNS[1], input$SHOW_RUNS[2], by = 1)
        plot_HIDE_RUNs_n <- all_runs[!all_runs %in% show_runs]
      }

      time_units <- c(input$TIME_UNIT_in, input$TIME_UNIT_out)

      if (is.null(input$HIDE_BOX_CPS)){
        # if (is.null(DISCONNECTED_BOXES()) | is.na(DISCONNECTED_BOXES())){
        if (is.na(DISCONNECTED_BOXES())){
          plot_HIDE_BOXES <- NULL
        } else {
          plot_HIDE_BOXES <- c(NULL, DISCONNECTED_BOXES())
        }
      } else {
        # if (is.null(DISCONNECTED_BOXES()) | is.na(DISCONNECTED_BOXES())){
        if (is.na(DISCONNECTED_BOXES())){
          plot_HIDE_BOXES <- input$HIDE_BOX_CPS
        } else {
          plot_HIDE_BOXES <- c(input$HIDE_BOX_CPS, DISCONNECTED_BOXES())
        }
      }


      #************************************** PLOT evD
      #### subset evD for plot (hide the first plot_HIDE_RUNs_n runs)
      if (is.null(plot_HIDE_RUNs_n)){
        evD<- evD()
      } else {
        evD <- evD()[-which(evD()$RUN_n %in% plot_HIDE_RUNs_n), ]
      }

      #### reset time for plot (Time_plot)
      evD$Time_plot_i <- evD$Time_COMPOSITE - min(evD$Time_COMPOSITE)
      evD <- clear_subset(evD)

      #### reset time units
      initial_time_unit <- time_units[1]
      display_time_unit <- time_units[2]
      evD <- time_converter(dataframe <- evD,
                            time_colname <- "Time_plot_i",
                            conv_timecolname <- "Time_plot",
                            former_unit <- initial_time_unit, # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"
                            new_unit <- display_time_unit)


      #### extract composite sub-runs (zones)
      k <- 1
      SERIES_RUN_ID_plot <- sort(unique(evD$RUN_n))
      for (k in 1:length(SERIES_RUN_ID_plot)){4
        min_time_loc <- min(evD[evD$RUN_n == SERIES_RUN_ID_plot[k], "Time"])
        if (k == 1){
          evD_zones <- evD[evD$RUN_n == SERIES_RUN_ID_plot[k] & evD$Time == min_time_loc, ]
        } else {
          evD_zones <- rbind(evD_zones, evD[evD$RUN_n == SERIES_RUN_ID_plot[k] & evD$Time == min_time_loc, ])
        }
        k <- k + 1
      }

      #### verticalize evD
      evD_zones_vert <- DF_verticalizer(df_hor = evD_zones, vert_col = ALL_BOXES())
      evD_vert <- DF_verticalizer(df_hor = evD, vert_col = ALL_BOXES())

      # #### hide unwanted boxes for delta plot
      if (is.null(plot_HIDE_BOXES) == FALSE){
        evD_zones_vert <- evD_zones_vert[-which(evD_zones_vert$VAR_TYPE %in% plot_HIDE_BOXES), ]
        evD_zones_vert <- clear_subset(evD_zones_vert)
        evD_vert <- evD_vert[-which(evD_vert$VAR_TYPE %in% plot_HIDE_BOXES), ]
        evD_vert <- clear_subset(evD_vert)
      }

      #### set limits of plot
      Ymin <- round(min(evD_vert$VAR), 0)-1
      Ymax <- round(max(evD_vert$VAR), 0)+1
      Ymin_zoom <- min(evD_vert$VAR) - 0.1*Ymax
      Ymax_zoom <- max(evD_vert$VAR) + 0.2*Ymax

      # Ybin <- 0.2
      Ybin <- signif((Ymax-Ymin)/10, digits = 1) # automatic definition of Ybin
      Xmax <- max(evD_vert$Time_plot) + 0.1*max(evD_vert$Time_plot)

      #### extract t0 and t_final delta values
      evD_initial <- evD_vert[evD_vert$Time_plot == min(evD_vert$Time_plot),]
      evD_final <- evD_vert[evD_vert$Time_plot == max(evD_vert$Time_plot),]

      Plot_title <- paste(SERIES_RUN_ID(), " (", min(LOG_SERIES()$RUN_n), "-", max(LOG_SERIES()$RUN_n), ")", sep = "")
      if (is.null(plot_HIDE_RUNs_n)){
        Sub_title <- "No hiden run."
      } else {
        Sub_title <- paste("Initial hidden runs num.: ", paste(LOG_SERIES()[plot_HIDE_RUNs_n, "RUN_n"], collapse = ", "), sep = "")
      }

      if (is.null(plot_HIDE_BOXES) == FALSE){
        Sub_title <- paste(Sub_title, " - Hidden boxes: ", paste(plot_HIDE_BOXES, collapse = ", "))
      }

      #### plot delta evol
      evD_plot <- ggplot2::ggplot(data = evD_vert, ggplot2::aes(x = Time_plot, y = VAR, color = VAR_TYPE))+
        ggplot2::geom_line(cex = 1)+
        ggplot2::theme_bw()+
        ggplot2::scale_y_continuous(limits=c(Ymin, Ymax), breaks=seq(Ymin, Ymax, by = Ybin), labels = dec_2)+
        ggplot2::coord_cartesian(ylim = c(Ymin_zoom, Ymax_zoom + 0.05*Ymax_zoom), xlim = c(0, Xmax))+
        ggplot2::labs(y = paste("\u03B4", ELEMENT()," (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(), ", \u2030)",  sep = ""),
                      x = paste("Time in", display_time_unit, sep = " "),
                      title = Plot_title,
                      subtitle = Sub_title) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = 16, face = "bold", margin = ggplot2::margin(b = 20, unit = "pt")),
                       plot.subtitle = ggplot2::element_text(hjust = 0, size = 12, face = "italic"),
                       axis.text = ggplot2::element_text(size = 12, color = "black", margin = ggplot2::margin(b = 5, unit = "pt")),
                       axis.title = ggplot2::element_text(size = 16, color = "black", margin = ggplot2::margin(b = 60, unit = "pt"))
        )+
        ggrepel::geom_text_repel(data = evD_final, ggplot2::aes(label = paste(VAR_TYPE, " (", dec_2(VAR), ")",  sep = ""), color = VAR_TYPE), nudge_x = 0.05*max(evD_vert$Time_plot),  hjust = 0)
      # geom_text_repel(data = evD_zones_vert, ggplot2::aes(label = paste(" (", dec_2(VAR), ")",  sep = ""), color = VAR_TYPE))
      # evD_plot

      #### annotate zones
      if (is.null(plot_HIDE_RUNs_n)){
        plot_HIDE_RUNs_n_loc <- 0
      } else {
        plot_HIDE_RUNs_n_loc <- plot_HIDE_RUNs_n
      }

      k <- 1
      for (k in 1:length(evD_zones$RUN_n)){
        evD_plot <- evD_plot +
          ggplot2::geom_vline(xintercept = evD_zones[k, "Time_plot"] ,  linetype = 2) +
          ggplot2::annotate(geom = "text", x = evD_zones[k, "Time_plot"], y = Ymax_zoom + 0.1*Ymax_zoom,
                            label = paste("  Run #: ", LOG_SERIES()[LOG_SERIES()$RUN_n == evD_zones$RUN_n[k], "RUN_n"], sep = ""), hjust = 0)+
          ggplot2::annotate(geom = "text", x = evD_zones[k, "Time_plot"], y = Ymax_zoom + 0.0*Ymax_zoom,
                            label = paste("  ", LOG_SERIES()[LOG_SERIES()$RUN_n == evD_zones$RUN_n[k], "COEFF_RUN"], sep = ""), hjust = 0)+
          ggplot2::annotate(geom = "text", x = evD_zones[k, "Time_plot"], y = Ymax_zoom - 0.1*Ymax_zoom,
                            label = paste("  ", LOG_SERIES()[LOG_SERIES()$RUN_n == evD_zones$RUN_n[k], "FLUX_MASTER"], sep = ""), hjust = 0)
        k <- k + 1
      }

      evD_plot

    } else {
      return()
    }
  })

  PLOT_SWEEP_STD <- reactive({
    if (is.null(SERIES_TYPE())){
      return()
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD"){
      #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# STEADY MAP PLOT


      ########################################
      SERIES_RUN_ID <- SERIES_RUN_ID() # "EXPLO_rKDN_vs_aECFBONE_w_Fx3_BL50"
      loc_BOX <- input$PICK_BOX_STD_DYN
      LOG_EXPLO <- LOG_SERIES()
      evD_final <- evD_final()

      X <- "VAR_EXPLO_1"
      # X_div <- NULL
      # # X_div <- "f_ECF_GIT"

      Y <-  "VAR_EXPLO_2"
      # Y_div <- NULL
      # # Y_div <- "a_ECF_BONE"

      # DROP RUN 1
      evD_final <- clear_subset(evD_final[evD_final$RUN_n != 1, ])

      # VERTICALIZE
      evD_final_vert <- DF_verticalizer(df_hor = evD_final, vert_col = ALL_BOXES())

      DF <- evD_final_vert

      if (levels(DF$LEGEND_EXPLO_1) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
        EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", levels(DF$VAR_EXPLO_1)[1], " to ", levels(DF$VAR_EXPLO_1)[length(levels(DF$VAR_EXPLO_1))], sep = ""), sep = "")
      } else {
        EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", paste(c(min(DF$VAR_EXPLO_1), max(DF$VAR_EXPLO_1)), collapse = " to "), collapse = ", "), sep = "")
      }

      if (levels(DF$LEGEND_EXPLO_2) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
        EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(levels(DF$VAR_EXPLO_2), collapse = ", "), sep = ""), sep = "")
      } else {
        EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(c(min(DF$VAR_EXPLO_2), max(DF$VAR_EXPLO_2)), collapse = " to "), collapse = ", "), sep = "")
      }

      EXPLO_subtitle_0 = paste(SERIES_RUN_ID(),
                               " (", min(LOG_SERIES()$RUN_n),
                               "-", max(LOG_SERIES()$RUN_n),
                               ") \n", "Hidden initial run: ",
                               paste(LOG_SERIES()[1, "COEFF_FLUX"], collapse = " / "),
                               sep = "")

      EXPLO_subtitle <- paste(EXPLO_subtitle_0, "\n",
                              EXPLO_subtitle_1, "\n",
                              EXPLO_subtitle_2, sep = "")


      if (input$PICK_BOX_STD_DYN_SUB != "0"){
        EXPLO_title <- paste("\u0394", ELEMENT(), " (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(),", \u2030", ") - [", loc_BOX, "-", input$PICK_BOX_STD_DYN_SUB, "]", sep = "")
      } else {
        EXPLO_title <- paste("\u03B4", ELEMENT(), " (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(),", \u2030", ") - [", loc_BOX, "]", sep = "")
      }

      ################################################ PLOT VAR_EXPLO_2 vs VAR_EXPLO_1
      DF_map_1 <- DF

      X_ID <- as.character(evD_final_vert[1,"LEGEND_EXPLO_1"])
      X_ID_legend <- paste(as.character(evD_final_vert[1,"LEGEND_EXPLO_1"]), " (", as.character(evD_final_vert[1,X]), " to ",  as.character(evD_final_vert[nrow(evD_final_vert),X]), ")", sep ="")
      DF_map_1$X_ID <- X_ID
      DF_map_1$X <- DF_map_1[,X]

      Y_ID <- as.character(evD_final_vert[1,"LEGEND_EXPLO_2"])
      Y_ID_legend <- paste(as.character(evD_final_vert[1,"LEGEND_EXPLO_2"]), " (", as.character(evD_final_vert[1,Y]), " to ",  as.character(evD_final_vert[nrow(evD_final_vert),Y]), ")", sep ="")
      DF_map_1$Y_ID <- Y_ID
      DF_map_1$Y <- DF_map_1[,Y]

      remove(evD_final_vert)

      DF_map_1 <- DF_map_1[,c("X_ID", "X", "Y_ID", "Y", "VAR_TYPE", "VAR")]

      names(DF_map_1) <- c("X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")

      DF_map_1_loc <- DF_map_1[DF_map_1$Z_ID == loc_BOX,]

      if (input$PICK_BOX_STD_DYN_SUB != "0"){
        DF_map_1_loc$Z <- DF_map_1_loc$Z - DF_map_1[DF_map_1$Z_ID == input$PICK_BOX_STD_DYN_SUB, "Z"]
        DF_map_1_loc$Z_ID <- paste(DF_map_1_loc$Z_ID, "-", input$PICK_BOX_STD_DYN_SUB, sep = "")
      }

      map_1 <- ggplot2::ggplot(data = DF_map_1_loc, ggplot2::aes(x = as.numeric(X), y = as.numeric(Y), z = Z))+
        ggplot2::geom_raster(ggplot2::aes(fill = Z), hjust=0.5, vjust=0.5, interpolate = F)+
        ggplot2::scale_fill_gradientn(colors = rainbow(200))+
        ggplot2::theme_bw()+
        ggplot2::labs(y = Y_ID_legend,
                      x = X_ID_legend,
                      title = EXPLO_title,
                      subtitle = EXPLO_subtitle)

      if (input$DISPLAY_CONTOUR == T){
        breaks_loc <- metR::MakeBreaks(binwidth = as.numeric(input$BINWIDTH_CONTOUR))
        map_1 <- map_1 +
          metR::geom_contour_fill(ggplot2::aes(z = Z), alpha = 1, breaks = breaks_loc) +
          metR::geom_contour2(ggplot2::aes(z = Z), breaks = breaks_loc) +
          metR::geom_label_contour(ggplot2::aes(z = Z), breaks = breaks_loc)
      }


      ################################################ PLOT VAR_EXPLO_2 vs VAR_EXPLO_1
      DF_map_2 <- DF

      if (as.character(input$PICK_X_norm) != "1"){
        X_ID <- paste(as.character(input$PICK_X), "/", as.character(input$PICK_X_norm), sep = "")
        DF_map_2$X_ID <- X_ID
        DF_map_2$X <- DF_map_2[,input$PICK_X]/DF_map_2[,input$PICK_X_norm]
      } else {
        X_ID <- as.character(input$PICK_X)
        DF_map_2$X_ID <- X_ID
        DF_map_2$X <- DF_map_2[,input$PICK_X]
      }

      if (as.character(input$PICK_Y_norm) != "1"){
        Y_ID <- paste(as.character(input$PICK_Y), "/", as.character(input$PICK_Y_norm), sep = "")
        DF_map_2$Y_ID <- Y_ID
        DF_map_2$Y <- DF_map_2[,input$PICK_Y]/DF_map_2[,input$PICK_Y_norm]
      } else {
        Y_ID <- as.character(input$PICK_Y)
        DF_map_2$Y_ID <- Y_ID
        DF_map_2$Y <- DF_map_2[,input$PICK_Y]
      }

      DF_map_2 <- DF_map_2[,c("X_ID", "X", "Y_ID", "Y", "VAR_TYPE", "VAR")]

      names(DF_map_2) <- c("X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")

      DF_map_2_loc <- DF_map_2[DF_map_2$Z_ID == loc_BOX,]

      if (input$PICK_BOX_STD_DYN_SUB != "0"){
        DF_map_2_loc$Z <- DF_map_2_loc$Z - DF_map_2[DF_map_2$Z_ID == input$PICK_BOX_STD_DYN_SUB, "Z"]
        DF_map_2_loc$Z_ID <- paste(DF_map_2_loc$Z_ID, "-", input$PICK_BOX_STD_DYN_SUB, sep = "")
      }


      map_2 <- ggplot2::ggplot(data = DF_map_2_loc,  ggplot2::aes(x = as.numeric(X), y = as.numeric(Y), z = Z))+
        ggplot2::geom_tile(ggplot2::aes(fill = Z), stat = "identity")+
        # geom_raster(ggplot2::aes(fill = Z), stat = "identity", hjust=0.5, vjust=0.5, interpolate = F)+
        ggplot2::scale_fill_gradientn(colors = rainbow(200))+
        ggplot2::scale_color_gradientn(colors = rainbow(200))+
        # metR::geom_contour2(ggplot2::aes(color = ..level..))+
        ggplot2::theme_bw()+
        ggplot2::labs(y = Y_ID,
                      x = X_ID,
                      title = EXPLO_title,
                      subtitle = EXPLO_subtitle)

      # coord_cartesian(xlim = c(min(DF_plot$X), max(DF_plot$X)), ylim = c(min(DF_plot$Y), max(DF_plot$Y)))

      if (input$DISPLAY_CONTOUR == T){
        breaks_loc <- metR::MakeBreaks(binwidth = as.numeric(input$BINWIDTH_CONTOUR))
        map_2 <- map_2 +
          metR::geom_contour_fill(ggplot2::aes(z = Z), alpha = 1, breaks = breaks_loc) +
          metR::geom_contour2(ggplot2::aes(z = Z), breaks = breaks_loc) +
          metR::geom_label_contour(ggplot2::aes(z = Z), breaks = breaks_loc)
      }

      # map_2
      multiplot(map_1, map_2, cols = 2)

    } else {
      return()
    }
  })

  PLOT_SWEEP_DYN <- reactive({
    if (is.null(SERIES_TYPE())){
      return()
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
      #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# DYN FAN PLOT
      # return()
      # #######################################
      # ARGUMENTS
      SERIES_RUN_ID <- SERIES_RUN_ID()
      loc_BOX <- input$PICK_BOX_STD_DYN
      loc_BOX_sub <- input$PICK_BOX_STD_DYN_SUB
      time_units <- c(input$TIME_UNIT_in, input$TIME_UNIT_out)
      evD <- evD()
      BOXES_IDs <- ALL_BOXES()
      HIDE_VALUES_VAR_EXPLO_1 <- input$HIDE_VALUES_VAR_EXPLO_1
      HIDE_VALUES_VAR_EXPLO_2 <- input$HIDE_VALUES_VAR_EXPLO_2
      TIME_ZOOM <- input$TIME_ZOOM
      DISPLAY_DRIFT <- input$DISPLAY_DRIFT

      min_time <- (0.01*max(evD$Time)*TIME_ZOOM[1])
      max_time <- (0.01*max(evD$Time)*TIME_ZOOM[2])
      time_list <- sort(unique(evD$Time))

      time_of_mapping <- input$TIME_MAP_DYN*0.01*max(time_list)


      if (input$TIME_MAP_DYN < 100){
        time_of_mapping <- time_list[time_list >= time_of_mapping][1]
      }

      if (loc_BOX_sub != "0"){
        Z_ID <- paste("\u0394", ELEMENT(), " [", loc_BOX, "-", loc_BOX_sub,"]", sep = "")
        evD$Z_ID <- Z_ID
        evD$Z <- evD[,loc_BOX] - evD[,loc_BOX_sub]
      } else {
        Z_ID <- paste("\u03B4", ELEMENT(), " [", loc_BOX, "]", sep = "")
        evD$Z_ID <- Z_ID
        evD$Z <- evD[, loc_BOX]
      }

      # CALCULATE DRIFT FROM t0 on Y

      if (DISPLAY_DRIFT == T){
        evD$Z_ID <- paste(evD$Z_ID, " DRIFT from t0", sep = "")
        list_SERIES_RUN_ID <- levels(evD$SERIES_RUN_ID)
        i <- 1
        for (i in 1:length(list_SERIES_RUN_ID)){
          min_T_SERIES_loc <- min(evD[evD$SERIES_RUN_ID == list_SERIES_RUN_ID[i], "Time"])
          evD[evD$SERIES_RUN_ID == list_SERIES_RUN_ID[i], "Z"] <- evD[evD$SERIES_RUN_ID == list_SERIES_RUN_ID[i], "Z"] -
            evD[evD$SERIES_RUN_ID == list_SERIES_RUN_ID[i] & evD$Time == min_T_SERIES_loc, "Z"]
          i <- i + 1
        }
      }


      #### DEFINE X AND Y

      if (as.character(input$PICK_X_norm) != "1"){
        if (input$PICK_X == "VAR_EXPLO_1"){
          X_ID_num <- as.character(evD[1,"LEGEND_EXPLO_1"])
        } else if (input$PICK_X == "VAR_EXPLO_2"){
          X_ID_num <- as.character(evD[1,"LEGEND_EXPLO_2"])
        } else {
          X_ID_num <- as.character(input$PICK_X)
        }
        if (input$PICK_X_norm == "VAR_EXPLO_1"){
          X_ID_denom <- as.character(evD[1,"LEGEND_EXPLO_1"])
        } else if (input$PICK_X_norm == "VAR_EXPLO_2"){
          X_ID_denom <- as.character(evD[1,"LEGEND_EXPLO_2"])
        } else {
          X_ID_denom <- as.character(input$PICK_X_norm)
        }
        X_ID <- paste(X_ID_num, "/", X_ID_denom, sep = "")
        evD$X_ID <- X_ID
        evD$X <- evD[,input$PICK_X]/evD[,input$PICK_X_norm]
      } else {
        if (input$PICK_X == "VAR_EXPLO_1"){
          X_ID <- as.character(evD[1,"LEGEND_EXPLO_1"])
        } else if (input$PICK_X == "VAR_EXPLO_2"){
          X_ID <- as.character(evD[1,"LEGEND_EXPLO_2"])
        } else {
          X_ID <- as.character(input$PICK_X)
        }
        evD$X_ID <- X_ID
        evD$X <- evD[,input$PICK_X]
      }

      if (as.character(input$PICK_Y_norm) != "1"){
        if (input$PICK_Y == "VAR_EXPLO_1"){
          Y_ID_num <- as.character(evD[1,"LEGEND_EXPLO_1"])
        } else if (input$PICK_Y == "VAR_EXPLO_2"){
          Y_ID_num <- as.character(evD[1,"LEGEND_EXPLO_2"])
        } else {
          Y_ID_num <- as.character(input$PICK_Y)
        }
        if (input$PICK_Y_norm == "VAR_EXPLO_1"){
          Y_ID_denom <- as.character(evD[1,"LEGEND_EXPLO_1"])
        } else if (input$PICK_Y_norm == "VAR_EXPLO_2"){
          Y_ID_denom <- as.character(evD[1,"LEGEND_EXPLO_2"])
        } else {
          Y_ID_denom <- as.character(input$PICK_Y_norm)
        }
        Y_ID <- paste(Y_ID_num, "/", Y_ID_denom, sep = "")
        evD$Y_ID <- Y_ID
        evD$Y <- evD[,input$PICK_Y]/evD[,input$PICK_Y_norm]
      } else {
        if (input$PICK_Y == "VAR_EXPLO_1"){
          Y_ID <- as.character(evD[1,"LEGEND_EXPLO_1"])
        } else if (input$PICK_Y == "VAR_EXPLO_2"){
          Y_ID <- as.character(evD[1,"LEGEND_EXPLO_2"])
        } else {
          Y_ID <- as.character(input$PICK_Y)
        }
        evD$Y_ID <- Y_ID
        evD$Y <- evD[,input$PICK_Y]
      }

      # CROP TIME
      DF_map <- evD

      if (is.null(HIDE_VALUES_VAR_EXPLO_1) == F){
        evD <- clear_subset(evD[-which(as.character(evD$VAR_EXPLO_1) %in% as.character(HIDE_VALUES_VAR_EXPLO_1)),])
      }

      if (is.null(HIDE_VALUES_VAR_EXPLO_2) == F){
        evD <- clear_subset(evD[-which(as.character(evD$VAR_EXPLO_2) %in% as.character(HIDE_VALUES_VAR_EXPLO_2)),])
      }
      DF <- clear_subset(evD[evD$Time >= min_time & evD$Time <= max_time,])

      #### DEFINE TITLES
      EXPLO_subtitle_0 = paste(SERIES_RUN_ID(),
                               " (", min(LOG_SERIES()$RUN_n),
                               "-", max(LOG_SERIES()$RUN_n),
                               ") \n", "Hidden initial run: ",
                               paste(LOG_SERIES()[1, "COEFF_FLUX"], collapse = " / "),
                               sep = "")

      if (levels(DF$LEGEND_EXPLO_1) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
        EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", levels(DF$VAR_EXPLO_1)[1], " to ", levels(DF$VAR_EXPLO_1)[length(levels(DF$VAR_EXPLO_1))], sep = ""), sep = "")
      } else {
        EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", paste(c(min(DF$VAR_EXPLO_1), max(DF$VAR_EXPLO_1)), collapse = " to "), collapse = ", "), sep = "")
      }

      if (levels(DF$LEGEND_EXPLO_2) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
        EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(levels(DF$VAR_EXPLO_2), collapse = ", "), sep = ""), sep = "")
      } else {
        EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(c(min(DF$VAR_EXPLO_2), max(DF$VAR_EXPLO_2)), collapse = " to "), collapse = ", "), sep = "")
      }

      EXPLO_subtitle <- paste(EXPLO_subtitle_0, "\n",
                              EXPLO_subtitle_1, "\n",
                              EXPLO_subtitle_2, sep = "")


      if (input$PICK_BOX_STD_DYN_SUB != "0"){
        EXPLO_title <- paste("\u0394", ELEMENT(), " (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(),", \u2030", ") - [", loc_BOX, "-", input$PICK_BOX_STD_DYN_SUB, "]", sep = "")
      } else {
        EXPLO_title <- paste("\u03B4", ELEMENT(), " (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(),", \u2030", ") - [", loc_BOX, "]", sep = "")
      }

      if (DISPLAY_DRIFT == T){
        EXPLO_title <- paste(EXPLO_title, " DRIFT from t0", sep = "")
      }

      DF <- DF[, c("Time", "X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")]

      # if (exists("VAR_EXPLO_1_lims")){
      #   DF <- DF[DF$VAR_EXPLO_1 <= VAR_EXPLO_1_lims[2] & DF$VAR_EXPLO_2 >= VAR_EXPLO_1_lims[1], ]
      #   remove(VAR_EXPLO_1_lims)
      # }
      #
      # if (exists("VAR_EXPLO_2_lims")){
      #   DF <- DF[DF$VAR_EXPLO_2 <= VAR_EXPLO_2_lims[2] & DF$VAR_EXPLO_2 >= VAR_EXPLO_2_lims[1], ]
      #   remove(VAR_EXPLO_2_lims)
      # }

      DF$TIME_OF_MAPPING <- 0
      DF[DF$Time == time_of_mapping,"TIME_OF_MAPPING"] <- 1
      #### reset time units
      initial_time_unit <- time_units[1]
      display_time_unit <- time_units[2]
      DF <- time_converter(dataframe <- DF,
                           time_colname <- "Time",
                           conv_timecolname <- "Time_plot",
                           former_unit <- initial_time_unit, # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"
                           new_unit <- display_time_unit)


      X.labs <- paste(DF[1,"X_ID"], ": ", as.character(sort(unique(DF[,"X"]))), sep = "")
      names(X.labs) <- as.character(sort(unique(DF[,"X"])))

      evD_plot <- ggplot2::ggplot(data = DF,  ggplot2::aes(x = Time_plot, y = Z, color = as.numeric(Y), group = as.numeric(Y)))+
        ggplot2::geom_line()+
        ggplot2::scale_color_gradientn(name = DF[1,"Y_ID"], colors = rainbow(100))+
        ggplot2::facet_grid(. ~ X, labeller = ggplot2::labeller(X = X.labs))+
        ggplot2::theme_bw()+
        ggplot2::theme(strip.text = ggplot2::element_text(size = 12, colour = "white", face = "bold"),
                       strip.background = ggplot2::element_rect(fill = "black"))+
        ggplot2::labs(x = paste("Time (", time_units[2], ")", sep = ""),
                      y = DF[1,"Z_ID"],
                      title = EXPLO_title,
                      subtitle = EXPLO_subtitle)


      if (time_of_mapping <= max_time & time_of_mapping >= min_time){
        evD_plot <- evD_plot + ggplot2::geom_vline(xintercept = DF[DF$TIME_OF_MAPPING == 1, "Time_plot"], linetype = "dashed", color = "black")
      }

      ################################################ MAP PLOT

      DF_map <- DF_map[, c("Time", "X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")]

      DF_map <- DF_map[DF_map$Time == time_of_mapping, ]

      #### reset time units
      initial_time_unit <- time_units[1]
      display_time_unit <- time_units[2]
      DF_map <- time_converter(dataframe <- DF_map,
                               time_colname <- "Time",
                               conv_timecolname <- "Time_plot",
                               former_unit <- initial_time_unit, # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"
                               new_unit <- display_time_unit)

      EXPLO_title <- paste(EXPLO_title, " at time = ", dec_2(DF_map[1, "Time_plot"]), " ", display_time_unit, sep = "")

      map_2 <- ggplot2::ggplot(data = DF_map,  ggplot2::aes(x = as.numeric(X), y = as.numeric(Y), z = Z))+
        ggplot2::geom_raster(ggplot2::aes(fill = Z), stat = "identity", hjust=0.5, vjust=0.5, interpolate = F)+
        ggplot2::scale_fill_gradientn(colors = rainbow(200))+
        ggplot2::scale_color_gradientn(colors = rainbow(200))+
        # metR::geom_contour2(ggplot2::aes(color = ..level..))+
        ggplot2::theme_bw()+
        ggplot2::labs(y = Y_ID,
                      x = X_ID,
                      title = EXPLO_title,
                      subtitle = EXPLO_subtitle)

      # coord_cartesian(xlim = c(min(DF_plot$X), max(DF_plot$X)), ylim = c(min(DF_plot$Y), max(DF_plot$Y)))

      if (input$DISPLAY_CONTOUR == T){
        breaks_loc <- metR::MakeBreaks(binwidth = as.numeric(input$BINWIDTH_CONTOUR))
        map_2 <- map_2 +
          metR::geom_contour_fill(ggplot2::aes(z = Z), alpha = 1, breaks = breaks_loc) +
          metR::geom_contour2(ggplot2::aes(z = Z), breaks = breaks_loc) +
          metR::geom_label_contour(ggplot2::aes(z = Z), breaks = breaks_loc)
      }

      multiplot(evD_plot, map_2, layout = matrix(c(1,1,1,1,1,2,2,2), nrow=1, byrow=TRUE))

    } else {
      return()
    }

  })

  output$PLOT <- renderPlot({
    if (is.null(SERIES_TYPE())){
      return("In order to plot")
    } else if (SERIES_TYPE()[1] == "CPS"){
      print(PLOT_CPS())
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD"){
      print(PLOT_SWEEP_STD())
    } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
      print(PLOT_SWEEP_DYN())
    }
  })

  observeEvent(input$downloadPLOT,{
    if (is.null(SERIES_TYPE())){
      return(NULL)
    } else {

      if (SERIES_TYPE()[1] == "CPS"){
        dir_PLOT <- paste(SERIES_RUN_dir(), "/0_CPS_DIGEST/", "00_PLOT_", SERIES_RUN_ID(), ".pdf", sep = "")
        pdf(dir_PLOT, width = 15, height = 10, pointsize = 1, useDingbats = FALSE)
        print(PLOT_CPS())
        dev.off()
      } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "STD"){

        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# STEADY MAP PLOT
        ########################################
        SERIES_RUN_ID <- SERIES_RUN_ID() # "EXPLO_rKDN_vs_aECFBONE_w_Fx3_BL50"
        loc_BOX <- input$PICK_BOX_STD_DYN
        LOG_EXPLO <- LOG_SERIES()
        evD_final <- evD_final()

        X <- "VAR_EXPLO_1"
        # X_div <- NULL
        # # X_div <- "f_ECF_GIT"

        Y <-  "VAR_EXPLO_2"
        # Y_div <- NULL
        # # Y_div <- "a_ECF_BONE"

        # DROP RUN 1
        evD_final <- clear_subset(evD_final[evD_final$RUN_n != 1, ])

        # VERTICALIZE
        evD_final_vert <- DF_verticalizer(df_hor = evD_final, vert_col = ALL_BOXES())

        DF <- evD_final_vert

        if (levels(DF$LEGEND_EXPLO_1) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
          EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", levels(DF$VAR_EXPLO_1)[1], " to ", levels(DF$VAR_EXPLO_1)[length(levels(DF$VAR_EXPLO_1))], sep = ""), sep = "")
        } else {
          EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", paste(c(min(DF$VAR_EXPLO_1), max(DF$VAR_EXPLO_1)), collapse = " to "), collapse = ", "), sep = "")
        }

        if (levels(DF$LEGEND_EXPLO_2) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
          EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(levels(DF$VAR_EXPLO_2), collapse = ", "), sep = ""), sep = "")
        } else {
          EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(c(min(DF$VAR_EXPLO_2), max(DF$VAR_EXPLO_2)), collapse = " to "), collapse = ", "), sep = "")
        }

        EXPLO_subtitle_0 = paste(SERIES_RUN_ID(),
                                 " (", min(LOG_SERIES()$RUN_n),
                                 "-", max(LOG_SERIES()$RUN_n),
                                 ") \n", "Hidden initial run: ",
                                 paste(LOG_SERIES()[1, "COEFF_FLUX"], collapse = " / "),
                                 sep = "")

        EXPLO_subtitle <- paste(EXPLO_subtitle_0, "\n",
                                EXPLO_subtitle_1, "\n",
                                EXPLO_subtitle_2, sep = "")


        if (input$PICK_BOX_STD_DYN_SUB != "0"){
          EXPLO_title <- paste("DELTA.", ELEMENT(), " (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(),", permil", ") - [", loc_BOX, "-", input$PICK_BOX_STD_DYN_SUB, "]", sep = "")
        } else {
          EXPLO_title <- paste("delta.", ELEMENT(), " (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(),", permil", ") - [", loc_BOX, "]", sep = "")
        }

        ################################################ PLOT VAR_EXPLO_2 vs VAR_EXPLO_1
        DF_map_1 <- DF

        X_ID <- as.character(evD_final_vert[1,"LEGEND_EXPLO_1"])
        X_ID_legend <- paste(as.character(evD_final_vert[1,"LEGEND_EXPLO_1"]), " (", as.character(evD_final_vert[1,X]), " to ",  as.character(evD_final_vert[nrow(evD_final_vert),X]), ")", sep ="")
        DF_map_1$X_ID <- X_ID
        DF_map_1$X <- DF_map_1[,X]

        Y_ID <- as.character(evD_final_vert[1,"LEGEND_EXPLO_2"])
        Y_ID_legend <- paste(as.character(evD_final_vert[1,"LEGEND_EXPLO_2"]), " (", as.character(evD_final_vert[1,Y]), " to ",  as.character(evD_final_vert[nrow(evD_final_vert),Y]), ")", sep ="")
        DF_map_1$Y_ID <- Y_ID
        DF_map_1$Y <- DF_map_1[,Y]

        remove(evD_final_vert)

        DF_map_1 <- DF_map_1[,c("X_ID", "X", "Y_ID", "Y", "VAR_TYPE", "VAR")]

        names(DF_map_1) <- c("X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")

        DF_map_1_loc <- DF_map_1[DF_map_1$Z_ID == loc_BOX,]

        if (input$PICK_BOX_STD_DYN_SUB != "0"){
          DF_map_1_loc$Z <- DF_map_1_loc$Z - DF_map_1[DF_map_1$Z_ID == input$PICK_BOX_STD_DYN_SUB, "Z"]
          DF_map_1_loc$Z_ID <- paste(DF_map_1_loc$Z_ID, "-", input$PICK_BOX_STD_DYN_SUB, sep = "")
        }

        map_1 <- ggplot2::ggplot(data = DF_map_1_loc, ggplot2::aes(x = as.numeric(X), y = as.numeric(Y), z = Z))+
          ggplot2::geom_raster(ggplot2::aes(fill = Z), hjust=0.5, vjust=0.5, interpolate = F)+
          ggplot2::scale_fill_gradientn(colors = rainbow(200))+
          ggplot2::theme_bw()+
          ggplot2::labs(y = Y_ID_legend,
                        x = X_ID_legend,
                        title = EXPLO_title,
                        subtitle = EXPLO_subtitle)

        if (input$DISPLAY_CONTOUR == T){
          breaks_loc <- metR::MakeBreaks(binwidth = as.numeric(input$BINWIDTH_CONTOUR))
          map_1 <- map_1 +
            metR::geom_contour_fill(ggplot2::aes(z = Z), alpha = 1, breaks = breaks_loc) +
            metR::geom_contour2(ggplot2::aes(z = Z), breaks = breaks_loc) +
            metR::geom_label_contour(ggplot2::aes(z = Z), breaks = breaks_loc)
        }


        ################################################ PLOT VAR_EXPLO_2 vs VAR_EXPLO_1
        DF_map_2 <- DF

        if (as.character(input$PICK_X_norm) != "1"){
          X_ID <- paste(as.character(input$PICK_X), "/", as.character(input$PICK_X_norm), sep = "")
          DF_map_2$X_ID <- X_ID
          DF_map_2$X <- DF_map_2[,input$PICK_X]/DF_map_2[,input$PICK_X_norm]
        } else {
          X_ID <- as.character(input$PICK_X)
          DF_map_2$X_ID <- X_ID
          DF_map_2$X <- DF_map_2[,input$PICK_X]
        }

        if (as.character(input$PICK_Y_norm) != "1"){
          Y_ID <- paste(as.character(input$PICK_Y), "/", as.character(input$PICK_Y_norm), sep = "")
          DF_map_2$Y_ID <- Y_ID
          DF_map_2$Y <- DF_map_2[,input$PICK_Y]/DF_map_2[,input$PICK_Y_norm]
        } else {
          Y_ID <- as.character(input$PICK_Y)
          DF_map_2$Y_ID <- Y_ID
          DF_map_2$Y <- DF_map_2[,input$PICK_Y]
        }

        DF_map_2 <- DF_map_2[,c("X_ID", "X", "Y_ID", "Y", "VAR_TYPE", "VAR")]

        names(DF_map_2) <- c("X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")

        DF_map_2_loc <- DF_map_2[DF_map_2$Z_ID == loc_BOX,]

        if (input$PICK_BOX_STD_DYN_SUB != "0"){
          DF_map_2_loc$Z <- DF_map_2_loc$Z - DF_map_2[DF_map_2$Z_ID == input$PICK_BOX_STD_DYN_SUB, "Z"]
          DF_map_2_loc$Z_ID <- paste(DF_map_2_loc$Z_ID, "-", input$PICK_BOX_STD_DYN_SUB, sep = "")
        }


        map_2 <- ggplot2::ggplot(data = DF_map_2_loc, ggplot2::aes(x = as.numeric(X), y = as.numeric(Y), z = Z))+
          ggplot2::geom_tile(ggplot2::aes(fill = Z), stat = "identity")+
          # geom_raster(ggplot2::aes(fill = Z), stat = "identity", hjust=0.5, vjust=0.5, interpolate = F)+
          ggplot2::scale_fill_gradientn(colors = rainbow(200))+
          ggplot2::scale_color_gradientn(colors = rainbow(200))+
          # metR::geom_contour2(ggplot2::aes(color = ..level..))+
          ggplot2::theme_bw()+
          ggplot2::labs(y = Y_ID,
                        x = X_ID,
                        title = EXPLO_title,
                        subtitle = EXPLO_subtitle)

        # coord_cartesian(xlim = c(min(DF_plot$X), max(DF_plot$X)), ylim = c(min(DF_plot$Y), max(DF_plot$Y)))

        if (input$DISPLAY_CONTOUR == T){
          breaks_loc <- metR::MakeBreaks(binwidth = as.numeric(input$BINWIDTH_CONTOUR))
          map_2 <- map_2 +
            metR::geom_contour_fill(ggplot2::aes(z = Z), alpha = 1, breaks = breaks_loc) +
            metR::geom_contour2(ggplot2::aes(z = Z), breaks = breaks_loc) +
            metR::geom_label_contour(ggplot2::aes(z = Z), breaks = breaks_loc)
        }

        # map_2

        dir_PLOT_1 <- paste(SERIES_RUN_dir(), "/0_STD_DIGEST/", "00_PLOT_", SERIES_RUN_ID(), "_1.pdf", sep = "")
        dir_PLOT_2 <- paste(SERIES_RUN_dir(), "/0_STD_DIGEST/", "00_PLOT_", SERIES_RUN_ID(), "_2.pdf", sep = "")

        pdf(dir_PLOT_1, width = 15, height = 10, pointsize = 1, useDingbats = FALSE)
        print(map_1)
        dev.off()

        pdf(dir_PLOT_2, width = 15, height = 10, pointsize = 1, useDingbats = FALSE)
        print(map_2)
        dev.off()

      } else if (SERIES_TYPE()[1] == "SWEEP" & SERIES_TYPE()[2] == "DYN"){
        #----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----#----# DYN FAN PLOT
        # return()
        # #######################################
        # ARGUMENTS
        SERIES_RUN_ID <- SERIES_RUN_ID()
        loc_BOX <- input$PICK_BOX_STD_DYN
        loc_BOX_sub <- input$PICK_BOX_STD_DYN_SUB
        time_units <- c(input$TIME_UNIT_in, input$TIME_UNIT_out)
        evD <- evD()
        BOXES_IDs <- ALL_BOXES()
        HIDE_VALUES_VAR_EXPLO_1 <- input$HIDE_VALUES_VAR_EXPLO_1
        HIDE_VALUES_VAR_EXPLO_2 <- input$HIDE_VALUES_VAR_EXPLO_2
        TIME_ZOOM <- input$TIME_ZOOM
        DISPLAY_DRIFT <- input$DISPLAY_DRIFT

        min_time <- (0.01*max(evD$Time)*TIME_ZOOM[1])
        max_time <- (0.01*max(evD$Time)*TIME_ZOOM[2])
        time_list <- sort(unique(evD$Time))

        time_of_mapping <- input$TIME_MAP_DYN*0.01*max(time_list)


        if (input$TIME_MAP_DYN < 100){
          time_of_mapping <- time_list[time_list >= time_of_mapping][1]
        }

        if (loc_BOX_sub != "0"){
          Z_ID <- paste("\u0394", ELEMENT(), " [", loc_BOX, "-", loc_BOX_sub,"]", sep = "")
          evD$Z_ID <- Z_ID
          evD$Z <- evD[,loc_BOX] - evD[,loc_BOX_sub]
        } else {
          Z_ID <- paste("\u03B4", ELEMENT(), " [", loc_BOX, "]", sep = "")
          evD$Z_ID <- Z_ID
          evD$Z <- evD[, loc_BOX]
        }

        # CALCULATE DRIFT FROM t0 on Y

        if (DISPLAY_DRIFT == T){
          evD$Z_ID <- paste(evD$Z_ID, " DRIFT from t0", sep = "")
          list_SERIES_RUN_ID <- levels(evD$SERIES_RUN_ID)
          i <- 1
          for (i in 1:length(list_SERIES_RUN_ID)){
            min_T_SERIES_loc <- min(evD[evD$SERIES_RUN_ID == list_SERIES_RUN_ID[i], "Time"])
            evD[evD$SERIES_RUN_ID == list_SERIES_RUN_ID[i], "Z"] <- evD[evD$SERIES_RUN_ID == list_SERIES_RUN_ID[i], "Z"] -
              evD[evD$SERIES_RUN_ID == list_SERIES_RUN_ID[i] & evD$Time == min_T_SERIES_loc, "Z"]
            i <- i + 1
          }
        }


        #### DEFINE X AND Y

        if (as.character(input$PICK_X_norm) != "1"){
          if (input$PICK_X == "VAR_EXPLO_1"){
            X_ID_num <- as.character(evD[1,"LEGEND_EXPLO_1"])
          } else if (input$PICK_X == "VAR_EXPLO_2"){
            X_ID_num <- as.character(evD[1,"LEGEND_EXPLO_2"])
          } else {
            X_ID_num <- as.character(input$PICK_X)
          }
          if (input$PICK_X_norm == "VAR_EXPLO_1"){
            X_ID_denom <- as.character(evD[1,"LEGEND_EXPLO_1"])
          } else if (input$PICK_X_norm == "VAR_EXPLO_2"){
            X_ID_denom <- as.character(evD[1,"LEGEND_EXPLO_2"])
          } else {
            X_ID_denom <- as.character(input$PICK_X_norm)
          }
          X_ID <- paste(X_ID_num, "/", X_ID_denom, sep = "")
          evD$X_ID <- X_ID
          evD$X <- evD[,input$PICK_X]/evD[,input$PICK_X_norm]
        } else {
          if (input$PICK_X == "VAR_EXPLO_1"){
            X_ID <- as.character(evD[1,"LEGEND_EXPLO_1"])
          } else if (input$PICK_X == "VAR_EXPLO_2"){
            X_ID <- as.character(evD[1,"LEGEND_EXPLO_2"])
          } else {
            X_ID <- as.character(input$PICK_X)
          }
          evD$X_ID <- X_ID
          evD$X <- evD[,input$PICK_X]
        }

        if (as.character(input$PICK_Y_norm) != "1"){
          if (input$PICK_Y == "VAR_EXPLO_1"){
            Y_ID_num <- as.character(evD[1,"LEGEND_EXPLO_1"])
          } else if (input$PICK_Y == "VAR_EXPLO_2"){
            Y_ID_num <- as.character(evD[1,"LEGEND_EXPLO_2"])
          } else {
            Y_ID_num <- as.character(input$PICK_Y)
          }
          if (input$PICK_Y_norm == "VAR_EXPLO_1"){
            Y_ID_denom <- as.character(evD[1,"LEGEND_EXPLO_1"])
          } else if (input$PICK_Y_norm == "VAR_EXPLO_2"){
            Y_ID_denom <- as.character(evD[1,"LEGEND_EXPLO_2"])
          } else {
            Y_ID_denom <- as.character(input$PICK_Y_norm)
          }
          Y_ID <- paste(Y_ID_num, "/", Y_ID_denom, sep = "")
          evD$Y_ID <- Y_ID
          evD$Y <- evD[,input$PICK_Y]/evD[,input$PICK_Y_norm]
        } else {
          if (input$PICK_Y == "VAR_EXPLO_1"){
            Y_ID <- as.character(evD[1,"LEGEND_EXPLO_1"])
          } else if (input$PICK_Y == "VAR_EXPLO_2"){
            Y_ID <- as.character(evD[1,"LEGEND_EXPLO_2"])
          } else {
            Y_ID <- as.character(input$PICK_Y)
          }
          evD$Y_ID <- Y_ID
          evD$Y <- evD[,input$PICK_Y]
        }

        # CROP TIME
        DF_map <- evD

        if (is.null(HIDE_VALUES_VAR_EXPLO_1) == F){
          evD <- clear_subset(evD[-which(as.character(evD$VAR_EXPLO_1) %in% as.character(HIDE_VALUES_VAR_EXPLO_1)),])
        }

        if (is.null(HIDE_VALUES_VAR_EXPLO_2) == F){
          evD <- clear_subset(evD[-which(as.character(evD$VAR_EXPLO_2) %in% as.character(HIDE_VALUES_VAR_EXPLO_2)),])
        }
        DF <- clear_subset(evD[evD$Time >= min_time & evD$Time <= max_time,])



        #### DEFINE TITLES

        EXPLO_subtitle_0 = paste(SERIES_RUN_ID(),
                                 " (", min(LOG_SERIES()$RUN_n),
                                 "-", max(LOG_SERIES()$RUN_n),
                                 ") \n", "Hidden initial run: ",
                                 paste(LOG_SERIES()[1, "COEFF_FLUX"], collapse = " / "),
                                 sep = "")

        if (levels(DF$LEGEND_EXPLO_1) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
          EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", levels(DF$VAR_EXPLO_1)[1], " to ", levels(DF$VAR_EXPLO_1)[length(levels(DF$VAR_EXPLO_1))], sep = ""), sep = "")
        } else {
          EXPLO_subtitle_1 <- paste(paste("Sweep param. #1 - ", levels(DF$LEGEND_EXPLO_1), ": ", paste(c(min(DF$VAR_EXPLO_1), max(DF$VAR_EXPLO_1)), collapse = " to "), collapse = ", "), sep = "")
        }

        if (levels(DF$LEGEND_EXPLO_2) %in% c("EXPLO_n_FLUX_MATRICES", "EXPLO_n_ALPHA_MATRICES")){
          EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(levels(DF$VAR_EXPLO_2), collapse = ", "), sep = ""), sep = "")
        } else {
          EXPLO_subtitle_2 <- paste(paste("Sweep param. #2 - ", levels(DF$LEGEND_EXPLO_2), ": ", paste(c(min(DF$VAR_EXPLO_2), max(DF$VAR_EXPLO_2)), collapse = " to "), collapse = ", "), sep = "")
        }



        EXPLO_subtitle <- paste(EXPLO_subtitle_0, "\n",
                                EXPLO_subtitle_1, "\n",
                                EXPLO_subtitle_2, sep = "")


        if (input$PICK_BOX_STD_DYN_SUB != "0"){
          EXPLO_title <- paste("DELTA.", ELEMENT(), " (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(),", permil", ") - [", loc_BOX, "-", input$PICK_BOX_STD_DYN_SUB, "]", sep = "")
        } else {
          EXPLO_title <- paste("delta.", ELEMENT(), " (", ISO_NUMERATOR(), "/", ISO_DENOMINATOR(),", permil", ") - [", loc_BOX, "]", sep = "")
        }

        if (DISPLAY_DRIFT == T){
          EXPLO_title <- paste(EXPLO_title, " DRIFT from t0", sep = "")
        }

        DF <- DF[, c("Time", "X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")]

        DF$TIME_OF_MAPPING <- 0
        DF[DF$Time == time_of_mapping,"TIME_OF_MAPPING"] <- 1
        #### reset time units
        initial_time_unit <- time_units[1]
        display_time_unit <- time_units[2]
        DF <- time_converter(dataframe <- DF,
                             time_colname <- "Time",
                             conv_timecolname <- "Time_plot",
                             former_unit <- initial_time_unit, # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"
                             new_unit <- display_time_unit)


        X.labs <- paste(DF[1,"X_ID"], ": ", as.character(sort(unique(DF[,"X"]))), sep = "")
        names(X.labs) <- as.character(sort(unique(DF[,"X"])))

        evD_plot <- ggplot2::ggplot(data = DF, ggplot2::aes(x = Time_plot, y = Z, color = as.numeric(Y), group = as.numeric(Y)))+
          ggplot2::geom_line()+
          ggplot2::scale_color_gradientn(name = DF[1,"Y_ID"], colors = rainbow(100))+
          ggplot2::facet_grid(. ~ X, labeller = ggplot2::labeller(X = X.labs))+
          ggplot2::theme_bw()+
          ggplot2::theme(strip.text = ggplot2::element_text(size = 12, colour = "white", face = "bold"),
                         strip.background = ggplot2::element_rect(fill = "black"))+
          ggplot2::labs(x = paste("Time (", time_units[2], ")", sep = ""),
                        y = DF[1,"Z_ID"],
                        title = EXPLO_title,
                        subtitle = EXPLO_subtitle)


        if (time_of_mapping <= max_time & time_of_mapping >= min_time){
          evD_plot <- evD_plot + ggplot2::geom_vline(xintercept = DF[DF$TIME_OF_MAPPING == 1, "Time_plot"], linetype = "dashed", color = "black")
        }

        ################################################ MAP PLOT

        DF_map <- DF_map[, c("Time", "X_ID", "X", "Y_ID", "Y", "Z_ID", "Z")]

        DF_map <- DF_map[DF_map$Time == time_of_mapping, ]

        #### reset time units
        initial_time_unit <- time_units[1]
        display_time_unit <- time_units[2]
        DF_map <- time_converter(dataframe <- DF_map,
                                 time_colname <- "Time",
                                 conv_timecolname <- "Time_plot",
                                 former_unit <- initial_time_unit, # "micros" "ms"     "s"      "min"    "h"      "d"      "wk"     "mo"     "yr"     "kyr"    "Myr"    "Gyr"
                                 new_unit <- display_time_unit)

        EXPLO_title <- paste(EXPLO_title, " at time = ", dec_2(DF_map[1, "Time_plot"]), " ", display_time_unit, sep = "")

        map_2 <- ggplot2::ggplot(data = DF_map, ggplot2::aes(x = as.numeric(X), y = as.numeric(Y), z = Z))+
          ggplot2::geom_raster(ggplot2::aes(fill = Z), stat = "identity", hjust=0.5, vjust=0.5, interpolate = F)+
          ggplot2::scale_fill_gradientn(colors = rainbow(200))+
          ggplot2::scale_color_gradientn(colors = rainbow(200))+
          # metR::geom_contour2(ggplot2::aes(color = ..level..))+
          ggplot2::theme_bw()+
          ggplot2::labs(y = Y_ID,
                        x = X_ID,
                        title = EXPLO_title,
                        subtitle = EXPLO_subtitle)

        # coord_cartesian(xlim = c(min(DF_plot$X), max(DF_plot$X)), ylim = c(min(DF_plot$Y), max(DF_plot$Y)))

        if (input$DISPLAY_CONTOUR == T){
          breaks_loc <- metR::MakeBreaks(binwidth = as.numeric(input$BINWIDTH_CONTOUR))
          map_2 <- map_2 +
            metR::geom_contour_fill(ggplot2::aes(z = Z), alpha = 1, breaks = breaks_loc) +
            metR::geom_contour2(ggplot2::aes(z = Z), breaks = breaks_loc) +
            metR::geom_label_contour(ggplot2::aes(z = Z), breaks = breaks_loc)
        }

        dir_PLOT_1 <- paste(SERIES_RUN_dir(), "/0_DYN_DIGEST/", "00_PLOT_", SERIES_RUN_ID(), "_1.pdf", sep = "")
        dir_PLOT_2 <- paste(SERIES_RUN_dir(), "/0_DYN_DIGEST/", "00_PLOT_", SERIES_RUN_ID(), "_2.pdf", sep = "")

        pdf(dir_PLOT_1, width = 20, height = 10, pointsize = 1, useDingbats = FALSE)
        print(evD_plot)
        dev.off()

        pdf(dir_PLOT_2, width = 10, height = 10, pointsize = 1, useDingbats = FALSE)
        print(map_2)
        dev.off()
      }
    }
  })

  output$DIAGRAM_F <- renderPlot({
    if (is.null(SERIES_TYPE())){
      return()
    } else {
      RUN_n_loc <- input$DIAG_RUN_loc

      dir_INPUT_loc <- paste(SERIES_RUN_dir(), "/", as.character(LOG_SERIES()[LOG_SERIES()$RUN_n == RUN_n_loc,"SERIES_RUN_ID"]), "_IN.Rda", sep = "")
      load(dir_INPUT_loc)
      BOX_META <- BOX_META_IN

      if (!is.na(DISCONNECTED_BOXES())){
        BOX_META <- clear_subset(BOX_META[-which(BOX_META$BOXES_ID %in% DISCONNECTED_BOXES()),])
      } else {
        BOX_META <- clear_subset(BOX_META)
      }

      LAYOUT <- as.matrix(BOX_META[,c("LAYOUT_X", "LAYOUT_Y")])
      DIAG_GROUPS <- as.factor(BOX_META[, c("INFINITE")])

      NET_FLUXES_title <-  paste("RUN # :", as.character(RUN_n_loc) , " - Flux config: " , as.character(LOG_SERIES()[LOG_SERIES()$RUN_n == RUN_n_loc, "FLUX_MASTER"]), sep = "")
      FLUXES_adj <- FLUXES_IN

      if (!is.na(DISCONNECTED_BOXES())){
        FLUXES_adj <- FLUXES_adj[,-which(names(FLUXES_adj) %in% DISCONNECTED_BOXES())]
        FLUXES_adj <- FLUXES_adj[-which(FLUXES_adj$BOXES_ID %in% DISCONNECTED_BOXES()),]
      }

      FLUXES_adj <- clear_subset(FLUXES_adj[ , !(names(FLUXES_adj) %in% "BOXES_ID")])

      # NET_FLUXES <- qgraph::qgraph(FLUXES_adj,
      #                              title = NET_FLUXES_title,
      #                              layout = LAYOUT,
      #                              edge.labels = T,
      #                              edge.label.color = "black",
      #                              shape = "square",
      #                              fade = F,
      #                              groups = DIAG_GROUPS,
      #                              color = rainbow(length(levels(DIAG_GROUPS)), s = 0.25),
      #                              legend = F,
      #                              edge.color = "black",
      #                              curve = 0.7, curveAll = F,
      #                              # edge.label.cex = 2.5,
      #                              edge.label.margin = 0.02)#,
      NET_FLUXES <- qgraph::qgraph(FLUXES_adj,
                                   title = NET_FLUXES_title,
                                   layout = LAYOUT,
                                   edge.labels = T,
                                   edge.label.color = "black",
                                   shape = "square",
                                   fade = F,
                                   groups = DIAG_GROUPS,
                                   color = rainbow(length(levels(DIAG_GROUPS)), s = 0.25),
                                   legend = F,
                                   edge.color = "black",
                                   # edge.label.cex = 2.5,
                                   edge.label.cex = 1.5*exp(-nrow(as.data.frame(FLUXES_adj))/19),
                                   edge.label.margin = 0.02,
                                   # asize = 8,
                                   asize = 4*exp(-nrow(as.data.frame(FLUXES_adj))/20)+2,
                                   # curve = 0.7,
                                   curve = 0.88*exp(-nrow(as.data.frame(FLUXES_adj))/17.54),
                                   # curveScale = T,
                                   curveAll = F,
                                   vsize = 8*exp(-nrow(as.data.frame(FLUXES_adj))/80)+1)

      NET_FLUXES

    }
  })

  output$DIAGRAM_A <- renderPlot({
    if (is.null(SERIES_TYPE())){
      return()
    } else {
      RUN_n_loc <- input$DIAG_RUN_loc

      dir_INPUT_loc <- paste(SERIES_RUN_dir(), "/", as.character(LOG_SERIES()[LOG_SERIES()$RUN_n == RUN_n_loc,"SERIES_RUN_ID"]), "_IN.Rda", sep = "")
      load(dir_INPUT_loc)
      BOX_META <- BOX_META_IN

      if (!is.na(DISCONNECTED_BOXES())){
        BOX_META <- clear_subset(BOX_META[-which(BOX_META$BOXES_ID %in% DISCONNECTED_BOXES()),])
      } else {
        BOX_META <- clear_subset(BOX_META)
      }

      LAYOUT <- as.matrix(BOX_META[,c("LAYOUT_X", "LAYOUT_Y")])
      DIAG_GROUPS <- as.factor(BOX_META[,c("INFINITE")])

      NET_COEFFS_title <-  paste("RUN # :", as.character(RUN_n_loc) , " - Coeff config: " , as.character(LOG_SERIES()[LOG_SERIES()$RUN_n == RUN_n_loc, "COEFF_RUN"]), sep = "")
      COEFFS_adj <- COEFFS_IN

      if (!is.na(DISCONNECTED_BOXES())){
        COEFFS_adj <- COEFFS_adj[,-which(names(COEFFS_adj) %in% DISCONNECTED_BOXES())]
        COEFFS_adj <- COEFFS_adj[-which(COEFFS_adj$BOXES_ID %in% DISCONNECTED_BOXES()),]
      }

      COEFFS_adj <- clear_subset(COEFFS_adj[ , !(names(COEFFS_adj) %in% "BOXES_ID")])

      COEFFS_adj <- 1000*log(COEFFS_adj)

      COEFFS_adj <- qgraph::qgraph(COEFFS_adj,
                                   title = NET_COEFFS_title,
                                   layout = LAYOUT,
                                   edge.labels = T,
                                   edge.label.color = "brown4",
                                   shape = "square",
                                   fade = F,
                                   groups = DIAG_GROUPS,
                                   color = rainbow(length(levels(DIAG_GROUPS)), s = 0.25),
                                   legend = F,
                                   edge.color = "brown4",
                                   # edge.label.cex = 2.5,
                                   edge.label.cex = 1.5*exp(-nrow(as.data.frame(COEFFS_adj))/19),
                                   edge.label.margin = 0.02,
                                   # asize = 8,
                                   asize = 4*exp(-nrow(as.data.frame(COEFFS_adj))/20)+2,
                                   # curve = 0.7,
                                   curve = 0.88*exp(-nrow(as.data.frame(COEFFS_adj))/17.54),
                                   curveAll = F,
                                   vsize = 8*exp(-nrow(as.data.frame(COEFFS_adj))/80)+1)
      COEFFS_adj

    }

  })

  output$summary_table = DT::renderDataTable({
    if (SERIES_TYPE()[1] == "CPS"){
      data_table <- LOG_SERIES()
      data_table$Displayed <- "."
      data_table[input$DIAG_RUN_loc, "Displayed"] <- "***"

      short_cols <- c("Displayed", "RUN_n", "T_LIM", "FLUX_MASTER", "COEFF_RUN", "UNBAL_FINITE_BOXES")
      data_table <- data_table[,short_cols]

      short_cols_ID <- c("Disp.", "Run #", "Duration", "Flux config.", "Coeff. config.", "Unbalanced boxes")
      names(data_table) <- short_cols_ID
      data_table

    } else {
      return(NULL)
    }

  }, style = "bootstrap4", filter = 'top', server = TRUE, options = list(
    pageLength = 10, autoWidth = TRUE)
  )

  CPS_REPORT <- reactive({
    if (is.null(SERIES_TYPE())){
      return(NULL)
    } else if (SERIES_TYPE()[1] == "CPS"){

      time_units <- c(input$TIME_UNIT_in, input$TIME_UNIT_out)
      LOG <- LOG_SERIES()
      BOXES_IDs_list <- strsplit(as.character(LOG[1,"BOXES_ID_list"]), split = "_")[[1]]
      INFINITE_BOXES_list <- strsplit(as.character(LOG[1,"INFINITE_BOXES_list"]), split = "_")[[1]]
      # if(is.null(INFINITE_BOXES_list) == FALSE | length(INFINITE_BOXES_list) != 0){
      if(!is.na(INFINITE_BOXES_list)){
        INFINITE_BOXES_list <- INFINITE_BOXES_list
        FINITE_BOXES_list <- BOXES_IDs_list[!BOXES_IDs_list %in% c(INFINITE_BOXES_list)]
      } else {
        INFINITE_BOXES_list <- NULL
        FINITE_BOXES_list <- BOXES_IDs_list
      }
      # FINITE_BOXES_list <- BOXES_IDs_list[!BOXES_IDs_list %in% c(INFINITE_BOXES_list)]
      RUN_n_list <- as.vector(LOG$RUN_n)
      T_lim_list <- as.vector(LOG$T_LIM)

      dir_COMPO_MASTER <- paste(SERIES_RUN_dir(), "/0_CPS_DIGEST/", "CPS_", SERIES_RUN_ID(), "_MASTER.xlsx", sep = "")
      COMPO_MASTER_RUN_LIST <- as.data.frame(readxl::read_excel(dir_COMPO_MASTER, "RUN_LIST"))
      COMPO_MASTER_FORCING_RAYLEIGH <- as.data.frame(readxl::read_excel(dir_COMPO_MASTER, "FORCING_RAYLEIGH"))
      COMPO_MASTER_FORCING_SIZE <- as.data.frame(readxl::read_excel(dir_COMPO_MASTER, "FORCING_SIZE"))
      COMPO_MASTER_FORCING_DELTA <- as.data.frame(readxl::read_excel(dir_COMPO_MASTER, "FORCING_DELTA"))
      COMPO_MASTER_FORCING_ALPHA <- as.data.frame(readxl::read_excel(dir_COMPO_MASTER, "FORCING_ALPHA"))
      COMPO_MASTER_RUN_LIST$Time_composite <- COMPO_MASTER_RUN_LIST$t_lim_list

      evS <- evS()
      if (!is.null(INFINITE_BOXES_list)){
        evS <- evS[,-which(names(evS) %in% INFINITE_BOXES_list)]
      } else {
        evS <- evS
      }
      evS_meta_colnames <- names(evS)[!names(evS) %in% FINITE_BOXES_list]

      i <- 1
      for (i in 1:length(FINITE_BOXES_list)){
        if (length(unique(evS[,FINITE_BOXES_list[i]])) == 1){
          evS <- evS[,-which(names(evS) %in% FINITE_BOXES_list[i])]
        }
        i <- i + 1
      }

      evS_box_colnames <- names(evS)[!names(evS) %in% evS_meta_colnames]

      # if (length(evS_box_colnames) != 0 | is.null(evS_box_colnames) == FALSE){
      if (length(evS_box_colnames) != 0){
        evS_vert <- DF_verticalizer(evS, vert_col = evS_box_colnames)
      }

      i <- 1
      Time <- 0
      for (i in 1:length(RUN_n_list)){
        RUN_n_loc <- RUN_n_list[i]

        COMPO_MASTER_RUN_LIST[COMPO_MASTER_RUN_LIST$COMPO_RUN_n == RUN_n_loc,"Time_composite"] <- Time
        DF_REPORT_loc <- data.frame(RUN_n = RUN_n_list[i],
                                    T_lim = T_lim_list[i],
                                    Time = Time,
                                    TYPE = NaN,
                                    FROM = NaN,
                                    TO = NaN,
                                    FROM_TO = NaN,
                                    BOX = NaN,
                                    VALUE = NaN)
        Time <- Time + T_lim_list[i]

        DF_REPORT_empty <- DF_REPORT_loc

        dir_INPUT_loc <- paste(SERIES_RUN_dir(), "/", as.character(LOG[LOG$RUN_n == RUN_n_loc,"SERIES_RUN_ID"]), "_IN.Rda", sep = "")
        load(dir_INPUT_loc)
        FLUXES_adj <- FLUXES_IN
        COEFFS_adj <- COEFFS_IN

        DF_REPORT_FLUXES <- clear_subset(DF_REPORT_empty[rep(seq_len(nrow(DF_REPORT_empty)), each = (nrow(FLUXES_adj))^2), ])
        DF_REPORT_FLUXES$TYPE <- "FLUX"
        FLUXES_vert_loc <- DF_verticalizer(df_hor = FLUXES_adj, vert_col = BOXES_IDs_list)
        DF_REPORT_FLUXES$FROM <- FLUXES_vert_loc$BOXES_ID
        DF_REPORT_FLUXES$TO <- FLUXES_vert_loc$VAR_TYPE
        DF_REPORT_FLUXES$VALUE <- FLUXES_vert_loc$VAR
        DF_REPORT_FLUXES$FROM_TO <- paste(DF_REPORT_FLUXES$FROM, DF_REPORT_FLUXES$TO, sep = "_")

        DF_REPORT_COEFFS <- clear_subset(DF_REPORT_empty[rep(seq_len(nrow(DF_REPORT_empty)), each = (nrow(COEFFS_adj))^2), ])
        DF_REPORT_COEFFS$TYPE <- "COEFF"
        COEFFS_vert_loc <- DF_verticalizer(df_hor = COEFFS_adj, vert_col = BOXES_IDs_list)
        DF_REPORT_COEFFS$FROM <- COEFFS_vert_loc$BOXES_ID
        DF_REPORT_COEFFS$TO <- COEFFS_vert_loc$VAR_TYPE
        DF_REPORT_COEFFS$VALUE <- COEFFS_vert_loc$VAR
        DF_REPORT_COEFFS$FROM_TO <- paste(DF_REPORT_COEFFS$FROM, DF_REPORT_COEFFS$TO, sep = "_")

        if (i == 1){
          DF_REPORT <- rbind(DF_REPORT_FLUXES, DF_REPORT_COEFFS)
        } else {
          DF_REPORT_loc <- rbind(DF_REPORT_FLUXES, DF_REPORT_COEFFS)
          DF_REPORT <- rbind(DF_REPORT, DF_REPORT_loc)
        }
      }

      if (nrow(DF_REPORT) != 0){
        # FLUX
        FLUXES_FROM_TO_list <- unique(DF_REPORT[DF_REPORT$TYPE == "FLUX","FROM_TO"])
        COEFFS_FROM_TO_list <- unique(DF_REPORT[DF_REPORT$TYPE == "COEFF","FROM_TO"])
        FLUXES_FROM_TO_list_keep <- FLUXES_FROM_TO_list
        COEFFS_FROM_TO_list_keep <- COEFFS_FROM_TO_list
        i <- 1
        for (i in 1:length(FLUXES_FROM_TO_list)){
          if(length(unique(DF_REPORT[DF_REPORT$TYPE == "FLUX" & DF_REPORT$FROM_TO == FLUXES_FROM_TO_list[i],"VALUE"])) == 1){
            FLUXES_FROM_TO_list_keep <- FLUXES_FROM_TO_list_keep[!FLUXES_FROM_TO_list_keep %in% FLUXES_FROM_TO_list[i]]
          }
          if(length(unique(DF_REPORT[DF_REPORT$TYPE == "COEFF" & DF_REPORT$FROM_TO == COEFFS_FROM_TO_list[i],"VALUE"])) == 1){
            COEFFS_FROM_TO_list_keep <- COEFFS_FROM_TO_list_keep[!COEFFS_FROM_TO_list_keep %in% COEFFS_FROM_TO_list[i]]
          }
          i <- i + 1
        }
        FLUXES_FROM_TO_list_drop <- FLUXES_FROM_TO_list[!FLUXES_FROM_TO_list %in% FLUXES_FROM_TO_list_keep]
        COEFFS_FROM_TO_list_drop <- COEFFS_FROM_TO_list[!COEFFS_FROM_TO_list %in% COEFFS_FROM_TO_list_keep]

        DF_REPORT <- DF_REPORT[-which(DF_REPORT$TYPE == "FLUX" & DF_REPORT$FROM_TO %in% FLUXES_FROM_TO_list_drop),  ]
        DF_REPORT <- DF_REPORT[-which(DF_REPORT$TYPE == "COEFF" & DF_REPORT$FROM_TO %in% COEFFS_FROM_TO_list_drop),  ]
        DF_REPORT <- clear_subset(DF_REPORT)
      }

      DF_REPORT_loc <- data.frame(RUN_n = NaN,
                                  T_lim = NaN,
                                  Time = NaN,
                                  TYPE = NaN,
                                  FROM = NaN,
                                  TO = NaN,
                                  FROM_TO = NaN,
                                  BOX = NaN,
                                  VALUE = NaN)

      DF_REPORT_empty <- DF_REPORT_loc

      if (nrow(COMPO_MASTER_FORCING_RAYLEIGH) != 0){
        DF_REPORT_RAYLEIGH <- clear_subset(DF_REPORT_empty[rep(seq_len(nrow(DF_REPORT_empty)), each = nrow(COMPO_MASTER_FORCING_RAYLEIGH)), ])
        DF_REPORT_RAYLEIGH$RUN_n <- COMPO_MASTER_FORCING_RAYLEIGH$COMPO_RUN_n
        DF_REPORT_RAYLEIGH$TYPE <- "RAYLEIGH_ALPHA"
        DF_REPORT_RAYLEIGH$FROM <- COMPO_MASTER_FORCING_RAYLEIGH$AFROM
        DF_REPORT_RAYLEIGH$TO <- COMPO_MASTER_FORCING_RAYLEIGH$ATO
        DF_REPORT_RAYLEIGH$FROM_TO <- paste(DF_REPORT_RAYLEIGH$FROM, DF_REPORT_RAYLEIGH$TO, sep = "_")
        DF_REPORT_RAYLEIGH$VALUE <- COMPO_MASTER_FORCING_RAYLEIGH$ALPHA_0
        i <- 1
        for (i in 1:nrow(COMPO_MASTER_RUN_LIST)){
          DF_REPORT_RAYLEIGH[DF_REPORT_RAYLEIGH$RUN_n == COMPO_MASTER_RUN_LIST[i,"COMPO_RUN_n"], "T_lim"] <- COMPO_MASTER_RUN_LIST[i,"t_lim_list"]
          DF_REPORT_RAYLEIGH[DF_REPORT_RAYLEIGH$RUN_n == COMPO_MASTER_RUN_LIST[i,"COMPO_RUN_n"], "Time"] <- COMPO_MASTER_RUN_LIST[i,"Time_composite"]
          i <- i + 1
        }
        DF_REPORT <- rbind(DF_REPORT, DF_REPORT_RAYLEIGH)
      }

      if (nrow(COMPO_MASTER_FORCING_SIZE) != 0){
        DF_REPORT_SIZE <- clear_subset(DF_REPORT_empty[rep(seq_len(nrow(DF_REPORT_empty)), each = nrow(COMPO_MASTER_FORCING_SIZE)), ])
        DF_REPORT_SIZE$RUN_n <- COMPO_MASTER_FORCING_SIZE$COMPO_RUN_n
        DF_REPORT_SIZE$TYPE <- "SIZE_FORCING"
        DF_REPORT_SIZE$BOX <- COMPO_MASTER_FORCING_SIZE$BOXES_ID
        DF_REPORT_SIZE$VALUE <- COMPO_MASTER_FORCING_SIZE$SIZE_INIT
        i <- 1
        for (i in 1:nrow(COMPO_MASTER_RUN_LIST)){
          DF_REPORT_SIZE[DF_REPORT_SIZE$RUN_n == COMPO_MASTER_RUN_LIST[i,"COMPO_RUN_n"], "T_lim"] <- COMPO_MASTER_RUN_LIST[i,"t_lim_list"]
          DF_REPORT_SIZE[DF_REPORT_SIZE$RUN_n == COMPO_MASTER_RUN_LIST[i,"COMPO_RUN_n"], "Time"] <- COMPO_MASTER_RUN_LIST[i,"Time_composite"]
          i <- i + 1
        }
        DF_REPORT <- rbind(DF_REPORT, DF_REPORT_SIZE)
      }

      if (nrow(COMPO_MASTER_FORCING_DELTA) != 0){
        DF_REPORT_DELTA <- clear_subset(DF_REPORT_empty[rep(seq_len(nrow(DF_REPORT_empty)), each = nrow(COMPO_MASTER_FORCING_DELTA)), ])
        DF_REPORT_DELTA$RUN_n <- COMPO_MASTER_FORCING_DELTA$COMPO_RUN_n
        DF_REPORT_DELTA$TYPE <- "DELTA_FORCING"
        DF_REPORT_DELTA$BOX <- COMPO_MASTER_FORCING_DELTA$BOXES_ID
        DF_REPORT_DELTA$VALUE <- COMPO_MASTER_FORCING_DELTA$DELTA_INIT
        i <- 1
        for (i in 1:nrow(COMPO_MASTER_RUN_LIST)){
          DF_REPORT_DELTA[DF_REPORT_DELTA$RUN_n == COMPO_MASTER_RUN_LIST[i,"COMPO_RUN_n"], "T_lim"] <- COMPO_MASTER_RUN_LIST[i,"t_lim_list"]
          DF_REPORT_DELTA[DF_REPORT_DELTA$RUN_n == COMPO_MASTER_RUN_LIST[i,"COMPO_RUN_n"], "Time"] <- COMPO_MASTER_RUN_LIST[i,"Time_composite"]
          i <- i + 1
        }
        DF_REPORT <- rbind(DF_REPORT, DF_REPORT_DELTA)
      }

      if (exists("evS_vert") == TRUE){
        DF_REPORT_evS <- clear_subset(DF_REPORT_empty[rep(seq_len(nrow(DF_REPORT_empty)), each = nrow(evS_vert)), ])
        DF_REPORT_evS$RUN_n <- evS_vert$RUN_n
        DF_REPORT_evS$T_lim <- evS_vert$Time
        DF_REPORT_evS$Time <-  evS_vert$Time_COMPOSITE
        DF_REPORT_evS$TYPE <- "SIZE"
        DF_REPORT_evS$BOX <- evS_vert$VAR_TYPE
        DF_REPORT_evS$VALUE <- evS_vert$VAR
        DF_REPORT <- rbind(DF_REPORT, DF_REPORT_evS)
      }


      DF_REPORT$GROUP <- DF_REPORT$BOX
      DF_REPORT[DF_REPORT$BOX == "NaN", "GROUP"] <- DF_REPORT[DF_REPORT$BOX == "NaN", "FROM_TO"]

      DF_REPORT$TYPE <- as.factor(DF_REPORT$TYPE)
      DF_REPORT$GROUP <- as.factor(DF_REPORT$GROUP)

      DF_plot <- DF_REPORT
      TYPE_list <- levels(DF_plot$TYPE)[!levels(DF_plot$TYPE) %in% c("SIZE", "DELTA_FORCING")]
      i <- 1
      for (i in 1:length(TYPE_list)){
        DF_plot_loc <- DF_plot[DF_plot$TYPE == TYPE_list[i],]
        DF_plot_loc$Time <- DF_plot_loc$T_lim + DF_plot_loc$Time
        i <- i + 1
        DF_plot <- rbind(DF_plot_loc, DF_plot)
      }

      DF_REPORT <- DF_plot

      initial_time_unit <- time_units[1]
      display_time_unit <- time_units[2]

      DF_REPORT <- time_converter(dataframe = DF_REPORT, time_colname = "Time", conv_timecolname = "Time_plot", former_unit = initial_time_unit, new_unit = display_time_unit)
      COMPO_MASTER_RUN_LIST <- time_converter(dataframe = COMPO_MASTER_RUN_LIST, time_colname = "Time_composite", conv_timecolname = "Time_plot", former_unit = initial_time_unit, new_unit = display_time_unit)
      COMPO_MASTER_RUN_LIST <- time_converter(dataframe = COMPO_MASTER_RUN_LIST, time_colname = "t_lim_list", conv_timecolname = "t_lim_list_plot", former_unit = initial_time_unit, new_unit = display_time_unit)

      DF_REPORT <- DF_REPORT[DF_REPORT$RUN_n %in% seq(input$SHOW_RUNS[1],input$SHOW_RUNS[2],1), ]

      DF_REPORT[DF_REPORT$TYPE %in% c("RAYLEIGH_ALPHA", "COEFF"),"VALUE"] <- 1000*log(DF_REPORT[DF_REPORT$TYPE %in% c("RAYLEIGH_ALPHA", "COEFF"),"VALUE"])

      COMPO_MASTER_RUN_LIST <- COMPO_MASTER_RUN_LIST[COMPO_MASTER_RUN_LIST$COMPO_RUN_n %in% seq(input$SHOW_RUNS[1],input$SHOW_RUNS[2],1), ]

      return(list(DF_REPORT, COMPO_MASTER_RUN_LIST))

    } else {
      return(NULL)
    }

  })

  CPS_PLOT_REPORT <- reactive({    ################################################################################################

    if (is.null(SERIES_TYPE())){
      return()
    } else if (SERIES_TYPE()[1] == "CPS"){

      DF_REPORT <- CPS_REPORT()[[1]]
      COMPO_MASTER_RUN_LIST <- CPS_REPORT()[[2]]

      CPS_PLOT_REPORT <- ggplot2::ggplot(data = DF_REPORT, ggplot2::aes(x = Time_plot, y = VALUE, color = TYPE))+
        ggplot2::geom_line(cex = 1)+
        # geom_point(pch = 19, cex = 3)+
        ggplot2::theme_bw()+
        ggplot2::theme(strip.text = ggplot2::element_text(size = 12, colour = "white", face = "bold"),
                       strip.background = ggplot2::element_rect(fill = "black"),
                       legend.position = "none")+
        ggplot2::facet_wrap(TYPE ~ GROUP, scales = "free_y", ncol = 3)#+

      i <- 1
      for (i in 1:nrow(COMPO_MASTER_RUN_LIST)){
        CPS_PLOT_REPORT <- CPS_PLOT_REPORT +
          ggplot2::geom_vline(xintercept = COMPO_MASTER_RUN_LIST[i,"Time_plot"], linetype = "dashed", color = "gray75", cex = 1)
        i <- i + 1
      }

      if (input$DIAG_RUN_loc %in% COMPO_MASTER_RUN_LIST$COMPO_RUN_n){
        CPS_PLOT_REPORT <- CPS_PLOT_REPORT + ggplot2::annotate("rect",
                                                               xmin = COMPO_MASTER_RUN_LIST[COMPO_MASTER_RUN_LIST$COMPO_RUN_n == input$DIAG_RUN_loc, "Time_plot"],
                                                               xmax = COMPO_MASTER_RUN_LIST[COMPO_MASTER_RUN_LIST$COMPO_RUN_n == input$DIAG_RUN_loc, "Time_plot"] + COMPO_MASTER_RUN_LIST[COMPO_MASTER_RUN_LIST$COMPO_RUN_n == input$DIAG_RUN_loc, "t_lim_list_plot"],
                                                               ymin = -Inf, ymax = +Inf, alpha = .2, fill = "red")
      }

      CPS_PLOT_REPORT <- CPS_PLOT_REPORT + ggplot2::geom_point(pch = 19, cex = 4)

      CPS_PLOT_REPORT

    } else {
      return()
    }

  })

  output$CPS_PLOT_REPORT <- renderPlot({
    print(CPS_PLOT_REPORT())
  }
  )

  output$CPS_PLOT_REPORT_yn <- reactive({
    if (!is.null(CPS_PLOT_REPORT())){
      CPS_PLOT_REPORT_yn <- "YES"
      return("YES")
    } else {
      CPS_PLOT_REPORT_yn <- "NO"
      return("NO")
    }
  })

  outputOptions(output, "CPS_PLOT_REPORT_yn", suspendWhenHidden = FALSE)

  observeEvent(input$download_CPS_REPORT_PLOT,{
    if (is.null(SERIES_TYPE())){
      return(NULL)
    } else {
      dir_PLOT <- paste(SERIES_RUN_dir(), "/0_CPS_DIGEST/", "00_CPS_REPORT_PLOT_", SERIES_RUN_ID(), ".pdf", sep = "")
      pdf(dir_PLOT, width = 15, height = 20, pointsize = 1, useDingbats = FALSE)
      show(CPS_PLOT_REPORT())
      dev.off()
    }
  })
}

#----#----#----#---- build the application #----#----#----#----#----#----#----#----
# shinyApp(ui = ui, server = server , options = list("quiet" , shiny::shinyOptions(shiny.trace = FALSE, shiny.fullstacktrace = FALSE, shiny.reactlog = FALSE)))
shinyApp(ui = ui, server = server)
