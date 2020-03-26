# Dashboard app based on R Shiny to visualise various transport data generated
# as part of the Inclusive and Healthy Mobility project at UCL

# Developed by Alistair Leak https://www.linkedin.com/in/alistairleak/ as part
# of the Inclusive and Healthy Mobility project at UCL.

## app.R ##
library(shinydashboard) # Dashboard.
library(data.table) # Use of data.tables - high performance data in R
library(leaflet) # Interface to leaflet maps
library(leaflet.minicharts) # Display flows on leaflet maps
library(RColorBrewer) # Color Ramps
library(dplyr) # tools for data manipulation
library(stringr) # tools for handling strings
library(lubridate) # Tools for handling dates and times
library(geosphere) # For rendering lines between longitudes and latitudes
library(dygraphs) # Access to dygraphs javascript in R
library(shinyWidgets) # Helper functions for shiny
library(htmltools) # Helper functions for Shiny
library(rbokeh) # Access to Bokeh plotting in R
library(maptools) # read and write shapefiles
library(shinyjs) # Additional java script functionality
library(forcats) # Helper functions for working with factors
library(rintrojs) # Javascript based introduction to app
library(DT) # package for rendering data tables
library(sf) # package for dealing with spatial data

#here are the SQL/connection packages
library(RPostgreSQL)
library(DBI)
library(glue)

#load in the env variables
db_name=Sys.getenv("DBNAME")
host_name=Sys.getenv("HOSTNAME")
user_name=Sys.getenv("USERNAME")
password_name=Sys.getenv("PASSNAME")

schema_name <- "public_dashboard"

#set up a connection
con <- dbConnect(dbDriver("PostgreSQL"), 
                 dbname = db_name, 
                 host = host_name, 
                 user = user_name, 
                 password = password_name)


#actual function part of app begins
ui <- function(request) {
  
  
  
  
  dashboardPage(
    dashboardHeader(title = "Dashboard"),
    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        # Here we will have an information dashboard showing various summary statistics which can
        # be found using the origin destination
        menuItem("Who Travels", tabName = "tab_dygraphs", icon = icon("dashboard")),
        # Here we will have our tool for
        #menuItem("Where and When (TfWM 123)", tabName = "tab_od_c", icon = icon("map")),
        menuItem("Where and When", tabName = "tab_od_c_real", icon = icon("map")),
        menuItem("Access to Services", tabName = "tab_access", icon = icon("dashboard")),
        menuItem("Who is Eligible", tabName = "tab_elig", icon = icon("user")),
        # Here we will have instructions, credits etc
        menuItem("About", tabName = "tab_about", icon = icon("info")),
        bookmarkButton(
          label = "Share",
          title = "Share the Current App State as a URL.",
          icon("paper-plane"),
          style = "color: #fff; margin: auto; border: none; background: none;"
        )
      )
    ),
    # Dashboard body holds each of the dashboard pages.
    dashboardBody(
      useShinyjs(),
      introjsUI(),
      useSweetAlert(),
      tabItems(
        tabItem(
          tabName = "tab_dygraphs",
          tags$h4("Explore who travels over time (ENCTS Concession).", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.1em;"),
          fluidRow(
            box(
              absolutePanel(
                top = 35, left = 20, style = "z-index: 1000;",
                dropdownButton(
                  circle = TRUE, status = "danger", icon = icon("gear"), width = "200px",
                  selectInput(
                    inputId = "dygraph_statistic", label = "Statistic",
                    choices = list("Passengers" = "passengers", "Journeys" = "journeys", "Journeys per person" = "journeys_pp")
                  ),
                  selectInput(
                    inputId = "dygraph_type", label = "Passenger Type",
                    choices = list(
                      "Over 60 Concession" = "Over 60 Concession",
                      "Disabled Concession" = "Disabled Concession"
                    )
                  ),
                  selectInput(
                    inputId = "dygraph_time_resolution", label = "Resolution",
                    choices = list("1 Month" = "month", "Quarter" = "quarter", "Year" = "year")
                  ),
                  tooltip = tooltipOptions(title = "Click to see inputs !")
                )
              ),
              title = "Passenger Breakdown", width = 12,
              column(12, dygraphOutput("dygraph_gender", height = "200px")),
              column(12, dygraphOutput("dygraph_age", height = "250px"))
            )
          )
        ),
        tabItem(
          tabName = "tab_od_c_real",
          tags$h4("Query regional passenger flows.", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.1em;"),
          # Top menu bar has all control elements
          fluidRow(
            box(
              width = 3, title = "When?", id = "od_box_1_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                airMonthpickerInput("od_c_real_date_start_1",
                                    label = "Start month",
                                    value = "2015-10-01",
                                    maxDate = "2016-08-01",
                                    minDate = "2014-01-01"
                )
              ),
              column(
                12,
                sliderInput("od_c_real_period",
                            label = "Period in months",
                            min = 1, max = 24, value = c(1, 24)
                )
              )
            ),
            box(
              width = 3, title = "Who?", id = "od_box_2_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                shinyjs::disabled(pickerInput(
                  inputId = "od_c_real_demographics",
                  label = "Demographics",
                  choices = list(
                    "Age" = list(
                      "60 - 65" = "60",
                      "65 - 70" = "65",
                      "70 - 75" = "70",
                      "75 - 80" = "75",
                      "80 +   " = "80"
                    ),
                    "Gender" = list(
                      "Male" = "m",
                      "Female" = "f"
                    )
                  ),
                  selected = c("60", "65", "70", "75", "80", "m", "f"),
                  options = list(
                    `actions-box` = T,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = T
                ))
              ),
              br(),
              column(
                12,
                shinyjs::disabled(pickerInput(
                  inputId = "od_c_real_type_1",
                  label = "Card type",
                  choices = list(
                    "Concessionary" = "con",
                    "Non-Concessionary" = "non",
                    "Disability" = "dis"
                  ),
                  selected = c("con", "non", "dis"),
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = T
                ))
              ),
              column(
                12,
                shinyjs::disabled(sliderInput("od_c_real_time_1",
                                              label = "Time of Day",
                                              min = 0, max = 24, post = ":00", value = c(0, 24)
                ))
              )
            ),
            box(
              width = 3, title = "Comparison", id = "od_box_3_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                radioGroupButtons(
                  label = "Compare flows to another time period?",
                  inputId = "od_c_real_version",
                  choices = list("Yes" = "comp", "No" = "static"),
                  selected = "static",
                  justified = T,
                  individual = T,
                  width = "100%",
                  status = "primary",
                  checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"),
                    no = icon("remove", lib = "glyphicon")
                  )
                )
              ),
              br(),
              column(
                12,
                airMonthpickerInput("od_c_real_date_start_2",
                                    label = "Start month of 2nd period",
                                    value = "2015-12-01",
                                    maxDate = "2016-08-01",
                                    minDate = "2014-01-01"
                )
              )
            ),
            box(
              width = 3, title = "Plot", id = "od_box_4_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                shinyjs::disabled(pickerInput(
                  inputId = "od_c_real_scale",
                  label = "Spatial scale ",
                  width = "100%",
                  choices = list(
                    "OA" = "oa",
                    "LSOA" = "lsoa",
                    "MSOA" = "msoa"
                  ),
                  selected = "lsoa",
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = F
                ))
              ),
              column( 
                12,
                numericInput(inputId= "od_c_real_nrows", 
                             label = 'Top % of flows', 
                             value = '1', min = 0, max = 100, step = 1,
                             width = '100%')
              ),
              column(
                12,
                actionButton("od_help_real", "Help", width = "100%"),
                actionButton("od_c_real_s_update", "Update",
                             width = "100%",
                             style = "color: #fff; background-color: 	#d9534f; border-color: #B40404"
                )
              )
            )
          ),
          # Second row contains maps and data output.
          fluidRow(
            tabBox(
              width = 12,
              tabPanel(
                title = "Map",
                absolutePanel(
                  top = "65px", left = "80px", style = "z-index: 1000;",
                  selectInput(
                    inputId = "od_c_real_zoom_to", label = NULL, width = "150px",
                    choices = list(
                      "West Midlands CA" = 0, "Birmingham" = 1, "Dudley" = 2,
                      "Coventry" = 3, "Sandwell" = 4, "Solihull" = 5,
                      "Walsall" = 6, "Wolverhampton" = 7
                    ),
                    selected = 0
                  )
                ),
                absolutePanel(
                  top = "65px", id = "od_map_id_real", width = 300, right = "40px", style = "z-index: 999; font-size = 5px",
                  box(
                    title = "Click on the map to view details", id = "od_map_real_toggle",
                    width = 320, style = "background: transparent; font-size: 8px;", status = "primary", solidHeader = T,
                    addSpinner(rbokehOutput("inbound_bokeh_plot_real", height = 250, width = 290), spin = "circle", color = "#E41A1C"),
                    addSpinner(rbokehOutput("outbound_bokeh_plot_real", height = 250, width = 290), spin = "circle", color = "#E41A1C")
                    , collapsible = T, collapsed = T
                  )
                ),
                addSpinner(leafletOutput("map_com_real", height = 600), spin = "circle", color = "#E41A1C")
              ),
              tabPanel(
                "Data",
                dataTableOutput("od_c_real_tbl"),
                downloadButton("od_c_real_downloader", "Download Data")
              )
            )
          )
        ),
        tabItem(
          tabName = "tab_access",
          tags$h4("Measure local access to services.", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.1em;"),
          tags$style(type = "text/css", "#ac_map {height: calc(70vh - 80px) !important}
                     #map_elig {height: calc(70vh - 80px) !important}; 
                     #test {position: absolute}"),
          fluidRow(
            box(
              width = 3, title = "What?", id = "ac_box_1", status = "primary", solidHeader = T, height = "140px",
              column(
                12,
                pickerInput(
                  inputId = "ac_type",
                  label = "Select service",
                  width = "100%",
                  choices = list(
                    "Clinic" = "Clinic",
                    "GP" = "GP",
                    "Hospital" = "Hospital",
                    "Retail Centre" = "Retail Centre",
                    "Train Station" = "Train Station",
                    "Supermarket (Small)" = "Supermarket (Small)",
                    "Supermarket (Medium)" = "Supermarket (Medium)",
                    "Supermarket (Large)" = "Supermarket (Large)",
                    "Supermarket (Very Large)" = "Supermarket (Very Large)"
                  ),
                  selected = "Hospital",
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = F
                )
              )
            ),
            box(
              width = 3, title = "Comparison", id = "ac_box_2", status = "primary", solidHeader = T, height = "140px",
              radioGroupButtons(
                label = "Compare accessibility to another time period?",
                inputId = "ac_c_version",
                choices = list("Yes" = "comp", "No" = "static"),
                selected = "static",
                width = "100%",
                individual = T,
                justified = T, status = "primary",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
              )
            ),
            box(
              width = 4, title = "When?", id = "ac_box_3", status = "primary", solidHeader = T, height = "140px",
              shinyjs::hidden(sliderTextInput(
                inputId = "ac_date",
                label = "Date(s)",
                grid = T,
                force_edges = T,
                choices = c(
                  "2010-10-05", "2011-10-04", "2013-10-08", "2014-01-07", "2014-04-15", "2014-07-08",
                  "2014-10-07", "2015-01-13", "2015-04-21", "2015-07-07", "2015-10-06", "2016-01-19", "2016-04-12",
                  "2016-07-19", "2016-10-11"
                )
              )),
              sliderTextInput(
                inputId = "ac_date_comp",
                label = "Date(s)",
                grid = T,
                force_edges = T,
                choices = c(
                  "2010-10-05", "2011-10-04", "2013-10-08", "2014-01-07", "2014-04-15", "2014-07-08",
                  "2014-10-07", "2015-01-13", "2015-04-21", "2015-07-07", "2015-10-06", "2016-01-19", "2016-04-12",
                  "2016-07-19", "2016-10-11"
                ), selected = c("2011-10-04", "2016-01-19")
              )
            ),
            box(
              width = 2, title = "Plot", id = "ac_box_4", status = "primary", solidHeader = T, height = "140px",
              actionButton("access_help", "Help", align = "center", width = "100%"),
              actionButton("ac_c_s_update", "Update",
                           align = "center", width = "100%",
                           style = "color: #fff; background-color: 	#d9534f; border-color: #B40404"
              )
            )
          ),
          fluidRow(
            tabBox(
              id = "test", width = 12,
              tabPanel(
                title = "Map",
                absolutePanel(
                  top = "65px", left = "80px", style = "z-index: 999;",
                  selectInput(
                    inputId = "ac_zoom_to", label = NULL, width = "150px",
                    choices = list(
                      "West Midlands CA" = 0, "Birmingham" = 1, "Dudley" = 2,
                      "Coventry" = 3, "Sandwell" = 4, "Solihull" = 5,
                      "Walsall" = 6, "Wolverhampton" = 7
                    ),
                    selected = 0
                  )
                ),
                addSpinner(leafletOutput("ac_map"), spin = "circle", color = "#E41A1C"),
                absolutePanel(
                  top = "65px", width = 300, right = "40px", style = "z-index: 999; font-size = 5px", id = "ac_map_id",
                  box(
                    title = "Click on the map to view details", id = "ac_map_toggle",
                    width = 320, style = "background: transparent; font-size: 8px;", status = "primary", solidHeader = T,
                    addSpinner(rbokehOutput("access_bokeh_plot", height = 250, width = 290),
                               spin = "circle",
                               color = "#E41A1C"
                    ), collapsible = T, collapsed = T
                  )
                )
              ),
              tabPanel(
                title = "Data",
                dataTableOutput("ac_tbl"),
                downloadButton("access_downloader", "Download Data")
              )
            )
          )
          ), tabItem(
            tabName = "tab_elig",
            tags$h4("View local eligibility to concessionary travel.", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.1em;"),
            fluidRow(
              box(
                width = 3, title = "What?", id = "elig_box_0", solidHeader = T, status = "primary", height = 140,
                radioGroupButtons(
                  inputId = "elig_source",
                  label = "What eligibility?",
                  choices = list("Historical" = "estimate", "Forecasts" = "forecast"),
                  selected = "estimate",
                  width = "100%",
                  individual = T,
                  justified = T, status = "primary",
                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                )
              ),
              box(
                width = 3, title = "Comparison", id = "elig_box_1", solidHeader = T, status = "primary", height = 140,
                radioGroupButtons(
                  inputId = "elig_version",
                  label = "Compare eligibility to another time period?",
                  choices = list("Yes" = "comp", "No" = "static"),
                  selected = "static",
                  width = "100%",
                  individual = T,
                  justified = T, status = "primary",
                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                )
              ),
              box(
                width = 4, title = "When?", id = "elig_box_2", solidHeader = T, status = "primary", height = 140,
                shinyjs::hidden(sliderTextInput(
                  inputId = "elig_year",
                  label = "Date(s)",
                  grid = T,
                  force_edges = T,
                  choices = c(2008:2016), selected = c(2013)
                )),
                sliderTextInput(
                  inputId = "elig_year_comp",
                  label = "Date(s)",
                  grid = T,
                  force_edges = T,
                  choices = c(2008:2016), selected = c(2010:2013)
                ),
                shinyjs::hidden(sliderTextInput(
                  inputId = "elig_forecast_year",
                  label = "Date(s)",
                  grid = T,
                  force_edges = T,
                  choices = c(2016:2041), selected = c(2016)
                )),
                shinyjs::hidden(sliderTextInput(
                  inputId = "elig_year_comp_forecast",
                  label = "Date(s)",
                  grid = T,
                  force_edges = T,
                  choices = c(2016:2041), selected = c(2016, 2020)
                ))
              ),
              box(
                width = 2, title = "Plot", id = "elig_box_3", solidHeader = T, status = "primary", height = 140,
                actionButton("elig_help", "Help", width = "100%"),
                actionButton("elig_update", "Update",
                             width = "100%",
                             style = "color: #fff; background-color: 	#d9534f; border-color: #B40404"
                )
              )
            ),
            fluidRow(
              tabBox(
                width = 12,
                tabPanel(
                  title = "Map", width = 12,
                  absolutePanel(
                    top = "65px", left = "80px", style = "z-index: 1000;",
                    selectInput(
                      inputId = "elig_zoom_to", label = NULL, width = "150px",
                      choices = list(
                        "West Midlands CA" = 0, "Birmingham" = 1, "Dudley" = 2,
                        "Coventry" = 3, "Sandwell" = 4, "Solihull" = 5,
                        "Walsall" = 6, "Wolverhampton" = 7
                      ),
                      selected = 0
                    )
                  ),
                  addSpinner(leafletOutput("map_elig"), spin = "circle", color = "#E41A1C"),
                  absolutePanel(
                    top = "65px", width = 300, right = "40px", style = "z-index: 999; font-size = 5px", id = "elig_map_id",
                    box(
                      title = "Click on the map to view details", id = "map_elig_toggle", width = 320,
                      style = "background: transparent; font-size: 8px;", status = "primary", solidHeader = T,
                      addSpinner(rbokehOutput("c_bokeh_plot", height = 250, width = 290), spin = "circle", color = "#E41A1C"),
                      collapsible = T, collapsed = T
                    )
                  )
                ),
                tabPanel(
                  title = "Data", width = 12,
                  dataTableOutput("elig_s_tbl"),
                  downloadButton("elig_downloader", "Download Data")
                )
              )
            )
          ),
        # Third tab content: Details about how to use tool and credits
        tabItem(
          tabName = "tab_about",
          p("This dashboard was developed as part of the research project 'Inclusive and Healthy Mobility: Understanding Trends in Concessionary Travel in the West Midlands', conducted at University College London in partnership with Transport for West Midlands. Click here to find more information about the project."),
          p("The dashboard was created using", tags$a(href = "https://shiny.rstudio.com/", "R Studio Shiny", target="_blank"), "by ", tags$a(href = "https://www.linkedin.com/in/alistairleak/", "Alistair Leak.", target="_blank")),
          p("Research team: ", tags$a(href = "https://www.ucl.ac.uk/bartlett/casa/jens-kandt", "Jens Kandt", target="_blank"), " (PI), ", tags$a(href = "https://www.geog.ucl.ac.uk//people/academic-staff/paul-longley", "Paul Longley", target="_blank"), " (Co-I), Alistair Leak, ", tags$a(href = "https://www.ucl.ac.uk/geospatial-analytics/people/ffion-carney", "Ffion Carney", target="_blank"), " - University College London"),
          p("Project partners: Chris Lane (Co-I), Daniel Pass, Phillip Evans, Anne Schweickert, Robert Walker  -  Transport for West Midlands"),
          h4("Data Sources"),
          tabBox(
            width = 12,
            
            tabPanel(
              "Who Travels",
              p("The trends are based on anonymous bus and tram boardings recorded on smart cards of passengers registered under the English National Concessionary Travel Scheme (ENCTS) in the West Midlands Combined Authority between 2009 and 2016.")
            ),
            tabPanel(
              "Where and When",
              p("Monthly origin-destination flows of ENCTS passengers between LSOAs between January 2014 and AugustS 2016."),
              p("The data record the frequency of journeys between LSOAs where it was possible to determine both journeys' origins and destinations. These counts do not represent all journeys and are limited to a specific service provider.")
            ),
            tabPanel(
              "Access to Services",
              p("Estimates of travel time to selected services based on bus timetables and walking trips along the ITN road network (2010-2016)."),
              p("Travel time is the mean minimum time to reach the selected service across a 24-hour period calculated at 30 minute intervals."),
              tags$ul(
                tags$li("Supermarkets: TBC"),
                tags$li("GPs, Clinics and Hospitals: TBC"),
                tags$li("Retail Centres: TBC"),
                tags$li("Train Stations: TBC")
              )
            ),
            tabPanel(
              "Who is Eligible",
              p("Residents eligible for ENCTS in the West Midlands Combined Authority"),
              tags$ul(
                tags$li("2010 to 2016 based on ONS mid-year population estimates."),
                tags$li("2016 to 2041 based on ONS population forecasts.")
              )
            ),
            tabPanel(
              "General",
              p("Office for National Statistics"),
              tags$ul(
                tags$li("Administrative boundaries (Output Area, Lower Super Output Area, Middle Super Output Area)"),
                tags$li("Administrative Area Centroids (Output Area, Lower Super Output Area, Middle Super Output Area)")
              )
            )
          )
        )
        )
    ),
    skin = "blue"
  )
}


# Server does all the backend work,
server <- function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  session$allowReconnect(T)
  
  # Function to range standardise frequencies for plotting.
  range01 <- function(x) {
    (x - min(x)) / (((max(x) - min(x))))
  }
  
  
  ###########################################################################
  # Who Travels -------------------------------------------------------------
  ###########################################################################
  
  
  # These will be used once we have day resolution data.
  
  dygraph_data_1 <- reactive({
    
    data <- setDT(dbGetQuery(con, glue("SELECT gender, SUM({input$dygraph_statistic}) AS value, CAST(DATE_TRUNC('{input$dygraph_time_resolution}', date) AS DATE) AS date FROM {schema_name}.dygraph_gender ",
                                 "WHERE type='{input$dygraph_type}' GROUP BY gender, CAST(DATE_TRUNC('{input$dygraph_time_resolution}', date) AS DATE);")))
    
    data <- dcast(data, date ~ gender, value.var = c("value"), fun.aggregate = sum)
    
    data
  })
  
  
  dygraph_data_2 <- reactive({
    
    data <- setDT(dbGetQuery(con, glue("SELECT age, SUM({input$dygraph_statistic}) AS value, CAST(DATE_TRUNC('{input$dygraph_time_resolution}', date) AS DATE) AS date FROM {schema_name}.dygraph_age ",
                                 "WHERE type='{input$dygraph_type}' GROUP BY age, CAST(DATE_TRUNC('{input$dygraph_time_resolution}', date) AS DATE);")))
    
    data <- dcast(data, date ~ age, value.var = c("value"), fun.aggregate = sum)
    
    data
  })
  
  
  # First interactive graph based on gender
  output$dygraph_gender <- renderDygraph({
    
    # Subset DT for msoas and aggregate by date and gender.
    data <- dygraph_data_1()
    
    if (input$dygraph_statistic == "journeys_pp") {
      stack_case <- FALSE
      dygraph_statistic <- "Journeys per Person"
    } else if (input$dygraph_statistic == "passengers") {
      stack_case <- FALSE
      dygraph_statistic <- "Passengers"
    } else {
      stack_case <- TRUE
      dygraph_statistic <- "Journeys"
    }
    
    
    
    # Create dygraph based on date and gender
    dygraph(
      data = data,
      main = paste0(
        str_to_title(input$dygraph_type),
        " ", dygraph_statistic,
        " by Gender (", str_to_title(input$dygraph_time_resolution), ")"
      ),
      group = "dygraphs"
    ) %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = F
      ) %>%
      dyOptions(stackedGraph = stack_case)
  })
  
  
  # Second interactive graph based on age
  output$dygraph_age <- renderDygraph({
    
    # Subset DT for msoas and aggregate by date and gender.
    data <- dygraph_data_2()
    
    if (input$dygraph_statistic == "journeys_pp") {
      stack_case <- FALSE
      dygraph_statistic <- "Journeys per Person"
    } else if (input$dygraph_statistic == "passengers") {
      stack_case <- FALSE
      dygraph_statistic <- "Passengers"
    } else {
      stack_case <- TRUE
      dygraph_statistic <- "Journeys"
    }
    
    # Create dygraph based on date and age
    dygraph(
      data = data,
      main = paste0(
        str_to_title(input$dygraph_type),
        " ", dygraph_statistic,
        " by Age (", str_to_title(input$dygraph_time_resolution), ")"
      ),
      group = "dygraphs"
    ) %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = F
      ) %>%
      dyOptions(stackedGraph = stack_case) %>%
      dyRangeSelector()
  })
  
 
  
  #############################################################################
  ##### Who travels Origin - Destination real data ----------------------------
  #############################################################################
  

  # Comparative OD data Map
  output$map_com_real <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-1.703929, 52.447027, zoom = 10)
  })
  
  observe({
    if (input$od_c_real_zoom_to == 0) {
      leafletProxy("map_com_real") %>% flyTo(lng = -1.891587, lat = 52.484689, zoom = 10)
    } else if (input$od_c_real_zoom_to == 1) {
      leafletProxy("map_com_real") %>% flyTo(lng = -1.890401, lat = 52.486243, zoom = 13L)
    } else if (input$od_c_real_zoom_to == 2) {
      leafletProxy("map_com_real") %>% flyTo(lng = -2.081112, lat = 52.512255, zoom = 13L)
    } else if (input$od_c_real_zoom_to == 3) {
      leafletProxy("map_com_real") %>% flyTo(lng = -1.519693, lat = 52.406822, zoom = 13L)
    } else if (input$od_c_real_zoom_to == 4) {
      leafletProxy("map_com_real") %>% flyTo(lng = -2.010793, lat = 52.536167, zoom = 13L)
    } else if (input$od_c_real_zoom_to == 5) {
      leafletProxy("map_com_real") %>% flyTo(lng = -1.777610, lat = 52.411811, zoom = 13L)
    } else if (input$od_c_real_zoom_to == 6) {
      leafletProxy("map_com_real") %>% flyTo(lng = -1.982919, lat = 52.586214, zoom = 13L)
    } else if (input$od_c_real_zoom_to == 7) {
      leafletProxy("map_com_real") %>% flyTo(lng = -2.128820, lat = 52.586973, zoom = 13L)
    }
  })
  
  
  observeEvent(input$od_c_real_period, {
    if (input$od_c_real_period != "custom") {
      shinyjs::show("od_c_real_date_start_1")
      shinyjs::hide("od_c_real_date_start_custom")
    } else {
      shinyjs::show("od_c_real_date_start_custom")
      shinyjs::hide("od_c_real_date_start_1")
    }
  })
  
  
  data_od_s_real <- reactive({
    
    #no demog_params in agg_dash parameters
    #age_string <- toString(sprintf("'%s'", input$od_c_real_demographics[grep("^[0-9]+$",input$od_c_real_demographics)]))
    
    #gender_string <- toString(sprintf("'%s'", input$od_c_real_demographics[grep("^[A-Z]+$",input$od_c_real_demographics)]))
    
    
    #do the main query
    DT2_s <- setDT(dbGetQuery(con, glue("SELECT j1.sum_nf, j1.origin, j1.destination, j1.orig_lat, j1.orig_lon, st_x(y1.geometry) AS dest_lon, st_y(y1.geometry) AS dest_lat ",
                                        "FROM (SELECT flow1.sum_nf, flow1.origin, flow1.destination, st_x(x1.geometry) AS orig_lon, st_y(x1.geometry) AS orig_lat ",
                                        "FROM (SELECT SUM(flow) AS sum_nf, origin, destination ",
                                        "FROM {schema_name}.lsoa_flows ",
                                        "WHERE (date BETWEEN '{as.Date(input$od_c_real_date_start_1[[1]])}' AND DATE '{as.Date(input$od_c_real_date_start_1[[1]])}' + INTERVAL '{as.numeric(input$od_c_real_period)-1} months') ",
                                        "GROUP BY origin, destination) flow1 ",
                                        "LEFT JOIN {schema_name}.all_points_wgs x1 ON (flow1.origin=x1.area_code)) j1 ",
                                        "LEFT JOIN {schema_name}.all_points_wgs y1 ON (j1.destination=y1.area_code) ",
                                        "ORDER BY sum_nf DESC;")))
    
    DT2_s <- na.omit(DT2_s, cols=c("origin", "destination", "orig_lon", "orig_lat", "dest_lon", "dest_lat"))
    
  })
  
  
  data_od_c_real <- reactive({
    
    #no demogs in agg_dash get parameters
    #age_string <- toString(sprintf("'%s'", input$od_c_real_demographics[grep("^[0-9]+$",input$od_c_real_demographics)]))
    
    #gender_string <- toString(sprintf("'%s'", input$od_c_real_demographics[grep("^[A-Z]+$",input$od_c_real_demographics)]))
    
    #do the main query
    
    DT2_c <- setDT(dbGetQuery(con, glue("SELECT coord1.perc_change, coord1.n_change, coord1.sum_nf_first AS n1, coord1.sum_nf_second AS n2, coord1.origin AS origin, coord1.destination AS destination, coord1.orig_lat, coord1.orig_lon, st_x(y1.geometry) AS dest_lon, st_y(y1.geometry) AS dest_lat FROM ",
                                        "(SELECT ROUND(((j1.sum_nf_second-j1.sum_nf_first)/j1.sum_nf_first)*100.0) AS perc_change, j1.sum_nf_second-j1.sum_nf_first AS n_change, j1.sum_nf_second, j1.sum_nf_first, j1.origin_first AS origin, j1.destination_first AS destination, st_x(x1.geometry) AS orig_lon, st_y(x1.geometry) AS orig_lat FROM ",
                                        "((SELECT CAST(SUM(flow) AS float) AS sum_nf_first, origin AS origin_first, destination AS destination_first ",
                                        "FROM {schema_name}.lsoa_flows  ",
                                        "WHERE (date BETWEEN '{as.Date(input$od_c_real_date_start_1[[1]])}' AND DATE '{as.Date(input$od_c_real_date_start_1[[1]])}' + INTERVAL '{as.numeric(input$od_c_real_period)-1} months') ",
                                        "GROUP BY origin, destination) first1 INNER JOIN  ",
                                        "(SELECT CAST(SUM(flow) AS float) AS sum_nf_second, origin, destination ",
                                        "FROM {schema_name}.lsoa_flows ",
                                        "WHERE AND (date BETWEEN '{as.Date(as.Date(input$od_c_real_date_start_2[[1]]))}' AND DATE '{as.Date(as.Date(input$od_c_real_date_start_2[[1]]))}' + INTERVAL '{as.numeric(input$od_c_real_period)-1} months') ",
                                        "GROUP BY origin, destination) second1 ON (first1.origin_first=second1.origin AND first1.destination_first=second1.destination)) j1 ",
                                        "LEFT JOIN {schema_name}.all_points_wgs x1 ON (j1.origin=x1.area_code)) coord1 ",
                                        "LEFT JOIN {schema_name}.all_points_wgs y1 ON (coord1.destination=y1.area_code) ",
                                        "ORDER BY ABS(coord1.n_change) DESC;")))
    
    DT2_c <- na.omit(DT2_c, cols=c("origin", "destination", "orig_lon", "orig_lat", "dest_lon", "dest_lat"))
    
    print("comp data load success!")
  })
  
  
  # Update map to reflect new criteria. (Static)
  
  observeEvent(input$od_c_real_s_update, {
    shinyjs::hide("od_map_real_toggle", anim = T, animType = "slide")
    
    print("test working!")
    
    
    if (input$od_c_real_version == "static") {
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {
        
        # Update progress bar.
        incProgress(1 / 4, message = "Importing data.")
        
        #request static data based on specified parameters
        data <- data_od_s_real()[,head(.SD, round(input$od_c_real_nrows/100*nrow(data_od_s_real()))),]
        
        # Update progress bar.
        incProgress(1 / 4, message = "Sampling for visualisation.")
        
        
        # Create domain for legend
        domain <- c(min(data$sum_nf), max(data$sum_nf))
        
        # subset markers based on scale.
        mrks <- dbGetQuery(con, glue("SELECT st_x(geometry) AS longitude, st_y(geometry) AS latitude, area_code FROM {schema_name}.all_points_wgs WHERE scale='lsoa';"))
        
        # Update progress bar.
        incProgress(1 / 4, message = "Building Map.")
        
        # Update static map with new lines data.
        map <- leafletProxy("map_com_real", data = data) %>%
          clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          clearFlows() %>%
          addCircleMarkers( 
            lng = mrks$longitude,
            lat = mrks$latitude,
            layerId = mrks$area_code,
            label = mrks$area_code, 
            radius = 3, weight = 0.1, color='black'  
          ) %>%
          addFlows(lng0 = data$orig_lon,
                   lat0 = data$orig_lat,
                   lng1 = data$dest_lon,
                   lat1 = data$dest_lat,
                   flow = data$sum_nf, 
                   color = colorNumeric("viridis", data$sum_nf)(data$sum_nf),
                   dir = 1,
                   maxThickness = 5,
                   opacity = 1
          ) %>%
          leaflet::addLegend("bottomright",
                             layerId = "od_map_real_legend",
                             pal = colorNumeric("viridis", domain),
                             values = domain,
                             title = "Journeys",
                             opacity = 0.8
          )
        
        # Update progress bar.
        incProgress(1 / 4, message = "Done.") 
      })
    }
  })
  
  # Update map to reflect new criteria.
  observeEvent(input$od_c_real_s_update, {
    if(input$od_c_real_version=="comp"){
      shinyjs::hide("od_map_real_toggle", anim = T, animType = "slide")
      
      
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {
        
        # Update progress bar.
        incProgress(1 / 4, message = "Importing Map Data")
        #10
        data <- data_od_c_real()[, head(.SD, round(input$od_c_real_nrows/100*nrow(data_od_c_real()))) ,]
        
        # Update progress bar.
        incProgress(1 / 4, message = "Sampling for visualisation.")
        
        # Create the colour breaks
        binpal <- colorBin("RdYlBu", bins = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, Inf), reverse=TRUE)
        
        # subset markers based on scale.
        mrks <- dbGetQuery(con, glue("SELECT st_x(geometry) AS longitude, st_y(geometry) AS latitude, area_code FROM {schema_name}.all_points_wgs WHERE scale='lsoa';"))
        
        # Update progress bar.
        incProgress(1 / 4, message = "Building Map.")
        
        
        map_com <- leafletProxy("map_com_real", data = data) %>%
          clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          clearFlows() %>%
          addCircleMarkers(   # 
            lng = mrks$longitude,
            lat = mrks$latitude,
            layerId = mrks$area_code,
            label = mrks$area_code,
            radius = 1.5, weight = 0.1, color = "grey"
          ) %>%
          leaflet::addLegend("bottomright",
                             layerId = "od_map_legend",
                             pal = colorBin("RdYlBu", bins = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, Inf), reverse=TRUE),
                             values = data$perc_change,
                             title = "Change (%)",
                             opacity = 0.8
          ) %>%
          addFlows(lng0 = data$orig_lon,
                   lat0 = data$orig_lat,
                   lng1 = data$dest_lon,
                   lat1 = data$dest_lat,
                   flow = data$n_change, 
                   color = binpal(data$perc_change),
                   dir = 1,
                   maxThickness = 5,
                   opacity =.5,
                   popup = popupArgs(labels = "Change in Flow",
                                     supValues = data.frame("PercChange" = data$perc_change, "OrigFlow" = data$n1, "CompFlow" = data$n2),
                                     supLabels = c("Percentage Change", "Original Flow", "Comparison Flow"))
          ) %>%
          showGroup(input$od_c_real_scale)
      })
    }
  })
  
  
  # Add lines to the destination map. Where do people go to?
  
  
  #need to sort this bit out!
  ################################
  observeEvent(input$map_com_real_marker_click, {
    shinyjs::show("od_map_real_toggle", anim = T, animType = "slide")
    
    print(glue("input click on {input$map_com_real_marker_click[[1]]}"))
    
    data2a <- data_od_s_real()[origin==input$map_com_real_marker_click[[1]], ,][,`:=`(dir="outbound", colour="#ff0000"),]
    
    print("dat 1")
    data2b <- data_od_s_real()[destination==input$map_com_real_marker_click[[1]], ,][,`:=`(dir = "inbound", colour = "#00ff00"),]
    
    data2 <- rbind(data2a, data2b)
    
    print("successfully bound data")
    
    if (nrow(data2) > 0) {
      
      # subset markers based on scale.
      mrks <- dbGetQuery(con, glue("SELECT st_x(geometry) AS longitude, st_y(geometry) AS latitude, area_code FROM {schema_name}.all_points_wgs WHERE scale='lsoa';"))
      
      print("got marks")
      
      # Update od outbound map with new lines data.
      map <- leafletProxy("map_com_real", data = data2) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addFlows(lng0 = data$orig_lon,
                 lat0 = data$orig_lat,
                 lng1 = data$dest_lon,
                 lat1 = data$dest_lat,
                 flow = data$sum_nf, 
                 color = data$colour,
                 dir = 1,
                 maxThickness = 5,
                 opacity = 1
        ) %>%
        addCircleMarkers(
          lng = mrks$longitude,
          lat = mrks$latitude,
          layerId = mrks$area_code,
          label = mrks$area_code,
          radius = 2, weight = 2
        )
    }
  })
  
  
  
  output$od_c_real_tbl <- renderDT(
    
    if (input$od_c_real_version == "comp") {
      
      #make readable datatable
      data_od_c_real()[, .(origin, 
                           destination,
                           T1_period = paste0(input$od_c_real_date_start_1, " +", as.numeric(input$od_c_real_period)-1, 'm'),
                           T2_period = paste0(as.Date(input$od_c_real_date_start_2), " +", as.numeric(input$od_c_real_period)-1, 'm'),
                           T1 = n1, 
                           T2 = n2, 
                           n_change,
                           perc_change)]
      
    } else if (input$od_c_real_version == "static") {
      
      #make readable datatable
      data_od_s_real()[, .(origin, 
                           destination,
                           T1 = sum_nf,
                           period = paste(input$od_c_real_date_start_1, " +", as.numeric(input$od_c_real_period)-1, 'm'))]
      
      
    }
    ,
    options = list(pageLength = 10, scrollX = T)
  )
  
  
  # Comparative Map, data download handler
  output$od_c_real_downloader <- downloadHandler(
    filename = function() {
      paste0(input$od_c_real_version, "_od.csv")
    },
    content = function(file) {
      fwrite(
        if (input$od_c_real_version == "comp") {
          
          #new data.table version
          data_od_c_real()[,.(origin, 
                              destination,
                              T1_period = paste0(input$od_c_real_date_start_1, " +", input$od_c_real_period-1, 'd'),
                              T2_period = paste0(as.Date(input$od_c_real_date_start_2), " +", input$od_c_real_period-1, 'd'),
                              T1 = n1, 
                              T2 = n2,
                              n_change,
                              perc_change)]
          
          } else if (input$od_c_real_version == "static") {
          
            #data table version
            data_od_s_real()[,.(origin, 
                                destination,
                                T1 = sum_nf,
                                period = paste(input$od_c_real_date_start_1, " +", input$od_c_real_period-1, 'd'))]
            
        }, file
      )
    }
  )
  
  
  
  observeEvent(input$map_com_real_marker_click, {
    leafletProxy("map_com_real") %>% clearControls()
  })
  
  # eligibility Plot change graph
  plot_data3_real <- reactive({
    data2a <- data_od_s_real()
    data2a <- data2a[origin == input$map_com_real_marker_click[[1]], ,][,`:=`(dir = "outbound", colour = "#ff0000"),][order(-sum_nf)][,head(.SD,10),]
  })
  
  # eligibility Plot change graph
  plot_data4_real <- reactive({
    data2b <- data_od_s_real()
    data2b <- data2b[destination == input$map_com_real_marker_click[[1]], ,][,`:=`(dir = "inbound", colour = "#00ff00"),][order(-sum_nf)][,head(.SD,10),]
  })
  
  output$inbound_bokeh_plot_real <- renderRbokeh({
    figure(
      data = plot_data4_real(),
      legend_location = "right",
      xlab = "",
      ylab = "Inbound Count"
    ) %>%
      ly_bar(
        x = as.character(origin),
        y = sum_nf,
        fill_color = "#ff0000"
      ) %>%
      theme_axis("x", major_label_orientation = 45)
  })
  
  output$outbound_bokeh_plot_real <- renderRbokeh({
    figure(
      data = plot_data3_real(),
      legend_location = "right",
      xlab = "",
      ylab = "Outbound Count"
    ) %>%
      ly_bar(
        x = as.character(destination),
        y = sum_nf,
        fill_color = "#00ff00"
      ) %>%
      theme_axis("x", major_label_orientation = 45)
  })
  
  
  ### Help tour for Origin Destination data
  od_steps_real <- reactive(data.frame(
    element = c("imaginary", "#od_box_1_real", "#od_box_2_real", "#od_box_3_real", "#od_box_4_real", "#od_map_id_real"),
    intro = c(
      "The purpose of this tab is to explore mobility patterns across the West Midlands Combined Authority. You are able to create and query origin-destination flows based on dates, times and passenger characteristics.",
      "First, specify the details of the date and time period you are interested in. The time period refers to the interval for which all flows will be counted, starting from the specified date.",
      "Next, you can filter flows by passenger gender, age, card time and the time of day.",
      "Do you wish to compare flows to another time period? If so, choose 'Yes', and specify the start date for the comparison. Note, the period and passenger characteristics will remain the same as for period 1.",
      "Last, specify plotting details. You can select from a range of spatial scales and the set the maximum number of flows to be plotted between origin-destination pairs. Once you have made your selections, hit 'Update'.",
      "Once you have updated your map, click on an area to view details."
    )
  ))
  
  observeEvent(input$od_help_real, {
    introjs(session, options = list(steps = od_steps_real(), "showBullets" = "false", "showProgress" = "true", "showStepNumbers" = "false", "nextLabel" = "Next", "prevLabel" = "Prev"))
  })
  
  
  
  
  
  
  
  
  ###########################################################################
  # Accessibility -----------------------------------------------------------
  ###########################################################################
  
  observeEvent(input$ac_c_version, {
    shinyjs::toggle("ac_date_comp")
    shinyjs::toggle("ac_date")
  })
  
  output$ac_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -1.891587, lat = 52.484689, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  
  observe({
    if (input$ac_zoom_to == 0) {
      leafletProxy("ac_map") %>% flyTo(lng = -1.891587, lat = 52.484689, zoom = 10)
    } else if (input$ac_zoom_to == 1) {
      leafletProxy("ac_map") %>% flyTo(lng = -1.890401, lat = 52.486243, zoom = 13L)
    } else if (input$ac_zoom_to == 2) {
      leafletProxy("ac_map") %>% flyTo(lng = -2.081112, lat = 52.512255, zoom = 13L)
    } else if (input$ac_zoom_to == 3) {
      leafletProxy("ac_map") %>% flyTo(lng = -1.519693, lat = 52.406822, zoom = 13L)
    } else if (input$ac_zoom_to == 4) {
      leafletProxy("ac_map") %>% flyTo(lng = -2.010793, lat = 52.536167, zoom = 13L)
    } else if (input$ac_zoom_to == 5) {
      leafletProxy("ac_map") %>% flyTo(lng = -1.777610, lat = 52.411811, zoom = 13L)
    } else if (input$ac_zoom_to == 6) {
      leafletProxy("ac_map") %>% flyTo(lng = -1.982919, lat = 52.586214, zoom = 13L)
    } else if (input$ac_zoom_to == 7) {
      leafletProxy("ac_map") %>% flyTo(lng = -2.128820, lat = 52.586973, zoom = 13L)
    }
  })
  
  # Create Reactive table for static accessibility
  s_data <- reactive({
    
    ac_dt <- setDT(dbGetQuery(con, glue("SELECT mean_min_time, origin AS area_code FROM {schema_name}.access_data WHERE date='{input$ac_date}' AND type='{input$ac_type}';")))
    
    ac_dt

  })
  
  # Create Reactive table for comparative accessibility
  s_data_comp <- reactive({
    
    ac_dt <- setDT(dbGetQuery(con, glue("SELECT j1.start_mean_min_time-acc2.mean_min_time AS change_min_time, j1.start_mean_min_time AS mean_min_time_start, acc2.mean_min_time AS mean_min_time_end, j1.origin AS area_code FROM
                                    (SELECT acc1.mean_min_time AS start_mean_min_time, acc1.origin FROM
                                        {schema_name}.access_data acc1 WHERE date='{as.Date(input$ac_date_comp[[1]])}' AND type='{input$ac_type}') j1 
                                        LEFT JOIN {schema_name}.access_data acc2 ON (j1.origin=acc2.origin) 
                                        WHERE date='{as.Date(input$ac_date_comp[[2]])}' AND type='{input$ac_type}';")))
    
    ac_dt <- na.omit(ac_dt)
    
  })
  
  output$ac_tbl <- renderDT(
    if (input$ac_c_version == "comp") {
      #data.table version
      s_data_comp()[,
                    .(area_code, 
                      type, 
                      T1_date = input$ac_date_comp[[1]], 
                      T2_date = input$ac_date_comp[[2]], 
                      T1_time = mean_min_time_start, 
                      T2_time = mean_min_time_end, 
                      Change = change_min_time)]
      
      } else if (input$ac_c_version == "static") {
      
        #data.table version
        s_data()[,
                 .(area_code, 
                   type, 
                   T1_date = as.Date(input$ac_date), 
                   T1_time = mean_min_time)]
        
    }
    ,
    options = list(pageLength = 10, scrollX = T)
  )
  
  # Comparative Map, data download handler
  output$access_downloader <- downloadHandler(
    filename = function() {
      paste0(input$ac_c_version, "_accessibility.csv")
    },
    content = function(file) {
      fwrite(
        if (input$ac_c_version == "comp") {
          #data.table version
          s_data_comp()[,
                        .(area_code, 
                          type, 
                          T1_date = input$ac_date_comp[[1]], 
                          T2_date = input$ac_date_comp[[2]], 
                          T1_time = mean_min_time_start, 
                          T2_time = mean_min_time_end, 
                          Change = change_min_time)]
          } else if (input$ac_c_version == "static") {
            
            #data.table version
            s_data()[,
                     .(area_code, 
                       type, 
                       T1_date = as.Date(input$ac_date), 
                       T1_time = mean_min_time)]
            
        }, file
      )
    }
  )
  
  # Accessibility Map update
  observeEvent(input$ac_c_s_update, {
    
    oa_cp <- st_read(dsn=con, layer=c(schema_name, "oa_boundaries"))
    
    if(input$ac_type=="Clinic"){
      markers <- st_read(dsn=con, layer=c(schema_name, "clinics"))
      mark_col <- "#e41a1c"
      markers$poi_name <- markers$Organisa_4
    }else if(input$ac_type=="GP"){
      markers <- st_read(dsn=con, layer=c(schema_name, "gps"))
      mark_col <- "#377eb8"
      markers$poi_name <- markers$Organisa_4
    }else if(input$ac_type=="Hospital"){
      markers <- st_read(dsn=con, layer=c(schema_name, "hospitals"))
      mark_col <- "#4daf4a"
      markers$poi_name <- markers$Organisa_4
    }else if(input$ac_type=="Retail Centre"){
      markers <- st_read(dsn=con, layer=c(schema_name, "retail_centres"))
      mark_col <- "#ffff33"
      markers$poi_name <- markers$Name
    }else if(input$ac_type=="Train Station"){
      markers <- st_read(dsn=con, layer=c(schema_name, "rail_stations"))
      mark_col <- "#f781bf"
      markers$poi_name <- markers$Name
    }else if(input$ac_type=="Supermarket (Small)"){
      markers <- st_read(dsn=con, layer=c(schema_name, "sm_small"))
      mark_col <- "#984ea3"
      markers$poi_name <- markers$retailer
    }else if(input$ac_type=="Supermarket (Medium)"){
      markers <- st_read(dsn=con, layer=c(schema_name, "sm_medium"))
      mark_col <- "#ff7f00"
      markers$poi_name <- markers$retailer
    }else if(input$ac_type=="Supermarket (Large)"){
      markers <- st_read(dsn=con, layer=c(schema_name, "sm_large"))
      mark_col <- "#ffff33"
    }else if(input$ac_type=="Supermarket (Very Large)"){
      markers <- st_read(dsn=con, layer=c(schema_name, "sm_very_large"))
      mark_col <- "#a65628"
      markers$poi_name <- markers$retailer
    }
    
    if (input$ac_c_version == "static") {
      
      palette <- "YlOrRd"
      reverse <- F
      oa_cp <- oa_cp %>% 
        left_join(s_data(), by = c("area_code"))
      
      domain <- c(1, max(abs(oa_cp$mean_min_time),na.rm=TRUE))
      
      legend_title <- "Travel Time (mins)"
      
      
    } else if (input$ac_c_version == "comp") {
      palette <- "RdYlGn"
      reverse <- T
      oa_cp <- oa_cp %>% 
        left_join(s_data_comp(), by = c("area_code"))
      
      #change name to simplify code
      oa_cp$mean_min_time <- oa_cp$change_min_time
      
      domain <- c((-1 * max(abs(oa_cp$mean_min_time),na.rm=TRUE)), max(abs(oa_cp$mean_min_time),na.rm=TRUE))
      
      legend_title <- "Change in Travel Time (mins)"
    }
    
    
    #make the palette
    pal <- colorNumeric(palette, domain, reverse = reverse)
    
    #generate random numbers to split up the dataframe
    #you can't display over 3,000 polygons in a single item so split into 4
    oa_cp$split_num <- sample(4, size = nrow(oa_cp), replace = TRUE)
    
    
    #generate the map
    leafletProxy("ac_map", deferUntilFlush = F) %>%
      clearControls() %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(
        data = oa_cp[oa_cp$split_num==1, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa_cp$area_code, " : ", mean_min_time, " mins.")),  
        layerId = ~area_code,
        highlightOptions = highlightOptions(color = "black", weight = 2)
      ) %>%
      addPolygons(
        data = oa_cp[oa_cp$split_num==2, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa_cp$area_code, " : ", mean_min_time, " mins.")),
        layerId = ~area_code,
        highlightOptions = highlightOptions(color = "black", weight = 2)
      ) %>%
      addPolygons(
        data = oa_cp[oa_cp$split_num==3, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa_cp$area_code, " : ", mean_min_time, " mins.")),
        layerId = ~area_code,
        highlightOptions = highlightOptions(color = "black", weight = 2)
      ) %>%
      addPolygons(
        data = oa_cp[oa_cp$split_num==4, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa_cp$area_code, " : ", mean_min_time, " mins.")),
        layerId = ~area_code,
        highlightOptions = highlightOptions(color = "black", weight = 2)
      ) %>%
      addCircleMarkers(data = markers, 
                       stroke = T, 
                       color = "black", 
                       weight = 1, 
                       fillColor = mark_col, 
                       radius = 7, 
                       fillOpacity = .8,
                       label = ~poi_name
      ) %>%
      leaflet::addLegend(
        position = "bottomright",
        title = legend_title,
        pal = pal,
        values = domain
      )
  })
  
  # Update Accessibility Legend.
  observeEvent(input$ac_c_s_update, {
    proxy <- leafletProxy("ac_map")
    
    if (input$ac_c_version == "static") {
      palette <- "YlOrRd"
      title <- "Travel Time (mins)"
      data <- s_data()
      reverse <- F
      domain <- c(1, max(abs(data$mean_min_time)))
    } else if (input$ac_c_version == "comp") {
      palette <- "RdYlGn"
      title <- html("Change in Travel Time (mins)")
      data <- s_data_comp()
      reverse <- T
      domain <- c(-1 * max(abs(data$mean_min_time)), 0, max(abs(data$mean_min_time)))
    }
    
    pal <- colorNumeric(palette, domain, reverse = reverse)
    proxy %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        title = title,
        pal = pal,
        values = domain
      )
  })
  
  observeEvent(input$ac_map_shape_click[[1]], {
    shinyjs::show("ac_map_toggle", anim = T, animType = "slide")
  })
  
  # Reactive Element for accessibility Graph.
  plot_data <- reactive({
    
    plot_data <- setDT(dbGetQuery(con, glue("SELECT date, origin, mean_min_time, type FROM {schema_name}.access_data WHERE origin='{input$ac_map_shape_click[[1]]}';")))
    
    
  })
  
  req(plot_data) 
  
  output$access_bokeh_plot <- renderRbokeh({
    figure(
      data = plot_data(),         
      legend_location = "right",
      xlab = "Date",
      ylab = "Minimum Journey Time (minutes)",
      title = input$ac_map_shape_click[[1]]
    ) %>%
      ly_lines(
        x = date,
        y = mean_min_time,
        color = type,
        width = 2
      ) %>%
      set_palette(discrete_color = pal_color(c(           
        "#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
        "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999"
      ))) %>%
      ly_points(
        x = date,
        y = mean_min_time,
        color = type,
        size = 4,
        hover = c(mean_min_time, type)
      ) %>% 
      x_axis(label=NULL)
  })
  
  ### Help tour for accessibility
  access_steps <- reactive(data.frame(
    element = c("imaginary", "#ac_box_1", "#ac_box_2", "#ac_box_3", "#ac_box_4", "#ac_map_id"),
    intro = c(
      "Measure local access to services in the West Midlands Combined Authority. You can select from a range of services and either view accessibility at one point in time or view changes in accessibility between two points in time. We define accessibility as travel time by bus including waiting time, interchange and walking to/from the bus stop. ",
      "Select a type of service from the drop-down menu.",
      "Do you want to measure changes in accessibility between two time points?",
      "What date(s) are you interested in?",
      "Update the map.",
      "Once you have updated your map, click on an area to view details."
    )
  ))
  
  observeEvent(input$access_help, {
    introjs(session,
            options =
              list(
                steps = access_steps(), "showBullets" = "false",
                "showProgress" = "true",
                "showStepNumbers" = "false",
                "nextLabel" = "Next",
                "prevLabel" = "Prev",
                "skipLabel" = "Skip"
              )
    )
  })
  
  
  ###########################################################################
  # Eligibility -----------------------------------------------------------
  ###########################################################################
  
  
  # Based on selected inputs, show the correct time slider.
  observeEvent(c(input$elig_source, input$elig_version), {
    if (input$elig_source == "estimate") {
      if (input$elig_version == "static") {
        shinyjs::show("elig_year")
        shinyjs::hide("elig_year_comp")
        shinyjs::hide("elig_forecast_year")
        shinyjs::hide("elig_year_comp_forecast")
      } else if (input$elig_version == "comp") {
        shinyjs::show("elig_year_comp")
        shinyjs::hide("elig_year")
        shinyjs::hide("elig_forecast_year")
        shinyjs::hide("elig_year_comp_forecast")
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == "static") {
        shinyjs::show("elig_forecast_year")
        shinyjs::hide("elig_year_comp")
        shinyjs::hide("elig_year")
        shinyjs::hide("elig_year_comp_forecast")
      } else if (input$elig_version == "comp") {
        shinyjs::show("elig_year_comp_forecast")
        shinyjs::hide("elig_year_comp")
        shinyjs::hide("elig_year")
        shinyjs::hide("elig_forecast_year")
      }
    }
  })
  
  # Eligibility Map ---------------------------------------------------------
  
  # Create Leaflet eligibility Map
  output$map_elig <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-1.703929, 52.447027, zoom = 10)
  })
  
  # Create the back-end for the zoom function
  observe({
    if (input$elig_zoom_to == 0) {
      leafletProxy("map_elig") %>% flyTo(lng = -1.891587, lat = 52.484689, zoom = 10)
    } else if (input$elig_zoom_to == 1) {
      leafletProxy("map_elig") %>% flyTo(lng = -1.890401, lat = 52.486243, zoom = 13L)
    } else if (input$elig_zoom_to == 2) {
      leafletProxy("map_elig") %>% flyTo(lng = -2.081112, lat = 52.512255, zoom = 13L)
    } else if (input$elig_zoom_to == 3) {
      leafletProxy("map_elig") %>% flyTo(lng = -1.519693, lat = 52.406822, zoom = 13L)
    } else if (input$elig_zoom_to == 4) {
      leafletProxy("map_elig") %>% flyTo(lng = -2.010793, lat = 52.536167, zoom = 13L)
    } else if (input$elig_zoom_to == 5) {
      leafletProxy("map_elig") %>% flyTo(lng = -1.777610, lat = 52.411811, zoom = 13L)
    } else if (input$elig_zoom_to == 6) {
      leafletProxy("map_elig") %>% flyTo(lng = -1.982919, lat = 52.586214, zoom = 13L)
    } else if (input$elig_zoom_to == 7) {
      leafletProxy("map_elig") %>% flyTo(lng = -2.128820, lat = 52.586973, zoom = 13L)
    }
  })
  
  # Reactive data for Static eligibility Map.
  lsoa_data <- reactive({
    elig_data <- setDT(dbGetQuery(con, glue("SELECT * FROM {schema_name}.el_pop_est ",
                                            "WHERE year={input$elig_year[1]};")))
  })
  
  # Reactive data for Static Forecast eligibility map.
  lsoa_data_forecast <- reactive({
    elig_data <- setDT(dbGetQuery(con, glue("SELECT * FROM {schema_name}.el_pop_for ",
                                            "WHERE year={input$elig_year[1]};")))
  })
  
  
  # Reactive data for Comparative eligibility Map.
  lsoa_data_comp <- reactive({
    elig_data <- setDT(dbGetQuery(con, glue("SELECT j1.area_code_x AS area_code, j1.count_x AS count_x, j1.count_y AS count_y, ROUND(CAST((100*(j1.count_y-j1.count_x)/j1.count_x) AS numeric), 2) AS count ",
                                            "FROM ((SELECT area_code AS area_code_x, count AS count_x FROM {schema_name}.el_pop_est ",
                                            "WHERE year={input$elig_year_comp[1]}) y1 INNER JOIN (SELECT area_code AS area_code_y, count AS count_y ",
                                            "FROM {schema_name}.el_pop_est WHERE year={input$elig_year_comp[2]}) y2 ON (y1.area_code_x=y2.area_code_y)) j1;")))
    
  })
  
  # Reactive data for Comparative eligibility Map.
  lsoa_data_forecast_comp <- reactive({
    elig_forecast <- setDT(dbGetQuery(con, glue("SELECT j1.area_code_x AS area_code, j1.count_x AS count_x, j1.count_y AS count_y, ROUND(CAST((100*(j1.count_y-j1.count_x)/j1.count_x) AS numeric), 2) AS count ",
                                                "FROM ((SELECT area_code AS area_code_x, count AS count_x FROM {schema_name}.el_pop_for ",
                                                "WHERE year={input$elig_year_comp_forecast[1]}) y1 INNER JOIN (SELECT area_code AS area_code_y, count AS count_y ",
                                                "FROM {schema_name}.el_pop_for WHERE year={input$elig_year_comp_forecast[2]}) y2 ON (y1.area_code_x=y2.area_code_y)) j1;")))
    
  })
  
  
  output$elig_s_tbl <- renderDT(
    
    if (input$elig_source == "estimate") {
      if (input$elig_version == "comp") {
        
        output_dt <- lsoa_data_comp()
        
        output_dt <- output_dt[,.(area_code, 
                                  t1_year = input$elig_year_comp[[1]], 
                                  t2_year = input$elig_year_comp[[2]],
                                  t1_pop = count_x, 
                                  t2_pop = count_y, 
                                  perc_change = count)]
        
        output_dt
        
        } else if (input$elig_version == "static") {
          
          lsoa_data()[,.(area_code, 
                         t1_year = input$elig_year_comp[[1]], 
                         t1_pop = count)]
          
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == "static") {
        
        la_data_forecast()[,.(area_code, 
                              t1_year = input$elig_year_comp_forecast[[1]], 
                              t1_pop = count)]
        
      } else if (input$elig_version == "comp") {
        
        la_data_forecast_comp()[,.(area_code,
                                   t1_year = input$elig_year_comp_forecast[[1]],
                                   t2_year = input$elig_year_comp_forecast[[2]], 
                                   t1_pop = count_x, 
                                   t2_pop = count_y, 
                                   perc_change = count)]
        
      }
    },
    options = list(pageLength = 10, scrollX = TRUE)
  )
  
  
  # Comparative Map, data download handler
  output$elig_downloader <- downloadHandler(
    filename = function() {
      paste0(input$elig_source, "_", input$elig_version, "_eligibility.csv")
    },
    content = function(file) {
      fwrite(
        if (input$elig_source == "estimate") {
          if (input$elig_version == "comp") {
            
            lsoa_data_comp()[,.(area_code,
                                t1_year = input$elig_year_comp[[1]], 
                                t2_year = input$elig_year_comp[[2]],
                                t1_pop = count_x, 
                                t2_pop = count_y, 
                                perc_change = count)]
            
          } else if (input$elig_version == "static") {
            
            lsoa_data()[,.(area_code, 
                           t1_year = input$elig_year_comp[[1]], 
                           t1_pop = count)]
            
          }
        } else if (input$elig_source == "forecast") {
          if (input$elig_version == "static") {
            
            la_data_forecast()[,.(area_code, 
                                  t1_year = input$elig_year_comp_forecast[[1]], 
                                  t1_pop = count)]
            
          } else if (input$elig_version == "comp") {
            
            la_data_forecast_comp()[,.(area_code,
                                       t1_year = input$elig_year_comp_forecast[[1]],
                                       t2_year = input$elig_year_comp_forecast[[2]], 
                                       t1_pop = count_x, 
                                       t2_pop = count_y, 
                                       perc_change = count)]
            
          }
        }
        , file
      )
    }
  )
  
  # Map update
  observeEvent(input$elig_update, {
    if (input$elig_source == "estimate") {
      
      temp_shp <- st_read(dsn=con, layer=c(schema_name, "lsoa_boundaries"))
      
      if (input$elig_version == "static") {
        palette <- "Blues"
        temp_shp <-  temp_shp %>% 
          inner_join(lsoa_data(), by=c("area_code"))
        domain <- c(1, max(abs(temp_shp$count)))
        suffix <- ""
      } else if (input$elig_version == "comp"){
        
        temp_shp <- temp_shp %>%
          inner_join(lsoa_data_comp(), by = c("area_code"))
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(temp_shp$count)), 0, max(abs(temp_shp$count)))
        suffix <- " %"
      }
    } else if (input$elig_source == "forecast") {
      
      temp_shp <- st_read(dsn=con, layer=c(schema_name, "la_boundaries"))
      
      if (input$elig_version == "static") {
        palette <- "Blues"
        
        temp_shp <- temp_shp %>% 
          left_join(la_data_forecast(), by = c("area_code"))
        domain <- c(1, max(abs(temp_shp$count)))
        suffix <- ""
      } else if (input$elig_version == "comp") {
        
        temp_shp <- temp_shp %>% 
          left_join(la_data_forecast_comp(), by = c("area_code"))
        
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(temp_shp$count)), 0, max(abs(temp_shp$count)))
        suffix <- " %"
      }
    }
    
    leafletProxy("map_elig") %>% 
      clearShapes() %>%
      addPolygons(
        data = temp_shp[1:floor(nrow(temp_shp)/ 2), ], 
        smoothFactor = 0,
        fillOpacity = 0.7,
        label = ~htmlEscape(paste0(area_code, " : ", count, suffix)),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = T),
        layerId = ~area_code,
        color = ~colorNumeric(palette, domain = domain)(count), weight = .5
      ) %>%
      addPolygons(
        data = temp_shp[(floor(nrow(temp_shp) / 2) + 1):nrow(temp_shp), ],
        smoothFactor = 0,
        fillOpacity = 0.7,
        layerId = ~area_code,
        label = ~htmlEscape(paste0(area_code, " : ", count, suffix)),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = T), 
        color = ~colorNumeric(palette, domain = domain)(count), weight = .5
      )
  })
  
  # Update Eligibility Legend.
  observeEvent(input$elig_update, {
    
    proxy <- leafletProxy("map_elig")
    
    if (input$elig_source == "estimate") {
      if (input$elig_version == "static") {
        title <- "People"
        palette <- "Blues"
        data <- lsoa_data()
        domain <- c(1, max(abs(data$count)))
      } else if (input$elig_version == "comp") {
        title <- "% Change"
        data <- lsoa_data_comp()
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(data$count)), 0, max(abs(data$count)))
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == "static") {
        title <- "People"
        palette <- "Blues"
        data <- la_data_forecast()
        domain <- c(1, max(abs(data$count)))
      } else if (input$elig_version == "comp") {
        title <- "Change"
        data <- la_data_forecast_comp() %>% rename(count = perc_change)
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(data$count)), 0, max(abs(data$count)))
      }
    }
    
    pal <- colorNumeric(palette, domain)
    proxy %>% clearControls() %>% 
      leaflet::addLegend(
        position = "bottomright",
        title = title,
        pal = pal,
        values = domain
      )
  })
  
  observeEvent(input$map_elig_shape_click[[1]], {
    shinyjs::show("map_elig_toggle", anim = T, animType = "slide")
  })
  
  
  
  
  # eligibility Plot change graph
  plot_data2 <- reactive({
    if (input$elig_source == "estimate") {
      
      elig_data <- dbGetQuery(con, glue("SELECT * FROM {schema_name}.d4_el0 
                                      WHERE area_code='{input$map_elig_shape_click[[1]]}';"))
      
    } else if (input$elig_source == "forecast") {
      
      elig_forecast <- dbGetQuery(con, glue("SELECT * FROM {schema_name}.d4_el1 
                                      WHERE area_code='{input$map_elig_shape_click[[1]]}';"))
      
    }
  })
  
  
  output$c_bokeh_plot <- renderRbokeh({
    figure(
      data = plot_data2(),
      legend_location = "right",
      xlab = "Year",
      ylab = "Count"
    ) %>%
      ly_lines(
        x = year,
        y = count,
        width = 2
      )
  })
  
  ### Help tour for Eligibility
  eligibility_steps <- reactive(data.frame(
    element = c("imaginary", "#elig_box_0", "#elig_box_1", "#elig_box_2", "#elig_box_3", "#elig_map_id"),
    intro = c(
      "Measure the number of residents who are eligible to the English National Concessionary Travel Scheme (ENCTS). You can view either historic figures or forecast figures of eligible residents.",
      "Do you want to see historic figures of eligible residents or forecast figures? Note: forecast figures are only available for local authorities.",
      "Do you want to measure changes in local eligibility between two time points?",
      "What date(s) are you interested in?",
      "Update the Map.",
      "Once you have updated your map, click on an area to view details."
    )
  ))
  
  observeEvent(input$elig_help, {
    introjs(session,
            options =
              list(
                steps = eligibility_steps(),
                "showBullets" = "false",
                "showProgress" = "true",
                "showStepNumbers" = "false",
                "nextLabel" = "Next",
                "prevLabel" = "Prev",
                "skipLabel" = "Skip"
              )
    )
  })
  
  # Force Shiny to Render the leaflet maps strait away.
  outputOptions(output, "dygraph_gender", suspendWhenHidden = F)
  outputOptions(output, "dygraph_age", suspendWhenHidden = F)
  outputOptions(output, "map_elig", suspendWhenHidden = F)
  outputOptions(output, "ac_map", suspendWhenHidden = F)
  outputOptions(output, "map_com_real", suspendWhenHidden = F)
  

}

onStop(function() {
  dbDisconnect(con)
})

shinyApp(ui, server, enableBookmarking = "url")


################################################################################
# Useful Information/Websites
#
# Intro.JS # Used to create the guided tours
# https://github.com/carlganz/rintrojs
#
# Shiny Dashboard # Framework for creating R Shiny Dashboard
# https://rstudio.github.io/shinydashboard/
#
# Shiny Widgets # Better input controls than standard shiny
# https://github.com/dreamRs/shinyWidgets
#
# rleaflet # Used for all mapping
# https://rstudio.github.io/leaflet/
#
# rbokeh # used to create interactive plots
# https://hafen.github.io/rbokeh/
#
# rdygraphs # used to create dygraphs on app landing page.
# https://rstudio.github.io/dygraphs/
