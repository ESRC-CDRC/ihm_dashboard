# Dashboard app based on R Shiny to visualise various transport data generated
# as part of the Inclusive and Healthy Mobility project at UCL

# Developed by Alistair Leak https://www.linkedin.com/in/alistairleak/ as part
# of the Inclusive and Healthy Mobility project at UCL.

## app.R ##
library(shinydashboard) # Dashboard.
library(data.table) # Use of data.tables - high performance data in R
library(leaflet) # Interface to leaflet maps
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

# options(shiny.trace = T)

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
                    choices = list("Passengers" = "pp", "Journeys" = "n", "Journeys per person" = "npp")
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
                    choices = list("1 Month" = "month", "Quarter" = "quarter")
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
                pickerInput(
                  inputId = "od_c_real_period",
                  label = "Period",
                  choices = list("1 Month" = 1, "2 Months" = 2, "3 months" = 3),
                  selected = 6,
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = F
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
                  selected = "msoa",
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = F
                ))
              ),
              column(
                12,
                pickerInput(
                  inputId = "od_c_real_nrows",
                  label = "Maximum number of flows",
                  width = "100%",
                  choices = list(
                    "100" = 100, "1,000" = 1000, "5,000" = 5000,
                    "10,000" = 10000, "All" = Inf
                  ),
                  selected = 1000,
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = F
                )
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
  
  
  ####### Start of Real Data Import #############################################
  # This is real data but at a much lower resolution thant he synthetic data. Further,
  # the data lacks much of the important demographic and time attribution.
  
  DT2 <-
    fread("data/od_data_real/mp_lsoa_avl_10_or_more_81_percent.csv", data.table = F) %>%
    transmute(
      date = as.Date(paste0(year, "-", month, "-1")),
      type = NA,
      age = NA,
      gender = NA,
      n = count,
      hour = NA,
      origin = orig,
      destination = dest,
      scale = "lsoa"
    ) %>%
    data.table()
  
  setkey(DT2, scale, date, gender, age, hour, type)
  
  ####### End of Real Data Import #############################################
  
  
  # Function to add origin and destination latitude and longitude to data.
  add_lat_lon <- . %>%
    left_join(area_lookup %>% select(area_code, longitude, latitude), by = c("origin" = "area_code")) %>%
    rename(x_lon = longitude, x_lat = latitude) %>%
    left_join(area_lookup %>% select(area_code, longitude, latitude), by = c("destination" = "area_code")) %>%
    rename(y_lon = longitude, y_lat = latitude) %>%
    rename(n = N)
  
  # Function to range standardise frequencies for plotting.
  range01 <- function(x) {
    (x - min(x)) / (((max(x) - min(x))))
  }
  
  
  ###########################################################################
  # Who Travels -------------------------------------------------------------
  ###########################################################################
  
  
  # These will be used once we have day resolution data.
  dygraph_data_age <- fread("data/trends/trends_age.csv", data.table = F) %>%
    filter(age5 >= 61) %>%
    transmute(date = as.Date(month), age = age5, type, n, npp, pp) %>%
    tidyr::gather(measure, value, -date, -age, -type)
  
  dygraph_data_gender <- fread("data/trends/trends_gender.csv", data.table = F) %>%
    transmute(date = as.Date(month), gender, n, npp, pp, type) %>%
    tidyr::gather(measure, value, -date, -gender, -type)
  
  
  dygraph_data_1 <- reactive({
    data <- dygraph_data_gender
    if (input$dygraph_time_resolution == "month") {
      data <- data %>%
        filter(measure == input$dygraph_statistic, type == input$dygraph_type) %>%
        transmute(date = round_date(date, "month"), gender, value) %>%
        tidyr::spread(gender, value) %>%
        group_by(date) %>%
        summarise_all(.funs = list("sum")) %>%
        data.table()
    } else if (input$dygraph_time_resolution == "quarter") {
      data <- data %>%
        filter(measure == input$dygraph_statistic, type == input$dygraph_type) %>%
        transmute(date = round_date(date, "3 months"), gender, value) %>%
        group_by(date, gender) %>%
        summarise_all(.funs = list("sum")) %>%
        tidyr::spread(gender, value) %>%
        data.table()
    } else {
      data
    }
  })
  
  
  dygraph_data_2 <- reactive({
    data <- dygraph_data_age
    if (input$dygraph_time_resolution == "month") {
      data <- data %>%
        filter(measure == input$dygraph_statistic, type == input$dygraph_type) %>%
        transmute(date = round_date(date, "month"), age, value) %>%
        tidyr::spread(age, value) %>%
        group_by(date) %>%
        summarise_all(.funs = list("sum")) %>%
        data.table()
    } else if (input$dygraph_time_resolution == "quarter") {
      data <- data %>%
        filter(measure == input$dygraph_statistic, type == input$dygraph_type) %>%
        transmute(date = round_date(date, "3 months"), age, value) %>%
        group_by(date, age) %>%
        summarise_all(.funs = list("sum")) %>%
        tidyr::spread(age, value) %>%
        data.table()
    } else {
      data
    }
  })
  
  
  # First interactive graph based on gender
  output$dygraph_gender <- renderDygraph({
    
    # Subset DT for msoas and aggregate by date and gender.
    data <- dygraph_data_1()
    
    if (input$dygraph_statistic == "npp") {
      stack_case <- FALSE
      dygraph_statistic <- "Journeys per Person"
    } else if (input$dygraph_statistic == "pp") {
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
    
    if (input$dygraph_statistic == "npp") {
      stack_case <- FALSE
      dygraph_statistic <- "Journeys per Person"
    } else if (input$dygraph_statistic == "pp") {
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
  
  area_lookup <- read.csv("data/spatial_data/area_lookup.csv")
  area_lookup <- data.table(area_lookup, key = "scale")
  
  msoa <- area_lookup["msoa", .(scale, area_code, longitude, latitude)]
  
 
  
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
    od_c_real_date_start <- as.Date(input$od_c_real_date_start_1[[1]])
    
    # Aggregate DT2. Note the use of the Key to optimise query speed.
    DT2[CJ(
      c("lsoa"),
      seq(as.Date(od_c_real_date_start), by = "month", length.out = as.numeric(input$od_c_real_period)),
      c(NA),
      c(NA),
      c(NA),
      c(NA)
    ),
    .("N" = sum(n)),
    by = c("origin", "destination")
    ][order(N)] %>% add_lat_lon()
  })
  
  
  data_od_c_real <- reactive({
    
    # Create progress bar.
    withProgress(message = "Processing Request", value = 0, {
      
      # Update progress bar.
      incProgress(1 / 3, message = "Import Time 1 Data")
      
      od_c_date_start_1 <- as.Date(input$od_c_real_date_start_1[[1]])
      od_c_date_start_2 <- as.Date(input$od_c_real_date_start_2[[1]])
      
      # Query the data for the first data point parameters.
      df1 <- DT2[CJ(
        c("lsoa"),
        seq(as.Date(od_c_real_date_start_1), by = "month", length.out = as.numeric(input$od_c_real_period)),
        c(NA),
        c(NA),
        c(NA),
        c(NA)
      ),
      .("N" = sum(n)),
      by = c("origin", "destination")
      ] %>% rename(n = N)
      
      # Update progress bar.
      incProgress(1 / 3, message = "Import Time 2 Data")
      
      # Query the data for the second data point parameters.
      df2 <- DT2[CJ(
        c("lsoa"),
        seq(as.Date(od_c_real_date_start_2), by = "month", length.out = as.numeric(input$od_c_real_period)),
        c(NA),
        c(NA),
        c(NA),
        c(NA)
      ),
      .("N" = sum(n)),
      by = c("origin", "destination")
      ] %>% rename(n = N)
      
      # Update progress bar.
      incProgress(1 / 3, message = "Merging Datasets")
      
      # Merge data for time points 1 and 2
      df1 %>%
        full_join(df2, by = c("origin", "destination")) %>%
        mutate(perc_change = as.numeric((n.y - n.x) / n.x) * 100) %>%
        left_join(area_lookup, by = c("origin" = "area_code")) %>%
        rename(x_lon = longitude, x_lat = latitude) %>%
        left_join(area_lookup, by = c("destination" = "area_code")) %>%
        rename(y_lon = longitude, y_lat = latitude) %>%
        arrange(desc(abs(perc_change)))
    })
  })
  
  
  # Update map to reflect new criteria. (Static)
  
  observeEvent(input$od_c_real_s_update, {
    shinyjs::hide("od_map_real_toggle", anim = T, animType = "slide")
    
    
    if (input$od_c_real_version == "static") {
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {
        
        # Update progress bar.
        incProgress(1 / 4, message = "Importing data.")
        
        # request static data based on specified parameters
        data <- data_od_s_real() %>%
          filter(complete.cases(.)) %>%
          arrange(desc(n)) %>%
          head(min(input$od_c_real_nrows, nrow(.)))
        
        domain <- c(0, max(data$n))
        # rescale n value to make the scale for mapping consistent.
        data$n <- (range01(data$n) + 0.01)
        
        # Update progress bar.
        incProgress(1 / 4, message = "Interpolating lines.")
        
        # Interpolate lines based on origin and destination lat lons.
        lines <- gcIntermediate(
          as.matrix(data[, c("x_lon", "x_lat")]),
          as.matrix(data[, c("y_lon", "y_lat")]),
          n = 0,
          addStartEnd = T,
          sp = T
        )
        
        # subset markers based on scale.
        mrks <- area_lookup[.("lsoa")]
        
        # Update progress bar.
        incProgress(1 / 4, message = "Building Map.")
        
        # Update static map with new lines data.
        map <- leafletProxy("map_com_real", data = lines) %>%
          clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          addPolylines(
            color = colorNumeric("Spectral", data$n)(data$n),
            weight = (data$n * 25), opacity = 0.4
          ) %>%
          addCircleMarkers(
            lng = mrks$longitude,
            lat = mrks$latitude,
            layerId = mrks$area_code,
            label = mrks$area_code,
            radius = 1, weight = 1
          ) %>%
          leaflet::addLegend("bottomright",
                             layerId = "od_map_real_legend",
                             pal = colorNumeric("Spectral", domain),
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
    if (input$od_c_real_version == "comp") {
      shinyjs::hide("od_map_real_toggle", anim = T, animType = "slide")
      
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {
        
        # Update progress bar.
        incProgress(1 / 4, message = "Importing Map Data")
        
        data <- data_od_c_real() %>%
          filter(complete.cases(.)) %>%
          head(min(nrow(.), input$od_c_real_nrows))
        
        # Create the colour breaks
        binpal <- colorBin("RdYlBu", bins = c(-100, -80, -60, -40, -20, 0, 20, 45, 60, 80, Inf))
        
        # Update progress bar.
        incProgress(1 / 4, message = "Interpolating Lines")
        
        # Interpolate lines based on origin and destination lat lons.
        lines2 <- gcIntermediate(
          as.matrix(data[, c("x_lon", "x_lat")]),
          as.matrix(data[, c("y_lon", "y_lat")]),
          n = 0,
          addStartEnd = T,
          sp = T
        )
        
        # subset markers based on scale.
        mrks <- area_lookup[.("lsoa")]
        
        incProgress(1 / 4, message = "Building Map.")
        
        # Update comparative map with new lines data.
        map_com <- leafletProxy("map_com_real", data = lines2) %>%
          clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          addPolylines(color = binpal(data$perc_change), weight = 1, opacity = 0.5) %>%
          addCircleMarkers(
            lng = mrks$longitude,
            lat = mrks$latitude,
            layerId = mrks$area_code,
            label = mrks$area_code,
            radius = 1, weight = 1
          ) %>%
          leaflet::addLegend("bottomright",
                             layerId = "od_map_legend",
                             pal = colorBin("RdYlBu", bins = c(-100, -80, -60, -40, -20, 0, 20, 45, 60, 80, Inf)),
                             values = c(-100, -80, -60, -40, -20, 0, 20, 45, 60, 80, 120),
                             title = "% Change",
                             labFormat = labelFormat(suffix = "%"),
                             opacity = 0.8
          )
      })
    }
  })
  
  
  # Add lines to the destination map. Where do people go to?
  observeEvent(input$map_com_real_marker_click, {
    shinyjs::show("od_map_real_toggle", anim = T, animType = "slide")
    
    data2a <- data_od_s_real() %>%
      filter(origin == input$map_com_real_marker_click[[1]]) %>%
      mutate(dir = "outbound", colour = "#ff0000")
    
    data2b <- data_od_s_real() %>%
      filter(destination == input$map_com_real_marker_click[[1]]) %>%
      mutate(dir = "inbound", colour = "#00ff00")
    
    data2 <- bind_rows(data2a, data2b)
    
    if (nrow(data2) > 0) {
      data2$n <- (range01(data2$n) + 0.1) * 5
      
      # Interpolate lines based on origin and destination lat lons.
      lines <- gcIntermediate(
        as.matrix(data2[, c("x_lon", "x_lat")]),
        as.matrix(data2[, c("y_lon", "y_lat")]),
        n = 0,
        addStartEnd = T,
        sp = T
      )
      
      # subset markers based on scale.
      mrks <- area_lookup["lsoa"]
      
      # Update od outbound map with new lines data.
      map <- leafletProxy("map_com_real", data = lines) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolylines(
          group = "od_line_layer", color = data2$colour,
          weight = 1 + (data2$n * 2), opacity = 0.3
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
  
  
  
  output$od_c_real_tbl <- renderDataTable(
    
    if (input$od_c_real_version == "comp") {
      data_od_c_real() %>%
        transmute(origin, destination,
                  T1_period = paste(input$od_c_real_date_start_1, "-", as.character(as.Date(input$od_c_real_date_start_1) %m+% months(as.numeric(input$od_c_real_period)) - 1)),
                  T2_period = paste(as.Date(input$od_c_real_date_start_2), "-", as.character(as.Date(input$od_c_real_date_start_2) %m+% months(as.numeric(input$od_c_real_period)) - 1)),
                  T1 = n.x, T2 = n.y, perc_change
        ) %>%
        left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("origin" = "area_code")) %>%
        left_join(area_lookup %>% select(area_code, "Dest Name" = msoa11nm), by = c("destination" = "area_code")) %>%
        select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", T1_period, T2_period, T1, T2, perc_change)
    } else if (input$od_c_real_version == "static") {
      data_od_s_real() %>%
        transmute(origin, destination,
                  T1 = n,
                  period = paste(input$od_c_real_date_start_1, "-", as.character(as.Date(input$od_c_real_date_start_1) %m+% months(as.numeric(input$od_c_real_period)) - 1))
        ) %>%
        left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("origin" = "area_code")) %>%
        left_join(area_lookup %>% select(area_code, "Dest Name" = msoa11nm), by = c("destination" = "area_code")) %>%
        select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", period, T1)
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
          data_od_c_real() %>%
            transmute(origin, destination,
                      T1_period = paste(input$od_c_real_date_start_1, "-", as.character(as.Date(input$od_c_real_date_start_1) %m+% months(as.numeric(input$od_c_real_period)) - 1)),
                      T2_period = paste(as.Date(input$od_c_real_date_start_2), "-", as.character(as.Date(input$od_c_real_date_start_2) %m+% months(as.numeric(input$od_c_real_period)) - 1)),
                      T1 = n.x, T2 = n.y, perc_change
            ) %>%
            left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("origin" = "area_code")) %>%
            left_join(area_lookup %>% select(area_code, "Dest Name" = msoa11nm), by = c("destination" = "area_code")) %>%
            select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", T1_period, T2_period, T1, T2, perc_change)
        } else if (input$od_c_real_version == "static") {
          data_od_s_real() %>%
            transmute(origin, destination,
                      T1 = n,
                      period = paste(input$od_c_real_date_start_1, "-", as.character(as.Date(input$od_c_real_date_start_1) %m+% months(as.numeric(input$od_c_real_period)) - 1))
            ) %>%
            left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("origin" = "area_code")) %>%
            left_join(area_lookup %>% select(area_code, "Dest Name" = msoa11nm), by = c("destination" = "area_code")) %>%
            select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", period, T1)
        }, file
      )
    }
  )
  
  
  
  observeEvent(input$map_com_real_marker_click, {
    leafletProxy("map_com_real") %>% clearControls()
  })
  
  # eligibility Plot change graph
  plot_data3_real <- reactive({
    data2a <- data_od_s_real() %>%
      filter(origin == input$map_com_real_marker_click[[1]]) %>%
      mutate(dir = "outbound", colour = "#ff0000") %>%
      arrange(desc(n)) %>%
      head(10)
  })
  
  # eligibility Plot change graph
  plot_data4_real <- reactive({
    data2b <- data_od_s_real() %>%
      filter(destination == input$map_com_real_marker_click[[1]]) %>%
      mutate(dir = "inbound", colour = "#00ff00") %>%
      arrange(desc(n)) %>%
      head(10)
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
        y = n,
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
        y = n,
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
  
  # Import the accessibility data
  ac_dt <- fread("data/accessibility/access_data_tues.csv", data.table = F) %>%
    filter(complete.cases(.)) %>%
    mutate(mean_min_time = round(ceiling(mean_min_time / 60)))
  
  # Import the output areas shape file.
  oa <- readShapePoly("data/spatial_data/oa_4326.shp", delete_null_obj = T)
  oa@data <- oa@data[, c("objectid", "oa11cd")]
  oa@data$oa11cd <- as.character(oa@data$oa11cd)
  
  # Import Accessibility POI Shape files
  
  clinics <- readShapePoints("data/POIs/Clinics/Clinics.shp")
  gps <- readShapePoints("data/POIs/GPs/GPs.shp")
  hsp <- readShapePoints("data/POIs/Hospitals/Hospitals.shp")
  ret <- readShapePoints("data/POIs/RetailCentres/retail_centres_2015_4326_x.shp")
  sm <- readShapePoints("data/POIs/Supermarkets/2016/Supermarkets_2016_4326.shp")
  sm_s <- sm[sm@data$size_band == "< 3,013 ft2 (280m2)", ]
  sm_m <- sm[sm@data$size_band == "3,013 < 15,069 ft2 (280 < 1,400 m2)", ]
  sm_l <- sm[sm@data$size_band == "15,069 < 30,138 ft2 (1,400 < 2,800 m2)", ]
  sm_v <- sm[sm@data$size_band == "30,138 ft2 > (2,800 m2)", ]
  ts <- readShapePoints("data/POIs/Railway Stations/2016/Railway_Stations_2016.shp")
  rm(sm)
  
  output$ac_map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -1.891587, lat = 52.484689, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # Example area_code to add datashine layers into the App.
      
      # addTiles(group = "Vulnerability", urlTemplate = "https://maps.cdrc.ac.uk/tiles/shine_urbanmask_dark2/{z}/{x}/{y}.png",
      #          option=tileOptions(tms = F, continuousWorld=T, opacity = 1)) %>%
      
      
      # Accessibility Amenity Circle Markers
      addCircleMarkers(data = clinics, group = "Clinic", stroke = T, color = "black", weight = 1, fillColor = "#e41a1c", radius = 7, fillOpacity = .8) %>%
      addCircleMarkers(data = gps, group = "GP", stroke = T, color = "black", weight = 1, fillColor = "#377eb8", radius = 7, fillOpacity = .8) %>%
      addCircleMarkers(data = hsp, group = "Hospital", stroke = T, color = "black", weight = 1, fillColor = "#4daf4a", radius = 7, fillOpacity = .8) %>%
      addCircleMarkers(data = sm_s, group = "Supermarket (Small)", stroke = T, color = "black", weight = 1, fillColor = "#984ea3", radius = 7, fillOpacity = .8) %>%
      addCircleMarkers(data = sm_m, group = "Supermarket (Medium)", stroke = T, color = "black", weight = 1, fillColor = "#ff7f00", radius = 7, fillOpacity = .8) %>%
      addCircleMarkers(data = sm_l, group = "Supermarket (Large)", stroke = T, color = "black", weight = 1, fillColor = "#ffff33", radius = 7, fillOpacity = .8) %>%
      addCircleMarkers(data = ret, group = "Retail Centre", stroke = T, color = "black", weight = 1, fillColor = "#ffff33", radius = 7, fillOpacity = .8) %>%
      addCircleMarkers(data = sm_v, group = "Supermarket (Very Large)", stroke = T, color = "black", weight = 1, fillColor = "#a65628", radius = 7, fillOpacity = .8) %>%
      addCircleMarkers(data = ts, group = "Train Station", stroke = T, color = "black", weight = 1, fillColor = "#f781bf", radius = 7, fillOpacity = .8) %>%
      hideGroup(c("Clinic", "GP", "Supermarket (Small)", "Supermarket (Medium)", "Supermarket (Large)", "Supermarket (Very Large)", "Train Station", "Retail Centre"))
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
    data <- oa@data %>%
      left_join(ac_dt %>%
                  filter(date == input$ac_date, type == input$ac_type), by = c("oa11cd" = "origin"))
  })
  
  # Create Reactive table for comparative accessibility
  s_data_comp <- reactive({
    data <- oa@data %>%
      left_join(
        ac_dt %>%
          filter(date == input$ac_date_comp[[1]], type == input$ac_type) %>%
          select(origin, type, mean_min_time_start = mean_min_time) %>%
          inner_join(
            ac_dt %>% filter(date == input$ac_date_comp[[2]], type == input$ac_type) %>% select(origin, type, mean_min_time_end = mean_min_time),
            by = c("origin", "type")
          ) %>%
          mutate(mean_min_time = mean_min_time_end - mean_min_time_start),
        by = c("oa11cd" = "origin")
      )
  })
  
  output$ac_tbl <- renderDataTable(
    if (input$ac_c_version == "comp") {
      s_data_comp() %>%
        transmute(oa11cd, type, T1_date = input$ac_date_comp[[1]], T2_date = input$ac_date_comp[[2]], T1_time = mean_min_time_start, T2_time = mean_min_time_end, Change = mean_min_time)
    } else if (input$ac_c_version == "static") {
      s_data() %>%
        select(oa11cd, type, T1_date = date, T1_time = mean_min_time)
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
          s_data_comp() %>%
            transmute(oa11cd, type, T1_date = input$ac_date_comp[[1]], T2_date = input$ac_date_comp[[2]], T1_time = mean_min_time_start, T2_time = mean_min_time_end, Change = mean_min_time)
        } else if (input$ac_c_version == "static") {
          s_data() %>%
            select(oa11cd, type, T1_date = date, T1_time = mean_min_time)
        }, file
      )
    }
  )
  
  # Accessibility Map update
  observeEvent(input$ac_c_s_update, {
    oa_cp <- oa
    
    if (input$ac_c_version == "static") {
      palette <- "YlOrRd"
      reverse <- F
      oa_cp@data <- oa_cp@data %>% left_join(s_data())
      domain <- c(1, max(abs(oa_cp@data$mean_min_time)))
    } else if (input$ac_c_version == "comp") {
      palette <- "RdYlGn"
      reverse <- T
      oa_cp@data <- oa_cp@data %>% left_join(s_data_comp())
      domain <- c((-1 * max(abs(oa_cp@data$mean_min_time))), max(abs(oa_cp@data$mean_min_time)))
    }
    
    leafletProxy("ac_map", deferUntilFlush = F) %>%
      clearShapes() %>%
      # Update Accessibility
      hideGroup(c(
        "Clinic", "GP", "Hospital", "Supermarket (Small)",
        "Supermarket (Medium)", "Supermarket (Large)", "Supermarket (Very Large)", "Train Station"
      )) %>%
      addPolygons(
        data = oa_cp[1:2500, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa11cd, " : ", mean_min_time, " mins.")),
        layerId = ~oa11cd,
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F)
      ) %>%
      addPolygons(
        data = oa_cp[2501:5000, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa11cd, " : ", mean_min_time, " mins.")),
        layerId = ~oa11cd,
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F)
      ) %>%
      addPolygons(
        data = oa_cp[5001:7500, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa11cd, " : ", mean_min_time, " mins.")),
        layerId = ~oa11cd,
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F)
      ) %>%
      addPolygons(
        data = oa_cp[7501:nrow(oa_cp), ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa11cd, " : ", mean_min_time, " mins.")),
        layerId = ~oa11cd,
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F)
      ) %>%
      showGroup(input$ac_type)
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
    ac_dt %>%
      filter(origin == na.omit(c(input$ac_map_shape_click[[1]], "E00045442"))[1]) %>%
      .[order(.$date), ]
  })
  
  output$access_bokeh_plot <- renderRbokeh({
    figure(
      data = plot_data(),
      legend_location = "right",
      xlab = "Date",
      ylab = "Minimum Journey Time (minutes)",
      title = input$ac_map_shape_click[[1]]
    ) %>%
      ly_lines(
        x = as.Date(date),
        y = mean_min_time,
        color = type,
        width = 2
      ) %>%
      set_palette(discrete_color = pal_color(c(
        "#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
        "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999"
      ))) %>%
      ly_points(
        x = as.Date(date),
        y = mean_min_time,
        color = type,
        size = 4,
        hover = c(mean_min_time, type)
      )
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
  
  
  # Import data for eligibility ---------------------------------------------
  elig_data <- fread("data/eligibility/eligible_pop_estimates.csv", data.table = F) %>%
    rename(geo_code = lsoa11cd)
  
  elig_forecast <- fread("data/eligibility/eligible_pop_forecasts.csv", data.table = F) %>%
    rename(geo_code = area_code)
  
  lsoa <- readShapePoly("data/spatial_data/lsoa_4326.shp")
  lsoa@data <- lsoa@data %>% transmute(geo_code = lsoa11cd)
  
  la <- readShapePoly("data/spatial_data/local_authorities_4326.shp")
  la@data <- la@data["geo_code"]
  
  
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
    elig_data[elig_data$year == input$elig_year[[1]], ]
  })
  
  # Reactive data for Static Forecast eligibility map.
  lsoa_data_forecast <- reactive({
    elig_forecast[elig_forecast$year == input$elig_forecast_year[[1]], ]
  })
  
  
  # Reactive data for Comparative eligibility Map.
  lsoa_data_comp <- reactive({
    elig_data[elig_data$year == input$elig_year_comp[[1]], ] %>%
      inner_join(elig_data[elig_data$year == input$elig_year_comp[[2]], ], by = "geo_code") %>%
      transmute(geo_code, value.y, value.x, value = round(100 * (value.y - value.x) / value.x, 1))
  })
  
  # Reactive data for Comparative eligibility Map.
  lsoa_data_forecast_comp <- reactive({
    elig_forecast[elig_forecast$year == input$elig_year_comp_forecast[[1]], ] %>%
      inner_join(elig_forecast[elig_forecast$year == input$elig_year_comp_forecast[[2]], ], by = "geo_code") %>%
      transmute(geo_code, value.y, value.x, perc_change = round((100 * (value.y - value.x) / value.x), 2))
  })
  
  
  output$elig_s_tbl <- renderDataTable(
    
    if (input$elig_source == "estimate") {
      if (input$elig_version == "comp") {
        lsoa_data_comp() %>%
          transmute(geo_code,
                    t1_year = input$elig_year_comp[[1]], t2_year = input$elig_year_comp[[2]],
                    t1_pop = value.x, t2_pop = value.y, perc_change = value
          ) %>%
          left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("geo_code" = "area_code"))
      } else if (input$elig_version == "static") {
        lsoa_data() %>%
          transmute(geo_code, t1_year = input$elig_year_comp[[1]], t1_pop = value) %>%
          left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("geo_code" = "area_code"))
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == "static") {
        lsoa_data_forecast() %>%
          transmute(geo_code, t1_year = input$elig_year_comp_forecast[[1]], t1_pop = value) %>%
          left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("geo_code" = "area_code"))
      } else if (input$elig_version == "comp") {
        lsoa_data_forecast_comp() %>%
          transmute(geo_code,
                    t1_year = input$elig_year_comp_forecast[[1]],
                    t2_year = input$elig_year_comp_forecast[[2]], t1_pop = value.x, t2_pop = value.y, perc_change
          ) %>%
          left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("geo_code" = "area_code"))
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
            lsoa_data_comp() %>%
              transmute(geo_code,
                        t1_year = input$elig_year_comp[[1]], t2_year = input$elig_year_comp[[2]],
                        t1_pop = value.x, t2_pop = value.y, perc_change = value
              ) %>%
              left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("geo_code" = "area_code"))
          } else if (input$elig_version == "static") {
            lsoa_data() %>%
              transmute(geo_code, t1_year = input$elig_year_comp[[1]], t1_pop = value) %>%
              left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("geo_code" = "area_code"))
          }
        } else if (input$elig_source == "forecast") {
          if (input$elig_version == "static") {
            lsoa_data_forecast() %>%
              transmute(geo_code, t1_year = input$elig_year_comp_forecast[[1]], t1_pop = value) %>%
              left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("geo_code" = "area_code"))
          } else if (input$elig_version == "comp") {
            lsoa_data_forecast_comp() %>%
              transmute(geo_code,
                        t1_year = input$elig_year_comp_forecast[[1]],
                        t2_year = input$elig_year_comp_forecast[[2]], t1_pop = value.x, t2_pop = value.y, perc_change
              ) %>%
              left_join(area_lookup %>% select(area_code, "Origin Name" = msoa11nm), by = c("geo_code" = "area_code"))
          }
        }
        , file
      )
    }
  )
  
  # Map update
  observeEvent(input$elig_update, {
    if (input$elig_source == "estimate") {
      temp <- lsoa
      
      if (input$elig_version == "static") {
        palette <- "Blues"
        temp@data <- temp@data %>% left_join(lsoa_data(), by = "geo_code")
        domain <- c(1, max(abs(temp@data$count)))
        suffix <- ""
      } else if (input$elig_version == "comp") {
        temp@data <- temp@data %>% left_join(lsoa_data_comp(), by = "geo_code")
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(temp@data$count)), 0, max(abs(temp@data$count)))
        suffix <- " %"
      }
    } else if (input$elig_source == "forecast") {
      temp <- la
      if (input$elig_version == "static") {
        palette <- "Blues"
        temp@data <- temp@data %>% left_join(lsoa_data_forecast(), by = "geo_code")
        domain <- c(1, max(abs(temp@data$count)))
        suffix <- ""
      } else if (input$elig_version == "comp") {
        temp@data <- temp@data %>% left_join(lsoa_data_forecast_comp() %>%
                                               rename(count = perc_change), by = "geo_code")
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(temp@data$count)), 0, max(abs(temp@data$count)))
        suffix <- " %"
      }
    }
    
    
    leafletProxy("map_elig") %>%
      clearShapes() %>%
      addPolygons(
        data = temp[1:floor(nrow(temp@data) / 2), ],
        smoothFactor = 0,
        fillOpacity = 0.7,
        label = ~htmlEscape(paste0(geo_code, " : ", count, suffix)),
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F),
        layerId = ~geo_code,
        color = ~colorNumeric(palette, domain = domain)(count), weight = .5
      ) %>%
      addPolygons(
        data = temp[(floor(nrow(temp@data) / 2) + 1):nrow(temp@data), ],
        smoothFactor = 0,
        fillOpacity = 0.7,
        layerId = ~geo_code,
        label = ~htmlEscape(paste0(geo_code, " : ", count, suffix)),
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F),
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
        data <- lsoa_data_forecast()
        domain <- c(1, max(abs(data$count)))
      } else if (input$elig_version == "comp") {
        title <- "Change"
        data <- lsoa_data_forecast_comp() %>% rename(count = perc_change)
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(data$count)), 0, max(abs(data$count)))
      }
    }
    
    pal <- colorNumeric(palette, domain)
    proxy %>% clearControls()
    proxy %>% addLegend(
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
      elig_data %>%
        filter(geo_code == na.omit(c(input$map_elig_shape_click[[1]], "E01008882"))[1]) %>%
        .[order(.$year), ]
    } else if (input$elig_source == "forecast") {
      elig_forecast %>%
        filter(geo_code == na.omit(c(input$map_elig_shape_click[[1]], "E08000025"))[1]) %>%
        .[order(.$year), ]
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
