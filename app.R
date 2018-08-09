# Dashboard app based on R Shiny to visualise various transport data generated
# as part of the Inclusive and Healthy Mobility project at UCL


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
library(shinyjs) # Addittional java script functionality
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
        menuItem("Where and When", tabName = "tab_od_c", icon = icon("map")),
        menuItem("Access to Services", tabName = "tab_access", icon = icon("dashboard")),
        menuItem("Who's Eligible", tabName = "tab_elig", icon = icon("user")),
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
          fluidRow(
            infoBox("Total Journeys", textOutput("n_journeys", inline = TRUE), icon = icon("signal"))
          ),
          fluidRow(
            box(
              absolutePanel(
                top = 35, left = 20, style = "z-index: 1000;",
                dropdownButton(
                  circle = TRUE, status = "danger", icon = icon("gear"), width = "200px",

                  selectInput(
                    inputId = "dygraph_time_resolution", label = "Resolution",
                    choices = list("1 Day" = "day", "1 week" = "week", "1 Month" = "month", "Quarter" = "quarter")
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
        # First tab content
        tabItem(tabName = "dashboard"),
        # Second Comparative Map tab content
        tabItem(
          tabName = "tab_od_c",
          # Top menu bar has all control elements
          fluidRow(
            box(
              width = 3, title = "When?", id = "od_box_1", status = "primary",
              solidHeader = T, style = "min-height: 255px;",
              column(
                12,
                airDatepickerInput("od_c_date_start_1",
                  label = "Period 1: Start date and time",
                  value = "2015-10-01"
                ),
                shinyjs::hidden(airDatepickerInput("od_c_date_start_custom",
                  label = "Period 1: Custom Period",
                  value = "2015-10-01",
                  range = TRUE
                ))
              ),
              column(
                12,
                sliderInput("od_c_time_1",
                  label = NULL,
                  min = 0, max = 24, post = ":00", value = c(0, 24)
                )
              ),
              column(
                12,
                pickerInput(
                  inputId = "od_c_period",
                  label = "Period",
                  choices = list("1 Day" = 1, "1 Week" = 6, "4 weeks" = 27, "Custom" = "custom"),
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
              width = 3, title = "Who?", id = "od_box_2", status = "primary",
              solidHeader = T, style = "min-height: 255px;",
              column(
                12,
                pickerInput(
                  inputId = "od_c_demographics",
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
                )
              ),
              br(),
              column(
                12,
                pickerInput(
                  inputId = "od_c_type_1",
                  label = "Card Type",
                  choices = list(
                    "Concessionary" = "con",
                    "Non-Concessionary" = "non",
                    "Dissability" = "dis"
                  ),
                  selected = c("con", "non", "dis"),
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = T
                )
              )
            ),
            box(
              width = 3, title = "Comparison", id = "od_box_3", status = "primary",
              solidHeader = T, style = "min-height: 255px;",
              column(
                12,
                radioGroupButtons(
                  label = "Compare?",
                  inputId = "od_c_version",
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
                airDatepickerInput("od_c_date_start_2",
                  label = "Period 2: Date",
                  value = "2015-10-04"
                ),
                h4("Comparison period and time of day are equal to period 1.")
              )
            ),
            box(
              width = 3, title = "Plot", id = "od_box_4", status = "primary",
              solidHeader = T, style = "min-height: 255px;",
              column(
                12,
                pickerInput(
                  inputId = "od_c_scale",
                  label = "Scale ",
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
                )
              ),
              column(
                12,
                pickerInput(
                  inputId = "od_c_nrows",
                  label = "Top~n. ",
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
              ), br(),
              column(
                12,
                actionButton("od_help", "Help", width = "100%"),
                actionButton("od_c_s_update", "Update", width = "100%", status = "primary")
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
                    inputId = "od_c_zoom_to", label = NULL, width = "150px",
                    choices = list(
                      "West Midlands" = 0, "Birmingham" = 1, "Dudley" = 2,
                      "Coventry" = 3, "Sandwell" = 4, "Solihull" = 5,
                      "Walsall" = 6, "Wolverhampton" = 7
                    ),
                    selected = 1
                  )
                ),
                absolutePanel(
                  top = "65px", id = "od_map_id", width = 300, right = "40px", style = "z-index: 999; font-size = 5px",
                  box(
                    title = "Select a Marker to View Trends", id = "od_map_toggle",
                    width = 320, style = "background: transparent; font-size: 8px;", status = "primary", solidHeader = T,
                    addSpinner(rbokehOutput("inbound_bokeh_plot", height = 250, width = 290), spin = "circle", color = "#E41A1C"),
                    addSpinner(rbokehOutput("outbound_bokeh_plot", height = 250, width = 290), spin = "circle", color = "#E41A1C")
                    , collapsible = T, collapsed = T
                  )
                ),
                addSpinner(leafletOutput("map_com", height = 600), spin = "circle", color = "#E41A1C")
              ),
              tabPanel(
                "Data",
                dataTableOutput("od_c_tbl"),
                downloadButton("od_c_downloader", "Download Data")
              )
            )
          )
        ),
        tabItem(
          tabName = "tab_access",
          # h3("Accessibility Explorer"),
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
                  label = "Amenity Type ",
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
                label = "Compare?",
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
                label = "Date",
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
                label = "Date",
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
              width = 2, title = "Plotting", id = "ac_box_4", status = "primary", solidHeader = T, height = "140px",
              actionButton("access_help", "Help", align = "center", width = "100%"),
              actionButton("ac_c_s_update", "Update", align = "center", width = "100%")
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
                      "West Midlands" = 0, "Birmingham" = 1,
                      "Dudley" = 2, "Coventry" = 3, "Sandwell" = 4,
                      "Solihull" = 5, "Walsall" = 6, "Wolverhampton" = 7
                    ),
                    selected = 1
                  )
                ),
                addSpinner(leafletOutput("ac_map"), spin = "circle", color = "#E41A1C"),
                absolutePanel(
                  top = "65px", width = 300, right = "40px", style = "z-index: 999; font-size = 5px", id = "ac_map_id",
                  box(
                    title = "Select an Area to View Trends", id = "ac_map_toggle",
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
          fluidRow(
            box(
              width = 3, title = "Source", id = "elig_box_0", solidHeader = T, status = "primary", height = 140,
              radioGroupButtons(
                inputId = "elig_source",
                label = "Source?",
                choices = list("Estimates" = "estimate", "Forecasts" = "forecast"),
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
                label = "Compare?",
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
                label = "Date",
                grid = T,
                force_edges = T,
                choices = c(2008:2016), selected = c(2013)
              )),
              sliderTextInput(
                inputId = "elig_year_comp",
                label = "Date",
                grid = T,
                force_edges = T,
                choices = c(2008:2016), selected = c(2010:2013)
              ),
              shinyjs::hidden(sliderTextInput(
                inputId = "elig_forecast_year",
                label = "Date",
                grid = T,
                force_edges = T,
                choices = c(2016:2041), selected = c(2016)
              )),
              shinyjs::hidden(sliderTextInput(
                inputId = "elig_year_comp_forecast",
                label = "Date",
                grid = T,
                force_edges = T,
                choices = c(2016:2041), selected = c(2016, 2020)
              ))
            ),
            box(
              width = 2, title = "Plotting", id = "elig_box_3", solidHeader = T, status = "primary", height = 140,
              actionButton("elig_help", "Help", width = "100%"),
              actionButton("elig_update", "Update", width = "100%")
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
                      "West Midlands" = 0, "Birmingham" = 1, "Dudley" = 2,
                      "Coventry" = 3, "Sandwell" = 4, "Solihull" = 5,
                      "Walsall" = 6, "Wolverhampton" = 7
                    ),
                    selected = 1
                  )
                ),
                addSpinner(leafletOutput("map_elig"), spin = "circle", color = "#E41A1C"),
                absolutePanel(
                  top = "65px", width = 300, right = "40px", style = "z-index: 999; font-size = 5px", id = "elig_map_id",
                  box(
                    title = "Select an Area to View Trends", id = "map_elig_toggle", width = 320,
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
          h2("Inclusive and Healthy Mobility: Understanding Trends in Concessionary Travel in the West Midlands"),
          h3("In this project, we intend to generate new insights into the changing mobility practices of senior residents."),
          p("In this project, we intend to generate new insights into the changing mobility practices of senior residents. The project is funded by the ESRC's Big Data Network Phase 3 and conducted in partnership with Transport for West Midlands (Co-I Chris Lane) and the UCL Consumer Data Research Centre (Co-I Prof Paul Longley)."),
          h2("Changing practices, changing transport"),
          p("There is an ongoing need to study the mobility practices of senior residents as lifestyle practices, age and ethnic characteristics in this group steadily diversify. At the same time, the transport system continues being transformed by new mobile technologies, the emergence of novel, demand-responsive and shared transport services as well as a dynamic information and communication environment propagating online services, shopping, entertainment and socialising, all of which affect the times and ways we travel."),
          h2("Decline in bus patronage"),

          p("Viewed against these developments, the project investigates the changing bus patronage of senior residents in the region of West Midlands (West Midlands Combined Authority - WMCA). Since 2009, the region has experienced a dramatic decline of approximately 25 per cent in bus patronage.* Understanding these trends is an urgent priority for urban transport authorities, not only for economic and operational reasons. Unmet mobility needs can also reinforce health disadvantage and social inequalities among potentially vulnerable groups, such as elderly residents."),
          h2("New forms of urban data"),
          p("WMCA's travel smartcards have produced a database of several hundred million records of bus boardings over nearly seven years. The data constitutes a powerful resource for research, particularly when it is linked to other datasets routinely collected by transport operators in England. Using data linkage procedures, we are developing a complex data processing framework that will permit contextual analysis of changing bus patronage with respect to regularity of daily travel, origins and destinations, travel distance and time and ??? by inference ??? delay experience, trip chaining, trip purpose and the time spent away from home using public transport."),
          h2("Inclusive urban transport systems"),
          p("Apart from contributions to transport and health geography, we expect the research to generate benefits for the UK urban transport sector through updated insights into senior residents' travel needs, the demonstration of transferable data processing solutions, impact assessments of network and service changes as well as an appraisal of strategic policy instruments, such as 'Mobility as a Service' and the English National Concessionary Travel Scheme. As part of a wider vision, the project will add to the debate on inclusive and health-promoting transport systems as a key enabler of independent living in later life stages. ")
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

  # Progress bar updates through compiling the server code.
  progressSweetAlert(
    session = session, id = "myprogress",
    title = "Work in progress",
    display_pct = T, value = 0, striped = T
  )

  # Data Import and preparation ---------------------------------------------

  # identify all input files in the application loading directory.
  files <- str_c("od_data/", list.files("od_data/"))[1:10]

  # Create an empty list then import each of the raw data files into R, This approach
  # allows the progress bar to be updated reflecting data loading progress.
  DT <- list()
  for (i in 1:length(files)) {
    DT[[i]] <- fread(files[i])
    updateProgressBar(session = session, id = "myprogress", value = 50 / length(files) * i, title = paste0("Importing ", i, " of ", length(files), " days."))
  }

  # Combine all tables into a single data.table
  DT <- rbindlist(DT)
  updateProgressBar(session = session, id = "myprogress", value = 70, title = "Combining Data part 1")

  # create the requires three new columns, timestamp, hour and date.
  DT[, c("timestamp", "hour", "date") := .(ymd_hms(str_c(date, " ", orig_time)), hour(ymd_hms(str_c(date, " ", orig_time))), as.Date(date))]

  # Drop columns from the source data which will be be required at a later time.
  DT[, c("dest_time", "orig_time", "orig_naptan", "dest_naptan", "tt_id", "orig_n", "dest_n") := NULL]

  updateProgressBar(session = session, id = "myprogress", value = 80, title = "Combining Data part 2")

  # Split and combine the databable. The goal is that a set of rows exist for each level of geography.
  DT <- rbindlist(list(
    DT[, c("date", "type", "age", "gender", "n", "hour", "orig_oa11cd", "dest_oa11cd")][, scale := "oa"],
    DT[, c("date", "type", "age", "gender", "n", "hour", "orig_lsoa11cd", "dest_lsoa11cd")][, scale := "lsoa"],
    DT[, c("date", "type", "age", "gender", "n", "hour", "orig_msoa11cd", "dest_msoa11cd")][, scale := "msoa"]
  ))
  # Update the column names to origin and destination.
  setnames(DT, c(7, 8), new = c("origin", "destination"))

  # Filter out incomplete cases. (Missing Origin or Destination)
  DT <- DT[complete.cases(DT)]

  updateProgressBar(session = session, id = "myprogress", value = 90, title = "Optimising the table")

  # Create the data.table key. This is critical to performance.
  setkey(DT, scale, date, gender, age, hour, type)

  # Function to add origin and destination latitude and longitude to data.
  add_lat_lon <- . %>%
    left_join(wm_lookup %>% select(code, longitude, latitude), by = c("origin" = "code")) %>%
    rename(x_lon = longitude, x_lat = latitude) %>%
    left_join(wm_lookup %>% select(code, longitude, latitude), by = c("destination" = "code")) %>%
    rename(y_lon = longitude, y_lat = latitude) %>%
    rename(n = N)

  # Function to range standardise frequencies for plotting.
  range01 <- function(x) {
    (x - min(x)) / (((max(x) - min(x))))
  }


  ###########################################################################
  # Who Travels -------------------------------------------------------------
  ###########################################################################


  dygraph_data_1 <- reactive({
    data <- DT[.("msoa"), .N, by = c("date", "gender")][, as.list(setattr(N, "names", gender)), by = list(date)]
    if (input$dygraph_time_resolution == "week") {
      data <- data %>% mutate(date = round_date(date, "week")) %>% group_by(date) %>% summarise_all(.funs = list("sum")) %>% data.table()
    } else if (input$dygraph_time_resolution == "month") {
      data <- data %>% mutate(date = round_date(date, "month")) %>% group_by(date) %>% summarise_all(.funs = list("sum")) %>% data.table()
    } else if (input$dygraph_time_resolution == "quarter") {
      data <- data %>% mutate(date = round_date(date, "3 months")) %>% group_by(date) %>% summarise_all(.funs = list("sum")) %>% data.table()
    } else {
      data
    }
  })


  dygraph_data_2 <- reactive({
    data <- DT[.("msoa"), .N, by = c("date", "age")][, as.list(setattr(N, "names", age)), by = list(date)]

    if (input$dygraph_time_resolution == "week") {
      data <- data %>% mutate(date = round_date(date, "week")) %>% group_by(date) %>% summarise_all(.funs = list("sum")) %>% data.table()
    } else if (input$dygraph_time_resolution == "month") {
      data <- data %>% mutate(date = round_date(date, "month")) %>% group_by(date) %>% summarise_all(.funs = list("sum")) %>% data.table()
    } else if (input$dygraph_time_resolution == "quarter") {
      data <- data %>% mutate(date = round_date(date, "3 months")) %>% group_by(date) %>% summarise_all(.funs = list("sum")) %>% data.table()
    } else {
      data
    }
  })





  # First interactive graph based on gender
  output$dygraph_gender <- renderDygraph({

    # Subset DT for msoas and aggregate by date and gender.
    data <- dygraph_data_1()

    # Create dygraph based on date and gender
    dygraph(data = data, main = paste0("Passengers by Gender (",str_to_title(input$dygraph_time_resolution),")"), 
            group = "dygraphs") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = F
      ) %>%
      dyOptions(stackedGraph = T)
  })

  # Second interactive graph based on age
  output$dygraph_age <- renderDygraph({

    # Subset DT for msoas and aggregate by date and gender.
    data <- dygraph_data_2()

    # Create dygraph based on date and age
    dygraph(data = data, main =paste0("Passengers by Age (",str_to_title(input$dygraph_time_resolution),")"), 
                                     group = "dygraphs") %>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = F
      ) %>%
      dyOptions(stackedGraph = T) %>%
      dyRangeSelector()
  })

  journeys_n <- reactive({
    DT[CJ("msoa", c(as.Date(strftime(req(input$dygraph_gender_date_window[[1]]), "%Y-%m-%d")):
    as.Date(strftime(req(input$dygraph_gender_date_window[[2]]), "%Y-%m-%d")))), .N]
  })

  output$n_journeys <- renderText({
    format(journeys_n(), nsmall = 0, big.mark = ",")
  })


  ###########################################################################
  # Who Travels Where Origin-Destination ------------------------------------
  ###########################################################################

  wm_lookup <- read.csv("data/spatial_data/wm_lookup.csv")
  wm_lookup <- data.table(wm_lookup, key = "scale")

  wm_msoa <- wm_lookup["msoa", .(scale, code, longitude, latitude)]

  observeEvent(input$od_c_version, {
    shinyjs::show("od_c_date_start_2", anim = T, animType = "slide")
  })

  # Comparative OD data Map
  output$map_com <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-1.703929, 52.447027, zoom = 10)
  })

  observe({
    if (input$od_c_zoom_to == 0) {
      leafletProxy("map_com") %>% flyTo(lng = -1.891587, lat = 52.484689, zoom = 10)
    } else if (input$od_c_zoom_to == 1) {
      leafletProxy("map_com") %>% flyTo(lng = -1.890401, lat = 52.486243, zoom = 13L)
    } else if (input$od_c_zoom_to == 2) {
      leafletProxy("map_com") %>% flyTo(lng = -2.081112, lat = 52.512255, zoom = 13L)
    } else if (input$od_c_zoom_to == 3) {
      leafletProxy("map_com") %>% flyTo(lng = -1.519693, lat = 52.406822, zoom = 13L)
    } else if (input$od_c_zoom_to == 4) {
      leafletProxy("map_com") %>% flyTo(lng = -2.010793, lat = 52.536167, zoom = 13L)
    } else if (input$od_c_zoom_to == 5) {
      leafletProxy("map_com") %>% flyTo(lng = -1.777610, lat = 52.411811, zoom = 13L)
    } else if (input$od_c_zoom_to == 6) {
      leafletProxy("map_com") %>% flyTo(lng = -1.982919, lat = 52.586214, zoom = 13L)
    } else if (input$od_c_zoom_to == 7) {
      leafletProxy("map_com") %>% flyTo(lng = -2.128820, lat = 52.586973, zoom = 13L)
    }
  })



  observeEvent(input$od_c_period, {
    if (input$od_c_period != "custom") {
      shinyjs::show("od_c_date_start_1")
      shinyjs::hide("od_c_date_start_custom")
    } else {
      shinyjs::show("od_c_date_start_custom")
      shinyjs::hide("od_c_date_start_1")
    }
  })


  data_od_s <- reactive({
    if (input$od_c_period != "custom") {
      od_c_date_start <- as.Date(input$od_c_date_start_1[[1]])
      od_c_date_end <- as.Date(input$od_c_date_start_1[[1]]) + as.numeric(input$od_c_period)
    } else if (input$od_c_period == "custom") {
      od_c_date_start <- input$od_c_date_start_custom[[1]]
      od_c_date_end <- input$od_c_date_start_custom[[2]]
    }


    # Aggregate DT. Note the use of the Key to optimise query speed.
    DT[CJ(
      c(input$od_c_scale),
      seq(od_c_date_start, od_c_date_end, by = 1),
      c(input$od_c_demographics),
      c(input$od_c_demographics),
      c(input$od_c_time_1[1]:input$od_c_time_1[2]), c(input$od_c_type_1)
    ),
    .N,
    by = c("origin", "destination")
    ][order(N)] %>% add_lat_lon()
  })

  data_od_c <- reactive({

    # Create progress bar.
    withProgress(message = "Processing Request", value = 0, {

      # Update progress bar.
      incProgress(1 / 3, message = "Import Time 1 Data")

      if (input$od_c_period != "custom") {
        od_c_date_start_1 <- as.Date(input$od_c_date_start_1)
        od_c_date_end_1 <- as.Date(input$od_c_date_start_1) + as.numeric(input$od_c_period)
        od_c_date_start_2 <- as.Date(input$od_c_date_start_2)
        od_c_date_end_2 <- as.Date(input$od_c_date_start_2) + as.numeric(input$od_c_period)
      } else if (input$od_c_period == "custom") {
        od_c_date_start_1 <- as.Date(input$od_c_date_start_custom[[1]])
        od_c_date_end_1 <- as.Date(input$od_c_date_start_custom[[2]])
        od_c_date_start_2 <- as.Date(input$od_c_date_start_2)
        od_c_date_end_2 <- as.Date(input$od_c_date_start_2) +
          (as.Date(input$od_c_date_start_custom[[2]]) - as.Date(input$od_c_date_start_custom[[1]]))
      }


      # Query the data for the first data point parameters.
      df1 <- DT[CJ(
        c(input$od_c_scale),
        seq(od_c_date_start_1, od_c_date_end_1, by = 1),
        c(input$od_c_demographics),
        c(input$od_c_demographics),
        c(input$od_c_time_1[1]:input$od_c_time_1[2]), c(input$od_c_type_1)
      ),
      .N,
      by = c("origin", "destination")
      ] %>% rename(n = N)

      # Update progress bar.
      incProgress(1 / 3, message = "Import Time 2 Data")

      # Query the data for the second data point parameters.
      df2 <- DT[CJ(
        c(input$od_c_scale),
        seq(od_c_date_start_2, od_c_date_end_2, by = 1),
        c(input$od_c_demographics),
        c(input$od_c_demographics),
        c(input$od_c_time_1[1]:input$od_c_time_1[2]), c(input$od_c_type_1)
      ),
      .N,
      by = c("origin", "destination")
      ] %>% rename(n = N)

      # Update progress bar.
      incProgress(1 / 3, message = "Merging Datasets")

      # Merge data for time points 1 and 2
      df1 %>%
        full_join(df2, by = c("origin", "destination")) %>%
        mutate(perc_change = as.numeric((n.y - n.x) / n.x) * 100) %>%
        left_join(wm_lookup, by = c("origin" = "code")) %>%
        rename(x_lon = longitude, x_lat = latitude) %>%
        left_join(wm_lookup, by = c("destination" = "code")) %>%
        rename(y_lon = longitude, y_lat = latitude) %>%
        arrange(desc(abs(perc_change)))
    })
  })



  # Update map to reflect new critera. (Static)

  observeEvent(input$od_c_s_update, {
    shinyjs::hide("od_map_toggle", anim = T, animType = "slide")


    if (input$od_c_version == "static") {
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {

        # Update progress bar.
        incProgress(1 / 4, message = "Importing data.")

        # request static data based on specified parameters
        data <- data_od_s() %>%
          filter(complete.cases(.)) %>%
          arrange(desc(n)) %>%
          head(min(input$od_c_nrows, nrow(.)))

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
        mrks <- wm_lookup[.(input$od_c_scale)]

        # Update progress bar.
        incProgress(1 / 4, message = "Building Map.")

        # Update static map with new lines data.
        map <- leafletProxy("map_com", data = lines) %>%
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
            layerId = mrks$code,
            label = mrks$code,
            radius = 1, weight = 1
          ) %>%
          leaflet::addLegend("bottomright",
            layerId = "od_map_legend",
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

  # Update map to reflect new critera.
  observeEvent(input$od_c_s_update, {
    if (input$od_c_version == "comp") {
      shinyjs::hide("od_map_toggle", anim = T, animType = "slide")

      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {

        # Update progress bar.
        incProgress(1 / 4, message = "Importing Map Data")

        data <- data_od_c() %>%
          filter(complete.cases(.)) %>%
          head(min(nrow(.), input$od_c_nrows))

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
        mrks <- wm_lookup[.(input$od_c_scale)]

        incProgress(1 / 4, message = "Building Map.")

        # Update comparative map with new lines data.
        map_com <- leafletProxy("map_com", data = lines2) %>%
          clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          addPolylines(color = binpal(data$perc_change), weight = 1, opacity = 0.5) %>%
          addCircleMarkers(
            lng = mrks$longitude,
            lat = mrks$latitude,
            layerId = mrks$code,
            label = mrks$code,
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
  observeEvent(input$map_com_marker_click, {
    shinyjs::show("od_map_toggle", anim = T, animType = "slide")

    data2a <- data_od_s() %>%
      filter(origin == input$map_com_marker_click[[1]]) %>%
      mutate(dir = "outbound", colour = "#ff0000")

    data2b <- data_od_s() %>%
      filter(destination == input$map_com_marker_click[[1]]) %>%
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
      mrks <- wm_lookup[input$od_c_scale]

      # Update od outbound map with new lines data.
      map <- leafletProxy("map_com", data = lines) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolylines(
          group = "od_line_layer", color = data2$colour,
          weight = 1 + (data2$n * 2), opacity = 0.3
        ) %>%
        addCircleMarkers(
          lng = mrks$longitude,
          lat = mrks$latitude,
          layerId = mrks$code,
          label = mrks$code,
          radius = 2, weight = 2
        )
    }
  })

output$od_c_tbl <- renderDataTable(

  if (input$od_c_period != "custom") {
    if (input$od_c_version == "comp") {
      data_od_c() %>%
        transmute(origin, destination,
          T1_period = paste(input$od_c_date_start_1, "-", as.character(as.Date(input$od_c_date_start_1) + as.numeric(input$od_c_period))),
          T2_period = paste(as.Date(input$od_c_date_start_2), "-", as.character(as.Date(input$od_c_date_start_2) + as.numeric(input$od_c_period))),
          T1 = n.x, T2 = n.y, perc_change
        ) %>%
        left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("origin" = "code")) %>%
        left_join(wm_lookup %>% select(code, "Dest Name" = msoa11nm), by = c("destination" = "code")) %>%
        select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", T1_period, T2_period, T1, T2, perc_change)
    } else if (input$od_c_version == "static") {
      data_od_s() %>%
        transmute(origin, destination,
          T1 = n,
          period = paste(input$od_c_date_start_1, "-", as.character(as.Date(input$od_c_date_start_1) + as.numeric(input$od_c_period)))
        ) %>%
        left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("origin" = "code")) %>%
        left_join(wm_lookup %>% select(code, "Dest Name" = msoa11nm), by = c("destination" = "code")) %>%
        select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", period, T1)
    }
  } else if (input$od_c_period == "custom") {
    if (input$od_c_version == "comp") {
      data_od_c() %>%
        transmute(origin, destination,
          T1_period = paste(input$od_c_date_start_custom[[1]], "-", input$od_c_date_start_custom[[2]]),
          T2_period = paste(as.Date(input$od_c_date_start_2), "-", 
                            as.character(as.Date(input$od_c_date_start_2) + (as.Date(input$od_c_date_start_custom[[2]]) - as.Date(input$od_c_date_start_custom[[1]])))),
          T1 = n.x, T2 = n.y, perc_change
        ) %>%
        left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("origin" = "code")) %>%
        left_join(wm_lookup %>% select(code, "Dest Name" = msoa11nm), by = c("destination" = "code")) %>%
        select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", T1_period, T2_period, T1, T2, perc_change)
    } else if (input$od_c_version == "static") {
      data_od_s() %>%
        transmute(origin, destination, period = paste(input$od_c_date_start_custom[[1]], "-", input$od_c_date_start_custom[[2]]), T1 = n) %>%
        left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("origin" = "code")) %>%
        left_join(wm_lookup %>% select(code, "Dest Name" = msoa11nm), by = c("destination" = "code")) %>%
        select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", period, T1)
    }
  }

  ,
  options = list(pageLength = 10, scrollX = T)
)




  # Comparative Map, data download handler
  output$od_c_downloader <- downloadHandler(
    filename = function() {
      paste0(input$od_c_version, "_od.csv")
    },
    content = function(file) {
      fwrite( if (input$od_c_period != "custom") {
        if (input$od_c_version == "comp") {
          data_od_c() %>%
            transmute(origin, destination,
                      T1_period = paste(input$od_c_date_start_1, "-", as.character(as.Date(input$od_c_date_start_1) + as.numeric(input$od_c_period))),
                      T2_period = paste(as.Date(input$od_c_date_start_2), "-", as.character(as.Date(input$od_c_date_start_2) + as.numeric(input$od_c_period))),
                      T1 = n.x, T2 = n.y, perc_change
            ) %>%
            left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("origin" = "code")) %>%
            left_join(wm_lookup %>% select(code, "Dest Name" = msoa11nm), by = c("destination" = "code")) %>%
            select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", T1_period, T2_period, T1, T2, perc_change)
        } else if (input$od_c_version == "static") {
          data_od_s() %>%
            transmute(origin, destination,
                      T1 = n,
                      period = paste(input$od_c_date_start_1, "-", as.character(as.Date(input$od_c_date_start_1) + as.numeric(input$od_c_period)))
            ) %>%
            left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("origin" = "code")) %>%
            left_join(wm_lookup %>% select(code, "Dest Name" = msoa11nm), by = c("destination" = "code")) %>%
            select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", period, T1)
        }
      } else if (input$od_c_period == "custom") {
        if (input$od_c_version == "comp") {
          data_od_c() %>%
            transmute(origin, destination,
                      T1_period = paste(input$od_c_date_start_custom[[1]], "-", input$od_c_date_start_custom[[2]]),
                      T2_period = paste(as.Date(input$od_c_date_start_2), "-", 
                                        as.character(as.Date(input$od_c_date_start_2) + (as.Date(input$od_c_date_start_custom[[2]]) - as.Date(input$od_c_date_start_custom[[1]])))),
                      T1 = n.x, T2 = n.y, perc_change
            ) %>%
            left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("origin" = "code")) %>%
            left_join(wm_lookup %>% select(code, "Dest Name" = msoa11nm), by = c("destination" = "code")) %>%
            select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", T1_period, T2_period, T1, T2, perc_change)
        } else if (input$od_c_version == "static") {
          data_od_s() %>%
            transmute(origin, destination, period = paste(input$od_c_date_start_custom[[1]], "-", input$od_c_date_start_custom[[2]]), T1 = n) %>%
            left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("origin" = "code")) %>%
            left_join(wm_lookup %>% select(code, "Dest Name" = msoa11nm), by = c("destination" = "code")) %>%
            select(Origin = origin, Dest = destination, "Origin Name", "Dest Name", period, T1)
        }
      }, file)
    }
  )

  observeEvent(input$map_com_marker_click, {
    leafletProxy("map_com") %>% clearControls()
  })

  # eligibility Plot change graph
  plot_data3 <- reactive({
    data2a <- data_od_s() %>%
      filter(origin == input$map_com_marker_click[[1]]) %>%
      mutate(dir = "outbound", colour = "#ff0000") %>%
      arrange(desc(n)) %>%
      head(10)
  })

  # eligibility Plot change graph
  plot_data4 <- reactive({
    data2b <- data_od_s() %>%
      filter(destination == input$map_com_marker_click[[1]]) %>%
      mutate(dir = "inbound", colour = "#00ff00") %>%
      arrange(desc(n)) %>%
      head(10)
  })

  output$inbound_bokeh_plot <- renderRbokeh({
    figure(
      data = plot_data4(),
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

  output$outbound_bokeh_plot <- renderRbokeh({
    figure(
      data = plot_data3(),
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
  od_steps <- reactive(data.frame(
    element = c("imaginary", "#od_box_1", "#od_box_2", "#od_box_3", "#od_box_4", "#od_map_id"),
    intro = c(
      "The purpose of this tab is to explore mobility patterns across the West Midlands. You are able to create and 
            interogate bespoke origin-destination matrices based on dates, times and specific passenger characteristcs.",
      "First specify the details of the first time period. You should select a date, the time of day and the length of the time period.",
      "Second, specify the characteristics of the passengers you wish to observe. Options include age, gender and card-type.",
      "Third, do you wish to make a comparison? If Yes, specify the start date for the comparison. Note, the period and passenger 
            characteristics will remain the same as for period 1.",
      "Last, specify plotting details. You can select a range of spatial scales and the number of observations you wish to plot. Once selected, update the Map",
      "Once the map is plotted, select a circle marker to view local origin-destination data."
    )
  ))

  observeEvent(input$od_help, {
    introjs(session, options = list(steps = od_steps(), "showBullets" = "false", "showProgress" = "true", "showStepNumbers" = "false", "nextLabel" = "Next", "prevLabel" = "Prev"))
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

  # Import the wm output areas shapefile.
  wm_oa <- readShapePoly("data/spatial_data/wm_oa_4326.shp", delete_null_obj = T)
  wm_oa@data <- wm_oa@data[, c("objectid", "oa11cd")]
  wm_oa@data$oa11cd <- as.character(wm_oa@data$oa11cd)

  # Import Amenity Shapefiles
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

      # Example code to add datashine layers into the App.

      # addTiles(group = "Vulnerability", urlTemplate = "https://maps.cdrc.ac.uk/tiles/shine_urbanmask_dark2/{z}/{x}/{y}.png",
      #          option=tileOptions(tms = F, continuousWorld=T, opacity = 1)) %>%


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
  wm_s_data <- reactive({
    data <- wm_oa@data %>%
      left_join(ac_dt %>%
        filter(date == input$ac_date, type == input$ac_type), by = c("oa11cd" = "origin"))
  })

  # Create Reactive table for comparative accessibility
  wm_s_data_comp <- reactive({
    data <- wm_oa@data %>%
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
      wm_s_data_comp() %>%
        transmute(oa11cd, type, T1_date = input$ac_date_comp[[1]], T2_date = input$ac_date_comp[[2]], T1_time = mean_min_time_start, T2_time = mean_min_time_end, Change = mean_min_time)
    } else if (input$ac_c_version == "static") {
      wm_s_data() %>%
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
          wm_s_data_comp() %>%
            transmute(oa11cd, type, T1_date = input$ac_date_comp[[1]], T2_date = input$ac_date_comp[[2]], T1_time = mean_min_time_start, T2_time = mean_min_time_end, Change = mean_min_time)
        } else if (input$ac_c_version == "static") {
          wm_s_data() %>%
            select(oa11cd, type, T1_date = date, T1_time = mean_min_time)
        }, file
      )
    }
  )

  # Map update
  observeEvent(input$ac_c_s_update, {
    wm_oa_cp <- wm_oa

    if (input$ac_c_version == "static") {
      palette <- "YlOrRd"
      reverse <- F
      wm_oa_cp@data <- wm_oa_cp@data %>% left_join(wm_s_data())
      domain <- c(1, max(abs(wm_oa_cp@data$mean_min_time)))
    } else if (input$ac_c_version == "comp") {
      palette <- "RdYlGn"
      reverse <- T
      wm_oa_cp@data <- wm_oa_cp@data %>% left_join(wm_s_data_comp())
      domain <- c((-1 * max(abs(wm_oa_cp@data$mean_min_time))), max(abs(wm_oa_cp@data$mean_min_time)))
    }

    leafletProxy("ac_map", deferUntilFlush = F) %>%
      clearShapes() %>%
      hideGroup(c(
        "Clinic", "GP", "Hospital", "Supermarket (Small)",
        "Supermarket (Medium)", "Supermarket (Large)", "Supermarket (Very Large)", "Train Station"
      )) %>%
      addPolygons(
        data = wm_oa_cp[1:2500, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa11cd, " : ", mean_min_time, " mins.")),
        layerId = ~oa11cd,
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F)
      ) %>%
      addPolygons(
        data = wm_oa_cp[2501:5000, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa11cd, " : ", mean_min_time, " mins.")),
        layerId = ~oa11cd,
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F)
      ) %>%
      addPolygons(
        data = wm_oa_cp[5001:7500, ],
        fillOpacity = 0.7,
        smoothFactor = 0,
        color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
        weight = .5,
        label = ~htmlEscape(paste0(oa11cd, " : ", mean_min_time, " mins.")),
        layerId = ~oa11cd,
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F)
      ) %>%
      addPolygons(
        data = wm_oa_cp[7501:nrow(wm_oa_cp), ],
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
      data <- wm_s_data()
      reverse <- F
      domain <- c(1, max(abs(data$mean_min_time)))
    } else if (input$ac_c_version == "comp") {
      palette <- "RdYlGn"
      title <- html("Change in Travel Time (mins)")
      data <- wm_s_data_comp()
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
      "The purpose of this tab is to visualise changes in accessibility across the West Midlands. You can pick 
            specific amenity types and either view annual elibility or make comparison between years.",
      "Select the type of Amenity you are interested in.",
      "Do you want to see a static or comparative map?",
      "What dates are you interested in?",
      "Update the Map",
      "Once the map is plotted, select a circle marker to view local accessibility data."
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
  elig_data <- fread("data/eligibility/eligible_pop_wm.csv", data.table = F) %>%
    rename(geo_code = lsoa11cd)
  elig_forecast <- fread("data/eligibility/elegible_pop_wm_forecast.csv", data.table = F) %>%
    rename(value = count, geo_code = AREA_CODE)

  wm_lsoa <- readShapePoly("data/spatial_data/wm_lsoa.shp")
  wm_lsoa@data <- wm_lsoa@data %>% transmute(geo_code = lsoa11cd)

  wm_la <- readShapePoly("data/spatial_data/infuse_dist_lyr_2011_clipped2.shp")
  wm_la@data <- wm_la@data["geo_code"]


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
  wm_lsoa_data <- reactive({
    elig_data[elig_data$measure == input$elig_year[[1]], ]
  })

  # Reactive data for Static Forecast eligibility map.
  wm_lsoa_data_forecast <- reactive({
    elig_forecast[elig_forecast$year == input$elig_forecast_year[[1]], ]
  })


  # Reactive data for Comparative eligibility Map.
  wm_lsoa_data_comp <- reactive({
    elig_data[elig_data$measure == input$elig_year_comp[[1]], ] %>%
      inner_join(elig_data[elig_data$measure == input$elig_year_comp[[2]], ], by = "geo_code") %>%
      transmute(geo_code, value.y, value.x, value = round(100 * (value.y - value.x) / value.x, 1))
  })

  # Reactive data for Comparative eligibility Map.
  wm_lsoa_data_forecast_comp <- reactive({
    elig_forecast[elig_forecast$year == input$elig_year_comp_forecast[[1]], ] %>%
      inner_join(elig_forecast[elig_forecast$year == input$elig_year_comp_forecast[[2]], ], by = "geo_code") %>%
      transmute(geo_code, value.y, value.x, perc_change = round((100 * (value.y - value.x) / value.x), 2))
  })


  output$elig_s_tbl <- renderDataTable(

    if (input$elig_source == "estimate") {
      if (input$elig_version == "comp") {
        wm_lsoa_data_comp() %>%
          transmute(geo_code,
            t1_year = input$elig_year_comp[[1]], t2_year = input$elig_year_comp[[2]],
            t1_pop = value.x, t2_pop = value.y, perc_change = value) %>%
          left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("geo_code" = "code"))
      } else if (input$elig_version == "static") {
        wm_lsoa_data() %>%
          transmute(geo_code, t1_year = input$elig_year_comp[[1]], t1_pop = value) %>%
          left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("geo_code" = "code"))
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == "static") {
        wm_lsoa_data_forecast() %>%
          transmute(geo_code, t1_year = input$elig_year_comp_forecast[[1]], t1_pop = value) %>%
          left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("geo_code" = "code"))
        } else if (input$elig_version == "comp") {
        wm_lsoa_data_forecast_comp() %>%
            transmute(geo_code, t1_year =input$elig_year_comp_forecast[[1]], 
                      t2_year = input$elig_year_comp_forecast[[2]], t1_pop = value.x, t2_pop = value.y, perc_change) %>%
            left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("geo_code" = "code"))
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
            wm_lsoa_data_comp() %>%
              transmute(geo_code,
                        t1_year = input$elig_year_comp[[1]], t2_year = input$elig_year_comp[[2]],
                        t1_pop = value.x, t2_pop = value.y, perc_change = value) %>%
              left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("geo_code" = "code"))
          } else if (input$elig_version == "static") {
            wm_lsoa_data() %>%
              transmute(geo_code, t1_year = input$elig_year_comp[[1]], t1_pop = value) %>%
              left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("geo_code" = "code"))
          }
        } else if (input$elig_source == "forecast") {
          if (input$elig_version == "static") {
            wm_lsoa_data_forecast() %>%
              transmute(geo_code, t1_year = input$elig_year_comp_forecast[[1]], t1_pop = value) %>%
              left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("geo_code" = "code"))
          } else if (input$elig_version == "comp") {
            wm_lsoa_data_forecast_comp() %>%
              transmute(geo_code, t1_year =input$elig_year_comp_forecast[[1]], 
                        t2_year = input$elig_year_comp_forecast[[2]], t1_pop = value.x, t2_pop = value.y, perc_change) %>%
              left_join(wm_lookup %>% select(code, "Origin Name" = msoa11nm), by = c("geo_code" = "code"))
          }
        }
        , file
      )
    }
  )

  # Map update
  observeEvent(input$elig_update, {
    if (input$elig_source == "estimate") {
      wm_temp <- wm_lsoa

      if (input$elig_version == "static") {
        palette <- "Blues"
        wm_temp@data <- wm_temp@data %>% left_join(wm_lsoa_data(), by = "geo_code")
        domain <- c(1, max(abs(wm_temp@data$value)))
        suffix <- ""
      } else if (input$elig_version == "comp") {
        wm_temp@data <- wm_temp@data %>% left_join(wm_lsoa_data_comp(), by = "geo_code")
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(wm_temp@data$value)), 0, max(abs(wm_temp@data$value)))
        suffix <- " %"
      }
    } else if (input$elig_source == "forecast") {
      wm_temp <- wm_la
      if (input$elig_version == "static") {
        palette <- "Blues"
        wm_temp@data <- wm_temp@data %>% left_join(wm_lsoa_data_forecast(), by = "geo_code")
        domain <- c(1, max(abs(wm_temp@data$value)))
        suffix <- ""
      } else if (input$elig_version == "comp") {
        wm_temp@data <- wm_temp@data %>% left_join(wm_lsoa_data_forecast_comp() %>%
          rename(value = perc_change), by = "geo_code")
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(wm_temp@data$value)), 0, max(abs(wm_temp@data$value)))
        suffix <- " %"
      }
    }



    leafletProxy("map_elig") %>%
      clearShapes() %>%
      addPolygons(
        data = wm_temp[1:floor(nrow(wm_temp@data) / 2), ],
        smoothFactor = 0,
        fillOpacity = 0.7,
        label = ~htmlEscape(paste0(geo_code, " : ", value, suffix)),
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F),
        layerId = ~geo_code,
        color = ~colorNumeric(palette, domain = domain)(value), weight = .5
      ) %>%
      addPolygons(
        data = wm_temp[(floor(nrow(wm_temp@data) / 2) + 1):nrow(wm_temp@data), ],
        smoothFactor = 0,
        fillOpacity = 0.7,
        layerId = ~geo_code,
        label = ~htmlEscape(paste0(geo_code, " : ", value, suffix)),
        highlightOptions = highlightOptions(color = "black", weight = 3, bringToFront = F),
        color = ~colorNumeric(palette, domain = domain)(value), weight = .5
      )
  })

  # Update Eligibility Legend.
  observeEvent(input$elig_update, {
    proxy <- leafletProxy("map_elig")

    if (input$elig_source == "estimate") {
      if (input$elig_version == "static") {
        title <- ""
        palette <- "Blues"
        data <- wm_lsoa_data()
        domain <- c(1, max(abs(data$value)))
      } else if (input$elig_version == "comp") {
        title <- ""
        data <- wm_lsoa_data_comp()
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(data$value)), 0, max(abs(data$value)))
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == "static") {
        title <- ""
        palette <- "Blues"
        data <- wm_lsoa_data_forecast()
        domain <- c(1, max(abs(data$value)))
      } else if (input$elig_version == "comp") {
        title <- ""
        data <- wm_lsoa_data_forecast_comp() %>% rename(value = perc_change)
        palette <- "RdYlGn"
        domain <- c(-1 * max(abs(data$value)), 0, max(abs(data$value)))
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
      .[order(.$measure), ]
  } else if (input$elig_source == "forecast") {
    elig_forecast %>% 
      filter(geo_code == na.omit(c(input$map_elig_shape_click[[1]], "E08000025"))[1]) %>%
      rename(measure = year) %>%
      .[order(.$measure), ]
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
        x = measure,
        y = value,
        width = 2
      )
  })

  ### Help tour for Elefibility
  eligibility_steps <- reactive(data.frame(
    element = c("imaginary", "#elig_box_1", "#elig_box_2", "#elig_box_3", "#elig_map_id"),
    intro = c(
      "The purpose of this tab is to visualise changes in eligibility for concessionary 
            services across the West Midlands. You can either view annual elibility or make comparison between years.",
      "Do you want to see a static or comparative map?",
      "What dates are you interested in?",
      "Update the Map",
      "Once the map is plotted, select a circle marker to view local eligibility data."
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
  outputOptions(output, "map_com", suspendWhenHidden = F)

  updateProgressBar(session = session, id = "myprogress", value = 100, title = "Done!")
  closeSweetAlert(session = session)
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