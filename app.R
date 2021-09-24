# Dashboard app based on R Shiny to visualise various transport data generated
# as part of the Inclusive and Healthy Mobility project at UCL

# Developed by Alistair Leak https://www.linkedin.com/in/alistairleak/ as part
# of the Inclusive and Healthy Mobility project at UCL.

# Improved and backend transferred to PostgreSQL by Sakinah Yusof, Alfie Long, and Timothy Bruce.

################################

## app.R ##
library(shinydashboard) # Dashboard.
library(data.table) # Use of data.tables - high performance data in R
library(leaflet) # Interface to leaflet maps
library(leaflet.minicharts) # Display flows on leaflet maps
library(RColorBrewer) # Color Ramps
library(dplyr) # tools for data manipulation
library(stringr) # tools for handling strings
library(lubridate) # Tools for handling dates and times
library(dygraphs) # Access to dygraphs javascript in R
library(shinyWidgets) # Helper functions for shiny
library(htmltools) # Helper functions for Shiny
library(rbokeh) # Access to Bokeh plotting in R
library(maptools) # read and write shapefiles
library(shinyjs) # Additional java script functionality
library(forcats) # Helper functions for working with factors
library(rintrojs) # Javascript based introduction to app
library(sf) # for creating sf objects
library(DT) # To render the data output tables
library(shinyTime) # To intuitively input time into the App
library(viridisLite) #tool to bind viridis to leaflet
library(viridis) #tool use viridis palette 
library(mapview) #To easily save displayed maps as .png or .pdf file

library(RPostgreSQL) # r to  PostgreSQL specific interface wrapper
library(DBI) # Basic SQL database connection package
library(pool) # Tool to handle multiple connections to the same database
library(glue) # To paste together complex text, mainly used in the SQL commands
library(wesanderson) #tool to use wespalette

####HTML fix
#to make NA value correctly positioned (underneath) in legend for continous variables
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

#options(shiny.trace = F)

#######################################################################################################
# Data ---------------------------------------------------------------------------------------

#If these variables being assigned inside of the server itself becomes problematic (about line 900)
#comment out the assigning of the variables there and uncomment and fill in here

#load in the env variables for connecting to database
db_name=Sys.getenv("DBNAME")
host_name=Sys.getenv("HOSTNAME")
user_name=Sys.getenv("AGGDASHUSERNAME")
password_name=Sys.getenv("AGGDASHPASSNAME")

port_no=Sys.getenv("PORTNAME")

schema_name <- Sys.getenv("AGGDASHSCHEMANAME")

#####################
#Set up DB connection pool
pool <- dbPool(drv = dbDriver("PostgreSQL"),
               dbname = db_name, 
               host = host_name, 
               port = port_no,
               user = user_name, 
               password = password_name)



#######################################################################################################
# UI definition ---------------------------------------------------------------------------------------

ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = tagList(tags$span(class = "logo-mini", icon("dyalog")), tags$span(class="logo-lg", "Dashboard")), tags$li(class = "dropdown", actionButton("full_reset", "Refresh App"))),
    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        #menu of all tabs
        menuItem("Where and When", icon = icon("project-diagram"), startExpanded = TRUE,
                 menuSubItem("O-D Flows", tabName = "tab_od_c_real_od"),
                 menuSubItem("In or Outflow by Area", tabName = "gen_att_tab")),
        menuItem("Who Travels", tabName = "tab_dygraphs", icon = icon("users")),
        menuItem("Access to Services", tabName = "tab_access", icon = icon("bus")),
        menuItem("Who is Eligible", tabName = "tab_elig", icon = icon("user")),
        # Here we will have instructions, credits etc
        menuItem("About", tabName = "tab_about", icon = icon("info")),
        bookmarkButton(
          label = "",
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
      tags$script(HTML("$('body').addClass('sidebar-mini');")),
      tabItems(
        
        ##### Where and When -------------------------------------------------------------------------------------------------------
        tabItem(
          tabName = "tab_od_c_real_od",
          tags$h4("Explore origin-destination flows", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.5em;"),
          
          ## Flows sub-tab ------
          
          # Top menu bar has all control elements
          fluidRow(
            box(
              width = 3, title = "When?", id = "od_box_1_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                airDatepickerInput("od_c_real_date_start_1",
                                   label = "Start month",
                                   value = "2015-10-01",
                                   maxDate = "2016-08-01",
                                   minDate = "2015-08-01",
                                   view = "months",
                                   minView = "months",
                                   dateFormat = "yyyy-mm"
                ),
                em(h5("These flows represent the trips on a certain provider. Data currently available for the period 2015-08-11 to 2016-08-18"))
              ),
              column(
                12,
                #shinyjs::disabled(
                sliderInput(
                  inputId = "od_c_real_period",
                  label = "Period in months",
                  min = 1, max = 24, post = " months(s)", value = 1
                )
                #)
              )
            ),
            box(
              width = 3, title = "Who?", id = "od_box_2_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                shinyjs::disabled(
                pickerInput(
                  inputId = "od_c_real_demographics",
                  label = "Demographics",
                  choices = list(
                    "Age" = list(
                      "61 - 65" = "61",
                      "66 - 70" = "66",
                      "71 - 75" = "71",
                      "76 - 80" = "76",
                      "81 +   " = "81"
                    ),
                    "Gender" = list(
                      "Male" = "M",
                      "Female" = "F"
                    )
                  ),
                  selected = c("61", "66", "71", "76", "81", "M", "F"),
                  options = list(
                    `actions-box` = T,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = T
                )
                )
              ),
              br(),
              column(
                12,
                shinyjs::disabled(
                pickerInput(
                  inputId = "od_c_real_type_1",
                  label = "Card type",
                  choices = list(
                    "Concessionary" = "con",
                    "Non-Concessionary" = "non",
                    "Disability" = "dis"
                  ),
                  selected = "con",
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = T
                )
                )
              ),
              column(
                12,
                shinyjs::disabled(
                sliderInput("od_c_real_time_1",
                            label = "Time of Day",
                            min = 0, max = 24, post = ":00", value = c(0, 24)
                )
                )
              )
            ),
            box(
              width = 3, title = "Compare to another time period", id = "od_box_3_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                materialSwitch(label=strong("Compare?"),
                               inputId = "od_c_real_version", status = "primary", value = FALSE
                               
                )
              ),
              br(), br(),
              column(
                12,
                airDatepickerInput("od_c_real_date_start_2",
                                   label = "Start month of 2nd period",
                                   value = "2015-11-01",
                                   maxDate = "2016-08-01",
                                   minDate = "2015-08-01",
                                   view = "months",
                                   minView = "months",
                                   dateFormat = "yyyy-mm"
                )
              )),
            box(
              width = 3, title = "Plot", id = "od_box_4_real", status = "warning",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                #shinyjs::disabled(  # sy: enabled other scale options
                pickerInput(
                  inputId = "od_c_real_scale",
                  label = "Spatial scale ",
                  width = "100%",
                  choices = list(
                    "LSOA" = "lsoa",
                    "MSOA" = "msoa",
                    "Local Authority" = "la"
                  ),
                  selected = "lsoa",
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = F
                )
                #)
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
                             style = "color: #fff; background-color: 	#FF8C00; border-color: #FF8C00")
              )
            )
          ),
          # Second row contains maps and data output.
          fluidRow(
            tabBox(id="od_map_fluid_row_id",
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
                     addSpinner(leafletOutput("map_com_real", height = 600), spin = "circle", color = "#E41A1C"),
                     absolutePanel(
                       top = "65px", width = 300, length = 700, right = "40px", style = "z-index: 999; font-size = 5px", id = "od_map_id",
                       box(
                         title = "Click on the map to view details", id = "od_map_real_toggle",
                         width = 320, style = "background: transparent; font-size: 8px;", status = "primary", solidHeader = T,
                         addSpinner(rbokehOutput("inbound_bokeh_plot_real", height = 250, width = 290),
                                    spin = "circle",
                                    color = "#E41A1C"
                         ),
                         addSpinner(rbokehOutput("outbound_bokeh_plot_real", height = 250, width = 290),
                                    spin = "circle",
                                    color = "#E41A1C"), collapsible = T, collapsed = T
                       )
                       
                     )),
                   tabPanel(
                     "Data",
                     dataTableOutput("od_c_real_tbl"),
                     downloadButton("od_c_real_downloader", "Download Data")
                   )
            )
          )),
        
        ## Generation / Attraction sub-tab ------
        tabItem(
          tabName = "gen_att_tab",
          tags$h4("Explore where and when trips take place", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.5em;"),
          fluidRow(
            box(
              width = 3, title = "When?", id = "gen_att_box_1_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                airDatepickerInput("gen_att_date_start_1",
                                   label = "Start month",
                                   value = "2015-08-01",
                                   maxDate = "2016-08-01",
                                   minDate = "2015-08-01",
                                   view = "months", 
                                   minView = "months",
                                   dateFormat = "yyyy-mm"
                ),
                em(h5("These data represent the trips on a certain provider. Data currently available for the period 2015-08-11 to 2016-08-18"))
              ),
              column(
                12,
                sliderInput(
                  inputId = "gen_att_period",
                  label = "Period in months",
                  min = 1, 
                  max = 24, 
                  post = " month(s)", 
                  value = 1
                )
                
              )
            ),
            box(
              width = 3, title = "Who?", id = "gen_att_2_real", status = "primary",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                shinyjs::disabled(
                pickerInput(
                  inputId = "gen_att_demographics",
                  label = "Demographics",
                  choices = list(
                    "Age" = list(
                      "61 - 65" = "61",
                      "66 - 70" = "66",
                      "71 - 75" = "71",
                      "76 - 80" = "76",
                      "81 +   " = "81"
                    ),
                    "Gender" = list(
                      "Male" = "M",
                      "Female" = "F"
                    )
                  ),
                  selected = c("61", "66", "71", "76", "81", "M", "F"),
                  options = list(
                    `actions-box` = T,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = T
                )
                )
              ),
              br(),
              column(
                12,
                shinyjs::disabled(
                  pickerInput(
                    inputId = "gen_att_type_1",
                    label = "Card type",
                    choices = list(
                      "Concessionary" = "con",
                      "Non-Concessionary" = "non",
                      "Disability" = "dis"
                    ),
                    selected = "con",
                    options = list(
                      `actions-box` = T,
                      size = 10
                    ),
                    multiple = T
                  )
                )
              ),
              column(
                12,
                shinyjs::disabled(
                sliderInput("gen_att_time_1",
                            label = "Time of Day",
                            min = 0, max = 24, post = ":00", value = c(0, 24)
                )
                )
              )
            ),
            box(
              width = 3, 
              title = "Compare to another time period",
              id = "gen_att_3_real", 
              status = "primary",
              solidHeader = T, 
              style = "min-height: 270px;",
              column(
                12,
                materialSwitch(label = strong("Compare?"),
                               inputId = "gen_att_c_version", status="primary", value = FALSE)
                
              ),
              br(), br(),
              column(
                12,
                airDatepickerInput("gen_att_date_start_2",
                                   label = "Start month of 2nd period",
                                   value = "2015-09-01",
                                   maxDate = "2016-08-01",
                                   minDate = "2015-08-01",
                                   view = "months", 
                                   minView = "months",
                                   dateFormat = "yyyy-mm"
                )
              )
            ),
            box(
              width = 3, title = "Plot", id = "gen_att_4_real", status = "warning",
              solidHeader = T, style = "min-height: 270px;",
              column(
                12,
                radioGroupButtons(
                  label = "Outflow or Inflow?",
                  inputId = "gen_att_or_version",
                  choices = list("Out" = "gen", "In" = "att"),
                  selected = "gen",
                  justified = T,
                  individual = T,
                  width = "100%",
                  checkIcon = list(
                    yes = icon("ok", lib = "glyphicon")
                  )
                )
              ),
              column(
                12,
                #shinyjs::disabled(  # sy: enabled other scale options
                pickerInput(
                  inputId = "att_gen_scale",
                  label = "Spatial scale ",
                  width = "100%",
                  choices = list(
                    "LSOA" = "lsoa",
                    "MSOA" = "msoa",
                    "Local Authority" = "la"
                  ),
                  selected = "lsoa",
                  options = list(
                    `actions-box` = T,
                    size = 10
                  ),
                  multiple = F
                )
                #)
              ),
              
              column(
                12,
                actionButton("gen_att_help_real", "Help", width = "100%"),
                actionButton("gen_att_c_update", "Update",
                             width = "100%",
                             style = "color: #fff; background-color: 	#FF8C00; border-color: #FF8C00"),
                shinyjs::hidden(actionButton("gen_att_c_reset", "Reset", width = "100%",
                                             style = "color: #fff; background-color: 	#000000; border-color: #000000"))
              ))
          ),
          # Second row contains maps and data output.
          fluidRow(
            HTML(html_fix),
            tabBox(
              width = 12,
              tabPanel(
                title = "Map",
                absolutePanel(
                  top = "65px", left = "80px", style = "z-index: 1000;",
                  selectInput(
                    inputId = "gen_att_zoom_to", label = NULL, width = "150px",
                    choices = list(
                      "West Midlands CA" = 0, "Birmingham" = 1, "Dudley" = 2,
                      "Coventry" = 3, "Sandwell" = 4, "Solihull" = 5,
                      "Walsall" = 6, "Wolverhampton" = 7
                    ),
                    selected = 0
                  )
                ),
                addSpinner(leafletOutput("map_gen_att", height = 600), spin = "circle", color = "#E41A1C"),
                absolutePanel(
                  top = "65px", width = 300, length = 350, right = "40px", style = "z-index: 999; font-size = 5px", id = "gen_att_map_id",
                  box(
                    title = "Click on the map to view details", id = "gen_att_map_toggle",
                    width = 320, style = "background: transparent; font-size: 8px;", status = "primary", solidHeader = T,
                    addSpinner(rbokehOutput("gen_att_bokeh_plot_real", height = 250, width = 290),
                               spin = "circle",
                               color = "#E41A1C"
                    ), collapsible = T, collapsed = T
                  )
                )
              ),
              
              
              tabPanel(
                "Data",
                dataTableOutput("gen_att_c_real_tbl"),
                downloadButton("gen_att_downloader", "Download Data")
              )
            )
          ) 
        ),
        
        ##### Who Travels -------------------------------------------------------------------------------------------------------------------
        tabItem(
          tabName = "tab_dygraphs",
          tags$h4("Explore who travels over time (ENCTS Concessions)", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.5em;"),
          
          #sidebar with demographic input options 
          
          sidebarLayout(
            sidebarPanel(width = 2,
                         verticalLayout(
                           box(width = 15, title = "Who and When", id = "tygraph_box_1", status = "primary", solidHeader = T, style = "min-height: 200px;", 
                               verticalLayout(
                                 shinyjs::disabled(
                                 selectInput(
                                   inputId = "dygraph_statistic", label = "Statistic",
                                   choices = list("Journeys" = "journeys", "Passengers" = "passengers", "Journeys per person" = "journeys_pp")
                                   )
                                 ),
                                 selectInput(
                                   inputId = "dygraph_type", label = "Passenger type",
                                   choices = list(
                                     "Over 60 Concession" = "Over 60 Concession",
                                     "Disabled Concession" = "Disabled Concession"
                                   )
                                 ),
                                 selectInput(
                                   inputId = "dygraph_time_resolution", label = "Resolution",
                                   choices = list("Month" = "month", "Quarter" = "quarter")
                                 ), 
                                 materialSwitch(inputId = "dygraph_table_show_1", label = "Show Datatable", status = "primary",
                                                value= FALSE)
                               )
                           )
                         )
            ),
            
            
            #main panel containing dygraphs
            
            mainPanel(width = 10,
                      tabBox(width = 12,
                             
                             
                             #Breakdown by Gender tab
                             
                             tabPanel(title = "Breakdown by gender", 
                                      fluidRow(width = 12,
                                               dygraphOutput("dygraph_gender", height = "500px", width = "98%")
                                      ), 
                                      
                                      #adds a row with the data table 
                                      
                                      fluidRow(box(width = 12, dataTableOutput("dt1"), downloadButton("gender_downloader", "Download Data"))
                                      )), 
                             
                             #Breakdown by Age tab
                             
                             tabPanel(title = "Breakdown by age", 
                                      fluidRow( 
                                        dygraphOutput("dygraph_age", height = "500px", width = "98%")), 
                                      fluidRow(box(width = 12, dataTableOutput("dt2"), downloadButton("age_downloader", "Download Data"))) 
                             )
                      )
            )
          )
        ), 
        
        ##### Access to Services -----------------------------------------------------------------------------------------------------
        
        tabItem(
          tabName = "tab_access",
          tags$h4("Explore local access to services", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.5em;"),
          tabPanel("Exploring Accessibility",
                   fluidRow(
                     box(
                       width = 3, title = "What?", id = "ac_box_1", status = "primary", solidHeader = T, style = "min-height: 95px;",
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
                       width = 3, title = "When?", id = "ac_box_2", status = "primary", solidHeader = T, height= "137px",
                       airDatepickerInput(
                         inputId = "ac_date",
                         label = "Date",
                         value = "2014-01-07",
                         maxDate = "2016-10-11",
                         minDate = "2010-10-05",
                         view="months",
                         minView = "month",
                         dateFormat = "yyyy-mm"
                       )
                     ),
                     #decrease the marigin under date picker
                     box(
                       width = 4, title = "Compare accessibility to another time period", id = "ac_box_3", 
                       status = "primary", solidHeader = T, height = "137px",
                       column(
                         5, br(),
                         materialSwitch(label = strong("Compare?"),
                                        inputId = "ac_c_version", status= "primary", value = FALSE)
                       ),
                       column(
                         7,
                         airDatepickerInput(
                           inputId = "ac_date_comp",
                           label = "2nd Date",
                           value = "2015-01-07",
                           maxDate = "2016-10-11",
                           minDate = "2010-10-05",
                           view="months", 
                           minView = "months",
                           dateFormat = "yyyy-mm"
                         )
                       )
                     ),
                     box(
                       width = 2, title = "Plot", id = "ac_box_4", status = "warning", solidHeader = T, style = "min-height: 95px;",
                       actionButton("access_help", "Help", align = "center", width = "100%"),
                       actionButton("ac_c_s_update", "Update",
                                    align = "center", width = "100%",
                                    style = "color: #fff; background-color: 	#FF8C00; border-color: #FF8C00"
                       )
                     )
                   ),
                   fluidRow(
                     tabBox(
                       width = 12,
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
                         
                         addSpinner(leafletOutput("ac_map", height = 600), spin = "circle", color = "#E41A1C"),
                         
                         absolutePanel(
                           top = "65px", width = 300, length = 350, right = "40px", style = "z-index: 999; font-size = 5px", id = "ac_map_id",
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
          )
          
          ), 
        
        ##### Who is eligible ----------------------------------------------------------------------------------------------------------------
        
        tabItem(
          tabName = "tab_elig",
          tags$h4("View local eligibility to concessionary travel", style = "display: block; font-size: 1.5em; margin-top: 0.1em; margin-bottom: 0.5em;"),
          fluidRow(
            box(
              width = 3, title = "What?", id = "elig_box_0", solidHeader = T, status = "primary", style="min-height: 95px",
              radioGroupButtons(
                inputId = "elig_source",
                label = "What eligibility?",
                choices = list("Historical" = "estimate", "Forecasts" = "forecast"),
                selected = "estimate",
                width = "100%",
                individual = T,
                justified = T,
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              )
            ),
            box(
              width = 3, title = "When?", id = "elig_box_1", solidHeader = T, status = "primary", style="min-height: 95px",
              # shinyjs::hidden(
              airDatepickerInput(
                inputId = "elig_year",
                label = "Date",
                value="2010",
                minDate="2008",
                maxDate="2016",
                view="years",
                minView="years",
                dateFormat="yyyy")
              # )
              ,
              airDatepickerInput(
                inputId = "elig_forecast_year",
                label = "Date",
                value = "2016",
                minDate="2016",
                maxDate="2041",
                view="years",
                minView="years",
                dateFormat="yyyy")
            ),
            box(
              width = 4, title = "Compare eligibility to another time period", id = "elig_box_2", solidHeader = T, status = "primary", style="min-height: 95px",
              column (5, br(), 
                      materialSwitch(inputId = "elig_version", label=strong("Compare?"), status = "primary", value = FALSE)  
              ),
              
              column(7,
                     airDatepickerInput(inputId = "elig_year_comp",
                                        label = "2nd Date",
                                        value="2011",
                                        minDate="2008",
                                        maxDate="2016",
                                        view="years",
                                        minView="years",
                                        dateFormat="yyyy"),
                     airDatepickerInput(inputId = "elig_year_comp_forecast",
                                        label = "2nd Date",
                                        value = "2017",
                                        minDate="2016",
                                        maxDate="2041",
                                        view="years",
                                        minView="years",
                                        dateFormat="yyyy")
                     
                     
              )
            ),
            box(
              width = 2, title = "Plot", id = "elig_box_3", solidHeader = T, status = "warning", style="min-height: 95px",
              actionButton("elig_help", "Help", width = "100%"),
              actionButton("elig_update", "Update",
                           width = "100%",
                           style = "color: #fff; background-color: 	#FF8C00; border-color: #FF8C00"
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
                addSpinner(leafletOutput("map_elig", height=600), spin = "circle", color = "#E41A1C"),
                absolutePanel(
                  top = "65px", width = 300, length = 350, right = "40px", style = "z-index: 999; font-size = 5px", id = "elig_map_id",
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
        # Third tab content: Details about how to use tool and credits ----------------------------------------
        tabItem(
          tabName = "tab_about",
          p("This dashboard was developed as part of the research project 'Inclusive and Healthy Mobility: Understanding Trends in Concessionary Travel in the West Midlands', conducted at University College London in partnership with Transport for West Midlands. Click here to find more information about the project."),
          p("The dashboard was created using", tags$a(href = "https://shiny.rstudio.com/", "R Studio Shiny", target="_blank"), " by ", tags$a(href = "https://www.linkedin.com/in/alistairleak/", "Alistair Leak", target="_blank"), ", and updated by ", tags$a(href = "https://github.com/aclong", "Alfie Long", target="_blank"),", ", tags$a(href = "https://github.com/Timothy-Bruce", "Timothy Bruce", target="_blank"), " and ", tags$a(href = "https://github.com/majki00", "Michal Iliev", target="_blank"), "."),
          p("Research team: ", tags$a(href = "https://www.ucl.ac.uk/bartlett/casa/jens-kandt", "Jens Kandt", target="_blank"), " (PI), ", tags$a(href = "https://www.geog.ucl.ac.uk//people/academic-staff/paul-longley", "Paul Longley", target="_blank"), " (Co-I), Alistair Leak, ",tags$a(href = "https://github.com/aclong", "Alfie Long", target="_blank"), ", " , tags$a(href = "https://www.ucl.ac.uk/geospatial-analytics/people/ffion-carney", "Ffion Carney", target="_blank"), " - University College London"),
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
              p("Origin-destination flows of ENCTS passengers between stops from August 2015 to August 2016 aggregated from bus stop-level up to OAs, LSOAs, and MSOAs."),
              p("The data record the frequency of journeys between stops, OAs, LSOAs, and MSOAs where it was possible to determine both journeys' origins and destinations. These counts do not represent all journeys and are limited to a specific service provider.")
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

####################################################################################################### 
# Functions ---------------------------------------------------------------------------------------

# Server does all the backend work,
server <- function(input, output, session) {
  
  
  #allow refreshing of browser or app
  session$allowReconnect(TRUE)
  
  #reset button
  observeEvent(input$full_reset, {
    
    #reload app
    session$reload()
    
  })
  
  # Function to range standardise frequencies for plotting.
  range01 <- function(x) {
    (x - min(x)) / (((max(x) - min(x))))
  }
  
  
  ########################################################################################################
  # Where and When Tab ----------------------------------------------------------------------------------
  ## Flow sub-tab-----
  
  
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
  
  observeEvent(input$od_c_real_version,{
    #allow to choose the second date only if the comaprison button is on (TRUE)
    if(input$od_c_real_version==FALSE){
      shinyjs::disable("od_c_real_date_start_2")
    } else if (input$od_c_real_version==TRUE){
      shinyjs::enable("od_c_real_date_start_2")
    }
  })
  
  data_od_s_real <- reactive({
    
    withProgress(message = "Processing Request", value = 0, {
      
      
      incProgress(1 / 2, message = "Importing data.")
      
      #subset points and lookup table columns according to scale 
      
      if(input$od_c_real_scale=="lsoa"){
        input_scale_string <- input$od_c_real_scale
      }else{
        input_scale_string <- paste0("lsoa, ",input$od_c_real_scale)
      }
      
      #SQL query using inputs
      
      DT2_s <- setDT(dbGetQuery(pool, glue(
        
        "SELECT f.sum_nf AS sum_nf, f.origin_area_code AS origin, f.destination_area_code AS destination, f.orig_lon, f.orig_lat, f.orig_common_name, st_x(y.geometry) AS dest_lon, st_y(y.geometry) AS dest_lat, COALESCE(y.common_name, f.destination_area_code) AS dest_common_name ",
        "FROM (SELECT e.sum_nf, e.origin_area_code, e.destination_area_code, st_x(x.geometry) AS orig_lon, st_y(x.geometry) AS orig_lat, COALESCE(x.common_name, e.origin_area_code) AS orig_common_name ",
        "FROM (SELECT SUM(d.sum_nf) AS sum_nf, d.origin_area_code, d.destination_area_code ",
        "FROM (SELECT c.sum_nf, c.destination, c.origin_area_code, j.{input$od_c_real_scale} AS destination_area_code ",
        "FROM (SELECT a.sum_nf, a.origin, a.destination, i.{input$od_c_real_scale} AS origin_area_code ", 
        "FROM (SELECT SUM(nf) AS sum_nf, origin, destination ",
        "FROM {schema_name}.flows_lsoa_n ",
        "WHERE (date BETWEEN '{as.Date(input$od_c_real_date_start_1[[1]])}' AND DATE '{as.Date(input$od_c_real_date_start_1[[1]])}' + INTERVAL '{input$od_c_real_period-1} months') ",
        "GROUP BY origin, destination) a ",
        "LEFT JOIN ",
        "(SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) i ON (a.origin = i.lsoa)) c ",
        "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) j ON (c.destination = j.lsoa)) d ",
        "GROUP BY d.destination_area_code, d.origin_area_code) e ",
        "LEFT JOIN (SELECT area_code, common_name, geometry FROM {schema_name}.all_points_wgs) x ON (e.origin_area_code=x.area_code)) f ",
        "LEFT JOIN (SELECT area_code, common_name, geometry FROM {schema_name}.all_points_wgs) y ON (f.destination_area_code=y.area_code) ",
        "ORDER BY sum_nf DESC;"
      )))
      
      
      DT2_s <- na.omit(DT2_s, cols=c("origin", "destination", "orig_lon", "orig_lat", "dest_lon", "dest_lat"))
      
      incProgress(2 / 2, message = "Outputting data table.")
      
      DT2_s
    })
  })
  
  data_od_c_real <- reactive({ 
    
    # Create progress bar.
    withProgress(message = "Processing Request", value = 0, {
      
      
      incProgress(1 / 2, message = "Importing data.")
      
      #################
      
      #subset points and lookup table columns according to scale 
      if(input$od_c_real_scale=="lsoa"){
        input_scale_string <- input$od_c_real_scale
      }else{
        input_scale_string <- paste0("lsoa, ",input$od_c_real_scale)
      }
      
      
      #put it all into the comparison query
      
      DT2_c <- setDT(dbGetQuery(pool, glue(
        
        "SELECT coord1.perc_change, coord1.n_change, coord1.sum_nf_first AS n1, coord1.sum_nf_second AS n2, coord1.origin AS origin, coord1.destination AS destination, coord1.orig_lat, coord1.orig_lon, coord1.orig_common_name, st_x(y1.geometry) AS dest_lon, st_y(y1.geometry) AS dest_lat, COALESCE(y1.common_name, coord1.destination) AS dest_common_name ",
        "FROM (SELECT ROUND(((j1.sum_nf_second-j1.sum_nf_first)/j1.sum_nf_first)*100.0) AS perc_change, j1.sum_nf_second-j1.sum_nf_first AS n_change, j1.sum_nf_second, j1.sum_nf_first, j1.origin_area_code_first AS origin, j1.destination_area_code_first AS destination, st_x(x1.geometry) AS orig_lon, st_y(x1.geometry) AS orig_lat, COALESCE(x1.common_name, j1.origin_area_code_first) AS orig_common_name ",
        "FROM ((SELECT CAST(SUM(d.sum_nf_first) AS float) AS sum_nf_first, d.origin_area_code_first, d.destination_area_code_first ",
        "FROM (SELECT c.sum_nf_first, c.destination_first, c.origin_area_code_first, j.{input$od_c_real_scale} AS destination_area_code_first ",
        "FROM (SELECT a.sum_nf_first, a.origin_first, a.destination_first, i.{input$od_c_real_scale} AS origin_area_code_first ", 
        "FROM (SELECT SUM(nf) AS sum_nf_first, origin AS origin_first, destination AS destination_first ",
        "FROM {schema_name}.flows_lsoa_n ",
        "WHERE date::date >= DATE '{as.Date(input$od_c_real_date_start_1[[1]])}' AND ",
        " date::date <= DATE '{as.Date(input$od_c_real_date_start_1[[1]])}' + INTERVAL '{input$od_c_real_period-1} months' ",
        "GROUP BY origin, destination) a ",
        "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) i ON (a.origin_first = i.lsoa)) c ",
        "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) j ON (c.destination_first = j.lsoa)) d ",
        "GROUP BY d.destination_area_code_first, d.origin_area_code_first) first1 ", 
        "INNER JOIN ",
        "(SELECT CAST(SUM(d2.sum_nf_second) AS float) AS sum_nf_second, d2.origin_area_code_second, d2.destination_area_code_second ",
        "FROM (SELECT c2.sum_nf_second, c2.destination_second, c2.origin_area_code_second, j2.{input$od_c_real_scale} AS destination_area_code_second ",
        "FROM (SELECT a2.sum_nf_second, a2.origin_second, a2.destination_second, i2.{input$od_c_real_scale} AS origin_area_code_second ", 
        "FROM (SELECT SUM(nf) AS sum_nf_second, origin AS origin_second, destination AS destination_second ",
        "FROM {schema_name}.flows_lsoa_n ",
        "WHERE date::date >= DATE '{as.Date(input$od_c_real_date_start_2[[1]])}' ",
        "AND date::date <= DATE '{as.Date(input$od_c_real_date_start_2[[1]])}' + INTERVAL '{input$od_c_real_period-1} months'  ",
        "GROUP BY origin, destination) a2 ",
        "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) i2 ON (a2.origin_second = i2.lsoa)) c2 ",
        "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) j2 ON (c2.destination_second = j2.lsoa)) d2 ",
        "GROUP BY d2.destination_area_code_second, d2.origin_area_code_second) second1 ON (first1.origin_area_code_first=second1.origin_area_code_second AND first1.destination_area_code_first=second1.destination_area_code_second)) j1 ", 
        "LEFT JOIN {schema_name}.all_points_wgs x1 ON (j1.origin_area_code_first=x1.area_code)) coord1 ",
        "LEFT JOIN {schema_name}.all_points_wgs y1 ON (coord1.destination=y1.area_code) ",
        "ORDER BY ABS(coord1.n_change) DESC;"
      )))
      
      #debug for server
      DT2_c <- na.omit(DT2_c, cols=c("origin", "destination", "orig_lon", "orig_lat", "dest_lon", "dest_lat"))
      
      incProgress(2 / 2, message = "Outputting data table.")
      
      DT2_c
    })
  })
  
  
  #auto map update when the dashboard opens for the first time
  #it displays O-D map for LSOA (default scale)
 
  observeEvent(input$od_map_fluid_row_id, 
               #once = TRUE makes sure that the action is only performed once, when we open the dashboard for the first time,
               # not every time we enter this tab
               once=TRUE, {
                 # Create progress bar.
                 withProgress(message = "Processing Request", value = 0, {
                   
                   # Update progress bar.
                   incProgress(1 / 4, message = "Importing data.")
                   
                   # request static data based on specified parameters
                   data <- data_od_s_real()[,head(.SD, round(input$od_c_real_nrows/100*nrow(data_od_s_real()))),]
                   
                   #validate(nrow(data)>0, "Please select a larger percentage of flows to display, current selection returns less than one.")
                   
                   # Update progress bar.
                   incProgress(1 / 4, message = "Sampling for visualisation.")
                   
                   # Create domain for legend
                   domain <- c(min(data$sum_nf), max(data$sum_nf))
                   
                   # get polygons for display
                   poly_shapes <- st_read(dsn=pool, layer=c(schema_name, glue("{input$od_c_real_scale}_boundaries")))
                   
                   #rename if no common name
                   poly_shapes$common_name[is.na(poly_shapes$common_name)] <- poly_shapes$area_code[is.na(poly_shapes$common_name)]
                   
                   # Update progress bar.
                   incProgress(1 / 4, message = "Building Map.")
                   
                   # Update static map with new lines data.
                   map <- leafletProxy("map_com_real", data = data) %>%
                     addPolygons(data = poly_shapes,
                                 color = "grey",
                                 fillOpacity = 0.,
                                 smoothFactor = 0,
                                 weight = .5,
                                 label = ~htmlEscape(common_name),  
                                 layerId = ~area_code,
                                 highlightOptions = highlightOptions(color = "black", weight = 2)
                     ) %>%
                     addFlows(lng0 = data$orig_lon,
                              lat0 = data$orig_lat,
                              lng1 = data$dest_lon,
                              lat1 = data$dest_lat,
                              flow = data$sum_nf, 
                              color = colorNumeric("viridis", data$sum_nf)(data$sum_nf),
                              dir = 1,
                              maxThickness = 4,
                              opacity = 0.9,
                              popup = popupArgs(labels = "Flow",
                                                supValues = data.frame("Orig" = data$orig_common_name, "Dest" = data$dest_common_name),
                                                supLabels = c("Origin", "Destination"))
                     ) %>%
                     leaflet::addLegend("bottomright",
                                        layerId = "od_map_real_legend",
                                        pal = colorNumeric("viridis", domain),
                                        values = domain,
                                        title = "Journeys",
                                        opacity = 0.8)
                   
                 })
               })
   
  # Update map to reflect new criteria. (Static)
  
  observeEvent(input$od_c_real_s_update , {
    shinyjs::hide("od_map_real_toggle", anim = T, animType = "slide")
    #hide sidebar when updating
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    
    
    if (input$od_c_real_version== FALSE) {
      
      
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {
        
        # Update progress bar.
        incProgress(1 / 4, message = "Importing data.")
        
        # request static data based on specified parameters
        data <- data_od_s_real()[,head(.SD, round(input$od_c_real_nrows/100*nrow(data_od_s_real()))),]
        
        #validate(nrow(data)>0, "Please select a larger percentage of flows to display, current selection returns less than one.")
        
        # Update progress bar.
        incProgress(1 / 4, message = "Sampling for visualisation.")
        
        #check if there are enough rows to visualise
        if(nrow(data)>0){
          
            # Create domain for legend
            domain <- c(min(data$sum_nf), max(data$sum_nf))
          
            # get polygons for display
            poly_shapes <- st_read(dsn=pool, layer=c(schema_name, glue("{input$od_c_real_scale}_boundaries")))
            
            #rename if no common name
            poly_shapes$common_name[is.na(poly_shapes$common_name)] <- poly_shapes$area_code[is.na(poly_shapes$common_name)]
            
            # Update progress bar.
            incProgress(1 / 4, message = "Building Map.")
            
            # Update static map with new lines data.
            map <- leafletProxy("map_com_real", data = data) %>%
              clearShapes() %>%
              clearControls() %>%
              clearMarkers() %>%
              clearFlows() %>%
              addPolygons(data = poly_shapes,
                          color = "grey",
                          fillOpacity = 0.,
                          smoothFactor = 0,
                          weight = .5,
                          label = ~htmlEscape(common_name),  
                          layerId = ~area_code,
                          highlightOptions = highlightOptions(color = "black", weight = 2)

              ) %>%
              addFlows(lng0 = data$orig_lon,
                       lat0 = data$orig_lat,
                       lng1 = data$dest_lon,
                       lat1 = data$dest_lat,
                       flow = data$sum_nf, 
                       color = colorNumeric("viridis", data$sum_nf)(data$sum_nf),
                       dir = 1,
                       maxThickness = 4,
                       opacity = 0.9,
                       popup = popupArgs(labels = "Flow",
                                         supValues = data.frame("Orig" = data$orig_common_name, "Dest" = data$dest_common_name),
                                         supLabels = c("Origin", "Destination"))
              ) %>%
              leaflet::addLegend("bottomright",
                                 layerId = "od_map_real_legend",
                                 pal = colorNumeric("viridis", domain),
                                 values = domain,
                                 title = "Journeys",
                                 opacity = 0.8
              )
          
          
          
        }else if(nrow(data)<1){
          showModal(modalDialog(
            title = "Not enough flows to display",
            "Please input a larger percentage of flows to display so number of flows is over 0.",
            easyClose = TRUE,
            footer = NULL
          ))
        }
        
        
        # Update progress bar.
        incProgress(1 / 4, message = "Done.") 
      })
    }
  })
  
  # Update map to reflect new criteria.
  observeEvent(input$od_c_real_s_update, {
    #hide sidebar when updating
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    
    
    if (input$od_c_real_version== TRUE) {  
      shinyjs::hide("od_map_real_toggle", anim = T, animType = "slide")
      
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {
        
        # Update progress bar.
        incProgress(1 / 4, message = "Importing Map Data")
        
        #get top percentage
        data <- data_od_c_real()[, head(.SD, round(input$od_c_real_nrows/100*nrow(data_od_c_real()))) ,]
        
        # Update progress bar.
        incProgress(1 / 4, message = "Sampling for visualisation.")
        
          # Create the colour breaks
          binpal <- colorBin("RdYlBu", bins = c(-100, -80, -60, -40, -20, 0, 20, 40, 60, 80, Inf), reverse=TRUE)
          
          # get polygons for display
          poly_shapes <- st_read(dsn=pool, layer=c(schema_name, glue("{input$od_c_real_scale}_boundaries")))
          
          #rename if no common name
          poly_shapes$common_name[is.na(poly_shapes$common_name)] <- poly_shapes$area_code[is.na(poly_shapes$common_name)]
          
          # Update progress bar.
          incProgress(1 / 4, message = "Building Map.")
          
          # make the map
          map_com <- leafletProxy("map_com_real", data = data) %>%
            clearShapes() %>%
            clearControls() %>%
            clearMarkers() %>%
            clearFlows() %>%
            addPolygons(data = poly_shapes,
                        color = "grey",
                        fillOpacity = 0.,
                        smoothFactor = 0,
                        weight = .5,
                        label = ~htmlEscape(common_name),  
                        layerId = ~area_code,
                        highlightOptions = highlightOptions(color = "black", weight = 2)
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
                     maxThickness = 4,
                     opacity =.5,
                     popup = popupArgs(labels = "Change in Flow",
                                       supValues = data.frame("PercChange" = data$perc_change, "OrigFlow" = data$n1, "CompFlow" = data$n2, "Orig"=data$orig_common_name, "Dest"=data$dest_common_name),
                                       supLabels = c("Percentage Change", "Original Flow", "Comparison Flow", "Origin", "Destination"))
            ) %>%
            showGroup(input$od_c_real_scale)
          
        
        
      })
    }
  })
  
  
  #click on markers to show flows from and to given spot
  observeEvent(input$map_com_real_shape_click, {
    shinyjs::show("od_map_real_toggle", anim = T, animType = "slide")
    
    #plot flows
    data2a <- data_od_s_real()[origin==input$map_com_real_shape_click[[1]], ,][,`:=`(dir="outbound", colour="#DC3220"),]
    
    data2b <- data_od_s_real()[destination==input$map_com_real_shape_click[[1]], ,][,`:=`(dir = "inbound", colour = "#005AB5"),]
    
    data2 <- rbind(data2a, data2b)
    
    
    if (nrow(data2) > 0) {
      
      
        
        # get polygons for display
        poly_shapes <- st_read(dsn=pool, layer=c(schema_name, glue("{input$od_c_real_scale}_boundaries")))
        
        #rename if no common name
        poly_shapes$common_name[is.na(poly_shapes$common_name)] <- poly_shapes$area_code[is.na(poly_shapes$common_name)]
        
        # Update od outbound map with new lines data.
        map_com <- leafletProxy("map_com_real") %>%
          clearShapes() %>%
          clearMarkers() %>%
          clearControls() %>%
          clearFlows() %>%
          addPolygons(data = poly_shapes,
                      color = "grey",
                      fillOpacity = 0.,
                      smoothFactor = 0,
                      weight = .5,
                      label = ~htmlEscape(common_name),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2)
          ) %>%
          addFlows(lng0 = data2$orig_lon,
                   lat0 = data2$orig_lat,
                   lng1 = data2$dest_lon,
                   lat1 = data2$dest_lat,
                   flow = data2$sum_nf, 
                   color = data2$colour,
                   dir = 1,
                   maxThickness = 5,
                   opacity = 0.7,
                   popup = popupArgs(labels = "Flow",
                                     supValues = data.frame("OrigFlow" = data2$orig_common_name, "DestFlow" = data2$dest_common_name),
                                     supLabels = c("Origin", "Destination"))
          )
      
      
    }else if(nrow(data2) < 1){
      
      showModal(modalDialog(
        title = "Not enough flows to display",
        "Please select a different stop or area which has flows with these filters.",
        easyClose = TRUE,
        footer = NULL
      ))
      
    }
    
    
  })
  
  
  output$od_c_real_tbl <- renderDT(
    
    if (input$od_c_real_version == TRUE) {
      
      #make readable datatable
      data_od_c_real()[, .(origin_code=origin, 
                           destination_code=destination,
                           origin_common_name=orig_common_name,
                           destination_common_name=dest_common_name,
                           T1_period = paste0(format(input$od_c_real_date_start_1, "%Y-%m"), " +", input$od_c_real_period-1, 'm'),
                           T2_period = paste0(format(input$od_c_real_date_start_2, "%Y-%m"), " +", input$od_c_real_period-1, 'm'),
                           T1 = n1, 
                           T2 = n2, 
                           n_change,
                           perc_change)]
      
      
    } else if (input$od_c_real_version == FALSE) {
      
      #make readable datatable
      data_od_s_real()[, .(origin_code=origin, 
                           destination_code=destination,
                           origin_common_name=orig_common_name,
                           destination_common_name=dest_common_name,
                           T1 = sum_nf,
                           period = paste0(format(input$od_c_real_date_start_1, "%Y-%m"), " +", input$od_c_real_period-1, 'm'))]
    }
    ,
    options = list(pageLength = 10, scrollX = T)
  )
  
  
  # Comparative Map, data download handler
  output$od_c_real_downloader <- downloadHandler(
    filename = function() {
      paste0(input$od_c_real_version,
             "_od_",
             "_",
             input$od_c_real_period,
             "_months_",
             format(input$od_c_real_date_start_1, "%Y-%m"),
             "_vs_",
             format(input$od_c_real_date_start_2, "%Y-%m"), 
             ".csv")
    },
    content = function(file) {
      fwrite(
        if (input$od_c_real_version == TRUE) {
          
          #new data.table version
          data_od_c_real()[,.(origin_code=origin, 
                              destination_code=destination,
                              origin_common_name=orig_common_name,
                              destination_common_name=dest_common_name,
                              T1_period = paste0(format(input$od_c_real_date_start_1, "%Y-%m"), " +", input$od_c_real_period-1, 'm'),
                              T2_period = paste0(format(input$od_c_real_date_start_2, "%Y-%m"), " +", input$od_c_real_period-1, 'm'),
                              T1 = n1, 
                              T2 = n2,
                              n_change,
                              perc_change)]
          
        } else if (input$od_c_real_version == FALSE) {
          
          #data table version
          data_od_s_real()[,.(origin_code=origin, 
                              destination_code=destination,
                              origin_common_name=orig_common_name,
                              destination_common_name=dest_common_name,
                              T1 = sum_nf,
                              period = paste0(format(input$od_c_real_date_start_1, "%Y-%m"), " +", input$od_c_real_period-1, 'm'))]
          
        }, file
      )
    }
  )
  
  observeEvent(input$map_com_real_shape_click, {
    leafletProxy("map_com_real") %>% 
      clearControls()
  })
  
  
  
  #plot these flows in a bar graph
  
  # Plot flow bars
  plot_data3_real <- reactive({
    
    data2a <- data_od_s_real()[origin == input$map_com_real_shape_click[[1]], ,
                               ][,`:=`(dir = "outbound", colour = "#DC3220"),
                                 ][order(-sum_nf)
                                   ][,head(.SD,10),
                                     ][order(-sum_nf)]
  })
  
  # Plot flow bars
  plot_data4_real <- reactive({
    
    data2b <- data_od_s_real()[destination == input$map_com_real_shape_click[[1]], ,
                               ][,`:=`(dir = "inbound", colour = "#005AB5"),
                                 ][order(-sum_nf)
                                   ][,head(.SD,10),
                                     ][order(-sum_nf)]
  })
  
  
  output$inbound_bokeh_plot_real <- renderRbokeh({
    
    #change data to factor 
    plot_d4r <- plot_data4_real()
    
    if(nrow(plot_d4r)>0){
      plot_d4r$origin <- factor(plot_d4r$origin, levels = plot_d4r$origin, labels = plot_d4r$orig_common_name)
      
      figure(
        data = plot_d4r,
        title = paste0("To ",plot_d4r$dest_common_name[1],": ", input$map_com_real_shape_click[[1]]),
        legend_location = "right",
        xlab = "",
        ylab = "Inbound Count",
        xlim = plot_d4r$origin
      ) %>%
        ly_bar(
          x = plot_d4r$origin,
          y = plot_d4r$sum_nf,
          fill_color = plot_d4r$colour,
          hover = TRUE
        ) %>%
        theme_axis("x", major_label_orientation = 45)
    }
  })
  
  
  output$outbound_bokeh_plot_real <- renderRbokeh({
    
    #need to turn the data to factors to order by size
    
    plot_d3r <- plot_data3_real()
    
    
    if(nrow(plot_d3r)>0){
      plot_d3r$destination <- factor(plot_d3r$destination, levels = plot_d3r$destination, labels = plot_d3r$dest_common_name)
      
      figure(
        data = plot_d3r,
        title = paste0("From ", plot_d3r$orig_common_name[1],": ",input$map_com_real_shape_click[[1]]),
        legend_location = "right",
        xlab = "",
        ylab = "Outbound Count",
        xlim = plot_d3r$destination #this orders the bars
      ) %>%
        ly_bar(
          x = plot_d3r$destination,
          y = plot_d3r$sum_nf,
          fill_color = plot_d3r$colour,
          hover = TRUE
        ) %>%
        theme_axis("x", major_label_orientation = 45)
      
    }
  })
  
  
  
  
  
  
  ### Help tour for Origin Destination data
  od_steps_real <- reactive(data.frame(
    element = c("imaginary", "#od_box_1_real", "#od_box_2_real", "#od_box_3_real", "#od_box_4_real", "#od_map_id"),
    intro = c(
      "The purpose of this tab is to explore mobility patterns across the West Midlands Combined Authority. You are able to create and query origin-destination flows based on dates, times and passenger characteristics.",
      "First, specify the details of the date and time period you are interested in. The time period refers to the interval for which all flows will be counted, starting from the specified date.",
      "Next, you can filter flows by passenger gender, age, card type and the time of day.",
      "Do you wish to compare flows to another time period? If so, turn on the 'Compare' switch, and specify the start date for the comparison. Note, the period and passenger characteristics will remain the same as for period 1.",
      "Last, specify plotting details. You can select from a range of spatial scales and the set the maximum number of flows to be plotted between origin-destination pairs. Once you have made your selections, hit 'Update'.",
      "Once you have updated your map, click on an area to view details."
    )
  ))
  
  observeEvent(input$od_help_real, {
    introjs(session, options = list(steps = od_steps_real(), "showBullets" = "false", "showProgress" = "true", "showStepNumbers" = "false", "nextLabel" = "Next", "prevLabel" = "Prev"))
  })
  
  ## Attraction / Generation sub-tab -----
  
  output$map_gen_att <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(-1.703929, 52.447027, zoom = 10)
  })
  
  observe({
    if (input$gen_att_zoom_to == 0) {
      leafletProxy("map_gen_att") %>% flyTo(lng = -1.891587, lat = 52.484689, zoom = 10)
    } else if (input$gen_att_zoom_to == 1) {
      leafletProxy("map_gen_att") %>% flyTo(lng = -1.890401, lat = 52.486243, zoom = 13L)
    } else if (input$gen_att_zoom_to == 2) {
      leafletProxy("map_gen_att") %>% flyTo(lng = -2.081112, lat = 52.512255, zoom = 13L)
    } else if (input$gen_att_zoom_to == 3) {
      leafletProxy("map_gen_att") %>% flyTo(lng = -1.519693, lat = 52.406822, zoom = 13L)
    } else if (input$gen_att_zoom_to == 4) {
      leafletProxy("map_gen_att") %>% flyTo(lng = -2.010793, lat = 52.536167, zoom = 13L)
    } else if (input$gen_att_zoom_to == 5) {
      leafletProxy("map_gen_att") %>% flyTo(lng = -1.777610, lat = 52.411811, zoom = 13L)
    } else if (input$gen_att_zoom_to == 6) {
      leafletProxy("map_gen_att") %>% flyTo(lng = -1.982919, lat = 52.586214, zoom = 13L)
    } else if (input$gen_att_zoom_to == 7) {
      leafletProxy("map_gen_att") %>% flyTo(lng = -2.128820, lat = 52.586973, zoom = 13L)
    }
  })
  
  observeEvent(input$gen_att_period, {
    if (input$gen_att_period != "custom") {
      shinyjs::show("gen_att_date_start_1")
      shinyjs::hide("gen_att_date_start_custom")
    } else {
      shinyjs::show("gen_att_date_start_custom")
      shinyjs::hide("gen_att_date_start_1")
    }
  })
  
  observeEvent(input$gen_att_c_version, {
    #allow to choose the second date only if the comaprison button is on (TRUE)
    if (input$gen_att_c_version==FALSE) {
      shinyjs::disable("gen_att_date_start_2")
    } else if (input$gen_att_c_version==TRUE){
      shinyjs::enable("gen_att_date_start_2")
    }
  })
  
  gen_att_s <- reactive({
    
    withProgress(message = "Processing Request", value = 0, {
      
      
      if(input$gen_att_or_version=="gen"){
        gen_or_att <- "origin"
      }else if(input$gen_att_or_version=="att"){
        gen_or_att <- "destination"
      }
      
      #scale string for aggregation
      if(input$att_gen_scale=="lsoa"){
        input_scale_string <- input$att_gen_scale
      }else{
        input_scale_string <- paste0("lsoa, ",input$att_gen_scale)
      }
      
      incProgress(1 / 2, message = "Importing data.")
      
      #turned all the previous stuff into SQL query

      DT2_s <- setDT(dbGetQuery(pool, glue("SELECT f.sum_nf AS sum_nf, f.area_code AS area_code, f.common_name AS common_name_x ",
                                           "FROM (SELECT a.sum_nf AS sum_nf, a.area_code AS area_code, COALESCE(x.common_name, a.area_code) AS common_name ",
                                           "FROM (SELECT SUM(j1.sum_nf) AS sum_nf, b.{input$att_gen_scale} AS area_code ",
                                           "FROM (SELECT SUM(nf) AS sum_nf, {gen_or_att} FROM {schema_name}.flows_lsoa_n ",
                                           "WHERE date::date >= DATE '{as.Date(input$gen_att_date_start_1[[1]])}' ",
                                           "AND date::date <= DATE '{as.Date(input$gen_att_date_start_1[[1]])}' + INTERVAL '{input$gen_att_period-1} months' ",
                                           "GROUP BY {gen_or_att}) j1 ",
                                           "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) b ON (j1.{gen_or_att} = b.lsoa) ",
                                           "GROUP BY b.{input$att_gen_scale}) a ",
                                           "LEFT JOIN (SELECT area_code, common_name FROM {schema_name}.all_points_wgs) x ON (a.area_code = x.area_code) ) f;")))      
      DT2_s <- na.omit(DT2_s)
      
      incProgress(2 / 2, message = "Outputting data table.")
      
      DT2_s
    })
  })
  
  
  gen_att_c <- reactive({ 
    
    # Create progress bar.
    withProgress(message = "Processing Request", value = 0, {
    
      if(input$gen_att_or_version=="gen"){
        gen_or_att <- "origin"
      }else if(input$gen_att_or_version=="att"){
        gen_or_att <- "destination"
      }
      
      #scale string for aggregation
      if(input$att_gen_scale=="lsoa"){
        input_scale_string <- input$att_gen_scale
      }else{
        input_scale_string <- paste0("lsoa, ",input$att_gen_scale)
      }
      
      incProgress(1 / 2, message = "Importing data.")
      
      #################
      #put it all inside one big query
      
      
      DT2_c <- setDT(dbGetQuery(pool, glue("SELECT j4.perc_change AS perc_change, j4.n_change AS n_change, j4.n2 AS n2, j4.n1 AS n1, j4.area_code AS area_code, j4.common_name AS common_name_x ",
                                           "FROM (SELECT ROUND(((j3.sum_nf_second-j3.sum_nf_first)/j3.sum_nf_first)*100.0) AS perc_change, j3.sum_nf_second-j3.sum_nf_first AS n_change, j3.sum_nf_second AS n2, j3.sum_nf_first AS n1, j3.area_code_first AS area_code, COALESCE(x.common_name, j3.area_code_first) AS common_name ",
                                           "FROM ((SELECT CAST(SUM(j1.sum_nf) AS float) AS sum_nf_first, b.{input$att_gen_scale} AS area_code_first ",
                                           "FROM (SELECT SUM(nf) AS sum_nf, {gen_or_att} ",
                                           "FROM {schema_name}.flows_lsoa_n ",
                                           "WHERE date::date >= '{as.Date(input$gen_att_date_start_1[[1]])}' AND ",
                                           "date::date <= DATE '{as.Date(input$gen_att_date_start_1[[1]])}' + INTERVAL '{input$gen_att_period-1} months' ",
                                           "GROUP BY {gen_or_att}) j1 ",
                                           "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) b ON (j1.{gen_or_att} = b.lsoa) ",
                                           "GROUP BY b.{input$att_gen_scale}) first1 ",
                                           "INNER JOIN ",
                                           "(SELECT CAST(SUM(j2.sum_nf) AS float) AS sum_nf_second, c.{input$att_gen_scale} AS area_code_second ",
                                           "FROM (SELECT SUM(nf) AS sum_nf, {gen_or_att} ",
                                           "FROM {schema_name}.flows_lsoa_n ",
                                           "WHERE date::date >= '{as.Date(input$gen_att_date_start_2)}' AND ",
                                           "date::date <= DATE '{as.Date(input$gen_att_date_start_2)}' + INTERVAL '{input$gen_att_period-1} months' ",
                                           "GROUP BY {gen_or_att}) j2 ",
                                           "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) c ON (j2.{gen_or_att} = c.lsoa) ",
                                           "GROUP BY c.{input$att_gen_scale}) second1 ON (first1.area_code_first=second1.area_code_second)) j3 ",
                                           "LEFT JOIN (SELECT area_code, common_name FROM {schema_name}.all_points_wgs) x ON (j3.area_code_first = x.area_code)) j4 ;")))
      
      DT2_c <- na.omit(DT2_c, cols = c("area_code"))
      
      incProgress(2 / 2, message = "Outputting data table.")
      
      DT2_c
    })
  })
  
  
  observeEvent(input$gen_att_c_update, {
    shinyjs::hide("gen_att_toggle", anim = T, animType = "slide")
    shinyjs::hide("gen_att_c_reset")
    #hide sidebar when updating
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    #enable compare switch after updating
    shinyjs::enable("gen_att_c_version")
    
    
    if (input$gen_att_c_version==FALSE) {    
      
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {
        
        # Update progress bar.
        incProgress(1 / 4, message = "Importing data.")
        
        if(input$gen_att_or_version=="gen"){
          pal <- "YlOrRd"
        }else if(input$gen_att_or_version=="att"){
          pal <- "YlGnBu"
        }
        
        # request static data based on specified parameters
        shape <- st_read(dsn=pool, layer=c(schema_name, glue("{input$att_gen_scale}_boundaries")))
        
        #replace NA common_names with code
        shape$common_name[is.na(shape$common_name)] <- shape$area_code[is.na(shape$common_name)]
        
        #add shape to the data
        data_shape <- shape %>% 
          left_join(gen_att_s(), by ="area_code")
        
        #add new column of 1-4 for splitting the data
        data_shape$split_num <- sample(4, size = nrow(data_shape), replace = TRUE)
        data_shape$sum_nf[is.na(data_shape$sum_nf)] <- 0
        
        # Update progress bar.
        incProgress(1 / 4, message = "Sampling for visualisation.")
        
        # Create domain for legend
        domain <- c(min(data_shape$sum_nf), max(data_shape$sum_nf))
        
        # Update progress bar.
        incProgress(1 / 4, message = "Building Map.")
        
        # Update static map with new lines data.
        leafletProxy("map_gen_att") %>%
          clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          clearFlows() %>%
          showGroup("static") %>%
          addPolygons(data = data_shape[data_shape$split_num==1,],
                      fillOpacity = 0.7,
                      smoothFactor = 0,
                      color = ~colorNumeric(pal, domain)(sum_nf),
                      weight = .5,
                      label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2),
                      group = "static"
          ) %>%
          addPolygons(data = data_shape[data_shape$split_num==2,],
                      fillOpacity = 0.7,
                      smoothFactor = 0,
                      color = ~colorNumeric(pal, domain)(sum_nf),
                      weight = .5,
                      label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2),
                      group = "static"
          ) %>%
          addPolygons(data = data_shape[data_shape$split_num==3,],
                      fillOpacity = 0.7,
                      smoothFactor = 0,
                      color = ~colorNumeric(pal, domain)(sum_nf),
                      weight = .5,
                      label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2),
                      group = "static"
          ) %>%
          addPolygons(data = data_shape[data_shape$split_num==4,],
                      fillOpacity = 0.7,
                      smoothFactor = 0,
                      color = ~colorNumeric(pal, domain)(sum_nf),
                      weight = .5,
                      label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2),
                      group = "static"
          ) %>%
          leaflet::addLegend(data=data_shape,
                             "bottomright",
                             layerId = "map_gen_att_legend_static",
                             pal = colorNumeric(pal, domain),
                             values = domain,
                             title = "Trips",
                             opacity = 0.8)
        
        # Update progress bar.
        incProgress(1 / 4, message = "Done.") 
      })
    }else if (input$gen_att_c_version==TRUE){  
      shinyjs::hide("map_gen_att_toggle", anim = T, animType = "slide")
      
      # Create progress bar.
      withProgress(message = "Processing Request", value = 0, {
        
        # Update progress bar.
        incProgress(1 / 4, message = "Importing Map Data")
        
        # request static data based on specified parameters
        shape <- st_read(pool, c(schema_name, glue("{input$att_gen_scale}_boundaries")))
        
        #if common_name is NA replace with area_code
        shape$common_name[is.na(shape$common_name)] <- shape$area_code[is.na(shape$common_name)]
        
        #ERROR if perc change is NA it may mean that one of the times had 0 and the other
        #had a value
        comp_data <- gen_att_c()
        
        #add shape to the data
        data_shape <- shape %>% left_join(comp_data, by = c("area_code"="area_code"))
        
        #add new column of 1-4 for splitting the data
        data_shape$split_num <- sample(4, size = nrow(data_shape), replace = TRUE)
        
        # Update progress bar.
        incProgress(1 / 4, message = "Sampling for visualisation.")
        
        # Create the colour breaks
        binpal <- colorBin("RdYlBu", bins = c(-100, -80, -60, -40, -20, -10, 10, 20, 40, 60, 80, Inf), reverse=T)
        
        # Update progress bar.
        incProgress(1 / 4, message = "Building Map.")
        
        
        # Update static map with new lines data.
        leafletProxy("map_gen_att", data = data_shape) %>%
          clearShapes() %>%
          clearControls() %>%
          clearMarkers() %>%
          clearFlows() %>%
          showGroup("comp") %>%
          addPolygons(data=data_shape[data_shape$split_num==1,],
                      fillOpacity = 0.7,
                      smoothFactor = 0,
                      color = ~binpal(perc_change),
                      weight = .5,
                      label = ~htmlEscape(paste0(common_name, " ", perc_change, "% change.")),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2),
                      group="comp") %>%
          addPolygons(data=data_shape[data_shape$split_num==2,],
                      fillOpacity = 0.7,
                      smoothFactor = 0,
                      color = ~binpal(perc_change),
                      weight = .5,
                      label = ~htmlEscape(paste0(common_name, " ", perc_change, "% change.")),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2),
                      group="comp") %>%
          addPolygons(data=data_shape[data_shape$split_num==3,],
                      fillOpacity = 0.7,
                      smoothFactor = 0,
                      color = ~binpal(perc_change),
                      weight = .5,
                      label = ~htmlEscape(paste0(common_name, " ", perc_change, "% change.")),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2),
                      group="comp") %>%
          addPolygons(data=data_shape[data_shape$split_num==4,],
                      fillOpacity = 0.7,
                      smoothFactor = 0,
                      color = ~binpal(perc_change),
                      weight = .5,
                      label = ~htmlEscape(paste0(common_name, " ", perc_change, "% change.")),  
                      layerId = ~area_code,
                      highlightOptions = highlightOptions(color = "black", weight = 2),
                      group="comp") %>%
          leaflet::addLegend("bottomright",
                             layerId = "map_gen_att_legend_comp",
                             pal = binpal,
                             values = data_shape$perc_change,
                             title = "Change (%)",
                             opacity = 0.8)
        
        
      })
    }
    
    
  })
  
  
  #######
  #put the map com poly click here
  
  gen_att_flow <- reactive({
    
    # Create progress bar.
    withProgress(message = "Processing Request", value = 0, {
      
      
      # Update progress bar.
      incProgress(1 / 4, message = "Importing Map Data")
      
      #scale string for aggregation
      if(input$att_gen_scale=="lsoa"){
        input_scale_string <- input$att_gen_scale
      }else{
        input_scale_string <- paste0("lsoa, ",input$att_gen_scale)
      }
      
      
      #db query
      #get flow data using the shape clicked code
      flows_dt <- setDT(dbGetQuery(pool, 
                                   glue(
                                     "SELECT f.sum_nf, f.origin_area_code AS origin, f.destination_area_code AS destination, f.orig_lon, f.orig_lat, f.orig_common_name, st_x(y.geometry) AS dest_lon, st_y(y.geometry) AS dest_lat, COALESCE(y.common_name, f.destination_area_code) AS dest_common_name ",
                                     "FROM (SELECT e.sum_nf, e.origin_area_code, e.destination_area_code, st_x(x.geometry) AS orig_lon, st_y(x.geometry) AS orig_lat, COALESCE(x.common_name, e.origin_area_code) AS orig_common_name ",
                                     "FROM (SELECT SUM(d.sum_nf) AS sum_nf, d.origin_area_code, d.destination_area_code ",
                                     "FROM (SELECT c.sum_nf, c.destination, c.origin_area_code, i.{input$att_gen_scale} AS destination_area_code ",
                                     "FROM (SELECT a.sum_nf, a.origin, a.destination, j.{input$att_gen_scale} AS origin_area_code ", 
                                     "FROM (SELECT SUM(nf) AS sum_nf, origin, destination ",
                                     "FROM {schema_name}.flows_lsoa_n ",
                                     "WHERE date::date >= '{as.Date(input$gen_att_date_start_1)}' AND ",
                                     "date::date <= DATE '{as.Date(input$gen_att_date_start_1)}' + INTERVAL '{input$gen_att_period-1} months' ",
                                     "GROUP BY origin, destination) a ",
                                     "LEFT JOIN ",
                                     "(SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la GROUP BY {input_scale_string}) j  ON (a.origin = j.lsoa)) c ",
                                     "LEFT JOIN (SELECT {input_scale_string} FROM {schema_name}.stop_oa_lsoa_msoa_la  GROUP BY {input_scale_string}) i ON (c.destination = i.lsoa)) d ",
                                     "GROUP BY d.destination_area_code, d.origin_area_code) e ",
                                     "LEFT JOIN (SELECT area_code, common_name, geometry FROM {schema_name}.all_points_wgs) x ON (e.origin_area_code=x.area_code) ) f ",
                                     "LEFT JOIN (SELECT area_code, common_name, geometry FROM {schema_name}.all_points_wgs) y ON (f.destination_area_code=y.area_code)  ",
                                     "ORDER BY sum_nf DESC;"
                                   )))
      
      # Update progress bar.
      incProgress(1 / 4, message = "Sampling for visualisation.")
      
      #colour and name
      if(input$gen_att_or_version=="gen"){
        
        flows_dt <- flows_dt[origin==input$map_gen_att_shape_click[[1]],,]
        
        
        
      }else if(input$gen_att_or_version=="att"){
        
        flows_dt <- flows_dt[destination==input$map_gen_att_shape_click[[1]],,]
        
        
      }
      
      #get rid of NAs
      flows_dt <- na.omit(flows_dt, cols=c("origin", "destination", "orig_lon", "dest_lon"))
      
      # Update progress bar.
      incProgress(1 / 4, message = "Building Map.")
      
      
      flows_dt
      
    })
    
  })
  
  #show info box about the reset button, only when the map is clicked for the first time
  observeEvent(input$map_gen_att_shape_click[[1]], once=TRUE, { 
    info_steps<-reactive(data.frame(
      element="#gen_att_4_real",
      intro= "If you want to return to the main map and/or choose another area press Reset."
    ))
    
    introjs(session, options = list(steps = info_steps(), "showBullets" = "false", "nextLabel" = "false", "prevLabel" = "false"))
  })
  
  # when a shape is clicked on show the inflow or outflow
  observeEvent(input$map_gen_att_shape_click[[1]], {
    shinyjs::show("gen_att_map_toggle", anim = T, animType = "slide")
    
    #show reset button when clicked
    shinyjs::show("gen_att_c_reset")
    #disable compare switch when clicked -> enable only after reseting
    shinyjs::disable("gen_att_c_version")
    
    #shape
    # request static data based on specified parameters
    shape <- st_read(dsn=pool, layer=c(schema_name, glue("{input$att_gen_scale}_boundaries")))
    
    #replace NA common_names with code
    shape$common_name[is.na(shape$common_name)] <- shape$area_code[is.na(shape$common_name)]
    
    flows_dt<-gen_att_flow()
    
    
    
    #colour and gen or att
    if(input$gen_att_or_version=="gen"){
      gen_or_att <- "origin"
      colour <- "#A80604"
      pal<-"YlGnBu"
      selected.label<-"Trips from"
      
      ####connect flow data with shapefile
      
      data_shape2 <- merge(shape, flows_dt, by.x="area_code", by.y="destination", all.x=TRUE)
      
    }else if(input$gen_att_or_version=="att"){
      gen_or_att <- "destination"
      colour <- "#16A643"
      pal<-"YlOrRd"
      selected.label<- "Trips to"
      ####connect flow data with shapefile
      
      data_shape2 <- merge(shape, flows_dt, by.x="area_code", by.y="origin", all.x=TRUE)
      
    }
    
    ####you can't display over 3,000 polygons in a single item so split into 4
    data_shape2$split_num <- sample(4, size = nrow(data_shape2), replace = TRUE)
    #data_shape2$sum_nf[is.na(data_shape2$sum_nf)] <- 0
    
    selected_area_poly <- data_shape2[data_shape2$area_code==input$map_gen_att_shape_click[[1]], ]
    
    
    #display the polygons
    if(nrow(flows_dt)==0) {
      showModal(modalDialog(
        title = "Not enough flows to display",
        "Please select a different area which has flows with these filters.",
        easyClose = TRUE,
        footer = NULL
      ))
      
    } else if(nrow(flows_dt)>0) {
      # Update od outbound map with new lines data.
      leafletProxy("map_gen_att") %>%
        hideGroup("static") %>%
        hideGroup("comp") %>%
        showGroup("click") %>%
        removeControl("map_gen_att_legend_static")  %>%
        removeControl("map_gen_att_legend_comp") %>%
        
        addPolygons(data = data_shape2[data_shape2$split_num==1,],
                    fillOpacity = 0.7,
                    smoothFactor = 0,
                    color = ~colorNumeric(palette=pal, domain=data_shape2$sum_nf, na.color = "#DEDEDE")(sum_nf),
                    weight = .5,
                    label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                    layerId = ~paste0("clickver_", area_code),
                    highlightOptions = highlightOptions(color = "black", weight = 2),
                    group = "click"
        ) %>%
        addPolygons(data = data_shape2[data_shape2$split_num==2,],
                    fillOpacity = 0.7,
                    smoothFactor = 0,
                    color = ~colorNumeric(palette=pal, domain=data_shape2$sum_nf, na.color = "#DEDEDE")(sum_nf),
                    weight = .5,
                    label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                    layerId = ~paste0("clickver_", area_code),
                    highlightOptions = highlightOptions(color = "black", weight = 2),
                    group = "click"
        ) %>%
        addPolygons(data = data_shape2[data_shape2$split_num==3,],
                    fillOpacity = 0.7,
                    smoothFactor = 0,
                    color = ~colorNumeric(palette=pal, domain=data_shape2$sum_nf, na.color = "#DEDEDE")(sum_nf),
                    weight = .5,
                    label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                    layerId = ~paste0("clickver_", area_code),
                    highlightOptions = highlightOptions(color = "black", weight = 2),
                    group = "click"
        ) %>%
        addPolygons(data = data_shape2[data_shape2$split_num==4,],
                    fillOpacity = 0.7,
                    smoothFactor = 0,
                    color = ~colorNumeric(palette=pal, domain=data_shape2$sum_nf, na.color = "#DEDEDE")(sum_nf),
                    weight = .5,
                    label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                    layerId = ~paste0("clickver_", area_code),
                    highlightOptions = highlightOptions(color = "black", weight = 2),
                    group = "click"
        ) %>%
        addPolygons(data = selected_area_poly,
                    smoothFactor = 0,
                    fillColor = ~colorNumeric(pal, domain=data_shape2$sum_nf, na.color = "#DEDEDE")(sum_nf),
                    fillOpacity = 0.7,
                    stroke=TRUE,
                    color = "black",
                    opacity=1,
                    weight = 2,
                    label = ~htmlEscape(paste0(common_name, " ", sum_nf, " trips.")),  
                    layerId = ~paste0("clickver_", area_code),
                    highlightOptions = highlightOptions(color = "black", weight = 2),
                    group = "click"
        ) %>%
        addLegend(position = "bottomright",
                  layerId = "map_gen_att_legend_click",
                  pal = colorNumeric(palette=pal, domain=data_shape2$sum_nf, na.color = "#DEDEDE") ,
                  values = data_shape2$sum_nf,
                  title = paste0(selected.label," ", selected_area_poly$common_name),
                  opacity = 0.8,
                  na.label= "No Flows",
                  group = "click_legend") 
      
    }
    
  })
  
  #when clicking the reset button - hide the click map layer and its legend
  #show either static or comp map depending on from where we enter the click map
  #workaround with legends; they did not work with group layers as they should
  observeEvent(input$gen_att_c_reset, {
    leafletProxy("map_gen_att") %>%
      hideGroup("click") %>%
      removeControl("map_gen_att_legend_click")
    
    if(input$gen_att_or_version=="gen"){
      pal <- "YlOrRd"
    }else if(input$gen_att_or_version=="att"){
      pal <- "YlGnBu"
    }
    
    if (input$gen_att_c_version == FALSE) {
      gen_att_s <- gen_att_s()
      domain<-c(min(gen_att_s$sum_nf), max(gen_att_s$sum_nf))
      
      leafletProxy("map_gen_att") %>%
        showGroup("static") %>%
        leaflet::addLegend(data=gen_att_s,
                           "bottomright",
                           layerId = "map_gen_att_legend_static",
                           pal = colorNumeric(pal, domain),
                           values = domain,
                           title = "Trips",
                           opacity = 0.8,
                           group = "static")
      
    } else if (input$gen_att_c_version == TRUE) {
      
      #shap data is necessary to display NAs in the legend; thus it is exactly the same as the original legend
      # request static data based on specified parameters
      shape <- st_read(pool, c(schema_name, glue("{input$att_gen_scale}_boundaries")))
      
      #if common_name is NA replace with area_code
      shape$common_name[is.na(shape$common_name)] <- shape$area_code[is.na(shape$common_name)]
      
      #ERROR if perc change is NA it may mean that one of the times had 0 and the other
      #had a value
      comp_data <- gen_att_c()
      
      #add shape to the data
      data_shape <- shape %>% left_join(comp_data, by = c("area_code"="area_code"))
      
      binpal <- colorBin("RdYlBu", bins = c(-100, -80, -60, -40, -20, -10, 10, 20, 40, 60, 80, Inf), reverse=T)
      
      leafletProxy("map_gen_att") %>%
        showGroup("comp") %>%
        leaflet::addLegend("bottomright",
                           layerId = "map_gen_att_legend_comp",
                           pal = binpal,
                           values = data_shape$perc_change,
                           title = "Change (%)",
                           opacity = 0.8)
    }
    
    #hide reset button after it is used 
    shinyjs::hide("gen_att_c_reset")
    #enable compare switch after the reset
    shinyjs::enable("gen_att_c_version")
    #hide details window
    shinyjs::hide("gen_att_map_toggle", anim = T, animType = "slide")
    
  })
  
  ########
  
  #plot bokeh plot
  output$gen_att_bokeh_plot_real <- renderRbokeh({
    
    #colour and gen or att
    if(input$gen_att_or_version=="gen"){
      gen_or_att <- "Origin"
      
      bounded <- "Outbound"
      
      colour <- "#005AB5"
      
      to_from <- "destination"
      
    }else if(input$gen_att_or_version=="att"){
      gen_or_att <- "Destination"
      
      bounded <- "Inbound"
      
      colour <- "#DC3220"
      
      to_from <- "origin"
    }
    
    #get data
    top_flow <- gen_att_flow()[,`:=`(dir = bounded, colour = colour),
                               ][order(-sum_nf)
                                 ][,head(.SD,10),
                                   ][order(-sum_nf)]
    
    if(nrow(top_flow)>0){
      
      #order by size of flows by setting variable to be factor
      
      if(input$gen_att_or_version=="gen"){
        
        #turn it into factor ordereb by the number of flows
        top_flow$destination <- factor(top_flow$destination, levels = top_flow$destination, labels = top_flow$dest_common_name)
        
        #plot the figure
        figure(
          data = top_flow,
          title = paste0(top_flow$orig_common_name[1],": ",  input$map_gen_att_shape_click[[1]]), 
          legend_location = "right",
          xlab = "",
          ylab = paste0(bounded," Count"),
          xlim = top_flow$destination
        ) %>%
          ly_bar(
            x = top_flow$destination,
            y = top_flow$sum_nf,
            fill_color = top_flow$colour,
            hover = TRUE
          ) %>%
          theme_axis("x", major_label_orientation = 45)
        
        
      }else if(input$gen_att_or_version=="att"){
        
        top_flow$origin <- factor(top_flow$origin, levels = top_flow$origin, labels = top_flow$orig_common_name)
        
        #plot the figure
        figure(
          data = top_flow,
          title = paste0(top_flow$dest_common_name[1],": ",input$map_gen_att_shape_click[[1]]),
          legend_location = "right",
          xlab = "",
          ylab = paste0(bounded," Count"),
          xlim = top_flow$origin
        ) %>%
          ly_bar(
            x = top_flow$origin,
            y = top_flow$sum_nf,
            fill_color = top_flow$colour,
            hover = TRUE
          ) %>%
          theme_axis("x", major_label_orientation = 45)
      }
    }else if(nrow(top_flow)<1){
      
      showModal(modalDialog(
        title = "Not enough flows to display",
        "Please select a different area which has flows with these filters.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  
  #gen att data table rendering
  
  output$gen_att_c_real_tbl <- renderDT(
    
    if (input$gen_att_c_version == TRUE) {
      
      
      #new data.table version
      gen_att_c()[,.(area_code, 
                     common_name = common_name_x,
                     T1_period = paste0(as.Date(input$gen_att_date_start_1), " +", input$gen_att_period-1, 'm'),
                     T2_period = paste0(as.Date(input$gen_att_date_start_2), " +", input$gen_att_period-1, 'm'),
                     T1 = n1, 
                     T2 = n2,
                     n_change,
                     perc_change)]
      
      
    } else if (input$gen_att_c_version == FALSE) {
      
      gen_att_s()[,.(area_code, common_name = common_name_x, T1 = sum_nf, period = paste(as.Date(input$gen_att_date_start_1), " +", input$gen_att_period-1, 'm'))]
      
    }
    ,
    options = list(pageLength = 10, scrollX = T)
  )
  
  
  # Comparative Map, data download handler
  output$gen_att_downloader <- downloadHandler(
    filename = function() {
      glue("oa_trip_{input$gen_att_or_version}_{input$gen_att_c_version}.csv")
    },
    content = function(file) {
      fwrite(
        if (input$gen_att_c_version == TRUE) {
          
          
          #new data.table version
          gen_att_c()[,.(area_code, 
                         common_name = common_name_x,
                         T1_period = paste0(as.Date(input$gen_att_date_start_1), " +", input$gen_att_period-1, 'm'),
                         T2_period = paste0(as.Date(input$gen_att_date_start_2), " +", input$gen_att_period-1, 'm'),
                         T1 = n1, 
                         T2 = n2,
                         n_change,
                         perc_change)]
          
        } else if (input$gen_att_c_version == FALSE) {
          
          gen_att_s()[,.(area_code, common_name = common_name_x, 
                         T1 = sum_nf,
                         period = paste(as.Date(input$gen_att_date_start_1), " +", input$gen_att_period-1, 'm'))]
        }, file
      )
    }
  )
  
  
  ### Help tour for Origin Destination data
  gen_att_steps_real <- reactive(data.frame(
    element = c("imaginary", "#gen_att_box_1_real", "#gen_att_2_real", "#gen_att_3_real", "#gen_att_4_real", "gen_att_map_toggle"),
    intro = c(
      "The purpose of this tab is to explore mobility patterns across the West Midlands Combined Authority. You are able to create and query where trips are generated from and where they are attracted to based on dates, times and passenger characteristics.",
      "First, specify the details of the date and time period you are interested in. The time period refers to the interval for which all trips will be counted, starting from the specified date.",
      "Next, you can filter flows by passenger gender, age, card type and the time of day.",
      "Do you wish to compare trips to another time period? If so, turn on the 'Compare' switch, and specify the start date for the comparison. Note, the period and passenger characteristics will remain the same as for period 1.",
      "Last, specify plotting details. Decide whether you want to see 'Generation' or 'Attraction' map and select from a range of spatial scales. From OA upwards a chloropleth map will be created using the shapes of the areas. Once you have made your selections, hit 'Update'.",
      "Once you have updated your map, click on an area to view details."
    )
  ))
  
  observeEvent(input$gen_att_help_real, {
    introjs(session, options = list(steps = gen_att_steps_real(), "showBullets" = "false", "showProgress" = "true", "showStepNumbers" = "false", "nextLabel" = "Next", "prevLabel" = "Prev"))
  })
  
  #######################################################################################################
  # Who Travels ---------------------------------------------------------------------------------------
  
  
  dygraph_data_1 <- reactive({
    data <- setDT(dbGetQuery(pool, glue("SELECT measure, gender, SUM(value) AS value, CAST(DATE_TRUNC('{input$dygraph_time_resolution}', date) AS DATE) AS date ",
                                        "FROM {schema_name}.dygraph_data ",
                                        "WHERE measure='{input$dygraph_statistic}' AND type='{input$dygraph_type}' ",
                                        "GROUP BY measure, date, gender;")))
    
    data <- dcast(data, date ~ gender, value.var = c("value"), fun.aggregate = sum)
    
  })
  
  
  
  dygraph_data_2 <- reactive({
    data <- setDT(dbGetQuery(pool, glue("SELECT measure, age, SUM(value) AS value, CAST(DATE_TRUNC('{input$dygraph_time_resolution}', date) AS DATE) AS date ",
                                        " FROM {schema_name}.dygraph_data ",
                                        "WHERE measure='{input$dygraph_statistic}' AND type='{input$dygraph_type}' ",
                                        "GROUP BY measure, date, age;")))
    
    data <- dcast(data, date ~ age, value.var = c("value"), fun.aggregate = sum)
  })
  
  
  
  # Unzoom plugin
  dyUnzoom <- function(dygraph) {
    dyPlugin(
      dygraph = dygraph,
      name = "Unzoom",
      path = system.file("plugins/unzoom.js", package = "dygraphs")
    )
  }
  
  # Data Table 1
  
  output$dt1<-renderDataTable({if(input$dygraph_table_show_1 == TRUE){data <- dygraph_data_1()}else {NULL}})
  
  # data download handler 1
  
  output$gender_downloader <- downloadHandler(
    filename = function(file) {
      paste0(input$dygraph_type , "_", input$dygraph_time_resolution, "_gender_concession.csv")
    },
    content = function(file) {fwrite(dygraph_data_1(), 
                                     file)
      
    }
  )
  
  # Data Table 2
  
  output$dt2<-renderDataTable({if(input$dygraph_table_show_1 == TRUE){data <- dygraph_data_2()}else {NULL}})
  
  # data download handler 2
  
  output$age_downloader<- downloadHandler(
    filename = function(file) {
      paste0(input$dygraph_type, "_", input$dygraph_time_resolution, "_age_concession.csv")
    },
    
    content = function(file) {fwrite(dygraph_data_2(), file)}
  )
  
  # First interactive graph based on gender
  output$dygraph_gender <- renderDygraph({
    
    data <- dygraph_data_1()
    
    if (input$dygraph_statistic == "journeys_pp") {
      
      stack_case <- FALSE
      dygraph_statistic <- "Journeys per Person"
    } else if (input$dygraph_statistic == "passengers") {
      
      stack_case <- FALSE
      dygraph_statistic <- "Passengers"
    } else {
      
      stack_case <- FALSE
      dygraph_statistic <- "Journeys"
    }
    
    # Create dygraph based on date and gender
    dygraph(
      data = data,
      main = paste0(
        str_to_title(input$dygraph_type),
        " ", dygraph_statistic,
        " by Gender (", str_to_title(input$dygraph_time_resolution), ")"
      ), group = "dygraphs"
    ) %>% 
       dySeries("F", label = "Female") %>% dySeries("M", label = "Male") %>% dySeries("U", label = "Unknown"
      ) %>%
      dyLegend(labelsSeparateLines = T)%>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = T
      ) %>%
      dyOptions(stackedGraph = stack_case, 
                colors = wes_palette("Darjeeling1")[c(1, 2, 3, 5)], 
                titleHeight = 25,
                maxNumberWidth = 20) %>% 
      dyRangeSelector(height = 30) %>%
      dyUnzoom() #
  })
  
  # Second interactive graph based on age
  output$dygraph_age <- renderDygraph({
    
    data <- dygraph_data_2()
    
    if (input$dygraph_statistic == "journeys_pp") {
      
      stack_case <- FALSE
      dygraph_statistic <- "Journeys per Person"
    } else if (input$dygraph_statistic == "passengers") {
      
      stack_case <- FALSE
      dygraph_statistic <- "Passengers"
    } else {
      
      stack_case <- FALSE
      dygraph_statistic <- "Journeys"
    }
    
    # Create dygraph based on date and age                
    dygraph(
      data = data,
      main = paste0(
        str_to_title(input$dygraph_type),
        " ", dygraph_statistic,
        " by Age (", str_to_title(input$dygraph_time_resolution), ")"
      ), group = "dygraphs"
      
    ) %>%
      dyLegend(labelsSeparateLines = T)%>%
      dyHighlight(
        highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.2,
        hideOnMouseOut = T
      ) %>%
      dyOptions(stackedGraph = stack_case, 
                colors = c("#63adbe", "#9ebe91", "#ebcc29", "#e67d01", "#f21a00"),
                titleHeight = 25,
                maxNumberWidth = 20) %>%
      dyRangeSelector(height = 30) %>%
      dyUnzoom() 
  })
  
  
  ######################################################################################################## 
  # Accessibility ---------------------------------------------------------------------------------------
  
  ##exploration tab ------
  
  observeEvent(input$ac_c_version, {
    #allow to choose the second date only if the comaprison button is on (TRUE)
    if(input$ac_c_version == FALSE){
      shinyjs::disable("ac_date_comp")
    }else if (input$ac_c_version == TRUE){
      shinyjs::enable("ac_date_comp")
      
    }
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
    
    ac_dt <- setDT(dbGetQuery(pool, glue("SELECT mean_min_time, origin AS area_code, type, date ", 
                                         "FROM {schema_name}.ac_dt WHERE DATE_TRUNC('month',date::timestamp) = DATE_TRUNC('quarter','{input$ac_date}'::timestamp) AND type='{input$ac_type}';")))
    
    ac_dt
  })
  
  # Create Reactive table for comparative accessibility
  s_data_comp <- reactive({
    
    ac_dt <- setDT(dbGetQuery(pool, glue("SELECT mean_min_time_start, start_origin AS area_code, start_date, mean_min_time_end, comp_date, mean_min_time_start-mean_min_time_end AS change_min_time ",
                                         " FROM  (",
                                         "SELECT mean_min_time AS mean_min_time_start, origin AS start_origin, date AS start_date ",
                                         " FROM {schema_name}.ac_dt ",
                                         " WHERE DATE_TRUNC('month',date::timestamp)= DATE_TRUNC('quarter','{as.Date(input$ac_date)}'::timestamp) AND type='{input$ac_type}' ",
                                         ") ac_1_tab ",
                                         " LEFT JOIN ",
                                         "(SELECT mean_min_time AS mean_min_time_end, origin AS comp_origin, date AS comp_date ",
                                         " FROM {schema_name}.ac_dt ",
                                         " WHERE DATE_TRUNC('month',date::timestamp)= DATE_TRUNC('quarter','{as.Date(input$ac_date_comp)}'::timestamp) AND type='{input$ac_type}' ",
                                         ") ac_2_tab ",
                                         "ON ac_1_tab.start_origin=ac_2_tab.comp_origin;")))
    
    ac_dt <- na.omit(ac_dt)
    
  })
  
  output$ac_tbl <- renderDT(
    if (input$ac_c_version == TRUE){
      #data.table version
      s_data_comp()[,
                    .(area_code, 
                      T1_date = start_date, 
                      T2_date = comp_date, 
                      T1_time = mean_min_time_start, 
                      T2_time = mean_min_time_end, 
                      Change = change_min_time)]
      
      
    } else if (input$ac_c_version == FALSE) {
      
      #data.table version
      s_data()[,
               .(area_code, 
                 T1_date = date, 
                 T1_time = mean_min_time)]
      
    }
    ,
    options = list(pageLength = 10, scrollX = T)
  )
  
  # Comparative Map, data download handler
  output$access_downloader <- downloadHandler(
    filename = function() {
      paste0(input$ac_c_version,"_", input$ac_type, "_accessibility.csv")
    },
    content = function(file) {
      fwrite(
        if (input$ac_c_version == TRUE) { 
          #data.table version
          s_data_comp()[,
                        .(area_code, 
                          T1_date = start_date, 
                          T2_date = comp_date, 
                          T1_time = mean_min_time_start, 
                          T2_time = mean_min_time_end, 
                          Change = change_min_time)]
          
          
        } else if (input$ac_c_version == FALSE) {
          #data.table version
          s_data()[,
                   .(area_code, 
                     T1_date = date, 
                     T1_time = mean_min_time)]
          
        }, file
      )
    }
  )
  
  # Accessibility Map update
  observeEvent(input$ac_c_s_update, {
    #hide sidebar when updating
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    
    withProgress(message = "Processing Request", value = 0, {
      
      incProgress(1/4, message = "Importing Data.")
      
      stop_cp <-st_read(dsn=pool, query = glue("SELECT geometry FROM  {schema_name}.all_points_wgs WHERE scale= 'stop';"))
      
      oa_cp <- st_read(dsn=pool, layer=c(schema_name, "oa_boundaries"))
      
      if(input$ac_type=="Clinic"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "clinics"))
        mark_col <- "#e41a1c"
        markers$poi_name <- markers$Organisa_4
      }else if(input$ac_type=="GP"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "gps"))
        mark_col <- "#377eb8"
        markers$poi_name <- markers$Organisa_4
      }else if(input$ac_type=="Hospital"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "hospitals"))
        mark_col <- "#4daf4a"
        markers$poi_name <- markers$Organisa_4
      }else if(input$ac_type=="Retail Centre"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "retail_centres"))
        mark_col <- "#EE82EE"
        markers$poi_name <- markers$Name
      }else if(input$ac_type=="Train Station"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "rail_stations"))
        mark_col <- "#f781bf"
        markers$poi_name <- markers$Name
      }else if(input$ac_type=="Supermarket (Small)"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "sm_small"))
        mark_col <- "#984ea3"
        markers$poi_name <- markers$retailer
      }else if(input$ac_type=="Supermarket (Medium)"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "sm_medium"))
        mark_col <- "#ff7f00"
        markers$poi_name <- markers$retailer
      }else if(input$ac_type=="Supermarket (Large)"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "sm_large"))
        mark_col <- "#ffff33"
      }else if(input$ac_type=="Supermarket (Very Large)"){
        markers <- st_read(dsn=pool, layer=c(schema_name, "sm_very_large"))
        mark_col <- "#a65628"
        markers$poi_name <- markers$retailer
      }
      
      
      
      if (input$ac_c_version == FALSE) {
        palette <- "YlOrRd"
        reverse <- F
        oa_cp <- oa_cp %>% 
          left_join(s_data(), by = c("area_code"))
        
        domain <- c(1, max(abs(oa_cp$mean_min_time),na.rm=TRUE))
        
        legend_title <- "Travel Time (mins)"
        
      } else if (input$ac_c_version == TRUE) {
        palette <- as.character(wes_palette("Zissou1"))
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
      
      #creating progress bar 
      
      incProgress(1/4, message = "Building Map")
      
      leafletProxy("ac_map", deferUntilFlush = F) %>%
        clearControls() %>%
        clearShapes() %>%
        clearFlows() %>%
        clearMarkers() %>%
        addPolygons(
          data = oa_cp[oa_cp$split_num==1, ],
          fillOpacity = 0.7,
          smoothFactor = 0,
          color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
          weight = .5,
          label = ~htmlEscape(paste0(area_code, " : ", mean_min_time, " mins.")),  
          layerId = ~area_code,
          highlightOptions = highlightOptions(color = "black", weight = 2)
        ) %>%
        addPolygons(
          data = oa_cp[oa_cp$split_num==2, ],
          fillOpacity = 0.7,
          smoothFactor = 0,
          color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
          weight = .5,
          label = ~htmlEscape(paste0(area_code, " : ", mean_min_time, " mins.")),
          layerId = ~area_code,
          highlightOptions = highlightOptions(color = "black", weight = 2)
        ) %>%
        addPolygons(
          data = oa_cp[oa_cp$split_num==3, ],
          fillOpacity = 0.7,
          smoothFactor = 0,
          color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
          weight = .5,
          label = ~htmlEscape(paste0(area_code, " : ", mean_min_time, " mins.")),
          layerId = ~area_code,
          highlightOptions = highlightOptions(color = "black", weight = 2)
        ) %>%
        addPolygons(
          data = oa_cp[oa_cp$split_num==4, ],
          fillOpacity = 0.7,
          smoothFactor = 0,
          color = ~colorNumeric(palette, domain, reverse = reverse)(mean_min_time),
          weight = .5,
          label = ~htmlEscape(paste0(area_code, " : ", mean_min_time, " mins.")),
          layerId = ~area_code,
          highlightOptions = highlightOptions(color = "black", weight = 2)
        ) %>%
        addCircleMarkers(data = markers, 
                         stroke = T, 
                         color = "black", 
                         weight = 1, 
                         fillColor = mark_col, 
                         radius = 5, 
                         fillOpacity = .8,
                         label = ~poi_name
        ) %>%
        addCircleMarkers(data= stop_cp, 
                         stroke =T, 
                         color = "grey",
                         weight = .8, 
                         fillColor = "#ADD8E6",
                         radius = 1,
                         #group ="Bus stops" allows the markers to be controlled from addLayersControl underneath 
                         group="Bus stops"
                         
        ) %>%           
        #hide bus stops as default
        hideGroup("Bus stops"
                  
        ) %>%
        #allow to show bus stops by clicking the button on the map
        addLayersControl(overlayGroups = "Bus stops", 
                         options = layersControlOptions(collapsed = FALSE),
                         position = "topleft"
        ) %>%
        
        leaflet::addLegend(
          position = "bottomright",
          title = legend_title,
          pal = pal,
          values = domain
        )
      
      incProgress(1/4, message = "Done")
      
      
    })
  })
  
  observeEvent(input$ac_map_shape_click[[1]], {
    shinyjs::show("ac_map_toggle", anim = T, animType = "slide")
  })
  
  # Reactive Element for accessibility Graph.
  plot_data <- reactive({
    
    plot_data <- setDT(dbGetQuery(pool, glue("SELECT date, origin, mean_min_time, type ", 
                                             "FROM {schema_name}.ac_dt WHERE origin='{input$ac_map_shape_click[[1]]}';")))
    
    
  })
  
  #request plot data
  req(plot_data) 
  
  #plot the line graph
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
      "What date are you interested in?",
      "Do you want to measure changes in accessibility between two time points? If so, turn on the 'Compare' switch, and specify the second date.",
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
  
  ##analysis tab -----------
  
  output$ac_map_analysis <- renderLeaflet({
    
    
    leaflet() %>%
      setView(lng = -1.891587, lat = 52.484689, zoom = 10) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
  })
  
  ########################################################################################################
  # Eligibility ---------------------------------------------------------------------------------------
  
  # Based on selected inputs, show the correct time slider.
  #allow to choose the second date only if the comaprison button is on (TRUE)
  observeEvent(c(input$elig_source, input$elig_version), {
    if (input$elig_source == "estimate") {
      if (input$elig_version == FALSE) {
        shinyjs::show("elig_year")
        shinyjs::show("elig_year_comp")
        shinyjs::disable("elig_year_comp")
        shinyjs::hide("elig_forecast_year")
        shinyjs::hide("elig_year_comp_forecast")
      } else if (input$elig_version == TRUE) {
        shinyjs::show("elig_year_comp")
        shinyjs::enable("elig_year_comp")
        shinyjs::show("elig_year")
        shinyjs::hide("elig_forecast_year")
        shinyjs::hide("elig_year_comp_forecast")
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == FALSE) {
        shinyjs::show("elig_forecast_year")
        shinyjs::hide("elig_year_comp")
        shinyjs::hide("elig_year")
        shinyjs::show("elig_year_comp_forecast")
        shinyjs::disable("elig_year_comp_forecast")
      } else if (input$elig_version == TRUE) {
        shinyjs::show("elig_year_comp_forecast")
        shinyjs::enable("elig_year_comp_forecast")
        shinyjs::hide("elig_year_comp")
        shinyjs::hide("elig_year")
        shinyjs::show("elig_forecast_year")
      }
    }
  })
  
  
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
    
    elig_data <- setDT(dbGetQuery(pool, glue("SELECT j.area_code AS area_code, j.year AS year, j.count AS count, COALESCE(x.common_name, j.area_code) AS common_name_x ",
                                             "FROM (SELECT * FROM {schema_name}.d4_el0 ",
                                             "WHERE year = '{format(as.Date(input$elig_year),'%Y')}' ) j ",
                                             "LEFT JOIN (SELECT area_code, common_name FROM {schema_name}.all_points_wgs) x ON (x.area_code = j.area_code); ")))
  })
  
  # Reactive data for Static Forecast eligibility map.
  la_data_forecast <- reactive({
    elig_forecast <- setDT(dbGetQuery(pool, glue("SELECT j.area_code AS area_code, j.year AS year, j.count AS count, COALESCE(x.common_name, j.area_code) AS common_name_x ",
                                                 "FROM (SELECT * FROM {schema_name}.d4_el1 ",
                                                 "WHERE year='{format(as.Date(input$elig_forecast_year),'%Y')}' ) j ",
                                                 "LEFT JOIN (SELECT area_code, common_name FROM {schema_name}.all_points_wgs) x ON (x.area_code = j.area_code); ")))
  })
  
  
  # Reactive data for Comparative eligibility Map.
  lsoa_data_comp <- reactive({
    elig_data <- setDT(dbGetQuery(pool, glue("SELECT j2.area_code AS area_code, j2.count_x AS count_x, j2.count_y AS count_y, j2.count AS count, j2.year_x AS year_x, j2.year_y AS year_y, j2.common_name_x AS common_name_x ",
                                             "FROM (SELECT j1.area_code_x AS area_code, j1.count_x AS count_x, j1.count_y AS count_y, ROUND(CAST((100*(j1.count_y-j1.count_x)/j1.count_x) AS numeric), 2) AS count, j1.year_x AS year_x, j1.year_y AS year_y, COALESCE(x.common_name, j1.area_code_x) AS common_name_x ",
                                             "FROM ((SELECT year AS year_x, area_code AS area_code_x, count AS count_x ",
                                             "FROM {schema_name}.d4_el0 WHERE year='{format(as.Date(input$elig_year),'%Y')}') y1 ", 
                                             "INNER JOIN (SELECT year AS year_y, area_code AS area_code_y, count AS count_y ", 
                                             "FROM {schema_name}.d4_el0 WHERE year={format(as.Date(input$elig_year_comp),'%Y')}) y2 ON (y1.area_code_x=y2.area_code_y)) j1 ",
                                             "LEFT JOIN (SELECT area_code, common_name FROM {schema_name}.all_points_wgs) x ON (j1.area_code_x = x.area_code)) j2 ;")))
    
    
  })
  
  # Reactive data for Comparative forecast eligibility Map.
  la_data_forecast_comp <- reactive({
    elig_forecast <- setDT(dbGetQuery(pool, glue("SELECT j2.area_code AS area_code, j2.count_x AS count_x, j2.count_y AS count_y, j2.count AS count, j2.year_x AS year_x, j2.year_y AS year_y, j2.common_name_x AS common_name_x ",
                                                 "FROM (SELECT j1.area_code_x AS area_code, j1.count_x AS count_x, j1.count_y AS count_y, ROUND(CAST((100*(j1.count_y-j1.count_x)/j1.count_x) AS numeric), 2) AS count, j1.year_x AS year_x, j1.year_y AS year_y, COALESCE(x.common_name, j1.area_code_x) AS common_name_x ", 
                                                 "FROM ((SELECT year AS year_x, area_code AS area_code_x, count AS count_x ", 
                                                 "FROM {schema_name}.d4_el1 WHERE year='{format(as.Date(input$elig_forecast_year),'%Y')}') y1 ", 
                                                 "INNER JOIN (SELECT year AS year_y, area_code AS area_code_y, count AS count_y ", 
                                                 "FROM {schema_name}.d4_el1 WHERE year='{format(as.Date(input$elig_year_comp_forecast),'%Y')}') y2 ON (y1.area_code_x=y2.area_code_y)) j1 ",
                                                 "LEFT JOIN (SELECT area_code, common_name FROM {schema_name}.all_points_wgs) x ON (j1.area_code_x = x.area_code)) j2  ;")))
    
  })
  
  output$elig_s_tbl <- renderDT(
    
    if (input$elig_source == "estimate") {
      if (input$elig_version == TRUE) {
        
        
        output_dt <- lsoa_data_comp()
        
        output_dt <- output_dt[,.(area_code, 
                                  common_name = common_name_x,
                                  t1_year = year_x, 
                                  t2_year = year_y,
                                  t1_pop = count_x, 
                                  t2_pop = count_y, 
                                  perc_change = count)]
        
        output_dt
        
      } else if (input$elig_version == FALSE) {
        
        lsoa_data()[,.(area_code, 
                       common_name = common_name_x, 
                       t1_year = year, 
                       t1_pop = count)]
        
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == FALSE) {
        la_data_forecast()[,.(area_code, 
                              common_name = common_name_x, 
                              t1_year = year, 
                              t1_pop = count)]
        
      } else if (input$elig_version == TRUE) {
        
        la_data_forecast_comp()[,.(area_code,
                                   common_name = common_name_x,
                                   t1_year = year_x,
                                   t2_year = year_y, 
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
          if (input$elig_version == TRUE) {
            
            lsoa_data_comp()[,.(area_code, 
                                common_name = common_name_x,
                                t1_year = year_x, 
                                t2_year = year_y,
                                t1_pop = count_x, 
                                t2_pop = count_y, 
                                perc_change = count)]
            
            
          } else if (input$elig_version == FALSE) {
            
            lsoa_data()[,.(area_code, 
                           common_name = common_name_x, 
                           t1_year = year, 
                           t1_pop = count)]
            
          }
        } else if (input$elig_source == "forecast") {
          if (input$elig_version == FALSE) {
            
            la_data_forecast()[,.(area_code,
                                  common_name = common_name_x, 
                                  t1_year = year, 
                                  t1_pop = count)]
            
          } else if (input$elig_version == TRUE) {
            
            la_data_forecast_comp()[,.(area_code,
                                       common_name = common_name_x,
                                       t1_year = year_x,
                                       t2_year = year_y, 
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
    #hide sidebar when updating
    shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    
    if (input$elig_source == "estimate") {
      
      #get shapefile from the database
      temp_shp <- st_read(dsn=pool, layer=c(schema_name, "lsoa_boundaries"))
      
      #replace unknown common names with area_code
      temp_shp$common_name[is.na(temp_shp$common_name)] <- temp_shp$area_code[is.na(temp_shp$common_name)]
      
      if (input$elig_version == FALSE) {
        palette <- "Blues"
        temp_shp <-  temp_shp %>% 
          inner_join(lsoa_data(), by=c("area_code"))
        domain <- c(1, max(abs(temp_shp$count)))
        suffix <- ""
      } else if (input$elig_version == TRUE){
        
        temp_shp <- temp_shp %>%
          inner_join(lsoa_data_comp(), by = c("area_code"))
        palette <- as.character(wes_palette("Zissou1"))
        domain <- c(-1 * max(abs(temp_shp$count)), 0, max(abs(temp_shp$count)))
        suffix <- " %"
      }
    } else if (input$elig_source == "forecast") {
      
      #get shapefile from the database
      temp_shp <- st_read(dsn=pool, layer=c(schema_name, "la_boundaries"))
      
      #replace unknown common names with area_code
      temp_shp$common_name[is.na(temp_shp$common_name)] <- temp_shp$area_code[is.na(temp_shp$common_name)]
      
      if (input$elig_version == FALSE) {
        palette <- "Blues"
        
        temp_shp <- temp_shp %>% 
          left_join(la_data_forecast(), by = c("area_code"))
        domain <- c(1, max(abs(temp_shp$count)))
        suffix <- ""
      } else if (input$elig_version == TRUE) {
        
        temp_shp <- temp_shp %>% 
          left_join(la_data_forecast_comp(), by = c("area_code"))
        
        palette <- as.character(wes_palette("Zissou1"))
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
        label = ~htmlEscape(paste0(temp_shp$common_name, " : ", count, suffix)),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = T),
        layerId = ~area_code,
        color = ~colorNumeric(palette, domain = domain)(count), weight = .5
      ) %>%
      addPolygons(
        data = temp_shp[(floor(nrow(temp_shp) / 2) + 1):nrow(temp_shp), ],
        smoothFactor = 0,
        fillOpacity = 0.7,
        layerId = ~area_code,
        label = ~htmlEscape(paste0(temp_shp$common_name, " : ", count, suffix)),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = T), 
        color = ~colorNumeric(palette, domain = domain)(count), weight = .5
      )
  })
  
  # Update Eligibility Legend.
  observeEvent(input$elig_update, {
    proxy <- leafletProxy("map_elig")
    
    if (input$elig_source == "estimate") {
      if (input$elig_version == FALSE) {
        title <- "People"
        palette <- "Blues"
        data <- lsoa_data()
        domain <- c(1, max(abs(data$count)))
      } else if (input$elig_version == TRUE) {
        title <- "% Change"
        data <- lsoa_data_comp()
        palette <- as.character(wes_palette("Zissou1"))
        domain <- c(-1 * max(abs(data$count)), 0, max(abs(data$count)))
      }
    } else if (input$elig_source == "forecast") {
      if (input$elig_version == FALSE) {
        title <- "People"
        palette <- "Blues"
        data <- la_data_forecast()
        domain <- c(1, max(abs(data$count)))
      } else if (input$elig_version == TRUE) {
        title <- "Change"
        data <- la_data_forecast_comp()
        palette <- as.character(wes_palette("Zissou1"))
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
      
      elig_data <- setDT(dbGetQuery(pool, glue("SELECT t1.year, t1.count, t1.area_code, t2.common_name ",
                                               "FROM (SELECT year, count, area_code FROM {schema_name}.d4_el0 ",
                                               "WHERE area_code='{input$map_elig_shape_click[[1]]}') t1 ",
                                               "LEFT JOIN (SELECT area_code, COALESCE(common_name, area_code) AS common_name ",
                                               "FROM {schema_name}.lsoa_boundaries) t2 ON (t1.area_code=t2.area_code);")))
      
      elig_data
      
    } else if (input$elig_source == "forecast") {
      
      elig_forecast <- setDT(dbGetQuery(pool, glue("SELECT t1.year, t1.count, t1.area_code, t2.common_name ",
                                                   "FROM (SELECT year, count, area_code FROM {schema_name}.d4_el1 ",
                                                   "WHERE area_code='{input$map_elig_shape_click[[1]]}') t1 ",
                                                   "LEFT JOIN (SELECT area_code, COALESCE(common_name, area_code) AS common_name ",
                                                   "FROM {schema_name}.la_boundaries) t2 ON (t1.area_code=t2.area_code);")))
    }
  })
  
  output$c_bokeh_plot <- renderRbokeh({
    
    plot_data <- plot_data2()
    
    figure(
      data = plot_data,
      title = paste0(plot_data$common_name[1], ": ",input$map_elig_shape_click[[1]]),
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
      "What date are you interested in?",
      "Do you want to measure changes in local eligibility between two time points? If so, turn on the 'Compare' switch, and specify the second date.",
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
  
  # Force Shiny to Render the leaflet maps straight away.
  outputOptions(output, "dygraph_gender", suspendWhenHidden = F)
  outputOptions(output, "dygraph_age", suspendWhenHidden = F)
  outputOptions(output, "map_elig", suspendWhenHidden = F)
  outputOptions(output, "ac_map", suspendWhenHidden = F)
  outputOptions(output, "map_com_real", suspendWhenHidden = F)
}

#onStop(function() {
#  dbDisconnect(con)
#})


shinyApp(ui, server, enableBookmarking = "url", onStart = function() {
  onStop(function() {
    poolClose(pool)
  })
})


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
