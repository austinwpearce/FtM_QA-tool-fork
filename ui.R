
# User interface
ui  <- navbarPage("Fieldprint Data QA Tool",
                  theme = shinythemes::shinytheme("united"),
                  tabPanel("File Upload", icon = icon("cloud-upload-alt"),
                           mainPanel(
                             jumbotron(header = "Welcome Field to Market Project Admins and Specialists", 
                                       content = message14,
                                       buttonLabel = "Learn more by watching a video tutorial"),
                             h3("Upload a Comprehensive Data Output File to Get Started"),
                             fileInput('file1', '', accept = c(".xlsx"), width = "600px"),
                             checkboxInput("fake_data", "Click here to load demonstration data", value = FALSE),
                             br(),
                             br(),
                             fluidRow(
                               column(6, panel_div("danger", "Data Upload Status", 
                                                   content = textOutput("report_first_page"))),
                               column(6, panel_div("warning", "Website Last Updated On", 
                                                   content = textOutput("update_date")))
                             ),
                             br(),
                             br(),
                             fluidRow(
                               column(6, panel_div(class_type = "info", 
                                                   panel_title = "Documentation and FAQs",
                                                   content = downloadLink("documentationPDF", "Download PDF"))),
                               column(6, panel_div(class_type = "success", 
                                                   panel_title = "Website Developer Information",
                                                   content = uiOutput("contact_info")))
                             ),
                             #tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
                             bsModal("videomodal", "Video Tutorial", "tabBut", size = "medium" ,
                                     p("Clicking the link below will open a new page on an external website"),
                                     a("QA Tool Video Tutorial", target = "_blank", 
                                       href = "https://vimeo.com/521517782")
                             )
                           )
                  ),
                  tabPanel("Data Filter", icon = icon("filter"),
                           textOutput("filter_text"),
                           br(),
                           fluidRow(
                             column(12, mainPanel(h3("Count of Crop Years by Crop and Season"), 
                                                  tableOutput("filter_table")))
                           ),
                           fluidRow(
                             column(4, style = "background-color:#FF6E00;", 
                                    checkboxGroupInput(inputId = "crop", 
                                                       label = "Deselect Crop(s) to Filter Out", 
                                                       choices = "", selected = "")),
                             column(4, style = "background-color:#8EBDDC;",
                                    checkboxGroupInput(inputId = "crop_year", 
                                                       label = "Deselect Crop Year(s) to Filter Out", 
                                                       choices = "", selected = "")),
                             column(4, style = "background-color:#8FC382;",
                                    checkboxGroupInput(inputId = "field", 
                                                       label = "Deselect Field(s) to Filter Out",
                                                       choices = "", selected = ""))
                           )
                  ),
                  tabPanel("Map View", icon = icon("map"),
                           textOutput("map_help_text"),
                           br(),
                           sidebarPanel(
                             width = 3,
                             selectInput("maps_crop_year_dropdown", "Select Crop Year(s)", 
                                         choices = "", multiple = T, selectize = F, selected = ""),
                             selectInput("maps_crop_dropdown", "Select Crop(s)", 
                                         choices = "", multiple = T, selectize = F, selected = ""),
                             radioButtons("field_centroid", "Plot", c("Boundaries" , "Centroids"), 
                                          selected = "Centroids"),
                             checkboxInput("hidenames", "Show Field Names", FALSE),
                             selectInput("basemap", "Select Basemap", choices = (basemap_providers),
                                         selected = "OpenStreetMap"),
                             selectInput("map_colors", "Select Fill Color",
                                         choices = c("Red" = "red", "Yellow" = "yellow", "Blue" = "blue", 
                                                     "Orange" = "orange", "Purple" = "Purple", "Green" = "green", 
                                                     "Black" = "black", "White" = "white",
                                                     "Illini Orange" = "#E84A27", "Illini Blue" = "#13294B"),
                                         selected = "blue")
                           ),
                           mainPanel(
                             tags$style(type = "text/css", "#map {height: calc(85vh - 80px) !important;}"),
                             leafletOutput("map"),
                             downloadButton("downloadMap", "Download")
                           )
                  ),
                  tabPanel("Sustainability Metrics", icon = icon("ruler"),
                           textOutput("sust_metrics_help_text"),
                           br(),
                           #useShinyalert()
                           actionLink("outlier", "What is an outlier?"),
                           br(),
                           br(),
                           p(strong("Metric Description")),
                           textOutput("helptext"),
                           br(),
                           sidebarPanel(
                             width = 3,
                             #selectInput("season_dropdown", "Select Crop Year", choices = ""),
                             #tags$hr(style="border-color:gray;"),
                             radioButtons("main_metrics", "Select Metric", choices = main_metrics),
                             conditionalPanel(
                               condition = "input.main_metrics == 'Energy Use'",
                               selectInput("energy_subcomponents", "Select Subcomponents", 
                                           choices = energyuse_btu_yield)
                             ),
                             conditionalPanel(
                               condition = "input.main_metrics == 'Greenhouse Gas Emissions'",
                               selectInput("ghg_subcomponents", "Select Subcomponents", choices = ghg_btu_yield)
                             ),
                             conditionalPanel(
                               condition = "input.main_metrics == 'Water Quality'",
                               selectInput("wqi_subcomponents", "Select Subcomponents", choices = wqi_components)
                             ),
                             tags$hr(style="border-color:black;"),
                             checkboxInput("hideoutlier", "Hide Outliers", FALSE),
                             #checkboxInput("unmask", "Unmask Field Names", FALSE)
                             tags$hr(style="border-color:black;"),
                             actionButton("MetricsOutliers", "Table of Potential Outliers", 
                                          icon = icon("table")),
                             bsModal("MetricsModal", "Table of Potential Outliers", 
                                     "MetricsOutliers", size = "large", p(message25), 
                                     DT::DTOutput("metrics_table_outliers"))
                             
                           ),
                           mainPanel(
                             plotOutput("sust_metrics_plot"), # "600px"
                             br(),
                             #p(strong("Table of Potential Outliers")),
                             #textOutput("description_table_outlier"),
                             #br(),
                             #tableOutput("sust_metrics_outliers")
                           )
                  ),
                  tabPanel("Crop Nutrition and Protection", icon = icon("seedling"),
                           textOutput("fertilizers_help_text"),
                           br(),
                           #useShinyalert()
                           actionLink("outlier2", "What is an outlier?"),
                           br(),
                           br(),
                           p(strong("Description")),
                           textOutput("crop_nutrition_helptext"),
                           br(),
                           sidebarPanel(
                             width = 3,
                             #selectInput("fert_season_dropdown", "Select Crop Year", choices = ""),
                             #tags$hr(style="border-color:gray;"),
                             radioButtons("fert_options", "Select Parameter", choices = fert_options),
                             conditionalPanel(
                               condition = "input.fert_options == 'Fertilizers'",
                               selectInput("fert_subcomponents", "Select Fertilizer", choices = c(
                                 "Nitrogen" = "Total N (lb / acre) for All Trips",
                                 "Phosphorus" = "Total P2O5 (lb / acre) for All Trips",
                                 "Potassium" = "Total K2O (lb / acre) for All Trips"
                               ))
                             ),
                             conditionalPanel(
                               condition = "input.fert_options == 'Crop Protectants'",
                               selectInput("chem_subcomponents", "Select Crop Protectant", 
                                           choices = chemical_options)
                             ),
                             tags$hr(style="border-color:black;"),
                             checkboxInput("fert_hideoutlier", "Hide Outliers", FALSE)#,
                             #checkboxInput("fert_unmask", "Unmask Field Names", FALSE)
                           ),
                           mainPanel(
                             plotOutput("fert_plot"),
                             br()#,
                             #p(strong("Table of Potential Outliers")),
                             #textOutput("description_table_outlier_fert"),
                             #br(),
                             #tableOutput("fert_table_outliers")
                           )
                  ),
                  tabPanel("Crop Rotations", icon = icon("file-alt"), # calendar, tractor, file-alt
                           sidebarPanel(
                             width = 3,
                             uiOutput("rot_dropdown"),
                             # selectInput("rot_dropdown", "Select Rotation", choices = ""),
                             checkboxInput("rotation_field_unmask", "Unmask Field Names", FALSE),
                             tags$hr(style="border-color:black;"),
                             actionButton("showTable", "Search Field & Rotation Table", icon = icon("table")),
                             bsModal("modalExample", "Rotation Table",
                                     "showTable", size = "large", DT::DTOutput("searchable_rot_table")),
                             tags$hr(style="border-color:black;"),
                             p(strong("Rotation Template Frequency Count")),
                             tableOutput("rotation_count")
                           ),
                           mainPanel(
                             p(strong(message6)),
                             br(),
                             plotlyOutput("rotation_graphs", height = "400px"),
                             br(),
                             p(strong(message4)),
                             tableOutput("rot_by_fields"),
                             br(),
                             p(strong(message5)),
                             tableOutput("rotation_table")
                           )
                  ),
                  tabPanel(
                    "Fieldprint Inputs Summary", icon = icon("table"),
                    br(),
                    textOutput("ag_summaries_text"),
                    br(),
                    br(),
                    sidebarPanel(
                      width = 2,
                      selectInput("selecttable", "Select Summary Table", choices = table_options)
                    ),
                    mainPanel(
                      textOutput("helptext_by_table"),
                      br(),
                      tableOutput("summary_table_output")
                    )
                  ),
                  tabPanel(
                    "Sandbox", icon = icon("chart-line"),
                    textOutput("sandbox_help_text"),
                    br(),
                    br(),
                    sidebarPanel(
                      width = 3,
                      selectInput("sandbox_x_dropdown", "Choose X-axis Variable", choices = ""),
                      selectInput("sandbox_y_dropdown", "Choose Y-axis Variable", choices = ""),
                      selectInput("sandbox_grouping_var", "Choose Color Variable", 
                                  choices = grouping_choices_byo, selected = "State"),
                      tags$hr(style="border-color:black;"),
                      checkboxInput("sandbox_hideoutlier", "Hide Outliers", FALSE),
                      #checkboxInput("sandbox_hidetrend", "Hide Average Trend Line", FALSE),
                      tags$hr(style="border-color:black;"),
                      actionButton("showsandboxTable", "Explore the Data in Table Form", icon = icon("table")),
                      bsModal("sandboxmodal", "Data Table", 
                              "showsandboxTable", size = "large", DT::DTOutput("sandbox_popup_table"))
                    ),
                    mainPanel(plotOutput("sandbox_plot"))
                  ),
                  tabPanel(
                    "Pivot Table", icon = icon("th-list"), # bars
                    textOutput("pivot_help_text"),
                    br(),
                    br(),
                    rpivotTable::rpivotTableOutput("pivot")
                  ),
                  tabPanel(
                    "Enhanced Table Search", icon = icon("search"),
                    textOutput("enhanced_table_help_text"),
                    br(),
                    mainPanel(
                      pickerInput("table_filtering", label = "Select Group of Variables", 
                                  choices = enhanced_table_options, multiple = T, 
                                  options = list(`actions-box` = TRUE),
                                  selected = "Field Information"),
                      DT::DTOutput("enhanced_table_search")
                    )
                  )
)