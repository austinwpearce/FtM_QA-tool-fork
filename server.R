
# Server
server <- function(input, output, session){
  
  #################################################################################################
  # Global reactives
  #################################################################################################
  
  # Reactive menu for crop years (applicable to all plots that filter by year)
  crop_year_menu  <- reactive({
    unique(sort(df()$`Crop Year`))
  })
  
  # Reactive menu for all variable names except for application trips
  allvarsmenu <- reactive({
    subset(names(df()), !grepl("^Application Trip|JSON", names(df())))
  })
  
  #################################################################################################
  # Tab 1 - file upload
  #################################################################################################
  
  # Message to display when data are loaded
  output$report_first_page <- renderText({
    paste("The data for", project_name(), "are ready for review")
  })
  
  # Documentation PDF to download
  output$documentationPDF <- downloadHandler(
    filename <- function() {
      paste("QA Tool Documentation", ".pdf", sep = "")
    },
    content <- function(file) {
      file.copy("QA Tool Documentation.pdf", file)
    },
    contentType = "application/pdf"
  )
  
  # Contact info
  output$contact_info <- renderUI({
    tagList(#"Contact", 
            a("Paul Hishmeh", 
              href = 'mailto:phishmeh@fieldtomarket.org?Subject=Fieldprint%20QA%20Tool', target='_top'),
            "- Data and Technology Director")
  })
  
  # Latest release date
  output$update_date <- renderText({
    "December 02 2021"
  })
  
  # Validation for input file
  ready_set <- reactive({
    validate(need(input$file1 != "", "No data have been uploaded"))
    input$file1
  })
  
  # Reading the first sheet to get project name
  dataframe_landing <- reactive({
    if(input$fake_data == FALSE){
      readxl::read_excel(ready_set()$datapath, sheet = 1)
    } else {
      readxl::read_excel("Demo_Comprehensive_Data.xlsx", 
                         sheet = 1)
    }
  })
  
  # Extracting project name
  project_name <- reactive({
    dataframe_landing() %>% 
      filter(`Field to Market` == "Fieldprint Project") %>% 
      pull("...2") %>% 
      sub("^\\S+\\s+", '', .)
  })
  
  # Validation for main dataset
  df_message <- reactive({
    validate(need(input$file1 != "", "No data have been uploaded"))
    input$file1
  })
  
  # To load fake dataset or user data
  df_prelim <- reactive({
    if(input$fake_data == FALSE){
      readxl::read_excel(df_message()$datapath, sheet = 2) %>%
        drop_na(`Soil Carbon`) %>% # to remove fields for which metrics were not run
        mutate(composite_field = paste(`Crop Year`, `Grower ID`, `Field Name`))
    } else {
      readxl::read_excel("Demo_Comprehensive_Data.xlsx", sheet = 2) %>%
        drop_na(`Soil Carbon`) %>% 
        mutate(composite_field = paste(`Crop Year`, `Grower ID`, `Field Name`))
    }
  })
  
  # This is the main dataset that feeds most of the processes here
  # Some variables added or formatted
  df <- reactive({
    df_subset2() %>% # previously df_prelim(), see tab 2 for origin of df_subset2
      mutate(`Crop Year` = as.factor(`Crop Year`),
             `Slope (%)` = as.numeric(`Slope (%)`),
             # 2021-09-03 soil organic matter is not found in the report anymore it seems
             #`Organic Matter Content (%)` = as.numeric(`Organic Matter Content (%)`),
             `Field Name` = str_replace_all(string = `Field Name`, 
                                            pattern = '"', 
                                            replacement = ""),
             `All Irrigation Water Applied (acre inch / acre)` =
               `Groundwater Irrigation: Water Applied (acre_inch / acre)` +
               `Surface Water Irrigation: Water Applied (acre_inch / acre)`) %>% 
      #-trying to rename manure column 7/22
      rename("Manure Application 1: Pounds of manure N applied per acre (lbs N/acre)" = 
               "Manure Application 1: Pounds of N per Acre (lb/acre)")
    #%>%
    # 2021-09-03 Lime content has a proper name now
    #  rename("Lime Amount (short ton / acre)" = `Product Amount (ton / acre)`)

  })
  
  # Long version of main dataset. Used for soil conservation, crop protectants, and manure N
  df_long <- reactive({
    df() %>%
      gather(key = "attribute",
             value = "value",
             -c(`Grower ID`, `Field Name`, `Location`,
                `State`, `Field GeoJSON`, `Crop Year`, Crop)) %>%
      mutate(value_numeric = as.numeric(str_extract(value, "^-?\\d+\\.*\\d*"))) #%>% # regex may change
      drop_na() #%>%
      # 2021-09-03 reorganized above, also we don't need stuff below anymore
      # mutate(`Crop Protectants` = case_when(
      #   str_detect(string = attribute, pattern = "Herbicides") ~ "Herbicides",
      #   str_detect(string = attribute, pattern = "Insecticides") ~ "Insecticides",
      #   str_detect(string = attribute, pattern = "Fungicides") ~ "Fungicides",
      #   str_detect(string = attribute, pattern = "Growth Regulator") ~ "Growth Regulators",
      #   str_detect(string = attribute, pattern = "Fumigants") ~ "Fumigants",
      #   str_detect(string = attribute, pattern = "Harvest Aids") ~ "Harvest Aids"),
      #   `N from Manure` = case_when(
      #     str_detect(string = attribute, pattern = "Pounds of N per Acre") ~ "YES"))
  })
  
  #################################################################################################
  # Tab 2 - global data filter
  #################################################################################################
  
  # Description to display in UI
  output$filter_text <- renderText({
    message31
  })
  
  # Extracting vector of crops
  crops <- reactive({
    sort(unique(df_prelim()$Crop))
  })
  
  # Vector of crops populates a menu in the UI
  observe({
    updateCheckboxGroupInput(session, "crop",
                             choices = crops(),
                             selected = crops()
    )
  })
  
  # Extracting vector of crop years
  crop_years <- reactive({
    sort(unique(df_prelim()$`Crop Year`))
  })
  
  # Vector of crop years populates a menu in the UI
  observe({
    updateCheckboxGroupInput(session, "crop_year",
                             choices = crop_years(),
                             selected = crop_years()
    )
  })
  
  # First filtering by crop and crop year
  df_subset1 <- reactive({
    df_prelim() %>% 
      filter(Crop %in% input$crop & `Crop Year` %in% input$crop_year)
  })
  
  # Extracting reactive vector of field names from the previous filtered dataset
  # composite_field is a string of crop year, grower id, and field name
  updated_field_vector <- reactive({
    sort(unique(df_subset1()$composite_field))
  })
  
  # Vector of field names populates a menu in the UI 
  observe({
    updateCheckboxGroupInput(session, "field",
                             choices = updated_field_vector(),
                             selected = updated_field_vector()
    )
  })
  
  # Second data filtering by field name (composite_field)
  # This is the dataset that will be read in tab 1 as df() and feeds most processes
  df_subset2 <- reactive({
    df_subset1() %>%
      filter(composite_field %in% input$field)
  })
  
  # Count table for crop years by crop and season to display in UI
  # It updates in real-time as data are added or filtered out
  output$filter_table <- renderTable(
    df_subset2()  %>% 
      group_by(Crop, `Crop Year`) %>% 
      tally() %>% 
      spread(key = `Crop Year`, value = n)
  )
  
  #################################################################################################
  # Tab 3 - map view
  #################################################################################################
  
  # Description to display in UI
  output$map_help_text <- renderText({
    message13
  })
  
  # Vector of crops
  crop_menu <- reactive({
    unique(sort(df()$Crop))
  })
  
  # Vector of crops updates dropdown menu for map view in UI
  observe({
    updateSelectInput(session, "maps_crop_dropdown",
                      choices = crop_menu(),
                      selected = crop_menu() # selects all crops
    )
  })
  
  # Crop year dropdown menu for map view
  observe({
    updateSelectInput(session, "maps_crop_year_dropdown",
                      choices = crop_year_menu(),
                      selected = crop_year_menu() # crop_year_menu()[1] selects the first one
    )
  })
  
  # Field boundaries extracted from GeoJSON variable
  field_boundaries <- reactive({
    df <- df()
    
    geojsonsf::geojson_sf(df$`Field GeoJSON`) %>%
      mutate(field_name = df$`Field Name`,
             crop_year = df$`Crop Year`,
             crop = df$Crop)
  })
  
  # Field centroids converted from field_boundaries()
  field_centroids <- reactive({
    field_boundaries() %>% 
      mutate(geometry = sf::st_point_on_surface(geometry))
  })
  
  # Creating a bounding box to keep the map in a fixed position across crops and years
  bounding_box <- reactive({
    sf::st_bbox(field_boundaries()) %>% 
      as.vector()
  })
  
  # Leaflet maps created with custom function in global.R
  # Depending on user input, boundaries or centroids are displayed
  mapReact <- reactive({
    switch(input$field_centroid,
           "Boundaries" = bound_function(field_boundaries(), 
                                         input$maps_crop_year_dropdown,
                                         input$maps_crop_dropdown,
                                         input$basemap, 
                                         input$hidenames, 
                                         input$map_colors,
                                         bounding_box()
           ),
           "Centroids" = centroid_function(field_centroids(), 
                                           input$maps_crop_year_dropdown,
                                           input$maps_crop_dropdown,
                                           input$basemap, 
                                           input$hidenames, 
                                           input$map_colors,
                                           bounding_box()
           )
    )
  })
  
  # Map to display in UI
  output$map <- renderLeaflet({
    mapReact()
  })
  
  # This updates the map as users zoom, pan, or add other map options
  user.created.map <- reactive({
    # call the foundational Leaflet map
    mapReact() %>%
      # store the view based on UI
      setView(lng = input$map_center$lng,
              lat = input$map_center$lat,
              zoom = input$map_zoom
      )
  })
  
  # The map customized by users is fed to this download function
  # Whatever the map is showing at the moment of clicking Download is what will be given to the user
  output$downloadMap <- downloadHandler(
    filename = paste(project_name(), ' Map View' , '.png', sep = ''),
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      saveWidget(user.created.map(), "temp.html", selfcontained = FALSE)
      webshot("temp.html", file = file, cliprect = "viewport")
    })
  
  #################################################################################################
  # Tab 4 - sustainability metrics
  #################################################################################################
  
  # Description to display in UI
  output$sust_metrics_help_text <- renderText({
    message2
  })
  
  # Popup help text about outliers for sustainability metrics tab
  observeEvent(input$outlier, {
    # Show a modal when the button is pressed
    shinyalert("What is an outlier?", text = outlier_helptip, type = "info",
               imageUrl = "https://online.stat.psu.edu/stat200/sites/stat200/files/inline-images/Boxplotwithdescriptions.png",
               imageWidth = 450,
               imageHeight = 350, animation = F)
  })
  
  # Metric description changes by metric selection
  output$helptext <- renderText({
    if (input$main_metrics == "Biodiversity") {
      message18
    } else if (input$main_metrics == "Irrigation Water Use") {
      message17
    } else if (input$main_metrics == "Land Use") {
      message21
    } else if (input$main_metrics == "Soil Carbon") {
      message15
    } else if (input$main_metrics == "Energy Use") {
      message19
    } else if (input$main_metrics == "Greenhouse Gas Emissions") {
      message20
    } else if (input$main_metrics == "Water Quality") {
      message22
    } else if (input$main_metrics == "Soil Conservation") {
      message16
    }
  })
  
  # Crop year vector to populate menu dropdown in UI for sustainability metrics plot
  #observe({
  #  updateSelectInput(session, "season_dropdown",
  #                    choices = crop_year_menu()
  #  )
  #})
  
  # Description of outliers to display in UI
  #output$description_table_outlier <- renderText({
  #  message25
  #})
  
  # Sustainability metrics plot
  # All metrics except for soil conservation, which gets its own plot
  eight_metrics <- reactive({
    if (input$main_metrics == "Biodiversity") {
      y_input <- "Biodiversity Score (Total % Realized HPI)"
    } else if (input$main_metrics == "Irrigation Water Use") {
      y_input <- "Irrigation Water Use Score"
    } else if (input$main_metrics == "Land Use") {
      y_input <- "Land Use Score (acre / yield units)"
    } else if (input$main_metrics == "Soil Carbon") {
      y_input <- "Soil Carbon"
    } else if (input$main_metrics == "Energy Use") {
      y_input <- input$energy_subcomponents
    } else if (input$main_metrics == "Greenhouse Gas Emissions") {
      y_input <- input$ghg_subcomponents
    } else if (input$main_metrics == "Water Quality") {
      y_input <- input$wqi_subcomponents
    } else if (input$main_metrics == "Soil Conservation") {
      y_input <- "Soil Conservation Score (ton / acre / year)"
    }
    
    # Dropdown options for Y-axis variable and crop year
    #cropyear_input <- input$season_dropdown
    
    # Adding outlier parameters
    data1 <- df() %>%
      #filter(`Crop Year` == cropyear_input) %>%
      group_by(`Crop Year`, Crop) %>%
      mutate(
        IQR = IQR(get(y_input), na.rm = T),
        Q25 = quantile(get(y_input), 0.25, na.rm = T),
        Q75 = quantile(get(y_input), 0.75, na.rm = T),
        is_outlier = (get(y_input) < Q25 - (3 * IQR) | get(y_input) > Q75 + (3 * IQR))
      )
    
    # Select dataset either with or without outlier based on checkbox = T/F
    if (input$hideoutlier) data1 <- filter(data1, is_outlier == F)
    
    plot1 <- data1 %>% 
      ggplot(aes(x = `Crop Year`, y = get(y_input))) +
      geom_boxplot(width = 0.2, outlier.shape = NA) +
      geom_jitter(width = 0.05, height = 0.05)+
      labs(x = "Crop Year", y = paste(stringr::str_wrap(y_input, width = 20))) +
      facet_wrap(~ Crop, scales = "free", ncol = 2) +
      ggthemes::theme_base() +
      theme(rect = element_rect(fill = "white", colour = "white"), 
            panel.border = element_rect(color = "black"),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            text = element_text(face = "bold"),
            strip.text = element_text(size = rel(1.5)))
    
    
    # Plot either dataset based on unmasking checkbox
    # if (input$unmask == F) {
    #   plot1 <- data1 %>%
    #     ggplot(aes(x = `Field Name`, y = get(y_input), color = `Crop`,
    #                text = paste("Field Name:", `Field Name`, "\n", "Metric Value:", get(y_input)))) +
    #     geom_point() +
    #     labs(y = "", x = "", color = "", title = paste(y_input, "- Season", cropyear_input)) +
    #     scale_color_brewer(palette = "Set1") +
    #     theme_bw() +
    #     theme(axis.text.x = element_text(angle = 0, size = rel(.75)),
    #           panel.grid.minor = element_blank(),
    #           plot.title = element_text(size = rel(0.9))) +
    #     scale_x_discrete(labels = anonymize) +
    #     facet_wrap(~ Crop, scales = "free") +
    #     coord_flip()
    # } else {
    #   plot1 <- data1 %>%
    #     ggplot(aes(x = `Field Name`, y = get(y_input), color = `Crop`, 
    #                text = paste("Field Name:", `Field Name`, "\n", "Metric Value:", get(y_input)))) +
    #     geom_point() +
    #     labs(y = "", x = "", color = "", title = paste(y_input, "- Season", cropyear_input)) +
    #     scale_color_brewer(palette = "Set1") +
    #     theme_bw() +
    #     theme(axis.text.x = element_text(angle = 0, size = rel(.75)),
    #           panel.grid.minor = element_blank(),
    #           plot.title = element_text(size = rel(0.9))) +
    #     facet_wrap(~ Crop, scales = "free") +
    #     coord_flip()
    # }
    #ggplotly(plot1, height = max(length(unique(data1$`Field Name`))*20, 200), tooltip = "text")
    print(plot1)
  })
  
  # Table of outliers for all metrics
  output$sust_metrics_outliers <- renderTable({
    if (input$main_metrics == "Biodiversity") {
      y_input <- "Biodiversity Score (Total % Realized HPI)"
    } else if (input$main_metrics == "Irrigation Water Use") {
      y_input <- "Irrigation Water Use Score"
    } else if (input$main_metrics == "Land Use") {
      y_input <- "Land Use Score (acre / yield units)"
    } else if (input$main_metrics == "Soil Carbon") {
      y_input <- "Soil Carbon"
    } else if (input$main_metrics == "Energy Use") {
      y_input <- input$energy_subcomponents
    } else if (input$main_metrics == "Greenhouse Gas Emissions") {
      y_input <- input$ghg_subcomponents
    } else if (input$main_metrics == "Water Quality") {
      y_input <- input$wqi_subcomponents
    } else if (input$main_metrics == "Soil Conservation") {
      y_input <- "Soil Conservation Score (ton / acre / year)"
    }
    
    # Adding outlier parameters
    tmp1 <- df() %>%
      group_by(`Crop Year`, Crop) %>%
      summarise(
        IQR = IQR(get(y_input), na.rm = T),
        Q25 = quantile(get(y_input), 0.25, na.rm = T),
        Q75 = quantile(get(y_input), 0.75, na.rm = T)
      )
    
    tmp2 <- df() %>%
      filter(`Crop Year` == input$season_dropdown) %>%
      left_join(tmp1, by = c("Crop Year","Crop")) %>%
      filter(get(y_input) > (Q75 + 3 * IQR) |
               get(y_input) < (Q25 - 3 * IQR)) %>%
      select(`Crop Year`, Crop, `Field Name`, all_of(y_input)) %>% 
      arrange(`Crop Year`, Crop)
    
    if (input$unmask == F) {
      tmp2 %>% 
        mutate(`Field Name` = anonymize(`Field Name`))
    } else {
      tmp2 
    }
  })
  
  # Soil Conservation plot
  soil_conservation <- reactive({
    outlier_y_input <- "Soil Conservation Score (ton / acre / year)"
    
    outliers <- df() %>%
      group_by(`Crop Year`, Crop) %>%
      mutate(
        IQR = IQR(get(outlier_y_input), na.rm = T),
        Q25 = quantile(get(outlier_y_input), 0.25, na.rm = T),
        Q75 = quantile(get(outlier_y_input), 0.75, na.rm = T),
        is_outlier = (get(outlier_y_input) < Q25 - 3*IQR | get(outlier_y_input) > Q75 + 3*IQR)
      ) %>% 
      ungroup() %>% 
      select(`Crop Year`, Crop, `Field Name`, is_outlier)
    
    varsubset <- lookup_names %>%
      filter(group == "soil_erosion_components") %>%
      pull(variable)
    
    y_input <- "value_numeric"
    
    tmp1 <- df_long() %>%
      left_join(outliers, by = c("Crop Year", "Crop", "Field Name")) %>% 
      filter(attribute %in% varsubset & `Crop Year` == input$season_dropdown) %>%
      mutate(`Field Name` = as.factor(`Field Name`))
    
    # Select dataset either with or without outlier based on checkbox = T/F
    if (input$hideoutlier) tmp1 <- filter(tmp1, is_outlier == F)
    
    if (input$unmask == F) {
      p <- tmp1 %>%
        mutate(attribute = ifelse(test = attribute == "Water Erosion (ton / acre / year)",
                                  yes = "Water Erosion",
                                  no = attribute)) %>%
        mutate(attribute = ifelse(test = attribute == "Wind Erosion (ton / acre / year)",
                                  yes = "Wind Erosion",
                                  no = attribute)) %>%
        ggplot(aes(x = `Field Name`, y = value_numeric, fill = attribute,
                   text = paste("Field Name:", `Field Name`, "\n", "Value:", get(y_input), "\n",
                                attribute))) +
        geom_col(position = "stack") +
        scale_fill_manual(name = "", values = c("Water Erosion" = "deepskyblue",
                                                "Wind Erosion" = "gold")) +
        labs(y = "", x = "", title = paste("Soil Conservation (ton / acre / year)",
                                           "- Season", input$season_dropdown)) +
        theme_bw() +
        theme(axis.text.x =element_text(angle = 0, size = rel(0.75)),
              plot.title = element_text(size = rel(0.90), margin=margin(0,0,10,0)),
              panel.spacing = unit(1, "lines")) +
        scale_x_discrete(labels = anonymize) +
        facet_wrap(~ Crop, scales = "free") + # free_x could be an option
        coord_flip()
    } else {
      p <- tmp1 %>%
        mutate(attribute = ifelse(test = attribute == "Water Erosion (ton / acre / year)",
                                  yes = "Water Erosion",
                                  no = attribute)) %>%
        mutate(attribute = ifelse(test = attribute == "Wind Erosion (ton / acre / year)",
                                  yes = "Wind Erosion",
                                  no = attribute)) %>%
        ggplot(aes(x = `Field Name`, y = value_numeric, fill = attribute,
                   text = paste("Field Name:", `Field Name`, "\n", "Value:", get(y_input), "\n",
                                attribute))) +
        geom_col(position = "stack") +
        scale_fill_manual(name = "", values = c("Water Erosion" = "deepskyblue",
                                                "Wind Erosion" = "gold")) +
        labs(y = "", x = "", title = paste("Soil Conservation (ton / acre / year)",
                                           "- Season", input$season_dropdown)) +
        theme_bw() +
        theme(axis.text.x =element_text(angle = 0, size = rel(0.75)),
              plot.title = element_text(size = rel(0.90), margin=margin(0,0,10,0)),
              panel.spacing = unit(1, "lines")) +
        facet_wrap(~ Crop, scales = "free") + # free_x could be an option
        coord_flip()
    }
    ggplotly(p, height = max(length(unique(tmp1$`Field Name`))*20, 200), tooltip = "text")
  })
  
  # Plot to display in UI, either seven metrics or soil conservation
  output$sust_metrics_plot <- renderPlot({
    #if (input$main_metrics == "Soil Conservation") {
    #  soil_conservation()
    #} else {
    eight_metrics() 
    #}
  })
  
  # Popup table of outliers
  
  metrics_outliers_calculations <- reactive({
    
    if (input$main_metrics == "Biodiversity") {
      y_input <- "Biodiversity Score (Total % Realized HPI)"
    } else if (input$main_metrics == "Irrigation Water Use") {
      y_input <- "Irrigation Water Use Score"
    } else if (input$main_metrics == "Land Use") {
      y_input <- "Land Use Score (acre / yield units)"
    } else if (input$main_metrics == "Soil Carbon") {
      y_input <- "Soil Carbon"
    } else if (input$main_metrics == "Energy Use") {
      y_input <- input$energy_subcomponents
    } else if (input$main_metrics == "Greenhouse Gas Emissions") {
      y_input <- input$ghg_subcomponents
    } else if (input$main_metrics == "Water Quality") {
      y_input <- input$wqi_subcomponents
    } else if (input$main_metrics == "Soil Conservation") {
      y_input <- "Soil Conservation Score (ton / acre / year)"
    }
    
    df() %>%
      #filter(`Crop Year` == cropyear_input) %>%
      group_by(`Crop Year`, Crop) %>%
      mutate(
        IQR = IQR(get(y_input), na.rm = T),
        Q25 = quantile(get(y_input), 0.25, na.rm = T),
        Q75 = quantile(get(y_input), 0.75, na.rm = T),
        is_outlier = (get(y_input) < Q25 - (3 * IQR) | get(y_input) > Q75 + (3 * IQR))
      ) %>% 
      filter(is_outlier == T) %>% 
      select(`Crop Year`, Crop, `Grower ID`, `Field Name`, paste(y_input))
  })
  
  output$metrics_table_outliers <- DT::renderDT(
    
    metrics_outliers_calculations(), 
    options = list(lengthChange = T, pageLength = 25, lengthMenu = c(25, 50, 75))
  )
  
  #################################################################################################
  # Tab 5 - fertilizer and crop protectants
  #################################################################################################
  
  # Description to show in UI
  output$fertilizers_help_text <- renderText({
    message2
  })
  
  # Popup help text about outliers for crop nutrition and protection tab
  observeEvent(input$outlier2, {
    # Show a modal when the button is pressed
    shinyalert("What is an outlier?", text = outlier_helptip, type = "info",
               imageUrl = "https://online.stat.psu.edu/stat200/sites/stat200/files/inline-images/Boxplotwithdescriptions.png",
               imageWidth = 450,
               imageHeight = 350, animation = F)
  })
  
  # Description to show in UI, it changes according to selection
  output$crop_nutrition_helptext <- renderText({
    if (input$fert_options == "Fertilizers") {
      message10
    } else if (input$fert_options == "Crop Protectants") {
      message11
    } else if (input$fert_options == "Lime") {
      message3
    } else if (input$fert_options == "N from Manure") {
      message23
    }
  })
  
  # Description of outlier table
  # output$description_table_outlier_fert <- renderText({
  #   message25
  # })
  
  # Crop year dropdown for fertilizers and crop protectants
  # observe({
  #   updateSelectInput(session, "fert_season_dropdown",
  #                     choices = crop_year_menu()
  #   )
  # })
  
  # Reactive menu for crop years with N from Manure applications
  manure_crop_years <- reactive({
    tmp1 <- df_long() %>%
      #filter(`N from Manure` == "YES")
      # 2021-09-03 variable name changes
      filter(attribute == "Number of Manure Applications" & value_numeric >= 1)
    
    unique(sort(droplevels(tmp1$`Crop Year`)))
  })
  
  # Reactive menu for crop years with lime applications
  lime_crop_years <- reactive({
    # 2021-09-03 variable name changes
    tmp1 <- df()[complete.cases(df()$`Lime Amount (ton / acre)`), ]
    
    unique(sort(droplevels(tmp1$`Crop Year`)))
  })
  
  chem_crop_years <- reactive({
    # 2021-09-03 variable name changes
    tmp1 <- df() %>% 
      filter(get(input$chem_subcomponents) > 0) %>% 
      pull(`Crop Year`)
    
    #unique(sort(droplevels(tmp1$`Crop Year`)))
  })
  
  # Crop protectant observer
  # If input is lime or manure only the years with applications are shown in the UI menu
  # observe({
  #   if (input$fert_options == "N from Manure") {
  #     updateSelectInput(session, "fert_season_dropdown",
  #                       label = "Select Crop Year",
  #                       choices = manure_crop_years())
  #   } else if (input$fert_options == "Lime") {
  #     updateSelectInput(session, "fert_season_dropdown",
  #                       label = "Select Crop Year",
  #                       choices = lime_crop_years())
  #   } else {
  #     updateSelectInput(session, "fert_season_dropdown",
  #                       label = "Select Crop Year",
  #                       choices = crop_year_menu())
  #   }
  # })
  
  # Plot for fertilizer and lime
  ag_input_plot <- reactive({
    if (input$fert_options == "Fertilizers") {
      y_input <- input$fert_subcomponents
    } else if (input$fert_options == "Lime") {
      y_input <- "Lime Amount (ton / acre)"
      validate(need(lime_crop_years(), message = "No lime applications were found"))
    } else if (input$fert_options == "Crop Protectants") {
      y_input <- input$chem_subcomponents
      validate(need(chem_crop_years(), message = "No applications were found for the selected category"))
    } else if (input$fert_options == "N from Manure") {
      y_input <- "Manure Application 1: Pounds of manure N applied per acre (lbs N/acre)"
    }
    
    # Dropdown options for Y-axis variable and crop year
    #cropyear_input <- input$fert_season_dropdown
    
    # Adding outlier parameters
    data1 <- df() %>%
      #filter(`Crop Year` == cropyear_input) %>%
      group_by(`Crop Year`, Crop) %>%
      mutate(
        IQR = IQR(get(y_input), na.rm = T),
        Q25 = quantile(get(y_input), 0.25, na.rm = T),
        Q75 = quantile(get(y_input), 0.75, na.rm = T),
        is_outlier = (get(y_input) < Q25 - (3 * IQR) | get(y_input) > Q75 + (3 * IQR))
      )
    
    # Select dataset either with or without outlier based on checkbox = T/F
    if (input$fert_hideoutlier) data1 <- filter(data1, is_outlier == F)
    
    plot2 <- data1 %>% 
      filter(get(y_input) > 0) %>% 
      ggplot(aes(x = `Crop Year`, y = get(y_input))) +
      geom_boxplot(width = 0.2, outlier.shape = NA) +
      geom_jitter(width = 0.05, height = 0.05) +
      labs(x = "Crop Year", y = paste(stringr::str_wrap(y_input, width = 20))) +
      facet_wrap(~ Crop, scales = "free", ncol = 2) +
      ggthemes::theme_base() +
      theme(rect = element_rect(fill = "white", color = "white"), 
            panel.border = element_rect(color = "black"),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            text = element_text(face = "bold"),
            strip.text = element_text(size = rel(1.5)))
    
    print(plot2)
    
    # # Plot either dataset
    # if (input$fert_unmask == F) {
    #   plot2 <- data1 %>%
    #     ggplot(aes(x = `Field Name`, y = get(y_input), color = `Crop`, 
    #                text = paste("Field Name:", `Field Name`, "\n", "Value:", get(y_input)))) +
    #     geom_point() +
    #     labs(y = "", x = "", color = "", title = paste(y_input, "- Season", cropyear_input)) +
    #     scale_color_brewer(palette = "Set1") +
    #     theme_bw() +
    #     theme(axis.text.x = element_text(angle = 0, size = rel(.75)),
    #           panel.grid.minor = element_blank(),
    #           plot.title = element_text(size = rel(0.9))) +
    #     scale_x_discrete(labels = anonymize) +
    #     facet_wrap(~ Crop, scales = "free") +
    #     coord_flip()
    # } else {
    #   plot2 <- data1 %>%
    #     ggplot(aes(x = `Field Name`, y = get(y_input), color = `Crop`, 
    #                text = paste("Field Name:", `Field Name`, "\n", "Value:", get(y_input)))) +
    #     geom_point() +
    #     labs(y = "", x = "", color = "", title = paste(y_input, "- Season", cropyear_input)) +
    #     scale_color_brewer(palette = "Set1") +
    #     theme_bw() +
    #     theme(axis.text.x = element_text(angle = 0, size = rel(.75)),
    #           panel.grid.minor = element_blank(),
    #           plot.title = element_text(size = rel(0.9))) +
    #     facet_wrap(~ Crop, scales = "free") +
    #     coord_flip()
    # }
    # ggplotly(plot2, height = max(length(unique(data1$`Field Name`))*20, 200), tooltip = "text")
  })
  
  # fert_lime_outliers <- reactive({
  #   if (input$fert_options == "Fertilizers") {
  #     y_input <- input$fert_subcomponents
  #   } else if (input$fert_options == "Lime") {
  #     y_input <- "Lime Amount (short ton / acre)"
  #   }
  #   
  #   tmp1 <- df() %>%
  #     group_by(`Crop Year`, Crop) %>%
  #     summarise(
  #       IQR = IQR(get(y_input), na.rm = T),
  #       Q25 = quantile(get(y_input), 0.25, na.rm = T),
  #       Q75 = quantile(get(y_input), 0.75, na.rm = T)
  #     )
  #   
  #   tmp2 <- df() %>%
  #     filter(`Crop Year` == input$fert_season_dropdown) %>%
  #     left_join(tmp1, by = c("Crop Year","Crop")) %>%
  #     filter(get(y_input) > (Q75 + 3 * IQR) |
  #              get(y_input) < (Q25 - 3 * IQR)) %>%
  #     select(`Crop Year`, Crop, `Field Name`, all_of(y_input)) %>% 
  #     arrange(`Crop Year`, Crop)
  #   
  #   if (input$fert_unmask == F) {
  #     tmp2 %>% 
  #       mutate(`Field Name` = anonymize(`Field Name`))
  #   } else {
  #     tmp2 
  #   }
  # })
  
  # Crop protectants plot
  cp_plot <- reactive({
    
    y_input <- "sum_cp"
    
    tmp1 <- df_long() %>%
      #filter(`Crop Year` == input$fert_season_dropdown) %>%
      group_by(`Crop Year`, `Field Name`, `Crop`, `Crop Protectants`) %>%
      summarize(sum_cp = sum(value_numeric, na.rm = T)) %>%
      drop_na() %>%
      ungroup() %>% # here stop and create table, to calculate outliers as well, then use dataset in plot
      filter(sum_cp > 0) %>% # watch out for this
      group_by(`Crop Year`, Crop, `Crop Protectants`) %>%
      mutate(
        IQR = IQR(get(y_input), na.rm = T),
        Q25 = quantile(get(y_input), 0.25, na.rm = T),
        Q75 = quantile(get(y_input), 0.75, na.rm = T),
        is_outlier = (get(y_input) < Q25 - (3 * IQR) | get(y_input) > Q75 + (3 * IQR))
      )
    
    # Select dataset either with or without outlier based on checkbox = T/F
    if (input$fert_hideoutlier) tmp1 <- filter(tmp1, is_outlier == F)
    #plot3 <-
    tmp1 %>%
      ggplot(aes(x = `Crop Year`, y = get(y_input))) +
      #geom_boxplot(width = 0.2) +
      geom_jitter(aes(shape = `Crop Protectants`, color = `Crop Protectants`), width = 0.10, size = 3) +
      labs(x = "Crop Year", y = "Total Number of Applications") +
      facet_wrap(~ Crop, scales = "free", ncol = 2) +
      scale_color_manual(name = "Type", values = c("Fumigants" = "darkblue",
                                                   "Fungicides" = "purple",
                                                   "Growth Regulators" = "red",
                                                   "Herbicides" = "darkgreen",
                                                   "Insecticides" = "black")) +
      ggthemes::theme_base() +
      guides(shape = FALSE) +
      theme(rect = element_rect(fill = "white", colour = "white"), 
            panel.border = element_rect(color = "black"),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            text = element_text(face = "bold"),
            strip.text = element_text(size = rel(1.5)))
    
    #print(plot3)
    
    # if (input$fert_unmask == F) {
    # plot3 <- tmp1 %>% 
    # ggplot(aes(x = `Field Name`, y = `sum_cp`, fill = `Crop Protectants`, 
    #            text = paste("Field Name:", `Field Name`, "\n", sum_cp, `Crop Protectants`))) +
    #   geom_col() +
    #   labs(y = "", x = "", title = paste("Number of Applied Crop Protectants", "- Season",
    #                                      input$fert_season_dropdown)) +
    #   facet_wrap(~ Crop, scales = "free") + # free_x to keep fields fixed 
    #   theme_bw() +
    #     scale_x_discrete(labels = anonymize) +
    #     scale_fill_manual(name = "", values = c("Fumigants" = "darkblue",
    #                                             "Fungicides" = "gold",
    #                                             "Growth Regulators" = "red",
    #                                             "Herbicides" = "darkgreen",
    #                                             "Insecticides" = "black")) +
    #     theme(axis.text.x = element_text(angle = 0, size = rel(0.75)),
    #         plot.title = element_text(size = rel(0.90))) +
    #   coord_flip()
    # } else {
    #   plot3 <- tmp1 %>% 
    #     ggplot(aes(x = `Field Name`, y = `sum_cp`, fill = `Crop Protectants`, 
    #                text = paste("Field Name:", `Field Name`, "\n", sum_cp, `Crop Protectants`))) +
    #     geom_col() +
    #     labs(y = "", x = "", title = paste("Number of Applied Crop Protectants", "- Season",
    #                                        input$fert_season_dropdown)) +
    #     facet_wrap(~ Crop, scales = "free") + # free_x to keep fields fixed 
    #     theme_bw() +
    #     scale_fill_manual(name = "", values = c("Fumigants" = "darkblue",
    #                                             "Fungicides" = "gold",
    #                                             "Growth Regulators" = "red",
    #                                             "Herbicides" = "darkgreen",
    #                                             "Insecticides" = "black")) +
    #     theme(axis.text.x = element_text(angle = 0, size = rel(0.75)),
    #           plot.title = element_text(size = rel(0.90))) +
    #     coord_flip()
    # }
    # ggplotly(plot3, height = max(length(unique(tmp1$`Field Name`))*20, 200), tooltip = "text")
  })
  
  # Outliers for crop protectants
  # cp_outliers <- reactive({
  #   y_input <- "sum_cp"
  #   
  #   tmp1 <- df_long() %>%
  #     filter(`Crop Year` == input$fert_season_dropdown) %>%
  #     group_by(`Crop Year`, `Field Name`, `Crop`, `Crop Protectants`) %>%
  #     summarize(sum_cp = sum(value_numeric, na.rm = T)) %>%
  #     drop_na() %>%
  #     ungroup() %>%
  #     filter(sum_cp > 0) %>% # watch out for this
  #     group_by(`Crop Year`, Crop, `Crop Protectants`) %>% # this calculates outliers by protectant category
  #     mutate(
  #       IQR = IQR(get(y_input), na.rm = T),
  #       Q25 = quantile(get(y_input), 0.25, na.rm = T),
  #       Q75 = quantile(get(y_input), 0.75, na.rm = T),
  #       is_outlier = (get(y_input) < Q25 - 3*IQR | get(y_input) > Q75 + 3*IQR)
  #     ) %>%
  #     ungroup() %>% 
  #     filter(is_outlier == T) %>% 
  #     select(`Crop Year`, Crop, `Crop Protectants`, `Field Name`, all_of(y_input)) %>%
  #     rename("Number of Crop Protectants" = "sum_cp") %>% 
  #     arrange(`Crop Year`, Crop, `Crop Protectants`)
  #   
  #   if (input$fert_unmask == F) {
  #     tmp1 %>% 
  #       mutate(`Field Name` = anonymize(`Field Name`))
  #   } else {
  #     tmp1 
  #   }
  # })
  
  # Manure N Application
  # This plot gives a warning
  # Warning: Factor `Crop Year` contains implicit NA, consider using `forcats::fct_explicit_na`
  # The filtering `N from Manure` == "YES" is causing it
  # There are no implicit NAs for Crop Year, ignore for now
  manure_app <- reactive({
    validate(need(manure_crop_years(), message = "No manure applications were found"))
    
    y_input <- "sum_manure"
    
    tmp1 <- df_long() %>%
      filter(`N from Manure` == "YES") %>%
      group_by(`Crop Year`, `Field Name`, `Crop`) %>%
      summarize(sum_manure = sum(value_numeric, na.rm = T)) %>%
      drop_na() %>%
      ungroup()
    
    # Select dataset either with or without outlier based on checkbox = T/F
    #if (input$fert_hideoutlier) tmp1 <- filter(tmp1, is_outlier == F)
    # plot4 <-
    
    tmp1 %>% 
      ggplot(aes(x = `Crop Year`, y = get(y_input))) +
      #geom_boxplot(width = 0.2) +
      geom_jitter(width = 0.05)+
      labs(x = "Crop Year", y = "N from Manure (lb / acre)") +
      facet_wrap(~ Crop, scales = "free", ncol = 2) +
      ggthemes::theme_base() +
      theme(rect = element_rect(fill = "white", colour = "white"), 
            panel.border = element_rect(color = "black"),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            text = element_text(face = "bold"),
            strip.text = element_text(size = rel(1.5)))
    
    #print(plot4)
    
    # if (input$fert_unmask == F) {
    # plot4 <- tmp1 %>% 
    #     filter(`Crop Year` == input$fert_season_dropdown) %>% 
    #   ggplot(aes(x = `Field Name`, y = `sum_manure`, 
    #              text = paste("Field Name:", `Field Name`, "\n", "Value:", sum_manure))) +
    #   geom_point() +
    #   labs(y = "", x = "", title = paste("Manure N (lb / acre)", "- Season", input$fert_season_dropdown)) +
    #   facet_wrap(~ Crop, scales = "free") +
    #   theme_bw() +
    #   scale_x_discrete(labels = anonymize) +
    #   theme(axis.text.x = element_text(angle = 0, size = rel(0.75)),
    #         plot.title = element_text(size = rel(0.90))) +
    #   coord_flip()
    # } else {
    #   plot4 <- tmp1 %>% 
    #     filter(`Crop Year` == input$fert_season_dropdown) %>% 
    #     ggplot(aes(x = `Field Name`, y = `sum_manure`, 
    #                text = paste("Field Name:", `Field Name`, "\n", "Value:", sum_manure))) +
    #     geom_point() +
    #     labs(y = "", x = "", title = paste("Manure N (lb / acre)", "- All Seasons")) +
    #     facet_wrap(~ Crop, scales = "free") +
    #     theme_bw() +
    #     theme(axis.text.x = element_text(angle = 0, size = rel(0.75)),
    #           plot.title = element_text(size = rel(0.90))) +
    #     coord_flip()
    # }
    # ggplotly(plot4, height = max(length(unique(tmp1$`Field Name`))*20, 200), tooltip = "text")
  })
  
  # Plots to display in UI depending on user selection
  output$fert_plot <- renderPlot({
    # if (input$fert_options == "Fertilizers") {
    #   fert_lime_plot()
    # } else if (input$fert_options == "Lime") {
    #   fert_lime_plot()
    # } else if (input$fert_options == "Crop Protectants") {
    #   cp_plot()
    # } else if (input$fert_options == "N from Manure") {
    #   manure_app()
    # }
    ag_input_plot()
  })
  
  # Outlier table to show in UI
  # output$fert_table_outliers <- renderTable({
  #   if (input$fert_options == "Fertilizers") {
  #     fert_lime_outliers()
  #   } else if (input$fert_options == "Lime") {
  #     fert_lime_outliers()
  #   } else if (input$fert_options == "Crop Protectants") {
  #     cp_outliers()
  #   }
  # })
  
  #################################################################################################
  # Tab 6 - crop rotations
  #################################################################################################
  
  # Description to show in UI for timeline plot
  output$timeline_helptext <- renderText({
    message6
  })
  
  # Description to show in UI for fields that selected the rotation template shown
  output$rot_field_helptext <- renderText({
    message4
  })
  
  # Description to show in UI for list of operations in rotation template
  output$rot_operation_helptext <- renderText({
    message5
  })
  
  # Vector of rotations to populate menu in UI
  output$rot_dropdown <- renderUI({
    tagList(
      selectInput("rot_dropdown", "Select Rotation", choices = rot_menu())
    )
  })
  
  # For crop rotation processing
  # There are a lot of steps to this
  df_crop_rotations <- reactive({
    # Vector for filtering
    var_list <- lookup_names %>%
      filter(group %in% c("general_info", "crop_rotation")) %>%
      pull(variable)
    
    # Minor modifications before extracting each JSON rotation:
    # If rotation JSON is "Custom", we replace it with Field Name and text in `Crop Rotation System`
    rotations <- df() %>% # df() to df_prelim(), the rotation process wasn't accepting df_subset2
      select(all_of(var_list)) %>%
      mutate(`Crop Rotation System V2` = paste(`Field Name`, `Crop Rotation System`),
             `Crop Rotation Event JSON V2` = str_replace_all(string = `Crop Rotation Event JSON`,
                                                             pattern = "Custom",
                                                             replacement = `Crop Rotation System V2`))
    
    # Extracting field names as-is, don't arrange anything
    cropyears <- rotations %>% 
      select(`Crop Year`, `Grower ID`, `Field Name`) %>% 
      mutate(rot_num = row_number())
    
    # Rotation JSON extraction
    # Opening a list to receive the JSON files
    single_list <- list(0)
    
    # Extraction loop to flatten each rotation JSON 
    for (i in seq_along(rotations$`Crop Rotation Event JSON V2`)) {
      single_list[[i]] <- jsonlite::fromJSON(rotations$`Crop Rotation Event JSON V2`[[i]], flatten = T)
    }
    
    # Binding all rotations together and editing names, etc
    step_1 <- plyr::rbind.fill(single_list) %>% 
      select(system_id, code, date, operation.id, operation.name, operation.opGroup1, crop.name)
    
    # Rotation IDs can be the same and still have different operations inside
    # Using diff(code) we find when a new rotation system starts
    step_2 <- step_1 %>%
      mutate(code_num = as.numeric(code),
             code_num_diff = c(NA, diff(code_num)),
             indicator = ifelse(code_num_diff %in% c(0:10), 0, 1),
             rot_num = cumsum(indicator),
             rot_factor = paste0(system_id, " V", rot_num)) %>% 
      group_by(system_id, rot_factor) %>% 
      mutate(num_ops = n()) %>% 
      ungroup() %>% 
      select(-c(code_num:indicator))
    
    # Merging back growers to each rotation
    step_3 <- step_2 %>%
      left_join(cropyears, by = "rot_num")
    
    # We need a rotation digital fingerprint to find the different rotations grouped under the same rotation ID
    step_4 <- step_3 %>%
      group_by(system_id, rot_factor, num_ops) %>% 
      mutate(rot_print = paste(date, operation.id, collapse = "", sep = "")) %>% 
      ungroup()
  })
  
  # Getting one of each rotation digital fingerprint
  # The list of rotations should be used to extract a representative rotation of each one from step_1
  # This also gives us how many fields used each rotation
  # There is also a list of what fields used which rotation variation further down
  rot_fingerprint_list <- reactive({
    df_crop_rotations() %>% 
      group_by(rot_print, num_ops) %>% 
      summarize(rot_to_extract = head(rot_factor, n = 1), obscount = n()) %>% 
      ungroup() %>% 
      mutate(cropyear_count = obscount / num_ops)
  })
  
  # Reactive menu that collects rotation IDs
  rot_menu  <- reactive({
    sort(unique(rot_fingerprint_list()$rot_to_extract))
  })
  
  # Vector for filtering representative rotations for final dataset for plotting
  representative_rotation_vector <- reactive({
    rot_fingerprint_list()$rot_to_extract
  })
  
  # Representative rotation list for plotting graphs and for the table of operations
  # This gets the name cleaning
  representative_rotation_list <- reactive({
    df_crop_rotations() %>% 
      filter(rot_factor %in% representative_rotation_vector()) %>% 
      mutate(date = as.Date(date, format = "%Y-%m-%d"),
             year = lubridate::year(date)) %>% 
      rename(`Rotation ID` = system_id,
             Year = year,
             `Operation Date` = date,
             `Operation Group` = operation.opGroup1,
             `Operation Description` = operation.name,
             `Crop Planted` = crop.name)
  })
  
  # What crop years used which rotations
  # Using rot_fingerprint_list to merge back to get 1 rotation name
  rot_variation_crop_year <- reactive({
    rot_fingerprint_list_V2 <- rot_fingerprint_list() %>% 
      select(rot_print, rot_to_extract)
    
    # This gives us rotation variation uniformity 
    step_5 <- df_crop_rotations() %>%
      left_join(rot_fingerprint_list_V2, by = "rot_print")
    
    # This is the table for showing what rotations were used in each crop year
    rot_variation_crop_year <- step_5 %>% 
      group_by(`Crop Year`, `Grower ID`, `Field Name`, system_id, rot_to_extract) %>%
      summarize(header = head(`Field Name`, n = 1)) %>%
      ungroup() %>% 
      group_by(`Grower ID`, `Field Name`, system_id, rot_to_extract) %>%
      mutate(collapsed_crop_years = paste(`Crop Year`, collapse = ", ")) %>% 
      ungroup() %>%
      select(system_id, rot_to_extract, `Grower ID`, `Field Name`, collapsed_crop_years) %>% 
      distinct() %>% 
      rename(`Rotation ID` = system_id,
             `Rotation Template` = rot_to_extract,
             `Crop Years` = collapsed_crop_years)
  })
  
  # For crop rotation graphs
  df_crop_rotations_graph <- reactive({
    rotation_timeline <- representative_rotation_list() %>%
      filter(rot_factor == input$rot_dropdown) %>%
      mutate(events = gsub("([A-Za-z]+).*", "\\1", `Operation Description`),
             planted_crop = gsub("([A-Za-z]+).*", "\\1", `Crop Planted`),
             events = paste(events, planted_crop, sep = " "), # add "\n" here for new line
             events = str_replace_all(string = events,
                                      pattern = " NA",
                                      replacement = ""),
             events2 = gsub("([A-Za-z]+).*", "\\1", `Operation Description`))
    
    descriptions1 <- rotation_timeline %>% 
      distinct(events2, events)
    
    # This prevents row_number increasing indefinitely with each rotation year
    descriptions2 <- descriptions1 %>%
      distinct(events2) %>% 
      mutate(length = row_number())
    
    descriptions3 <- descriptions1 %>% 
      left_join(descriptions2, by = "events2") %>% 
      select(-1)
    
    rotation_timeline <- rotation_timeline %>%
      left_join(descriptions3, by = "events2") # changed here to events2 on 10-29-2020
  })
  
  # Popup rotation table to show in UI
  output$searchable_rot_table <- DT::renderDT(
    rot_variation_crop_year() %>%
      select(-`Rotation ID`), filter = "top", server = F,
    options = list(lengthChange = T, pageLength = 25, lengthMenu = c(25, 50, 75))
  )
  
  # Rotation freq count to show in UI
  output$rotation_count <- renderTable({
    rot_fingerprint_list() %>% 
      select(rot_to_extract, cropyear_count) %>%
      arrange(desc(cropyear_count)) %>%
      mutate(cropyear_count = as.character(cropyear_count)) %>% 
      rename("Rotation Template" = 1,
             "Crop Year Count" = 2)
  })
  
  # Table of rotations by field to show in UI
  output$rot_by_fields <- function() {
    tmp1 <- subset(rot_variation_crop_year(), `Rotation Template` == input$rot_dropdown)
    
    if (input$rotation_field_unmask == F) {
      tmp1 %>%
        select(-`Rotation ID`) %>%
        arrange(`Grower ID`) %>%
        mutate(`Field Name` = anonymize(`Field Name`)) %>% 
        kable("html") %>%
        kable_styling(full_width = F, position = "left") %>%
        collapse_rows(columns = 1:2, valign = "top")
    } else {
      tmp1 %>%
        select(-`Rotation ID`) %>%
        arrange(`Grower ID`) %>%
        kable("html") %>%
        kable_styling(full_width = F, position = "left") %>%
        collapse_rows(columns = 1:2, valign = "top")
    }
  }
  
  # Table of operations within each rotation to show in UI
  output$rotation_table <- function(){
    options(knitr.kable.NA = "") # To omit printing NAs in the output table
    
    representative_rotation_list() %>%
      filter(rot_factor == input$rot_dropdown) %>% 
      select(rot_factor, Year,`Operation Date`, `Operation Group`, `Operation Description`, `Crop Planted`) %>% 
      rename("Rotation Template" = "rot_factor") %>% 
      arrange(Year,`Operation Date`) %>% 
      knitr::kable("html") %>%
      kable_styling(full_width = T) %>%
      collapse_rows(columns = 1, valign = "top")
  }
  
  # Timeline graph for each rotation to show in UI
  output$rotation_graphs <- renderPlotly({
    # We first need some processing to be able to add a background color that changes with rotation year
    year_vector <- unique(df_crop_rotations_graph()$Year)
    min_year_vector <- min(year_vector)
    max_year_vector <- max(year_vector)
    
    # Color function
    color_palette <- if (length(year_vector) < 6) {
      c("orange", "blue", "red", "purple", "gray", "yellow")
    } else {sample(colors(), size = length(year_vector))}
    
    # EXtracting some vectors for min, max dates, and max Y-axis value
    min_date <- min(df_crop_rotations_graph()$`Operation Date`)
    max_date <- max(df_crop_rotations_graph()$`Operation Date`)
    max_y <- max(df_crop_rotations_graph()$length) + 1
    
    # Processing needed to assign background colors by rotation year
    background_step1 <- data.frame()
    
    for (i in (min_year_vector:max_year_vector)) {
      Year <- i
      xmin <- (paste0("00", i, "-01", "-01"))
      xmax <- (paste0("00", i, "-12", "-31"))
      fill <- color_palette[i]
      
      background_step0 <- (cbind(Year, xmin, xmax, fill)) 
      background_step1 <- rbind(background_step1, background_step0)
    }
    
    background_step2 <- background_step1 %>% 
      mutate(xmin = as.Date(xmin, origin = "1970-01-01"),
             xmax = as.Date(xmax, origin = "1970-01-01"),
             ymin = 0,
             ymax = max_y,
             Year = as.numeric(as.character(Year)))
    
    background_step3 <- background_step2 %>% 
      mutate(xmin = ifelse(test = (Year == min(year_vector)),
                           yes = min_date - 15,
                           no = xmin),
             xmax = ifelse(test = (Year == max(year_vector)),
                           yes = max_date + 15,
                           no = xmax)) %>%
      mutate(xmin = as.Date(xmin, origin = "1970-01-01"),
             xmax = as.Date(xmax, origin = "1970-01-01"))
    
    year_label_vector <- background_step3 %>% 
      mutate(half.year = xmin + floor((xmax - xmin)/2),
             year_label = paste("Year", Year))
    
    # Graph to show in UI
    plot_rot <- df_crop_rotations_graph() %>%
      rename("Rotation Template" = "rot_factor") %>% 
      ggplot(aes(x = `Operation Date`, y = 0, label = events, color = events,
                 text = paste("Date", `Operation Date`, "\n",
                              `Operation Description`))) +
      geom_rect(data = background_step3, inherit.aes = FALSE,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.6),
                fill = background_step3$fill, alpha = 0.2) +
      geom_hline(yintercept = 0, color = "black", size = 0.3) +
      geom_segment(aes(y = length, yend = 0, xend = `Operation Date`), color = 'black', size = 0.2) +
      geom_point(aes(y = 0, size = 3)) +
      geom_text(aes(y = length + 0.25, label = events), size = 4) +
      scale_x_date(labels = date_format("%b"), date_breaks = "1 month", #date_format("Year %y %b")
                   expand = expansion(add = c(15, 15))) +
      labs(title = paste("Crop Rotation Template:", input$rot_dropdown)) +
      theme_classic() +
      theme(axis.line.y=element_blank(),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x =element_text(angle = 0, size = rel(1.2)),
            axis.ticks.x =element_blank(),
            axis.line.x =element_blank(),
            legend.position = "none") +
      annotate(geom = "text", x = year_label_vector$half.year,
               y = max_y, 
               label = year_label_vector$year_label, 
               size = 5, colour = "black")
    
    ggplotly(plot_rot, tooltip = "text")
  })
  
  #################################################################################################
  # Tab 7 - agronomic summaries
  #################################################################################################
  
  # Description to show in UI
  output$ag_summaries_text <- renderText({
    message24
  })
  
  # Description to show in UI that changes with user selection
  output$helptext_by_table <- renderText({
    if (input$selecttable == "Yield") {
      message26
    } else if (input$selecttable == "Fertilizers") {
      message27
    } else if (input$selecttable == "Crop Protectants") {
      message28
    } else if (input$selecttable == "Field Crop Sequence") {
      message29
    } else if (input$selecttable == "Grower Participation") {
      message30
    }
  })
  
  # Repeated fields processing (this doesn't reflect if a grower keeps participating)
  field_usage_count <- reactive({
    field_usage_count <- df() %>%
      group_by(`Field Name`) %>% 
      count() %>% 
      arrange(desc(n))
  })
  
  repeated_fields <- reactive({
    repeated_fields <- field_usage_count() %>% 
      rename("Total" = n)
  })
  
  repeated_field_usage_summary <- function(){
    options(knitr.kable.NA = "")
    
    # A field cannot have >1 counter per year
    remove_error_fields <- df() %>%
      group_by(`Crop Year`, `Field Name`) %>% 
      count() %>% 
      filter(n > 1) %>% 
      pull(`Field Name`)
    
    df() %>%
      filter(!(`Field Name` %in% remove_error_fields)) %>% 
      mutate(cropyear = paste0("Crop\nYear", "\n", `Crop Year`)) %>% 
      group_by(cropyear, `Field Name`) %>% 
      count(Crop) %>% 
      ungroup() %>% 
      arrange(desc(n)) %>% 
      mutate(Crop = ifelse(test = is.na(Crop),
                           yes = cell_spec(Crop, background = "red", align = "center",
                                           background_as_tile = T, bold = T, color = "green"),
                           no = cell_spec(Crop, background = "green", align = "center",
                                          background_as_tile = T, bold = T, color = "white"))) %>%
      spread(key = cropyear, value = Crop) %>% 
      left_join(repeated_fields(), by = "Field Name") %>% 
      arrange(desc(Total)) %>% 
      rename("Total Crop Years" = "Total") %>% 
      select(-n) %>% 
      kable(format = "html", escape = F, align = "c") %>%
      kable_styling("striped", full_width = T)
  }
  
  # Repeated growers processing (this is relevant if Grower ID is meaningful)
  grower_usage_count <- reactive({
    field_usage_count <- df() %>% 
      group_by(`Grower ID`) %>% 
      count() %>% 
      arrange(desc(n))
  })
  
  repeated_growers <- reactive({
    repeated_fields <- grower_usage_count() %>% 
      rename("Total" = n)
  })
  
  repeated_growerid_summary <- function(){
    options(knitr.kable.NA = "")
    
    df() %>%
      mutate(cropyear = paste0("Crop\nYear", "\n", `Crop Year`)) %>% 
      group_by(cropyear, `Grower ID`) %>% 
      count() %>% 
      ungroup() %>% 
      arrange(desc(n)) %>% 
      mutate(n = ifelse(test = n > 0,
                        yes = cell_spec(n, background = "green", align = "center",
                                        background_as_tile = T, bold = T, color = "white"),
                        no = cell_spec(n, background = "red", align = "center",
                                       background_as_tile = T, color = "red"))) %>%
      spread(key = cropyear, value = n) %>% 
      left_join(repeated_growers(), by = "Grower ID") %>% 
      arrange(desc(Total)) %>% 
      rename("Total Crop Years" = "Total") %>% 
      kable(format = "html", escape = F, align = "c") %>%
      kable_styling("striped", full_width = T)
  }
  
  # Summary tables to show in UI
  output$summary_table_output <- function() {
    if (input$selecttable == "Yield") {
      df() %>%
        group_by(`Crop Year`, Crop) %>%
        summarize(`Yield` = round(mean(`Adjusted Yield`), 1),
                  `Number of Fields` = n()) %>%
        knitr::kable("html", align = "c") %>%
        kable_styling(full_width = T)
      
    } else if (input$selecttable == "Fertilizers") {
      df() %>%
        group_by(`Crop Year`, Crop) %>%
        summarize(`Nitrogen` = round(mean(`Total N (lb / acre) for All Trips`), 1),
                  `Phosphorus` = round(mean(`Total P2O5 (lb / acre) for All Trips`), 1),
                  `Potassium` = round(mean(`Total K2O (lb / acre) for All Trips`), 1)) %>%
        knitr::kable("html", align = "c") %>%
        kable_styling(full_width = T) %>%
        add_header_above(c(" ", " ","Fertilizer Rates (lb/acre)" = 3))
    } else if (input$selecttable == "Crop Protectants") {
      df() %>%
        group_by(`Crop Year`, Crop) %>%
        summarize(`Herbicides` = round(mean(`Total Herbicides for All Trips`), 1),
                  `Insecticides` = round(mean(`Total Insecticides for All Trips`), 1),
                  `Fungicides` = round(mean(`Total Fungicides for All Trips`), 1),
                  `Growth Regulators` = round(mean(`Total Growth Regulators for All Trips`), 1),
                  `Fumigant` = round(mean(`Total Fumigant for All Trips`), 1),
                  `Harvest Aids` = round(mean(`Total Harvest Aids for All Trips`), 1),
                    ) %>%
        ungroup() %>%
        # drop_na() %>%
        # group_by(`Crop Year`, `Crop`, `Crop Protectants`) %>%
        # summarize(mean_apps = round(mean(sum_apps, na.rm = T), 1)) %>%
        # spread(key = `Crop Protectants`, value = mean_apps) %>%
        knitr::kable("html", align = "c") %>%
        kable_styling(full_width = T)
    } else if (input$selecttable == "Field Crop Sequence") {
      repeated_field_usage_summary()
    } else if (input$selecttable == "Grower Participation") {
      repeated_growerid_summary()
    }
  }
  
  #################################################################################################
  # Tab 8 - sandbox
  #################################################################################################
  
  # Description to show in UI
  output$sandbox_help_text <- renderText(
    message7
  )
  
  # Vector that populates dropdown menu in UI
  observe({
    updateSelectInput(session, "sandbox_x_dropdown",
                      choices = allvarsmenu(),
                      selected = "Adjusted Yield"
    )
  })
  
  # Vector that populates dropdown menu in UI
  observe({
    updateSelectInput(session, "sandbox_y_dropdown",
                      choices = allvarsmenu(),
                      selected = "Energy Use Score"
    )
  })
  
  # Popup table to display in UI, it shows the same data selected by the user
  output$sandbox_popup_table <- DT::renderDT(
    df() %>%
      select(`Crop Year`, Crop, `Field Name`, 
             all_of(input$sandbox_x_dropdown), all_of(input$sandbox_grouping_var),
             all_of(input$sandbox_y_dropdown)),
    filter = "top", server = F, 
    options = list(lengthChange = T, pageLength = 25, lengthMenu = c(25, 50, 75))
  )
  
  # Plot to display in UI
  output$sandbox_plot <- renderPlot({
    y_input <- input$sandbox_y_dropdown
    
    # Adding outlier parameters
    # IQR limit of 2 to be more stringent
    data1 <- df() %>%
      select(all_of(input$sandbox_x_dropdown), all_of(input$sandbox_y_dropdown),
             all_of(input$sandbox_grouping_var), `Crop Year`, Crop) %>%
      group_by(Crop) %>%
      mutate(
        IQR = IQR(get(y_input), na.rm = T),
        Q25 = quantile(get(y_input), 0.25, na.rm = T),
        Q75 = quantile(get(y_input), 0.75, na.rm = T),
        is_outlier = (get(y_input) < Q25 - (2 * IQR) | get(y_input) > Q75 + (2 * IQR))
      ) %>% 
      ungroup()
    
    # Select dataset either with or without outlier based on checkbox = T/F
    if (input$sandbox_hideoutlier) data1 <- filter(data1, is_outlier == F)
    
    data1 %>%
      ggplot(aes(x = get(input$sandbox_x_dropdown), y = get(input$sandbox_y_dropdown),
                 color = as.factor(get(input$sandbox_grouping_var)))) + #,
      #text = paste0(input$sandbox_x_dropdown, ": ", get(input$sandbox_x_dropdown), "\n",
      #               input$sandbox_y_dropdown, ": ", get(input$sandbox_y_dropdown)))) +
      #geom_point() +
      geom_jitter(width = 0.05, size = 2) +
      labs(y = paste(stringr::str_wrap(input$sandbox_y_dropdown, width = 20)), 
           x = paste(input$sandbox_x_dropdown), color = paste(input$sandbox_grouping_var)) + #,
      #     title = paste("X-axis Variable:", input$sandbox_x_dropdown, "|",
      #                   "Y-axis Variable:", input$sandbox_y_dropdown)) +
      #theme(axis.text.x = element_text(angle = 90, size = rel(.75)),
      #      panel.grid.minor = element_blank(),
      #      plot.title = element_text(size = rel(0.9))) +
      facet_wrap(~ Crop, scales = "free", ncol = 2) +
      ggthemes::theme_base() +
      theme(rect = element_rect(fill = "white", colour = "white"), 
            panel.border = element_rect(color = "black"),
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
            text = element_text(face = "bold"),
            strip.text = element_text(size = rel(1.5)))
    
    # if (input$sandbox_hidetrend) {
    #   #plot5 <-
    #    
    # } else {
    #   # plot5 <-
    #    data1 %>%
    #     ggplot(aes(x = get(input$sandbox_x_dropdown), y = get(input$sandbox_y_dropdown),
    #                color = as.factor(get(input$sandbox_grouping_var)),
    #                text = paste0(input$sandbox_x_dropdown, ": ", get(input$sandbox_x_dropdown), "\n",
    #                             input$sandbox_y_dropdown, ": ", get(input$sandbox_y_dropdown)))) +
    #     geom_point() +
    #     #stat_summary(fun.y = "mean", geom = "line", color = "black", size = 1 , 
    #     #             aes(group = factor(`Crop`))) +
    #     labs(y = "", x = "", color = "",
    #          title = paste("X-axis Variable:", input$sandbox_x_dropdown, "|",
    #                        "Y-axis Variable:", input$sandbox_y_dropdown)) +
    #     theme_bw() +
    #     theme(axis.text.x = element_text(angle = 90, size = rel(.75)),
    #           panel.grid.minor = element_blank(),
    #           plot.title = element_text(size = rel(0.9))) +
    #     facet_wrap(~ Crop, scales = "free") +
    #     geom_smooth(method = "lm")
    # }
    #ggplotly(plot5, tooltip = "text") #height = max(length(unique(data1$get(y_input)))*20, 200) 
  })
  
  #################################################################################################
  # Tab 9 - build pivot chart
  #################################################################################################
  
  # Description to show in UI
  output$pivot_help_text <- renderText(
    message8
  )
  
  # Pivot table function
  output$pivot <- rpivotTable::renderRpivotTable({
    # Removing application trip variables and Field GeoJSON
    df <- df() %>% 
      select(!starts_with("Application Trip")) %>% 
      select(-c(`Field GeoJSON`, `Crop Rotation Event JSON`))
    
    rpivotTable::rpivotTable(df, rows = "Crop Year",
                             col = "Crop",
                             aggregatorName = "Average",
                             vals = "Adjusted Yield",
                             rendererName="Table")
  })
  
  #################################################################################################
  # Tab 10 - enhanced table search
  #################################################################################################
  
  # Description to show in UI
  output$enhanced_table_help_text <- renderText(
    message1
  )
  
  # This creates groups of variables to select
  select_filter <- reactive({
    tmp1 <- data.frame(variable = names(df())) %>% 
      left_join(lookup_names, by = "variable") %>% 
      drop_na()
    
    tmp1 %>%
      filter(!(variable %in% c("Field GeoJSON", "Crop Rotation Event JSON"))) %>% 
      filter(!(group %in% "crop_rotation")) %>% 
      filter(label %in% c(input$table_filtering)) %>% 
      pull(variable) %>% 
      unique()
  })
  
  # Table to display in UI that gets filtered by select_filter
  output$enhanced_table_search <- DT::renderDT(
    df() %>%
      select(select_filter()),
    filter = "top", server = F, class = 'cell-border stripe',
    options = list(lengthChange = T, pageLength = 5, lengthMenu = c(5, 15, 25), autoWidth = TRUE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#8EBDDC', 'color': '#fff'});",
                     "}"))
  )
}
