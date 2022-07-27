library(shiny)
library(shinyBS)
library(shinyalert)
library(shinyWidgets)
library(shinyLP)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)
library(webshot)
library(kableExtra)
library(plotly)
library(scales)

webshot::install_phantomjs() # needed to download map image

options(scipen = 999) # to prevent R from using scientific notation

# Look-up table needed for subsetting
lookup_names <- read_csv("lookup_names.csv")

# Help text
message1 <- "Here you can search the raw comprehensive data output file with enhanced filtering and column selection by groups. Choose the group of columns that are of interest to you from the dropdown menu below. You can sort by values or add text searches above each column to find fields or targeted inputs."

message2 <- "Use these plots to identify any outlier values that are significantly higher or lower than other fields. Outliers may indicate that an error was made during data entry."

message3 <- "When applied, lime can have a large impact on energy use and GHG scores. Lime application rates larger than 10-15 short ton/acre may be incorrect (a short ton is equivalent to 2,000 lb)."

message4 <- "List of fields using the crop rotation selected"

message5 <- "List of operations for the crop rotation selected"

message6 <- "Timeline of operations for the crop rotation selected. Plot background color changes by rotation year"

message7 <- "A scatter plot allows you to visualize the relationship between any two variables from the comprehensive data output file. Some variables might not conform to plotting. The Color Variable dropdown menu has been limited to variables related to field information. Click the checkbox to remove outliers from the current plot."

message8 <- "Here you can create a pivot table of your choice. Nearly all variables from the comprehensive data output file are available to you. Drag items from the leftmost list to the row grouping section (the middle section) or column grouping section (right above the output table) as desired. Above the row grouping section there are two dropdown menus. The top dropdown menu displays various summary functions, such as Count, Sum, Average, Maximums, etc. Below the function menu you can select a variable to summarize."

#message9 <- "In this space you can build your own scatter plots and pivot tables."

message10 <- 'Here you can visualize the total amounts of N-P-K applied during the crop year as calculated from the sum of all your fertilization trips. The nitrogen applied does not include nitrogen from manure. To see how much nitrogen was applied from manure, please select the option "N from Manure".'

message11 <- "This graph displays the total number of applied crop protectants during each crop year."

message12 <- "Please upload an unmodified Fieldprint comprehensive data output file."

message13 <- "Use this map to check field locations and boundaries to identify if any errors were made in the field delineation step. You can zoom in and out to see reference roads and other landmarks. The Download button located below the map will save an image of the current map view. Clicking the checkbox will make field names visible. You can also see field names by hovering your mouse pointer over a field. There are several basemaps available, explore the options to find the most appropriate basemap for your needs. Please do not print or share maps with visible field names to protect farmers' privacy."

message14 <- "Field to Market's Quality Analysis (QA) tool will help you identify outliers, find errors, and gain insights about your Continuous Improvement Project"

message15 <- "The Soil Carbon Metric is measured using the USDA NRCS Soil Conditioning Index. Scores ranges from -1 to +1. Positive values (> 0.05) indicate that soil carbon is increasing. As the value approaches +1, the confidence that there is a gain in soil carbon increases. Inversely, as the value approaches -1, the confidence increases that you are losing soil carbon."

message16 <- "The Soil Conservation metric is a measure of soil lost to erosion from water and wind and reported as tons of soil lost per acre. Lower numbers are desirable."

message17 <- "Irrigation Water Use is expressed as the amount of water, measured in acre inches, required to produce a unit of yield (pound, bushel, etc.). The metric is calculated using the difference irrigation contributes to yield improvement."

message18 <- "Biodiversity is assessed using the Habitat Potential Index (HPI). HPI scores the potential for
a farm to provide wildlife habitat on land or in water. HPI scores range from 0-100 and measure the
level of opportunity to improve or maximize habitat potential. Higher scores are desirable and indicate a greater potential to support wildlife habitat. Scores less than 50% represent significant opportunities for improving
habitat potential, whereas values of 50-80% indicate moderate realized potential and scores greater than 80%
demonstrate farms that have maximized opportunities for biodiversity to flourish."

message19 <- "The Fieldprint Platform measures all the energy required to produce a crop, from pre-plant to first point of sale or delivery at the processing facility. This includes direct energy used for operating equipment, pumping irrigation water, grain drying and transport as well as embedded energy, which is required to produce crop inputs like seeds, fertilizers and crop protectants. Energy use is expressed as British thermal units (BTU) per unit of crop production (bushel, pounds, etc.) or per acre."

message20 <- "Greenhouse gas emissions are reported as pounds of carbon dioxide equivalent (CO2e) per crop unit produced (bushels, pounds, etc.). CO2e means all other emission sources are converted to the equivalent amount of CO2. CO2 emissions also result from electricity and fuel usage as well as from burning crop residues. The Fieldprint Platform uses data on crop type, region, and soil texture to determine how much nitrous oxide (N2O) results from additions of nitrogen (N). Methane is only calculated for rice, and emissions are based on region of the country. To calculate CH4 emissions, the Fieldprint Platform evaluates a farmer's responses to questions about water management, organic and fertilizer amendments and other management practices."

message21 <- "Land use efficiency is a measure of the amount of land (in acres) used to produce a unit of crop (bushels, pounds, etc.). For example, in sorghum, land use is measured in acres/bushel, while in cotton as acres/pound of lint. This is an inverse of yield measures, which are expressed as bushels per acre or pounds of lint per acre."

message22 <- "The Fieldprint Platform uses the Stewardship Tool for Environmental Performance (STEP) from USDA NRCS to indirectly measure the quality of water leaving a farm field. The Water Quality Score measures the number of nutrient loss pathways which have been mitigated. Scores range from 0 (no pathways mitigated) to 4 (all pathways mitigated); high scores are desirable. You can visualize the Water Quality Score along with all of its components to learn where scores are lower and find opportunities for improvement."

message23 <- "This plot shows you how much N from manure (in units of lb. N / acre) was applied during the crop year. If more than one manure application was conducted, the N rates were added together."

message24 <- "These tables summarize the average input values by crop and year and are provided for use in quality analysis of data entry."

outlier_helptip <- "An outlier is an observation found outside the overall pattern of a distribution. The presence of an outlier might indicate a problem, such as an error in data entry."

message25 <- "The fields in the table below (if any) have been identified as extreme outliers for the selected metric score for a given crop and year. Please use your best judgement to determine if a review of the input data is warranted. Observations are classified as extreme outliers when their metric score or parameter value exceeds the interquartile range by a factor of three."

message26 <- "This is a table of average yield by crop and crop year."

message27 <- "This is a table of average N-P-K rates by crop and crop year. Nitrogen from manure is not included in the summary table."

message28 <- "This is a table of average number of crop protectants applied by crop, crop year, and protectant category."

message29 <- "This table shows the crop history for each field and crop year."

message30 <- "This table shows how many crop years were entered by each Grower ID during a given growing season. Ideally, Grower ID would list the producers that managed their respective fields entered for the duration of the project. Knowing which fields are/were managed by each producer will likely enhance the quality of a statistical analysis. If the project is ongoing, it could also help to tailor educational materials and interventions related to the objective of the project."

message31 <- "Return here at any time to include or filter out data by Crop, Crop Year, and/or Field Name. The Field Name shown here is a combination of Crop Year, Grower ID, and Field Name to avoid issues with duplication. By default, all project data are included from the start. When Crops and/or Crop Years are included or filtered out, the list of Field Names and the table with Crop Year counts will update accordingly. The website will fail if all items for Crop, Crop Year, or Field Name are filtered out. The data filtering only applies to the current website session and it does not modify the uploaded Comprehensive Data Output File."

# Main metrics vector
main_metrics <- c("Biodiversity", "Energy Use", "Greenhouse Gas Emissions", "Irrigation Water Use", "Land Use", "Soil Carbon", "Soil Conservation", "Water Quality")

# Fertilizer and crop protectant options
fert_options <- c("Fertilizers", "Crop Protectants", "Lime", "N from Manure")

# Crop rotation options
options_crop_rotation <- c("Crop Rotations by Field", "Crop Rotation Operations", "Crop Rotation Timeline")

# Vectors needed to fill dropdown menus for Energy Use, GHG Emissions, WQI, and BYO
energyuse_btu_acre <- lookup_names %>% 
  filter(group %in% c("energy_components")) %>% 
  filter(str_detect(variable, pattern = "btu / acre")) %>% 
  pull(variable)

energyuse_btu_yield <- lookup_names %>% 
  filter(group %in% c("energy_components")) %>% 
  filter(str_detect(variable, pattern = "btu / yield units")) %>% 
  arrange(variable) %>% 
  pull(variable)
  

energyuse_btu_yield <- c("Energy Use Score", energyuse_btu_yield)

ghg_btu_acre <- lookup_names %>% 
  filter(group %in% c("ghg_components")) %>% 
  filter(str_detect(variable, pattern = "lbs CO2e / acre")) %>% 
  pull(variable)

ghg_btu_yield <- lookup_names %>% 
  filter(group %in% c("ghg_components")) %>% 
  filter(str_detect(variable, pattern = "lbs CO2e / yield units")) %>% 
  arrange(variable) %>% 
  pull(variable)

ghg_btu_yield <- c("GHG Score", ghg_btu_yield)

wqi_components <- c("Water Quality Score",
                    "Surface Phosphorus Pathway Ratio",
                    "Subsurface Phosphorus Pathway Ratio",
                    "Surface Nitrogen Pathway Ratio",
                    "Subsurface Nitrogen Pathway Ratio")

grouping_choices_byo <- lookup_names %>% 
  filter(group %in% "general_info") %>% 
  pull(variable)

enhanced_table_options <- sort(unique(lookup_names$label))
enhanced_table_options <- enhanced_table_options[enhanced_table_options != "Crop Rotation"]

# Providers for basemaps
basemap_providers <- sort(c("OpenStreetMap", "OpenTopoMap", "Stamen.TonerLite",
                            "Esri", "Esri.WorldImagery", "Stamen.TopOSMFeatures",
                            "CartoDB", "Stamen.TonerHybrid", "Stamen"))

# Map functions
# For boundaries
bound_function <- function(df, year_subset, crop_subset, basemap, show_field_names, mapcolors, bounding_box) {
  df %>%
    filter(crop_year %in% c(year_subset) & crop %in% c(crop_subset)) %>% 
    leaflet() %>% 
    addProviderTiles(basemap) %>% 
    addScaleBar() %>% 
    addPolygons(color = "black", fill = T, fillColor = mapcolors, weight = 1, fillOpacity = 1, 
                label = ~as.character(field_name),
                labelOptions = labelOptions(noHide = show_field_names, direction = "top", textOnly = T)) %>% 
    fitBounds(bounding_box[1], bounding_box[2], bounding_box[3], bounding_box[4])
}

# For centroids
centroid_function <- function(df, year_subset, crop_subset, basemap, show_field_names, mapcolors, bounding_box) {
  df %>%
    filter(crop_year %in% c(year_subset) & crop %in% c(crop_subset)) %>% 
    leaflet() %>% 
    addProviderTiles(basemap) %>% 
    addScaleBar() %>%
    addCircleMarkers(radius = 5,
                     color = mapcolors,
                     stroke = FALSE, 
                     fillOpacity = 1,
                     label = ~as.character(field_name),
                     labelOptions = labelOptions(noHide = show_field_names, direction = "top", textOnly = T)) %>%
    fitBounds(bounding_box[1], bounding_box[2], bounding_box[3], bounding_box[4])
}

# Masking function for field names
anonymize <- function(x, algo = "crc32"){
  unq_hashes <- vapply(unique(x), function(object) digest::digest(object, algo = algo), 
                       FUN.VALUE = "", USE.NAMES = TRUE)
  unname(unq_hashes[x])
}

# Options for summary tables
table_options <- c("Yield", "Fertilizers", "Crop Protectants", "Field Crop Sequence", "Grower Participation")

# Options for crop protectants
chemical_options <- c("Total Herbicides for All Trips",
                      "Total Insecticides for All Trips",
                      "Total Fungicides for All Trips",
                      "Total Growth Regulators for All Trips",
                      "Total Fumigant for All Trips",
                      "Total Harvest Aids for All Trips")

