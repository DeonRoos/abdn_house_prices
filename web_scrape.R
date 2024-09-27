# Script to fetch house details from ASPC site ----------------------------
# Uses advice from David (Rosie's partner) to get the info:

# Packages ----------------------------------------------------------------
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(stringr)
library(purrr)

# URL ---------------------------------------------------------------------
url <- "https://api.aspc.co.uk/Property/GetProperties?PrimaryPropertyType=Buy&SortBy=PublishedDesc&LastUpdated=AddedAnytime&PropertyType=Residential&HasGarage=false&HasDoubleGarage=false&HasGarden=false&IsNewBuild=false&HasGreenEnergy=false&HasOnStreetParking=false&HasOffStreetParking=false&HasElectricVehicleChargePoint=false&HasGrannyFlat=false&IsSelfContained=false&HasGroundFloorBedroomBathroom=false&IsLowCostHome=false&IsDevelopmentOpportunity=false&HasLand=false&FloorAreaMin=&ExcludeUnderOffer=false&IncludeClosedProperties=true&ClosedDatesSearch=14&ByFixedPriceOnly=false&ResultMode=NONE&ResultView=LIST&search-origin=search-results&Sort=PublishedDesc&Page=1&PageSize=12"

# Helper functions --------------------------------------------------------

extract_coordinates <- function(wkt) {
  coords <- gsub("POINT \\(|\\)", "", wkt)
  lon_lat <- as.numeric(unlist(strsplit(coords, " ")))
  return(lon_lat)
}

convert_to_utm <- function(lon_lat) {
  point <- st_sfc(st_point(lon_lat), crs = 4326)  # WGS 84
  utm_point <- st_transform(point, crs = 32630)  # UTM zone 30N
  return(st_coordinates(utm_point))
}

fetch_properties <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    properties_df <- as.data.frame(data)
    return(properties_df)
  } else {
    print(paste("Request failed with status code:", status_code(response)))
    return(NULL)
  }
}

# Load previous properties ------------------------------------------------

file_path <- "property_data.csv"

if (file.exists(file_path)) {
  existing_data <- read.csv(file_path, stringsAsFactors = FALSE)
} else {
  existing_data <- data.frame()
}

existing_data$DateAdded <- as.Date(existing_data$DateAdded)

# Scraping ----------------------------------------------------------------
all_properties <- list()

n_props <- 500  # Number of properties currently listed

# For loop to go through each page
for (i in 1:ceiling(n_props / 12)) {  # divide by 12 given 12 properties shown per page
  print(paste("Fetching page:", i))
  updated_url <- gsub("Page=1", paste0("Page=", i), url)
  properties_df <- fetch_properties(updated_url)
  
  if (!is.null(properties_df)) {
    properties_df$DateAdded <- Sys.Date()  # Add DateAdded to new properties
    
    # Store the fetched properties into the all_properties list
    all_properties[[length(all_properties) + 1]] <- properties_df
  }
  # Short pause to prevent 404
  Sys.sleep(1)
}

# Combine all properties into one data frame --------------------------------
if (length(all_properties) > 0) {
  new_properties_df <- bind_rows(all_properties)
} else {
  new_properties_df <- data.frame()  # Ensure it is a data frame even if no properties found
}

# Process new properties only if there are any new ones
if (nrow(new_properties_df) > 0) {
  
  # Remove photos and sort out SQL nested dataframes
  new_properties_df <- new_properties_df |>
    select(-Photos) |>
    unnest(Location) |>  # Unnest the 'Location' column
    unnest(Spatial) |> 
    unnest(Geography) |> 
    unnest(SolicitorAccount, names_sep = "_")
  
  # Processing new properties
  new_properties_df <- new_properties_df |>
    mutate(
      # Check if "Garden" is mentioned
      has_garden = ifelse(str_detect(CategorisationDescription, "Garden"), "Yes", "No"),
      
      # Extract the Council Tax Band (CT Band) value
      council_tax_band = str_extract(CategorisationDescription, "(?i)CT\\s*band\\s*-\\s*([A-Z])") |>
        str_extract("[A-Z]$"),  # Extract the letter just before the closing parenthesis
      council_tax_band = ifelse(council_tax_band == "T", NA, council_tax_band),
      
      # Extract the EPC Band value
      epc_band = str_extract(CategorisationDescription, "(?i)EPC\\s*band\\s*-\\s*([A-Z])") |>
        str_extract("[A-Z]$"),  # Extract the letter just before the closing parenthesis
      epc_band = ifelse(epc_band == "T", NA, epc_band),
      
      # Count the number of floors based on keywords (assuming max floors is 2)
      num_floors = case_when(
        str_detect(CategorisationDescription, "Ground flr") & str_detect(CategorisationDescription, "1st flr") ~ 2,
        str_detect(CategorisationDescription, "Ground flr") ~ 1,
        TRUE ~ 1  # Default to 1 if no floor indicators are found
      ),
      
      # Check if Parking is mentioned
      parking_type = case_when(
        str_detect(CategorisationDescription, "Double Garage") ~ "Double Garage",
        str_detect(CategorisationDescription, "Garage") ~ "Garage",
        str_detect(CategorisationDescription, "Parking") ~ "Parking",
        TRUE ~ "No parking"  # Default if none are found
      ),
      
      # Tidy house type term
      HouseType = case_when(
        HouseFormat == 1 ~ "Detached",
        HouseFormat == 2 ~ "Semi-Detached",
        HouseFormat == 3 ~ "Terraced",
        TRUE ~ NA_character_  # In case of unexpected values
      ),
      
      # Ensure DateAdded is of date type
      DateAdded = as.Date(DateAdded)  # Convert to Date type
    )
  
  # Extract coordinates from WellKnownText and convert to UTM
  new_properties_df <- new_properties_df |>
    mutate(
      Coordinates = map(WellKnownText, extract_coordinates),
      Latitude = sapply(Coordinates, function(x) x[2]),
      Longitude = sapply(Coordinates, function(x) x[1])
    ) |> 
    # Convert to UTM
    mutate(
      UTM_Coordinates = map(Coordinates, convert_to_utm),
      UTM_Easting = sapply(UTM_Coordinates, function(x) x[1]),
      UTM_Northing = sapply(UTM_Coordinates, function(x) x[2])
    )
  
  # Remove coordinates (just to tidy dataset somewhat)
  new_properties_df <- new_properties_df |>
    select(-Coordinates, -UTM_Coordinates)
  
  # Reconstruct URL for each property
  new_properties_df <- new_properties_df |>
    mutate(
      AddressLineDash = str_replace_all(AddressLine1, " ", "-"),  # Replace spaces with hyphens
      property_url = paste0("https://www.aspc.co.uk/search/property/", Id, "/", AddressLineDash, "/", City, "/")
    )
  
  # Handle FloorArea NA
  new_properties_df$FloorArea[new_properties_df$FloorArea == 0] <- NA
  
  # Perform anti_join with existing data to filter out existing properties
  if (nrow(existing_data) > 0) {
    new_properties_df <- anti_join(new_properties_df, existing_data, by = c("Id", "Price"))
  }
  
  # Combine new properties with existing data
  final_data <- bind_rows(existing_data, new_properties_df)
} else {
  final_data <- existing_data  # Keep existing data if no new properties found
}

# Write final data
write.csv(final_data, file_path, row.names = FALSE)
