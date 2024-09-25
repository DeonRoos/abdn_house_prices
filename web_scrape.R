# Script to fetch house details from ASPC site ----------------------------
# Uses advice from David (Rosie's partner) to get the info:

# Packages ----------------------------------------------------------------
library(httr)
library(jsonlite)
library(dplyr)
library(sf)
library(lubridate)

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
  existing_data <- read.csv(file_path)
} else {
  existing_data <- data.frame()
}

# Scraping ----------------------------------------------------------------
all_properties <- list()

for (i in 1:400) {  # 400 given 12 properties per page and ca. 4000-5000 properties
  print(paste("Fetching page:", i))
  updated_url <- gsub("Page=1", paste0("Page=", i), url)
  properties_df <- fetch_properties(updated_url)
  if (!is.null(properties_df)) {
    properties_df$DateAdded <- Sys.Date()
    if ("Location.Spatial.Geography.WellKnownText" %in% colnames(properties_df)) {
      lon_lat <- t(sapply(properties_df$Location.Spatial.Geography.WellKnownText, extract_coordinates))
      colnames(lon_lat) <- c("Longitude", "Latitude")
      properties_df <- cbind(properties_df, lon_lat)
      utm_coords <- t(apply(lon_lat, 1, convert_to_utm))
      colnames(utm_coords) <- c("UTM_Easting", "UTM_Northing")
      properties_df <- cbind(properties_df, utm_coords)
    }
    if (nrow(existing_data) > 0) {
      new_properties <- anti_join(properties_df, existing_data, 
                                  by = c("PropertyID", "Price"))
    } else {
      new_properties <- properties_df
    }
    if (nrow(new_properties) > 0) {
      all_properties[[length(all_properties) + 1]] <- new_properties
    }
  }
  Sys.sleep(1)
}

# Merge each page into one df ---------------------------------------------
if (length(all_properties) > 0) {
  new_properties_df <- bind_rows(all_properties)
  final_data <- bind_rows(existing_data, new_properties_df)
} else {
  print("No new properties found.")
}

final_data <- final_data[,c(1, 3, 4, 5, 6, 8, 11, 13, 23, 24, 27, 33)]
colnames(final_data) <- c("id", "type", "bedrooms", "bathrooms", "living", "price", "under_offer", "house", "floor_area", "resident", "solicitor", "date_added")
head(final_data)
nrow(final_data)

write.csv(final_data, file_path, row.names = FALSE)
