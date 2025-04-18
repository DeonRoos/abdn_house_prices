# Script to fetch house details from ASPC site
# Setup -------------------------------------------------------------------
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(sf)
library(lubridate)
library(stringr)
library(purrr)
library(DBI)
library(RSQLite)

url <- "https://api.aspc.co.uk/Property/GetProperties?PrimaryPropertyType=Buy&SortBy=PublishedDesc&LastUpdated=AddedAnytime&PropertyType=Residential&HasGarage=false&HasDoubleGarage=false&HasGarden=false&IsNewBuild=false&HasGreenEnergy=false&HasOnStreetParking=false&HasOffStreetParking=false&HasElectricVehicleChargePoint=false&HasGrannyFlat=false&IsSelfContained=false&HasGroundFloorBedroomBathroom=false&IsLowCostHome=false&IsDevelopmentOpportunity=false&HasLand=false&FloorAreaMin=&ExcludeUnderOffer=false&IncludeClosedProperties=true&ClosedDatesSearch=14&ByFixedPriceOnly=false&ResultMode=NONE&ResultView=LIST&search-origin=search-results&Sort=PublishedDesc&Page=1&PageSize=12"

# Helpers -----------------------------------------------------------------
extract_coordinates <- function(wkt) {
  coords <- gsub("POINT \\(|\\)", "", wkt)
  lon_lat <- as.numeric(unlist(strsplit(coords, " ")))
  return(lon_lat)
}

convert_to_utm <- function(lon_lat) {
  point <- st_sfc(st_point(lon_lat), crs = 4326)
  utm_point <- st_transform(point, crs = 32630)
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

# Load Previous -----------------------------------------------------------
file_path  <- "property_data.csv"
db         <- dbConnect(SQLite(), dbname = "property_app_db.sqlite")
table_name <- "property_data"

if (dbExistsTable(db, table_name)) {
  message("Reading existing buy-property data from SQLite table: ", table_name)
  existing_data <- dbReadTable(db, table_name)
} else if (file.exists(file_path)) {
  message("No 'property_data' table found in DB; reading from CSV instead.")
  existing_data <- read.csv(file_path, stringsAsFactors = FALSE)
} else {
  existing_data <- data.frame()
}
existing_data$DateAdded <- as.Date(existing_data$DateAdded)

# Scrape ------------------------------------------------------------------
page_size <- 12
page <- 1

all_properties <- list()

repeat {
  current_url <- gsub("Page=1", paste0("Page=", page), url)
  message("Fetching page ", page, "...")
  properties_df <- fetch_properties(current_url)
  if (is.null(properties_df) || nrow(properties_df) == 0) {
    message("No more properties returned, stopping.")
    break
  }
  properties_df$DateAdded <- Sys.Date()
  all_properties[[length(all_properties) + 1]] <- properties_df
  page <- page + 1
  Sys.sleep(1)
}

# Combine -----------------------------------------------------------------
if (length(all_properties) > 0) {
  new_properties_df <- bind_rows(all_properties)
} else {
  new_properties_df <- data.frame()
}

# Process -----------------------------------------------------------------
if (nrow(new_properties_df) > 0) {
  new_properties_df <- new_properties_df |>
    select(-any_of(c("Photos", "Broadband", "Mobile"))) |>
    unnest(Location) |>
    unnest(Spatial) |>
    unnest(Geography) |>
    unnest(SolicitorAccount, names_sep = "_") |>
    mutate(
      has_garden = ifelse(str_detect(CategorisationDescription, "Garden"), "Yes", "No"),
      council_tax_band = str_extract(CategorisationDescription, "(?i)CT\\s*band\\s*-\\s*([A-Z])") |>
        str_extract("[A-Z]$"),
      council_tax_band = ifelse(council_tax_band == "T", NA, council_tax_band),
      epc_band = str_extract(CategorisationDescription, "(?i)EPC\\s*band\\s*-\\s*([A-Z])") |>
        str_extract("[A-Z]$"),
      epc_band = ifelse(epc_band == "T", NA, epc_band),
      num_floors = case_when(
        str_detect(CategorisationDescription, "Ground flr") & str_detect(CategorisationDescription, "1st flr") ~ 2,
        str_detect(CategorisationDescription, "Ground flr") ~ 1,
        TRUE ~ 1
      ),
      parking_type = case_when(
        str_detect(CategorisationDescription, "Double Garage") ~ "Double Garage",
        str_detect(CategorisationDescription, "Garage")        ~ "Garage",
        str_detect(CategorisationDescription, "Parking")       ~ "Parking",
        TRUE ~ "No parking"
      ),
      HouseType = case_when(
        HouseFormat == 1 ~ "Detached",
        HouseFormat == 2 ~ "Semi-Detached",
        HouseFormat == 3 ~ "Terraced",
        TRUE ~ NA_character_
      ),
      DateAdded = as.Date(DateAdded)
    ) |>
    mutate(
      Coordinates = map(WellKnownText, extract_coordinates),
      Latitude    = sapply(Coordinates, function(x) x[2]),
      Longitude   = sapply(Coordinates, function(x) x[1])
    ) |>
    mutate(
      UTM_Coordinates = map(Coordinates, convert_to_utm),
      UTM_Easting     = sapply(UTM_Coordinates, function(x) x[1]),
      UTM_Northing    = sapply(UTM_Coordinates, function(x) x[2])
    ) |>
    select(-Coordinates, -UTM_Coordinates) |>
    mutate(
      AddressLineDash = str_replace_all(AddressLine1, " ", "-"),
      property_url    = paste0("https://www.aspc.co.uk/search/property/", Id, "/", AddressLineDash, "/", City, "/")
    )
  
  new_properties_df$FloorArea[new_properties_df$FloorArea == 0] <- NA
  
  if (nrow(existing_data) > 0) {
    new_properties_df <- anti_join(new_properties_df, existing_data, by = c("Id", "Price"))
  }
  
  final_data <- bind_rows(existing_data, new_properties_df)
} else {
  final_data <- existing_data
}

# Write -------------------------------------------------------------------
write.csv(final_data, file_path, row.names = FALSE)
if (dbExistsTable(db, table_name)) dbRemoveTable(db, table_name)
dbWriteTable(db, table_name, final_data)
dbDisconnect(db)
