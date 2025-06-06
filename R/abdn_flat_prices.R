# Script to analyse rent Prices in Aberdeenshire
# See web_scrape.R for scraping data from ASPC
# Deon Roos

# Packages ----------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(mgcv)          
library(dplyr)
library(leaflet)
library(htmlwidgets)
library(here)
library(ggmap)
library(lubridate)
library(sf)
library(httr)
library(jsonlite)
library(dotenv)
library(stringr)
library(tidyr)
library(DBI)
library(RSQLite)

# Environment -------------------------------------------------------------
dotenv::load_dot_env()
register_google(key = Sys.getenv("GOOGLE_MAPS_API_KEY"))
api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
source("sbs_theme.R")
theme_set(sbs_theme())

# Data --------------------------------------------------------------------

# Connect to SQLite -------------------------------------------------------
db <- dbConnect(SQLite(), dbname = "property_app_db.sqlite")

# Attempt to read from SQLite first ---------------------------------------
table_name <- "rent_data"
csv_path   <- "rent_data.csv"

if (dbExistsTable(db, table_name)) {
  message("Reading flat data from SQLite table: ", table_name)
  df <- dbReadTable(db, table_name)
} else {
  message("No 'abdn_flats' table in DB; reading from CSV instead.")
  df <- read.csv(csv_path, header = TRUE)
}

# Removing redundant variables
df <- df |> 
  select(-starts_with("Mobile."),
         -starts_with("Broadband."),
         -PropertyType, -PriceType, -RentalPricePeriod, -UnderOffer, -FormattedClosedDate,
         -CoordinateSystemId, -WellKnownText, -ClosingDateVisible, -ClosedDateVisible, -ClosedMessage, 
         -HideOpenActionsDisplay, -ClosingDate, -ClosedDate, -IsOpen, -ResidentialType, -HouseFormat, 
         -SolicitorAccount_Id, -SolicitorAccount_WebsiteAddress, -FormattedPrice, -ViewingArrangements, 
         -ViewingTiming, -BookingUrl, -IsSellerManaged, -CategorisationDescription,
         -FormattedClosingDate, -OrganisationName, -County, -Country)


# Data processing ---------------------------------------------------------

df <- df[!duplicated(paste(df$AddressLine1, df$Price)),]

df$Rooms <- df$Bedrooms + df$PublicRooms
df$DateAdded <- as.Date(df$DateAdded)
earliest_date <- min(df$DateAdded)
df <- df %>%
  mutate(DayAdded = as.numeric(difftime(DateAdded, earliest_date, units = "days")))

df$FloorArea <- ifelse(df$FloorArea == 0, NA, df$FloorArea)
df$council_tax_band <- ifelse(df$council_tax_band == "N", NA, df$council_tax_band)

df$HouseType <- ifelse(is.na(df$HouseType), "Flat", df$HouseType)

df <- df |> 
  drop_na()

# Commute time to ABDN uni ------------------------------------------------

origin <- "57.168010390142236,-2.106429897150815" # abdn uni
get_commute_time <- function(Latitude, Longitude, api_key, origin) {
  url <- paste0(
    "https://maps.googleapis.com/maps/api/distancematrix/json?units=metric",
    "&origins=", origin,
    "&destinations=", Latitude, ",", Longitude,
    "&mode=walking",
    "&key=", api_key
  )

  # Make the API request
  response <- GET(url)

  # Parse the response
  content <- fromJSON(content(response, "text"), flatten = TRUE)

  # Extract commute time (in seconds)
  commute_time <- content$rows$elements[[1]]$duration.value

  return(commute_time)
}

df$commute_time <- mapply(get_commute_time, df$Latitude, df$Longitude, MoreArgs = list(api_key = api_key, origin = origin))
df$commute_time_minutes <- df$commute_time / 60

# Urban/rural classification ----------------------------------------------
shapefile_path <- "C:/abdn_house_Prices/data/SG_UrbanRural_2020/SG_UrbanRural_2020.shp"
urban_rural_shp <- st_read(shapefile_path)
df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)
urban_rural_shp <- st_transform(urban_rural_shp, crs = st_crs(df_sf))
df <- as.data.frame(st_join(df_sf, urban_rural_shp, left = TRUE))
coordinates <- st_coordinates(df_sf)
df$Longitude <- coordinates[, 1]  # Longitude (X)
df$Latitude <- coordinates[, 2]  # Latitude (Y)
df$geometry <- NULL

df <- df |> 
  select(-OfficePropertyId, -LineOneLocation, -LineTwoLocation, -Locality,
         -Postcode, -PropertyIconKey, -AddressLineDash, -commute_time,
         -Shape_Leng, -Shape_Area, -UR8Class, -UR6Class, -UR6Name, -UR6Desc,
         -UR3Class, -UR3Name, -UR3Desc, -UR2Class, -UR2Name, -UR2Desc)

df <- df %>%
  rename(EPC = epc_band,
         Tax = council_tax_band,
         Address = AddressLine1,
         Solicitor = SolicitorAccount_Name,
         Furnished = furnished,
         Garden = has_garden,
         Easting = UTM_Easting,
         Northing = UTM_Northing,
         PropertyURL = property_url,
         UniCommuteTime = commute_time_minutes,
         UrbanRural = UR8Name,
         UrbanRuralDescription = UR8Desc
         )

## EDA ---------------------------------------------------------------------

ggplot(df) +
  geom_point(aes(x = DateAdded, y = Price))

ggplot(df) +
  geom_point(aes(x = FloorArea, y = Price))

ggplot(df) +
  geom_point(aes(x = Rooms, y = Price))

ggplot(df) +
  geom_boxplot(aes(x = HouseType, y = Price))

ggplot(df) +
  geom_boxplot(aes(x = Furnished, y = Price))

ggplot(df) +
  geom_boxplot(aes(x = Garden, y = Price))

ggplot(df) +
  geom_boxplot(aes(x = Tax, y = Price))

ggplot(df) +
  geom_boxplot(aes(x = EPC, y = Price))

ggplot(df) +
  geom_boxplot(aes(x = UrbanRural, y = Price))

ggplot(df) +
  geom_boxplot(aes(x = City, y = Price))

ggplot(df) +
  geom_point(aes(x = Easting, y = Northing, colour = Price)) +
  scale_colour_viridis_c()

ggplot(df) +
  geom_point(aes(x = UniCommuteTime, y = Price))

ggplot(df) +
  geom_boxplot(aes(x = Solicitor, y = Price))


# Save data for students --------------------------------------------------

write.table(df, "C:/006-BI3010/Workshops/Workshop 2 - Data vis/Data/abdn_flats.txt", row.names = FALSE)

dbDisconnect(db)


# Linear model ------------------------------------------------------------
lm1 <- lm(Price ~ FloorArea + Rooms + HouseType + EPC + Tax + Furnished + DayAdded,
          data = df)

saveRDS(lm1, file = "C:/flat_rent/data/lm_m1.rds")


# Expected price ----------------------------------------------------------

prds <- predict(lm1, se.fit = TRUE)
df$expect <- round(prds$fit)
df$low <- round(prds$fit - 1.96 * prds$se.fit)
df$upp <- round(prds$fit + 1.96 * prds$se.fit)

df$diffn <- df$Price - df$expect
df$diff <- scales::comma(df$Price - df$expect)

df$over <- ifelse(df$Price > df$upp, "Overpriced", 
                  ifelse(df$Price < df$low, "Underpriced", 
                         "Fairly Priced"))

## Square meters -----------------------------------------------------------

# Updated the `FloorArea` variable to use 'FloorArea'
nu_data <- data.frame(
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Rooms = median(df$Rooms, na.rm = TRUE),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  FloorArea = seq(min(df$FloorArea, na.rm = TRUE), max(df$FloorArea, na.rm = TRUE), length.out = 25),
  UrbanRural = "Large Urban Areas",
  Furnished = "Fully furnished"
)

prds <- predict(lm1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p1 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = FloorArea, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = FloorArea, y = Price), size = 0.5) +
  geom_line(data = nu_data, aes(x = FloorArea, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Square meters") +
  sbs_theme()
p1

## Rooms ----------------------------------------------------------------

nu_data <- data.frame(
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Rooms = seq(min(df$Rooms, na.rm = TRUE), max(df$Rooms, na.rm = TRUE), length.out = 25),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  FloorArea = median(df$FloorArea, na.rm = TRUE),
  UrbanRural = "Large Urban Areas",
  Furnished = "Fully furnished"
)

prds <- predict(lm1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p2 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = Rooms, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = Rooms, y = Price), size = 0.5) +
  geom_line(data = nu_data, aes(x = Rooms, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Rooms") +
  sbs_theme()
p2

## EPC ----------------------------------------------------------------

nu_data <- data.frame(
  EPC = unique(df$EPC),
  Tax = "C",
  HouseType = "Flat",
  Rooms = median(df$Rooms, na.rm = TRUE),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  FloorArea = median(df$FloorArea, na.rm = TRUE),
  UrbanRural = "Large Urban Areas",
  Furnished = "Fully furnished"
)

prds <- predict(lm1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p3 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = EPC, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  geom_point(data = nu_data, aes(x = EPC, y = fit), size = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "EPC") +
  sbs_theme()
p3

## Tax ----------------------------------------------------------------

nu_data <- data.frame(
  EPC = "C",
  Tax = unique(df$Tax),
  HouseType = "Flat",
  Rooms = median(df$Rooms, na.rm = TRUE),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  FloorArea = median(df$FloorArea, na.rm = TRUE),
  UrbanRural = "Large Urban Areas",
  Furnished = "Fully furnished"
)

prds <- predict(lm1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p4 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = Tax, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  geom_point(data = nu_data, aes(x = Tax, y = fit), size = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Tax Band") +
  sbs_theme()
p4

## Furnished ----------------------------------------------------------------

nu_data <- data.frame(
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Rooms = median(df$Rooms, na.rm = TRUE),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  FloorArea = median(df$FloorArea, na.rm = TRUE),
  UrbanRural = "Large Urban Areas",
  Furnished = unique(df$Furnished)
)

prds <- predict(lm1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p5 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = Furnished, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  geom_point(data = nu_data, aes(x = Furnished, y = fit), size = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Furnished") +
  sbs_theme()
p5

## Urban Rural ----------------------------------------------------------------

nu_data <- data.frame(
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Rooms = median(df$Rooms, na.rm = TRUE),
  FloorArea = median(df$FloorArea, na.rm = TRUE),
  DayAdded = seq(min(df$DayAdded, na.rm = TRUE), max(df$DayAdded, na.rm = TRUE), length.out = 25),
  #UrbanRural = unique(df$UrbanRural),
  Furnished = "Fully furnished"
)

prds <- predict(lm1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p6 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = DayAdded, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_line(data = nu_data, aes(x = DayAdded, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Days since start") +
  sbs_theme()
p6

## Predicted versus response -----------------------------------------------

df$low1 <- 0 - sigma(lm1) + df$Price
df$upp1 <- 0 + sigma(lm1) + df$Price
df$low2 <- 0 - 2 * sigma(lm1) + df$Price
df$upp2 <- 0 + 2 * sigma(lm1) + df$Price

p7 <- ggplot(df) +
  geom_ribbon(aes(x = Price, ymin = low1, ymax = upp1), fill = "red", alpha = 0.4) +
  geom_ribbon(aes(x = Price, ymin = low2, ymax = upp2), fill = "red", alpha = 0.4) +
  geom_point(aes(x = Price, y = expect), size = 1) +
  geom_abline(intercept = 0, slope = 1, colour = "white", linetype = 1) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Listed rent (£)",
       y = "Predicted rent (£)") +
  sbs_theme()
p7


## Relationships -----------------------------------------------------------

design <- "
AB
CD
EF
GG
"

lm_plots <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + plot_layout(design = design)
lm_plots

ggsave("C:/flat_rent/www/lm_plots.png", plot = lm_plots, width = 18, height = 14, dpi = 400)


# GAM with space ----------------------------------------------------------
mp <- list(
  c(3, 0.1, 1), # 3 for GP, 0.1 ca. 10 km range of rho, 1 as default for scale
  c(3, 0.1, 1)
)
m1 <- gam(Price ~ 
            te(Longitude, Latitude, k = 5, bs = "gp", m = mp) +
            s(DayAdded, bs = "cr", k = 10) +
            FloorArea + 
            Rooms + 
            Tax + 
            EPC +
            HouseType +
            Furnished,
          data = df,
          method = "REML")

saveRDS(m1, file = "C:/flat_rent/data/model_m1.rds")

# Expected price ----------------------------------------------------------

prds <- predict(m1, se.fit = TRUE)
df$expect_gam <- round(prds$fit)
df$low_gam <- round(prds$fit - 1.96 * prds$se.fit)
df$upp_gam <- round(prds$fit + 1.96 * prds$se.fit)

df$diffn_gam <- df$Price - df$expect_gam
df$diff_gam <- scales::comma(df$Price - df$expect_gam)

df$over_gam <- ifelse(df$Price > df$upp_gam, "Overpriced", 
                  ifelse(df$Price < df$low_gam, "Underpriced", 
                         "Fairly Priced"))

## Space ------------------------------------------------------------

# get map of whole region for plotting
lon_bar <- (min(df$Longitude, na.rm = TRUE) + max(df$Longitude, na.rm = TRUE))/2
lat_bar <- (min(df$Latitude, na.rm = TRUE) + max(df$Latitude, na.rm = TRUE))/2

abdnshire <- get_map(location = c(lon_bar, lat_bar), 
                     zoom = 10, 
                     mapHouseType = "hybrid", 
                     source = "google", 
                     messaging = FALSE)

nu_data <- expand.grid(
  Latitude = seq(min(df$Latitude), max(df$Latitude), length.out = 200),
  Longitude = seq(min(df$Longitude), max(df$Longitude), length.out = 200),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Furnished = "Fully furnished",
  UrbanRural = "Large Urban Areas",
  Rooms = median(df$Rooms),
  FloorArea = median(df$FloorArea)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit

nu_data$fit[exclude.too.far(
  nu_data$Latitude, nu_data$Longitude,
  df$Latitude, df$Longitude,
  dist = 0.05)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

p1 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white") +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nrent (£)") +
  sbsvoid_theme()
p1


## Aberdeen ----------------------------------------------------------------

min_lon <- -2.25
max_lon <- -2.03
min_lat <- 57.06
max_lat <- 57.21

abdn <- get_map(location = c(-2.1433691553190624, 57.149481894948565), 
                zoom = 12, 
                mapHouseType = "hybrid", 
                source = "google", 
                messaging = FALSE)

nu_data <- expand.grid(
  Latitude = seq(min(min_lat), max(max_lat), length.out = 200),
  Longitude = seq(min(min_lon), max(max_lon), length.out = 200),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Furnished = "Fully furnished",
  UrbanRural = "Large Urban Areas",
  Rooms = median(df$Rooms),
  FloorArea = median(df$FloorArea)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit

nu_data$fit[exclude.too.far(
  nu_data$Latitude, nu_data$Longitude,
  df$Latitude, df$Longitude,
  dist = 0.1)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

p2 <- ggmap(abdn) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), na.rm = TRUE, alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white") +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nrent (£)") +
  sbsvoid_theme()
p2

gam_space <- p1 / p2

ggsave("C:/flat_rent/www/rent_maps.png", plot = gam_space, width = 18, height = 14, dpi = 400)

## Square meters -----------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Furnished = "Fully furnished",
  UrbanRural = "Large Urban Areas",
  Rooms = median(df$Rooms),
  FloorArea = seq(min(df$FloorArea), max(df$FloorArea), length.out = 25)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p3 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = FloorArea, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = FloorArea, y = Price), size = 1) +
  geom_line(data = nu_data, aes(x = FloorArea, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Square meters") +
  sbs_theme()
p3

## Rooms ----------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Furnished = "Fully furnished",
  UrbanRural = "Large Urban Areas",
  Rooms = seq(min(df$Rooms), max(df$Rooms), length.out = 25),
  FloorArea = median(df$FloorArea)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p4 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = Rooms, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = Rooms, y = Price), size = 1) +
  geom_line(data = nu_data, aes(x = Rooms, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Rooms") +
  sbs_theme()
p4

## EPC ----------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  EPC = unique(df$EPC),
  Tax = "C",
  HouseType = "Flat",
  Furnished = "Fully furnished",
  UrbanRural = "Large Urban Areas",
  Rooms = median(df$Rooms),
  FloorArea = median(df$FloorArea)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p5 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = EPC, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  geom_point(data = nu_data, aes(x = EPC, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking Price",
       x = "EPC") +
  sbs_theme()
p5

## Tax ----------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  EPC = "C",
  Tax = unique(df$Tax),
  HouseType = "Flat",
  Furnished = "Fully furnished",
  UrbanRural = "Large Urban Areas",
  Rooms = median(df$Rooms),
  FloorArea = median(df$FloorArea)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p6 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = Tax, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  geom_point(data = nu_data, aes(x = Tax, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Tax Band") +
  sbs_theme()
p6

## Furnished ----------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  DayAdded = median(df$DayAdded, na.rm = TRUE),
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Furnished = unique(df$Furnished),
  UrbanRural = "Large Urban Areas",
  Rooms = median(df$Rooms),
  FloorArea = median(df$FloorArea)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p7 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = Furnished, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  geom_point(data = nu_data, aes(x = Furnished, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Furnished") +
  sbs_theme()
p7

## Urban Rural ----------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  EPC = "C",
  Tax = "C",
  HouseType = "Flat",
  Rooms = median(df$Rooms, na.rm = TRUE),
  FloorArea = median(df$FloorArea, na.rm = TRUE),
  DayAdded = seq(min(df$DayAdded, na.rm = TRUE), max(df$DayAdded, na.rm = TRUE), length.out = 25),
  #UrbanRural = unique(df$UrbanRural),
  Furnished = "Fully furnished"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p8 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = DayAdded, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = DayAdded, y = Price), size = 1) +
  geom_line(data = nu_data, aes(x = DayAdded, y = fit)) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "Days since start") +
  sbs_theme()
p8

## Predicted versus response -----------------------------------------------

df$low1 <- 0 - sigma(m1) + df$Price
df$upp1 <- 0 + sigma(m1) + df$Price
df$low2 <- 0 - 2 * sigma(m1) + df$Price
df$upp2 <- 0 + 2 * sigma(m1) + df$Price

df$expect <- predict(m1)

p9 <- ggplot(df) +
  geom_ribbon(aes(x = Price, ymin = low1, ymax = upp1), fill = "red", alpha = 0.4) +
  geom_ribbon(aes(x = Price, ymin = low2, ymax = upp2), fill = "red", alpha = 0.4) +
  geom_point(aes(x = Price, y = expect), size = 1) +
  geom_abline(intercept = 0, slope = 1, colour = "white") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Listed rent (£)",
       y = "Predicted rent (£)") +
  sbs_theme()
p9

library(patchwork)
design <- "
AB
CD
EF
GG
"

gam_plots <- p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(design = design)
gam_plots
ggsave("C:/flat_rent/www/rent_figs.png", plot = gam_plots, width = 18, height = 14, dpi = 400)

# Leaflet map -------------------------------------------------------------

pricing <- awesomeIconList(
  "Underpriced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "green",
    library = "fa"
  ),
  "Fairly Priced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "white",
    library = "fa"
  ),
  "Overpriced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "red",
    library = "fa"
  )
)

abdn_map <- leaflet(df) %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addAwesomeMarkers(
    data = df,
    #lng = ~Longitude, Latitude = ~Latitude,
    icon = ~ pricing[over],
    label = ~paste0(
      Address, " (£", Price, ")"
    ),
    clusterOptions = markerClusterOptions(maxClusterRadius = 0),
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", df$Address, "</b></span><br>",
      "<span style='font-size: 14px;'><b>", df$UrbanRural, "</b></span><br>",
      "<span style='font-size: 16px;'><b>Commute</b></span>",
      "<br><b>Time to walk to Uni:</b> ", ifelse(df$UniCommuteTime < 60, 
                                          paste0(round(df$UniCommuteTime), " minutes"), 
                                          paste0(round(df$UniCommuteTime / 60, 1), " hours")),
      "<hr>",
      "<span style='font-size: 16px;'><b>Rental details</b></span>",
      "<br><b>Rent fairness:</b> ", df$over,
      "<br><b>Rent:</b> £", scales::comma(df$Price), " (£", abs(round(df$diffn, digits = 0)),
      ifelse(round(df$diffn, digits = 0) < 0, " under expected)", ifelse(round(df$diffn, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Rent (LM):</b> £", scales::comma(round(df$expect, digits = 0)),
      "<br><b>Expected Rent Range (LM):</b> £", round(df$low, digits = 0), " - £", round(df$upp, digits = 0), 
      "<br><i>Expected Rent (GAM):</i> £", scales::comma(df$expect_gam),
      "<br><i>Expected Rent Range (GAM):</i> £", round(df$low_gam, digits = 0), " - £", round(df$upp_gam, digits = 0), 
      "<hr>",
      "<span style='font-size: 16px;'><b>Flat details</b></span>",
      "<br><b>Date added:</b> ", df$DateAdded,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      df$HouseType, "<br>",
      df$Bedrooms, " bedrooms", "<br>",
      df$PublicRooms, " living rooms", "<br>",
      df$Bathrooms, " bathrooms", "<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      df$FloorArea, " m<sup>2</sup>", "<br>",
      "EPC: ", toupper(df$EPC), "<br>",
      "Tax: ", toupper(df$Tax), "<br>",
      "Furnished: ", df$Furnished, "<br>",
      "</td></tr>",
      "</table>",
      "<hr>",
      "<br><a href='", df$PropertyURL, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  )

abdn_map

saveWidget(abdn_map, here("C:/flat_rent", "www", "abdn_map.html"), selfcontained = TRUE)
