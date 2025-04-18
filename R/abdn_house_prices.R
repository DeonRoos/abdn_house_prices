# Script to analyse house Prices in Aberdeenshire
# Deon Roos

# Setup -------------------------------------------------------------------
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

dotenv::load_dot_env()
register_google(key = Sys.getenv("GOOGLE_MAPS_API_KEY"))
api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
source("sbs_theme.R")
theme_set(sbs_theme())

# Load data ---------------------------------------------------------------
db <- dbConnect(SQLite(), dbname = "property_app_db.sqlite")
table_name <- "property_data"
csv_path   <- "property_data.csv"

if (dbExistsTable(db, table_name)) {
  message("Reading house data from SQLite table: ", table_name)
  df <- dbReadTable(db, table_name)
} else {
  message("No 'house_prices' table in DB; reading from CSV instead.")
  df <- read.csv(csv_path, header = TRUE)
}

# Data cleaning -----------------------------------------------------------
df <- df |> 
  select(-PropertyType, -PriceType, -RentalPricePeriod, -UnderOffer, -FormattedClosingDate,
         -FormattedClosedDate, -OrganisationName, -County, -Country, -CoordinateSystemId, 
         -WellKnownText, -ClosingDateVisible, -ClosedDateVisible, -ClosedMessage, -HideOpenActionsDisplay,
         -ClosingDate, -ClosedDate, -IsOpen, -ResidentialType, -HouseFormat, -SolicitorAccount_Id,
         -SolicitorAccount_WebsiteAddress, -FormattedPrice, -ViewingArrangements, -ViewingTiming,
         -BookingUrl, -IsSellerManaged,
         -CategorisationDescription)

df <- df[df$AddressLine1 != "Blackbriggs",]
df <- df[!duplicated(paste(df$AddressLine1, df$Price)),]
df <- df[df$Latitude > 56.077553904659,]
df$rooms <- df$Bedrooms + df$PublicRooms
df$date <- as.Date(df$DateAdded)
earliest_date <- min(df$date)
df <- df %>%
  mutate(days_since = as.numeric(difftime(date, earliest_date, units = "days")))

df$FloorArea <- ifelse(df$FloorArea == 0, NA, df$FloorArea)
df$council_tax_band <- ifelse(df$council_tax_band == "N", NA, df$council_tax_band)
df$HouseType <- ifelse(is.na(df$HouseType), "Flat", df$HouseType)
df <- df |> drop_na()

# Price per square meter --------------------------------------------------
df$Price_FloorArea <- df$Price / df$FloorArea

# Urban/rural classification ----------------------------------------------
shapefile_path <- "C:/abdn_house_Prices/data/SG_UrbanRural_2020/SG_UrbanRural_2020.shp"
urban_rural_shp <- st_read(shapefile_path)
df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)
urban_rural_shp <- st_transform(urban_rural_shp, crs = st_crs(df_sf))
df <- as.data.frame(st_join(df_sf, urban_rural_shp, left = TRUE))
coords <- st_coordinates(df_sf)
df$Longitude <- coords[, 1]
df$Latitude <- coords[, 2]
df$geometry <- NULL

df <- df |> 
  select(-UR2Desc, -UR2Name, -UR2Class,
         -UR3Desc, -UR3Name, -UR3Class,
         -UR6Desc, -UR6Name, -UR6Class,
         -UR8Desc, -UR8Class)

# EDA ---------------------------------------------------------------------
ggplot(df, aes(x = Price, fill = HouseType)) +
  geom_density(alpha = 0.6) +
  labs(x = "Price", y = "Density")

df |> 
  group_by(date, HouseType) |> 
  summarise(count = n(), .groups = "drop") |> 
  filter(date != min(date)) |> 
  filter(date != ymd("2025-03-04")) |> # deeper scrape
  ggplot(aes(x = date, y = count, color = HouseType)) +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  labs(x = "Date", y = "Number of Listings", color = "House Type")

df |>
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |> 
  filter(date != min(date)) |> 
  filter(date != ymd("2025-03-04")) |> # deeper scrape
  mutate(fortnight = floor_date(date, "week") - days(1) + weeks((as.integer(difftime(date, floor_date(date, "week"), units = "days")) %/% 14) * 2)) |> 
  group_by(fortnight, HouseType) |> 
  summarise(count = n(), .groups = 'drop') |> 
  ggplot(aes(x = fortnight, y = count, color = HouseType)) +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  labs(x = "Fortnight", y = "Number of new listings", color = "House Type")

df |>
  mutate(month = month(date)) |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |> 
  filter(date != min(date)) |> 
  filter(date != ymd("2025-03-04")) |> # deeper scrape
  group_by(month, HouseType) |> 
  summarise(count = n(), .groups = 'drop') |> 
  ggplot(aes(x = month, y = count, color = HouseType)) +
  geom_point(size = 2) +
  geom_line(size = 0.5) +
  labs(x = "Month", y = "Number of Listings", color = "House Type")

ggplot(df, aes(x = Price)) +
  geom_histogram(alpha = 0.6, binwidth = 10000) +
  labs(x = "Price")

ggplot(df, aes(x = HouseType, y = Price, fill = HouseType)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "EPC", y = "Price")

ggplot(df, aes(x = reorder(City, -Price, FUN = median), y = Price)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "Area", y = "Price") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(df, aes(x = council_tax_band, y = Price, fill = HouseType)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "Tax Band", y = "Price")

ggplot(df, aes(x = reorder(SolicitorAccount_Name, -Price, FUN = median), y = Price)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "Solicitor", y = "Price") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(df, aes(x = epc_band, y = Price)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "EPC", y = "Price")

df[df$date == Sys.Date(),]

df[df$Bedrooms == 2 &
     df$Bathrooms == 1 &
     df$PublicRooms == 1 &
     (df$FloorArea > 80 & df$FloorArea < 110) & 
     df$has_garden == "Yes" &
     df$HouseType == "Detached" &
     df$UR8Name == "Accessible Rural Areas" &
     df$council_tax_band == "D",]

# Modeling ----------------------------------------------------------------
mp <- list(c(3, 0.05, 1), c(3, 0.05, 1))
df$HouseType <- factor(df$HouseType)
df$epc_band <- factor(df$epc_band)
df$council_tax_band <- factor(df$council_tax_band)
df$UR8Name <- factor(df$UR8Name)
df$has_garden <- factor(df$has_garden)
df$num_floors <- factor(df$num_floors)
df$parking_type <- factor(df$parking_type)
df$City <- factor(df$City)
df$SolicitorAccount_Name <- factor(df$SolicitorAccount_Name)

m1 <- bam(
  Price ~
    s(days_since, by = HouseType, k = 5, bs = "cr") +
    t2(Longitude, Latitude, k = 30, m = mp, bs = "gp") +
    UR8Name +
    HouseType +
    s(FloorArea, k = 3, bs = "cr") +
    s(Bedrooms, k = 3, bs = "cr") +
    s(PublicRooms, k = 3, bs = "cr") +
    s(Bathrooms, k = 3, bs = "cr") +
    num_floors + parking_type + has_garden + epc_band + council_tax_band,
  data = df,
  method = "fREML"
)

saveRDS(m1, file = "C:/abdn_app/data/model_m1.rds")
summary(m1)
gam.check(m1)

# Predictions -------------------------------------------------------------
prds <- predict(m1, se.fit = TRUE)
df <- df |> mutate(
  expect = round(prds$fit, 0),
  low = round(prds$fit - 1.96 * prds$se.fit),
  upp = round(prds$fit + 1.96 * prds$se.fit),
  diffn = Price - expect,
  over = case_when(
    Price > upp ~ "OverPriced",
    Price < low ~ "UnderPriced",
    TRUE ~ "Fairly Priced"
  )
)

# Standardised prediction
df_std <- df |> 
  mutate(std_Latitude = 57.15403687694044,
         std_Longitude = -2.1006741041665826,
         std_UR8Name = "Large Urban Areas",
         std_days_since = 100)
df_std <- df_std |> 
  mutate(Latitude = std_Latitude,
         Longitude = std_Longitude,
         UR8Name = std_UR8Name,
         days_since = std_days_since)
std_prds <- predict(m1, newdata = df_std)
df <- df |> mutate(std_expect = round(std_prds, 0))

# Viewing criteria --------------------------------------------------------
df <- df |> 
  mutate(viewing = ifelse(
    ((over == "UnderPriced" | over == "Fairly Priced") & Price <= 250000) | 
      ((over == "OverPriced" | over == "Fairly Priced") & expect <= 250000) &
      FloorArea > 80 & HouseType != "terrace",
    "View", "Meh")
  )

# House Price prediction for a specific house -----------------------------
dream_house <- data.frame(
  Latitude = 57.15483899436254,
  Longitude = -2.269886390197508,
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  Bedrooms = 2,
  PublicRooms = 2,
  Bathrooms = 2, 
  epc_band = "C",
  council_tax_band = "D",
  FloorArea = 100,
  days_since = as.numeric(ymd(Sys.Date()) - earliest_date),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = dream_house, se.fit = TRUE)
paste0("# £", round(prds$fit, -3)/1000, "k [£",
       round(prds$fit - 1.96 * prds$se.fit, -3)/1000, "k-£",
       round(prds$fit + 1.96 * prds$se.fit, -3)/1000, "k] (n = ",
       nrow(df), ") ", stringr::str_to_title(dream_house$HouseType)
)

## Over time ---------------------------------------------------------------

nu_data <- expand.grid(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = unique(df$HouseType),
  epc_band = "C",
  council_tax_band = "E",
  Bedrooms = median(df$Bedrooms),
  PublicRooms = median(df$PublicRooms),
  Bathrooms = median(df$Bathrooms),
  FloorArea = median(df$FloorArea),
  UR8Name = "Accessible Rural Areas",
  days_since = seq(min(df$days_since), max(df$days_since), length.out = 25),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)
nu_data$HouseType <- factor(nu_data$HouseType, levels = c("Detached",
                                                          "Semi-Detached",
                                                          "Terraced",
                                                          "Flat"))
nu_data$date <- min(df$date) + nu_data$days_since
prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p1 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = date, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = date, y = fit)) +
  facet_wrap(~HouseType) +
  labs(y = "Expected\nPrice",
       x = "Date")
p1

## Floor area ----------------------------------------------------------------

nu_data <- expand.grid(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),#seq(min(df$rooms), max(df$rooms), length.out = 25),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = seq(min(df$FloorArea), max(df$FloorArea), length.out = 25),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p2 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = FloorArea, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = FloorArea, y = fit)) +  
  labs(x = "Square Meters",
       y = "Expected\nPrice")
p2

## House Type --------------------------------------------------------------
nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = c("Semi-Detached", "Detached", "Terraced", "Flat"),
  UR8Name = "Accessible Rural Areas",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  epc_band = "C",
  council_tax_band = "E",
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p3 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = HouseType, y = fit, ymin = low, ymax = upp), width = 0.1, colour = "white") +
  geom_point(data = nu_data, aes(x = HouseType, y = fit), size = 3.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Expected\nPrice",
       x = "House Type")
p3

## Rooms ----------------------------------------------------------------

nu_data <- expand.grid(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  Bedrooms = seq(min(df$Bedrooms), max(df$Bedrooms), length.out = 25),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p4 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = Bedrooms, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = Bedrooms, y = fit)) +  
  labs(x = "Bedrooms",
       y = "Expected\nPrice")
p4

## PublicRooms ----------------------------------------------------------------

nu_data <- expand.grid(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  PublicRooms = seq(min(df$PublicRooms), max(df$PublicRooms), length.out = 25),
  Bathrooms = median(df$Bathrooms),
  Bedrooms = median(df$Bedrooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p4.5 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = PublicRooms, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = PublicRooms, y = fit)) +  
  labs(x = "Public Rooms",
       y = "Expected\nPrice")
p4.5

## Bathrooms ----------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = seq(min(df$Bathrooms), max(df$Bathrooms), length.out = 25),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p5 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = Bathrooms, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = Bathrooms, y = fit)) +
  labs(y = "Expected\nPrice",
       x = "Bathrooms")
p5

## epc_band ------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = unique(df$epc_band),
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p6 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = epc_band, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = epc_band, y = fit), size = 2.5) +
  labs(y = "Expected\nPrice",
       x = "EPC Band")
p6

## council_tax_band ------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = unique(df$council_tax_band),
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p7 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = council_tax_band, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = council_tax_band, y = fit), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Expected\nPrice",
       x = "Tax Band")
p7


## Parking -------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = unique(df$parking_type),
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p9 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = parking_type, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = parking_type, y = fit), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(y = "Expected\nPrice",
       x = "Parking Type")
p9

## Garden -------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = unique(df$has_garden),
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p9.1 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = has_garden, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = has_garden, y = fit), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(y = "Expected\nPrice",
       x = "Garden")
p9.1

## Floors -------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = unique(df$num_floors),
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p9.2 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = num_floors, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = num_floors, y = fit), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(y = "Expected\nPrice",
       x = "Number of\nFloors")
p9.2

## Urban|Rural -------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = unique(df$UR8Name),
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p8 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = UR8Name, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = UR8Name, y = fit), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_color_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(y = "Expected\nPrice",
       x = "Urban | Rural\nDesignation")
p8

## Predicted versus response -----------------------------------------------

df$low1 <- 0 - sigma(m1) + df$expect
df$upp1 <- 0 + sigma(m1) + df$expect
df$low2 <- 0 - 2 * sigma(m1) + df$expect
df$upp2 <- 0 + 2 * sigma(m1) + df$expect

p10 <- ggplot(df) +
  geom_ribbon(aes(x = expect, ymin = low1, ymax = upp1), fill = "red", alpha = 0.4) +
  geom_ribbon(aes(x = expect, ymin = low2, ymax = upp2), fill = "red", alpha = 0.4) +
  geom_point(aes(x = expect, y = Price), size = 1, alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, colour = "white") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Listed Price (£)",
       x = "Expected Price (£)") +
  sbs_theme()

design <- "
AABBKKLL
CCDDGGHH
EEFFIIJJ
MMMMMMMM
MMMMMMMM
"

p_figs <- p10 + p2 + p3 + p4 + p4.5 + p5 + p6 + p7 + p8 + p9 + p9.1 + p9.2 + p1 + plot_layout(design = design)
p_figs

ggsave("C:/abdn_app/www/trends.png", plot = p_figs, width = 18, height = 14, dpi = 400)


## Aberdeenshire far -------------------------------------------------------

# get map of whole region for plotting
Longitude_bar <- (min(df$Longitude, na.rm = TRUE) + max(df$Longitude, na.rm = TRUE))/2
Latitude_bar <- (min(df$Latitude, na.rm = TRUE) + max(df$Latitude, na.rm = TRUE))/2

abdnshire <- get_map(location = c(Longitude_bar, Latitude_bar), 
                     zoom = 8, 
                     mapHouseType = "hybrid", 
                     source = "google", 
                     messaging = FALSE)

nu_data <- expand.grid(
  Latitude = seq(min(df$Latitude), max(df$Latitude), length.out = 200),
  Longitude = seq(min(df$Longitude), max(df$Longitude), length.out = 200),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$Latitude, nu_data$Longitude,
  df$Latitude, df$Longitude,
  dist = 0.02)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map1 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, binwidth = 100000) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, linetype = 2, binwidth = 50000) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitudegitude",
       y = "Latitudeitude",
       fill = "Expected\nHouse\nListing\nPrice (£)") +
  sbsvoid_theme()
map1

## Aberdeenshire close -----------------------------------------------------

# get map of whole region for plotting
Longitude_bar <- (min(df$Longitude, na.rm = TRUE) + max(df$Longitude, na.rm = TRUE))/2
Latitude_bar <- (min(df$Latitude, na.rm = TRUE) + max(df$Latitude, na.rm = TRUE))/2

abdnshire <- get_map(location = c(Longitude_bar, Latitude_bar), 
                zoom = 9, 
                mapHouseType = "hybrid", 
                source = "google", 
                messaging = FALSE)

nu_data <- expand.grid(
  Latitude = seq(min(df$Latitude), max(df$Latitude), length.out = 200),
  Longitude = seq(min(df$Longitude), max(df$Longitude), length.out = 200),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$Latitude, nu_data$Longitude,
  df$Latitude, df$Longitude,
  dist = 0.015)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map2 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, linetype = 2, binwidth = 25000) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitudegitude",
       y = "Latitudeitude",
       fill = "Expected\nPrice (£)") +
  sbsvoid_theme()
map2

## Aberdeen ----------------------------------------------------------------

min_Longitude <- -2.25
max_Longitude <- -2.03
min_Latitude <- 57.06
max_Latitude <- 57.21

abdn <- get_map(location = c(-2.1433691553190624, 57.149481894948565), 
                zoom = 12, 
                mapHouseType = "hybrid", 
                source = "google", 
                messaging = FALSE)

nu_data <- expand.grid(
  Latitude = seq(min_Latitude, max_Latitude, length.out = 200),
  Longitude = seq(min_Longitude, max_Longitude, length.out = 200),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since),
  has_garden = "Yes",
  num_floors = 1,
  parking_type = "Garage",
  SolicitorAccount_Name = "Aberdein Considine"
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$Latitude, nu_data$Longitude,
  df$Latitude, df$Longitude,
  dist = 0.05)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map3 <- ggmap(abdn) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), na.rm = TRUE, alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, linetype = 2, binwidth = 25000) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitudegitude",
       y = "Latitudeitude",
       fill = "Expected\nPrice (£)") +
  sbsvoid_theme()
map3

p_maps <- map2 / map3
ggsave("C:/abdn_app/www/plot_maps.png", plot = p_maps, width = 18, height = 14, dpi = 400)

# Leaflet map pricing --------------------------------------------------------

# Create subsets for each HouseType
df_detached <- subset(df, HouseType == "Detached")
df_semi <- subset(df, HouseType == "Semi-Detached")
df_terraced <- subset(df, HouseType == "Terraced")
df_flat <- subset(df, HouseType == "Flat")

# Define the pricing icon list
pricing <- awesomeIconList(
  "UnderPriced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "green",
    library = "fa"
  ),
  "Fairly Priced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "white",
    library = "fa"
  ),
  "OverPriced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "red",
    library = "fa"
  )
)

# Create the leaflet map with layers for each HouseType
abdn_map_Price <- leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  
  # Add markers for Detached
  addAwesomeMarkers(
    data = df_detached,
    icon = ~pricing[over],
    group = "Detached",
    label = ~paste0(AddressLine1, " (£", abs(round(Price / 1000, digits = 0)), "k)"),
    clusterOptions = markerClusterOptions(maxClusterRadius = 0),
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", AddressLine1, "</b></span>",
      "<br><span style='font-size: 10px;'><b>", UR8Name, "</b></span>",
      "<hr>",
      "<span style='font-size: 16px;'><b>Price</b></span>",
      "<br><b>Fairness:</b> ", over,
      "<br><b>Asking Price:</b>", 
      paste0(" £", scales::comma(Price), 
             " (", abs(round(diffn / 1000, digits = 0)), "k",
             ifelse(round(diffn / 1000, digits = 0) < 0, 
                    " under expected)", 
                    ifelse(round(diffn / 1000, digits = 0) > 0, 
                           " over expected)", ")"))),      
      "<br><b>Expected Price:</b>", paste0("£", scales::comma(round(expect, digits = -3))),
      "<br><b>Expected Price Range:</b>", 
      paste0("£", round(low / 1000, digits = 0), "k - ", round(upp / 1000, digits = 0), "k"),
      "<br><b><abbr title='This prediction assumes all properties were sold on the same day and were in the same location to make comparisons easier.'>Standardised Expected Price:</abbr></b>",
      paste0("£", scales::comma(round(std_expect, digits = -3))),
      "<br><b>Price per m<sup>2</sup>:</b> ", 
      paste0( "£", round(Price/FloorArea, digits = 0), " per m<sup>2</sup>"),  
      "<hr>",   
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", date,
      "<br><b>House Type:</b> ", HouseType,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      Bedrooms, " bedrooms", "<br>",
      PublicRooms, " living rooms", "<br>",
      Bathrooms, " bathrooms", "<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      FloorArea, " m<sup>2</sup>", "<br>",
      "EPC: ", toupper(epc_band), "<br>",
      "Council Tax: ", toupper(council_tax_band), "<br>",
      "</td></tr>",
      "</table>",
      "<hr>",
      "<br><a href='", property_url, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  ) %>%
  
  # Repeat for Semi-Detached
  addAwesomeMarkers(
    data = df_semi,
    icon = ~pricing[over],
    group = "Semi-Detached",
    label = ~paste0(AddressLine1, " (£", abs(round(Price / 1000, digits = 0)), "k)"),
    clusterOptions = markerClusterOptions(maxClusterRadius = 0),
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", AddressLine1, "</b></span>",
      "<br><span style='font-size: 10px;'><b>", UR8Name, "</b></span>",
      "<hr>",
      "<span style='font-size: 16px;'><b>Price</b></span>",
      "<br><b>Fairness:</b> ", over,
      "<br><b>Asking Price:</b>", 
      paste0(" £", scales::comma(Price), 
             " (", abs(round(diffn / 1000, digits = 0)), "k",
             ifelse(round(diffn / 1000, digits = 0) < 0, 
                    " under expected)", 
                    ifelse(round(diffn / 1000, digits = 0) > 0, 
                           " over expected)", ")"))),      
      "<br><b>Expected Price:</b>", paste0("£", scales::comma(round(expect, digits = -3))),
      "<br><b>Expected Price Range:</b>", 
      paste0("£", round(low / 1000, digits = 0), "k - ", round(upp / 1000, digits = 0), "k"),
      "<br><b><abbr title='This prediction assumes all properties were sold on the same day and were in the same location to make comparisons easier.'>Standardised Expected Price:</abbr></b>",
      paste0("£", scales::comma(round(std_expect, digits = -3))),
      "<br><b>Price per m<sup>2</sup>:</b> ", 
      paste0( "£", round(Price/FloorArea, digits = 0), " per m<sup>2</sup>"),  
      "<hr>",      
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", date,
      "<br><b>House Type:</b> ", HouseType,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      Bedrooms, " bedrooms", "<br>",
      PublicRooms, " living rooms", "<br>",
      Bathrooms, " bathrooms", "<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      FloorArea, " m<sup>2</sup>", "<br>",
      "EPC: ", toupper(epc_band), "<br>",
      "Council Tax: ", toupper(council_tax_band), "<br>",
      "</td></tr>",
      "</table>",
      "<hr>",
      "<br><a href='", property_url, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  ) %>%
  
  # Add markers for Terraced
  addAwesomeMarkers(
    data = df_terraced,
    icon = ~pricing[over],
    group = "Terraced",
    label = ~paste0(AddressLine1, " (£", abs(round(Price / 1000, digits = 0)), "k)"),
    clusterOptions = markerClusterOptions(maxClusterRadius = 0),
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", AddressLine1, "</b></span>",
      "<br><span style='font-size: 10px;'><b>", UR8Name, "</b></span>",
      "<hr>",
      "<span style='font-size: 16px;'><b>Price</b></span>",
      "<br><b>Fairness:</b> ", over,
      "<br><b>Asking Price:</b>", 
      paste0(" £", scales::comma(Price), 
             " (", abs(round(diffn / 1000, digits = 0)), "k",
             ifelse(round(diffn / 1000, digits = 0) < 0, 
                    " under expected)", 
                    ifelse(round(diffn / 1000, digits = 0) > 0, 
                           " over expected)", ")"))),      
      "<br><b>Expected Price:</b>", paste0("£", scales::comma(round(expect, digits = -3))),
      "<br><b>Expected Price Range:</b>", 
      paste0("£", round(low / 1000, digits = 0), "k - ", round(upp / 1000, digits = 0), "k"),
      "<br><b><abbr title='This prediction assumes all properties were sold on the same day and were in the same location to make comparisons easier.'>Standardised Expected Price:</abbr></b>",
      paste0("£", scales::comma(round(std_expect, digits = -3))),
      "<br><b>Price per m<sup>2</sup>:</b> ", 
      paste0( "£", round(Price/FloorArea, digits = 0), " per m<sup>2</sup>"),  
      "<hr>",      
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", date,
      "<br><b>House Type:</b> ", HouseType,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      Bedrooms, " bedrooms", "<br>",
      PublicRooms, " living rooms", "<br>",
      Bathrooms, " bathrooms", "<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      FloorArea, " m<sup>2</sup>", "<br>",
      "EPC: ", toupper(epc_band), "<br>",
      "Council Tax: ", toupper(council_tax_band), "<br>",
      "</td></tr>",
      "</table>",
      "<hr>",
      "<br><a href='", property_url, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  ) %>%
  
  # Add markers for Flat
  addAwesomeMarkers(
    data = df_flat,
    icon = ~pricing[over],
    group = "Flat",
    label = ~paste0(AddressLine1, " (£", abs(round(Price / 1000, digits = 0)), "k)"),
    clusterOptions = markerClusterOptions(maxClusterRadius = 0),
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", AddressLine1, "</b></span>",
      "<br><span style='font-size: 10px;'><b>", UR8Name, "</b></span>",
      "<hr>",
      "<span style='font-size: 16px;'><b>Price</b></span>",
      "<br><b>Fairness:</b> ", over,
      "<br><b>Asking Price:</b>", 
      paste0(" £", scales::comma(Price), 
             " (", abs(round(diffn / 1000, digits = 0)), "k",
             ifelse(round(diffn / 1000, digits = 0) < 0, 
                    " under expected)", 
                    ifelse(round(diffn / 1000, digits = 0) > 0, 
                           " over expected)", ")"))),      
      "<br><b>Expected Price:</b>", paste0("£", scales::comma(round(expect, digits = -3))),
      "<br><b>Expected Price Range:</b>", 
      paste0("£", round(low / 1000, digits = 0), "k - ", round(upp / 1000, digits = 0), "k"),
      "<br><b><abbr title='This prediction assumes all properties were sold on the same day and were in the same location to make comparisons easier.'>Standardised Expected Price:</abbr></b>",
      paste0("£", scales::comma(round(std_expect, digits = -3))),
      "<br><b>Price per m<sup>2</sup>:</b> ", 
      paste0( "£", round(Price/FloorArea, digits = 0), " per m<sup>2</sup>"),  
      "<hr>",      
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", date,
      "<br><b>House Type:</b> ", HouseType,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      Bedrooms, " bedrooms", "<br>",
      PublicRooms, " living rooms", "<br>",
      Bathrooms, " bathrooms", "<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      FloorArea, " m<sup>2</sup>", "<br>",
      "EPC: ", toupper(epc_band), "<br>",
      "Council Tax: ", toupper(council_tax_band), "<br>",
      "</td></tr>",
      "</table>",
      "<hr>",
      "<br><a href='", property_url, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  ) %>%

  # Add layer control
  addLayersControl(
    overlayGroups = c("Detached", "Semi-Detached", "Terraced", "Flat"),
    options = layersControlOptions(collapsed = FALSE)
  )

saveWidget(abdn_map_Price, here("C:/abdn_app", "www", "abdn_homes_pricing.html"), selfcontained = FALSE)

# Gatehouse prediction ----------------------------------------------------
gatehouse <- df[df$AddressLine1 == "Gatehouse Cottage",]
preds <- predict(m1, gatehouse, se.fit = TRUE)
preds$fit
preds$fit - preds$se.fit * 1.96
preds$fit + preds$se.fit * 1.96

gatehouse$date <- Sys.Date()
gatehouse <- gatehouse %>%
  mutate(days_since = as.numeric(difftime(date, earliest_date, units = "days")))
preds <- predict(m1, gatehouse, se.fit = TRUE)
preds$fit
preds$fit - preds$se.fit * 1.96
preds$fit + preds$se.fit * 1.96

gatehouse$epc_band <- "D"
preds <- predict(m1, gatehouse, se.fit = TRUE)
preds$fit
preds$fit - preds$se.fit * 1.96
preds$fit + preds$se.fit * 1.96

gatehouse <- df[df$AddressLine1 == "Gatehouse Cottage",]
gatehouse$epc_band <- "C"
gatehouse$Bedrooms <- 3
gatehouse$Bathrooms <- 2
gatehouse$FloorArea <- 110
preds <- predict(m1, gatehouse, se.fit = TRUE)
preds$fit
preds$fit - preds$se.fit * 1.96
preds$fit + preds$se.fit * 1.96


min_Longitude <- -2.28
max_Longitude <- -2.05
min_Latitude <- 57.22
max_Latitude <- 57.37

abdn <- get_map(location = c(gatehouse$Longitude, gatehouse$Latitude), 
                zoom = 12, 
                mapHouseType = "hybrid", 
                source = "google", 
                messaging = FALSE)
gatehouse <- df[df$AddressLine1 == "Gatehouse Cottage",]
nu_data <- expand.grid(
  Latitude = seq(min_Latitude, max_Latitude, length.out = 200),
  Longitude = seq(min_Longitude, max_Longitude, length.out = 200),
  HouseType = gatehouse$HouseType,
  UR8Name = gatehouse$UR8Name,
  epc_band = gatehouse$epc_band,
  council_tax_band = gatehouse$council_tax_band,
  rooms = gatehouse$rooms,
  Bedrooms = gatehouse$Bedrooms,
  Bathrooms = gatehouse$Bathrooms,
  PublicRooms = gatehouse$PublicRooms,
  FloorArea = gatehouse$FloorArea,
  days_since = gatehouse$days_since,
  has_garden = gatehouse$has_garden,
  num_floors = gatehouse$num_floors,
  parking_type = gatehouse$parking_type,
  SolicitorAccount_Name = gatehouse$SolicitorAccount_Name
)

nu_data$fit <- predict(m1, newdata = nu_data)

map4 <- ggmap(abdn) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), na.rm = TRUE, alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, linetype = 2, binwidth = 25000) +
  geom_point(data = df, aes(x = Longitude, y = Latitude), colour = "white") +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nPrice (£)") +
  sbsvoid_theme()
map4
