# Script to analyse house Prices in Aberdeenshire
# Deon Roos

# Packages ----------------------------------------------------------------
library(googlesheets4) 
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

dotenv::load_dot_env()
register_google(key = Sys.getenv("GOOGLE_MAPS_API_KEY"))
api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
source("sbs_theme.R")
theme_set(sbs_theme())


# Load data ---------------------------------------------------------------
# df <- read_sheet("https://docs.google.com/spreadsheets/d/1WQNnBK6P4ml9o8XyA9zMXXe2Rh-pY6boQEA1zR1nsus/edit?usp=sharing",
#                  sheet = "Sheet1", trim_ws = TRUE)
file_path <- "property_data.csv"
df <- read.csv(file_path, header = TRUE)

# Data cleaning -----------------------------------------------------------

# Removing beaurocrat variables
df <- df |> 
  select(-PropertyType, -PriceType, -RentalPricePeriod, -UnderOffer, -FormattedClosingDate,
         -FormattedClosedDate, -OrganisationName, -County, -Country, -CoordinateSystemId, 
         -WellKnownText, -ClosingDateVisible, -ClosedDateVisible, -ClosedMessage, -HideOpenActionsDisplay,
         -ClosingDate, -ClosedDate, -IsOpen, -ResidentialType, -HouseFormat, -SolicitorAccount_Id,
         -SolicitorAccount_WebsiteAddress, -FormattedPrice, -ViewingArrangements, -ViewingTiming,
         -BookingUrl, -IsSellerManaged,
         -CategorisationDescription)

#df <- df[df$AddressLine1 != "Blackbriggs",] # Removing property that is 3 houses
df <- df[!duplicated(paste(df$AddressLine1, df$Price)),] # Remove duplicates
df$rooms <- df$Bedrooms + df$PublicRooms
df$date <- as.Date(df$DateAdded)
earliest_date <- min(df$date)
df <- df %>%
  mutate(days_since = as.numeric(difftime(date, earliest_date, units = "days")))

df$FloorArea <- ifelse(df$FloorArea == 0, NA, df$FloorArea)
df$council_tax_band <- ifelse(df$council_tax_band == "N", NA, df$council_tax_band)

df$HouseType <- ifelse(is.na(df$HouseType), "Flat", df$HouseType)

df <- df |> 
  drop_na()

# # Commute time ------------------------------------------------------------
# origin <- "57.168010390142236,-2.106429897150815" # Aberdeen University
# bioss <- "57.13310577362852,-2.158274358135975" # BioSS location
# 
# get_commute_info <- function(Latitude, Longitude, api_key, origin) {
#   url <- paste0(
#     "https://maps.googleapis.com/maps/api/distancematrix/json?units=metric",
#     "&origins=", origin,
#     "&destinations=", Latitude, ",", Longitude,
#     "&mode=driving",
#     "&key=", api_key
#   )
#   response <- GET(url)
#   content <- fromJSON(content(response, "text"), fLatitudeten = TRUE)
#   if (!is.null(content$rows$elements[[1]]$duration.value) && !is.null(content$rows$elements[[1]]$distance.value)) {
#     commute_time <- content$rows$elements[[1]]$duration.value
#     commute_distance <- content$rows$elements[[1]]$distance.value
#     return(c(commute_time, commute_distance))
#   } else {
#     return(c(NA, NA))
#   }
# }
# 
# commute_info <- mapply(get_commute_info, df$Latitude, df$Longitude, MoreArgs = list(api_key = api_key, origin = origin))
# commute_info <- as.data.frame(t(commute_info))
# colnames(commute_info) <- c("commute_time", "commute_distance")
# df <- cbind(df, commute_info)
# df$commute_time_uni <- df$commute_time / 60
# df$commute_distance_uni <- round(df$commute_distance / 1000, digits = 1)
# 
# commute_info <- mapply(get_commute_info, df$Latitude, df$Longitude, MoreArgs = list(api_key = api_key, origin = bioss))
# commute_info <- as.data.frame(t(commute_info))
# colnames(commute_info) <- c("commute_time_bioss", "commute_distance_bioss")
# df <- cbind(df, commute_info)
# df$commute_time_bioss <- df$commute_time_bioss / 60
# df$commute_distance_bioss <- round(df$commute_distance_bioss / 1000, digits = 1)

# Price per square meter --------------------------------------------------
df$Price_FloorArea <- df$Price / df$FloorArea

# Urban/rural classification ----------------------------------------------
shapefile_path <- "C:/abdn_house_Prices/data/SG_UrbanRural_2020/SG_UrbanRural_2020.shp"
urban_rural_shp <- st_read(shapefile_path)
df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)
urban_rural_shp <- st_transform(urban_rural_shp, crs = st_crs(df_sf))
df <- as.data.frame(st_join(df_sf, urban_rural_shp, left = TRUE))
coordinates <- st_coordinates(df_sf)
df$Longitude <- coordinates[, 1]  # Longitudegitude (X)
df$Latitude <- coordinates[, 2]  # Latitudeitude (Y)
df$geometry <- NULL

# EDA ---------------------------------------------------------------------

ggplot(df, aes(x = Price, fill = HouseType)) +
  geom_density(alpha = 0.6) +
  labs(x = "Price", y = "Density")

df |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |> 
  mutate(fortnight = floor_date(date, unit = "week") - days(1) + weeks((as.integer(difftime(date, floor_date(date, unit = "week"), units = "days")) %/% 14) * 2)) |> 
  group_by(fortnight, HouseType) |> 
  summarise(count = n(), .groups = 'drop') |> 
  ggplot(aes(x = fortnight, y = count, color = HouseType)) +
  geom_point() +
  geom_line(size = 0.5) +
  labs(
    x = "Fortnight",
    y = "Number of Listings",
    color = "House HouseType"
  )

ggplot(df, aes(x = Price, fill = UR8Name)) +
  geom_density(alpha = 0.6) +
  labs(x = "Price", y = "Density")

ggplot(df, aes(x = council_tax_band, y = Price, fill = HouseType)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "council_tax_band", y = "Price")

ggplot(df, aes(x = epc_band, y = Price, fill = HouseType)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "epc_band", y = "Price")

# The AI MaCHiNE LeARniNg model -------------------------------------------
mp <- list(c(3, 5000, 1), c(3, 5000, 1))
df$HouseType <- factor(df$HouseType)
df$epc_band <- factor(df$epc_band)
df$council_tax_band <- factor(df$council_tax_band)
df$UR8Name <- factor(df$UR8Name)

m1 <- gam(Price ~ 
            te(UTM_Easting, UTM_Northing, k = 23, m = mp, bs = "gp") +
            te(FloorArea, rooms, k = 8, bs = "cr") +
            #s(days_since, by = HouseType, k = 10, bs = "cr") +
            # UR8Name : HouseType +
            # UR8Name + 
            HouseType + 
            Bathrooms + 
            epc_band + 
            council_tax_band,
          data = df, method = "REML")

# Save model
saveRDS(m1, file = "C:/abdn_app/data/model_m1.rds")

# Predictions -------------------------------------------------------------
prds <- predict(m1, se.fit = TRUE)
df <- df |> 
  mutate(
    expect = round(prds$fit, digits = 0),
    low = round(prds$fit - 1.96 * prds$se.fit),
    upp = round(prds$fit + 1.96 * prds$se.fit),
    diffn = Price - expect,
    over = case_when(
      Price > upp ~ "OverPriced",
      Price < low ~ "UnderPriced",
      TRUE ~ "Fairly Priced"
    )
  )

# Viewing criteria --------------------------------------------------------
df <- df |> 
  mutate(
    viewing = ifelse(
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
  rooms = 4, Bathrooms = 2, epc_band = "C", council_tax_band = "D", FloorArea = 100,
  days_since = as.numeric(ymd(Sys.Date()) - earliest_date)
)

prds <- predict(m1, newdata = dream_house, se.fit = TRUE)
paste0("# £", round(prds$fit, digits = -3)/1000, "k [£",
       round(prds$fit - 1.96 * prds$se.fit, digits = -3)/1000, "k-£",
       round(prds$fit + 1.96 * prds$se.fit, digits = -3)/1000, "k] (n = ",
       nrow(df), ") ", stringr::str_to_title(dream_house$HouseType)
)
# £240k (n = 175)
# £246k (n = 201)
# £233k [£208k-£257k] (n = 235) Semi
# £229k [£204k-£254k] (n = 237) Semi
# £226k [£202k-£250k] (n = 261) Semi
# £236k [£212k-£259k] (n = 261) Detached
# £236k [£212k-£260k] (n = 271) Detached
# £241k [£219k-£262k] (n = 296) Detached
# £240k [£219k-£262k] (n = 299) Detached
# £244k [£222k-£266k] (n = 301) Detached
# £240k [£220k-£261k] (n = 332) Detached
# £239k [£220k-£259k] (n = 385) Detached
# £240k [£220k-£259k] (n = 383) Detached (Removed stupid multi-million £ mansion)
# £243k [£222k-£263k] (n = 383) Detached (Including time in model)
# £253k [£226k-£281k] (n = 385) Detached (with GP, k = 20 for space)
# £240k [£215k-£264k] (n = 385) Detached (dropped Bathrooms to 2 and FloorArea to 100)
# £241k [£216k-£265k] (n = 394) Detached
# £242k [£218k-£267k] (n = 410) Detached
# £242k [£218k-£267k] (n = 412) Detached
# £264k [£233k-£295k] (n = 511) Detached
# £262k [£232k-£293k] (n = 541) Detached
# £259k [£229k-£289k] (n = 548) Detached
# £251k [£224k-£279k] (n = 577) Detached
# £238k [£216k-£261k] (n = 601) Detached
# £236k [£213k-£259k] (n = 604) Detached
# £243k [£217k-£269k] (n = 604) Detached (Manually specified Matern params for shorter spatial aurocorreLatitudeion)
# £239k [£215k-£263k] (n = 612) Detached
# £240k [£217k-£264k] (n = 629) Detached
# £246k [£226k-£266k] (n = 773) Detached
# £245k [£225k-£264k] (n = 796) Detached
# £244k [£225k-£263k] (n = 824) Detached
# £239k [£219k-£260k] (n = 824) Detached (model now includes house HouseType specific time reLatitudeionship)
# £245k [£225k-£266k] (n = 873) Detached (more k for GP and time)
# £245k [£225k-£266k] (n = 913) Detached
# £246k [£226k-£265k] (n = 983) Detached
# £249k [£229k-£270k] (n = 1067) Detached
# £251k [£230k-£273k] (n = 1125) Detached
# £262k [£236k-£289k] (n = 1215) Detached (included urban rural designations to model)
# £265k [£239k-£292k] (n = 1309) Detached
# £267k [£249k-£286k] (n = 4472) Detached (now scrapping from ASPC)

## Over time ---------------------------------------------------------------

nu_data <- expand.grid(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = unique(df$HouseType),
  epc_band = "c",
  council_tax_band = "e",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  UR8Name = "Accessible Rural Areas",
  days_since = seq(min(df$days_since), max(df$days_since), length.out = 25)
)
nu_data$date <- min(df$date) + nu_data$days_since
prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p1 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = date, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = date, y = fit)) +
  facet_wrap(~HouseType) +
  labs(y = "Asking Price",
       x = "Date")
p1

## M:Room Interaction ----------------------------------------------------------------

nu_data <- expand.grid(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  HouseType = "Detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "C",
  council_tax_band = "E",
  rooms = seq(min(df$rooms), max(df$rooms), length.out = 25),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = seq(min(df$FloorArea), max(df$FloorArea), length.out = 25),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data)
nu_data$fit <- prds

nu_data$fit[exclude.too.far(
  nu_data$rooms, nu_data$FloorArea,
  df$rooms, df$FloorArea,
  dist = 0.1)] <- NA

p2 <- ggplot() +
  geom_raster(data = nu_data, aes(x = rooms, y = FloorArea, fill = fit)) +
  scale_fill_viridis_c(option = "magma", na.value = "transparent", labels = scales::comma) +
  labs(y = "Square meters",
       x = "Rooms",
       fill = "Expected\nPrice")
p2

## House HouseType --------------------------------------------------------------
nu_data <- data.frame(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = c("semi", "detached", "terrace"),
  UR8Name = "Accessible Rural Areas",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  epc_band = "c",
  council_tax_band = "e",
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p3 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = HouseType, y = fit, ymin = low, ymax = upp), width = 0.1, colour = "white") +
  geom_point(data = nu_data, aes(x = HouseType, y = fit), size = 3.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "House HouseType")
p3

## House HouseType:Urban--------------------------------------------------------------
nu_data <- expand.grid(
  Latitude = median(df$Latitude), 
  Longitude = median(df$Longitude),
  HouseType = c("semi", "detached", "terrace"),
  UR8Name = unique(df$UR8Name),
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  epc_band = "c",
  council_tax_band = "e",
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p4 <- ggplot() +
  geom_errorbar(data = nu_data, 
                aes(x = HouseType, y = fit, ymin = low, ymax = upp, colour = UR8Name), 
                width = 0.1, position = position_dodge(width = 0.5)) +
  geom_point(data = nu_data, 
             aes(x = HouseType, y = fit, colour = UR8Name), 
             size = 3.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_color_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(y = "Asking Price",
       x = "House HouseType",
       colour = "Urban|Rural")
p4

## Bathrooms ----------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  HouseType = "detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "c",
  council_tax_band = "e",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = seq(min(df$Bathrooms), max(df$Bathrooms), length.out = 25),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p5 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = Bathrooms, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = Bathrooms, y = fit)) +
  labs(y = "Asking Price",
       x = "Bathrooms")
p5


## epc_band ------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  HouseType = "detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = unique(df$epc_band),
  council_tax_band = "e",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p6 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = epc_band, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = epc_band, y = fit), size = 2.5) +
  labs(y = "Asking Price",
       x = "epc_band")
p6

## council_tax_band ------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  HouseType = "detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "c",
  council_tax_band = unique(df$council_tax_band),
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p7 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = council_tax_band, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = council_tax_band, y = fit), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking Price",
       x = "council_tax_band band")
p7


## Urban|Rural -------------------------------------------------------------

nu_data <- data.frame(
  Latitude = median(df$Latitude),
  Longitude = median(df$Longitude),
  HouseType = "detached",
  UR8Name = unique(df$UR8Name),
  epc_band = "c",
  council_tax_band = "e",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p8 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = UR8Name, y = fit, ymin = low, ymax = upp, colour = UR8Name), width = 0.1, linewidth = 1) +
  geom_point(data = nu_data, aes(x = UR8Name, y = fit, colour = UR8Name), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_color_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(y = "Asking Price",
       x = "Urban | Rural")
p8

## Predicted versus response -----------------------------------------------

df$low1 <- 0 - sigma(m1) + df$expect
df$upp1 <- 0 + sigma(m1) + df$expect
df$low2 <- 0 - 2 * sigma(m1) + df$expect
df$upp2 <- 0 + 2 * sigma(m1) + df$expect

p9 <- ggplot(df) +
  geom_ribbon(aes(x = expect, ymin = low1, ymax = upp1), fill = "red", alpha = 0.4) +
  geom_ribbon(aes(x = expect, ymin = low2, ymax = upp2), fill = "red", alpha = 0.4) +
  geom_point(aes(x = expect, y = Price), size = 1) +
  geom_abline(intercept = 0, slope = 1, colour = "white") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Listed Price (£)",
       x = "Predicted Price (£)") +
  sbs_theme()

design <- "
CCDD
EEFF
GGHH
IIJJ
#KK#
"

p_figs <- p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + plot_layout(design = design)
p_figs

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
  Longitude = seq(min(df$Longitude)-0.1, max(df$Longitude), length.out = 200),
  HouseType = "detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "c",
  council_tax_band = "e",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since)
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$Latitude, nu_data$Longitude,
  df$Latitude, df$Longitude,
  dist = 0.04)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map1 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, lineHouseType = 2, binwidth = 25000) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitudegitude",
       y = "Latitudeitude",
       fill = "Expected\nPrice (£)") +
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
  Longitude = seq(min(df$Longitude)-0.1, max(df$Longitude), length.out = 200),
  HouseType = "detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "c",
  council_tax_band = "e",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since)
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$Latitude, nu_data$Longitude,
  df$Latitude, df$Longitude,
  dist = 0.04)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map2 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, lineHouseType = 2, binwidth = 25000) +
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
  HouseType = "detached",
  UR8Name = "Accessible Rural Areas",
  epc_band = "c",
  council_tax_band = "e",
  rooms = median(df$rooms),
  Bedrooms = median(df$Bedrooms),
  Bathrooms = median(df$Bathrooms),
  PublicRooms = median(df$PublicRooms),
  FloorArea = median(df$FloorArea),
  days_since = median(df$days_since)
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$Latitude, nu_data$Longitude,
  df$Latitude, df$Longitude,
  dist = 0.1)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map3 <- ggmap(abdn) +
  geom_tile(data = nu_data, aes(x = Longitude , y = Latitude, fill = fit), na.rm = TRUE, alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = Longitude , y = Latitude, z = fit), colour = "white", size = 0.5, lineHouseType = 2, binwidth = 25000) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitudegitude",
       y = "Latitudeitude",
       fill = "Expected\nPrice (£)") +
  sbsvoid_theme()
map3

p_maps <- map2 / map3
ggsave("C:/abdn_app/www/plot_maps.png", plot = p_maps)

# Leaflet map pricing --------------------------------------------------------

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

abdn_map_Price <- leaflet(df) %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addAwesomeMarkers(
    data = df,
    lng = ~Longitude, Latitude = ~Latitude,
    icon = ~ pricing[over],
    label = ~paste0(
      house, " (£", abs(round(Price / 1000, digits = 0)), "k)"
    ),
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", house, "</b></span>",
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
      "<br><b>Expected Price:</b>", paste0("£", scales::comma(expect)),
      "<br><b>Expected Price Range:</b>", 
      paste0("£", round(low / 1000, digits = 0), "k - ", round(upp / 1000, digits = 0), "k"),
      "<br><b>Price per m<sup>2</sup>:</b> ", 
      paste0( "£", round(Price/FloorArea, digits = 0), " per m<sup>2</sup>"),  
      "<hr>",      
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", date,
      "<br><b>House HouseType:</b> ", HouseType,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      Bedrooms, " bedrooms", "<br>",
      PublicRooms, " PublicRooms rooms", "<br>",
      Bathrooms, " bathrooms", "<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      FloorArea, " m<sup>2</sup>", "<br>",
      "epc_band: ", toupper(epc_band), "<br>",
      "council_tax_band: ", toupper(council_tax_band), "<br>",
      "</td></tr>",
      "</table>",
      "<hr>",
      "<span style='font-size: 16px;'><b>Commutes</b></span>",
      "<table style='width:100%;'>",
      "<tr>",
      "<td><b>To Aberdeen Uni</b></td>",
      "<td><b>To BioSS</b></td>",
      "</tr>",
      "<tr>",
      "<td>Time: ", round(commute_time_uni), " minutes</td>",
      "<td>Time: ", round(commute_time_bioss), " minutes</td>",
      "</tr>",
      "<tr>",
      "<td>Distance: ", commute_distance_uni, " km</td>",
      "<td>Distance: ", commute_distance_bioss, " km</td>",
      "</tr>",
      "</table>",
      "<hr>",
      "<br><a href='", link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  )

abdn_map_Price

saveWidget(abdn_map_Price, here("C:/abdn_app", "www", "abdn_homes_pricing.html"), selfcontained = TRUE)

