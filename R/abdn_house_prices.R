# Script to analyse house prices in Aberdeenshire
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
df <- read_sheet("https://docs.google.com/spreadsheets/d/1WQNnBK6P4ml9o8XyA9zMXXe2Rh-pY6boQEA1zR1nsus/edit?usp=sharing",
                 sheet = "Sheet1", trim_ws = TRUE)

# Data cleaning -----------------------------------------------------------
df <- df[df$house != "Blackbriggs",] # Removing property that is 3 houses
df <- df[!duplicated(paste(df$house, df$price)),] # Remove duplicates
df$rooms <- df$beds + df$living
df$date <- as.Date(df$date)
earliest_date <- min(df$date)
df <- df %>%
  mutate(days_since = as.numeric(difftime(date, earliest_date, units = "days")))

# Commute time ------------------------------------------------------------
origin <- "57.168010390142236,-2.106429897150815" # Aberdeen University
bioss <- "57.13310577362852,-2.158274358135975" # BioSS location

get_commute_info <- function(lat, lon, api_key, origin) {
  url <- paste0(
    "https://maps.googleapis.com/maps/api/distancematrix/json?units=metric",
    "&origins=", origin,
    "&destinations=", lat, ",", lon,
    "&mode=driving",
    "&key=", api_key
  )
  response <- GET(url)
  content <- fromJSON(content(response, "text"), flatten = TRUE)
  if (!is.null(content$rows$elements[[1]]$duration.value) && !is.null(content$rows$elements[[1]]$distance.value)) {
    commute_time <- content$rows$elements[[1]]$duration.value
    commute_distance <- content$rows$elements[[1]]$distance.value
    return(c(commute_time, commute_distance))
  } else {
    return(c(NA, NA))
  }
}

commute_info <- mapply(get_commute_info, df$lat, df$lon, MoreArgs = list(api_key = api_key, origin = origin))
commute_info <- as.data.frame(t(commute_info))
colnames(commute_info) <- c("commute_time", "commute_distance")
df <- cbind(df, commute_info)
df$commute_time_uni <- df$commute_time / 60
df$commute_distance_uni <- round(df$commute_distance / 1000, digits = 1)

commute_info <- mapply(get_commute_info, df$lat, df$lon, MoreArgs = list(api_key = api_key, origin = bioss))
commute_info <- as.data.frame(t(commute_info))
colnames(commute_info) <- c("commute_time_bioss", "commute_distance_bioss")
df <- cbind(df, commute_info)
df$commute_time_bioss <- df$commute_time_bioss / 60
df$commute_distance_bioss <- round(df$commute_distance_bioss / 1000, digits = 1)

# Price per square meter --------------------------------------------------
df$price_sqmt <- df$price / df$sqmt

# Urban/rural classification ----------------------------------------------
shapefile_path <- "C:/abdn_house_prices/data/SG_UrbanRural_2020/SG_UrbanRural_2020.shp"
urban_rural_shp <- st_read(shapefile_path)
df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
urban_rural_shp <- st_transform(urban_rural_shp, crs = st_crs(df_sf))
df <- as.data.frame(st_join(df_sf, urban_rural_shp, left = TRUE))
coordinates <- st_coordinates(df_sf)
df$lon <- coordinates[, 1]  # Longitude (X)
df$lat <- coordinates[, 2]  # Latitude (Y)
df$geometry <- NULL

# EDA ---------------------------------------------------------------------

ggplot(df, aes(x = price, fill = type)) +
  geom_density(alpha = 0.6) +
  labs(x = "Price", y = "Density")

df |> 
  mutate(date = as.Date(date, format = "%Y-%m-%d")) |> 
  mutate(fortnight = floor_date(date, unit = "week") - days(1) + weeks((as.integer(difftime(date, floor_date(date, unit = "week"), units = "days")) %/% 14) * 2)) |> 
  group_by(fortnight, type) |> 
  summarise(count = n(), .groups = 'drop') |> 
  ggplot(aes(x = fortnight, y = count, color = type)) +
  geom_point() +
  geom_line(size = 0.5) +
  labs(
    x = "Fortnight",
    y = "Number of Listings",
    color = "House Type"
  )

ggplot(df, aes(x = price, fill = UR8Name)) +
  geom_density(alpha = 0.6) +
  labs(x = "Price", y = "Density")

ggplot(df, aes(x = tax, y = price, fill = type)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "Tax", y = "Price")

ggplot(df, aes(x = epc, y = price, fill = type)) +
  geom_boxplot(alpha = 0.6) +
  labs(x = "EPC", y = "Price")

# The AI MaCHiNE LeARniNg model -------------------------------------------
mp <- list(c(3, 0.1, 1), c(3, 0.1, 1))
df$type <- factor(df$type)
df$UR8Name <- factor(df$UR8Name)

m1 <- gam(price ~ 
            te(lon, lat, k = 23, m = mp, bs = "gp") +
            te(sqmt, rooms, k = 8, bs = "cr") +
            s(days_since, by = type, k = 10, bs = "cr") +
            UR8Name : type +
            UR8Name + 
            type + 
            baths + 
            epc + 
            tax,
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
    diffn = price - expect,
    over = case_when(
      price > upp ~ "Overpriced",
      price < low ~ "Underpriced",
      TRUE ~ "Fairly priced"
    )
  )

# Viewing criteria --------------------------------------------------------
df <- df |> 
  mutate(
    viewing = ifelse(
      ((over == "Underpriced" | over == "Fairly priced") & price <= 250000) | 
        ((over == "Overpriced" | over == "Fairly priced") & expect <= 250000) &
        sqmt > 80 & type != "terrace" & commute_time_uni < 30 & rooms >= 3,
      "View", "Meh")
  )

# House price prediction for a specific house -----------------------------
dream_house <- data.frame(
  lat = 57.15483899436254,
  lon = -2.269886390197508,
  type = "detached",
  UR8Name = "Accessible Rural Areas",
  rooms = 4, baths = 2, epc = "c", tax = "d", sqmt = 100,
  days_since = as.numeric(ymd(Sys.Date()) - earliest_date)
)

prds <- predict(m1, newdata = dream_house, se.fit = TRUE)
paste0("# £", round(prds$fit, digits = -3)/1000, "k [£",
       round(prds$fit - 1.96 * prds$se.fit, digits = -3)/1000, "k-£",
       round(prds$fit + 1.96 * prds$se.fit, digits = -3)/1000, "k] (n = ",
       nrow(df), ") ", stringr::str_to_title(dream_house$type)
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
# £240k [£215k-£264k] (n = 385) Detached (dropped baths to 2 and sqmt to 100)
# £241k [£216k-£265k] (n = 394) Detached
# £242k [£218k-£267k] (n = 410) Detached
# £242k [£218k-£267k] (n = 412) Detached
# £264k [£233k-£295k] (n = 511) Detached
# £262k [£232k-£293k] (n = 541) Detached
# £259k [£229k-£289k] (n = 548) Detached
# £251k [£224k-£279k] (n = 577) Detached
# £238k [£216k-£261k] (n = 601) Detached
# £236k [£213k-£259k] (n = 604) Detached
# £243k [£217k-£269k] (n = 604) Detached (Manually specified Matern params for shorter spatial aurocorrelation)
# £239k [£215k-£263k] (n = 612) Detached
# £240k [£217k-£264k] (n = 629) Detached
# £246k [£226k-£266k] (n = 773) Detached
# £245k [£225k-£264k] (n = 796) Detached
# £244k [£225k-£263k] (n = 824) Detached
# £239k [£219k-£260k] (n = 824) Detached (model now includes house type specific time relationship)
# £245k [£225k-£266k] (n = 873) Detached (more k for GP and time)
# £245k [£225k-£266k] (n = 913) Detached
# £246k [£226k-£265k] (n = 983) Detached
# £249k [£229k-£270k] (n = 1067) Detached
# £251k [£230k-£273k] (n = 1125) Detached
# £262k [£236k-£289k] (n = 1215) Detached (included urban rural designations to model)
# £265k [£239k-£292k] (n = 1309) Detached

## Over time ---------------------------------------------------------------

nu_data <- expand.grid(
  lat = median(df$lat), 
  lon = median(df$lon),
  type = unique(df$type),
  epc = "c",
  tax = "e",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
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
  facet_wrap(~type) +
  labs(y = "Asking price",
       x = "Date")
p1

## M:Room Interaction ----------------------------------------------------------------

nu_data <- expand.grid(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  UR8Name = "Accessible Rural Areas",
  epc = "c",
  tax = "e",
  rooms = seq(min(df$rooms), max(df$rooms), length.out = 25),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = seq(min(df$sqmt), max(df$sqmt), length.out = 25),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data)
nu_data$fit <- prds

nu_data$fit[exclude.too.far(
  nu_data$rooms, nu_data$sqmt,
  df$rooms, df$sqmt,
  dist = 0.1)] <- NA

p2 <- ggplot() +
  geom_raster(data = nu_data, aes(x = rooms, y = sqmt, fill = fit)) +
  scale_fill_viridis_c(option = "magma", na.value = "transparent", labels = scales::comma) +
  labs(y = "Square meters",
       x = "Rooms",
       fill = "Expected\nprice")
p2

## House type --------------------------------------------------------------
nu_data <- data.frame(
  lat = median(df$lat), 
  lon = median(df$lon),
  type = c("semi", "detached", "terrace"),
  UR8Name = "Accessible Rural Areas",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
  epc = "c",
  tax = "e",
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p3 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = type, y = fit, ymin = low, ymax = upp), width = 0.1, colour = "white") +
  geom_point(data = nu_data, aes(x = type, y = fit), size = 3.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking price",
       x = "House type")
p3

## House type:Urban--------------------------------------------------------------
nu_data <- expand.grid(
  lat = median(df$lat), 
  lon = median(df$lon),
  type = c("semi", "detached", "terrace"),
  UR8Name = unique(df$UR8Name),
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
  epc = "c",
  tax = "e",
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p4 <- ggplot() +
  geom_errorbar(data = nu_data, 
                aes(x = type, y = fit, ymin = low, ymax = upp, colour = UR8Name), 
                width = 0.1, position = position_dodge(width = 0.5)) +
  geom_point(data = nu_data, 
             aes(x = type, y = fit, colour = UR8Name), 
             size = 3.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_color_discrete(labels = function(x) str_wrap(x, width = 15)) +
  labs(y = "Asking price",
       x = "House type",
       colour = "Urban|Rural")
p4

## Baths ----------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  UR8Name = "Accessible Rural Areas",
  epc = "c",
  tax = "e",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = seq(min(df$baths), max(df$baths), length.out = 25),
  living = median(df$living),
  sqmt = median(df$sqmt),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p5 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = baths, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = baths, y = fit)) +
  labs(y = "Asking price",
       x = "Bathrooms")
p5


## EPC ------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  UR8Name = "Accessible Rural Areas",
  epc = unique(df$epc),
  tax = "e",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p6 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = epc, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = epc, y = fit), size = 2.5) +
  labs(y = "Asking price",
       x = "EPC")
p6

## Tax ------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  UR8Name = "Accessible Rural Areas",
  epc = "c",
  tax = unique(df$tax),
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data <- nu_data |> 
  mutate(fit = prds$fit, low = fit - 1.96 * prds$se.fit, upp = fit + 1.96 * prds$se.fit)

p7 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = tax, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = tax, y = fit), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking price",
       x = "Tax band")
p7


## Urban|Rural -------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  UR8Name = unique(df$UR8Name),
  epc = "c",
  tax = "e",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
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
  labs(y = "Asking price",
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
  geom_point(aes(x = expect, y = price), size = 1) +
  geom_abline(intercept = 0, slope = 1, colour = "white") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Listed price (£)",
       x = "Predicted price (£)") +
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
lon_bar <- (min(df$lon, na.rm = TRUE) + max(df$lon, na.rm = TRUE))/2
lat_bar <- (min(df$lat, na.rm = TRUE) + max(df$lat, na.rm = TRUE))/2

abdnshire <- get_map(location = c(lon_bar, lat_bar), 
                     zoom = 8, 
                     maptype = "hybrid", 
                     source = "google", 
                     messaging = FALSE)

nu_data <- expand.grid(
  lat = seq(min(df$lat), max(df$lat), length.out = 200),
  lon = seq(min(df$lon)-0.1, max(df$lon), length.out = 200),
  type = "detached",
  UR8Name = "Accessible Rural Areas",
  epc = "c",
  tax = "e",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
  days_since = median(df$days_since)
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$lat, nu_data$lon,
  df$lat, df$lon,
  dist = 0.04)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map1 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = lon , y = lat, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5, linetype = 2, binwidth = 25000) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice (£)") +
  sbsvoid_theme()
map1

## Aberdeenshire close -----------------------------------------------------

# get map of whole region for plotting
lon_bar <- (min(df$lon, na.rm = TRUE) + max(df$lon, na.rm = TRUE))/2
lat_bar <- (min(df$lat, na.rm = TRUE) + max(df$lat, na.rm = TRUE))/2

abdnshire <- get_map(location = c(lon_bar, lat_bar), 
                zoom = 9, 
                maptype = "hybrid", 
                source = "google", 
                messaging = FALSE)

nu_data <- expand.grid(
  lat = seq(min(df$lat), max(df$lat), length.out = 200),
  lon = seq(min(df$lon)-0.1, max(df$lon), length.out = 200),
  type = "detached",
  UR8Name = "Accessible Rural Areas",
  epc = "c",
  tax = "e",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
  days_since = median(df$days_since)
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$lat, nu_data$lon,
  df$lat, df$lon,
  dist = 0.04)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map2 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = lon , y = lat, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5, linetype = 2, binwidth = 25000) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice (£)") +
  sbsvoid_theme()
map2

## Aberdeen ----------------------------------------------------------------

min_lon <- -2.25
max_lon <- -2.03
min_lat <- 57.06
max_lat <- 57.21

abdn <- get_map(location = c(-2.1433691553190624, 57.149481894948565), 
                zoom = 12, 
                maptype = "hybrid", 
                source = "google", 
                messaging = FALSE)

nu_data <- expand.grid(
  lat = seq(min_lat, max_lat, length.out = 200),
  lon = seq(min_lon, max_lon, length.out = 200),
  type = "detached",
  UR8Name = "Accessible Rural Areas",
  epc = "c",
  tax = "e",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
  days_since = median(df$days_since)
)

nu_data$fit <- predict(m1, newdata = nu_data)

nu_data$fit[exclude.too.far(
  nu_data$lat, nu_data$lon,
  df$lat, df$lon,
  dist = 0.1)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

map3 <- ggmap(abdn) +
  geom_tile(data = nu_data, aes(x = lon , y = lat, fill = fit), na.rm = TRUE, alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5, binwidth = 50000) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5, linetype = 2, binwidth = 25000) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice (£)") +
  sbsvoid_theme()
map3

p_maps <- map2 / map3
ggsave("C:/abdn_app/www/plot_maps.png", plot = p_maps)

# Leaflet map pricing --------------------------------------------------------

pricing <- awesomeIconList(
  "Underpriced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "green",
    library = "fa"
  ),
  "Fairly priced" = makeAwesomeIcon(
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

abdn_map_price <- leaflet(df) %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addAwesomeMarkers(
    data = df,
    lng = ~lon, lat = ~lat,
    icon = ~ pricing[over],
    label = ~paste0(
      house, " (£", abs(round(price / 1000, digits = 0)), "k)"
    ),
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", house, "</b></span>",
      "<br><span style='font-size: 10px;'><b>", UR8Name, "</b></span>",
      "<hr>",
      "<span style='font-size: 16px;'><b>Price</b></span>",
      "<br><b>Fairness:</b> ", over,
      "<br><b>Asking Price:</b>", 
      paste0(" £", scales::comma(price), 
             " (", abs(round(diffn / 1000, digits = 0)), "k",
             ifelse(round(diffn / 1000, digits = 0) < 0, 
                    " under expected)", 
                    ifelse(round(diffn / 1000, digits = 0) > 0, 
                           " over expected)", ")"))),      
      "<br><b>Expected Price:</b>", paste0("£", scales::comma(expect)),
      "<br><b>Expected Price Range:</b>", 
      paste0("£", round(low / 1000, digits = 0), "k - ", round(upp / 1000, digits = 0), "k"),
      "<br><b>Price per m<sup>2</sup>:</b> ", 
      paste0( "£", round(price/sqmt, digits = 0), " per m<sup>2</sup>"),  
      "<hr>",      
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", date,
      "<br><b>House type:</b> ", type,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      beds, " bedrooms", "<br>",
      living, " living rooms", "<br>",
      baths, " bathrooms", "<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      sqmt, " m<sup>2</sup>", "<br>",
      "EPC: ", toupper(epc), "<br>",
      "Tax: ", toupper(tax), "<br>",
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

abdn_map_price

saveWidget(abdn_map_price, here("C:/abdn_app", "www", "abdn_homes_pricing.html"), selfcontained = TRUE)

