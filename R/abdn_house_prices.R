# Script to analyse house prices in Aberdeenshire
# Deon Roos

# Packages ----------------------------------------------------------------
library(googlesheets4) # For loading in the data from google sheets
library(ggplot2)       # For data visualiations
library(patchwork)
library(mgcv)          # For non-linear models
library(dplyr)
library(leaflet)
library(ggmap); register_google(key = "AIzaSyDZKT-rGxUv21PxNFO4elG-n4m31gOqn_M")
library(lubridate)
library(htmlwidgets)
library(geosphere)
library(httr)
library(jsonlite)
source("sbs_theme.R")
theme_set(sbs_theme())

df <- read_sheet("https://docs.google.com/spreadsheets/d/1WQNnBK6P4ml9o8XyA9zMXXe2Rh-pY6boQEA1zR1nsus/edit?usp=sharing",
                 sheet = "Sheet1", 
                 trim_ws = TRUE)

# Code to check if house has been added -----------------------------------
# When doing data entry
# house <- "18 Cypress Walk"
# df[df$house == house,]
df <- df[df$house != "Blackbriggs",] # Weird house that's actually 3 houses so inflates local area price
 
# Remove duplicates -------------------------------------------------------
df$house[duplicated(paste(df$house, df$price))]
df[duplicated(paste(df$house, df$price)),]
df <- df[!duplicated(paste(df$house, df$price)),]
df$rooms <- df$beds + df$living

df$date <- as.Date(df$date)
earliest_date <- min(df$date)
df <- df %>%
  mutate(days_since = as.numeric(difftime(date, earliest_date, units = "days")))

df |> 
  group_by(type) |> 
  summarise(
    avg_price = median(price, na.rm = TRUE),
    avg_rooms = median(rooms, na.rm = TRUE),
    avg_sqmt = median(sqmt, na.rm = TRUE),
    most_common_tax = names(which.max(table(tax))),
    most_common_epc = names(which.max(table(epc)))
  )

# The AI MaCHiNE LeARniNg model -------------------------------------------
mp <- list(
  c(3, 0.1, 1), # 3 for GP, 0.1 ca. 10 km range of rho, 1 as default for scale
  c(3, 0.1, 1)
)
df$type <- factor(df$type)
m1 <- gam(price ~ 
            te(lon, lat, k = 23, m = mp, bs = "gp") +
            te(sqmt, rooms, k = 8, bs = "cr") +
            s(days_since, by = type, k = 10, bs = "cr") +
            type +
            baths +
            epc +
            tax,
          data = df,
          method = "REML")

saveRDS(m1, file = "C:/abdn_app/data/model_m1.rds")

## Generate predictions ----------------------------------------------------
prds <- predict(m1, se.fit = TRUE)
df$expect <- round(prds$fit, digits = 0)
df$low <- round(prds$fit - 1.96 * prds$se.fit)
df$upp <- round(prds$fit + 1.96 * prds$se.fit)

## Process for pop up ------------------------------------------------------

df$diffn <- df$price - df$expect
df$diff <- scales::comma(df$price - df$expect)

slice_max(df, diffn, n = 10)

df$over <- ifelse(df$price > df$upp, "Overpriced", 
                  ifelse(df$price < df$low, "Underpriced", 
                         "Fairly priced"))

## Commute time ------------------------------------------------------------

api_key <- "AIzaSyDZKT-rGxUv21PxNFO4elG-n4m31gOqn_M"
origin <- "57.168010390142236,-2.106429897150815" # abdn uni
bioss <- "57.13310577362852,-2.158274358135975"
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
df$commute_time_minutes <- df$commute_time / 60
df$commute_distance <- round(df$commute_distance / 1000, digits = 1)

commute_info <- mapply(get_commute_info, df$lat, df$lon, MoreArgs = list(api_key = api_key, origin = bioss))
commute_info <- as.data.frame(t(commute_info))
colnames(commute_info) <- c("commute_time_bioss", "commute_distance_bioss")
df <- cbind(df, commute_info)
df$commute_time_minutes_bioss <- df$commute_time_bioss / 60
df$commute_distance_bioss <- round(df$commute_distance_bioss / 1000, digits = 1)

# Price per sqmt ----------------------------------------------------------
df$price_sqmt <- df$price / df$sqmt

## Vieweing criteria ----------------------------------------------------------

df$viewing <- ifelse((
  ((df$over == "Underpriced" | df$over == "Fairly priced") & df$price <= 250000) |   # Underpriced and in price range                  
    ((df$over == "Overpriced" | df$over == "Fairly priced") & df$expect <= 250000)) & # Overpriced but expected in price range
    df$sqmt > 80 &                                    # Not tiny
    df$type != "terrace" &                            # Detached
    df$commute_time_minutes < 30 &                     # Within 20 minute drive of uni
    df$rooms >= 3,                                      # Enough rooms for offices etc
  "View", "Meh")


# Save output -------------------------------------------------------------

# saveRDS(df, file = "C:/abdn_app/app/processed_data.rds")

## House Price prediction -----------------------------------------------------

dream_house <- data.frame(
  lat = 57.15483899436254,
  lon = -2.269886390197508,
  type = "detached",
  rooms = 4,
  baths = 2,
  epc = "c",
  tax = "d",
  sqmt = 100,
  days_since = as.numeric(ymd(Sys.Date()) - min(ymd(df$date))) # Today
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

# Figures -----------------------------------------------------------------

## Time --------------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat), 
  lon = median(df$lon),
  type = "detached",
  epc = "c",
  tax = "e",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt),
  days_since = seq(min(df$days_since), max(df$days_since), length.out = 25)
)
nu_data$date <- min(df$date) + nu_data$days_since
prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p1 <- ggplot() +
  # geom_point(data = df, aes(x = date, y = price)) +
  # geom_hline(yintercept = 250000, linetype = 2) +
  geom_ribbon(data = nu_data, aes(x = date, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = date, y = fit)) +
  # scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Date")
p1
## M:Room Interaction ----------------------------------------------------------------

nu_data <- expand.grid(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  epc = "c",
  tax = "e",
  rooms = seq(min(df$rooms), max(df$rooms[df$price < 1500000]), length.out = 25),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = seq(min(df$sqmt), max(df$sqmt[df$price < 1500000]), length.out = 25),
  days_since = median(df$days_since)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit

nu_data$fit[exclude.too.far(
  nu_data$rooms, nu_data$sqmt,
  df$rooms, df$sqmt,
  dist = 0.1)] <- NA

p4 <- ggplot() +
  geom_raster(data = nu_data, aes(x = rooms, y = sqmt, fill = fit)) +
  scale_fill_viridis_c(option = "magma", na.value = "transparent", labels = scales::comma) +
  labs(y = "Square meters",
       x = "Rooms",
       fill = "Expected\nprice")

## House type --------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat), 
  lon = median(df$lon),
  type = c("semi", "detached", "terrace"),
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
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p5 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = type, y = fit, ymin = low, ymax = upp), width = 0.1, colour = "white") +
  geom_point(data = nu_data, aes(x = type, y = fit), size = 3.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking price",
       x = "Square meters")

## Baths ----------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
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
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p6 <- ggplot() +
  # geom_point(data = df, aes(x = baths, y = price)) +
  # geom_hline(yintercept = 250000, linetype = 2) +
  geom_ribbon(data = nu_data, aes(x = baths, y = fit, ymin = low, ymax = upp), alpha = 1) +
  geom_line(data = nu_data, aes(x = baths, y = fit)) +
  labs(y = "Asking price",
       x = "Bathrooms")

## Space ------------------------------------------------------------


# Aberdeenshire far -------------------------------------------------------


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
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

nu_data$fit[exclude.too.far(
  nu_data$lat, nu_data$lon,
  df$lat, df$lon,
  dist = 0.04)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

p7.1 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = lon , y = lat, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5) +
  #geom_point(data = df, aes(x = lon, y = lat), colour = "white", size = 0.1) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  #coord_sf() +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice (£)") +
  sbsvoid_theme()
p7.1

# Aberdeenshire close -----------------------------------------------------

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
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

nu_data$fit[exclude.too.far(
  nu_data$lat, nu_data$lon,
  df$lat, df$lon,
  dist = 0.04)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

p7 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = lon , y = lat, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5) +
  #geom_point(data = df, aes(x = lon, y = lat), colour = "white", size = 0.1) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  #coord_sf() +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice (£)") +
  sbsvoid_theme()
p7
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
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

nu_data$fit[exclude.too.far(
  nu_data$lat, nu_data$lon,
  df$lat, df$lon,
  dist = 0.1)] <- NA

nu_data <- nu_data[complete.cases(nu_data),]

p8 <- ggmap(abdn) +
  geom_tile(data = nu_data, aes(x = lon , y = lat, fill = fit), na.rm = TRUE, alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white", size = 0.5) +
  # geom_point(data = df[df$lat > min_lat & df$lat < max_lat &
  #                        df$lon > min_lon & df$lon < max_lon,], aes(x = lon, y = lat), colour = "white", size = 0.1) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  #coord_sf() +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice (£)") +
  sbsvoid_theme()

## EPC ------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
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
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p9 <- ggplot() +
  #geom_jitter(data = df, aes(x = epc, y = price), width = 0.1, alpha = 0.1) +
  geom_errorbar(data = nu_data, aes(x = epc, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = epc, y = fit), size = 2.5) +
  #scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "EPC")

## Tax ------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
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
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p10 <- ggplot() +
  # geom_jitter(data = df, aes(x = tax, y = price), width = 0.1, height = 0, alpha = 0.2) +
  geom_errorbar(data = nu_data, aes(x = tax, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1, colour = "white") +
  geom_point(data = nu_data, aes(x = tax, y = fit), size = 2.5) +
  scale_y_continuous(labels = scales::comma) +
  #scale_colour_brewer(palette = "Dark2", direction = -1) +
  labs(y = "Asking price",
       x = "Tax band")

## Predicted versus response -----------------------------------------------

df$low1 <- 0 - sigma(m1) + df$expect
df$upp1 <- 0 + sigma(m1) + df$expect
df$low2 <- 0 - 2 * sigma(m1) + df$expect
df$upp2 <- 0 + 2 * sigma(m1) + df$expect

p11 <- ggplot(df) +
  geom_ribbon(aes(x = expect, ymin = low1, ymax = upp1), fill = "red", alpha = 0.4) +
  geom_ribbon(aes(x = expect, ymin = low2, ymax = upp2), fill = "red", alpha = 0.4) +
  geom_point(aes(x = expect, y = price), size = 1) +
  geom_abline(intercept = 0, slope = 1, colour = "white") +
  scale_x_continuous(labels = scales::comma, breaks = seq(from = 0, to = 1000000, by = 100000)) +
  scale_y_continuous(labels = scales::comma, breaks = seq(from = 0, to = 1000000, by = 100000)) +
  labs(x = "Listed price (£)",
       y = "Predicted price (£)") +
  coord_fixed() +
  sbs_theme()

design <- "
CCDD
EEFF
GGHH
#KK#
"

p_maps <- p7 / p8 
ggsave("C:/abdn_app/www/plot_maps.png", plot = p_maps)

p_figs <- p4 + p1 + p5 + p6 + p9 + p10 + p11 + plot_layout(design = design)
p_figs
# ggsave("C:/abdn_app/www/plot_figs.png", plot = p_figs)










# Maps --------------------------------------------------------------------

library(here)
#df <- readRDS(here("data", "processed_data.rds"))

## Leaflet map pricing -------------------------------------------------------------

pricing <- awesomeIconList(
  "Underpriced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "green",
    library = "fa"
  ),
  "Fairly priced" = makeAwesomeIcon(
    icon = "home",
    markerColor = "beige",
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
    label = ~ house,
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", house, "</b></span>",
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
      "<td>Time: ", round(commute_time_minutes), " minutes</td>",
      "<td>Time: ", round(commute_time_minutes_bioss), " minutes</td>",
      "</tr>",
      "<tr>",
      "<td>Distance: ", commute_distance, " km</td>",
      "<td>Distance: ", commute_distance_bioss, " km</td>",
      "</tr>",
      "</table>",
      "<hr>",
      "<br><a href='", link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  )

#abdn_map_price

saveWidget(abdn_map_price, here::here("output", file = "abdn_homes_pricing.html"), selfcontained = TRUE)
saveWidget(abdn_map_price, here("C:/abdn_app", "www", "abdn_homes_pricing.html"), selfcontained = TRUE)

## Leaflet map viewing ---------------------------------------------------

# Define icon lists
visit <- awesomeIconList(
  "View" = makeAwesomeIcon(
    icon = "home",
    markerColor = "lightblue",
    library = "fa"
  )
)

novisit <- awesomeIconList(
  "Meh" = makeAwesomeIcon(
    icon = "home",
    markerColor = "lightgray",
    library = "fa"
  )
)

# Subset the data based on viewing status
df_view <- df[df$viewing == "View", ]
df_meh <- df[df$viewing == "Meh", ]

# Create the Leaflet map with OpenStreetMap tiles and the updated popup layout
abdn_map_view <- leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addAwesomeMarkers(
    data = df_view,
    lng = ~lon, lat = ~lat,
    icon = ~ visit[viewing],
    label = ~ house,
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", house, "</b></span>",
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
      "<hr>",      "<span style='font-size: 16px;'><b>House details</b></span>",
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
      "<td>Time: ", round(commute_time_minutes), " minutes</td>",
      "<td>Time: ", round(commute_time_minutes_bioss), " minutes</td>",
      "</tr>",
      "<tr>",
      "<td>Distance: ", commute_distance, " km</td>",
      "<td>Distance: ", commute_distance_bioss, " km</td>",
      "</tr>",
      "</table>",
      "<hr>",
      "<br><a href='", link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "View"
  ) %>%
  addAwesomeMarkers(
    data = df_meh,
    lng = ~lon, lat = ~lat,
    icon = ~ novisit[df_meh$viewing],
    label = ~ house,
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", house, "</b></span>",
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
      "<td>Time: ", round(commute_time_minutes), " minutes</td>",
      "<td>Time: ", round(commute_time_minutes_bioss), " minutes</td>",
      "</tr>",
      "<tr>",
      "<td>Distance: ", commute_distance, " km</td>",
      "<td>Distance: ", commute_distance_bioss, " km</td>",
      "</tr>",
      "</table>",
      "<hr>",
      "<br><a href='", link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "Meh"
  ) %>%
  addLayersControl(
    overlayGroups = c("View", "Meh"),
    options = layersControlOptions(collapsed = TRUE)  # Adjust collapse behavior as needed
  )

#abdn_map_view

saveWidget(abdn_map_view, here::here("output", file = "abdn_viewing.html"), selfcontained = TRUE)
saveWidget(abdn_map_view, here("C:/abdn_app", "www", "abdn_viewing.html"), selfcontained = TRUE)


## Leaflet map viewing today -----------------------------------------------------

today_df <- df[ymd(df$date) == ymd(Sys.Date()),]

df_view <- today_df[today_df$viewing == "View", ]
df_meh <- today_df[today_df$viewing == "Meh", ]

abdn_map_view_today <- leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addAwesomeMarkers(
    data = df_view,
    lng = ~lon, lat = ~lat,
    icon = ~ visit[viewing],
    label = ~ house,
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", house, "</b></span>",
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
      "<br><b>Price per m<sup>2</sup>:</b> ", paste0( "£", round(price/sqmt, digits = 0)), " per m<sup>2</sup>",
      "<hr>",
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", date,
      "<br><b>House type:</b> ", type,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      beds, " bedrooms ", "<br>",
      living, " living rooms", "<br>",
      baths, " bathrooms","<br>",
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
      "<td>Time: ", round(commute_time_minutes), " minutes</td>",
      "<td>Time: ", round(commute_time_minutes_bioss), " minutes</td>",
      "</tr>",
      "<tr>",
      "<td>Distance: ", commute_distance, " km</td>",
      "<td>Distance: ", commute_distance_bioss, " km</td>",
      "</tr>",
      "</table>",
      "<hr>",
      "<br><a href='", link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "View"
  ) %>%
  addAwesomeMarkers(
    data = df_meh,
    lng = ~lon, lat = ~lat,
    icon = ~ novisit[viewing],
    label = ~ house,
    popup = ~paste(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", house, "</b></span>",
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
      "<br><b>Price per m<sup>2</sup>:</b> ", paste0( "£", round(price/sqmt, digits = 0)), " per m<sup>2</sup>",
      "<hr>",
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", date,
      "<br><b>House type:</b> ", type,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      beds, " bedrooms ", "<br>",
      living, " living rooms", "<br>",
      baths, " bathrooms","<br>",
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
      "<td>Time: ", round(commute_time_minutes), " minutes</td>",
      "<td>Time: ", round(commute_time_minutes_bioss), " minutes</td>",
      "</tr>",
      "<tr>",
      "<td>Distance: ", commute_distance, " km</td>",
      "<td>Distance: ", commute_distance_bioss, " km</td>",
      "</tr>",
      "</table>",
      "<hr>",
      "<br><a href='", link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "Meh"
  ) %>%
  addLayersControl(
    overlayGroups = c("View", "Meh"),
    options = layersControlOptions(collapsed = TRUE)  # Adjust collapse behavior as needed
  )

abdn_map_view_today
saveWidget(abdn_map_view_today, here::here("output", file = "abdn_viewing_today.html"), selfcontained = TRUE)
saveWidget(abdn_map_view_today, here("C:/abdn_app", "www", "abdn_viewing_today.html"), selfcontained = TRUE)


## Leaflet map today -----------------------------------------------------

abdn_map_price_today <- leaflet(today_df) %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addAwesomeMarkers(
    lng = ~lon, lat = ~lat,
    icon = ~ pricing[over],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", today_df$house, "</b></span>",
      "<hr>",
      "<span style='font-size: 16px;'><b>Price:</b></span>",
      "<br><b>Fairness:</b> ", today_df$over,
      "<br><b>Price per m<sup>2</sup>:</b> ", paste0( "£", round(today_df$price/today_df$sqmt, digits = 0)), " per m<sup>2</sup>",
      "<br><b>Asking Price:</b>", 
      paste0(" £", scales::comma(today_df$price), 
             " (", abs(round(today_df$diffn / 1000, digits = 0)), "k",
             ifelse(round(today_df$diffn / 1000, digits = 0) < 0, 
                    " under expected)", 
                    ifelse(round(today_df$diffn / 1000, digits = 0) > 0, 
                           " over expected)", ")"))),
      "<br><b>Expected Price:</b> £", scales::comma(today_df$expect),
      "<br><b>Expected Price Range:</b>", 
      paste0(" £", round(today_df$low / 1000, digits = 0), "k - ", 
             round(today_df$upp / 1000, digits = 0), "k"),
      "<hr>",
      "<span style='font-size: 16px;'><b>House details</b></span>",
      "<br><b>Date added:</b> ", today_df$date,
      "<br><b>House type:</b> ", today_df$type,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      today_df$beds, " bedrooms", "<br>",
      today_df$living, " living rooms", "<br>",
      today_df$baths, " bathrooms ","<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      today_df$sqmt, " m<sup>2</sup>", "<br>",
      "EPC: ", toupper(today_df$epc), "<br>",
      "Tax: ", toupper(today_df$tax), "<br>",
      "</td></tr>",
      "</table>",
      "<hr>",
      "<span style='font-size: 16px;'><b>Commutes:</b></span>",
      "<table style='width:100%;'>",
      "<tr>",
      "<td><b>To Aberdeen Uni</b></td>",
      "<td><b>To BioSS</b></td>",
      "</tr>",
      "<tr>",
      "<td>Time: ", round(today_df$commute_time_minutes), " minutes</td>",
      "<td>Time: ", round(today_df$commute_time_minutes_bioss), " minutes</td>",
      "</tr>",
      "<tr>",
      "<td>Distance: ", today_df$commute_distance, " km</td>",
      "<td>Distance: ", today_df$commute_distance_bioss, " km</td>",
      "</tr>",
      "</table>",
      "<hr>",
      "<br><a href='", today_df$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  )

abdn_map_price_today

