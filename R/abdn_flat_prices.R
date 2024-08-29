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
library(here)
source("sbs_theme.R")
theme_set(sbs_theme())


df <- read_sheet("https://docs.google.com/spreadsheets/d/1ay5qIHe6MK1ZLZLLDDgAVWbfnvFjc2wX2HWLeto2LKw/edit?usp=sharing",
                 sheet = "Sheet1", 
                 trim_ws = TRUE,
                 na = "NA")

df$house[duplicated(df$house)]
df <- df[!duplicated(df$house),]
# Number of rooms ---------------------------------------------------------

df$rooms <- df$beds + df$living

df <- df[complete.cases(df),]

# Commute time to ABDN uni ------------------------------------------------

api_key <- "AIzaSyDZKT-rGxUv21PxNFO4elG-n4m31gOqn_M"
origin <- "57.168010390142236,-2.106429897150815" # abdn uni
get_commute_time <- function(lat, lon, api_key, origin) {
  url <- paste0(
    "https://maps.googleapis.com/maps/api/distancematrix/json?units=metric",
    "&origins=", origin,
    "&destinations=", lat, ",", lon,
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

df$commute_time <- mapply(get_commute_time, df$lat, df$lon, MoreArgs = list(api_key = api_key, origin = origin))
df$commute_time_minutes <- df$commute_time / 60

write.table(df, "C:/006-BI3010/Workshops/Workshop 2 - Data vis/Data/abdn_flats.txt", row.names = FALSE)

# Linear model ------------------------------------------------------------
m1 <- lm(price ~ sqmt + rooms + epc + tax,
          data = df)

saveRDS(m1, file = "C:/flat_rent/data/lm_m1.rds")


prds <- predict(m1, se.fit = TRUE)
df$expect <- round(prds$fit)
df$low <- round(prds$fit - 1.96 * prds$se.fit)
df$upp <- round(prds$fit + 1.96 * prds$se.fit)

df$diffn <- df$price - df$expect
df$diff <- scales::comma(df$price - df$expect)

df$over <- ifelse(df$price > df$upp, "Overpriced", 
                  ifelse(df$price < df$low, "Underpriced", 
                         "Fairly priced"))


# Leaflet map -------------------------------------------------------------

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

abdn_map <- leaflet(df) %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addAwesomeMarkers(
    #data = df,
    lng = ~lon, lat = ~lat,
    icon = ~ pricing[over],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<span style='font-size: 20px;'><b>", df$house, "</b></span><br>",
      "<span style='font-size: 16px;'><b>Commute</b></span>",
      "<br><b>Time to walk:</b> ", ifelse(df$commute_time_minutes < 60, 
                                          paste0(round(df$commute_time_minutes), " minutes"), 
                                          paste0(round(df$commute_time_minutes / 60, 1), " hours")),
      "<hr>",
      "<span style='font-size: 16px;'><b>Rental details</b></span>",
      "<br><b>Rent fairness:</b> ", df$over,
      "<br><b>Rent:</b> £", scales::comma(df$price), " (£", abs(round(df$diffn, digits = 0)),
      ifelse(round(df$diffn, digits = 0) < 0, " under expected)", ifelse(round(df$diffn, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Rent:</b> £", scales::comma(df$expect),
      "<br><b>Expected Rent Range:</b> £", round(df$low, digits = 0), " - ", round(df$upp, digits = 0), 
      "<hr>",
      "<span style='font-size: 16px;'><b>Flat details</b></span>",
      "<br><b>Date added:</b> ", df$date,
      "<table style='width: 100%; border-collapse: collapse;'>",
      "<tr><td style='width: 50%; vertical-align: top;'>",
      df$beds, " bedrooms", "<br>",
      df$living, " living rooms", "<br>",
      df$baths, " bathrooms", "<br>",
      "</td><td style='width: 50%; vertical-align: top;'>",
      df$sqmt, " m<sup>2</sup>", "<br>",
      "EPC: ", toupper(df$epc), "<br>",
      "Tax: ", toupper(df$tax), "<br>",
      "</td></tr>",
      "</table>",
      "<hr>",
      "<br><a href='", df$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  )

abdn_map

saveWidget(abdn_map, here("C:/flat_rent", "www", "abdn_map.html"), selfcontained = TRUE)

# Square meters -----------------------------------------------------------

nu_data <- data.frame(
  epc = "c",
  tax = "c",
  commute_time_minutes = median(df$commute_time_minutes),
  rooms = median(df$rooms),
  sqmt = seq(min(df$sqmt), max(df$sqmt), length.out = 25)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p1 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = sqmt, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = sqmt, y = price), size = 1) +
  geom_line(data = nu_data, aes(x = sqmt, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Square meters") +
  sbs_theme()

# Rooms ----------------------------------------------------------------

nu_data <- data.frame(
  epc = "c",
  tax = "c",
  commute_time_minutes = median(df$commute_time_minutes),
  rooms = seq(min(df$rooms), max(df$rooms), length.out = 25),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p2 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = rooms, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = rooms, y = price), size = 1) +
  geom_line(data = nu_data, aes(x = rooms, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Rooms") +
  sbs_theme()

# Commute time ----------------------------------------------------------------

nu_data <- data.frame(
  epc = "c",
  tax = "c",
  commute_time_minutes = seq(min(df$commute_time_minutes), max(df$commute_time_minutes), length.out = 25),
  rooms = median(df$rooms),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p3 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = commute_time_minutes, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = commute_time_minutes, y = price), size = 1) +
  geom_line(data = nu_data, aes(x = commute_time_minutes, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Commute time to Uni") +
  sbs_theme()
p3
# EPC ----------------------------------------------------------------

nu_data <- data.frame(
  epc = unique(df$epc),
  tax = "c",
  commute_time_minutes = median(df$commute_time_minutes),
  rooms = median(df$rooms),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p4 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = epc, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  #geom_point(data = df, aes(x = epc, y = price)) +
  geom_point(data = nu_data, aes(x = epc, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "EPC") +
  sbs_theme()
p4
# Tax ----------------------------------------------------------------

nu_data <- data.frame(
  epc = "c",
  tax = unique(df$tax),
  commute_time_minutes = median(df$commute_time_minutes),
  rooms = median(df$rooms),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p5 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = tax, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  #geom_point(data = df, aes(x = epc, y = price)) +
  geom_point(data = nu_data, aes(x = tax, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Tax Band") +
  sbs_theme()
p5
# Predicted versus response -----------------------------------------------

df$low1 <- 0 - sigma(m1) + df$price
df$upp1 <- 0 + sigma(m1) + df$price
df$low2 <- 0 - 2 * sigma(m1) + df$price
df$upp2 <- 0 + 2 * sigma(m1) + df$price

p6 <- ggplot(df) +
  geom_ribbon(aes(x = price, ymin = low1, ymax = upp1), fill = "red", alpha = 0.4) +
  geom_ribbon(aes(x = price, ymin = low2, ymax = upp2), fill = "red", alpha = 0.4) +
  geom_point(aes(x = price, y = expect), size = 1) +
  geom_abline(intercept = 0, slope = 1, colour = "white") +
  scale_x_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(x = "Listed rent (£)",
       y = "Predicted rent (£)") +
  sbs_theme()


library(patchwork)
design <- "
AB
CD
EE
"

lm_plots <- p1 + p2 + p4 + p5 + p6 + plot_layout(design = design)

ggsave("C:/flat_rent/www/lm_plots.png", plot = lm_plots)


# GAM with space ----------------------------------------------------------
mp <- list(
  c(3, 0.1, 1), # 3 for GP, 0.1 ca. 10 km range of rho, 1 as default for scale
  c(3, 0.1, 1)
)
m1 <- gam(price ~ 
            # te(lon, lat, k = 15, bs = "cr") +
            # te(sqmt, rooms, k = 5, bs = "cr") +
            te(lon, lat, k = 5, bs = "gp", m = mp) +
            sqmt + rooms + tax + epc,
          data = df,
          method = "REML")

saveRDS(m1, file = "C:/flat_rent/data/model_m1.rds")

# Space ------------------------------------------------------------

# get map of whole region for plotting
lon_bar <- (min(df$lon, na.rm = TRUE) + max(df$lon, na.rm = TRUE))/2
lat_bar <- (min(df$lat, na.rm = TRUE) + max(df$lat, na.rm = TRUE))/2

abdnshire <- get_map(location = c(lon_bar, lat_bar), 
                     zoom = 10, 
                     maptype = "hybrid", 
                     source = "google", 
                     messaging = FALSE)

nu_data <- expand.grid(
  lat = seq(min(df$lat), max(df$lat), length.out = 200),
  lon = seq(min(df$lon), max(df$lon), length.out = 200),
  epc = "c",
  tax = "c",
  rooms = median(df$rooms),
  sqmt = median(df$sqmt),
  commute_time_minutes = median(df$commute_time_minutes)
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

p1 <- ggmap(abdnshire) +
  geom_tile(data = nu_data, aes(x = lon , y = lat, fill = fit), alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white") +
  geom_point(data = df, aes(x = lon, y = lat), colour = "white", size = 0.5) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  #coord_sf() +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nrent (£)") +
  sbsvoid_theme()
p1


# Aberdeen ----------------------------------------------------------------

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
  lat = seq(min(min_lat), max(max_lat), length.out = 200),
  lon = seq(min(min_lon), max(max_lon), length.out = 200),
  epc = "c",
  tax = "c",
  rooms = median(df$rooms),
  sqmt = median(df$sqmt),
  commute_time_minutes = median(df$commute_time_minutes)
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

p2 <- ggmap(abdn) +
  geom_tile(data = nu_data, aes(x = lon , y = lat, fill = fit), na.rm = TRUE, alpha = 0.6) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white") +
  geom_point(data = df[df$lat > min_lat & df$lat < max_lat &
                         df$lon > min_lon & df$lon < max_lon,], aes(x = lon, y = lat), colour = "white", size = 0.5) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  #coord_sf() +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nrent (£)") +
  sbsvoid_theme()
p2

gam_space <- p1 / p2

ggsave("C:/flat_rent/www/rent_maps.png", plot = gam_space)

# Square meters -----------------------------------------------------------

nu_data <- data.frame(
  epc = "c",
  tax = "c",
  commute_time_minutes = median(df$commute_time_minutes),
  lat = median(df$lat),
  lon = median(df$lon),
  rooms = median(df$rooms),
  sqmt = seq(min(df$sqmt), max(df$sqmt), length.out = 25)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p3 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = sqmt, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = sqmt, y = price), size = 1) +
  geom_line(data = nu_data, aes(x = sqmt, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Square meters") +
  sbs_theme()
p3

# Rooms ----------------------------------------------------------------

nu_data <- data.frame(
  epc = "c",
  tax = "c",
  lat = median(df$lat),
  lon = median(df$lon),
  commute_time_minutes = median(df$commute_time_minutes),
  rooms = seq(min(df$rooms), max(df$rooms), length.out = 25),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p4 <- ggplot() +
  geom_ribbon(data = nu_data, aes(x = rooms, y = fit, ymin = low, ymax = upp), fill = "white", alpha = 0.4) +
  geom_point(data = df, aes(x = rooms, y = price), size = 1) +
  geom_line(data = nu_data, aes(x = rooms, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Rooms") +
  sbs_theme()
p4

# EPC ----------------------------------------------------------------

nu_data <- data.frame(
  epc = unique(df$epc),
  tax = "c",
  lat = median(df$lat),
  lon = median(df$lon),
  commute_time_minutes = median(df$commute_time_minutes),
  rooms = median(df$rooms),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p6 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = epc, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  #geom_point(data = df, aes(x = epc, y = price)) +
  geom_point(data = nu_data, aes(x = epc, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "EPC") +
  sbs_theme()

# Tax ----------------------------------------------------------------

nu_data <- data.frame(
  epc = "c",
  tax = unique(df$tax),
  lat = median(df$lat),
  lon = median(df$lon),
  commute_time_minutes = median(df$commute_time_minutes),
  rooms = median(df$rooms),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p7 <- ggplot() +
  geom_errorbar(data = nu_data, aes(x = tax, ymin = low, ymax = upp), colour = "white", width = 0.1) +
  #geom_point(data = df, aes(x = epc, y = price)) +
  geom_point(data = nu_data, aes(x = tax, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Tax Band") +
  sbs_theme()


df$low1 <- 0 - sigma(m1) + df$price
df$upp1 <- 0 + sigma(m1) + df$price
df$low2 <- 0 - 2 * sigma(m1) + df$price
df$upp2 <- 0 + 2 * sigma(m1) + df$price

df$expect <- predict(m1)

p8 <- ggplot(df) +
  geom_ribbon(aes(x = price, ymin = low1, ymax = upp1), fill = "red", alpha = 0.4) +
  geom_ribbon(aes(x = price, ymin = low2, ymax = upp2), fill = "red", alpha = 0.4) +
  geom_point(aes(x = price, y = expect), size = 1) +
  geom_abline(intercept = 0, slope = 1, colour = "white") +
  scale_x_continuous(labels = scales::comma, limits = c(0, NA)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(x = "Listed rent (£)",
       y = "Predicted rent (£)") +
  sbs_theme()
p8

library(patchwork)
design <- "
AB
CD
EE
"

gam_plots <- p3 + p4 + p6 + p7 + p8 + plot_layout(design = design)

ggsave("C:/flat_rent/www/rent_figs.png", plot = gam_plots)


# Export to BI3010 --------------------------------------------------------

write.table(df, "C:/006-BI3010/Workshops/Workshop 2 - Data vis/Data/abdn_flats.txt", row.names = FALSE)

# 
# prds <- predict(m1, se.fit = TRUE)
# df$expect <- round(prds$fit)
# df$low <- round(prds$fit - 1.96 * prds$se.fit)
# df$upp <- round(prds$fit + 1.96 * prds$se.fit)
# 
# df$diffn <- df$price - df$expect
# df$diff <- scales::comma(df$price - df$expect)
# 
# df$over <- ifelse(df$price > df$upp, "Overpriced", 
#                   ifelse(df$price < df$low, "Underpriced", 
#                          "Fairly priced"))
# 
# # # Leaflet map -------------------------------------------------------------
# 
# pricing <- awesomeIconList(
#   "Underpriced" = makeAwesomeIcon(
#     icon = "home",
#     markerColor = "green",
#     library = "fa"
#   ),
#   "Fairly priced" = makeAwesomeIcon(
#     icon = "home",
#     markerColor = "beige",
#     library = "fa"
#   ),
#   "Overpriced" = makeAwesomeIcon(
#     icon = "home",
#     markerColor = "red",
#     library = "fa"
#   )
# )
# 
# abdn_map <- leaflet(df) %>%
#   addProviderTiles(provider = "OpenStreetMap") %>%
#   addAwesomeMarkers(
#     #data = df,
#     lng = ~lon, lat = ~lat,
#     icon = ~ pricing[over],
#     label = ~ house,
#     popup = paste0(
#       "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
#       "<span style='font-size: 20px;'><b>", df$house, "</b></span><br>",
#       "<span style='font-size: 16px;'><b>Commute</b></span>",
#       "<br><b>Time to walk:</b> ", ifelse(df$commute_time_minutes < 60, 
#                                           paste0(round(df$commute_time_minutes), " minutes"), 
#                                           paste0(round(df$commute_time_minutes / 60, 1), " hours")),
#       "<hr>",
#       "<span style='font-size: 16px;'><b>Rental details</b></span>",
#       "<br><b>Rent fairness:</b> ", df$over,
#       "<br><b>Rent:</b> £", scales::comma(df$price), " (£", abs(round(df$diffn, digits = 0)),
#       ifelse(round(df$diffn, digits = 0) < 0, " under expected)", ifelse(round(df$diffn, digits = 0) > 0, " over expected)", ")")),
#       "<br><b>Expected Rent:</b> £", scales::comma(df$expect),
#       "<br><b>Expected Rent Range:</b> £", round(df$low, digits = 0), " - ", round(df$upp, digits = 0), 
#       "<hr>",
#       "<span style='font-size: 16px;'><b>Flat details</b></span>",
#       "<br><b>Date added:</b> ", df$date,
#       "<table style='width: 100%; border-collapse: collapse;'>",
#       "<tr><td style='width: 50%; vertical-align: top;'>",
#       df$beds, " bedrooms", "<br>",
#       df$living, " living rooms", "<br>",
#       df$baths, " bathrooms", "<br>",
#       "</td><td style='width: 50%; vertical-align: top;'>",
#       df$sqmt, " m<sup>2</sup>", "<br>",
#       "EPC: ", toupper(df$epc), "<br>",
#       "Tax: ", toupper(df$tax), "<br>",
#       "</td></tr>",
#       "</table>",
#       "<hr>",
#       "<br><a href='", df$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
#       "</div>"
#     )
#   )
# 
# abdn_map
# 
# saveWidget(abdn_map, here::here("output", file = "abdn_rent.html"), selfcontained = TRUE)
# 
# 
# 
# # How to split cost -------------------------------------------------------
# 
# # Assume:
# # 100 square meter flat
# # 3 bedrooms and 1 living room
# # 3 people rent
# # Bedroom 1 is 10 sqmt
# # Bedroom 2 is 12 sqmt
# # Bedroom 3 is 8 sqmt
# 
# calculate_rent <- function(model, total_square_meters, room_sizes, true_rental_price) {
#   number_of_rooms <- length(room_sizes)
#   expected_rental_price <- coef(model)[1] + coef(model)[3] * number_of_rooms + coef(model)[2] * total_square_meters
#   total_bedroom_area <- sum(room_sizes)
#   idealized_bedroom_rents <- (room_sizes / total_bedroom_area) * expected_rental_price
#   realized_bedroom_rents <- (room_sizes / total_bedroom_area) * true_rental_price
#   list(
#     expected_rental_price = round(expected_rental_price, digits = 0),
#     idealized_bedroom_rents = round(idealized_bedroom_rents, digits = 0),
#     realized_bedroom_rents = round(realized_bedroom_rents, digits = 0),
#     price_per_smqt = true_rental_price/total_square_meters
#   )
# }
# 
# total_square_meters <- 109
# room_sizes <- c(10, 12, 14)
# true_rental_price <- 975
# 
# rent_details <- calculate_rent(m1, total_square_meters, room_sizes, true_rental_price)
# 
# paste0("Rent per person: £", 
#        rent_details$idealized_bedroom_rents[1], ", £",
#        rent_details$idealized_bedroom_rents[2], ", £",
#        rent_details$idealized_bedroom_rents[3],  ". ",
#        "Given an expected rent of £", rent_details$expected_rental_price)
# 
# paste0("Rent per person: £", 
#        rent_details$realized_bedroom_rents[1], ", £",
#        rent_details$realized_bedroom_rents[2], ", £",
#        rent_details$realized_bedroom_rents[3],  ". ",
#        "Given a rent of £", true_rental_price)
# 
# cat("Realized bedroom rents:", rent_details$realized_bedroom_rents, "\n")
