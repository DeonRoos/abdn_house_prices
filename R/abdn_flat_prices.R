library(googlesheets4) # For loading in the data from google sheets
library(ggplot2)       # For data visualiations
library(mgcv)          # For non-linear models
library(leaflet)
theme_set(theme_minimal())

df <- read_sheet("https://docs.google.com/spreadsheets/d/1ay5qIHe6MK1ZLZLLDDgAVWbfnvFjc2wX2HWLeto2LKw/edit?usp=sharing",
                 sheet = "Sheet1", 
                 trim_ws = TRUE)

df <- df[!duplicated(df$house),]
# Number of rooms ---------------------------------------------------------

df$rooms <- df$beds + df$living

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


# Linear model ------------------------------------------------------------
m1 <- lm(price ~ sqmt * rooms + commute_time_minutes,
          data = df)


# Summary -----------------------------------------------------------------
summary(m1)

par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))

prds <- predict(m1, se.fit = TRUE)
df$expect <- round(prds$fit)
df$low <- round(prds$fit - 1.96 * prds$se.fit)
df$upp <- round(prds$fit + 1.96 * prds$se.fit)

df$diffn <- df$price - df$expect
df$diff <- scales::comma(df$price - df$expect)

df$over <- ifelse(df$price > df$upp, "Overpriced", 
                  ifelse(df$price < df$low, "Underpriced", 
                         "Fairly priced"))

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

abdn_map <- leaflet(df) %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addAwesomeMarkers(
    #data = df,
    lng = ~lon, lat = ~lat,
    icon = ~ pricing[over],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df$house, "</b>",
      "<br><b>Walk to Aberdeen Uni:</b> ", round(df$commute_time_minutes), " minutes",
      "<hr>",
      "<br><b>Pricing:</b> ", df$over,
      "<br><b>Asking Price:</b> £", scales::comma(df$price), " (£", abs(round(df$diffn, digits = 0)),
      ifelse(round(df$diffn, digits = 0) < 0, " under expected)", ifelse(round(df$diffn, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df$expect),
      "<br><b>Expected Price Range:</b> £", round(df$low, digits = 0), " - ", round(df$upp, digits = 0), 
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df$date,
      "<br><b># Bedrooms:</b> ", df$beds,
      "<br><b># Living rooms:</b> ", df$living,
      "<br><b># Bathrooms:</b> ", df$baths,
      "<br><b>Square meters:</b> ", df$sqmt,
      "<br><b>EPC rating:</b> ", df$epc,
      "<br><b>Tax band:</b> ", df$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  )

abdn_map

dream_house <- data.frame(
  rooms = 1,
  sqmt = 60,
  commute_time_minutes = 5
)

predict(m1, newdata = dream_house)

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

ggplot() +
  geom_point(data = df, aes(x = sqmt, y = price)) +
  geom_ribbon(data = nu_data, aes(x = sqmt, y = fit, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(data = nu_data, aes(x = sqmt, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Square meters")

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

ggplot() +
  geom_point(data = df, aes(x = rooms, y = price)) +
  geom_ribbon(data = nu_data, aes(x = rooms, y = fit, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(data = nu_data, aes(x = rooms, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Rooms")

# Interaction ----------------------------------------------------------------

nu_data <- expand.grid(
  epc = "c",
  tax = "c",
  rooms = seq(min(df$rooms), max(df$rooms), length.out = 25),
  baths = median(df$baths),
  sqmt = seq(min(df$sqmt), max(df$sqmt), length.out = 25),
  commute_time_minutes = median(df$commute_time_minutes)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit

ggplot() +
  geom_raster(data = nu_data, aes(x = rooms, y = sqmt, fill = fit)) +
  scale_fill_viridis_c(option = "magma", na.value = "transparent", labels = scales::comma) +
  labs(y = "Square meters",
       x = "Rooms (Bedrooms + Living)",
       fill = "Expected\nprice")

# Predicted versus response -----------------------------------------------

ggplot(df) +
  geom_point(aes(x = price, y = expect)) +
  geom_abline(intercept = 0, slope = 1)

# GAM with space ----------------------------------------------------------

m1 <- gam(price ~ 
            # te(lon, lat, k = 15, bs = "cr") +
            # te(sqmt, rooms, k = 5, bs = "cr") +
            te(lon, lat, k = 5, bs = "cr") +
            sqmt * rooms +
            commute_time_minutes,
          data = df,
          method = "REML")

# Space ------------------------------------------------------------

nu_data <- expand.grid(
  lat = seq(min(df$lat), max(df$lat), length.out = 50),
  lon = seq(min(df$lon), max(df$lon), length.out = 50),
  commute_time_minutes = median(df$commute_time_minutes),
  rooms = median(df$rooms),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

nu_data$fit[exclude.too.far(
  nu_data$lat, nu_data$lon,
  df$lat, df$lon,
  dist = 0.08)] <- NA

p1 <- ggplot() +
  geom_raster(data = nu_data, aes(x = lon , y = lat, fill = fit)) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white") +
  geom_point(data = df, aes(x = lon, y = lat), colour = "white", size = 0.5) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  coord_sf() +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice")

# Interaction ----------------------------------------------------------------

nu_data <- expand.grid(
  lat = median(df$lat),
  lon = median(df$lon),
  rooms = seq(min(df$rooms), max(df$rooms), length.out = 25),
  sqmt = seq(min(df$sqmt), max(df$sqmt), length.out = 25),
  commute_time_minutes = median(df$commute_time_minutes)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit

p2 <- ggplot() +
  geom_raster(data = nu_data, aes(x = rooms, y = sqmt, fill = fit)) +
  scale_fill_viridis_c(option = "magma", na.value = "transparent", labels = scales::comma) +
  labs(y = "Square meters",
       x = "Rooms (Bedrooms + Living)",
       fill = "Expected\nprice")

# Interaction ----------------------------------------------------------------

nu_data <- expand.grid(
  lat = median(df$lat),
  lon = median(df$lon),
  rooms = median(df$rooms),
  sqmt = median(df$sqmt),
  commute_time_minutes = seq(min(df$commute_time_minutes), max(df$commute_time_minutes), length.out = 25)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

p3 <- ggplot() +
  geom_point(data = df, aes(x = commute_time_minutes, y = price), alpha = 0.2) +
  geom_ribbon(data = nu_data, aes(x = commute_time_minutes, y = fit, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(data = nu_data, aes(x = commute_time_minutes, y = fit)) +
  #scale_colour_brewer(palette = "Dark2", direction = -1) +
  labs(y = "Asking price",
       x = "Time to walk to Uni")

library(patchwork)
design <- "
AA
AA
BC
"
p1 + p2 + p3 + plot_layout(design = design)

prds <- predict(m1, se.fit = TRUE)
df$expect <- round(prds$fit)
df$low <- round(prds$fit - 1.96 * prds$se.fit)
df$upp <- round(prds$fit + 1.96 * prds$se.fit)

df$diffn <- df$price - df$expect
df$diff <- scales::comma(df$price - df$expect)

df$over <- ifelse(df$price > df$upp, "Overpriced", 
                  ifelse(df$price < df$low, "Underpriced", 
                         "Fairly priced"))

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

abdn_map <- leaflet(df) %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addAwesomeMarkers(
    #data = df,
    lng = ~lon, lat = ~lat,
    icon = ~ pricing[over],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df$house, "</b>",
      "<br><b>Walk to Aberdeen Uni:</b> ", round(df$commute_time_minutes), " minutes",
      "<hr>",
      "<br><b>Pricing:</b> ", df$over,
      "<br><b>Asking Price:</b> £", scales::comma(df$price), " (£", abs(round(df$diffn, digits = 0)),
      ifelse(round(df$diffn, digits = 0) < 0, " under expected)", ifelse(round(df$diffn, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df$expect),
      "<br><b>Expected Price Range:</b> £", round(df$low, digits = 0), " - ", round(df$upp, digits = 0), 
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df$date,
      "<br><b># Bedrooms:</b> ", df$beds,
      "<br><b># Living rooms:</b> ", df$living,
      "<br><b># Bathrooms:</b> ", df$baths,
      "<br><b>Square meters:</b> ", df$sqmt,
      "<br><b>EPC rating:</b> ", df$epc,
      "<br><b>Tax band:</b> ", df$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  )

abdn_map

saveWidget(abdn_map, here::here("output", file = "abdn_rent.html"), selfcontained = TRUE)
