library(googlesheets4) # For loading in the data from google sheets
library(ggplot2)       # For data visualiations
library(mgcv)          # For non-linear models
library(leaflet)
library(lubridate)
library(htmlwidgets)
theme_set(theme_minimal())

df <- read_sheet("https://docs.google.com/spreadsheets/d/1WQNnBK6P4ml9o8XyA9zMXXe2Rh-pY6boQEA1zR1nsus/edit?usp=sharing",
                 sheet = "Sheet1", 
                 trim_ws = TRUE)

# Code to check if house has been added -----------------------------------
# When doing data entry
house <- "6 Mundurno Road"
df[df$house == house,]#

# Remove duplicates -------------------------------------------------------
df <- df[!duplicated(df),]
df$rooms <- df$beds + df$living

summary(df)

ggplot(df) +
  geom_jitter(aes(x = sqmt, y = price),
              height = 0, width = 0.15) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma)

ggplot(df) +
  geom_jitter(aes(x = rooms, y = price),
              height = 0, width = 0.15) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma)

ggplot(df) +
  geom_jitter(aes(x = beds, y = price),
              height = 0, width = 0.15) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma)

ggplot(df) +
  geom_jitter(aes(x = living, y = price),
              height = 0, width = 0.15) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma)

ggplot(df) +
  geom_jitter(aes(x = baths, y = price),
              height = 0, width = 0.15) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma)

ggplot(df) +
  geom_tile(aes(x = beds, y = living, fill = price)) +
  scale_fill_viridis_c(option = "magma", limits = c(0,NA), labels = scales::comma)

ggplot(df) +
  geom_jitter(aes(x = epc, y = price),
              height = 0, width = 0.15) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma)


# The AI MaCHiNE LeARniNg model -------------------------------------------
m1 <- gam(price ~ 
            te(lon, lat, k = 15, bs = "cr") +
            te(sqmt, rooms, k = 5, bs = "cr") +
            type +
            baths +
            epc +
            tax,
          data = df,
          method = "REML")

# Generate predictions ----------------------------------------------------
prds <- predict(m1, se.fit = TRUE)
df$expect <- round(prds$fit)
df$low <- round(prds$fit - 1.96 * prds$se.fit)
df$upp <- round(prds$fit + 1.96 * prds$se.fit)

# Process for pop up ------------------------------------------------------

df$diffn <- df$price - df$expect
df$diff <- scales::comma(df$price - df$expect)

df$over <- ifelse(df$price > df$upp, "Overpriced", 
                  ifelse(df$price < df$low, "Underpriced", 
                         "Fairly priced"))

# House criteria ----------------------------------------------------------

df$viewing <- ifelse(df$over != "Overpriced" & # Can't be over priced
                       df$price <= 250000 &    # Below help2 buy ISA threshold
                       df$sqmt > 100 &         # Not tiny
                       df$rooms > 3,           # Enough rooms for offices etc
                     "View", "Meh")

# Leaflet map -------------------------------------------------------------

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
      "<hr>",
      "<br><b>Pricing:</b> ", df$over,
      "<br><b>Asking Price:</b> £", scales::comma(df$price), " (", abs(round(df$diffn / 1000, digits = 0)), "k",
      ifelse(round(df$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df$expect),
      "<br><b>Expected Price Range:</b> £", round(df$low / 1000, digits = 0), "k - ", round(df$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df$date,
      "<br><b>House type:</b> ", df$type,
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

saveWidget(abdn_map, here::here("output", file = "abdn_homes_pricing.html"), selfcontained = TRUE)

# Leaflet map data prep ---------------------------------------------------

visit <- awesomeIconList(
  "View" = makeAwesomeIcon(
    icon = "home",
    markerColor = "green",
    library = "fa"
  )
  )
novisit <- awesomeIconList(
  "Meh" = makeAwesomeIcon(
    icon = "home",
    markerColor = "red", 
    library = "fa"
  )
)

df_view <- df[df$viewing == "View", ]
df_meh <- df[df$viewing == "Meh", ]

# Leaflet map -------------------------------------------------------------

abdn_map <- leaflet() %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addAwesomeMarkers(
    data = df_view,
    lng = ~lon, lat = ~lat,
    icon = ~ visit[viewing],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_view$house, "</b>",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_view$viewing,
      "<br><b>Pricing:</b> ", df_view$over,
      "<br><b>Asking Price:</b> £", scales::comma(df_view$price), " (", abs(round(df_view$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_view$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_view$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_view$expect),
      "<br><b>Expected Price Range:</b> £", round(df_view$low / 1000, digits = 0), "k - ", round(df_view$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_view$date,
      "<br><b>House type:</b> ", df_view$type,
      "<br><b># Bedrooms:</b> ", df_view$beds,
      "<br><b># Living rooms:</b> ", df_view$living,
      "<br><b># Bathrooms:</b> ", df_view$baths,
      "<br><b>Square meters:</b> ", df_view$sqmt,
      "<br><b>EPC rating:</b> ", df_view$epc,
      "<br><b>Tax band:</b> ", df_view$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_view$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "View"
  ) %>%
  addAwesomeMarkers(
    data = df_meh,
    lng = ~lon, lat = ~lat,
    icon = ~ novisit[viewing],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_meh$house, "</b>",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_meh$viewing,
      "<br><b>Pricing:</b> ", df_meh$over,
      "<br><b>Asking Price:</b> £", scales::comma(df_meh$price), " (", abs(round(df_meh$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_meh$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_meh$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_meh$expect),
      "<br><b>Expected Price Range:</b> £", round(df_meh$low / 1000, digits = 0), "k - ", round(df_meh$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_meh$date,
      "<br><b>House type:</b> ", df_meh$type,
      "<br><b># Bedrooms:</b> ", df_meh$beds,
      "<br><b># Living rooms:</b> ", df_meh$living,
      "<br><b># Bathrooms:</b> ", df_meh$baths,
      "<br><b>Square meters:</b> ", df_meh$sqmt,
      "<br><b>EPC rating:</b> ", df_meh$epc,
      "<br><b>Tax band:</b> ", df_meh$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_meh$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "Meh"
  ) %>%
  addLayersControl(
    overlayGroups = c("View", "Meh"),
    options = layersControlOptions(collapsed = TRUE)  # Adjust collapse behavior as needed
  )

abdn_map

saveWidget(abdn_map, here::here("output", file = "abdn_homes.html"), selfcontained = TRUE)

# Listings from today -----------------------------------------------------

today_df <- df[ymd(df$date) == ymd(Sys.Date()),]

df_view <- today_df[today_df$viewing == "View", ]
df_meh <- today_df[today_df$viewing == "Meh", ]

abdn_map <- leaflet() %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addAwesomeMarkers(
    data = df_view,
    lng = ~lon, lat = ~lat,
    icon = ~ visit[viewing],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_view$house, "</b>",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_view$viewing,
      "<br><b>Pricing:</b> ", df_view$over,
      "<br><b>Asking Price:</b> £", scales::comma(df_view$price), " (", abs(round(df_view$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_view$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_view$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_view$expect),
      "<br><b>Expected Price Range:</b> £", round(df_view$low / 1000, digits = 0), "k - ", round(df_view$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_view$date,
      "<br><b>House type:</b> ", df_view$type,
      "<br><b># Bedrooms:</b> ", df_view$beds,
      "<br><b># Living rooms:</b> ", df_view$living,
      "<br><b># Bathrooms:</b> ", df_view$baths,
      "<br><b>Square meters:</b> ", df_view$sqmt,
      "<br><b>EPC rating:</b> ", df_view$epc,
      "<br><b>Tax band:</b> ", df_view$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_view$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "View"
  ) %>%
  addAwesomeMarkers(
    data = df_meh,
    lng = ~lon, lat = ~lat,
    icon = ~ novisit[viewing],
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_meh$house, "</b>",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_meh$viewing,
      "<br><b>Pricing:</b> ", df_meh$over,
      "<br><b>Asking Price:</b> £", scales::comma(df_meh$price), " (", abs(round(df_meh$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_meh$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_meh$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_meh$expect),
      "<br><b>Expected Price Range:</b> £", round(df_meh$low / 1000, digits = 0), "k - ", round(df_meh$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_meh$date,
      "<br><b>House type:</b> ", df_meh$type,
      "<br><b># Bedrooms:</b> ", df_meh$beds,
      "<br><b># Living rooms:</b> ", df_meh$living,
      "<br><b># Bathrooms:</b> ", df_meh$baths,
      "<br><b>Square meters:</b> ", df_meh$sqmt,
      "<br><b>EPC rating:</b> ", df_meh$epc,
      "<br><b>Tax band:</b> ", df_meh$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_meh$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "Meh"
  ) %>%
  addLayersControl(
    overlayGroups = c("View", "Meh"),
    options = layersControlOptions(collapsed = TRUE)  # Adjust collapse behavior as needed
  )

abdn_map

# Spatial prediction ------------------------------------------------------

gratia::draw(m1)

# Specific prediction -----------------------------------------------------

dream_house <- data.frame(
  lat = 57.15483899436254,
  lon = -2.269886390197508,
  type = "semi",
  beds = 3,
  living = 1,
  rooms = 4,
  baths = 3,
  epc = "c",
  tax = "d",
  sqmt = 110
)

prds <- predict(m1, newdata = dream_house, se.fit = TRUE)
prds$fit
prds$fit - 1.96 * prds$se.fit
prds$fit + 1.96 * prds$se.fit
# £240k (n = 175)
# £246k (n = 201)
# £233k [£208k-£257k] (n = 235)
# £229k [£204k-£254k] (n = 237)

# Square meters -----------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat), 
  lon = median(df$lon),
  type = "detached",
  epc = "c",
  tax = "c",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = seq(min(df$sqmt), max(df$sqmt), length.out = 25)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

ggplot() +
  geom_point(data = df, aes(x = sqmt, y = price)) +
  geom_hline(yintercept = 250000, linetype = 2) +
  geom_ribbon(data = nu_data, aes(x = sqmt, y = fit, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(data = nu_data, aes(x = sqmt, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Square meters")

# Rooms ----------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  epc = "c",
  tax = "c",
  rooms = seq(min(df$rooms), max(df$rooms), length.out = 25),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

ggplot() +
  geom_point(data = df, aes(x = rooms, y = price)) +
  geom_hline(yintercept = 250000, linetype = 2) +
  geom_ribbon(data = nu_data, aes(x = rooms, y = fit, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(data = nu_data, aes(x = rooms, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Rooms")

# Interaction ----------------------------------------------------------------

nu_data <- expand.grid(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  epc = "c",
  tax = "c",
  rooms = seq(min(df$rooms), max(df$rooms), length.out = 25),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = seq(min(df$sqmt), max(df$sqmt), length.out = 25)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit

ggplot() +
  geom_raster(data = nu_data, aes(x = rooms, y = sqmt, fill = fit)) +
  scale_fill_viridis_c(option = "magma") +
  labs(y = "Square meters",
       x = "Rooms",
       fill = "Expected\nprice")

# House type --------------------------------------------------------------

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
  tax = "c"
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

ggplot() +
  geom_errorbar(data = nu_data, aes(x = type, y = fit, ymin = low, ymax = upp), width = 0.1) +
  geom_point(data = nu_data, aes(x = type, y = fit), size = 3.5) +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Asking price",
       x = "Square meters")

# Bedrooms ----------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  epc = "c",
  tax = "c",
  rooms = median(df$rooms),
  beds = seq(min(df$beds), max(df$beds), length.out = 25),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

ggplot() +
  geom_point(data = df, aes(x = beds, y = price)) +
  geom_hline(yintercept = 250000, linetype = 2) +
  geom_ribbon(data = nu_data, aes(x = beds, y = fit, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(data = nu_data, aes(x = beds, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Living rooms")

# Living rooms ------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  epc = "c",
  tax = "c",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = seq(min(df$living), max(df$living), length.out = 25),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

ggplot() +
  geom_point(data = df, aes(x = living, y = price)) +
  geom_hline(yintercept = 250000, linetype = 2) +
  geom_ribbon(data = nu_data, aes(x = living, y = fit, ymin = low, ymax = upp), alpha = 0.3) +
  geom_line(data = nu_data, aes(x = living, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "Living rooms")

# Space ------------------------------------------------------------

nu_data <- expand.grid(
  lat = seq(min(df$lat), max(df$lat), length.out = 50),
  lon = seq(min(df$lon), max(df$lon), length.out = 50),
  type = "detached",
  epc = "c",
  tax = "c",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

nu_data$fit[exclude.too.far(
  nu_data$lat, nu_data$lon,
  df$lat, df$lon,
  dist = 0.1)] <- NA

ggplot() +
  geom_raster(data = nu_data, aes(x = lon , y = lat, fill = fit)) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice")

# EPC ------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat),
  lon = median(df$lon),
  type = "detached",
  epc = unique(df$epc),
  tax = "c",
  rooms = median(df$rooms),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

ggplot() +
  geom_jitter(data = df, aes(x = epc, y = price), width = 0.1, alpha = 0.1) +
  geom_errorbar(data = nu_data, aes(x = epc, y = fit, ymin = low, ymax = upp), width = 0.1) +
  geom_point(data = nu_data, aes(x = epc, y = fit)) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  labs(y = "Asking price",
       x = "EPC")

# Tax ------------------------------------------------------------

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
  sqmt = median(df$sqmt)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit
nu_data$low <- prds$fit - prds$se.fit * 1.96
nu_data$upp <- prds$fit + prds$se.fit * 1.96

ggplot() +
  geom_jitter(data = df, aes(x = tax, y = price), width = 0.1, height = 0, alpha = 0.2) +
  geom_errorbar(data = nu_data, aes(x = tax, y = fit, ymin = low, ymax = upp), width = 0.1, linewidth = 1) +
  geom_point(data = nu_data, aes(x = tax, y = fit), size = 2.5) +
  scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
  #scale_colour_brewer(palette = "Dark2", direction = -1) +
  labs(y = "Asking price",
       x = "Tax band")

# Predicted versus response -----------------------------------------------

sig <- sigma(m1)

ggplot(df) +
  geom_point(aes(x = expect, y = price, colour = over), size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = 1.96 * sig, slope = 1, linetype = 2) +
  geom_abline(intercept = 1.96 * -sig, slope = 1, linetype = 2) +
  scale_y_continuous(limits = c(NA,NA), labels = scales::comma) +
  scale_x_continuous(limits = c(NA,NA), labels = scales::comma) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Expected price",
       y = "Listed price",
       colour = "Pricing")
  

ggplot(df) +
  geom_point(aes(x = price, y = diffn, colour = over)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_rect(xmin = 180000, xmax =  250000, ymin = min(df$diffn), ymax = 0, linetype = 2, fill = "transparent", colour = "black") +
  scale_y_continuous(limits = c(NA,NA), labels = scales::comma) +
  scale_x_continuous(limits = c(NA,NA), labels = scales::comma)

ggplot(df) +
  geom_point(aes(x = diffn, y = price, colour = viewing, size = viewing)) +
  geom_vline(xintercept = 0) +
  geom_rect(ymin = 180000, ymax =  250000, xmin = min(df$diffn), xmax = 0, linetype = 2, fill = "transparent", colour = "black") +
  scale_y_continuous(limits = c(NA,NA), labels = scales::comma) +
  scale_x_continuous(limits = c(NA,NA), labels = scales::comma)
