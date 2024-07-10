library(googlesheets4) # For loading in the data from google sheets
library(ggplot2)       # For data visualiations
library(mgcv)          # For non-linear models
library(leaflet)
theme_set(theme_minimal())

df <- read_sheet("https://docs.google.com/spreadsheets/d/1ay5qIHe6MK1ZLZLLDDgAVWbfnvFjc2wX2HWLeto2LKw/edit?usp=sharing",
                 sheet = "Sheet1", 
                 trim_ws = TRUE)

summary(df)

ggplot(df) +
  geom_jitter(aes(x = sqmt, y = price),
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

m1 <- gam(price ~ 
            ti(lon, lat, k = 10) +
            sqmt * beds +
            living +
            baths +
            epc +
            tax,
          data = df,
          method = "REML")
df$expect <- round(predict(m1), digits = 0)
df$diff <- df$price - df$expect
df$over <- ifelse(df$diff > -5000 & df$diff < 5000, "Fairly priced",
                  ifelse(df$diff < -5000, "Underpriced", 
                         "Overpriced"))
df$diff <- scales::comma(df$price - df$expect)
df$diffn <- df$price - df$expect

priceCol <- colorFactor(palette = c("white", "red", "blue"), df$over)

leaflet() %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addCircleMarkers(data = df, lng = ~lon, lat = ~lat, radius = 5,
                   options = markerOptions(opacity = 1),
                   color = ~priceCol(over),
                   popup = paste("House:", df$house,
                                 "<br>Date:", df$date,
                                 "<br>Price:", scales::comma(df$price),
                                 "<br>Expected Price:", scales::comma(df$expect),
                                 "<br>Pricing:", df$over,
                                 "<br>House type:", df$type,
                                 "<br># Beds:", df$beds,
                                 "<br># Living:", df$living,
                                 "<br># Baths:", df$baths,
                                 "<br>Square meters:", df$sqmt,
                                 "<br>EPC rating:", df$epc,
                                 "<br>Tax band:", df$tax,
                                 "<br>", df$link)
  )

gratia::draw(m1)

dream_house <- data.frame(
  lat = 57.15483899436254,
  lon = -2.269886390197508,
  type = "detached",
  beds = 3,
  baths = 2,
  living = 1,
  epc = "c",
  tax = "c",
  sqmt = 100
)

predict(m1, newdata = dream_house)

# Square meters -----------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat), 
  lon = median(df$lon),
  type = "detached",
  epc = "c",
  tax = "c",
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


# House type --------------------------------------------------------------

nu_data <- data.frame(
  lat = median(df$lat), 
  lon = median(df$lon),
  type = c("semi", "detached", "terrace"),
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
  # geom_point(data = df, aes(x = type, y = price), alpha = 0.2) +
  geom_hline(yintercept = 250000, linetype = 2) +
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
  geom_raster(data = nu_data, aes(x = lon , y = lat, fill = fit)) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma) +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice")


# Predicted versus response -----------------------------------------------

ggplot(df) +
  geom_point(aes(x = price, y = expect)) +
  geom_abline(intercept = 0, slope = 1)

ggplot(df) +
  geom_point(aes(x = price, y = diffn, colour = over)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_rect(xmin = 180000, xmax =  250000, ymin = min(df$diffn), ymax = 0, linetype = 2, fill = "transparent", colour = "black") +
  #geom_vline(xintercept = 250000, linetype = 2) +
  scale_y_continuous(limits = c(NA,NA), labels = scales::comma) +
  scale_x_continuous(limits = c(NA,NA), labels = scales::comma)

df$viewing <- ifelse(df$diffn < -5000 & 
                       df$price <= 250000 & 
                       df$sqmt > 100 &
                       df$beds > 2 &
                       (df$beds >= 2 | df$living > 1), 
                     "View", "Meh")

viewCol <- colorFactor(palette = c("white", "red"), df$viewing)

leaflet() %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addCircleMarkers(data = df, lng = ~lon, lat = ~lat, radius = 5,
                   options = markerOptions(opacity = 1),
                   color = ~viewCol(viewing),
                   popup = paste("House:", df$house,
                                 "<br>Date:", df$date,
                                 "<br>Price:", scales::comma(df$price),
                                 "<br>Expected Price:", scales::comma(df$expect),
                                 "<br>View:", df$viewing,
                                 "<br>Pricing:", df$over,
                                 "<br>House type:", df$type,
                                 "<br># Beds:", df$beds,
                                 "<br># Living:", df$living,
                                 "<br># Baths:", df$baths,
                                 "<br>Square meters:", df$sqmt,
                                 "<br>EPC rating:", df$epc,
                                 "<br>Tax band:", df$tax,
                                 "<br>", df$link)
  )
