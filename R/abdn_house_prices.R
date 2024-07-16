library(googlesheets4) # For loading in the data from google sheets
library(ggplot2)       # For data visualiations
library(mgcv)          # For non-linear models
library(leaflet)
library(lubridate)
library(htmlwidgets)
library(geosphere)
library(httr)
library(jsonlite)
theme_set(theme_minimal())

df <- read_sheet("https://docs.google.com/spreadsheets/d/1WQNnBK6P4ml9o8XyA9zMXXe2Rh-pY6boQEA1zR1nsus/edit?usp=sharing",
                 sheet = "Sheet1", 
                 trim_ws = TRUE)

# Code to check if house has been added -----------------------------------
# When doing data entry
house <- "71 Boswell Road"
df[df$house == house,]

# Remove duplicates -------------------------------------------------------
df <- df[!duplicated(df),]
df$rooms <- df$beds + df$living

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

abdn_uni <- c(57.168010390142236, -2.106429897150815)

calculate_distance <- function(lat, lon, abdn_uni) {
  point <- c(lat, lon)
  dist <- distHaversine(point, abdn_uni)
  return(dist)
}

df$distance <- mapply(calculate_distance, df$lat, df$lon, MoreArgs = list(abdn_uni = abdn_uni))
df$uni_dist <- ifelse(df$distance <= 25000, "Close Enough", "Too Far")

# Commute time ------------------------------------------------------------

api_key <- "AIzaSyDZKT-rGxUv21PxNFO4elG-n4m31gOqn_M"
origin <- "57.168010390142236,-2.106429897150815" # abdn uni
bioss <- "57.13310577362852,-2.158274358135975"
get_commute_time <- function(lat, lon, api_key, origin) {
  url <- paste0(
    "https://maps.googleapis.com/maps/api/distancematrix/json?units=metric",
    "&origins=", origin,
    "&destinations=", lat, ",", lon,
    "&mode=driving",
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

df$commute_time_bioss <- mapply(get_commute_time, df$lat, df$lon, MoreArgs = list(api_key = api_key, origin = bioss))
df$commute_time_bioss_minutes <- df$commute_time_bioss / 60

ggplot(df[df$lat > 57.08645329329407 & df$lat < 57.23073295950531 &
            df$lon > -2.278825150318679 & df$lon < 2.0547631408365854,]) +
  geom_point(aes(x = lon, y = lat, colour = commute_time_minutes)) +
  scale_colour_viridis_c()

ggplot(df) +
  geom_point(aes(x = lon, y = lat, colour = commute_time_bioss_minutes)) +
  scale_colour_viridis_c()

# House criteria ----------------------------------------------------------

df$viewing <- ifelse(df$over != "Overpriced" &                 # Can't be over priced
                       df$price <= 260000 &                    # Below help2 buy ISA threshold
                       df$price >= 180000 &                    # Below help2 buy ISA threshold
                       df$sqmt > 100 &                         # Not tiny
                       df$house != "terrace" &                 # Not a terrace
                       df$commute_time_minutes < 20 &          # Within 20 minute drive of uni
                       #df$commute_time_bioss_minutes < 20 &    # Within 20 minute drive of uni
                       df$rooms > 4,                           # Enough rooms for offices etc
                     "View", "Meh")

table(df$viewing)

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
      "<br><b>Commute to Aberdeen Uni:</b> ", round(df$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(df$commute_time_bioss_minutes), " minutes",
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
      "<br><b>Commute to Aberdeen Uni:</b> ", round(df_view$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(df_view$commute_time_bioss_minutes), " minutes",
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
      "<br><b>Commute to Aberdeen Uni:</b> ", round(df_meh$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(df_meh$commute_time_bioss_minutes), " minutes",
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

saveWidget(abdn_map, here::here("output", file = "abdn_viewing.html"), selfcontained = TRUE)


# Price range -------------------------------------------------------------

df$budget <- ifelse(df$price > 180000 & df$price <= 250000, "Within budget",
                    ifelse(df$price > 250000 & df$price <= 260000, "Maybe within",
                           ifelse(df$price > 260000 & df$price <= 280000, "If seller desperate",
                                  ifelse(df$price < 180000, "Cheap",
                                  "Out of budget")
                           )
                    )
)

within <- awesomeIconList(
  "Within budget" = makeAwesomeIcon(
    icon = "home",
    markerColor = "green",
    library = "fa"
  )
)
maybe <- awesomeIconList(
  "Maybe within" = makeAwesomeIcon(
    icon = "home",
    markerColor = "white", 
    library = "fa"
  )
)
desperate <- awesomeIconList(
  "If seller desperate" = makeAwesomeIcon(
    icon = "home",
    markerColor = "orange", 
    library = "fa"
  )
)
oob <- awesomeIconList(
  "Out of budget" = makeAwesomeIcon(
    icon = "home",
    markerColor = "red", 
    library = "fa"
  )
)

cheap <- awesomeIconList(
  "Cheap" = makeAwesomeIcon(
    icon = "home",
    markerColor = "lightblue", 
    library = "fa"
  )
)

df_within <- df[df$budget == "Within budget", ]
df_maybe <- df[df$budget == "Maybe within", ]
df_desperate <- df[df$budget == "If seller desperate", ]
df_oob <- df[df$budget == "Out of budget", ]
df_cheap <- df[df$budget == "Cheap", ]


# Leaflet map -------------------------------------------------------------

abdn_map <- leaflet() %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addAwesomeMarkers(
    data = df_within,
    lng = ~lon, lat = ~lat,
    icon = ~ within[budget],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_within$house, "</b>",
      "<br><b>Commute to Aberdeen Uni:</b> ", round(df_within$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(df_within$commute_time_bioss_minutes), " minutes",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_within$viewing,
      "<br><b>Pricing:</b> ", df_within$over,
      "<br><b>Budget:</b> ", df_within$budget,
      "<br><b>Asking Price:</b> £", scales::comma(df_within$price), " (", abs(round(df_within$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_within$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_within$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_within$expect),
      "<br><b>Expected Price Range:</b> £", round(df_within$low / 1000, digits = 0), "k - ", round(df_within$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_within$date,
      "<br><b>House type:</b> ", df_within$type,
      "<br><b># Bedrooms:</b> ", df_within$beds,
      "<br><b># Living rooms:</b> ", df_within$living,
      "<br><b># Bathrooms:</b> ", df_within$baths,
      "<br><b>Square meters:</b> ", df_within$sqmt,
      "<br><b>EPC rating:</b> ", df_within$epc,
      "<br><b>Tax band:</b> ", df_within$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_within$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "Within budget"
  ) %>%
  addAwesomeMarkers(
    data = df_cheap,
    lng = ~lon, lat = ~lat,
    icon = ~ cheap[budget],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_cheap$house, "</b>",
      "<br><b>Commute to Aberdeen Uni:</b> ", round(df_cheap$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(df_cheap$commute_time_bioss_minutes), " minutes",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_cheap$viewing,
      "<br><b>Pricing:</b> ", df_cheap$over,
      "<br><b>Budget:</b> ", df_cheap$budget,
      "<br><b>Asking Price:</b> £", scales::comma(df_cheap$price), " (", abs(round(df_cheap$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_cheap$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_cheap$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_cheap$expect),
      "<br><b>Expected Price Range:</b> £", round(df_cheap$low / 1000, digits = 0), "k - ", round(df_cheap$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_cheap$date,
      "<br><b>House type:</b> ", df_cheap$type,
      "<br><b># Bedrooms:</b> ", df_cheap$beds,
      "<br><b># Living rooms:</b> ", df_cheap$living,
      "<br><b># Bathrooms:</b> ", df_cheap$baths,
      "<br><b>Square meters:</b> ", df_cheap$sqmt,
      "<br><b>EPC rating:</b> ", df_cheap$epc,
      "<br><b>Tax band:</b> ", df_cheap$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_cheap$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "Cheap"
  ) %>%
  addAwesomeMarkers(
    data = df_maybe,
    lng = ~lon, lat = ~lat,
    icon = ~ maybe[budget],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_maybe$house, "</b>",
      "<br><b>Commute to Aberdeen Uni:</b> ", round(df_maybe$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(df_maybe$commute_time_bioss_minutes), " minutes",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_maybe$viewing,
      "<br><b>Pricing:</b> ", df_maybe$over,
      "<br><b>Budget:</b> ", df_maybe$budget,
      "<br><b>Asking Price:</b> £", scales::comma(df_maybe$price), " (", abs(round(df_maybe$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_maybe$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_maybe$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_maybe$expect),
      "<br><b>Expected Price Range:</b> £", round(df_maybe$low / 1000, digits = 0), "k - ", round(df_maybe$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_maybe$date,
      "<br><b>House type:</b> ", df_maybe$type,
      "<br><b># Bedrooms:</b> ", df_maybe$beds,
      "<br><b># Living rooms:</b> ", df_maybe$living,
      "<br><b># Bathrooms:</b> ", df_maybe$baths,
      "<br><b>Square meters:</b> ", df_maybe$sqmt,
      "<br><b>EPC rating:</b> ", df_maybe$epc,
      "<br><b>Tax band:</b> ", df_maybe$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_maybe$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "Maybe within"
  ) %>%
  addAwesomeMarkers(
    data = df_desperate,
    lng = ~lon, lat = ~lat,
    icon = ~ desperate[budget],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_desperate$house, "</b>",
      "<br><b>Commute to Aberdeen Uni:</b> ", round(df_desperate$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(df_desperate$commute_time_bioss_minutes), " minutes",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_desperate$viewing,
      "<br><b>Pricing:</b> ", df_desperate$over,
      "<br><b>Budget:</b> ", df_desperate$budget,
      "<br><b>Asking Price:</b> £", scales::comma(df_desperate$price), " (", abs(round(df_desperate$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_desperate$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_desperate$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_desperate$expect),
      "<br><b>Expected Price Range:</b> £", round(df_desperate$low / 1000, digits = 0), "k - ", round(df_desperate$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_desperate$date,
      "<br><b>House type:</b> ", df_desperate$type,
      "<br><b># Bedrooms:</b> ", df_desperate$beds,
      "<br><b># Living rooms:</b> ", df_desperate$living,
      "<br><b># Bathrooms:</b> ", df_desperate$baths,
      "<br><b>Square meters:</b> ", df_desperate$sqmt,
      "<br><b>EPC rating:</b> ", df_desperate$epc,
      "<br><b>Tax band:</b> ", df_desperate$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_desperate$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "If seller desperate"
  ) %>%
  addAwesomeMarkers(
    data = df_oob,
    lng = ~lon, lat = ~lat,
    icon = ~ oob[budget],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", df_oob$house, "</b>",
      "<br><b>Commute to Aberdeen Uni:</b> ", round(df_oob$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(df_oob$commute_time_bioss_minutes), " minutes",
      "<hr>",
      "<br><b>View suggestion:</b> ", df_oob$viewing,
      "<br><b>Pricing:</b> ", df_oob$over,
      "<br><b>Budget:</b> ", df_oob$budget,
      "<br><b>Asking Price:</b> £", scales::comma(df_oob$price), " (", abs(round(df_oob$diffn / 1000, digits = 0)), "k",
      ifelse(round(df_oob$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(df_oob$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(df_oob$expect),
      "<br><b>Expected Price Range:</b> £", round(df_oob$low / 1000, digits = 0), "k - ", round(df_oob$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", df_oob$date,
      "<br><b>House type:</b> ", df_oob$type,
      "<br><b># Bedrooms:</b> ", df_oob$beds,
      "<br><b># Living rooms:</b> ", df_oob$living,
      "<br><b># Bathrooms:</b> ", df_oob$baths,
      "<br><b>Square meters:</b> ", df_oob$sqmt,
      "<br><b>EPC rating:</b> ", df_oob$epc,
      "<br><b>Tax band:</b> ", df_oob$tax,
      "<br>",
      "<hr>",
      "<br><a href='", df_oob$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    ),
    group = "Out of budget"
  ) %>%
  addLayersControl(
    overlayGroups = c(
      "Cheap",
      "Within budget",
      "Maybe within",
      "If seller desperate",
      "Out of budget"),
    options = layersControlOptions(collapsed = TRUE)  # Adjust collapse behavior as needed
  )

abdn_map

saveWidget(abdn_map, here::here("output", file = "abdn_budget.html"), selfcontained = TRUE)


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

abdn_map <- leaflet(today_df) %>%
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addAwesomeMarkers(
    #data = today_df,
    lng = ~lon, lat = ~lat,
    icon = ~ pricing[over],
    label = ~ house,
    popup = paste0(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.5;'>",
      "<b>", today_df$house, "</b>",
      "<br><b>Commute to Aberdeen Uni:</b> ", round(today_df$commute_time_minutes), " minutes",
      "<br><b>Commute to BioSS:</b> ", round(today_df$commute_time_bioss_minutes), " minutes",
      "<hr>",
      "<br><b>Pricing:</b> ", today_df$over,
      "<br><b>Asking Price:</b> £", scales::comma(today_df$price), " (", abs(round(today_df$diffn / 1000, digits = 0)), "k",
      ifelse(round(today_df$diffn / 1000, digits = 0) < 0, " under expected)", ifelse(round(today_df$diffn / 1000, digits = 0) > 0, " over expected)", ")")),
      "<br><b>Expected Price:</b> £", scales::comma(today_df$expect),
      "<br><b>Expected Price Range:</b> £", round(today_df$low / 1000, digits = 0), "k - ", round(today_df$upp / 1000, digits = 0), "k",
      "<br>",
      "<hr>",
      "<br><b>Date added:</b> ", today_df$date,
      "<br><b>House type:</b> ", today_df$type,
      "<br><b># Bedrooms:</b> ", today_df$beds,
      "<br><b># Living rooms:</b> ", today_df$living,
      "<br><b># Bathrooms:</b> ", today_df$baths,
      "<br><b>Square meters:</b> ", today_df$sqmt,
      "<br><b>EPC rating:</b> ", today_df$epc,
      "<br><b>Tax band:</b> ", today_df$tax,
      "<br>",
      "<hr>",
      "<br><a href='", today_df$link, "' target='_blank' style='color: #2A5DB0; text-decoration: none;'><b>House listing</b></a>",
      "</div>"
    )
  )

abdn_map

saveWidget(abdn_map, here::here("output", file = "abdn_budget.html"), selfcontained = TRUE)


# Spatial prediction ------------------------------------------------------

gratia::draw(m1)

# Specific prediction -----------------------------------------------------

dream_house <- data.frame(
  lat = 57.15483899436254,
  lon = -2.269886390197508,
  type = "detached",
  beds = 3,
  living = 1,
  rooms = 4,
  baths = 3,
  epc = "c",
  tax = "d",
  sqmt = 110
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
  rooms = seq(min(df$rooms), max(df$rooms[df$price < 1500000]), length.out = 25),
  beds = median(df$beds),
  baths = median(df$baths),
  living = median(df$living),
  sqmt = seq(min(df$sqmt), max(df$sqmt[df$price < 1500000]), length.out = 25)
)

prds <- predict(m1, newdata = nu_data, se.fit = TRUE)
nu_data$fit <- prds$fit

nu_data$fit[exclude.too.far(
  nu_data$rooms, nu_data$sqmt,
  df$rooms, df$sqmt,
  dist = 0.1)] <- NA

ggplot() +
  geom_raster(data = nu_data, aes(x = rooms, y = sqmt, fill = fit)) +
  scale_fill_viridis_c(option = "magma", na.value = "transparent", labels = scales::comma) +
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
  dist = 0.08)] <- NA

ggplot() +
  geom_raster(data = nu_data, aes(x = lon , y = lat, fill = fit)) +
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white") +
  geom_point(data = df, aes(x = lon, y = lat), colour = "white", size = 0.5) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  coord_sf() +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Expected\nprice")

# Aberdeen ----------------------------------------------------------------

nu_data <- expand.grid(
  lat = seq(57.08645329329407, 57.23073295950531, length.out = 50),
  lon = seq(-2.278825150318679, -2.0547631408365854, length.out = 50),
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
  geom_contour(data = nu_data, aes(x = lon , y = lat, z = fit), colour = "white") +
  #geom_point(data = df, aes(x = lon, y = lat), colour = "white", size = 0.5) +
  scale_fill_viridis_c(option = "magma", labels = scales::comma, na.value = "transparent") +
  coord_sf() +
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
  #geom_jitter(data = df, aes(x = epc, y = price), width = 0.1, alpha = 0.1) +
  geom_errorbar(data = nu_data, aes(x = epc, y = fit, ymin = low, ymax = upp), width = 0.1) +
  geom_point(data = nu_data, aes(x = epc, y = fit)) +
  #scale_y_continuous(limits = c(0,NA), labels = scales::comma) +
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
  scale_y_continuous(limits = c(0,1000000), labels = scales::comma) +
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
  scale_y_continuous(limits = c(NA,1000000), labels = scales::comma) +
  scale_x_continuous(limits = c(NA,1000000), labels = scales::comma) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x = "Expected price",
       y = "Listed price",
       colour = "Pricing")
  

ggplot(df) +
  geom_point(aes(x = price, y = diffn, colour = over)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_rect(xmin = 180000, xmax =  250000, ymin = min(df$diffn), ymax = 0, linetype = 2, fill = "transparent", colour = "black") +
  scale_y_continuous(limits = c(NA,1000000), labels = scales::comma) +
  scale_x_continuous(limits = c(NA,1000000), labels = scales::comma)

ggplot(df) +
  geom_point(aes(x = diffn, y = price, colour = viewing, size = viewing)) +
  geom_vline(xintercept = 0) +
  geom_rect(ymin = 180000, ymax =  250000, xmin = min(df$diffn), xmax = 0, linetype = 2, fill = "transparent", colour = "black") +
  scale_y_continuous(limits = c(NA,NA), labels = scales::comma) +
  scale_x_continuous(limits = c(NA,NA), labels = scales::comma)
