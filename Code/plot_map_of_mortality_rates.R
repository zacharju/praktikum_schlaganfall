#case_fatality_rate_per_country <- function(data = cleaned) {
  
#Creating a tibble that maps country and country code as defined in the paper
  
country_lookup <- tibble(
  COUNTRY = c("Albania", "Argentina", "Australia", "Austria", "Belgium", "Brazil", "Bulgaria", "Canada", "Chile", 
                "Czech Republic", "Denmark", "Ireland", "Finland", "France", "Georgia", "Germany", 
                "Greece", "Hong Kong", "Hungary", "India", "Indonesia", "Israel", "Italy", "Japan", 
                "Latvia", "Malaysia", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", 
                "Romania", "Singapore", "Slovakia", "Slovenia", "South Africa", "Spain", 
                "Sri Lanka", "Sweden", "Switzerland", "Thailand", "Turkey", "United Kingdom", "United States"),
  CNTRYNUM = c(43, 29, 1, 2, 3, 42, 4, 5, 6, 7, 8, 9, 10, 11, 32, 12, 31, 30, 36, 37, 41, 
                 13, 14, 38, 39, 40, 15, 16, 17, 18, 19, 33, 34, 44, 20, 21, 22, 23, 24, 25, 26, 35, 27, 28)
)
  
#Changing all values in the COUNTRY variable with the tibble created above
  
cleaned$COUNTRY <- vapply(cleaned$CNTRYNUM, function(x) {
  i <- which(country_lookup$CNTRYNUM == x)
    if (length(i) == 1) {
      return(country_lookup$COUNTRY[i])
    } else {
      return(NA_character_)
    }
}, character(1))
  
#Saving map data in variable "world"
world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")
  
#Filtering relevant countries
countries_sf <- world[world$name_long %in% unique(cleaned$COUNTRY), ]
  
  
# Calculating midpoints for every country
middle <- data.frame(
  name = countries_sf$name_long,
  lon = st_coordinates(st_centroid(countries_sf))[ ,1],
  lat = st_coordinates(st_centroid(countries_sf))[ ,2]
)
  
#Manually adding Hong Kong and Singapore
  
manual_coords <- tibble(
  name = c("Hong Kong", "Singapore"),
  lon = c(114.1694, 103.8198),  # Longitudes
  lat = c(22.3193, 1.3521)       # Latitudes
)
  
middle <- bind_rows(middle, manual_coords)
  
#Creating a table with the proportion of people who have died of stroke after 6 months
  
cfr_per_country <- cleaned |>
  group_by(COUNTRY) |>
  summarise(deaths = sum(FDEADC %in% c(1,2,3)), #Patients that dies after 6 months of stroke
            total_cases = n(),
            cfr_rate = deaths/total_cases) |>
  arrange(desc(cfr_rate)) |>
  print(n = 36)
  
#Joining the creating table with the table that contains the calculated midpoints
cfr_per_country <- cfr_per_country |>
  left_join(middle, by = c("COUNTRY" = "name")) |>
  print(n=36)
  
#Plotting
#Option 1
ggplot() + 
  geom_sf(data = world, colour = "grey30", fill = "antiquewhite2") + 
  geom_point(data = cfr_per_country, 
             mapping = aes(x = lon, y = lat, color = cfr_rate), 
             size = 5, alpha = 0.9) +
  coord_sf() +
  labs(x = "Längengrad", 
       y = "Breitengrad", 
       color = "Anteil", 
       title = "Proportion der nach 6 Monaten verstorbenen Schlaganfallpatienten pro Land") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180,180, 40)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-90, 90, 20)) + 
  scale_color_gradientn(colors = viridis::viridis(100, direction = -1)[10:100]) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 15)
)

#Option 2
ggplot() + 
  geom_sf(data = world, colour = "grey30", fill = "antiquewhite2") + 
  geom_point(data = cfr_per_country, 
             mapping = aes(x = lon, y = lat, color = cfr_rate), 
             size = 5, alpha = 0.9) +
  coord_sf() +
  labs(x = "Längengrad", 
       y = "Breitengrad", 
       color = "Anteil", 
       title = "Proportion der nach 6 Monaten verstorbenen Schlaganfallpatienten pro Land") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180,180, 40)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-90, 90, 20)) + 
  scale_color_gradientn(colors = viridis::magma(100, direction = -1)[20:100]) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 15)
)

#Option 3
ggplot() + 
  geom_sf(data = world, colour = "grey30", fill = "antiquewhite2") + 
  geom_point(data = cfr_per_country, 
             mapping = aes(x = lon, y = lat, color = cfr_rate), 
             size = 5, alpha = 0.9) +
  coord_sf() +
  labs(x = "Längengrad", 
       y = "Breitengrad", 
       color = "Anteil", 
       title = "Proportion der nach 6 Monaten verstorbenen Schlaganfallpatienten pro Land") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180,180, 40)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-90, 90, 20)) + 
  scale_color_gradientn(colors = viridis::inferno(100, direction = -1)[10:100]) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 15)
  )

#Option 4
ggplot() + 
  geom_sf(data = world, colour = "grey30", fill = "antiquewhite2") + 
  geom_point(data = cfr_per_country, 
             mapping = aes(x = lon, y = lat, color = cfr_rate), 
             size = 5, alpha = 0.9) +
  coord_sf() +
  labs(x = "Längengrad", 
       y = "Breitengrad", 
       color = "Anteil", 
       title = "Proportion der nach 6 Monaten verstorbenen Schlaganfallpatienten pro Land") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180,180, 40)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-90, 90, 20)) + 
  scale_color_gradientn(colors = viridis::mako(100, direction = -1)[10:100]) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 15)
  )

#Alternative visualisation: bar plot
ggplot(cfr_per_country, aes(x = reorder(COUNTRY, cfr_rate), y = cfr_rate)) +
  geom_col(fill = "steelblue") +  
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion der nach 6 Monaten verstorbenen Schlaganfallpatienten pro Land",
    x = "Land",
    y = "Anteil"
  ) + theme_minimal()
#}