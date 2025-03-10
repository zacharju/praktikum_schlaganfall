library(rnaturalearth) # für ne_countries
library(tidyverse)
library(rnaturalearthdata)
library(sf)

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

cleaned$COUNTRY <- vapply(cleaned$CNTRYNUM, function(x) {
  i <- which(country_lookup$CNTRYNUM == x)
  if (length(i) == 1) {
    return(country_lookup$COUNTRY[i])
  } else {
    return(NA_character_)
  }
}, character(1))


#Kartendaten werden in World gespeichert
world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")

# Länder filtern
countries_sf <- world[world$name_long %in% unique(cleaned$COUNTRY), ]


# Mittelpunkte der Länder berechnen
middle <- data.frame(
  name = countries_sf$name_long,
  lon = st_coordinates(st_centroid(countries_sf))[ ,1],
  lat = st_coordinates(st_centroid(countries_sf))[ ,2]
)

#Manuelles Hinzufügen von Hong Kong und Singapur

manual_coords <- tibble(
  name = c("Hong Kong", "Singapore"),
  lon = c(114.1694, 103.8198),  # Longitudes
  lat = c(22.3193, 1.3521)       # Latitudes
)

middle <- bind_rows(middle, manual_coords)

#Tabelle mit Mortalitätsraten pro Land
letality_per_country<- cleaned |>
  group_by(COUNTRY) |>
  summarise(deaths = sum(FDEADC %in% c(1,2,3)), #Patienten, die nach 6 Monaten an Schlaganfall gestorben sind
            total_cases = n(),
            letality_rate = deaths/total_cases) |>
  arrange(desc(letality_rate)) |>
  print(n = 36)


# Left Join
letality_per_country_sf <- letality_per_country |>
  left_join(middle, by = c("COUNTRY" = "name")) |>
  print(n=36)


ggplot() + 
  geom_sf(data = world, colour = "grey30", fill = "antiquewhite") + 
  geom_point(data = letality_per_country_sf, 
             mapping = aes(x = lon, y = lat, size = letality_rate), 
             colour = "red", alpha = 0.8) + 
  coord_sf() +
  labs(x = "Längengrad", y = "Breitengrad", size = "Letalitätsrate", 
       title = "6-Monats-Letalität nach Schlaganfall pro Land") +
  scale_x_continuous(expand = c(0, 0), breaks = seq(-180,180, 40)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(-90, 90, 20)) +
  scale_size_area(max_size = 7) +  # Korrekte Skalierung über die Fläche
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
    plot.title = element_text(face = "bold", size = 15)
)

#Andere Optionen
#Bar Plot
ggplot(letality_per_country, aes(x = reorder(COUNTRY, letality_rate), y = letality_rate)) +
  geom_col(fill = "steelblue") +  
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "6-Monats-Letalität pro Land",
    x = "Land",
    y = "Anteil"
  ) +
  theme_minimal()