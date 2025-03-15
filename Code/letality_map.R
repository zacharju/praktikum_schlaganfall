patients_per_country <- function(data = cleaned) {
  
  # Creating a tibble that maps country and country code
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
  
  # Replacing CNTRYNUM values with country names
  data$COUNTRY <- vapply(data$CNTRYNUM, function(x) {
    i <- which(country_lookup$CNTRYNUM == x)
    if (length(i) == 1) {
      return(country_lookup$COUNTRY[i])
    } else {
      return(NA_character_)
    }
  }, character(1))
  
  # Loading world map data
  world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")
  
  # Creating a table with patient data (absolute numbers)
  patients_per_country <- data |>
    filter(!is.na(stroke.type)) |>
    group_by(COUNTRY) |>
    summarise(total_patients = n()) |>
    arrange(desc(total_patients))
  
  # Merging patient data with world map data
  countries_sf <- world |>
    left_join(patients_per_country, by = c("name_long" = "COUNTRY"))
  
  # Setting intervals
  breaks <- c(2, 10, 50, 200, 1000, 6000) #Daten rechtsschief

  # Extracting colors
  lajolla_colors <- scico::scico(palette = "lajolla", n = 6, direction = -1)[1:6]
  
  # Plotting
  ggplot() + 
    geom_sf(data = countries_sf, aes(fill = total_patients), color = "grey30") +
    scale_fill_stepsn(
      colors = lajolla_colors,
      na.value = "white",
      breaks = breaks,
      limits = c(0, 7000),
      show.limits = TRUE
    ) +
    coord_sf() +
    labs(
      x = "Breitengrad", 
      y = "Längengrad", 
      fill = "Anzahl", 
      title = "Absolute Häufigkeiten der Schlaganfallpatienten nach Land"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
    )
}

################################################################################################

patients_per_country_2 <- function(data = cleaned) {
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
  
  # Mapping country code to country name
  cleaned$COUNTRY <- vapply(cleaned$CNTRYNUM, function(x) {
    i <- which(country_lookup$CNTRYNUM == x)
    if (length(i) == 1) {
      return(country_lookup$COUNTRY[i])
    } else {
      return(NA_character_)
    }
  }, character(1))
  
  # Load world map data
  world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")
  
  # Filter countries based on the cleaned dataset
  countries_sf <- world[world$name_long %in% unique(cleaned$COUNTRY), ]
  
  # Calculate centroids (midpoints) for each country
  middle <- data.frame(
    name = countries_sf$name_long,
    lon = st_coordinates(st_centroid(countries_sf))[ ,1],
    lat = st_coordinates(st_centroid(countries_sf))[ ,2]
  )
  
  # Manually add coordinates for Hong Kong and Singapore
  manual_coords <- tibble(
    name = c("Hong Kong", "Singapore"),
    lon = c(114.1694, 103.8198),  # Longitudes
    lat = c(22.3193, 1.3521)       # Latitudes
  )
  
  middle <- bind_rows(middle, manual_coords)
  
  # Create table with patient data (total cases)
  patient_data_per_country <- cleaned |>
    group_by(COUNTRY) |>
    summarise(total_patients = n()) |>
    arrange(desc(total_patients)) |>
    print(n = 36)
  
  # Left Join with midpoints
  patient_data_sf <- patient_data_per_country |>
    left_join(middle, by = c("COUNTRY" = "name")) |>
    print(n=36)
  
  # Plot with dot size based on total number of patients
  ggplot() + 
    geom_sf(data = world, colour = "grey30", fill = "antiquewhite") + 
    geom_point(data = patient_data_sf, 
               mapping = aes(x = lon, y = lat, size = total_patients), 
               colour = "red", alpha = 0.8) + 
    coord_sf() +
    labs(x = "Längengrad", y = "Breitengrad", size = "Anzahl der Patienten", 
         title = "Absolute Häufigkeiten der Schlaganfallpatienten pro Land") +
    scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 40)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(-90, 90, 20)) +
    # Defining a more detailed size scale
    scale_size_continuous(
      range = c(2, 7),  # Define the min and max size for the points
      breaks = c(50, 200, 500, 1000, 5000, 6000),  # Set breakpoints for different categories
      labels = c("(0,50)", "(51,200)", "(201,500)", "(501,1000)", "(1001,5000)", "(>5000)"), 
      guide = guide_legend(reverse = TRUE)
    ) +  
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 15)
    )
}

################################################################################################

case_fatality_per_country <- function(data = cleaned) {
  
  # Creating a tibble that maps country and country code
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
  
  # Replacing CNTRYNUM values with country names
  data$COUNTRY <- vapply(data$CNTRYNUM, function(x) {
    i <- which(country_lookup$CNTRYNUM == x)
    if (length(i) == 1) {
      return(country_lookup$COUNTRY[i])
    } else {
      return(NA_character_)
    }
  }, character(1))
  
  # Loading world map data
  world <- ne_countries(scale = 110, type = "countries", returnclass = "sf")
  
  # Creating a table with case fatality rates
  cfr_per_country <- data |>
    filter(!is.na(stroke.type)) |>
    group_by(COUNTRY) |>
    summarise(deaths = sum(FDEAD == "Y"), # Patients who died after 6 months
              total_cases = n(),
              cfr = deaths / total_cases) |>
    arrange(desc(cfr))
  
  # Merging CFR data with world map data
  countries_sf <- world |>
    left_join(cfr_per_country, by = c("name_long" = "COUNTRY"))
  
  # Plotting the map with colored countries
  
  quantiles <- quantile(cfr_per_country$cfr, probs = seq(0, 1, length.out = 8))
  labels <- round(quantiles, 2)
  ggplot() + 
    geom_sf(data = countries_sf, aes(fill = cfr), color = "grey30") +
    scale_fill_stepsn(
      colors = scico::scico(palette = "lajolla", 100, direction = -1), 
      na.value = "white", 
      breaks = quantiles,
      labels = scales::percent(labels)
    ) +
    coord_sf() +
    labs(
      x = "Längengrad", 
      y = "Breitengrad", 
      fill = "Letalität", 
      title = "Letalität (case fatality) innerhalb von 6 Monaten nach Land"
    ) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = gray(.5), linetype = "dashed", linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5)
    )
}

################################################################################################

alternative_cf <- function(data = cleaned) {
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
  
  data$COUNTRY <- vapply(data$CNTRYNUM, function(x) {
    i <- which(country_lookup$CNTRYNUM == x)
    if (length(i) == 1) {
      return(country_lookup$COUNTRY[i])
    } else {
      return(NA_character_)
    }
  }, character(1))
  
  german_country_names <- tibble(
    COUNTRY = c("Albania", "Argentina", "Australia", "Austria", "Belgium", "Brazil", "Bulgaria", "Canada", "Chile", 
                "Czech Republic", "Denmark", "Ireland", "Finland", "France", "Georgia", "Germany", 
                "Greece", "Hong Kong", "Hungary", "India", "Indonesia", "Israel", "Italy", "Japan", 
                "Latvia", "Malaysia", "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", 
                "Romania", "Singapore", "Slovakia", "Slovenia", "South Africa", "Spain", 
                "Sri Lanka", "Sweden", "Switzerland", "Thailand", "Turkey", "United Kingdom", "United States"),
    
    COUNTRY_GERMAN = c("Albanien", "Argentinien", "Australien", "Österreich", "Belgien", "Brasilien", "Bulgarien", "Kanada", "Chile", 
                       "Tschechische Republik", "Dänemark", "Irland", "Finnland", "Frankreich", "Georgien", "Deutschland", 
                       "Griechenland", "Hongkong", "Ungarn", "Indien", "Indonesien", "Israel", "Italien", "Japan", 
                       "Lettland", "Malaysia", "Niederlande", "Neuseeland", "Norwegen", "Polen", "Portugal", 
                       "Rumänien", "Singapur", "Slowakei", "Slowenien", "Südafrika", "Spanien", 
                       "Sri Lanka", "Schweden", "Schweiz", "Thailand", "Türkei", "Vereinigtes Königreich", "Vereinigte Staaten")
  )
  
  #Creating a table with the proportion of people who have died of stroke after 6 months
  
  cfr_per_country <- data |>
    filter(!is.na(stroke.type)) |>
    group_by(COUNTRY) |>
    summarise(deaths = sum(FDEAD == "Y"),
              total_cases = n(),
              cfr = deaths/total_cases) |>
    arrange(desc(cfr))
  
  #Joining the tibble with cfr_per_country to get the German names
  
  cfr_per_country <- cfr_per_country |> 
    left_join(german_country_names, by = "COUNTRY") |> 
    mutate(COUNTRY = COUNTRY_GERMAN)
  
  #Plotting
  
  ggplot(cfr_per_country, aes(x = reorder(COUNTRY, cfr), y = cfr)) +
    geom_col(fill = "steelblue") +  
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = "Letalität (case fatality) innerhalb von 6 Monaten nach Land",
      x = "Land",
      y = "Letalität"
    ) + 
    theme_minimal() +
    theme(
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13)
    )
}