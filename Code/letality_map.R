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
      colors = viridis::mako(100, direction = -1), 
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
    theme_minimal()
}