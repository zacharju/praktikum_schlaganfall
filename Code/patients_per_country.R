patients_per_country <- function(data = cleaned) {

  # Creating a tibble that maps country and country code
  country_lookup <- tibble(
    COUNTRY = c(
      "Albania", "Argentina", "Australia", "Austria", "Belgium", "Brazil",
      "Bulgaria", "Canada", "Chile", "Czech Republic", "Denmark", "Ireland",
      "Finland", "France", "Georgia", "Germany", "Greece", "Hong Kong",
      "Hungary", "India", "Indonesia", "Israel", "Italy", "Japan",
      "Latvia", "Malaysia", "Netherlands", "New Zealand", "Norway",
      "Poland", "Portugal", "Romania", "Singapore", "Slovakia", "Slovenia",
      "South Africa", "Spain", "Sri Lanka", "Sweden", "Switzerland", "Thailand",
      "Turkey", "United Kingdom", "United States"
    ),
    CNTRYNUM = c(
      43, 29, 1, 2, 3, 42, 4, 5, 6, 7, 8, 9, 10, 11, 32, 12, 31, 30, 36, 37, 41,
      13, 14, 38, 39, 40, 15, 16, 17, 18, 19, 33, 34, 44, 20, 21, 22, 23, 24,
      25, 26, 35, 27, 28
    )
  )

  # Replacing CNTRYNUM values with country names
  data$COUNTRY <- vapply(data$CNTRYNUM, function(x) {
    i <- which(country_lookup$CNTRYNUM == x)
    if (length(i) == 1) {
      country_lookup$COUNTRY[i]
    } else {
      NA_character_
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
  breaks <- c(200, 800, 3500, 6000) # Daten rechtsschief

  # Extracting colors
  lajolla_colors <- scico::scico(palette = "lajolla", n = 6,
                                 direction = -1)[1:6]

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
    scale_x_continuous(expand = c(0, 0), breaks = seq(-180, 180, 40)) +
    scale_y_continuous(expand = c(0, 0), breaks = seq(-90, 90, 20)) +
    theme_bw() +
    theme(
      panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                      linewidth = 0.5),
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5,
                                lineheight = 1.2),
      axis.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)
    )

}
