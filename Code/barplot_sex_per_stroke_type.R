barplot_sex_per_stroke_type <- function(data = cleaned) {
  ggplot(data, aes(x = stroke.type, fill = SEX)) +
    geom_bar(position = "fill") +
    labs(title = "Vergleich von Geschlecht und Schlaganfalltyp",
         x = "Schlaganfalltyp", y = "Anteil", fill = "Geschlecht") +
    theme_minimal() +
    coord_flip() +
    scale_y_continuous(labels = scales::percent)
}