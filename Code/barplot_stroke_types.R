barplot_stroke_types <- function(data = cleaned) {
  ggplot(data, aes(x = stroke.type)) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(title = "Häufigkeit der Schlaganfalltypen",
         x = "Schlaganfalltyp",
         y = "Anzahl") +
    theme_minimal()
}