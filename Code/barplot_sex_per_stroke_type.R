barplot_sex_per_stroke_type <- function(data = cleaned) {
  ggplot(data, aes(x = stroke.type, fill = SEX)) +
    geom_bar(position = "fill", width = 0.7, alpha = 0.9) +
    labs(title = "Vergleich von Geschlecht und Schlaganfalltyp",
         x = "Schlaganfalltyp", y = "Anteil", fill = "Geschlecht") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorogisch", "Ischaemic Stroke" = "Ischämisch", "Indeterminate Stroke" = "Unbestimmt", "Not a Stroke" = "Kein Schlaganfall")) +
    scale_fill_manual(
      values = c("M" = "#00BFC4", "F" = "#F8766D"),
      labels = c("M" = "Männlich", "F" = "Weiblich")
    ) +
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.4),
      axis.title = element_text(face = "bold", size = 10), 
      panel.grid.major.y = element_blank(),  
      panel.grid.minor.y = element_blank()
    )
}