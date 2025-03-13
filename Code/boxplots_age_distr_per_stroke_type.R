boxplots_age_distr_per_stroke_type <- function(data = cleaned) {
  filtered <- data |>
    filter(!is.na(stroke.type))
  ggplot(filtered, aes(x = stroke.type, y = AGE, fill = stroke.type)) +
    geom_boxplot(width = 0.7, alpha = 0.85) +
    labs(title = "Alter nach Typ des Schlaganfalls", x = "Schlaganfalltyp", y = "Alter") +
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorrhagisch", "Ischaemic Stroke" = "Ischämisch", "Unknown Stroke" = "Unbestimmt")) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 5),  
      labels = seq(0, 100, by = 5)
    ) +
    scale_fill_manual(values = c("#472d7b", "#218f8d", "#90d743")) +
    theme_minimal() + 
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 15), 
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 14),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}