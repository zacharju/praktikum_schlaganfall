boxplots_age_distr_per_stroke_type <- function(data = cleaned) {
  ggplot(data, aes(x = stroke.type, y = AGE, fill = stroke.type)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Alter nach Typ des Schlaganfalls", x = "Schlaganfalltyp", y = "Alter") +
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorrhagisch", "Ischaemic Stroke" = "Ischämisch", "Indeterminate Stroke" = "Unbestimmt", "Not a Stroke" = "Kein Schlaganfall")) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 5),  
      labels = seq(0, 100, by = 5)
    ) +
    scale_fill_viridis_d(option = "magma", alpha = 0.85) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 10), 
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    ) 
}