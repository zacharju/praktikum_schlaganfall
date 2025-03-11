plot_recurrence_by_age_and_typ <- function(data = cleaned) {
  
  filtered <- data |>
    filter(!is.na(recurring.stroke.type))
  
  ggplot(filtered, aes(x = recurring.stroke.type, y = AGE, fill = recurring.stroke.type)) +
    geom_boxplot() +
    labs(title = "Alter nach Typ des wiederkehrenden Schlaganfalls",
         x = "Typ des wiederkehrenden Schlaganfalls",
         y = "Alter der Patienten") + 
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorogisch", "Ischaemic Stroke" = "Ischämisch", "Unknown Stroke" = "Unbekannt")) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.85) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 5),  
      labels = seq(0, 100, by = 5)
    ) +
    facet_wrap(~ SEX, labeller = as_labeller(c("M" = "Männlich", "F" = "Weiblich"))) +
    theme_minimal() + 
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 10), 
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}