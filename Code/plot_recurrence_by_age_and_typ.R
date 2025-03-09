plot_recurrence_by_age_and_typ <- function(clean_data) {
  
  filtered_data <- clean_data %>%
    filter(!is.na(recurring.stroke.type))
  
  ggplot(filtered_data, aes(x = recurring.stroke.type, y = AGE, fill = recurring.stroke.type)) +
    geom_boxplot() +
    labs(title = "Alter und Typen von wiederkehrenden Schlaganfällen",
         x = "Typ vom wiederkehrenden Schlaganfall",
         y = "Alter", 
         fill = "Typ") + 
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorogisch", "Ischaemic Stroke" = "Ischämisch", "Unknown Stroke" = "Unbekannt")) +  
    scale_fill_brewer(palette = "Pastel1", labels = c("Haemorrhagic Stroke" = "Hämorogischer Schlaganfall", "Ischaemic Stroke" = "Ischämischer Schlaganfall", "Unknown Stroke" = "Unbekannter Schlaganfall")) +  
    scale_y_continuous(
      breaks = seq(0, 100, by = 5),  
      labels = seq(0, 100, by = 5)
    ) +
    facet_wrap(~ SEX, labeller = as_labeller(c("M" = "Männlich", "F" = "Weiblich"))) + 
    theme_minimal() + 
    theme(
      aspect.ratio = 1,
      plot.background = element_rect(colour = "black", linewidth = 1, fill = "white"), 
      plot.title = element_text(face = "bold", size = 13, hjust = 0.6),
      axis.title = element_text(face = "bold", size = 10), 
      axis.title.x = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}
