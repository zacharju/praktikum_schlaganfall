plot_recurrence_by_age <- function(clean_data) {
  ggplot(clean_data, aes(x = recurring, y = AGE, fill = recurring)) +
    geom_boxplot() +
    labs(title = "Alter und Wiederkehrende Schlaganfälle",
         x = "Wiederkehrender Schlaganfall",
         y = "Alter", 
         fill = "Wiederkehrend") +
    scale_x_discrete(labels = c("Yes" = "Ja", "No" = "Nein")) +  
    scale_fill_brewer(palette = "Pastel1", labels = c("Yes" = "Ja", "No" = "Nein")) +  
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