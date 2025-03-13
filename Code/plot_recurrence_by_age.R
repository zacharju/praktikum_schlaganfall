plot_recurrence_by_age <- function(data = cleaned) {
  filtered <- data |>
    filter(!is.na(stroke.type))
  ggplot(filtered, aes(x = recurring, y = AGE, fill = recurring)) +
    geom_boxplot(width = 0.6, alpha = 0.85) +
    labs(title = "Alter in Bezug auf wiederkehrende Schlaganfälle",
         x = "Schlaganfall",
         y = "Alter der Patienten") +
    scale_x_discrete(labels = c("Yes" = "Wiederkehrend", "No" = "Nicht wiederkehrend")) + 
    scale_fill_manual(values = c("Yes" = "#f8850f", "No" = "#3e4c8a"))+  
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),  
      labels = seq(0, 100, by = 10)
    ) +
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