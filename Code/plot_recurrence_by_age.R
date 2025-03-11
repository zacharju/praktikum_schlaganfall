plot_recurrence_by_age <- function(data = cleaned) {
  ggplot(data, aes(x = recurring, y = AGE, fill = recurring)) +
    geom_boxplot() +
    labs(title = "Alter in Bezug auf wiederkehrende Schlaganfälle",
         x = "Schlaganfall",
         y = "Alter der Patienten") +
    scale_x_discrete(labels = c("Yes" = "Wiederkehrend", "No" = "Nicht wiederkehrend")) + 
    scale_fill_manual(values = c("Yes" = "#D55E00", "No" = "#0072B2"))+  
    scale_y_continuous(
      breaks = seq(0, 100, by = 5),  
      labels = seq(0, 100, by = 5)
    ) +
    theme_minimal() + 
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 10), 
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}