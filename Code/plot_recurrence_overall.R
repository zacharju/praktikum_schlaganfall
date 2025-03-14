plot_recurrence_overall <- function(data = cleaned) {
  filtered <- data |>
    filter(!is.na(stroke.type))
  ggplot(filtered, aes(x = recurring, fill = recurring)) +
    geom_bar(width = 0.6, alpha = 0.85, fill = "grey", colour = "black") +
    geom_text(stat = "count", 
              aes(label = sprintf("%d (%.1f%%)", ..count.., ..count../sum(..count..)*100)), 
              vjust = -0.5, size = 4.3) +  # Display absolute values + percentage over the bars
    labs(title = "Häufigkeit wiederkehrender Schlaganfälle",
         x = "Schlaganfall",
         y = "Anzahl der Patienten") +
    scale_x_discrete(labels = c("Yes" = "Wiederkehrend", "No" = "Nicht wiederkehrend")) + 
    scale_y_continuous(
      breaks = seq(0, 20000, by = 2500),  
      labels = seq(0, 20000, by = 2500), 
      expand = expansion(mult = c(0, 0.05))
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
