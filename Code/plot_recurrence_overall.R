plot_recurrence_overall <- function(clean_data) {
  ggplot(clean_data, aes(x = recurring, fill = recurring)) +
    geom_bar() +
    geom_text(stat = "count", 
              aes(label = sprintf("%d (%.1f%%)", ..count.., ..count../sum(..count..)*100)), 
              vjust = -0.5, size = 4) +  # Absolutwerte + Prozentsatz über die Balken anzeigen
    labs(title = "Häufigkeit von wiederkehrenden Schlaganfällen",
         x = "Wiederkehrender Schlaganfall",
         y = "Anzahl der Patienten", 
         fill = "Wiederkehrend") +
    scale_x_discrete(labels = c("Yes" = "Ja", "No" = "Nein")) +  
    scale_fill_brewer(palette = "Pastel1", labels = c("Yes" = "Ja", "No" = "Nein")) + 
    scale_y_continuous(
      breaks = seq(0, 20000, by = 2500),  
      labels = seq(0, 20000, by = 2500) 
    ) +
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