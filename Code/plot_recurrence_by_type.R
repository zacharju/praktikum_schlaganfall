plot_recurrence_by_type <- function(data = cleaned) {
  
  filtered <- cleaned |>
    filter(!is.na(recurring.stroke.type))
  
  ggplot(filtered, aes(x = recurring.stroke.type, fill = recurring.stroke.type)) +
    geom_bar(position = "dodge", width = 0.6) +
    geom_text(stat = "count", 
              aes(label = sprintf("%d (%.1f%%)", ..count.., ..count../sum(..count..)*100)), 
              vjust = -0.5, size = 3) +  
    labs(title = "Häufigkeit wiederkehrender Schlaganfälle nach Typ",
         x = "Typ des wiederkehrenden Schlaganfalls",
         y = "Anzahl der Patienten") +
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorogisch", "Ischaemic Stroke" = "Ischämisch", "Unknown Stroke" = "Unbekannt")) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.85) +
    scale_y_continuous(
      breaks = seq(0, 225, by = 25),
      labels = seq(0, 225, by = 25), 
      expand = expansion(mult = c(0, 0.05))
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