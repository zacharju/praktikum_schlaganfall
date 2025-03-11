barplot_stroke_types <- function(data = cleaned) {
  ggplot(data, aes(x = stroke.type, fill = stroke.type)) +
    geom_bar(width = 0.7) +
    geom_text(stat = "count", 
              aes(label = sprintf("%d (%.1f%%)", ..count.., ..count../sum(..count..)*100)), 
              vjust = -0.5, size = 3) +
    labs(title = "Häufigkeit der Schlaganfalltypen",
         x = "Schlaganfalltyp",
         y = "Anzahl der Patienten") +
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorrhagisch", "Ischaemic Stroke" = "Ischämisch", "Indeterminate Stroke" = "Unbestimmt", "Not a Stroke" = "Kein Schlaganfall")) +
    scale_y_continuous(
      breaks = seq(0, 20000, by = 2500),  
      labels = seq(0, 20000, by = 2500), 
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_viridis_d(option = "magma", alpha = 0.7) +
    theme_minimal() + 
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 13, hjust = 0.4),
      axis.title = element_text(face = "bold", size = 10), 
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}