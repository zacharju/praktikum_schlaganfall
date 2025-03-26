barplot_stroke_types_update <- function(data = cleaned) {
  filtered <- data |>
    filter(!is.na(stroke.type))
  ggplot(filtered, aes(x = factor(stroke.type, levels = c("Haemorrhagic Stroke", "Ischaemic Stroke", "Indeterminate Stroke")), fill = stroke.type)) +
    geom_bar(width = 0.7, alpha = 0.85) +
    geom_text(stat = "count", 
              aes(label = sprintf("%d (%.1f%%)", ..count.., ..count../sum(..count..)*100)), 
              vjust = -0.5, size = 4.3) +
    labs(title = "Häufigkeit der Schlaganfalltypen",
         x = "Schlaganfalltyp",
         y = "Anzahl der Patienten") +
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorrhagisch", "Ischaemic Stroke" = "Ischämisch", "Indeterminate Stroke" = "Indeterminate")) +
    scale_y_continuous(
      breaks = seq(0, 20000, by = 2500),  
      labels = seq(0, 20000, by = 2500), 
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_fill_manual(
      values = c("Haemorrhagic Stroke" = "#472d7b", 
                 "Ischaemic Stroke" = "#218f8d", 
                 "Indeterminate Stroke" = "#b6308b")
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