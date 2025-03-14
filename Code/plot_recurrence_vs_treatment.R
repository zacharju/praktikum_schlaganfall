plot_recurrence_vs_treatment <- function(data = cleaned) {
  
  filtered <- data |> 
    filter(!is.na(stroke.type)) |>
    filter(!is.na(recurring.stroke.type)) |> 
    filter(!is.na(treatment))
  
  ggplot(filtered, aes(x = treatment, fill = recurring.stroke.type)) +
    geom_bar(position = "fill", width = 0.8, alpha = 0.85) +  
    geom_text(stat = "count", 
              aes(label = sprintf("%.1f%%", ..count.. / tapply(..count.., ..x.., sum)[..x..] * 100)),
              position = position_fill(vjust = 0.5),  
              size = 4.2) +  
    labs(
      title = "Typen von wiederkehrenden Schlaganfällen je nach Therapie",
      x = "Medikation",
      y = "Anteil", 
      fill = "Schlaganfalltyp"
    ) +
    scale_fill_manual(
      name = "Schlaganfalltyp",
      values = c("#472d7b", "#218f8d", "#90d743"),
      labels = c("Haemorrhagic Stroke" = "Hämorrhagisch", 
                 "Ischaemic Stroke" = "Ischämisch", 
                 "Unknown Stroke" = "Unbestimmt")
    ) +
    scale_x_discrete(labels = c("No Treatment" = "Keine Medikation")) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) + 
    coord_flip() +
    theme_minimal() + 
    theme(
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 15), 
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major.y = element_blank(),  
      panel.grid.minor.y = element_blank(), 
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)
    )
}