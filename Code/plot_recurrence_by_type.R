plot_recurrence_by_type <- function(data = cleaned) {
  
  filtered <- cleaned |>
    filter(!is.na(stroke.type)) |>
    filter(!is.na(recurring.stroke.type))
  
  ggplot(filtered, aes(x = recurring.stroke.type, fill = recurring.stroke.type)) +
    geom_bar(position = "dodge", width = 0.6, alpha = 0.85) +
    geom_text(stat = "count", 
              aes(label = sprintf("%d (%.1f%%)", ..count.., ..count../sum(..count..)*100)), 
              vjust = -0.5, size = 4.1) +  
    labs(title = "Häufigkeit wiederkehrender Schlaganfälle nach Typ und Geschlecht",
         x = "Schlaganfalltyp",
         y = "Anzahl der Patienten") +
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorrhagisch", "Ischaemic Stroke" = "Ischämisch", "Unknown Stroke" = "Unbestimmt")) +
    scale_fill_manual(values = c("#472d7b", "#218f8d", "#90d743")) +
    scale_y_continuous(
      breaks = seq(0, 225, by = 25),
      labels = seq(0, 225, by = 25), 
      expand = expansion(mult = c(0, 0.05))
    ) +
    facet_wrap(~ SEX, labeller = as_labeller(c("M" = "Männlich", "F" = "Weiblich"))) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 15), 
      strip.text = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}