plot_recurrence_by_age_and_typ <- function(data = cleaned) {
  
  filtered <- cleaned |>
    filter(!is.na(stroke.type)) |>
    filter(!is.na(recurring.stroke.type))
  
  ggplot(filtered, aes(x = recurring.stroke.type, y = AGE, fill = recurring.stroke.type)) +
    geom_boxplot(width = 0.6, alpha = 0.85) +
    labs(title = "Alter nach Typ des wiederkehrenden Schlaganfalls",
         x = "Schlaganfalltyp",
         y = "Alter der Patienten") + 
    scale_x_discrete(labels = c("Haemorrhagic Stroke" = "Hämorrhagisch", "Ischaemic Stroke" = "Ischämisch", "Unknown Stroke" = "Unbestimmt")) +
    scale_fill_manual(values = c("#472d7b", "#218f8d", "#90d743")) +
    scale_y_continuous(
      breaks = seq(0, 100, by = 10),  
      labels = seq(0, 100, by = 10)
    ) +
    theme_minimal() + 
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 15), 
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}
