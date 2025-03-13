treatment_efficacy <- function(data = cleaned) {
  data |> 
    filter(stroke.type != "Not a Stroke") |> 
    filter(!is.na(treatment)) |> 
    group_by(treatment) |> 
    summarise(
      total = n(),
      second_stroke = sum(recurring == "Yes", na.rm = TRUE),
      proportion = second_stroke / total
    ) |>  
    ggplot(aes(x = treatment, y = proportion, fill = treatment)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%.1f%%", proportion * 100)), 
              vjust = -0.5, size = 4.3) +
    labs(
      title = "Relative Häufigkeit eines zweiten Schlaganfalls nach Therapie",
      x = "Medikation",
      y = "Anteil der Patienten mit erneutem Schlaganfall"
    ) +
    scale_x_discrete(labels = c("No Treatment" = "Keine Medikation")) +
    scale_y_continuous(labels = scales :: percent, expand = expansion(mult = c(0, 0.05))) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size =15), 
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      strip.text = element_text(size = 14)
      ) +
    scale_fill_manual(values = c("#6c00a8", "#c7427c", "#f78212", "#fccd25"))
}