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
    geom_col() +
    labs(
      title = "Relative Häufigkeit eines zweiten Schlaganfalls nach Behandlung",
      x = "Behandlung",
      y = "Proportion der Patienten mit erneutem Schlaganfall"
    ) +
    theme_minimal() +
    scale_y_continuous(labels = percent) +
    theme(legend.position = "none")
}