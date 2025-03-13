survival_rate <- function(data = cleaned) {
  data |>
    filter(FDEADC == 2 | FDEADC == 3) |> 
    filter(!is.na(FDEADD)) |> 
    filter(stroke.type != "Not a Stroke") |>  
    filter(!is.na(treatment)) |> 
    group_by(treatment) |>  
    mutate(
      FDEADD_count = ave(FDEADD, FDEADD, FUN = length)  # Zähle, wie oft jeder FDEADD-Wert vorkommt
    ) |> 
    mutate(treatment_count = n()) |>  
    select(treatment, FDEADD, FDEADD_count, treatment_count) |> 
    arrange(treatment, FDEADD) |>
    filter(!duplicated(FDEADD)) |>  
    mutate(
      FDEADD_count_cumsum = cumsum(FDEADD_count),  # Kumulierte Summe von FDEADD_count
      survival_rate = 1 - FDEADD_count_cumsum / treatment_count 
    ) |> 
    ungroup() |>   
  ggplot(aes(x = FDEADD, y = survival_rate, colour = treatment)) +
    geom_line(size = 1) +
    labs(x = "Tage seit Randomisierung", y = "Überlebensrate") +
    scale_y_continuous(labels = scales :: percent, expand = expansion(mult = c(0, 0.05))) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 15), 
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 14),
    ) +
    scale_color_viridis_d(option = "plasma")
}