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
    ungroup()  
  
  
  ggplot(d, aes(x = FDEADD, y = survival_rate, colour = treatment)) +
    geom_line(size = 1) +
    labs(x = "Tage seit Randomisierung", y = "Überlebesrate")
}