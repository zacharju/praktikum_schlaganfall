survival_rate <- function(data = cleaned) {

  data |>
    filter(FDEADC == 2 | FDEADC == 3) |>
    filter(!is.na(FDEADD)) |>
    filter(!is.na(stroke.type)) |>
    filter(!is.na(treatment)) |>
    group_by(treatment) |>
    mutate(
      # Count how often each FDEADD value occurs
      FDEADD_count = ave(FDEADD, FDEADD, FUN = length)
    ) |>

    mutate(treatment_count = n()) |>
    select(treatment, FDEADD, FDEADD_count, treatment_count) |>
    arrange(treatment, FDEADD) |>
    filter(!duplicated(FDEADD)) |>
    mutate(
      # Cumulative sum of FDEADD_count
      FDEADD_count_cumsum = cumsum(FDEADD_count),
      survival_rate = 1 - FDEADD_count_cumsum / treatment_count
    ) |>
    ungroup() |>
    ggplot(aes(x = FDEADD, y = survival_rate, colour = treatment)) +
    geom_line(size = 1) +
    labs(x = "Tage seit Randomisierung", y = "Überlebensrate",
         title = "Überlebensrate nach Erstschlaganfall: Einfluss der Therapie")
  +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.05))) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5,
                                lineheight = 1.2),
      axis.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)
    ) +
    scale_color_manual(values = c("#6c00a8", "#c7427c", "#f78212", "#fccd25"))

}
