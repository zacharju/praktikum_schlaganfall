survival_rate_with_survivors <- function(data = cleaned) {

  survival_data <- data |>
    filter(!is.na(treatment)) |>
    filter(!is.na(stroke.type)) |>
    group_by(treatment) |>
    mutate(treatment_count = n()) |>
    filter(!is.na(FDEADD)) |>
    mutate(FDEADD_count = ave(FDEADD, FDEADD, FUN = length)) |>
    select(treatment, FDEADD, FDEADD_count, treatment_count) |>
    arrange(treatment, FDEADD) |>
    filter(!duplicated(FDEADD)) |>
    mutate(
      FDEADD_count_cumsum = cumsum(FDEADD_count),
      survival_rate = 1 - FDEADD_count_cumsum / treatment_count
    ) |>
    ungroup()

  # Determine maximum observation time
  max_time <- max(survival_data$FDEADD, na.rm = TRUE)

  # Ensure that each treatment line goes all the way to the end
  survival_data_complete <- survival_data |>
    group_by(treatment) |>
    summarize(
      last_survival_rate = last(survival_rate),
      last_day = last(FDEADD)
    ) |>
    # If a treatment has already reached the end, ignore it
    filter(last_day < max_time) |>
    mutate(FDEADD = max_time) |>
    select(treatment, FDEADD, survival_rate = last_survival_rate) |>
    bind_rows(survival_data) |>
    # Ensure that everything is sorted correctly
    arrange(treatment, FDEADD)

  ggplot(survival_data_complete, aes(
    x = FDEADD, y = survival_rate,
    color = treatment, linetype = treatment
  )) +
    geom_line(linewidth = 0.7) +
    labs(
      x = "Tage seit Randomisierung",
      y = "Überlebensrate",
      title = "Überlebensrate nach Erstschlaganfall: Einfluss der Therapie",
      color = "Medikation", linetype = "Medikation"
    ) +
    scale_y_continuous(labels = scales::percent,
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_color_manual(
      values = rep("steelblue", length(unique(survival_data_complete$treatment))
      ),
      labels = c("No Treatment" = "Keine Medikation")
    ) +
    scale_linetype_manual(
      values = c("dotdash", "dashed", "dotted", "solid"),
      labels = c("No Treatment" = "Keine Medikation")
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 14)
    )

}
