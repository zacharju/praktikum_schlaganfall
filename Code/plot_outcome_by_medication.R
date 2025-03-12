plot_outcome_by_medication <- function(data = cleaned) {
  therapy <- data |>
    mutate(OCCODE = ifelse(OCCODE == 0 | OCCODE == 9, NA, OCCODE)) |>
    mutate(OCCODE = recode(OCCODE,
                           `1` = "Tot",
                           `2` = "Abhängig",
                           `3` = "Nicht genesen",
                           `4` = "Genesen"))
  
  therapy_summary <- therapy |> 
    filter(!is.na(OCCODE), !is.na(treatment)) |>
    count(treatment, OCCODE) |>
    group_by(treatment) |>  
    mutate(prop = n / sum(n))
  
  ggplot(therapy_summary) +
    geom_bar(stat = "identity", position = "dodge", aes(x = fct_reorder(treatment, prop, .desc = TRUE), y = prop, fill = OCCODE), width = 0.8)+
    labs(title = "Zustand der Patienten nach 6 Monaten je nach Therapie während des Krankenhausaufenthalts", x = "Medikation",
         y = "Anteil", fill = "Ergebnis") +
    theme_minimal()+
    scale_fill_okabe_ito() +
    scale_x_discrete(labels = c("No Treatment" = "Keine Medikation")) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
    theme_minimal() + 
    theme(
      legend.position = c(0.92, 0.855),
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 10),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}