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
    geom_bar(stat = "identity", position = "dodge", aes(x = fct_reorder(treatment, prop, .desc = TRUE), y = prop, fill = OCCODE))+
    labs(title = "Zustand der Patienten nach 6 Monaten je nach Therapie während des Krankenhausaufenthalts", x = "Medikation",
         y = "Anteil", fill = "Ergebnis") +
    theme_minimal()+
    scale_fill_okabe_ito() +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() + 
    theme(
      legend.title = element_text(size = 10), 
      plot.title = element_text(size = 13, hjust = 0),
      axis.title = element_text(size = 10), 
      axis.title.x = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank()
    )
}