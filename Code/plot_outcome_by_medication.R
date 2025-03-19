plot_outcome_by_medication <- function(data = cleaned) {
  therapy <- data |>
    mutate(OCCODE = ifelse(OCCODE == 0 | OCCODE == 9, NA, OCCODE)) |>
    mutate(OCCODE = recode(OCCODE,
                           `1` = "Verstorben",
                           `2` = "Pflegebedürftig",
                           `3` = "Nicht genesen aber nicht pflegebedürftig",
                           `4` = "Genesen"))
  
  therapy_summary <- therapy |> 
    filter(!is.na(OCCODE), !is.na(treatment)) |>
    count(treatment, OCCODE) |>
    group_by(treatment) |>  
    mutate(prop = n / sum(n))
  
  ggplot(therapy_summary) +
    geom_bar(stat = "identity", position = "dodge", aes(x = fct_reorder(treatment, prop, .desc = TRUE), y = prop, fill = OCCODE), width = 0.8, alpha = 0.85)+
    labs(title = "Einfluss der zweiwöchigen Therapie auf den\nPatientenzustand nach 6 Monaten", x = "Medikation",
         y = "Anteil", fill = "Ergebnis") +
    scale_fill_okabe_ito() +
    scale_x_discrete(labels = c("No Treatment" = "Keine Medikation")) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05))) +
    theme_minimal() + 
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, lineheight = 1.2),
      axis.title = element_text(face = "bold", size = 15), 
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank(), 
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 14),
      legend.position = "top"
    )
}