plot_outcome_by_medication <- function(data) {
  library(viridis)
  library(ggmosaic)
  library(ggokabeito)
  library(forcats)
  
  therapy <- data |>
    mutate(heparin = ifelse(DLH14 == "Y" | DMH14 == "Y" | DHH14 == "Y", "Y", "N")) |>
    mutate(two_week_therapy = case_when(
      DASP14 == "Y" & heparin == "N" ~ "Aspirin",
      DASP14 == "N" & heparin == "Y" ~ "Heparin",
      DASP14 == "Y" & heparin == "Y" ~ "Beides",
      TRUE ~ "Keine Medikation"
    )) |>
    mutate(OCCODE = ifelse(OCCODE == 0 | OCCODE == 9, NA, OCCODE)) |>
    mutate(OCCODE = recode(OCCODE,
                           `1` = "Tot",
                           `2` = "Abhängig",
                           `3` = "Nicht genesen",
                           `4` = "Genesen"))
  
  therapy_summary <- therapy |>
    filter(!is.na(OCCODE) & !is.na(two_week_therapy)) |>
    group_by(two_week_therapy, OCCODE) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(two_week_therapy) |>
    mutate(prop = n / sum(n))
  
  ggplot(therapy_summary, aes(x = reorder(two_week_therapy, -prop), y = prop, fill = OCCODE)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Zustand der Patienten nach 6 Monaten je nach Therapie", x = "Medikation",
         y = "Anteil", fill = "Ergebnis") +
    theme_minimal() +
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