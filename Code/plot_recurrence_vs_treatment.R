plot_recurrence_vs_treatment <- function(data) {
  
  filtered <- data |> 
    filter(!is.na(recurring.stroke.type)) |> 
    filter(!is.na(treatment))

    ggplot(filtered, aes(x = treatment, fill = recurring.stroke.type)) +
    geom_bar(position = "dodge") +
    geom_text(stat = "count", 
              aes(label = sprintf("%d (%.1f%%)", ..count.., ..count../sum(..count..)*100)), 
              position = position_dodge(width = 0.9),  
              vjust = -0.5, 
              size = 3) +  
    labs(
      title = "Typen von wiederkehrenden Schlaganfällen je nach Behandlung",
      x = "Behandlung",
      y = "Anzahl der Patienten",
      fill = "Schlaganfalltyp"
    ) +
    scale_fill_brewer(palette = "Pastel1", labels = c("Haemorrhagic Stroke" = "Hämorogischer Schlaganfall", "Ischaemic Stroke" = "Ischämischer Schlaganfall", "Unknown Stroke" = "Unbekannter Schlaganfall")) +  
    scale_x_discrete(labels = c("No Treatment" = "Keine Behandlung")) +
    scale_y_continuous(
        breaks = seq(0, 175, by = 25),  
        labels = ifelse(seq(0, 175, by = 25) %% 25 == 0, seq(0, 175, by = 25), "") 
      ) +
    theme_minimal() + 
    theme(
      plot.background = element_rect(colour = "black", linewidth = 1, fill = "white"), 
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      axis.title = element_text(face = "bold", size =10), 
      axis.title.x = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank())
}