plot_recurrence_vs_treatment <- function(data = cleaned) {
  
  filtered <- data |> 
    filter(!is.na(stroke.type)) |>
    filter(!is.na(recurring.stroke.type)) |> 
    filter(!is.na(treatment))

    ggplot(filtered, aes(x = treatment, fill = recurring.stroke.type)) +
    geom_bar(position = "dodge", width = 0.8, alpha = 0.85) +
    #geom_text(stat = "count", 
              #aes(label = sprintf("%d (%.1f%%)", ..count.., ..count../tapply(..count.., ..x.., sum)[..x..] * 100)), 
              #position = position_dodge(width = 0.9),  
              #vjust = -0.5, 
              #size = 3) +  
    labs(
      title = "Typen von wiederkehrenden Schlaganfällen je nach Therapie",
      x = "Medikation",
      y = "Anzahl der Patienten",
      fill = "Schlaganfalltyp"
    ) +
      scale_fill_manual(
        values = c("#472d7b", "#218f8d", "#90d743"),
        labels = c("Haemorrhagic Stroke" = "Hämorrhagisch", 
                   "Ischaemic Stroke" = "Ischämisch", 
                   "Unknown Stroke" = "Unbestimmt")
      ) +
    scale_x_discrete(labels = c("No Treatment" = "Keine Medikation")) +
    scale_y_continuous(
        breaks = seq(0, 175, by = 25),  
        labels = ifelse(seq(0, 175, by = 25) %% 25 == 0, seq(0, 175, by = 25), ""),
        expand = expansion(mult = c(0, 0.05))  
      ) +
    theme_minimal() + 
    theme(
      #legend.position = c(0.9, 0.83),
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 15), 
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major.x = element_blank(),  
      panel.grid.minor.x = element_blank(), 
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)
    )
}