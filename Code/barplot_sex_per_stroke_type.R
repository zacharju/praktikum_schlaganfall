barplot_sex_per_stroke_type <- function(data = cleaned) {

  filtered <- data |>
    filter(!is.na(stroke.type))

  ggplot(filtered, aes(x = stroke.type, fill = SEX)) +
    geom_bar(position = "fill", width = 0.7, alpha = 0.85) +
    labs(
      title = "Schlaganfalltyp unterteilt nach Geschlecht",
      x = "Schlaganfalltyp", y = "Anteil", fill = "Geschlecht"
    ) +
    coord_flip() +
    scale_y_continuous(
      labels = scales::percent,
      expand = expansion(mult = c(0, 0.05))
    ) +
    geom_text(stat = "count",
              aes(label = sprintf("%.1f%%", after_stat(count) /
                                    tapply(after_stat(count),
                                           after_stat(x), sum)[after_stat(x)]
                                  * 100)
              ),
              position = position_fill(vjust = 0.5),
              size = 4.2) +
    scale_x_discrete(labels = c(
      "Haemorrhagic Stroke" = "Hämorrhagisch",
      "Ischaemic Stroke" = "Ischämisch",
      "Indeterminate Stroke" = "Indeterminate"
    )) +
    scale_fill_manual(
      values = c("M" = "#1054E7", "F" = "#E71010"),
      labels = c("M" = "Männlich", "F" = "Weiblich")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 19, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 15),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 14),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.text = element_text(size = 13),
      legend.title = element_text(size = 14)
    )

}
