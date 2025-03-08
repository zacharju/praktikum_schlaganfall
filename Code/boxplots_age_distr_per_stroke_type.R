boxplots_age_distr_per_stroke_type <- function(data = cleaned) {
  ggplot(data, aes(x = stroke.type, y = AGE, fill = stroke.type)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Alter nach Schlaganfalltyp", x = "Schlaganfalltyp", y = "Alter") +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip() 
}