load_data <- function() {

  data <- read.csv(data_path)
  as_tibble(data)

}
