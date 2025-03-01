
load_data <- function() {
  data <- read.csv(data_path)
  data <- as_tibble(data)
  return(data)
}
    