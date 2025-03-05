clean_data <- function(data) {
  cleaned <- data |> 
    pivot_longer(cols = c(DDIAGISC, DDIAGHA, DDIAGUN, DNOSTRK),
                 names_to = "stroke.type",
                 values_to = "value") |> 
    mutate(value = na_if(value, "")) |>
    filter(value == "Y") |>
    select(-value) |>
    mutate(stroke.type = str_replace_all(stroke.type, c(
      "DDIAGISC" = "Ischaemic Stroke",
      "DDIAGHA" = "Haemorrhagic Stroke",
      "DDIAGUN" = "Indeterminate Stroke",
      "DNOSTRK" = "Not a Stroke"
    )))
  
  #Erstellen der Spalten recurring, recurring.stroke.type und date.of.recurrence
  cleaned |>
    mutate(
      recurring = ifelse(rowSums(cleaned[, c("DRSISC", "DRSH", "DRSUNK")] == "Y") > 0, "Yes", "No"),
      recurring.stroke.type = ifelse(DRSISC == "Y", "Ischaemic Stroke",
                                     ifelse(DRSH == "Y", "Haemorrhagic Stroke", 
                                            ifelse(DRSUNK == "Y", "Unknown Stroke", NA))),
      date.of.recurrence = ifelse(DRSISC == "Y", DRSISCD,
                                  ifelse(DRSH == "Y", DRSHD,
                                         ifelse(DRSUNK == "Y", DRSUNKD, NA))))
  
}