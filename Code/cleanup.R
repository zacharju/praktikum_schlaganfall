clean_data <- function(data) {

  cleaned <- data |>
    pivot_longer(
      cols = c(DDIAGISC, DDIAGHA, DDIAGUN, DNOSTRK),
      names_to = "stroke.type",
      values_to = "value"
    ) |>
    mutate(value = na_if(value, "")) |>
    filter(value == "Y") |>
    select(-value) |>
    mutate(stroke.type = str_replace_all(stroke.type, c(
      "DDIAGISC" = "Ischaemic Stroke",
      "DDIAGHA" = "Haemorrhagic Stroke",
      "DDIAGUN" = "Indeterminate Stroke",
      "DNOSTRK" = NA_character_
    )))

  cleaned <- cleaned |>
    mutate(
      recurring = ifelse(rowSums(cleaned[, c("DRSISC", "DRSH", "DRSUNK")] == "Y"
                         ) > 0, "Yes", "No"),
      recurring.stroke.type = ifelse(DRSISC == "Y", "Ischaemic Stroke",
        ifelse(DRSH == "Y", "Haemorrhagic Stroke",
          ifelse(DRSUNK == "Y", "Unknown Stroke", NA)
        )
      ),
      date.of.recurrence = ifelse(DRSISC == "Y", DRSISCD,
        ifelse(DRSH == "Y", DRSHD,
          ifelse(DRSUNK == "Y", DRSUNKD, NA)
        )
      )
    )

  cleaned <- cleaned |>
    mutate(DASP14 = toupper(DASP14)) |>
    mutate(
      treatment = case_when(
        rowSums(cleaned[, c("DASP14", "DLH14", "DMH14", "DSCH", "DIVH")] == "U")
        > 0 ~ NA_character_,
        DASP14 == "Y" & (rowSums(cleaned[, c("DLH14", "DMH14", "DSCH", "DIVH")]
                                 == "Y") > 0)
        ~ "Aspirin & Heparin",
        DASP14 == "Y" ~ "Aspirin",
        rowSums(cleaned[, c("DLH14", "DMH14", "DSCH", "DIVH")] == "Y") > 0
        ~ "Heparin",
        rowSums(cleaned[, c("DASP14", "DLH14", "DMH14", "DSCH", "DIVH")] == "N")
        == 5 ~ "No Treatment",
        TRUE ~ NA_character_
      )
    )

}
