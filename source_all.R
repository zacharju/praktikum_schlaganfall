
source("settings.R")
lapply(list.files(
  "Code", 
  pattern = "\\.R$",
  ignore.case = TRUE,
  full.names = TRUE,
  recursive = TRUE
),
source
)
