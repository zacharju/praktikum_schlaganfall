source("settings.R")
lapply(list.files(
  "Code",
  pattern = "\\.R$",
  ignore.case = TRUE,
  full.names = TRUE,
  recursive = TRUE
) , function(f) source(f, encoding = "UTF-8")
)
