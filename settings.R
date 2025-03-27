# This is where all global variables and file paths are defined and the packages
# are loaded

require("tidyverse")
require("scales")
require("rnaturalearth")
require("rnaturalearthdata")
require("sf")
require("viridis")
require("ggokabeito")
require("scico")
require("forcats")

data_path <- "./Daten/Rohdaten/data.CSV"

utils::globalVariables(c(
  "DDIAGISC", "DDIAGHA", "DDIAGUN", "DNOSTRK",
  "DRSISC", "DRSH", "DRSUNK", "DRSISCD", "DRSHD", "DRSUNKD",
  "DASP14", "DLH14", "DMH14", "DSCH", "DIVH", "AGE", "SEX",
  "value", "stroke.type", "..x..", "..count..", "FDEAD",
  "COUNTRY", "total_patients", "deaths", "total_cases",
  "cfr_rate", "COUNTRY_GERMAN", "OCCODE", "treatment",
  "prop", "recurring.stroke.type", "recurring", "total",
  "second_stroke", "proportion", "FDEADD", "treatment_count",
  "FDEADD_count", "last_day", "last_survival_rate",
  "FDEADD_count_cumsum", "FDEADC"
))
