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
  "DASP14", "DLH14", "DMH14", "DSCH", "DIVH"
))
