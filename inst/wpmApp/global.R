# Libraries import
library(shinydashboard)
library(DT)
library(RColorBrewer)
library(dplyr)
library(data.table)
library(ggplot2)
library(extrafont)

# import modules and functions from another R files
source(file.path("modules", "data_import.r"), local = TRUE)
source(file.path("modules","plate_spec.r"), local = TRUE)
source(file.path("functions", "fonctions.r"), local = TRUE)
source(file.path("functions", "plotPlateMaps.r"), local = TRUE)

# import the font for ggplots
library(extrafont)
font_import(paths = "./www/fonts/")
windowsFonts(sans="MyriadSetPro-Ultralight")
windowsFonts(sans="Amatic")
loadfonts(device="win")
loadfonts(device="postscript")
