# Libraries import
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(dplyr)
library(data.table)
library(ggplot2)
library(extrafont)

# import modules and functions from another R files
source(file.path("modules", "data_import.r"), local = TRUE)
source(file.path("modules","plate_spec.r"), local = TRUE)
# source(file.path("modules","backtracking.r"), local = TRUE)
source(file.path("functions", "fonctions.r"), local = TRUE)
source(file.path("functions", "plotPlateMaps.r"), local = TRUE)

# import the font for ggplots
#font_import(paths = "./www/fonts/")
# windowsFonts("MyriadSetPro-Ultralight")
# windowsFonts("Amatic-Bold")
# loadfonts(device="win")
# loadfonts(device="postscript")
