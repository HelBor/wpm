# Libraries import
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycustomloader)
library(DT)
library(RColorBrewer)
library(data.table)

#library(dplyr)
#library(ggplot2)
#library(readr)
#library(stringr)
library(tidyverse) # meta package containing the packages above
# library(extrafont)
library(logging)

file.remove("./testing.log")

basicConfig()
addHandler(writeToFile, file = "./testing.log")
loginfo('Initiating the log file...')


library(reactlog)
# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)
# import modules and functions from another R files
source(file.path("modules", "data_import.r"), local = TRUE)
source(file.path("modules","plate_spec.r"), local = TRUE)
source(file.path("modules","backtracking.r"), local = TRUE)
source(file.path("modules","data_export.r"), local = TRUE)
source(file.path("../../R", "fonctions.r"), local = TRUE)
source(file.path("../../R", "plotPlateMaps.r"), local = TRUE)

# import the font for ggplots
#font_import(paths = "./www/fonts/")
# windowsFonts("MyriadSetPro-Ultralight")
# windowsFonts("Amatic-Bold")
# loadfonts(device="win")
# loadfonts(device="postscript")
