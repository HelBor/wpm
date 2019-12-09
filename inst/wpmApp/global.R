# Libraries import
library(shinydashboard)
library(DT)
library(RColorBrewer)
library(dplyr)
library(data.table)
library(ggplot2)


# import modules and functions from another R files
source(file.path("modules", "data_import.r"), local = TRUE)
source(file.path("modules","plate_spec.r"), local = TRUE)
source("fonctions.r")
source("plotPlateMaps.r")