# Libraries import
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycustomloader)
library(DT)
library(RColorBrewer)
library(data.table)
library(tidyverse)
library(logging)

basicConfig()

library(reactlog)
# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)
# import modules and functions from another R files
source(file.path("modules", "data_import.r"), local = TRUE)
source(file.path("modules","plate_spec.r"), local = TRUE)
source(file.path("modules","backtracking.r"), local = TRUE)
source(file.path("../../R", "fonctions.r"), local = TRUE)
source(file.path("../../R", "plotPlateMaps.r"), local = TRUE)


