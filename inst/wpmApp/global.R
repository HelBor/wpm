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
# tell shiny to log all reactivity
options(shiny.reactlog = TRUE)
# import modules and functions from another R files
source(file.path("modules", "data_import.R"), local = TRUE)
source(file.path("modules","plate_spec.R"), local = TRUE)
source(file.path("modules","backtracking.R"), local = TRUE)
source(file.path("modules","random.R"), local = TRUE)
source(file.path("../../R", "functions.R"), local = TRUE)
source(file.path("../../R", "plot_plate_maps.R"), local = TRUE)
