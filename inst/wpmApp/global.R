# Libraries import
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycustomloader)
library(RColorBrewer)
library(ggplot2)
library(logging)

logging::basicConfig()

# import modules
source(file.path("modules", "data_import.R"), local = TRUE)
source(file.path("modules","plate_spec.R"), local = TRUE)
source(file.path("modules","backtracking.R"), local = TRUE)

# import functions
source(file.path("../../R", "constraints.R"), local = TRUE)
source(file.path("../../R", "resample.R"), local = TRUE)
source(file.path("../../R", "solve_cell.R"), local = TRUE)
source(file.path("../../R", "random_walk.R"), local = TRUE)
source(file.path("../../R", "generate_map.R"), local = TRUE)
source(file.path("../../R", "balanced_distribution.R"), local = TRUE)
source(file.path("../../R", "blank_coord.R"), local = TRUE)
source(file.path("../../R", "conversion.R"), local = TRUE)
source(file.path("../../R", "draw_map.R"), local = TRUE)