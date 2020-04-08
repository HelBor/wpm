# # Libraries import
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycustomloader)
library(ggplot2)
# library(logging)
#
#
logging::basicConfig()
#
# # import modules
source(file.path("modules", "data_import.R"), local = TRUE)
source(file.path("modules","plate_spec.R"), local = TRUE)
source(file.path("modules","backtracking.R"), local = TRUE)
source(file.path("modules", "constraints.R"), local = TRUE)
source(file.path("modules", "resample.R"), local = TRUE)
source(file.path("modules", "solve_cell.R"), local = TRUE)
source(file.path("modules", "random_walk.R"), local = TRUE)
source(file.path("modules", "generate_map.R"), local = TRUE)
source(file.path("modules", "balanced_distribution.R"), local = TRUE)
source(file.path("modules", "blank_coord.R"), local = TRUE)
source(file.path("modules", "conversion.R"), local = TRUE)
source(file.path("modules", "draw_map.R"), local = TRUE)