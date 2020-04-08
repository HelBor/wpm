# # Libraries import
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycustomloader)
library(ggplot2)



logging::basicConfig()
print(paste0("working directory", getwd()))


#
# # import modules
source(file.path(".", "modules/data_import.R"), local = TRUE)$value
source(file.path(".","modules/plate_spec.R"), local = TRUE)$value
source(file.path(".","modules/backtracking.R"), local = TRUE)$value
source(file.path(".", "modules/constraints.R"), local = TRUE)$value
source(file.path(".", "modules/resample.R"), local = TRUE)$value
source(file.path(".", "modules/solve_cell.R"), local = TRUE)$value
source(file.path(".", "modules/random_walk.R"), local = TRUE)$value
source(file.path(".", "modules/generate_map.R"), local = TRUE)$value
source(file.path(".", "modules/balanced_distribution.R"), local = TRUE)$value
source(file.path(".", "modules/blank_coord.R"), local = TRUE)$value
source(file.path(".", "modules/conversion.R"), local = TRUE)$value
source(file.path(".", "modules/draw_map.R"), local = TRUE)$value