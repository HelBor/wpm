# # Libraries import
print(paste0("(global.R) wd-1: ", getwd()))
library(shiny)
print(paste0("(global.R) wd-2: ", getwd()))
library(shinydashboard)
print(paste0("(global.R) wd-3: ", getwd()))
library(shinyWidgets)
print(paste0("(global.R) wd-4: ", getwd()))
library(shinycustomloader)
print(paste0("(global.R) wd-5: ", getwd()))
library(ggplot2)
print(paste0("(global.R) wd-6: ", getwd()))


logging::basicConfig()
print(paste0("(global.R) wd-7: ", getwd()))
wd <- getwd()

#
# # import modules
source(file.path(wd, "modules/data_import.R"), local = TRUE)$value
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