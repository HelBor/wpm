##' data_import module UI Function
##'
##' @description A shiny Module.for data import
##' @param id Internal parameters for {shiny}.
##' @noRd
mod_data_import_file_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
    shiny::fluidRow(
        shiny::textOutput(ns("text"))
    )
}

##' data_import module Server Function
##'
##' @description server part of the data import module. Allows to browse a file
##' in CSV, text or TSV format, create a dataframe and set the column names.
##' @param input,output,session Internal shiny parameters
##' @return dataframe containing the data to place on the plates plan(s).
##' @noRd
mod_data_import_file_server <- function(id){
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            toReturn <- shiny::reactiveValues(
                df = NULL
            )
            
            output$text <- shiny::renderText({"Here we will see the fields useful for user file upload"})

            shiny::observe({
                toReturn$df <- "file dataframe"
            })
            return(toReturn)
            
        }
    )
}