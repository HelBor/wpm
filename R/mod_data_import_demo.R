
##'
##' @description A shiny Module.for data import
##' @param id Internal parameters for {shiny}.
##' @noRd
mod_data_import_demo_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
    shiny::fluidRow(
        
        shiny::h4("Please select the grouping variable"),
        shinyWidgets::pickerInput(
            inputId = ns("group_picker"),
            choices = NULL,
            selected = NULL
        ),
        shiny::textOutput(ns("text"))
    )
}

##' data_import module Server Function
##'
##' @description server part of the data import module. Allows to use the 
##' dataset from the wpm package.
##' @param input,output,session Internal shiny parameters
##' @return dataframe containing the data to place on the plates plan(s).
##' @noRd
mod_data_import_demo_server <- function(id){
    shiny::moduleServer(
        id,
        function(input, output, session) {
            ns <- session$ns
            toReturn <- shiny::reactiveValues(
                df = NULL
            )
            choices <- utils::data(package = "wpm", envir = environment())$results[, "Item"]
            utils::data(list = choices)
            df <- get(choices)
            
            shiny::observeEvent(!is.null(df), {
                
                shinyWidgets::updatePickerInput(session = session, inputId = "group_picker",
                                  choices = c("none",colnames(df)))
            })
            
            output$text <- shiny::renderText({
                "Here we will see the fields useful for demo dataset"
            })
            
            shiny::observe({
                logging::loginfo("class df: %s", class(df))
                #toReturn$df <- "demo dataframe"
                toReturn$df <- df
            })
            
            return(toReturn)
    
    })
    
}

