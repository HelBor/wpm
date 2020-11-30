##' data_import module UI Function
##'
##' @description A shiny Module. For the demo dataset import
##' @param id Internal parameters for {shiny}.
##' @noRd
mod_data_import_demo_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
    shiny::fluidRow(
        
        shiny::h4("Please select the grouping variable"),
        shinyWidgets::pickerInput(
            inputId = ns("group_picker"),
            choices = "none",
            selected = NULL
        )
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
                df = NULL,
                df_wpm = NULL
            )
            
            choices <- utils::data(package = "wpm", envir = environment())$results[, "Item"]
            utils::data(list = choices)
            df <- get(choices)
            
            shiny::observeEvent(!is.null(df), {
                
                shinyWidgets::updatePickerInput(session = session, inputId = "group_picker",
                                  choices = c("none",colnames(df)))
            })
            
            
            df_wpm_format <- shiny::reactive({
                logging::loginfo("input$group_picker = %s", input$group_picker)

                if(!is.null(df)){

                    if (input$group_picker == "none") {
                        out <- data.frame(Sample = df$samples, Group = as.factor(1))
                    }else{
                        # check if user enter an existing field
                        if (input$group_picker %in% colnames(df)) {
                            out <- data.frame(Sample = df$samples,
                                              Group = as.factor(df[[input$group_picker]]))
                        }
                    }
                    print(class(df))
                    out$Sample <- as.character(out$Sample)
                    out$ID <- seq_len(nrow(out))

                }
                return(out)

            })
            
            
            
            shiny::observe({
                logging::loginfo("class df: %s", class(df))
                toReturn$df <- df
                toReturn$df_wpm <- df_wpm_format()
            })
            
            return(toReturn)
    
    })
    
}

