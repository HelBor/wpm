#' data_import UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @keywords internal
#' 
mod_data_import_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fileInput(ns("file"),
                         label = NULL,
                         accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
                         ),
        shiny::fluidRow(
            shiny::column(
                width = 5,
                shiny::h4("Does have the dataset a Header?")
            ),
            shiny::column(
                width = 5,
                shinyWidgets::switchInput(
                    inputId = ns("heading"),
                    label = NULL,
                    value = FALSE,
                    onLabel = "Yes",
                    offLabel = "No",
                    onStatus = "success",
                    offStatus = "danger"
                )
            )
        ),
        
        shiny::hr(),
        ## Input: Select quotes
        shiny::fluidRow(
            shiny::column(
                width = 10,
                shiny::h4("Please select the appropriate quote")
            ),
            shiny::column(
                width = 2,
                align = "right",
                shinyWidgets::dropdownButton(
                    shiny::h4("What are quotes?"),
                    shiny::div("Character strings in a file can be quoted, meaning they 
                    are surrounded by quotes (Eg: \"string\" or \'string\') ",
                    shiny::br(), "If you don't see your data on the right side (number of
                    samples to zero), you need to change the quote option"),
                    icon = shiny::icon("info-circle"),
                    tooltip = shinyWidgets::tooltipOptions(title = "Help"),
                    status = "warning",
                    size = "sm",
                    width = "350px"
                )
            )
        ),
        shiny::selectInput(
            ns("quote"),
            label = NULL,
            c("None" = "None", "Single quote" = "'", "Double quote" = "\""),
            selected = NULL
        ),
        shiny::hr(),
        ## Input: Select separator
        shiny::h4("Please select the appropriate separator"),
        shinyWidgets::awesomeRadio(
            inputId = ns("sep_input"),
            label = NULL,
            choices = c("Semicolon" = ";", "Comma" = ",", "Tab" = "\t"),
            selected = ";",
            status = "warning"
        ),
        shiny::hr(),
        shiny::h4("Please choose a Project name"),
        shiny::textInput(inputId = "project_title",
                         label = NULL,
                         value = "",
                         placeholder = "my project title")
        
    )
}

##' data_import Server Function
##'
##' @description server part of the data import module. Allows to browse a file
##' in CSV, text or TSV format, create a dataframe and set the column names.
##' @param input,output,session Internal shiny parameters
##' @param stringsAsFactors Boolean, argument passed to the function `read.csv2`
##' @return dataframe containing the data to place on the plates plan(s).
mod_data_import_server <- function(input, output, session, stringsAsFactors){
    
    ## The selected file, if any
    userFile <- shiny::reactive({
        ## If no file is selected, don't do anything
        shiny::validate(
            shiny::need(input$file, message = FALSE)
        )
        input$file
    })
    shiny::observe({
        logging::loginfo("File %s was uploaded",
                         userFile()$name,
                         logger = "data_import"
        )
    })
    
    ## The user's data, parsed into a data frame
    dataframe <- shiny::reactive({
        df <- utils::read.csv2(userFile()$datapath,
                               header = input$heading,
                               quote = input$quote,
                               sep = input$sep_input,
                               stringsAsFactors = stringsAsFactors
        )
        
        ## check if file contains groups or not
        if (length(colnames(df)) == 1) {
            colnames(df) <- "Sample"
            df$Group <- as.factor(1)
        }else{
            colnames(df) <- c("Sample", "Group")
        }
        
        df$Sample <- as.character(df$Sample)
        df$ID <- seq_len(nrow(df))
        
        return(df)
    })
    ## Return the reactive that yields the data frame
    return(dataframe)
}

## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")

## To be copied in the server
# callModule(mod_data_import_server, "data_import_ui_1")

