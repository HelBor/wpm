##' data_import module UI Function
##'
##' @description A shiny Module.for data import
##' @param id Internal parameters for {shiny}.
##' @noRd
mod_data_import_file_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
    # shiny::fluidRow(
    #     shiny::textOutput(ns("text"))
    # )
    shiny::tagList(
        # Input File section
        shiny::fluidRow(
            shiny::fluidRow(
                shiny::column(
                    width = 10,
                    shiny::h4("Please use txt or CSV format.")),
                shiny::column(
                    width = 2,
                    align = "right",
                    shinyWidgets::dropdownButton(
                        shiny::h4("File format"),
                        shiny::div("File must be in CSV (comma/semi-colon separator), TSV (tabulation separator) or Text format.
                            It must contain at least the field samples names.
                            If samples pertain to different groups, you can specify
                            a second column containing the group names.
                            See the Help tab for more details."),
                        icon = shiny::icon("info-circle"),
                        tooltip = shinyWidgets::tooltipOptions(
                            title = "Help"),
                        status = "warning",
                        size = "sm",
                        width = "350px"
                    ))
            ),
            shiny::fileInput(ns("file"),
                             label = NULL,
                             accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
            ),
            #***********************************************************
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
                c("None" = "none", "Single quote" = "single", "Double quote" = "double"),
                selected = "None"
            ),
            
            
            shiny::conditionalPanel(
                condition = "output.panel",
                #***********************************************************
                shiny::hr(),
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
                #***********************************************************
                shiny::fluidRow(
                    shiny::column(
                        width = 5,
                        shiny::h4("Does have the dataset row names?")
                    ),
                    shiny::column(
                        width = 5,
                        shinyWidgets::switchInput(
                            inputId = ns("rnames"),
                            label = NULL,
                            value = FALSE,
                            onLabel = "Yes",
                            offLabel = "No",
                            onStatus = "success",
                            offStatus = "danger"
                        )
                    )
                ),
                #***********************************************************
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
                
                
                #***********************************************************
                shiny::hr(),
                shiny::h4("Please select the grouping variable"),
                shinyWidgets::pickerInput(
                    inputId = ns("group_picker"),
                    choices = NULL,
                    selected = NULL
                ),
                ns = ns
            ) # end of conditionalPanel
        ) # end of fluidRow
    ) # end of tagList
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

            ## The selected file, if any
            userFile <- shiny::reactive({
                ## If no file is selected, don't do anything
                shiny::validate(
                    shiny::need(input$file, message = FALSE)
                )
                input$file
            })
            
            q_input <- shiny::reactive({
                if(input$quote == "none"){
                    q <- ""
                }else if(input$quote == "single"){
                    q <- "'"
                }else{
                    q <- "\""
                }
                return(q)
            })
            
            # this part updates the picker input when the user gives a file and modifies
            #  some other parameters. This picker input allows the user to specify the
            #  grouping variable
            shiny::observeEvent({
                input$file
                input$heading
                input$quote
                input$sep_input
            }, {
                df <- utils::read.csv2(userFile()$datapath,
                                       header = input$heading, quote = q_input(),
                                       sep = input$sep_input, stringsAsFactors = FALSE,
                                       nrows = 1)
                shinyWidgets::updatePickerInput(session = session, "group_picker",
                                                choices = c("none",colnames(df)))
            })
            
            ## The user's data, reshaped into a valid data frame for WPM
            dataframe <- shiny::reactive({
                df <- tryCatch(
                    {
                        message("Trying to read the file with the specified parameters...")
                        utils::read.csv2(
                            userFile()$datapath, header = input$heading,
                            quote = q_input(), sep = input$sep_input,
                            stringsAsFactors = FALSE, nrows = 1)
                    },
                    error=function(cond) {
                        message(cond)
                        return(NULL)
                    },
                    warning=function(cond) {
                        message(cond)
                        return(NULL)
                    },
                    finally={
                        message(paste("Processed file:", input$file$name))
                    }
                )
                shiny::validate(
                    shiny::need(input$group_picker %in% c("none",colnames(df)),
                                "The picker provided is not a valid column name.")
                )
                logging::loginfo("input$group_picker = %s", input$group_picker)
                logging::loginfo("class de df: %s",class(df))
                if(!is.null(df)){
                    df <- convertCSV(
                        userFile()$datapath, row_names = input$rnames,
                        gp_field = input$group_picker, header = input$heading,
                        quote = q_input(), sep = input$sep_input,
                        stringsAsFactors = FALSE)
                }
                logging::loginfo("class de df: %s", class(df))
                return(df)
            })
            
            # for the conditionalPanel that allows to tune upload parameters only if
            # file is correctly imported
            output$panel <- shiny::reactive({
                logging::loginfo("class de dataframe(): %s", class(dataframe()))
                !is.null(dataframe()$df_csv)
            })
            shiny::outputOptions(output, "panel", suspendWhenHidden = FALSE)
            
            
            
            #             
            # output$text <- shiny::renderText({"Here we will see the fields useful for user file upload"})

            shiny::observe({
                toReturn$df <- dataframe()$df_csv
                toReturn$df_wpm <- dataframe()$df_wpm
            })
            return(toReturn)
            
        }
    )
}