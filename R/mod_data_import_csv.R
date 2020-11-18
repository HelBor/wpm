# module d'importation d'un fichier CSV ou TXT"
mod_data_import_csv_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
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
                ns = ns)
            )
    )
}




mod_data_import_csv_server <- function(input, output, session, userFile){

    print(userFile)
    toReturn <- shiny::reactiveValues(
        df_user = NULL,
        df_wpm = NULL
    )

    q_input <- shiny::reactive({

        if(input$quote == "none"){
            print("on est dans le q_input == none")
            q <- ""
        }else if(input$quote == "single"){
            print("on est dans le q_input == single")
            q <- "'"
        }else{
            print("on est dans le q_input == double")
            q <- "\""
        }
        return(q)
    })

    # this part updates the picker input when the user gives a file and modifies
    #  some other parameters. This picker input allows the user to specify the
    #  grouping variable
    shiny::observeEvent({
        input$heading
        input$quote
        input$sep_input
    }, {
        print("on est dans observeEvent de import_csv")
        df <- utils::read.csv2(userFile$datapath,
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
                    userFile$datapath, header = input$heading,
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
                message(paste("Processed file:", userFile$name))
            }
        )
        shiny::validate(
            shiny::need(input$group_picker %in% c("none",colnames(df)),
                        "The picker provided is not a valid column name.")
        )
        logging::loginfo("input$group_picker = %s", input$group_picker)
        if(!is.null(df)){
            df <- convertCSV(
                userFile$datapath, row_names = input$rnames,
                gp_field = input$group_picker, header = input$heading,
                quote = q_input(), sep = input$sep_input,
                stringsAsFactors = FALSE)
        }
        return(df)
    })

    # for the conditionalPanel that allows to tune upload parameters only if
    # file is correctly imported
    output$panel <- shiny::reactive({
        !is.null(dataframe()$df_csv)
    })
    shiny::outputOptions(output, "panel", suspendWhenHidden = FALSE)

    shiny::observe({
        toReturn$df_user <- dataframe()$df_csv
        toReturn$df_wpm <- dataframe()$df_wpm
    })
    return(toReturn)
}
