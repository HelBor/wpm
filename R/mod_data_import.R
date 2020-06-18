##' data_import UI Function
##'
##' @description A shiny Module.
##' @param id,input,output,session Internal parameters for {shiny}.
##' @noRd
mod_data_import_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
    shiny::tagList(
        # Input File section
        shiny::fluidRow(
            shiny::column(
                width = 6,
                shinydashboard::box(
                    status = "warning",
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = TRUE,
                    title = shiny::h3("Upload dataset"),
                    #***********************************************************
                    shiny::fluidRow(
                        shiny::column(
                            width = 10,
                            shiny::h4("Please use CSV format.")),
                        shiny::column(
                            width = 2,
                            align = "right",
                            shinyWidgets::dropdownButton(
                                shiny::h4("File format"),
                                shiny::div("File must be in CSV format.
                                    It must contain at least the field samples names.
                                    If samples pertain to different groups, you can specify
                                    a second column containing the group names.
                                    See the vignette for more details."),
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
                        c("None" = "None", "Single quote" = "'", "Double quote" = "\""),
                        selected = NULL
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
                            inputId = ns("GroupPicker"),
                            choices = NULL,
                            selected = NULL
                        ),
                        ns = ns)

                ) # end of box upload dataset
            ), # end column 1
            shiny::column(
                width = 6,
                shiny::fluidRow(
                    shiny::column(
                        width = 6,
                        shinydashboard::box(
                            title = shiny::h3("Check that your file is correctly
                                          read by WPM"),
                            solidHeader = TRUE, collapsible = TRUE,
                            width = 12, status = "warning",
                            DT::dataTableOutput(ns("csv_table"))
                        ),
                    ),
                    shiny::column(
                        width= 6,
                        shinydashboard::box(
                            title = shiny::h3("Preview output template"),
                            solidHeader = TRUE, collapsible = TRUE,
                            width = 12, status = "warning",
                            DT::dataTableOutput(ns("wpm_table"))
                        ),
                        shinydashboard::valueBoxOutput(ns("nb_ech"), width = 6),
                        shinydashboard::valueBoxOutput(ns("nb_gp"), width = 6)
                    )
                )
            ) # end column 2
        )
    )
}

##' data_import Server Function
##'
##' @description server part of the data import module. Allows to browse a file
##' in CSV, text or TSV format, create a dataframe and set the column names.
##' @param input,output,session Internal shiny parameters
##' @return dataframe containing the data to place on the plates plan(s).
##' @noRd
mod_data_import_server <- function(input, output, session){

    toReturn <- shiny::reactiveValues(
        df = NULL,
        distinct_gps = NULL,
        gp_levels = NULL,
        nb_samples = 0
    )

    ## The selected file, if any
    userFile <- shiny::reactive({
        ## If no file is selected, don't do anything
        shiny::validate(
            shiny::need(input$file, message = FALSE)
        )
        input$file
    })

    # this part updates the picker input when the user gives a file and modifies
    #  some other parameters
    shiny::observeEvent({
        input$file
        input$heading
        input$quote
        input$sep_input
        }, {
        df <- utils::read.csv2(userFile()$datapath,
                               header = input$heading, quote = input$quote,
                               sep = input$sep_input, stringsAsFactors = FALSE,
                               nrows = 1)
        shinyWidgets::updatePickerInput(session = session, "GroupPicker",
                                        choices = c("none",colnames(df)))
    })
    ## The user's data, reshaped into a valid data frame for WPM
    dataframe <- shiny::reactive({
        df <- tryCatch(
            {
                message("Trying to read the CSV file with the specified parameters...")
                utils::read.csv2(
                    userFile()$datapath, header = input$heading,
                    quote = input$quote, sep = input$sep_input,
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
            shiny::need(input$GroupPicker %in% c("none",colnames(df)),
                        "The picker provided is not a valid column name.")
        )
        logging::loginfo("input$GroupPicker = %s", input$GroupPicker)
        if(!is.null(df)){
            df <- convertCSV(
                userFile()$datapath, row_names = input$rnames,
                gp_field = input$GroupPicker, header = input$heading,
                quote = input$quote, sep = input$sep_input,
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

    output$csv_table <- DT::renderDataTable(DT::datatable({
        shiny::validate(
            shiny::need(!is.null(dataframe()$df_csv), "Wrong set of parameters, we can
            not read your file... Please correct those that have been misinformed")
        )
        dataframe()$df_csv
    }, rownames = FALSE)
    )

    output$wpm_table <- DT::renderDataTable(DT::datatable({
        shiny::validate(
            shiny::need(!is.null(dataframe()$df_wpm), "Wrong set of parameters, we can
            not read your file... Please correct those that have been misinformed")
        )
        dataframe()$df_wpm
    }, rownames = FALSE)
    )



    output$nb_ech <- shinydashboard::renderValueBox({
        if (is.null(dataframe()$df_wpm)) {
            shinydashboard::valueBox(
                value = 0 ,
                subtitle = "Total number of samples to place",
                color = "teal")
        }else{
            shinydashboard::valueBox(
                value = nrow(dataframe()$df_wpm) ,
                subtitle = "Total number of samples to place",
                icon = shiny::icon("list"),
                color = "teal")
        }
    })
    ## Vector containing the different group names
    gp_levels <- shiny::reactive({
        nb <- NULL
        if (is.null(dataframe()$df_wpm)) {
            nb <- 0
        }else if ("Group" %in% colnames(dataframe()$df_wpm)) {
            nb <- unique(dataframe()$df_wpm$Group)
        }
        return(nb)
    })

    ## The number of distinct groups in the file
    distinct_gps <- shiny::reactive({
        d_gp <- NULL
        if (is.null(dataframe()$df_wpm)) {
            d_gp <- 0
        }else if ("Group" %in% colnames(dataframe()$df_wpm)) {
            d_gp <- length(unique(dataframe()$df_wpm$Group))
        }
        shiny::validate(
            shiny::need(d_gp <= 12,
                        message = "The number of separate groups must not
                        exceed 12.")
        )

        return(d_gp)
    })
    # the number of samples in the dataset
    nb_s <- shiny::reactive({
        if (is.null(dataframe()$df_wpm)) {
            nb <- 0
        }else{
            nb <- nrow(dataframe()$df_wpm)
        }
        return(nb)
    })

    output$nb_gp <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = distinct_gps(),
            subtitle = "Total number of distinct groups",
            icon = shiny::icon("layer-group"),
            color = "teal")
    })
    shiny::outputOptions(output, "nb_gp", suspendWhenHidden = FALSE)

    shiny::observe({
        toReturn$df <- dataframe()$df_wpm
        toReturn$distinct_gps <- distinct_gps()
        toReturn$gp_levels <- gp_levels()
        toReturn$nb_samples <- nb_s()
    })
    ## Return the reactive that yields the data frame
    return(toReturn)
}

## To be copied in the UI
# mod_data_import_ui("data_import_ui_1")

## To be copied in the server
# callModule(mod_data_import_server, "data_import_ui_1")

