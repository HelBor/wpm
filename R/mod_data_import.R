##' data_import UI Function
##'
##' @description A shiny Module.
##'
##' @param id,input,output,session Internal parameters for {shiny}.
##'
##' @noRd
##'
##' @keywords internal
##'
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
                    solidHeader = TRUE,
                    title = shiny::h3("1 - Upload dataset"),
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
                    shiny::fluidRow(
                        shiny::column(
                            width = 5,
                            shiny::h4("Does have the dataset a row names?")
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
                ) # end of box upload dataset
            ), # end column 1
            shiny::column(
                width = 6,
                shiny::fluidRow(
                    shinydashboard::box(title = shiny::h3("Your dataset"),
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        width = 6,
                                        status = "warning",
                                        DT::dataTableOutput(ns("table"))
                    ),
                    shinydashboard::valueBoxOutput(ns("nb_ech"), width = 3),
                    shinydashboard::valueBoxOutput(ns("nb_gp"), width = 3)
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
    shiny::observe({
        logging::loginfo("File %s was uploaded",
                        userFile()$name,
                        logger = "data_import"
        )
    })

    ## The user's data, parsed into a data frame
    dataframe <- shiny::reactive({
        csv_f <- convertCSV(dt_path = userFile()$datapath,
                            head = input$heading, qt = input$quote,
                            sep = input$sep_input,
                            row_names = input$rnames)
        return(csv_f)
    })

    output$table <- DT::renderDataTable(DT::datatable({
        if (!is.null(dataframe())) {
            if (methods::is(dataframe(), "data.frame")
                | methods::is(dataframe(), "matrix")) {
                logging::loginfo("dataframe/matrix successfully created",
                                logger = "server")
            }
            dataframe()
        }
    }, rownames = FALSE)
    )

    output$nb_ech <- shinydashboard::renderValueBox({
        if (is.null(dataframe())) {
            shinydashboard::valueBox(
                value = 0 ,
                subtitle = "Total number of samples to place",
                color = "teal")
        }else{
            shinydashboard::valueBox(
                value = nrow(dataframe()) ,
                subtitle = "Total number of samples to place",
                icon = shiny::icon("list"),
                color = "teal")
        }
    })
    ## Vector containing the different group names
    gp_levels <- shiny::reactive({
        nb <- NULL
        if (is.null(dataframe())) {
            nb <- 0
        }else if ("Group" %in% colnames(dataframe())) {
            nb <- unique(dataframe()$Group)
        }
        return(nb)
    })

    ## The number of distinct groups in the file
    distinct_gps <- shiny::reactive({
        d_gp <- NULL
        if (is.null(dataframe())) {
            d_gp <- 0
        }else if ("Group" %in% colnames(dataframe())) {
            d_gp <- length(unique(dataframe()$Group))
        }
        shiny::validate(
            shiny::need(d_gp <= 12,
                        message = "The number of separate groups must not
                        exceed 12.")
        )

        return(d_gp)
    })

    nb_s <- shiny::reactive({
        if (is.null(dataframe())) {
            nb <- 0
        }else{
            nb <- nrow(dataframe())
        }
        return(nb)
    })

    output$nb_gp <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = distinct_gps() ,
            subtitle = "Total number of distinct groups",
            icon = shiny::icon("layer-group"),
            color = "teal")
    })
    shiny::outputOptions(output, "nb_gp", suspendWhenHidden = FALSE)

    shiny::observe({

        toReturn$df <- dataframe()
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

