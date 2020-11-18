# module principal d'importation de fichier
mod_data_import_2_ui <- function(id){
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
                    shiny::h4("Do you want to use the demo dataset?"),
                    shinyWidgets::switchInput(
                        inputId = ns("data_test"),
                        onStatus = "success",
                        offStatus = "danger",
                        value = F
                    ),
                    shiny::conditionalPanel(
                        condition = "input.data_test == T",
                        mod_data_import_dataset_ui(ns("d_test")),
                        ns = ns
                    ),
                    shiny::conditionalPanel(
                        condition = "input.data_test == F",
                        shiny::fileInput(ns("file"),
                                         label = NULL,
                                         accept = c(
                                             "text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")
                        ),
                        shiny::uiOutput("panel_parameters"),
                        ns = ns
                    )


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
                            DT::dataTableOutput(ns("user_table"))
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

##' data_import module Server Function
##'
##' @description server part of the data import module. Allows to browse a file
##' in CSV, text or TSV format, create a dataframe and set the column names.
##' @param input,output,session Internal shiny parameters
##' @return dataframe containing the data to place on the plates plan(s).
##' @noRd
mod_data_import_2_server <- function(input, output, session){

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

    file_extension <- shiny::reactive({
        return(tools::file_ext(userFile()$datapath))
    })


    output$panel_parameters <- shiny::renderUI({
        if(!is.null(userFile()) & !is.null(file_extension())){
            print(paste("file ext:",file_extension()))
            if(file_extension() %in% c("csv","txt")){
                mod_data_import_csv_ui("input_csv")
            }
        }

    })




    user_df <- shiny::reactive({

        if(!is.null(input$data_test)){

            if(input$data_test){
                print("on est dans le if de user_df")
                df <- shiny::callModule(mod_data_import_dataset_server,
                                        id = "d_test", session = session)
            }else{
                print("on est dans le else de user_df")
                if(!is.null(file_extension())){
                    print(paste("file ext:",file_extension()))
                    if(file_extension() %in% c("csv","txt")){
                        df <- shiny::callModule(mod_data_import_csv_server,
                                                id = "input_csv",
                                                session = session,
                                                userFile = userFile())
                    }
                }

            }
            return(df)
        }




    })


    output$user_table <- DT::renderDataTable(
        DT::datatable({
            shiny::validate(
                shiny::need(!is.null(user_df()$df_csv), "Wrong set of parameters, we can
                not read your file... Please correct those that have been misinformed")
            )
            user_df()$df_csv
        },
        rownames = FALSE,
        options = list(columnDefs = list(list(className = 'dt-center', targets ="_all")),
                       pageLength = 5))
    )

    output$wpm_table <- DT::renderDataTable(
        DT::datatable({
            shiny::validate(
                shiny::need(!is.null(user_df()$df_wpm), "Wrong set of parameters, we can
                not read your file... Please correct those that have been misinformed")
            )
            user_df()$df_wpm
        },
        rownames = FALSE,
        options = list(columnDefs = list(list(className = 'dt-center', targets ="_all")),
                       pageLength = 5))
    )


    output$nb_ech <- shinydashboard::renderValueBox({
        if (is.null(user_df()$df_wpm)) {
            shinydashboard::valueBox(
                value = 0 ,
                subtitle = "Total number of samples to place",
                color = "teal")
        }else{
            shinydashboard::valueBox(
                value = nrow(user_df()$df_wpm) ,
                subtitle = "Total number of samples to place",
                icon = shiny::icon("list"),
                color = "teal")
        }
    })
    ## Vector containing the different group names
    gp_levels <- shiny::reactive({
        nb <- NULL
        if (is.null(user_df()$df_wpm)) {
            nb <- 0
        }else if ("Group" %in% colnames(user_df()$df_wpm)) {
            nb <- unique(user_df()$df_wpm$Group)
        }
        return(nb)
    })

    ## The number of distinct groups in the file
    distinct_gps <- shiny::reactive({
        d_gp <- NULL
        if (is.null(user_df()$df_wpm)) {
            d_gp <- 0
        }else if ("Group" %in% colnames(dataframe()$df_wpm)) {
            d_gp <- length(unique(user_df()$df_wpm$Group))
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
        if (is.null(user_df()$df_wpm)) {
            nb <- 0
        }else{
            nb <- nrow(user_df()$df_wpm)
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
        toReturn$df <- user_df()$df_wpm
        toReturn$distinct_gps <- distinct_gps()
        toReturn$gp_levels <- gp_levels()
        toReturn$nb_samples <- nb_s()
    })
    ## Return the reactive that yields the data frame
    return(toReturn)
}
