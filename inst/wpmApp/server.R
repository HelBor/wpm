rm(list = ls())

options(encoding = "UTF-8")


##' @importFrom DT renderDataTable datatable
server <- function(input, output, session) {

    ##****************************************************************************
    ## Input file part
    ##****************************************************************************
    datafile <- shiny::callModule(csvFile, "datafile", stringsAsFactors = FALSE)

    output$table <- DT::renderDataTable(DT::datatable({
        if (!is.null(datafile())) {
            if (is(datafile(), "data.frame") | is(datafile(), "matrix")) {
                logging::loginfo("dataframe/matrix successfully created",
                                 logger = "server")
            }
            datafile()
        }
        }, rownames = FALSE)
    )

    output$nb_ech <- shinydashboard::renderValueBox({
        if (is.null(datafile())) {
            shinydashboard::valueBox(
                value = 0 ,
                subtitle = "Total number of samples to place",
                color = "teal")
        }else{
            shinydashboard::valueBox(
                value = nrow(datafile()) ,
                subtitle = "Total number of samples to place",
                icon = shiny::icon("list"),
                color = "teal")
        }
    })


    ## Vector containing the different group names
    gp_levels <- shiny::reactive({
        nb <- NULL
        if (is.null(datafile())) {
            nb <- 0
        }else if ("Group" %in% colnames(datafile())) {
            nb <- unique(datafile()$Group)
        }
        return(nb)
    })

    ## The number of distinct groups in the file
    distinct_gps <- shiny::reactive({
        d_gp <- NULL
        if (is.null(datafile())) {
            d_gp <- 0
        }else if ("Group" %in% colnames(datafile())) {
            d_gp <- length(unique(datafile()$Group))
        }
        shiny::validate(
            shiny::need(d_gp <= 12,
                        message = "The number of separate groups must not exceed 12.")
        )
        return(d_gp)
    })

    output$distinct_gps <- shiny::reactive({
        return(distinct_gps())
    })
    shiny::outputOptions(output, "distinct_gps", suspendWhenHidden = FALSE)

    output$nb_gp <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = distinct_gps() ,
            subtitle = "Total number of distinct groups",
            icon = shiny::icon("layer-group"),
            color = "teal")
    })

    ##****************************************************************************
    ## Plate specification part
    ## Includes the dimensions of the plate, the layout of the blanks,
    ## the prohibited wells, the spatial constraints of the plate
    ##****************************************************************************

    plate_specs <- shiny::callModule(
        plateSpec,
        "plate",
        nb_samp_gps = distinct_gps,
        gp_levels = gp_levels,
        project_name = shiny::reactive(input$project_title),
        nb_samples = shiny::reactive(nrow(datafile()))
    )

    ##****************************************************************************
    ## backtracking module part
    ## launched only if start button is clicked and required parameters are validated
    ##****************************************************************************
    shiny::observeEvent(input$start_WPM_Btn,{

        ## requires that the dimensions of the plate be greater than 0
        shiny::validate(
            shiny::need(!is.null(datafile()),
                        "requires a user data file"),
            shiny::need(nrow(datafile()) > 1,
                        "requires a non empty data file"),
            shiny::need(plate_specs$nb_lines > 0,
                        "requires a number of rows greater than 0"),
            shiny::need(plate_specs$nb_cols > 0,
                        "requires a number of columns greater than 0")
        )

        logging::loginfo("distinct_gps :%s", distinct_gps())
        logging::loginfo("gp_levels :%s", gp_levels())
        data_export <- shiny::callModule(
            module = backtrack,
            id = "backtrack",
            df = datafile(),
            max_iter = input$nb_iter,
            distinct_sample_gps = distinct_gps,
            gp_levels = gp_levels,
            forbidden_wells = shiny::reactive(plate_specs$forbidden_wells),
            rows = shiny::reactive(plate_specs$nb_lines),
            columns = shiny::reactive(plate_specs$nb_cols),
            nb_plates = shiny::reactive(plate_specs$nb_plates),
            constraint = shiny::reactive(plate_specs$neighborhood_mod),
            project_name = shiny::reactive(input$project_title)
        )


        shiny::observeEvent(data_export$final_df,{
            logging::loginfo(
                "data_export$final_df: %s",
                class(data_export$final_df),
                logger = "server"
            )
            if (!is(data_export$final_df, "data.frame")) {
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "WPM failed...",
                    text = "Seems that we reeched the maximal number of iterations
                    without any result... Try again by increasing the number of iterations. ",
                    type = "error"
                )
            }else{
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Success !!",
                    text = "All in order, you can check your results in the Results Panel",
                    type = "success"
                )
            }
        })
    })


}
