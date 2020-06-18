##' The server-side of the Well-Plate Maker application
##'
##' @param input,output,session Internal parameters for {shiny}.
##'
##' @noRd
app_server <- function(input, output, session ) {
    ##*************************************************************************
    ## Home panel module
    shiny::callModule(mod_home_server, id = "home")
    ##*************************************************************************
    ## Input file part
    datafile <- shiny::callModule(mod_data_import_server,
                                id = "data_import",
                                session = session)

    ##*************************************************************************
    ## Plate specification part
    plate_dimensions <- shiny::callModule(
        mod_plate_dimensions_server,
        id = "p_dim"
    )

    forbidden_w <- shiny::callModule(
        mod_special_wells_server,
        id = "special1",
        status = "forbidden",
        p_dimensions = shiny::reactive(plate_dimensions)
    )
    fixed_w <- shiny::callModule(
        mod_special_wells_server,
        id = "special2",
        status = "fixed",
        p_dimensions = shiny::reactive(plate_dimensions)
    )

    plate_specs <- shiny::callModule(
        mod_plate_specifications_server,
        "plate",
        nb_samp_gps = shiny::reactive(datafile$distinct_gps),
        gp_levels = shiny::reactive(datafile$gp_levels),
        project_name = shiny::reactive(input$project_title),
        nb_samples = shiny::reactive(datafile$nb_samples),
        p_dimensions = shiny::reactive(plate_dimensions),
        forbid_wells = forbidden_w,
        fixed_wells = fixed_w
    )

    shiny::observe({
        logging::loginfo("button status: %s", input$start_WPM_Btn)
    })

    ##*************************************************************************
    ## backtracking module part
    ##
    shiny::observeEvent(input$start_WPM_Btn,{
        logging::logwarn("button status dans l'observeEvent %s", input$start_WPM_Btn)
        logging::loginfo("class(plate_specs$special_wells) = %s", class(plate_specs$special_wells))
        logging::loginfo("nrow(plate_specs$special_wells) = %s", nrow(plate_specs$special_wells))

        ## requires that the dimensions of the plate be greater than 0
        shiny::validate(
            shiny::need(!is.null(datafile$df),
                        "requires a user data file"),
            shiny::need(datafile$nb_samples > 1,
                        "requires a non empty data file"),
            if(is.null(plate_specs$special_wells)){
                shiny::need(
                    plate_specs$total_nb_wells >= datafile$nb_samples,
                    "need a number of available wells greater or equal than number
                    of samples to place"
                )
            }else{
                shiny::need(
                    plate_specs$total_nb_wells >= datafile$nb_samples + nrow(plate_specs$special_wells),
                    "need a number of available wells greater or equal than number
                    of samples to place"
                )
            }

        )

        logging::loginfo("distinct_gps :%s", datafile$distinct_gps)
        logging::loginfo("gp_levels :%s", datafile$gp_levels)
        logging::loginfo("nb max iter: %s", input$nb_iter)
        final_data <- shiny::callModule(
            module = mod_backtracking_server,
            id = "backtrack",
            df = datafile$df,
            max_iter = input$nb_iter,
            plate_options = plate_specs)


        shiny::observeEvent(final_data, {
            if (methods::is(final_data$d, "data.frame")) {
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "Success !!",
                    text = "All in order, you can check your results in the
        Results Panel",
                    type = "success"
                )
                # *********************************************************************
                # export module part
                shiny::callModule(
                    module = mod_data_export_server,
                    id = "data_export",
                    df = final_data$d,
                    distinct_sample_gps = shiny::reactive(datafile$distinct_gps),
                    gp_levels = shiny::reactive(datafile$gp_levels),
                    plate_opts = plate_specs,
                    project_name = shiny::reactive(input$project_title)
                )
            }else{
                shinyWidgets::sendSweetAlert(
                    session = session,
                    title = "WPM failed...",
                    text = "Seems that we reeched the maximal number of
        iterations without any result... Try again by increasing
        the number of iterations. ",
                    type = "error"
                )
            }
        })

    })
    ## Help panel module
    shiny::callModule(mod_help_server, id = "help")
}
