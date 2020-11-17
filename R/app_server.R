##' The server-side of the Well-Plate Maker application
##'
##' @param input,output,session Internal parameters for {shiny}.
##'
##' @noRd
app_server <- function(input, output, session ) {
    launch_wpm <- shiny::reactiveValues(dt = NULL, flag = NULL)

    ##*************************************************************************
    ## Home panel module
    shiny::callModule(mod_home_server, id = "home")
    ##*************************************************************************
    ## Input file part
    datafile <- shiny::callModule(mod_data_import_server,
                                id = "data_import",
                                session = session)
    
    
    project_title <- shiny::callModule(mod_project_title_server,
                                       id = "project_title",
                                       session = session)
    

    ##*************************************************************************
    ## Plate specifications part
    plate_dimensions <- shiny::callModule(
        mod_plate_dimensions_server,
        id = "p_dim",
        nb_samples = shiny::reactive(datafile$nb_samples)
    )

    forbidden_w <- shiny::callModule(
        mod_special_wells_server,
        id = "special1",
        status = "forbidden",
        p_dimensions = plate_dimensions
    )
    fixed_w <- shiny::callModule(
        mod_special_wells_server,
        id = "special2",
        status = "fixed",
        p_dimensions = plate_dimensions
    )

    plate_specs <- shiny::callModule(
        mod_plate_specifications_server,
        "plate",
        nb_samp_gps = shiny::reactive(datafile$distinct_gps),
        gp_levels = shiny::reactive(datafile$gp_levels),
        project_name = shiny::reactive(project_title()),
        nb_samples = shiny::reactive(datafile$nb_samples),
        p_dimensions = shiny::reactive(plate_dimensions),
        forbid_wells = shiny::reactive(forbidden_w),
        fixed_wells = shiny::reactive(fixed_w)
    )

    ##*************************************************************************
    ## backtracking module part
    ##
    final_data <- shiny::eventReactive(input$start_WPM_Btn, {
        logging::loginfo("class(plate_specs$special_wells) = %s", class(plate_specs$special_wells))
        logging::loginfo("nrow(plate_specs$special_wells) = %s", nrow(plate_specs$special_wells))

        ## requires that the dimensions of the plate be greater than 0
        shiny::validate(
            shiny::need(!is.null(datafile$df),
                        "requires a user data file"),
            shiny::need(!is.null(project_title()), "requires a project title"),
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
        return(shiny::callModule(
            module = mod_backtracking_server,
            id = "backtrack",
            df = datafile$df,
            max_iter = input$nb_iter,
            plate_options = plate_specs))

    })
    # to control when to activate sendSweetAlert
    shiny::observe({
        input$start_WPM_Btn

        if(!is.null(final_data()$d)){
            launch_wpm$dt <- final_data()$d
            launch_wpm$flag <- 1
        }else{
            launch_wpm$flag <- 0
        }
    })


    # *********************************************************************
    # export module part
    #
    shiny::observeEvent(launch_wpm$flag, {
        logging::loginfo("flag: %s",launch_wpm$flag)
        if(launch_wpm$flag == 1){
            shinyWidgets::sendSweetAlert(
                session = session,
                title = "Success !!",
                text = "All in order, you can check your results in the
                Results Panel",
                type = "success"
            )
            shiny::callModule(
                module = mod_data_export_server,
                id = "data_export",
                df = launch_wpm$dt,
                distinct_sample_gps = datafile$distinct_gps,
                gp_levels = datafile$gp_levels,
                plate_opts = plate_specs,
                project_name = project_title()
            )
        }else if(launch_wpm$flag == 0){
            shinyWidgets::sendSweetAlert(
                session = session,
                title = "WPM failed...",
                text = "Seems that we reeched the maximal number of iterations
                without any result... Try again by changing some parameters
                and/or increasing the number of iterations. ",
                type = "error"
            )
        }

    })

    # *********************************************************************
    ## Help panel module
    shiny::callModule(mod_help_server, id = "help")
}
