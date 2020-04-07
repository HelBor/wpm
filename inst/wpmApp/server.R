rm(list=ls())

options(encoding = "UTF-8")


server <- function(input, output, session) {

  #*****************************************************************************
  # Input file part
  #*****************************************************************************
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)


  output$table <- DT::renderDataTable(DT::datatable({
    if(!is.null(datafile())){
      if(class(datafile()) == "data.frame" | class(datafile()) == "matrix"){
        loginfo("dataframe/matrix successfully created", logger = "server")
      }
      datafile()
    }
  }, rownames=FALSE)
  )

  output$nb_ech <- shinydashboard::renderValueBox({
    if(is.null(datafile())){
      valueBox(value = 0 , subtitle = "Total number of samples to place", color="teal")
    }else{
      valueBox(value = nrow(datafile()) , subtitle = "Total number of samples to place", icon=icon("list"), color="teal")
    }
  })



  gp_levels <- reactive({
    nb <- NULL
    if(is.null(datafile())){
      nb <- 0
    }else if("Group" %in% colnames(datafile())){
      nb <- unique(datafile()$Group)
    }
    return(nb)
  })

  distinct_gps <- reactive({
    d_gp <- NULL
    if(is.null(datafile())){
      d_gp <- 0
    }else if("Group" %in% colnames(datafile())){
      d_gp <- length(unique(datafile()$Group))

    }
    validate(
      need(d_gp <= 12, message = "The number of separate groups must not exceed 12.")
    )
    return(d_gp)
  })

  output$distinct_gps <- reactive({
    return(distinct_gps())
  })
  outputOptions(output, "distinct_gps", suspendWhenHidden = FALSE)

  output$nb_gp <- renderValueBox({
    valueBox(value = distinct_gps() , subtitle = "Total number of distinct groups", icon=icon("layer-group"), color="teal")
  })




  #*****************************************************************************
  # Plate specification part
  # Includes the dimensions of the plate, the layout of the blanks,
  # the prohibited wells, the spatial constraints of the plate
  #*****************************************************************************

  plate_specs <- callModule(plateSpec,
                            "plate",
                            nb_samp_gps = distinct_gps,
                            gp_levels = gp_levels,
                            project_name = reactive(input$project_title),
                            nb_samples = reactive(nrow(datafile()))
                            )

  #*****************************************************************************
  # backtracking module part
  # launched only if start button is clicked and required parameters are validated
  #*****************************************************************************


    observeEvent(input$start_WPM_Btn,{

      # requires that the dimensions of the plate be greater than 0
      validate(
        need(!is.null(datafile()), "requires a user data file"),
        need(nrow(datafile()) > 1, "requires a non empty data file"),
        need(plate_specs$nb_lines > 0, "requires a number of rows greater than 0"),
        need(plate_specs$nb_cols > 0, "requires a number of columns greater than 0")
      )

      loginfo("distinct_gps :%s", distinct_gps())
      loginfo("gp_levels :%s", gp_levels())
      data_export <- callModule(module = backtrack,
                                id = "backtrack",
                                df = datafile(),
                                max_iter = input$nb_iter,
                                distinct_sample_gps = distinct_gps,
                                gp_levels = gp_levels,
                                forbidden_wells = reactive(plate_specs$forbidden_wells),
                                rows = reactive(plate_specs$nb_lines),
                                columns = reactive(plate_specs$nb_cols),
                                nb_plates = reactive(plate_specs$nb_plates),
                                constraint = reactive(plate_specs$neighborhood_mod),
                                project_name = reactive(input$project_title)
                                )


      observeEvent(data_export$final_df,{
        loginfo("data_export$final_df: %s", class(data_export$final_df), logger = "server")
        if(class(data_export$final_df) != "data.frame"){
          sendSweetAlert(
            session = session,
            title = "WPM failed...",
            text = "Seems that we reeched the maximal number of iterations without any result... Try again by increasing the number of iterations. ",
            type = "error"
          )
        }else{
          sendSweetAlert(
            session = session,
            title = "Success !!",
            text = "All in order, you can check your results in the Results Panel",
            type = "success"
          )

        }


      })


    })




}