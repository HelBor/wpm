rm(list=ls())

options(encoding = "UTF-8")


server <- function(input, output, session) {
  #*****************************************************************************
  # Input file part
  #*****************************************************************************
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)

  output$table <- renderDataTable(datatable({
    if(class(datafile()) == "data.frame" | class(datafile()) == "matrix"){
      loginfo("dataframe/matrix successfully created", logger = "server")
    }
    datafile() }, rownames=FALSE)
  )

  output$nb_ech <- renderValueBox({
    if(is.null(datafile())){
      valueBox(value = 0 , subtitle = "Total number of samples to place", color="aqua")
    }else{
      valueBox(value = nrow(datafile()) , subtitle = "Total number of samples to place", icon=icon("list"))
    }
  })


  distinct_gps <- reactive({
    if(is.null(datafile())){
      return(0)
    }else{
      return(length(unique(datafile()[,2])))
    }
  })

  output$nb_gp <- renderValueBox({
    valueBox(value = distinct_gps() , subtitle = "Total number of distinct groups", icon=icon("layer-group"))
  })




  #*****************************************************************************
  # Plate specification part
  # Includes the dimensions of the plate, the layout of the blanks,
  # the prohibited wells, the spatial constraints of the plate
  #*****************************************************************************

  plate_specs <- callModule(plateSpec,
                            "plate",
                            project_name = input$project_title,
                            nb_samples = reactive(nrow(datafile()))
                            )


  #*****************************************************************************
  # backtracking module part
  # launched only if start button is clicked and required parameters are validated
  #*****************************************************************************

  # startBtn <- eventReactive(input$start_WPM_Btn, {
  #   validate(
  #     need(datafile(), "requires a user data file"),
  #     need(plate_specs$nb_lines > 0, "requires a number of rows greater than 0"),
  #     need(plate_specs$nb_cols > 0, "requires a number of columns greater than 0")
  #   )
  #   "Done"
  # })
  #
  # output$pressedBtn <- renderText({
  #   startBtn()
  # })



  observeEvent(input$start_WPM_Btn,{
    # requires that the dimensions of the plate be greater than 0
    validate(
      need(!is.null(datafile()), "requires a user data file"),
      need(plate_specs$nb_lines > 0, "requires a number of rows greater than 0"),
      need(plate_specs$nb_cols > 0, "requires a number of columns greater than 0")
      # add the check of nb samples <= total wells - blanks - forbidden
      # need(length(datafile()$Sample.name) <= (plate_specs$nb_lines * plate_specs$nb_cols),
      #      "the dimensions of the plate are not compatible with the number of samples to be placed"),
      # need(length(datafile()$Sample.name) <= ((plate_specs$nb_lines * plate_specs$nb_cols) - plate_specs$forbidden_wells),
      #      "")
    )

    data_export <- callModule(module = backtrack,
               id = "backtrack",
               df = datafile(),
               nb_g = distinct_gps(),
               max_iter = input$nb_iter,
               forbidden_wells = reactive(plate_specs$forbidden_wells),
               rows = reactive(plate_specs$nb_lines),
               columns = reactive(plate_specs$nb_cols),
               constraint = reactive(plate_specs$neighborhood_mod),
               project_name = reactive(input$project_title)
               )

    # output$pressedBtn <- renderText({
    #   "Done"
    # })

    callModule(module = export,
               id = "export",
               df = reactive(data_export$final_df),
               plot = reactive(data_export$final_map)
    )
  })


}