rm(list=ls())

options(encoding = "UTF-8")


server <- function(input, output, session) {
  #*****************************************************************************
  # Input file part
  #*****************************************************************************
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)

  output$table <- renderDataTable(datatable({
    datafile() }, rownames=FALSE)
  )

  output$nb_ech <- renderValueBox({
    if(is.null(datafile())){
      valueBox(value = 0 , subtitle = "Total number of samples to place", color="aqua")
    }else{
      valueBox(value = nrow(datafile()) , subtitle = "Total number of samples to place", icon=icon("list"))
    }
  })

  output$nb_gp <- renderValueBox({
    if(is.null(datafile())){
      valueBox(value = 0 , subtitle = "Total number of groups", color="navy")
    }else{
      valueBox(value = length(unique(datafile()[,2])) , subtitle = "Total number of distinct groups", icon=icon("layer-group"))
    }
  })




  #*****************************************************************************
  # Plate specification part
  # Includes the dimensions of the plate, the layout of the blanks,
  # the prohibited wells, the spatial constraints of the surrounding area
  #*****************************************************************************
  plateSpecifications <- callModule(plateSpec, "plate")

  observeEvent(input$start_WPM_Btn,{
    callModule(module = backtrack, id = "backtrack", df=datafile(), df_forbidden=, )
  })


}