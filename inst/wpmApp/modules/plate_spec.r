# This R script contains the UI and Server side of the app to manage the plates
# specifications.




# Module UI function
plateSpecUI <- function(id, label = "Plate specifications") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fluidRow(
    column(width=6,
      box(status="primary",
          width = 12,
          title=h3("2 - Plate dimensions"),
          numericInput(ns("plate_lines"), label = "lines", value=0, min=0),
          numericInput(ns("plate_cols"), label="columns", value=0, min=0),
          numericInput(ns("no_plates"), label="How many plates?", value=0, min=1)
      ),
      box(status="primary",
          width = 12,
          title=h3("3 - How to place Blanks on the plate"),
          #blank parameter input
          radioButtons(ns("blank_mode"), "Please choose a mode",
                       choices = c("No blanks" = "none",
                                   "Per line" = "by_row",
                                   "Per column" = "by_column",
                                   "Checkerboard" = "checkerboard"),
                       selected = "none")
      ),
      box(status="primary",
          width = 12,
          title=h3("4 - Forbidden Wells"),
          uiOutput("forbidden_wells_input")

      )
    ),
    # Plate specification outputs
    column(width = 6,
           fluidRow(infoBoxOutput(ns("warning_plate"), width = 12)),
           fluidRow(valueBoxOutput(ns("total_nb_wells"), width = 6),
                    valueBoxOutput(ns("nb_plates_to_fill"), width = 6)
                    )


    )
  )
}

# Module server function
plateSpec <- function(input, output, session) {

  totalNbWells <- reactive({
    as.numeric(input$plate_lines)*as.numeric(input$plate_cols)
  })

  nbPlatesToFill <- reactive({
    as.numeric(input$no_plates)
  })

  output$warning_plate <- renderInfoBox({
    infoBox(title="We assume that all the plates to be filled have the same dimensions.",
            icon = icon("exclamation-triangle"),
            color = "red",
            fill=TRUE)
  })

  output$total_nb_wells <- renderValueBox({
    valueBox(value=totalNbWells(), subtitle = "Number of Wells to fill", icon = icon("vials"))
  })

  output$nb_plates_to_fill <- renderValueBox({
    valueBox(value=nbPlatesToFill(), subtitle = "Number of plates to fill")
  })

  blanks_wells <- reactive({
    placeBlanksOnPlate(input$plate_lines,input$plate_cols,input$blank_mode)
  })

  # For the forbidden wells
  output$interdit <- renderUI({
    textInput("forbid_select", h4("Enter Line Letter & Column number, each box separated by commas"),
              value = "Ex: A1,B2")
  })


  forbid_wells <- reactive({
    as.vector(unlist(strsplit(as.character(input$forbid_select), split=",")))
  })

  blanks_df <- reactive({
    combineForbiddenWellsWithBlanks(blanks_wells, forbid_wells)
  })

  if(class(blanks_df) == "data.frame"){
    # plot the plate
    output$PlotOut <- renderPlot({
      drawPlateMap(df = blanks_df, 2, plate_lines = input$plate_lines, plate_cols = input$plate_cols)
    })

  }else{
    output$PlotOutput <- renderInfoBox({
      infoBox(title=blanks_df,
              icon = icon("exclamation-triangle"),
              color = "red",
              fill=TRUE)
  }


}