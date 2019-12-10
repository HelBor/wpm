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
          textInput("forbid_select", h4("Enter Line Letter & Column number,
                                         each box separated by commas."),
                    value = "",
                    placeholder = "Ex: A1,B2,C3")
      )
    ),
    # Plate specification outputs
    column(width = 6,
           fluidRow(infoBoxOutput(ns("warning_plate"), width = 12)),
           fluidRow(valueBoxOutput(ns("total_nb_wells"), width = 6),
                    valueBoxOutput(ns("nb_plates_to_fill"), width = 6)
                    ),
           fluidRow(uiOutput("plotOut"))

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


  wells <- reactiveValues(
    "blank_wells" = placeBlanksOnPlate(input$plate_lines,input$plate_cols,input$blank_mode),
    "forbid_wells" = as.vector(unlist(strsplit(as.character(input$forbid_select), split=",")))
  )

  isolate(wells$forbid_wells)
  #forbid_wells <- reactive({
  #  as.vector(unlist(strsplit(as.character(input$forbid_select), split=",")))
  #})

  wells_df <- reactiveValues({
    "wells" = combineForbiddenWellsWithBlanks(wells$blank_wells, wells$forbid_wells)
  })



  output$plotOut <- renderUI({
    if(is.null(wells_df$wells) ){
      renderPlot({
        drawPlateMap(df = wells_df$wells, 2, plate_lines = input$plate_lines, plate_cols = input$plate_cols)
      })
    }else{
      renderInfoBox({
        infoBox(title="Error - One or more of the prohibited wells do not exist
    #depending on the plate sizes that have been provided",
                icon = icon("exclamation-triangle"),
                color = "red",
                fill=TRUE)
      })
    }
  })

}