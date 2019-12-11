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
          textInput(ns("forbid_select"), h4("Enter Line Letter & Column number,
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
           fluidRow(verbatimTextOutput(ns("chosen_blank_mode")),
                    dataTableOutput(ns("forbidden_wells")),
                    uiOutput(ns("plotOut"))
                    )

    )
  )
}

# Module server function
plateSpec <- function(input, output, session) {

  output$warning_plate <- renderInfoBox({
    infoBox(title="We assume that all the plates to be filled have the same dimensions.",
            icon = icon("exclamation-triangle"),
            color = "red",
            fill=TRUE)
  })
  totalNbWells <- reactive({
    as.numeric(input$plate_lines)*as.numeric(input$plate_cols)
    })

  output$total_nb_wells <- renderValueBox({
    valueBox(value=totalNbWells(), subtitle = "Number of Wells to fill", icon = icon("vials"))
  })

  nbPlatesToFill <- reactive({
    as.numeric(input$no_plates)
  })

  output$nb_plates_to_fill <- renderValueBox({

    valueBox(value=nbPlatesToFill(), subtitle = "Number of plates to fill")
  })

  output$chosen_blank_mode <- renderPrint({
    input$blank_mode
  })

  output$forbidden_wells <- renderPrint({
    as.character(unlist(strsplit(as.character(input$forbid_select), split=",")))
  })

  blank_wells <- reactive({
    placeBlanksOnPlate(as.numeric(input$plate_lines),
                                    as.numeric(input$plate_cols),
                                    as.character(input$blank_mode))
  })

  forbid_wells <- reactive({
    as.vector(unlist(strsplit(as.character(input$forbid_select), split=",")))
  })

  wells <- reactive({
    combineForbiddenWellsWithBlanks(blank_wells(), forbid_wells())
  })

  output$plotOut <- renderUI({

  print(class(wells()))
    if(!is.null(wells()) ){
      print("on est dans le if pour ploter")
      #renderPlot({
      drawPlateMap(df = wells(), 1, plate_lines = input$plate_lines, plate_cols = input$plate_cols)
      #})
    }else if(!is.null(forbid_wells()) & !is.null(blank_wells()) & is.null(wells())){
      print("on est censé afficher le message d'erreur")
      #renderInfoBox({
        infoBox(title="Error - One or more of the prohibited wells do not exist
    #depending on the plate sizes that have been provided",
                icon = icon("exclamation-triangle"),
                color = "red",
                width = 12,
                fill=TRUE)
      #})
    }
  })

}