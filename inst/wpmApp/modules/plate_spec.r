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
          numericInput(ns("no_plates"), label="How many plates?", value=1, min=1)
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
                                         each box separated by commas without spaces.\n
                                            The wells already filled with a blank
                                            will not appear crossed out."),
                    value = NULL,
                    placeholder = "Ex: A1,B2,C3")
      ),
      box(status = "primary",
          width = 12,
          title = h3("5 - Neighborhood contraints"),
          conditionalPanel(ns("input.blank_mode == 'by_row'"),
                           textOutput("row_constraint")
                          ),
          conditionalPanel(ns("input.blank_mode == 'by_column'"),
                           textOutput("column_constraint")
                           ),
          conditionalPanel(ns("input.blank_mode == 'none'"),
                           radioButtons(ns("constraint_select"),label = NULL,
                                        choices = c("North-South" = "NS",
                                                    "West-East" = "WE",
                                                    "North-South-West-East" = "NSWE")
                                        )
                           )

          )
    ),
    # Plate specification outputs
    column(width = 6,
           fluidRow(infoBoxOutput(ns("warning_plate"), width = 12)),
           fluidRow(valueBoxOutput(ns("total_nb_wells"), width = 6),
                    valueBoxOutput(ns("nb_plates_to_fill"), width = 6)
                    ),
           plotOutput(ns("plotOut"), height = 500)
    )
  )
}

# Module server function
plateSpec <- function(input, output, session) {

  totalNbWells <- reactive({
    as.numeric(input$plate_lines)*as.numeric(input$plate_cols)
    })

  output$total_nb_wells <- renderValueBox({
    valueBox(value=totalNbWells(),
             subtitle = "Number of fillable wells",
             icon = icon("vials"))
  })

  output$nb_plates_to_fill <- renderValueBox({
    valueBox(value=as.numeric(input$no_plates),
             subtitle = "Number of plates to fill",
             icon = icon("dice-four"))
  })

  output$warning_plate <- renderInfoBox({
    infoBox(title="We assume that all the plates to be filled have the same dimensions.",
            icon = icon("exclamation-triangle"),
            color = "red",
            fill=TRUE)
  })



  blank_wells <- reactive({
    placeBlanksOnPlate(as.numeric(input$plate_lines),
                                    as.numeric(input$plate_cols),
                                    as.character(input$blank_mode))
  })

  forbid_wells <- reactive({
    # si des cases interdites on été saisies, alors on transforme en un df compatible
    # avec la suite du code
    if(input$forbid_select != ""){
      fw <- as.vector(unlist(strsplit(as.character(input$forbid_select),
                                      split=",")))
      convertVector2Df(fw, input$plate_lines, input$plate_cols)
    }else{
      return(NULL)
    }
  })

  wells_to_plot <- reactive({
    # s'il y a des blancs et des cases interdites alors il faut les rassembler
    if(!is.null(blank_wells()) & !is.null(forbid_wells()) ){
      result <- base::rbind(blank_wells(), forbid_wells())
      result <- distinct(result, Row, Column, .keep_all = TRUE)
      result
    # s'il n'y a pas de blancs, qu'il y a des interdits, on ne renvoie que les
    # interdits
    }else if(input$blank_mode == "none" & !is.null(forbid_wells()) ){
      forbid_wells()
    # s'il n'y a que des blancs, on ne renvoie que ça
    }else if(!is.null(blank_wells()) & is.null(forbid_wells())){
      blank_wells()
    }
  })

  output$plotOut <- renderPlot({
    # pour que la fonction drawPlateMap fonctionne, il faut donner un nombre de
    # lignes et de colonnes > 0 et au minimum un dataframe vide avec les bons
    # noms de colonne
    if(input$plate_lines != 0 & input$plate_cols != 0){
      if(is.null(wells_to_plot())){
        df <- setnames(setDF(lapply(c(NA, NA, NA, NA, NA, NA), function(...) character(0))),
                       c("Sample.name", "Group", "Well", "Status", "Row", "Column"))
        drawPlateMap(df = df,
                     1,
                     plate_lines = input$plate_lines,
                     plate_cols = input$plate_cols)
      }else{
        drawPlateMap(df = wells_to_plot(),
                     2,
                     plate_lines = input$plate_lines,
                     plate_cols = input$plate_cols)
      }
    }
  })

  output$row_constraint <- renderText({
    print("You have selected the Per Row mode, therefore the only available
    neighborhood constraint is 'West-East'.")
  })

  output$column_constraint <- renderText({
    print("You have selected the 'Per Column' mode, so the only neighbor constraint
    available is 'North-South'.")
  })
}