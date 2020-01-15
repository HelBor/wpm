# This R script contains the UI and Server side of the app to manage the plates
# specifications.




# Module UI function
plateSpecUI <- function(id, label = "Plate specifications") {
  # Create a namespace function using the provided id
  ns <- NS(id)

  fluidRow(
    column(width=6,
      box(status="warning",
          width = 12,
          title=h3("2 - Plate dimensions"),
          h4("How many lines on your plate?"),
          numericInput(ns("plate_lines"), label = NULL,
                       value=0, min=0,
                       width = "80px"),
          h4("how many columns on your plate?"),
          numericInput(ns("plate_cols"), label = NULL,
                       value=0, min=0,
                       width = "80px"),
          h4("How many plates?"),
          numericInput(ns("no_plates"), label = NULL,
                       value=1, min=1,
                       width = "80px")
      ),
      box(status="warning",
          width = 12,
          title=h3("3 - Plate constraints"),
          h4("How to place Blanks on the plate"),

          awesomeRadio(inputId = ns("blank_mode"),
                       label = NULL,
                       choices = c("No blanks" = "none",
                                   "Per line" = "by_row",
                                   "Per column" = "by_column",
                                   "Checkerboard" = "checkerboard"),
                       selected = NULL,
                       status = "warning"

          ),
          hr(),
          h4("Neighborhood contraints"),

          conditionalPanel(condition = "input.blank_mode == 'by_row'",
                           awesomeRadio(inputId = ns("constraint_row"),
                                        label = NULL,
                                        choices = c(
                                                    "West-East" = "WE",
                                                    "None" = "none"),
                                        selected = NULL,
                                        status = "warning"

                           ),
                           ns = ns),
          conditionalPanel(condition = "input.blank_mode == 'by_column'",
                           awesomeRadio(inputId = ns("constraint_column"),
                                        label = NULL,
                                        choices = c("North-South" = "NS",
                                                    "None" = "none"),
                                        selected = NULL,
                                        status = "warning"

                           ),
                           ns = ns),
          conditionalPanel(condition = "input.blank_mode == 'none'",
                           awesomeRadio(inputId = ns("constraint_none"),
                                        label = NULL,
                                        choices = c("North-South" = "NS",
                                                   "West-East" = "WE",
                                                   "North-South-West-East" = "NEWS",
                                                   "None" = "none"),
                                        selected = NULL,
                                        status = "warning"

                           ),
                           ns = ns),
          conditionalPanel(condition = "input.blank_mode == 'checkerboard'",
                           div(
                             HTML(paste("You have selected the ",
                                        tags$span(style="color:red", "Checkerboard"),
                                        "mode, therefore there are no available neighborhood constraints.",
                                        sep = " ")
                             )
                           ),
                           ns = ns)
      ),
      box(status="warning",
          width = 12,
          title=h3("4 - Forbidden Wells"),
          textInput(ns("forbid_select"), h4("Enter Line Letter & Column number,
                                         each box separated by commas without spaces.\n
                                            The wells already filled with a blank
                                            will not appear crossed out."),
                    value = NULL,
                    placeholder = "Ex: A1,B2,C3")
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
plateSpec <- function(input, output, session, project_name, nb_samples) {

  toReturn <- reactiveValues(
    nb_lines = NULL,
    nb_cols = NULL,
    nb_plates = NULL,
    forbidden_wells = NULL,
    neighborhood_mod = NULL
  )


  p_lines <- reactive({
    if(is.na(input$plate_lines)){
      return(0)
    }else{
      return(input$plate_lines)
    }
  })

  p_cols <- reactive({
    if(is.na(input$plate_cols)){
      return(0)
    }else{
      return(input$plate_cols)
    }
  })

  nb_p <- reactive({
    if(is.na(input$no_plates)){
      return(0)
    }else{
      return(input$no_plates)
    }
  })

  totalNbWells <- reactive({
    tNbW <- p_lines()*p_cols()*nb_p()
    loginfo("totalNbWells = %d", tNbW, logger = "plate_spec")
    return(tNbW)
    })

  output$total_nb_wells <- renderValueBox({
    valueBox(value=totalNbWells(),
             subtitle = "Number of fillable wells",
             icon = icon("vials"),
             color="teal")
  })

  output$nb_plates_to_fill <- renderValueBox({
    valueBox(value=as.numeric(input$no_plates),
             subtitle = "Number of plates to fill",
             icon = icon("dice-four"),
             color="teal")
  })

  output$warning_plate <- renderInfoBox({
    infoBox(title=HTML(paste("We assume that all the plates to be filled have the same dimensions.",
                       br(),
                       "Also, when you want to generate more than 1 plate,",
                       br(),
                       "WPM uses balanced group workforces to distribute the samples within the plates."
                         )),
            icon = icon("exclamation-triangle"),
            color = "red",
            fill=TRUE)
  })



  blank_wells <- reactive({
    validate(
      need((p_lines() > 0 & p_cols() > 0), "requires a plate with positive dimensions.")
    )
    placeBlanksOnPlate(p_lines(),
                       p_cols(),
                       as.character(input$blank_mode))
  })

  forbid_wells <- reactive({
    # si des cases interdites on été saisies, alors on transforme en un df compatible
    # avec la suite du code
    if(input$forbid_select != ""){
      fw <- as.vector(unlist(strsplit(as.character(input$forbid_select),
                                      split=",")))
      return(convertVector2Df(fw, p_lines(), p_cols()))
    }else{
      return(NULL)
    }
  })

  wells_to_plot <- reactive({
    ret <- NULL
    loginfo("nb samples : %d, totalNbWells : %d", nb_samples(), totalNbWells(), logger = "plate_spec")

    if(is.null(blank_wells())){
      nb_b <- 0
      loginfo("nb_b: %s", nb_b, logger = "plate_spec")
    }else{
      nb_b <- nrow(blank_wells())
      loginfo("nb_b: %s", nb_b, logger = "plate_spec")
    }
    if(is.null(forbid_wells())){
      nb_f <- 0
      loginfo("nb_f: %s", nb_f, logger = "plate_spec")
    }else{
      nb_f <- nrow(forbid_wells())
      loginfo("nb_f: %s", nb_f, logger = "plate_spec")
    }
    validate(
      need(nb_samples() <= (totalNbWells()),
           "The dimensions of the plate are not compatible with the number of samples to be placed.
           Please increase the number of plates to fill or provide a dataset with fewer samples.")
    )
    # s'il y a des blancs et des cases interdites alors il faut les rassembler
    if(!is.null(blank_wells()) & !is.null(forbid_wells()) ){
      validate(
        need(nb_samples() <= (totalNbWells() - (nb_b*nb_p()) - (nb_f*nb_p())),
             "The blank mode and/or forbidden wells selected are not compatible with the plate's dimensions and the number of samples to be placed.
             If you want to keep this blank mode, please increase the number of plates to fill or provide a dataset with fewer samples.
             Otherwise, please change the blank mode.")
      )
      result <- base::rbind(blank_wells(), forbid_wells())
      result <- distinct(result, Row, Column, .keep_all = TRUE)
      ret <- result
      # s'il n'y a pas de blancs, qu'il y a des interdits, on ne renvoie que les
      # interdits
    }else if(input$blank_mode == "none" & !is.null(forbid_wells()) ){
      validate(
        need(nb_samples() <= (totalNbWells() - (nb_f*nb_p())),
             "The forbidden wells selected are not compatible with the plate's dimensions and the number of samples to be placed.
             To solve this issue, please:
             - decrease the number of forbidden wells
             - or increase the number of plates to fill
             - or provide a dataset with fewer samples.")
      )
      ret <- forbid_wells()
      # s'il n'y a que des blancs, on ne renvoie que ça
    }else if(!is.null(blank_wells()) & is.null(forbid_wells())){
      validate(
        need(nb_samples() <= (totalNbWells() - (nb_b*nb_p())),
             "The blank mode selected is not compatible with the plate's dimensions and the number of samples to be placed.
             If you want to keep this blank mode, please increase the number of plates to fill or provide a dataset with fewer samples.
             Otherwise, please change the blank mode.")
      )
      ret <- blank_wells()
    }

    return(ret)
  })


  output$plotOut <- renderPlot({
    # pour que la fonction drawPlateMap fonctionne, il faut donner un nombre de
    # lignes et de colonnes > 0 et au minimum un dataframe vide avec les bons
    # noms de colonne
    if(p_lines() != 0 & p_cols() != 0){
      if(is.null(wells_to_plot())){
        df <- setnames(setDF(lapply(c(NA, NA, NA, NA, NA, NA), function(...) character(0))),
                       c("Sample.name", "Group", "Well", "Status", "Row", "Column"))
        drawPlateMap(df = df,
                     1,
                     plate_lines = p_lines(),
                     plate_cols = p_cols(),
                     project_title = project_name)
      }else{
        drawPlateMap(df = wells_to_plot(),
                     2,
                     plate_lines = p_lines(),
                     plate_cols = p_cols(),
                     project_title = project_name)
      }
    }
  })

  nbh_mod <- reactive({
    if(input$blank_mode == "by_row"){
      return(input$constraint_row)
    }else if(input$blank_mode == "by_column"){
      return(input$constraint_column)
    }else if(input$blank_mode == "none"){
      return(input$constraint_none)
    }else if(input$blank_mode == "checkerboard"){
      return("none")
    }
  })

  observe({
    loginfo("nb of plate lines : %d", p_lines(), logger = "plate_spec")
    loginfo("nb of plate cols : %d", p_cols(), logger = "plate_spec")
    loginfo("selected mode : %s", nbh_mod(), logger = "plate_spec")
    loginfo("number of forbidden wells: %s ", nrow(wells_to_plot()), logger = "plate_spec")
    toReturn$nb_lines <- p_lines()
    toReturn$nb_cols <- p_cols()
    toReturn$nb_plates <- nb_p()
    # dataframe which contains the blanks and forbidden wells
    toReturn$forbidden_wells <- wells_to_plot()
    toReturn$neighborhood_mod <- nbh_mod()
  })

  return(toReturn)
}