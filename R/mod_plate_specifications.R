##' plate_specifications UI Function
##'
##' @description A shiny Module allowing to process the parameters specified 
##' by the user in order to generate the plate plans afterwards.
##'
##' @param id Internal parameters for {shiny}.
##' @return a Fuild Page layout
mod_plate_specifications_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::fluidRow(
        shiny::column(
            width = 6,
            shinydashboard::box(
                status = "warning",
                width = 12,
                solidHeader = TRUE,
                title = shiny::h3("4 - Blanks"),
                shiny::fluidRow(
                    shiny::column(
                        width = 10,
                        shiny::h4("How to place Blanks on the plate")
                    ),
                shiny::column(
                    width = 2,
                    align = "right",
                    shinyWidgets::dropdownButton(
                        shiny::h4("What are blanks?"),
                        shiny::div("By blanks we mean a preparation without a biological
                             sample in it.", shiny::br(), "You can place them in 
                             line or column. In these two cases there will be blanks 
                             every other line/column."),
                        icon = shiny::icon("info-circle"),
                        tooltip = shinyWidgets::tooltipOptions(title = "Help"),
                        status = "warning",
                        size = "sm",
                        width = "350px"
                    )
                )
            ),
            
            shiny::fluidRow(
                shiny::column(
                    width = 5,
                    shinyWidgets::awesomeRadio(
                        inputId = ns("blank_mode"),
                        label = NULL,
                        choices = c("No blanks" = "none",
                                    "Per line" = "by_row",
                                    "Per column" = "by_column",
                                    "Checkerboard" = "checkerboard",
                                    "Choose by hand" = "by_hand"
                        ),
                        selected = NULL,
                        status = "warning"
                    )
                ),
                shiny::column(
                    width = 7,
                    shiny::conditionalPanel(
                        condition = "input.blank_mode == 'by_row' | input.blank_mode == 'by_column'",
                        shinyWidgets::awesomeRadio(
                            inputId = ns("start_blank"),
                            label = shiny::h4("starting placing in:"),
                            choices = c("even" = "even", "odd" = "odd"),
                            selected = NULL,
                            status = "warning"
                        ),
                        ns = ns
                    ),
                    shiny::conditionalPanel(
                        condition = "input.blank_mode == 'by_hand'",
                        shiny::textInput(
                            ns("hand_select"),
                            shiny::h4("Enter Line Letter & Column number, each box 
                            separated by commas without spaces. \n The wells already filled
                            as forbidden will not be drawn as 'Blank'."),
                            value = NULL,
                            placeholder = "Ex: A1,B2,C3"
                        ),
                        ns = ns
                    )
                )
            ),
            
            shiny::hr(),
            shiny::fluidRow(
                shiny::column(
                    width = 10,
                    shiny::h4("Neighborhood contraints")
                ),
                shiny::column(
                    width = 2,
                    align = "right",
                    shinyWidgets::dropdownButton(
                        shiny::h4("What are neighborhood constraints?"),
                        shiny::div(""),
                        icon = shiny::icon("info-circle"),
                        tooltip = shinyWidgets::tooltipOptions(title = "Help"),
                        status = "warning",
                        size = "sm",
                        width = "350px"
                    )
                )
            ),
            
            shiny::conditionalPanel(
                condition = "input.blank_mode == 'by_row'",
                shinyWidgets::awesomeRadio(
                    inputId = ns("constraint_row"),
                    label = NULL,
                    choices = c("West-East" = "WE", "None" = "none"),
                    selected = NULL,
                    status = "warning"
                ),
                ns = ns),
            
            shiny::conditionalPanel(
                condition = "input.blank_mode == 'by_column'",
                shinyWidgets::awesomeRadio(
                    inputId = ns("constraint_column"),
                    label = NULL,
                    choices = c("North-South" = "NS", "None" = "none"),
                    selected = NULL,
                    status = "warning"
                ),
                ns = ns),
            
            shiny::conditionalPanel(
                condition = "input.blank_mode == 'none' & output.nb_gps > 3",
                shinyWidgets::awesomeRadio(
                    inputId = ns("constraint_none_sup3"),
                    label = NULL,
                    choices = c("North-South" = "NS",
                                "West-East" = "WE",
                                "North-South-West-East" = "NEWS",
                                "None" = "none"),
                    selected = NULL,
                  status = "warning"
                ),
                ns = ns),
            
            shiny::conditionalPanel(
                condition = "input.blank_mode == 'none' & output.nb_gps <= 3",
                shinyWidgets::awesomeRadio(
                    inputId = ns("constraint_none_inf3"),
                    label = NULL,
                    choices = c("North-South" = "NS",
                                "West-East" = "WE",
                                "None" = "none"),
                    selected = NULL,
                    status = "warning"
                ),
                ns = ns),
            
            shiny::conditionalPanel(
                condition = "input.blank_mode == 'by_hand' & output.nb_gps > 3",
                shinyWidgets::awesomeRadio(
                    inputId = ns("constraint_by_hand_sup3"),
                    label = NULL,
                    choices = c("North-South" = "NS",
                                "West-East" = "WE",
                                "North-South-West-East" = "NEWS",
                                "None" = "none"),
                    selected = NULL,
                    status = "warning"
                ),
                ns = ns),
            
            
            shiny::conditionalPanel(
                condition = "input.blank_mode == 'by_hand' & output.nb_gps <= 3",
                shinyWidgets::awesomeRadio(
                    inputId = ns("constraint_by_hand_inf3"),
                    label = NULL,
                    choices = c("North-South" = "NS",
                                "West-East" = "WE",
                                "None" = "none"),
                    selected = NULL,
                    status = "warning"),
                ns = ns),
            
            shiny::conditionalPanel(
                condition = "input.blank_mode == 'checkerboard'",
                shiny::div(
                    shiny::HTML(
                        paste("You have selected the ",
                              shiny::span(style = "color:red", "Checkerboard"),
                              "mode, therefore there are no vailable 
                              neighborhood constraints.",
                              sep = " ")
                    )
                ),
                ns = ns)
            ) # end of box
        ), # end of column 1
      
        ## Plate specification outputs
        shiny::column(
            width = 6,
            shinycustomloader::withLoader(
                shiny::plotOutput(ns("plotOut"), height = 500),
                type = "html",
                loader = "loader7"
            )
        ) # end of column 2
    )
}

##' plate_specifications Server Function
##' 
##' @description Server part of the plate specifications module. Allows the 
##' user to specify all the parameters needed to run the backatracking module.
##' 
##' @param input,output,session Internal parameters of {shiny}
##' @param nb_samp_gps Integer, the number of distinct groups in the file
##' @param gp_levels Vector of group levels
##' @param project_name Character, is the name of the user's project
##' @param nb_samples Integer, number of samples in the dataset
##' @importFrom rlang .data
##' @return toReturn, `ReactiValues` object containing:
##' * nb_lines: the number of lines of the plate to be filled
##' * nb_cols: the number of columns of the plate to be filled
##' * nb_plates: the number of plates to fill
##' * forbidden_wells: dataframe containing the wells for forbidden, blank and 
##'   not Random wells.
##' * neighborhood_mod: Character string specifying the spatial constraint.
##' @noRd
mod_plate_specifications_server <- function(
    input, output, session, nb_samp_gps, gp_levels, project_name, nb_samples,
    p_dimensions, forbid_wells, notRandom_wells){
  
    toReturn <- shiny::reactiveValues(
        nb_lines = NULL,
        nb_cols = NULL,
        nb_plates = NULL,
        special_wells = NULL,
        neighborhood_mod = NULL
    )

    p_lines <- shiny::reactive({
      return(p_dimensions()$nb_lines)
    })
    p_cols <- shiny::reactive({
      return(p_dimensions()$nb_cols)
    }) 
    nb_p <- shiny::reactive({
      return(p_dimensions()$nb_plates)
    }) 
    totalNbWells <- shiny::reactive({
        tNbW <- p_lines() * p_cols() * nb_p()
        logging::loginfo("totalNbWells = %d", tNbW, logger = "plate_spec")
        return(tNbW)
    })
    
    output$nb_gps <- shiny::reactive({
        return(nb_samp_gps())
    })

    shiny::outputOptions(output, "nb_gps", suspendWhenHidden = FALSE)
    
    blank_wells <- shiny::reactive({
        shiny::validate(
            shiny::need((p_lines() > 0 & p_cols() > 0),
                        "requires a plate with positive dimensions.")
        )
          if (input$blank_mode != "by_hand") {
            defineBlankCoords(p_lines(),
                              p_cols(),
                              as.character(input$blank_mode),
                              input$start_blank)
        }else{
            return(convertVector2Df(input$hand_select, p_lines(), p_cols(), status = "blank"))
        }
      
    })
    
    wells_to_plot <- shiny::reactive({
        ret <- NULL
        if (is.null(forbid_wells())) {
            nb_f <- 0
        }else{
            nb_f <- nrow(forbid_wells())
        }
        
        if (is.null(blank_wells())) {
            nb_b <- 0
        }else{
            nb_b <- nrow(blank_wells())
        }
        
        if (is.null(notRandom_wells())) {
            nb_nR <- 0
        }else{
            nb_nR <- nrow(notRandom_wells())
        }
        
        shiny::validate(
            shiny::need(nb_samples() <= (totalNbWells()),
                  "The dimensions of the plate are not compatible with the number of
                  samples to be placed. Please increase the number of plates to fill
                  or provide a dataset with fewer samples.")
        )

        ## if there are forbidden wells
        if (!is.null(forbid_wells())) {
            ## if there are blank wells
            if (!is.null(blank_wells())) {
                ## if there are NotRandom wells
                if (!is.null(notRandom_wells())) {
                    shiny::validate(
                        shiny::need(
                            nb_samples() <= (totalNbWells() - (nb_b * nb_p()) - (nb_f * nb_p()) - (nb_nR * nb_p())),
                            "The dimensions of the plate are not compatible with the number 
                            of samples to be placed. Maybe are you specifying to many 
                            forbidden/blanks/notRandom wells.")
                    )
                    ## We put the forbidden wells first because they have priority over
                    ## the blanks ones.
                    result <- base::rbind(forbid_wells(),
                                          blank_wells(),
                                          notRandom_wells())
                    result <- dplyr::distinct(result, .data$Row, .data$Column,
                                              .keep_all = TRUE)
                    ret <- result
                ## If there is no NotRandom wells
                }else{
                    shiny::validate(
                        shiny::need(
                            nb_samples() <= (totalNbWells() - (nb_b * nb_p()) - (nb_f * nb_p())),
                            "The blank mode and/or forbidden wells selected are not 
                            compatible with the plate's dimensions and the number of samples 
                            to be placed. If you want to keep this blank mode, please 
                            increase the number of plates to fill or provide a dataset with 
                            fewer samples. Otherwise, please change the blank mode.")
                    )
                    ## We put the forbidden wells first because they have priority over
                    ## the blanks ones.
                    result <- base::rbind(forbid_wells(), blank_wells())
                    result <- dplyr::distinct(result, .data$Row, .data$Column,
                                              .keep_all = TRUE)
                    ret <- result
                }
            ## if there is no blank wells
            }else{
                ## if there is NotRandom well
                if (!is.null(notRandom_wells())) {
                      shiny::validate(
                          shiny::need(
                              nb_samples() <= (totalNbWells() - (nb_f * nb_p()) - (nb_nR * nb_p()) ),
                              "The dimensions of the plate are not compatible with the number 
                              of samples to be placed. Maybe are you specifying to many 
                              forbidden/notRandom wells."
                        )
                      )
                      result <- base::rbind(forbid_wells(), notRandom_wells())
                      result <- dplyr::distinct(result, .data$Row, .data$Column,
                                                .keep_all = TRUE)
                      ret <- result
                ## if there is no NotRandom well
                }else{
                    shiny::validate(
                      shiny::need(nb_samples() <= (totalNbWells() - (nb_f * nb_p())),
                              "The forbidden wells selected are not compatible with the
                              plate's dimensions and the number of samples to be placed.
                              To solve this issue, please:
                              - decrease the number of forbidden wells
                              - or increase the number of plates to fill
                              - or provide a dataset with fewer samples.")
                    )
                    ret <- forbid_wells()
                }
            }
          
        }else{
            ## if there are blank wells
            if (!is.null(blank_wells())) {
                ## if there are NotRandom wells
                if (!is.null(notRandom_wells())) {
                    shiny::validate(
                        shiny::need(
                            nb_samples() <= (totalNbWells() - (nb_b * nb_p()) - (nb_nR * nb_p()) ),
                            "The dimensions of the plate are not compatible with the number 
                            of samples to be placed. Maybe are you specifying to many 
                            blanks/notRandom wells.")
                    )
                    result <- base::rbind(blank_wells(), notRandom_wells())
                    result <- dplyr::distinct(result, .data$Row, .data$Column,
                                              .keep_all = TRUE)
                    ret <- result
                ## if there is no NotRandom well
                }else{
                    shiny::validate(
                        shiny::need(nb_samples() <= (totalNbWells() - (nb_b*nb_p())),
                              "The blank mode selected is not compatible with the plate's 
                              dimensions and the number of samples to be placed. If you 
                              want to keep this blank mode, please increase the number of
                              plates to fill or provide a dataset with fewer samples.
                              Otherwise, please change the blank mode.")
                    )
                    ret <- blank_wells()
                }
            ## if there is no blank well
            }else{
                ## if there are NotRandom wells
                if (!is.null(notRandom_wells())) {
                    shiny::validate(
                        shiny::need(nb_samples() <= totalNbWells() - (nb_nR*nb_p()),
                        "The dimensions of the plate are not compatible with the number of 
                        samples to be placed. Maybe are you specifying to many notRandom 
                        wells.")
                    )
                    ret <- notRandom_wells()
                ## si pas de NotRandom
                }else{
                    ret <- NULL
                }
            }
        }
        
        return(ret)
    })
    

    
    
    output$plotOut <- shiny::renderPlot({
        ## for the drawMap function to work properly, we must give a number of rows
        ## and columns greater than 0 and give at least an empty dataframe with the
        ## correct column names
        if (p_lines() != 0 & p_cols() != 0) {
            if (is.null(wells_to_plot())) {
                df <- data.frame(lapply(c(NA, NA, NA, NA, NA, NA, NA),
                                        function(...) character(0)),
                                 stringsAsFactors = FALSE)
                colnames(df) <- c("Sample", "Group", "ID", "Well", "Status", "Row",
                                  "Column")
                
                drawMap(df = df,
                        sample_gps = nb_samp_gps(),
                        gp_levels = gp_levels(),
                        plate_lines = p_lines(),
                        plate_cols = p_cols(),
                        project_title = project_name())
            }else{
                drawMap(df = wells_to_plot(),
                        sample_gps = nb_samp_gps(),
                        gp_levels = gp_levels(),
                        plate_lines = p_lines(),
                        plate_cols = p_cols(),
                        project_title = project_name())
            }
        }
    })
    
    nbh_mod <- shiny::reactive({
        nbh_mod <- NULL
        if (input$blank_mode == "by_row") {
            nbh_mod <- input$constraint_row
        }else if (input$blank_mode == "by_column") {
            nbh_mod <- input$constraint_column
        }else if (input$blank_mode == "by_hand") {
            if (nb_samp_gps() > 3) {
                nbh_mod <- input$constraint_by_hand_sup3
            }else{
                nbh_mod <- input$constraint_by_hand_inf3
            }
        }else if (input$blank_mode == "none") {
            if (nb_samp_gps() > 3) {
                nbh_mod <- input$constraint_none_sup3
            }else{
                nbh_mod <- input$constraint_none_inf3
            }
        }else if (input$blank_mode == "checkerboard") {
            nbh_mod <- "none"
        }
        
        return(nbh_mod)
    })
    
    shiny::observe({
        toReturn$nb_lines <- p_lines()
        toReturn$nb_cols <- p_cols()
        toReturn$nb_plates <- nb_p()
        # dataframe which contains the blanks and forbidden wells
        toReturn$special_wells <- wells_to_plot()
        toReturn$neighborhood_mod <- nbh_mod()
    })

    
    return(toReturn)
}
    
## To be copied in the UI
# mod_plate_specifications_ui("plate_specifications_ui_1")
    
## To be copied in the server
# callModule(mod_plate_specifications_server, "plate_specifications_ui_1")
 
