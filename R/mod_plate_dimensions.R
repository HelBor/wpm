##' plate_dimensions UI Function
##'
##' @description A shiny Module allowing to specify the plate dimensions  and
##' number
##'
##' @param id Internal parameters for {shiny}.
##' @noRd
mod_plate_dimensions_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::fluidRow(
        shiny::column(width = 6,
            shinydashboard::box(
                status = "warning",
                width = 12,
                collapsible = TRUE,
                solidHeader = F,
                title = shiny::h3("Plate dimensions"),
                shiny::selectInput(
                    ns("plate_size"),
                    label = NULL,
                    choices = c(
                        "6" = "s6",
                        "24" = "s24",
                        "48" = "s48",
                        "96" = "s96",
                        "384" = "s384",
                        "1536" = "s1536",
                        "custom" = "custom"
                    ),
                    selected = NULL),
                shiny::conditionalPanel(
                    condition = "input.plate_size == 'custom'",
                    shiny::h4("How many lines on your plate?"),
                    shiny::numericInput(
                        ns("plate_lines"),
                        label = NULL,
                        value = 0,
                        min = 0,
                        width = "80px"),
                    shiny::h4("how many columns on your plate?"),
                    shiny::numericInput(
                        ns("plate_cols"),
                        label = NULL,
                        value = 0,
                        min = 0,
                        width = "80px"),
                    ns = ns
                ),
                shiny::h4("How many plates?"),
                shiny::numericInput(
                    ns("no_plates"),
                    label = NULL,
                    value = 1,
                    min = 1,
                    width = "80px")
            )
        ),
        shiny::column(width = 6,
            shiny::fluidRow(
                shinydashboard::infoBoxOutput(
                    ns("warning_plate"),
                    width = 12)
            ),
            shiny::fluidRow(
                shinydashboard::valueBoxOutput(
                    ns("total_nb_wells"),
                    width = 6),
                shinydashboard::valueBoxOutput(
                    ns("nb_plates_to_fill"),
                    width = 6)
            ),
            shiny::fluidRow(
                shiny::uiOutput(ns("dim_check"))
            )
        ) # end of column 2
    )
}


##' plate_dimensions Server Function
##'
##' @description Server part of the plate_dimensions module. Allows the
##' user to specify the plate dimensions and their number.
##'
##' @param input,output,session internal parameters of shiny
##' @param nb_samples a reactive object corresponding to the total number of
##' samples
##'
##' @noRd
mod_plate_dimensions_server <- function(input, output, session, nb_samples){

    toReturn <- shiny::reactiveValues(
        nb_lines = NULL,
        nb_cols = NULL,
        nb_plates = NULL
    )

    p_lines <- shiny::reactive({
        nb <- NULL
        switch(input$plate_size,
               NULL = {nb <- 0},
               "s6" = {nb <- 2},
               "s24" = {nb <- 4},
               "s48" = {nb <- 6},
               "s96" = {nb <- 8},
               "s384" = {nb <- 16},
               "s1536" = {nb <- 32},
               "custom" = {nb <- input$plate_lines}
        )
        return(nb)
    })

    p_cols <- shiny::reactive({
        nb <- NULL
        switch(input$plate_size,
               NULL = {nb <- 0},
               "s6" = {nb <- 3},
               "s24" = {nb <- 6},
               "s48" = {nb <- 8},
               "s96" = {nb <- 12},
               "s384" = {nb <- 24},
               "s1536" = {nb <- 48},
               "custom" = {nb <- input$plate_cols}
        )
        return(nb)
    })

    nb_p <- shiny::reactive({
        if (is.na(input$no_plates)) {
            return(0)
        }else{
            return(input$no_plates)
        }
    })

    totalNbWells <- shiny::reactive({
        tNbW <- p_lines() * p_cols() * nb_p()
        return(tNbW)
    })

    output$total_nb_wells <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = totalNbWells(),
            subtitle = "Number of fillable wells",
            icon = shiny::icon("vials"),
            color = "teal")
    })

    output$nb_plates_to_fill <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = as.numeric(nb_p()),
            subtitle = "Number of plates to fill",
            icon = shiny::icon("dice-four"),
            color = "teal")
    })

    output$warning_plate <- shinydashboard::renderInfoBox({
        shinydashboard::infoBox(
            title = shiny::HTML(
                paste("We assume that all the plates to be filled have the same
                  dimensions.", shiny::br(), "Also, when you want to generate more
                  than 1 plate,", shiny::br(), "WPM uses balanced group workforces
                  to distribute the samples within the plates.")),
            icon = shiny::icon("info-circle"),
            color = "yellow",
            fill = TRUE)
    })

    output$dim_check <- shiny::renderUI({
        logging::loginfo("nb_samples %s", nb_samples())
        if(nb_samples() > totalNbWells()){
            shinydashboard::infoBox(
                title = "selected dimensions are not compatible with the number of samples.",
                color = "red",
                width = 12,
                icon = shiny::icon("times")
            )
        }else{
            shinydashboard::infoBox(
                title = "Plate compatible with samples.",
                color = "green",
                width = 12,
                icon = shiny::icon("check-circle")
            )
        }
    })



    shiny::observe({
        toReturn$nb_lines <- p_lines()
        toReturn$nb_cols <- p_cols()
        toReturn$nb_plates <- nb_p()
    })
    return(toReturn)
}
