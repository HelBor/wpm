##' @noRd
mod_plate_dimensions_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::fluidRow(
        shiny::column(width = 6,
            shinydashboard::box(
                status = "warning",
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
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
            )
        ) # end of column 2
    )
}

##' @noRd
mod_plate_dimensions_server <- function(input, output, session){

    toReturn <- shiny::reactiveValues(
        nb_lines = NULL,
        nb_cols = NULL,
        nb_plates = NULL
    )

    p_lines <- shiny::reactive({
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

    shiny::observe({
        toReturn$nb_lines <- p_lines()
        toReturn$nb_cols <- p_cols()
        toReturn$nb_plates <- nb_p()
    })
    return(toReturn)
}
