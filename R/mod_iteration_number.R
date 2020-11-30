##' UI part for module maximum number of iterations
##'
##' @description A shiny Module for the number of iterations parameter
##' @param id Internal parameter for {shiny}.
##' @noRd
mod_iteration_number_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
            shiny::column(
                width = 6,
                shinydashboard::box(
                    status = "warning",
                    width = 12,
                    collapsible = TRUE,
                    solidHeader = F,
                    title = shiny::h3("Number of iterations"),
                    shiny::h4(
                        "Please specify the maximum number of iterations
                                    that WPM can perform. Default value is 20."),
                    shinyWidgets::knobInput(
                        inputId = "nb_iter",
                        label = NULL,
                        value = 20,
                        min = 0,
                        width = 80,
                        height = 80,
                        displayPrevious = TRUE,
                        lineCap = "round",
                        fgColor = "#f0ad4e",
                        inputColor = "#f0ad4e"
                    )
                ) # end of box
            )
        )
    )
}

##'  Server part for module maximum number of iterations
##'
##' @description server part of the number of iterations module.
##' @param input,output,session Internal shiny parameters
##' @noRd
mod_iteration_number_server <- function(input, output, session){

}
