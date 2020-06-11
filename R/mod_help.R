##' Help page UI Function
##'
##' @description UI part of a shiny Module to build the Help page application
##'
##' @param id Internal parameter for shiny.
##'
##' @noRd
mod_help_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
            shinydashboard::box(width = 12,
                status = "warning",
                mod_insert_md_ui(ns("helpMd"))
            )
        )
    )
}

##' Home page Server Function
##' @description Server part of a shiny Module to build the Home page application
##' @noRd
mod_help_server <- function(input,output, session){
    shiny::callModule(mod_insert_md_server, "helpMd", "app/md/help.md")
}
