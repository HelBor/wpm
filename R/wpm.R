##' Run the Shiny Application of Well Plate Maker
##'
##' @param ... A series of options to be used inside the app.
##' @return a shiny application object with golem options
##'
##' @examples
##' if(interactive()) {wpm()}
##' @export
wpm <- function(...) {
    golem::with_golem_options(
        app = shiny::shinyApp(ui = app_ui, server = app_server,
                              options = list("port" = 8000)),
        golem_opts = list(...)
    )
}

