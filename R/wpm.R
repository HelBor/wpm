#' Run the Shiny Application of Well Plate Maker
#'
#' @param ... A series of options to be used inside the app.
#' @return a shiny application object with golem options
#' @export
#' @examples
#' wpm()
wpm <- function(...) {
    golem::with_golem_options(
        app = shiny::shinyApp(
            ui = app_ui, 
            server = app_server
        ), 
        golem_opts = list(...)
    )
}
 
