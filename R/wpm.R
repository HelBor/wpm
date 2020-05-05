##' Well-Plate Maker : a GUI for building well-plates plans.
##' For more details on how to use WPM, please see the vignette using
##' browseVignette("wpm")
##' @title wpm
##' @return launches the WPM app in a new window in the default browser
##' @author Helene  Borges
##' @examples
##' wpm()
##' @export

wpm <- function() {
    if (interactive()) {
        options(shiny.maxRequestSize = 1024^3)
        a <- shiny::runApp(system.file("wpmApp", package = "wpm"),
            launch.browser = TRUE)
        return(invisible(a))
    }
    return(NULL)
}
