##' Well-Plate Maker : a GUI for building well-plates plans.
##'
##' @title wpm
##' @return launches the WPM app in a new window in the default browser
##' @author Hélène  Borges
##' @example
##' wpm()
##' @export

wpm <- function(){
  require(shiny)
  G <- .GlobalEnv
  if (interactive()){
    options(shiny.maxRequestSize=1024^3)
    a=shiny::runApp(system.file("wpmApp",package="wpm"),
                    launch.browser = TRUE)
    return(invisible(a))
  } else {return (NULL)}
}