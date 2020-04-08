##' Well-Plate Maker : a GUI for building well-plates plans.
##'
##' @title wpm
##' @return launches the WPM app in a new window in the default browser
##' @author Helene  Borges
##' @examples
##' wpm()
##' @export
print(paste0("(wpm.R-1) working directory: ", getwd()))
wpm <- function(){
  require(shiny)
  print(paste0("(wpm.R-2) working directory: ", getwd()))
  # G <- .GlobalEnv
  print(paste0("(wpm.R-3) working directory: ", getwd()))
  if (interactive()){
    print(paste0("(wpm.R-4) working directory: ", getwd()))
    options(shiny.maxRequestSize=1024^3)
    print(paste0("(wpm.R-5) working directory: ", getwd()))
    a=shiny::runApp(system.file("wpmApp",package="wpm"),
                    launch.browser = TRUE)
    print(paste0("(wpm.R-6) working directory: ", getwd()))
    return(invisible(a))
  }
  print(paste0("(wpm.R-7) working directory: ", getwd()))
  return (NULL)

}
print(paste0("(wpm.R) working directory: ", getwd()))