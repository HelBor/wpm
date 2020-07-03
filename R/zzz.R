.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        cli::cli_h1("This is the Well-Plate Maker package."),
        cli::cli_text("If you want to use the shiny application, then simply run: {.fn wpm}.",
                "If you want to see the Tutorial vignette and use WPM in command line, run: {.code browseVignettes('wpm')}.")

    )
}
