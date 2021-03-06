##' Backtracking Server Function
##'
##' @description Server part of the shiny Module to generate the plate map
##' using backtracking algorithm
##'
##' @param input,output,session internal shiny parameters
##' @param df dataframe of user's data
##' @param max_iter integer, the maximal number of iterations to do
##' @param plate_options reactiveValues object which contains the following fields:
##' - **special_wells**: the special wells not to place randomly on the plate(s)
##' - **nb_lines**: the number of lines on the plate(s)
##' - **nb_cols**: the number of columns on the plate(s)
##' - **nb_plates**: the number plates.
##' - **total_nb_wells**: the total number of wells
##' - **neighborhood_mod**: the spatial constraint.
##'
##' @return reactiveValues object containing the final dataframe to export
##' @noRd
mod_backtracking_server <- function(input, output, session, df, max_iter, plate_options){
    toReturn <- shiny::reactiveValues(d = NULL)
    user_data <- shiny::reactive({
        df$Group <- as.factor(df$Group)
        df$Well <- as.character(NA)
        df$Status <- as.factor("toRandom")
        df$Row <- as.numeric(NA)
        df$Column <- as.numeric(NA)
        return(df)
    })

    ## map is a list of dataframes containing: user data + special wells, ready
    ## to be plotted or/and exported
    map <- shiny::reactive({
        progress <- shiny::Progress$new()
        progress$set(message = "WPM running...", value = 0)
        on.exit(progress$close())
        updateProgress <- function(value = NULL, detail = NULL) {
            if (is.null(value)) {
                value <- progress$getValue()
            }
            progress$set(value = value, detail = detail)
            progress$inc(amount = 1 / max_iter)
        }
        return(backtracking(max_iter = max_iter, user_data = user_data(),
                            wells = plate_options$special_wells,
                            rows = plate_options$nb_lines,
                            columns = plate_options$nb_cols,
                            nb_plates = plate_options$nb_plates,
                            constraint = plate_options$neighborhood_mod,
                            prog = updateProgress)
        )

    })



    ## update objects to return to the server part
    shiny::observe({
        if (is.null(map())) {
            logging::loginfo("map is null so we return NULL")
            # toReturn$final_df <- NULL
            toReturn$d <- NULL
        }else{
            logging::loginfo("map isn't null so we return map")
            # toReturn$final_df <- map()
            toReturn$d <- map()
        }
    })
    return(toReturn)
}
