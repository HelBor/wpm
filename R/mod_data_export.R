##' Data export UI Function
##'
##' @description A shiny Module to make final ouptuts visible and downloadable
##'
##' @param id Internal parameters for shiny.
##'
##' @noRd
mod_data_export_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
            shinydashboard::box(title = shiny::h3("Your dataset"),
                                collapsible = TRUE,
                                width = 12,
                                status = "warning",
                                shinycustomloader::withLoader(
                                    shiny::uiOutput(ns("data_export")),
                                    type = "html",
                                    loader = "loader7"
                                )
                                
            )
        ),
        shiny::fluidRow(
            shinydashboard::box(
                title = shiny::h3("Plate Layout Experiment"),
                width = 12,
                status = "warning",
                shinycustomloader::withLoader(
                    shiny::uiOutput(ns("mapPlot")),
                    type = "html",
                    loader = "loader7"
                )
            )
        )
    )
}

##' Data export Server Function
##' 
##' @description Server part of the shiny Module to make final outputs 
##' downloadable
##' 
##' @param input,output,session internal shiny parameters
##' @param df dataframe of user's data
##' @param max_iter integer, the maximal number of iterations to do
##' @param distinct_sample_gps number of distinct groups in the data
##' @param gp_levels the different group levels names
##' @param plate_opts reactiveValues object containing all the plate options.
##' @param project_name the user's project title
##' 
##' @return toReturn a ReactiveValues object containing the final dataframe 
##' and the ggplot object(s) corresponding to the plate plan(s).
##' @noRd
mod_data_export_server <- function(input, output, session, df, distinct_sample_gps,
                                    gp_levels, plate_opts, project_name){
    ## Dataframe export
    output$data_export <- shiny::renderUI({
        project <- stringr::str_replace_all(string = project_name(),
                                            pattern = " ",
                                            repl = "")
        shiny::column(width = 12,
                      DT::renderDataTable(DT::datatable({df()}, rownames = FALSE)),
                      shiny::downloadHandler(
                          filename = function() {
                              paste("data-", Sys.Date(), "-", project, ".csv", sep = "")
                          },
                          content = function(file) {
                              utils::write.csv2(df(), file, row.names = FALSE,
                                                quote = FALSE)
                          }
                      )
        )
    })
    
    ## Plots export
    map_plot <- shiny::reactive({
        if (!is.null(df())) {
            toPlot = list()
            toPlot <- lapply(X = seq_len(plate_opts$nb_plates),
                             function(x) drawMap(
                                 df = df()[which(df()$Plate == x),],
                                 sample_gps = distinct_sample_gps(),
                                 gp_levels = gp_levels(),
                                 plate_lines = plate_opts$nb_lines,
                                 plate_cols = plate_opts$nb_cols,
                                 project_title = project_name()
                             )
            )
            return(toPlot)
        }
    })
    
    output$mapPlot <- shiny::renderUI({
        lapply(seq_along(map_plot()), function(i){
            shinydashboard::box(
                title = shiny::h4(paste0("Plate ", i)),
                width = 6,
                shiny::renderPlot({map_plot()[[i]]}),
                shiny::downloadHandler(
                    filename = function() {
                        paste("plot", i, ".png", sep = "")
                    },
                    content = function(file) {
                        ggplot2::ggsave(
                            filename = file,
                            plot = map_plot()[[i]],
                            width = 10,
                            height = 7,
                            units = "in"
                        )
                    }
                )
            )
            
        })# end lapply
    })
}

## To be copied in the UI
# mod_backtracking_ui("backtracking_ui_1")

## To be copied in the server
# callModule(mod_backtracking_server, "backtracking_ui_1")
