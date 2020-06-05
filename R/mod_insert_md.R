##' Server part of the module inserting markdown files in the app
##' @param id internal parameter for shiny
##' @noRd
mod_insert_md_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::uiOutput(ns("insertMD"))
}


##' Server part of the module inserting markdown files in the app
##' @param input,output,session internal parameters for shiny
##' @param path_to_file character, path to the markdown file to load
##' @noRd
mod_insert_md_server <- function(input, output, session, path_to_file){
    output$insertMD <- shiny::renderUI({
        tryCatch(
            {
                shiny::includeMarkdown(app_sys(path_to_file))
            }
            , warning = function(w) {
                
                shiny::tags$p("Path to file not found")
                
            }, error = function(e) {
                logging::loginfo("Error : in mod_insert_md :%s",
                                 conditionMessage(e))
            }, finally = {
                #cleanup-code 
            })
        
    })
}
