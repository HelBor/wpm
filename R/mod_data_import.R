mod_data_import_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::fluidRow(
        # inputs part
        shiny::column(width = 6,
            shinydashboard::tabBox(
                title = "Upload dataset",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = ns("upload_tabset"),
                side = "right",
                width = 12,
                selected = "demo",
                shiny::tabPanel(
                    title = "Load the demo dataset",
                    value = "demo",
                    icon = shiny::icon("database"),
                    mod_data_import_demo_ui(ns("demo"))
                ),
                shiny::tabPanel(
                    title = "Upload a file",
                    value = "user",
                    icon = shiny::icon("file-upload"),
                    mod_data_import_file_ui(ns("user_file"))
                )
            )
        ),
        # output part
        shiny::column(width=6,
            shiny::fluidRow(
                shinydashboard::box(
                    title = shiny::h3("Preview output template"),
                    solidHeader = TRUE, collapsible = TRUE,
                    width = 12, status = "warning",
                    DT::dataTableOutput(ns("default_table")),
                    shiny::textOutput(ns("default_text"))
                )

            )
        )
    )

}


mod_data_import_server <- function(id){
    shiny::moduleServer(
        id,
        function(input, output, session) {
            
            ns <- session$ns
            
            toReturn <- shiny::reactiveValues(
                df = NULL,
                distinct_gps = NULL,
                gp_levels = NULL,
                nb_samples = 0
            )
            

            demo_mod <- mod_data_import_demo_server("demo")
            file_mod <- mod_data_import_file_server("user_file")
                
            df <- shiny::reactive({
                if(input$upload_tabset == "demo" ){
                    return(demo_mod$df)

                }else{
                    return(file_mod$df)
                    
                }
            })


            output$default_table <- DT::renderDataTable(
                if(!is.null(df())){
                    if(methods::is(df(), "data.frame")){
                        DT::datatable({df()},
                                      rownames = FALSE,
                                      options = list(columnDefs = list(list(className = 'dt-center', targets ="_all")),
                                                     pageLength = 5)
                        )
                    }
                }
            )
            
            output$default_text <- shiny::renderText({
                if(methods::is(df(), "character")){
                    df()
                }
            })
            

                
            shiny::observe({
                toReturn$df <- df()
                # toReturn$distinct_gps <- distinct_gps()
                # toReturn$gp_levels <- gp_levels()
                # toReturn$nb_samples <- nb_s()
            })

            
            return(toReturn)
        }
    )
}

