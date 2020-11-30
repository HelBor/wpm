mod_data_import_ui <- function(id){
    ns <- shiny::NS(id)

    shiny::fluidRow(
        # inputs part
        shiny::column(width = 6,
            shinydashboard::box(
                status = "warning",
                width = 12,
                collapsible = TRUE,
                solidHeader = F,
                title = shiny::h3("Upload the dataset"),

                shinydashboard::tabBox(
                    title = "",
                    id = ns("upload_tabset"),
                    side = "left",
                    width = 12,
                    selected = "user_csv",
                    shiny::tabPanel(
                        title = "Upload a file",
                        value = "user_csv",
                        icon = shiny::icon("file-upload"),
                        shiny::column(width = 12,
                                      mod_data_import_file_ui(ns("csv_file"))
                        )
                    ),
                    shiny::tabPanel(
                        title = "Load the demo dataset",
                        value = "demo",
                        icon = shiny::icon("database"),
                        shiny::column(width = 12,
                                      mod_data_import_demo_ui(ns("demo"))
                        )

                    )
                )
            )
        ),
        # output part
        shiny::column(width = 6,
            shiny::fluidRow(
                shiny::column(
                    width = 7,
                    style='padding:0px;margin:0px;',
                    shinydashboard::box(style = "overflow-x: scroll;",
                        title = shiny::h3("Check that your file is correctly read by WPM"),
                        solidHeader = F, collapsible = TRUE,
                        width = 12, status = "warning",
                        DT::dataTableOutput(ns("default_table")),
                        shiny::textOutput(ns("default_text"))
                    )),
                shiny::column(
                    width = 5,
                    style = 'padding:0px;margin:0px;',
                    shinydashboard::box(style = "overflow-x: scroll;",
                        title = shiny::h3("Preview output template"),
                        solidHeader = F, collapsible = TRUE,
                        width = 12, status = "warning",
                        DT::dataTableOutput(ns("wpm_table"))
                    ),
                    shinydashboard::valueBoxOutput(ns("nb_ech"), width = 6),
                    shinydashboard::valueBoxOutput(ns("nb_gp"), width = 6)
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
            file_mod <- mod_data_import_file_server("csv_file")
            # complete here if new module of import

            df <- shiny::reactive({
                if(input$upload_tabset == "demo" ){
                    return(demo_mod$df)

                }else{
                    return(file_mod$df)

                }
                # complete here if new module of import
            })



            df_wpm <- shiny::reactive({
                if(input$upload_tabset == "demo" ){
                    return(demo_mod$df_wpm)

                }else{
                    return(file_mod$df_wpm)

                }
                # complete here if new module of import
            })




            output$nb_ech <- shinydashboard::renderValueBox({
                if (is.null(df_wpm())) {
                    shinydashboard::valueBox(
                        value = 0 ,
                        subtitle = "Total number of samples to place",
                        color = "teal")
                }else{
                    shinydashboard::valueBox(
                        value = nrow(df_wpm()) ,
                        subtitle = "Total number of samples to place",
                        icon = shiny::icon("list"),
                        color = "teal")
                }
            })

            ## Vector containing the different group names
            gp_levels <- shiny::reactive({
                nb <- NULL
                if (is.null(df_wpm())) {
                    nb <- 0
                }else if ("Group" %in% colnames(df_wpm())) {
                    nb <- unique(df_wpm()$Group)
                }
                return(nb)
            })

            ## The number of distinct groups in the file
            distinct_gps <- shiny::reactive({
                d_gp <- NULL
                if (is.null(df_wpm())) {
                    d_gp <- 0
                }else if ("Group" %in% colnames(df_wpm())) {
                    d_gp <- length(unique(df_wpm()$Group))
                }
                shiny::validate(
                    shiny::need(d_gp <= 12,
                                message = "The number of separate groups must not
                        exceed 12.")
                )

                return(d_gp)
            })
            # the number of samples in the dataset
            nb_s <- shiny::reactive({
                if (is.null(df_wpm())) {
                    nb <- 0
                }else{
                    nb <- nrow(df_wpm())
                }
                return(nb)
            })

            output$nb_gp <- shinydashboard::renderValueBox({
                shinydashboard::valueBox(
                    value = distinct_gps(),
                    subtitle = "Total number of distinct groups",
                    icon = shiny::icon("layer-group"),
                    color = "teal")
            })
            shiny::outputOptions(output, "nb_gp", suspendWhenHidden = FALSE)





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


            output$wpm_table <- DT::renderDataTable(
                if(!is.null(df_wpm())){
                    if(methods::is(df_wpm(), "data.frame")){
                        DT::datatable({df_wpm()},
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
                #
                # print("data import : on est dans le observe du toReturn")
                # logging::loginfo("nb_s: %s", nb_s())
                # logging::loginfo("distinct_gps: %s", distinct_gps())
                # logging::loginfo("gp_levels: %s", gp_levels())
                #
                # print("-------------------------------------------")
                toReturn$df <- df_wpm()
                toReturn$distinct_gps <- distinct_gps()
                toReturn$gp_levels <- gp_levels()
                toReturn$nb_samples <- nb_s()
            })


            return(toReturn)
        }
    )
}

