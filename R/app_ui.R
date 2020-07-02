
## Header elements for the visualization
header <- shinydashboard::dashboardHeader(title = "Well-Plate Maker",
    titleWidth = 215
)



## Sidebar elements for the search visualizations
sidebar <- shinydashboard::dashboardSidebar(
    width = 215,
    shinydashboard::sidebarMenu(
        style = "position: fixed; overflow: visible;width: 200px;",
        shinydashboard::menuItem(
            "Home", tabName = "home", icon = shiny::icon("home")),
        shinydashboard::menuItem(
            "Parameters", tabName = "parameters", icon = shiny::icon("cogs")),
        shinydashboard::menuItem(
            "Results", tabName = "results", icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem(
            "Help", tabName = "help", icon = shiny::icon("info-circle"))
    )
)


##' The application User-Interface
##'
##' @param request Internal parameter for `{shiny}`.
##'     DO NOT REMOVE.
##' @noRd
app_ui <- function(request) {
    shiny::tagList(
        golem_add_external_resources(),
        shinydashboard::dashboardPage(
            header = header,
            sidebar = sidebar,
            body = shinydashboard::dashboardBody(
                shiny::tags$head(
                    shiny::tags$link(rel = "stylesheet", type = "text/css",
                    href = "custom.css")
                ),
                shinydashboard::tabItems(
                    shinydashboard::tabItem(
                        tabName = "home",
                        mod_home_ui("home")
                    ), # end of tabItem 1
                    shinydashboard::tabItem(
                        tabName = "parameters",
                        shiny::h1("Parameters"),
                        mod_data_import_ui("data_import"),
                        shiny::fluidRow(
                            shiny::column(width = 6,
                                shinydashboard::box(
                                    status = "warning",
                                    width = 12,
                                    collapsible = TRUE,
                                    solidHeader = TRUE,
                                    title = shiny::h3("Please choose a Project name"),
                                    shiny::textInput(inputId = "project_title",
                                                     label = NULL,
                                                     value = "",
                                                     placeholder = "my project title")
                                )
                            )
                        ),
                        mod_plate_dimensions_ui("p_dim"),
                        mod_special_wells_ui("special1"), # forbidden wells
                        mod_plate_specifications_ui("plate"),
                        mod_special_wells_ui("special2"), # fixed wells
                        shiny::fluidRow(
                            shiny::column(
                                width = 12,
                                mod_iteration_number_ui("max_iter"),
                                shinyWidgets::useSweetAlert(),
                                shinyWidgets::actionBttn(
                                    inputId = "start_WPM_Btn",
                                    label = "Start WPM",
                                    icon = shiny::icon("play"),
                                    color = "warning",
                                    style = "unite"
                                )

                            )# end of column
                        ) # end of fluiRow 3
                    ),# end of tabItem 2
                    shinydashboard::tabItem(
                        tabName = "results",
                        shiny::h1("Your results"),
                        mod_data_export_ui("data_export")
                    ), # end of tabItem 3
                    shinydashboard::tabItem(
                        tabName = "help",
                        mod_help_ui("help")
                    )# end of tabItem 4
                ) # end of tabItems
            ),
            skin = "yellow"))
}

##' Add external Resources to the Application
##'
##' This function is internally used to add external
##' resources inside the Shiny application.
##'
##' @noRd
golem_add_external_resources <- function(){
    golem::add_resource_path(
        'www', system.file('app/www', package = 'wpm')
    )

    shiny::tags$head(
        golem::favicon(),
        shiny::tags$link(rel = "stylesheet",
                        type = "text/css",
                        href = "www/css/custom.css"),
        golem::bundle_resources(
            path = app_sys('app/www'), app_title = 'wpm'
        )
    )
}
