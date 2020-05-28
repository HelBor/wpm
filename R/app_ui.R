
## Header elements for the visualization
header <- shinydashboard::dashboardHeader(title = "Well-Plate Maker",
                                          titleWidth = 200
)



## Sidebar elements for the search visualizations
sidebar <- shinydashboard::dashboardSidebar(
  width = 200,
  
  shinydashboard::sidebarMenu(
    style = "position: fixed; overflow: visible;width: 200px;",
    shinydashboard::menuItem("Home",
                             tabName = "home",
                             icon = shiny::icon("home")),
    shinydashboard::menuItem("Parameters",
                             tabName = "parameters",
                             icon = shiny::icon("cogs")),
    shinydashboard::menuItem("Results",
                             tabName = "results",
                             icon = shiny::icon("chart-bar"))
  )
)


#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @noRd
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
            mod_home_ui()
          ), # end of tabItem 1
          
          shinydashboard::tabItem(
            tabName = "parameters",
            shiny::h1("Parameters"),
            # Input File section
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shinydashboard::box(
                  status = "warning",
                  width = 12,
                  solidHeader = TRUE,
                  title = shiny::h3("1 - Upload dataset"),
                  shiny::fluidRow(
                    shiny::column(
                      width = 10,
                      shiny::h4("Please use CSV format.")),
                    shiny::column(
                      width = 2,
                      align = "right",
                      shinyWidgets::dropdownButton(
                        shiny::h4("File format"),
                        shiny::div("File must be in CSV format.
                        It must contain at least the field samples names.
                        If samples pertain to different groups, you can specify 
                        a second column containing the group names.
                        See the vignette for more details."),
                        icon = shiny::icon("info-circle"),
                        tooltip = shinyWidgets::tooltipOptions(
                          title = "Help"),
                        status = "warning",
                        size = "sm",
                        width = "350px"
                      )
                    )
                  ),
                  mod_data_import_ui("data_import"),
                ) # end of box upload dataset
              ), # end column 1
              shiny::column(
                width = 6,
                shiny::fluidRow(
                  shinydashboard::box(title = shiny::h3("Your dataset"),
                                      solidHeader = TRUE,
                                      collapsible = TRUE,
                                      width = 6,
                                      status = "warning",
                                      DT::dataTableOutput("table")
                  ),
                  shinydashboard::valueBoxOutput("nb_ech", width = 3),
                  shinydashboard::valueBoxOutput("nb_gp", width = 3)
                )
              ) # end column 2
            ), # end of fluidRow 1: input file
            
            ## plate dimensions
            mod_plate_dimensions_ui("p_dim"),
            mod_special_wells_ui("special1"),
            ## plate specification inputs & outputs
            mod_plate_specifications_ui("plate"),
            mod_special_wells_ui("special2"),
            ## Iteration option and submit button
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shinydashboard::box(
                  status = "warning",
                  width = 12,
                  solidHeader = TRUE,
                  title = shiny::h3("6 - Number of iterations"),
                  shiny::h4("Please specify the maximum number of iterations
                            that WPM can perform. Default value is 20."),
                  shinyWidgets::knobInput(
                    inputId = "nb_iter",
                    label = NULL,
                    value = 20,
                    min = 0,
                    width = 80,
                    height = 80,
                    displayPrevious = TRUE,
                    lineCap = "round",
                    fgColor = "#f0ad4e",
                    inputColor = "#f0ad4e"
                  ),
                  shiny::div(
                    shinyWidgets::useSweetAlert(),
                    shinyWidgets::actionBttn(inputId = "start_WPM_Btn",
                                             label = "Start WPM",
                                             icon = shiny::icon("play"),
                                             color = "warning",
                                             style = "unite"
                    )
                  )
                ) # end of box
              )# end of column
            ) # end of fluiRow 3: Max iterations for WPM and start WPM button
  
          ),# end of tabItem 2
          #
          shinydashboard::tabItem(
            tabName = "results",
            shiny::h1("Your results"),
            mod_data_export_ui("data_export")
          )# end of tabItem 3

        ) # end of tabItems
      ),
      skin = "yellow")
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @noRd
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
      path = app_sys('app/www'),
      app_title = 'wpm'
    )
  )
}
