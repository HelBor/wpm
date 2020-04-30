
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
                                 icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem("Help",
                                 tabName = "help",
                                 icon = shiny::icon("hands-helping"))
    )
)



##' Body elements for the search visualizations.
##' @importFrom DT dataTableOutput
body <- shinydashboard::dashboardBody(
    shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    shinydashboard::tabItems(
        shinydashboard::tabItem(
            tabName = "home",

            shiny::fluidRow(
              shiny::div(img(src = 'images/wpm_logo.png', width = 300),
                         style = "text-align:center;"),
              shiny::div(img(src = 'images/wpm_name.png', width = 300),
                         style = "text-align:center;")
            ),

            shiny::fluidRow(

                shiny::column(
                    width = 9,
                    shinydashboard::box(
                        width = 12,
                        status = "warning",
                        shiny::h1("Welcome to Well Plate Maker!"),
                        shiny::div("WPM is a shiny application (web-based UI)
                        allowing you to create a plan of well-plate for your
                        experiments by controlling batch effects.",br(),br(),
                        "Indeed, the placement of samples on a plate can raise
                        questions when we want to take into account batch effects
                        (because a placement on a plate is a technical source of
                        variation), which may confound the discovery of real
                        biological variation. The question therefore arises
                        mainly when the samples are divided into different
                        groups (here, we speak of a group to define a category
                        of the factor to be studied. Eg: different treatments to
                         compare, different stages of development, etc.)",
                        br(),br(),
                        "The plate plan is built using a backtracking-inspired
                        algorithm with some specific spatial constraints which
                        can be chosen by the user. The plate is filled randomly
                        (and not linearly), i.e. the plate is not filled from
                        left to right (or from top to bottom, etc.). This avoids
                        having to end up with a checkerboard plate plan when the
                        numbers in the groups are unbalanced (which would
                        correspond to a form of batch effect).",
                        style = "font-size:18px")
                    )# end of box
                ),# end column 1
                shiny::column(
                    width = 3,
                    shinydashboard::valueBox(
                        value = "0.99.0",
                        subtitle = "R package version",
                        width = 12,
                        color = "teal",
                        icon = shiny::icon("box-open")
                    )

                ) # end column 2
            ), # end fluidRow 1
            shiny::fluidRow(
                shiny::column(
                    width = 9,
                    shinydashboard::box(
                        width = 12,
                        status = "warning",
                        shiny::h1("Citing our work"),
                        shiny::div("If you use it, please cite the following
                                   reference:",
                                   style = "font-size:18px"
                                   )
                    )
                ) # end column 1
            ) # end fluidRow 2
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
                        shiny::h4("Please use .csv format. \n File must contain
                        2 columns: samples in the first one, and group number
                                  in the second one."),
                        csvFileInput("datafile"),
                        #-----------------------------------------------------------
                        shiny::hr(),
                        shiny::h4("Please choose a Project name"),
                        shiny::textInput(inputId = "project_title",
                        label = NULL,
                        value = "",
                        placeholder = "my project title")
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

            ## plate specification inputs & outputs
            plateSpecUI("plate"),
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

        shinydashboard::tabItem(
            tabName = "results",
            shiny::h1("Your results"),
            backtrackUI("backtrack")
        ),# end of tabItem 3

        shinydashboard::tabItem(tabName = "help",
            shiny::fluidRow(
                shinydashboard::box(
                    width = 12,
                    status = "warning",
                    shiny::h1("Contact"),
                    shiny::div("A vignette is available in the package explaining
                               how to use WPM in detail.",
                               style = "font-size:18px"),
                    shiny::div("Please contact me if you need any help, or if you
                               want to report a bug or if you wish to make
                               comments/suggestions:",
                               style = "font-size:18px"),
                    br(),
                    shiny::column(
                        width = 4,
                        shinydashboard::valueBox(
                            value = "New issue",
                            subtitle = "https://github.com/HelBor/wpm/issues",
                            width = 12,
                            color = "teal",
                            icon = shiny::icon("github")
                        )

                    ),
                    shiny::column(
                        width = 4,
                        shinydashboard::valueBox(
                            value = "Email",
                            subtitle = "helene.borges@cea.fr",
                            width = 12,
                            color = "teal",
                            icon = shiny::icon("envelope")
                        )
                    )
                ) # end of box
            ) # end of fluiRow

        ) # end of tabItem 4
    ) # end of tabItems
)

ui <- shinydashboard::dashboardPage(header, sidebar, body, skin = "yellow")
