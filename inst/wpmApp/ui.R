# Header elements for the visualization
header <- dashboardHeader(title="Well-Plate Maker",
                          titleWidth = 250
)



# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(
  width = 150,
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Parameters", tabName = "parameters", icon = icon("cogs")),
    menuItem("Results", tabName = "results", icon = icon("chart-bar")),
    menuItem("Export", tabName = "export", icon = icon("download"))
  )
)



#Body elements for the search visualizations.
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h2("Welcome to Well Plate Maker!"),
            div(""),
            br(),
            h3("1 - Fill the Parameters section"),
            h3("2 - Check the Results section"),
            h3("3 - Export your results if everything is ok")
    ),
    tabItem(tabName = "parameters",
            h2("Parameters"),

            # Input File section
            fluidRow(
              column(width=6,
                box(status="warning",
                    width = 12,
                    title=h3("1 - Upload dataset"),
                    h4("Please use .csv format. \n File must
                                 contain 2 columns: samples in the first one,
                                 and group number in the second one."),
                    csvFileInput("datafile"
                                 ),
                    hr(),
                    h4("Please choose a Project name for the plot title"),
                    textInput(inputId = "project_title",
                              label = NULL,
                              value = "",
                              placeholder = "my project title")
                )
              ),
              column(width=6,
                box(title = h3("Your dataset"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    status = "warning",
                    dataTableOutput("table")
                    ),
                fluidRow(valueBoxOutput("nb_ech", width=3),
                         valueBoxOutput("nb_gp", width=3)
                         )
              )
            ), # end of fluidRow 1: input file

            # plate specification inputs & outputs
            plateSpecUI("plate"),

            fluidRow(
              column(width=6,
                     box(status="warning",
                         width = 12,
                         title=h3("5 - Number of iterations"),
                         div("Please specify the maximum number of iterations
                             that WPM can perform. Default value is 20."),
                         knobInput(
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
                         div(
                           useSweetAlert(),
                           actionBttn(inputId="start_WPM_Btn",
                                      label = "Start WPM",
                                      icon = icon("play"),
                                      color = "warning",
                                      style = "unite"
                                )
                            ),
                         div(
                           withLoader(
                            textOutput("pressedBtn"),
                            type = "html",
                            loader = "loader3"
                           )
                         )



                     )

                     )# end of column
            ) # end of fluiRow 3: Max iterations for WPM and start WPM button

    ),# end of tabItem 2
    tabItem(tabName = "results",
            h2("Your results"),
            backtrackUI("backtrack")
    ),# end of tabItem 3
    tabItem(tabName = "export",
            h2("Download your data here"),
            exportUI("export")

    )
  )
)




ui <- dashboardPage(header, sidebar, body, skin = "yellow")