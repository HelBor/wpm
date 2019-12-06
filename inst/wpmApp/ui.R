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
                box(status="primary",
                    width = 12,
                    title=h3("1 - Upload dataset"),
                    csvFileInput("datafile", "Please use .csv format. \n File must
                                 contain 2 columns: samples in the first one,
                                 and group number in the second one."
                                 )
                )
              ),
              column(width=6,
                box(title = h3("Your dataset"),
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    status = "primary",
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

            ), # end of fluiRow 4: forbidden wells
            fluidRow(

            ), # end of fluiRow 5: neighborhood contraints
            fluidRow(

            ) # end of fluidRow 6: start the plate maker
    ),# end of tabItem 2
    tabItem(tabName = "results",
            h2("Your results")
    ),# end of tabItem 3
    tabItem(tabName = "export",
            h2("Export your results")

    )
  )
)




ui <- dashboardPage(header, sidebar, body, skin = "yellow")