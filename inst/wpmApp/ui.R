# Header elements for the visualization
header <- dashboardHeader(title="Well-Plate Maker",
                          titleWidth = 200
)



# Sidebar elements for the search visualizations
sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Parameters", tabName = "parameters", icon = icon("cogs")),
    menuItem("Results", tabName = "results", icon = icon("chart-bar")),
    menuItem("Help", tabName = "help", icon = icon("hands-helping"))
  )
)



#Body elements for the search visualizations.
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(tabName = "home",

            fluidRow(
              div(img(src='images/wpm_logo.png', width = 300), style = "text-align:center;"),
              div(img(src='images/wpm_name.png', width = 300), style = "text-align:center;")
            ),


            fluidRow(

              column(width = 9,
                box(width = 12,
                    status = "warning",
                    h1("Welcome to Well Plate Maker!"),
                    div("WPM is a shiny application (web-based GUI) allowing you to create a
                  plan of well-plate for your experiments by controlling batch effects.",
                        br(),br(),
                        "Indeed, the placement of samples on a plate can raise questions
                  when we want to take into account batch effects (because a placement on a plate is a technical
                  source of variation), which may confound the discovery of real
                  biological variation. The question therefore arises mainly when
                  the samples are divided into different groups (here, we speak
                  of a group to define a category of the factor to be studied.
                  Eg: different treatments to compare, different stages of development, etc.)",
                        br(),br(),
                        "The plate plan is built using a backtracking-inspired algorithm
                  with some specific spatial constraints which can be chosen by the user.
                  The plate is filled randomly (and not linearly), i.e. the
                  plate is not filled from left to right (or from top to bottom,
                  etc.). This avoids having to end up with a checkerboard plate
                  plan when the numbers in the groups are unbalanced (which would
                  correspond to a form of batch effect).",
                        style = "font-size:18px"),

                    # div(img(src = "images/blanks_per_line_plate.png", width = "30%"),
                    #     img(src = "images/blanks_per_column_plate.png", width = "30%"),
                    #     img(src = "images/blanks_by_hand_plate.png", width = "30%"),
                    #     style = "display: inline-block;"
                    # )
                )# end of box
              ),# end column 1
              column(width = 3,
                     valueBox(value = "0.99.0",
                              subtitle = "R package version",
                              width = 12,
                              color = "teal",
                              icon = icon("box-open"))


              ) # end column 2
            ), # end fluidRow 1
            fluidRow(
              column(width =9,
                box(width = 12,
                    status = "warning",
                    h1("Citing our work"),
                    div("If you use it, please cite the following reference:",
                        style = "font-size:18px"

                    )
                )
              )
            )

    ),
    tabItem(tabName = "parameters",
            h1("Parameters"),
            # Input File section
            fluidRow(
              column(width=6,
                box(status="warning",
                    width = 12,
                    solidHeader = TRUE,
                    title=h3("1 - Upload dataset"),
                    h4("Please use .csv format. \n File must
                                 contain 2 columns: samples in the first one,
                                 and group number in the second one."),
                    csvFileInput("datafile"
                                 ),

                    #-------------------------------------------------------------------------
                    hr(),
                    h4("Please choose a Project name"),
                    textInput(inputId = "project_title",
                              label = NULL,
                              value = "",
                              placeholder = "my project title")

                    #-------------------------------------------------------------------------
                )
              ),
              column(width=6,
                fluidRow(
                  box(title = h3("Your dataset"),
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      width = 6,
                      status = "warning",
                      dataTableOutput("table")
                  ),
                  valueBoxOutput("nb_ech", width=3),
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
                         solidHeader = TRUE,
                         title=h3("6 - Number of iterations"),
                         h4("Please specify the maximum number of iterations
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
                             )
                     )

                     )# end of column
            ) # end of fluiRow 3: Max iterations for WPM and start WPM button

    ),# end of tabItem 2
    tabItem(tabName = "results",
            h1("Your results"),
            conditionalPanel(condition = "output.distinct_gps > 1",
                             backtrackUI("backtrack")),
            conditionalPanel(condition = "output.distinct_gps == 1",
                             randomUI("random"))


    ),# end of tabItem 3

    tabItem(tabName = "help",
            fluidRow(
              box(width = 12,
                  status = "warning",
                  h1("Contact"),
                  div("A vignette is available in the package explaining how to use WPM in detail.",
                      style = "font-size:18px"),
                  div("Please contact me if you need any help, or if you want to
                    report a bug or if you wish to make comments/suggestions:",
                      style = "font-size:18px"),
                  br(),
                  column(width = 4,
                         valueBox(value = "New issue",
                                  subtitle = "https://github.com/HelBor/wpm/issues",
                                  width = 12,
                                  color = "teal",
                                  icon = icon("github"))

                  ),
                  column(width = 4,
                         valueBox(value = "Email",
                                  subtitle = "helene.borges@cea.fr",
                                  width = 12,
                                  color = "teal",
                                  icon = icon("envelope"))
                  )
              ) # end of box
            ) # end of fluiRow

    ) # end of tabItem 4
  ) # end of tabItems
)




ui <- dashboardPage(header, sidebar, body, skin = "yellow")