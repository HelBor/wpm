mod_home_ui <- function(id){
    shiny::tagList(
        shiny::fluidRow(
            shiny::div(shiny::img(src = 'www/images/wpm_logo.png',
                                  width = 300),
                       style = "text-align:center;"),
            shiny::div(shiny::img(src = 'www/images/wpm_name.png',
                                  width = 300),
                       style = "text-align:center;")
        ),
        shiny::fluidRow(
            
            shiny::column(
                width = 9,
                shinydashboard::box(
                    width = 12,
                    status = "warning",
                    shiny::h1("Welcome to Well Plate Maker!"),
                    shiny::div(
                        "WPM is a shiny application (web-based UI) allowing you to 
                  create a plan of well-plate for your experiments by 
                  controlling batch effects.", shiny::br(), shiny::br(), 
                        "Indeed, the placement of samples on a plate can raise 
                  questions when we want to take into account batch effects 
                  (because a placement on a plate is a technical source of 
                  variation), which may confound the discovery of real 
                  biological variation. The question therefore arises mainly 
                  when the samples are divided into different groups (here, we 
                  speak of a group to define a category of the factor to be 
                  studied. Eg: different treatments to compare, different 
                  stages of development, etc.)", shiny::br(), shiny::br(), 
                        "The plate plan is built using a backtracking-inspired 
                  algorithm with some specific spatial constraints which can 
                  be chosen by the user. The plate is filled randomly (and not
                  linearly), i.e. the plate is not filled from left to right 
                  (or from top to bottom, etc.). This avoids having to end up 
                  with a checkerboard plate plan when the numbers in the groups
                  are unbalanced (which would correspond to a form of batch 
                  effect).", style = "font-size:18px")
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
        ), # end fluidRow 2
        shiny::fluidRow(
            shiny::column(
                width = 12,
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
        ), # end fluidRow 3
        shiny::fluidRow(
            shiny::column(
                width = 12,
                shinydashboard::box(
                    width = 12,
                    status = "warning",
                    shiny::h1("Contact"),
                    shiny::div(
                        "A vignette is available in the package explaining how to use 
                WPM in detail. Simply enter",
                        shiny::code("browseVignettes(\"wpm\")"), "in the console.",
                        style = "font-size:18px"),
                    shiny::div("Please contact me if you need any help, or if you 
                want to report a bug or if you wish to make comments or 
                           suggestions:", style = "font-size:18px"),
                    shiny::br(),
                    shiny::column(
                        width = 6,
                        shinydashboard::valueBox(
                            value = "New issue",
                            subtitle = "https://github.com/HelBor/wpm/issues",
                            href = "https://github.com/HelBor/wpm/issues",
                            width = 12,
                            color = "teal",
                            icon = shiny::icon("github")
                        )
                        
                    ),
                    shiny::column(
                        width = 6,
                        shinydashboard::valueBox(
                            value = "Email",
                            subtitle = "helene.borges@cea.fr",
                            width = 12,
                            color = "teal",
                            icon = shiny::icon("envelope")
                        )
                    )
                ) # end of box
            ) # end column
        ) # end of fluiRow 4
    )
}

mod_home_server <- function(input,output, session){
    
}
