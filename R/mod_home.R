##' Home page UI Function
##'
##' @description UI part of a shiny Module to build the Home page application
##'
##' @param id Internal parameter for shiny.
##'
##' @noRd
mod_home_ui <- function(id){
    ns <- shiny::NS(id)
    shiny::tagList(
        shiny::fluidRow(
            shiny::div(shiny::img(src = 'www/images/wpm_logo.png', width = 300),
                       style = "text-align:center;"),
            shiny::div(shiny::img(src = 'www/images/wpm_name.png', width = 300),
                       style = "text-align:center;")
        ),
        shiny::fluidRow(
            shiny::column(width = 9,
                shinydashboard::box(width = 12, status = "warning",
                    mod_insert_md_ui(ns("homeMd"))
                )# end of box
            ),# end column 1
            shiny::column(width = 3,
                shinydashboard::valueBoxOutput(ns("wpmVersion"), width = 12)
            ) # end column 2
        ), # end fluidRow 2
        shiny::fluidRow(
            shiny::column(width = 9,
                shinydashboard::box(width = 12, status = "warning",
                    mod_insert_md_ui(ns("citeUsMd"))
                )
            ) # end column 1
        ), # end fluidRow 3
        shiny::fluidRow(
            shiny::column(width = 9,
                shinydashboard::box(width = 12, status = "warning",
                    mod_insert_md_ui(ns("contactMd")),
                    shiny::column(width = 6,
                        shinydashboard::valueBoxOutput(ns("newIssue"),
                                                       width = 12)
                    ),
                    shiny::column(width = 6,
                        shinydashboard::valueBoxOutput(ns("email"), width = 12)
                    )
                ) # end of box
            ) # end column
        ) # end of fluiRow 4
    )
}

##' Home page Server Function
##' @description Server part of a shiny Module to build the Home page application
##' @noRd
mod_home_server <- function(input,output, session){
    
    # to add the welcome message
    shiny::callModule(mod_insert_md_server, "homeMd", "app/md/home.md")
    shiny::callModule(mod_insert_md_server, "citeUsMd", "app/md/cite_us.md")
    # to add the contact text
    shiny::callModule(mod_insert_md_server, "contactMd", "app/md/contact.md")
    output$wpmVersion <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = as.character(utils::packageVersion("wpm")),
            width = 12,
            subtitle = "R package version",
            icon = shiny::icon("box-open"),
            color = "teal")
    })
    output$newIssue <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = "New issue",
            width = 12,
            subtitle = "https://github.com/HelBor/wpm/issues",
            href = "https://github.com/HelBor/wpm/issues",
            icon = shiny::icon("github"),
            color = "teal")
    })
    output$email <- shinydashboard::renderValueBox({
        shinydashboard::valueBox(
            value = "email",
            width = 12,
            subtitle = "helene.borges@cea.fr",
            icon = shiny::icon("envelope"),
            color = "teal")
    })
}
