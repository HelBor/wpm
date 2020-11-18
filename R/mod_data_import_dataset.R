# module d'importation du dataset test présent dans le package wpm
mod_data_import_dataset_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
    shiny::fluidRow(
        shiny::column(width = 6,
            shiny::h4("Please select the grouping variable"),
            shinyWidgets::pickerInput(
                inputId = ns("group_picker"),
                choices = NULL,
                selected = NULL
            )
        )

    )

}




mod_data_import_dataset_server <- function(input, output, session){

    toReturn <- shiny::reactiveValues(
        df_user = NULL,
        df_wpm = NULL
    )
    df_user <- utils::data("data_test",package = "wpm")


    print(class(df_user))
    print(df_user)
    # # this part updates the picker input when the user gives a file and modifies
    # #  some other parameters. This picker input allows the user to specify the
    # #  grouping variable
    # shiny::observeEvent({
    #     df_user
    # }, {
    #     shinyWidgets::updatePickerInput(session = session, "group_picker",
    #                                     choices = c("none",colnames(df_user)))
    # })


    df_wpm <- shiny::reactive({
        if(!is.null(df_user)){
            reshapeDataframe(df_user, input$group_picker)
        }

    })

    shiny::observe({
        toReturn$df_user <- df_user
        toReturn$df_wpm <- df_wpm()
    })
    return(toReturn)



}
