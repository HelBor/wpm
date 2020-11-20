age <- factor(sample(c("20-30","30-40","40-50","50-60","60-70"), 193, replace = TRUE), levels = c("20-30","30-40","40-50","50-60","60-70"))
gender <- factor(sample(c("M","F"), 193, replace = TRUE), levels = c("M","F"))
diabetes <- factor(sample(c(0,1), 193, replace = TRUE), levels = c(0,1))
treatment <- factor(sample(c("Ctrl","A","B","C"), 193, replace = TRUE), levels = c("Ctrl","A","B","C"))



df <- data.frame(
    "samples" = stringr::str_c("sample_",seq_len(193)),
    "age" = age,
    "gender" = gender,
    "treatment" = treatment,
    "diabetes" = diabetes
)



# module d'importation du dataset test présent dans le package wpm
mod_data_import_dataset_ui <- function(id){
    ## Create a namespace function using the provided id
    ns <- shiny::NS(id)
    shiny::fluidRow(
        shiny::column(width = 6,
                      shiny::h4("Please select the grouping variable"),
                      shinyWidgets::pickerInput(
                          inputId = ns("group_picker"),
                          choices = c(NULL,colnames(df)),
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
    
    
    print(head(df))
    
    
    
    
    df_wpm <- shiny::reactive({
        logging::loginfo("input$group_picker = %s", input$group_picker)
        if(!is.null(df)){
            if (is.null(input$group_picker)) {
                out <- data.frame(Sample = df$samples, Group = as.factor(1))
            }else{
                # check if user enter an existing field
                if (input$group_picker %in% colnames(df)) {
                    out <- data.frame(Sample = df$samples,
                                      Group = as.factor(df[[input$group_picker]]))
                }
            }
            out$Sample <- as.character(out$Sample)
            out$ID <- seq_len(nrow(out))
        }
        return(out)
    })
    
    print(head(df_wpm()))
    
    shiny::observe({
        toReturn$df_user <- df
        toReturn$df_wpm <- df_wpm()
    })
    return(toReturn)
    
    
    
}
