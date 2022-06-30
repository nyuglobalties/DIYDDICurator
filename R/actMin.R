actMin_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Actions to Minimize Losses",
           rHandsontableOutput(ns("actMin")),
           tags$br(),
           actionButton(ns("save_actMin"), "Save actions to minimize losses"),
           tags$hr(),
           p('Summary of actions taken to minimize data loss. Includes 
                 information on actions such as follow-up visits, supervisory 
                 checks, historical matching, estimation, etc.')
           )
}

actMin_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$actMin <- renderRHandsontable({
      req(dat())
      actMin <- tibble(
        value = character(),
        lang = character()
      )
      for (a in dat()$stdyDscr$method$dataColl$actMin) {
        if(is.null(a$lang)) a$lang <- NA_character_
        actMin <- add_row(actMin, 
                          value = a$value, 
                          lang = a$lang)
      }
      rht <- rhandsontable(actMin, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_actMin, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_actMin <- hot_to_r(input$actMin)
          updatedData$stdyDscr$method$dataColl$actMin <- NULL
          new_actMin <- list()
          for(i in 1:length(updated_actMin$value)) {
            new <- list(value = updated_actMin$value[i],
                        lang  = updated_actMin$lang[i]
            )
            new_actMin <- c(new_actMin, list(new))
          }
          updatedData$stdyDscr$method$dataColl$actMin <- new_actMin
          updatedData$stdyDscr$method$dataColl$actMin <- recurse_write(updatedData$stdyDscr$method$dataColl$actMin)
          updatedData$stdyDscr$method$dataColl$actMin <- lapply(updatedData$stdyDscr$method$dataColl$actMin,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}