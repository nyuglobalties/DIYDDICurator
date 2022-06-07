resInstru_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Type of Research Instrument",
           rHandsontableOutput(ns("resInstru")),
           tags$br(),
           actionButton(ns('save_resInstru'), "Save type of research instrument"),
           tags$hr(),
           p('The type of data collection instrument used. "Structured" 
                 indicates an instrument in which all respondents are asked the 
                 same questions/tests, possibly with precoded answers. If a 
                 small portion of such a questionnaire includes open-ended 
                 questions, provide appropriate comments. "Semi-structured" 
                 indicates that the research instrument contains mainly 
                 open-ended questions. "Unstructured" indicates that in-depth 
                 interviews were conducted.')
           )
}

resInstru_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$resInstru <- renderRHandsontable({
      req(dat())
      resInstru <- tibble(
        value = character(),
        type = character(),
        lang = character()
      )
      for (r in dat()$stdyDscr$method$dataColl$resInstru) {
        resInstru <- add_row(resInstru, 
                             value = r$value, 
                             type = r$type,
                             lang = r$lang)
      }
      rht <- rhandsontable(resInstru, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_resInstru, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_resInstru <- hot_to_r(input$resInstru)
          updatedData$stdyDscr$method$dataColl$resInstru <- NULL
          new_resInstru <- list()
          for(i in 1:length(updated_resInstru$value)) {
            new <- list(value = updated_resInstru$value[i],
                        type = updated_resInstru$type[i],
                        lang  = updated_resInstru$lang[i]
            )
            new_resInstru <- c(new_resInstru, list(new))
          }
          updatedData$stdyDscr$method$dataColl$resInstru <- new_resInstru
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}