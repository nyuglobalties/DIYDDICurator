collMode_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Mode of Data Collection",
           rHandsontableOutput(ns("collMode")),
           tags$br(),
           actionButton(ns("save_collMode"), "Save mode of data collection"),
           tags$hr(),
           p('The method used to collect the data; instrumentation 
                 characteristics.')
        )
  
}

collMode_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$collMode <- renderRHandsontable({
      req(dat())
      collMode <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$collMode) == 0) {
        collMode <- add_row(collMode, value = NA_character_, lang = "")
      }
      
      for (c in dat()$stdyDscr$method$dataColl$collMode) {
        if(is.null(c$lang)) c$lang <- NA_character_
        collMode <- add_row(collMode, 
                            value = c$value, 
                            lang = c$lang)
      }
      rht <- rhandsontable(collMode, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_collMode, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_collMode <- hot_to_r(input$collMode)
          updatedData$stdyDscr$method$dataColl$collMode <- NULL
          new_collMode <- list()
          for(i in 1:length(updated_collMode$value)) {
            for(i in 1:length(updated_collMode$value)) {
              if(!is.na(updated_collMode$value[i])) {
                new <- list(value = updated_collMode$value[i],
                            lang  = updated_collMode$lang[i]
                )
                new_collMode <- c(new_collMode, list(new))
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$collMode <- new_collMode
          updatedData$stdyDscr$method$dataColl$collMode <- recurse_write(updatedData$stdyDscr$method$dataColl$collMode)
          updatedData$stdyDscr$method$dataColl$collMode <- lapply(updatedData$stdyDscr$method$dataColl$collMode,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}