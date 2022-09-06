collectorTraining_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Collector Training",
           rHandsontableOutput(ns("collectorTraining")),
           tags$br(),
           actionButton(ns("save_collectorTraining"), "Save collector training"),
           tags$hr(),
           p('Describes the training provided to data collectors including 
             interviewer training, process testing, compliance with standards 
             etc. This is repeatable for language and to capture different 
             aspects of the training process. The type attribute allows 
             specification of the type of training being described.')
  )
  
}

collectorTraining_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$collectorTraining <- renderRHandsontable({
      req(dat())
      collectorTraining <- tibble(
        value = character(),
        type = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$collectorTraining) == 0) {
        collectorTraining <- add_row(collectorTraining, value = NA_character_, type = "", lang = "")
      }
      
      
      for (c in dat()$stdyDscr$method$dataColl$collectorTraining) {
        if(is.null(c$type)) c$type <- NA_character_
        if(is.null(c$lang)) c$lang <- NA_character_
        collectorTraining <- add_row(collectorTraining, 
                            value = c$value, 
                            type = c$type,
                            lang = c$lang)
      }
      rht <- rhandsontable(collectorTraining, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_collectorTraining, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_collectorTraining <- hot_to_r(input$collectorTraining)
          updatedData$stdyDscr$method$dataColl$collectorTraining <- NULL
          new_collectorTraining <- list()
          for(i in 1:length(updated_collectorTraining$value)) {
            if(!is.na(updated_collectorTraining$value[i])) {
              if(updated_collectorTraining$value[i] != "") {
                new <- list(value = updated_collectorTraining$value[i],
                            type = updated_collectorTraining$type[i],
                            lang  = stringr::str_extract(updated_collectorTraining$lang[i], "^[a-z]{2}")
                )
                new_collectorTraining <- c(new_collectorTraining, list(new))
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$collectorTraining <- new_collectorTraining
          updatedData$stdyDscr$method$dataColl$collectorTraining <- recurse_write(updatedData$stdyDscr$method$dataColl$collectorTraining)
          updatedData$stdyDscr$method$dataColl$collectorTraining <- lapply(updatedData$stdyDscr$method$dataColl$collectorTraining,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}