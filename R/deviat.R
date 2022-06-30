deviat_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Major Deviations from Sample Design",
           rHandsontableOutput(ns("deviat")),
           tags$br(),
           actionButton(ns("save_deviat"), "Save major deviations from Sample Design"),
           tags$hr(),
           p('Information indicating correspondence as well as discrepancies 
                 between the sampled units (obtained) and available statistics 
                 for the population (age, sex-ratio, marital status, etc.) as a 
                 whole.'))
}

deviat_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$deviat <- renderRHandsontable({
      req(dat())
      deviat <- tibble(
        value = character(),
        lang = character()
      )
      for (d in dat()$stdyDscr$method$dataColl$deviat) {
        if(is.null(d$lang)) d$lang <- NA_character_
        deviat <- add_row(deviat, 
                          value = d$value, 
                          lang = d$lang)
      }
      rht <- rhandsontable(deviat, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_deviat, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_deviat <- hot_to_r(input$deviat)
          updatedData$stdyDscr$method$dataColl$deviat <- NULL
          new_deviat <- list()
          for(i in 1:length(updated_deviat$value)) {
            new <- list(value = updated_deviat$value[i],
                        lang  = updated_deviat$lang[i]
            )
            new_deviat <- c(new_deviat, list(new))
          }
          updatedData$stdyDscr$method$dataColl$deviat <- new_deviat
          updatedData$stdyDscr$method$dataColl$deviat <- recurse_write(updatedData$stdyDscr$method$dataColl$deviat)
          updatedData$stdyDscr$method$dataColl$deviat <- lapply(updatedData$stdyDscr$method$dataColl$deviat,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}