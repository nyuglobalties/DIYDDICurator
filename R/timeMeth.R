timeMeth_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Time Method",
           rHandsontableOutput(ns("timeMeth")),
           tags$br(),
           actionButton(ns("save_timeMeth"), "Save time method"),
           tags$hr(),
           p('The time method or time dimension of the data collection.')
           )
}

timeMeth_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$timeMeth <- renderRHandsontable({
      req(dat())
      timeMeth <- tibble(
        value = character(),
        lang = character()
      )
      for (t in dat()$stdyDscr$method$dataColl$timeMeth) {
        if(is.null(t$lang)) t$lang <- NA_character_
        timeMeth <- add_row(timeMeth, 
                            value = t$value, 
                            lang = t$lang)
      }
      rht <- rhandsontable(timeMeth, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_timeMeth, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_timeMeth <- hot_to_r(input$timeMeth)
          updatedData$stdyDscr$method$dataColl$timeMeth <- NULL
          new_timeMeth <- list()
          for(i in 1:length(updated_timeMeth$value)) {
            new <- list(value = updated_timeMeth$value[i],
                        lang  = updated_timeMeth$lang[i]
            )
            new_timeMeth <- c(new_timeMeth, list(new))
          }
          updatedData$stdyDscr$method$dataColl$timeMeth <- new_timeMeth
          updatedData$stdyDscr$method$dataColl$timeMeth <- recurse_write(updatedData$stdyDscr$method$dataColl$timeMeth)
          updatedData$stdyDscr$method$dataColl$timeMeth <- lapply(updatedData$stdyDscr$method$dataColl$timeMeth,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}