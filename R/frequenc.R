frequenc_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Frequency of Data Collection",
           rHandsontableOutput(ns("frequenc")),
           tags$br(),
           actionButton(ns("save_frequenc"), "Save frequency"),
           tags$hr(),
           p('For data collected at more than one point in time, the 
                 frequency with which the data were collected.')
  )
}

frequenc_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$frequenc <- renderRHandsontable({
      req(dat())
      frequenc <- tibble(
        value = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$method$dataColl$frequenc) == 0) {
        frequenc <- add_row(frequenc, value = NA_character_, lang = "")
      }
      
      for (f in dat()$stdyDscr$method$dataColl$frequenc) {
        if(is.null(f$lang)) f$lang <- NA_character_
        frequenc <- add_row(frequenc, 
                            value = f$value, 
                            lang = f$lang)
      }
      rht <- rhandsontable(frequenc, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_frequenc, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_frequenc <- hot_to_r(input$frequenc)
          updatedData$stdyDscr$method$dataColl$frequenc <- NULL
          new_frequenc <- list()
          for(i in 1:length(updated_frequenc$value)) {
            if(!is.na(updated_frequenc$value[i])) {
              if(updated_frequenc$value[i] != "") {
                new <- list(value = updated_frequenc$value[i],
                            lang  = updated_frequenc$lang[i]
                            )
                new_frequenc <- c(new_frequenc, list(new))
              }
            }
          }
          updatedData$stdyDscr$method$dataColl$frequenc <- new_frequenc
          updatedData$stdyDscr$method$dataColl$frequenc <- recurse_write(updatedData$stdyDscr$method$dataColl$frequenc)
          updatedData$stdyDscr$method$dataColl$frequenc <- lapply(updatedData$stdyDscr$method$dataColl$frequenc,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}