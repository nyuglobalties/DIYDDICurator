collSitu_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Characteristics of Data Collection Situation",
           rHandsontableOutput(ns("collSitu")),
           tags$br(),
           actionButton(ns("save_collSitu"), "Save characteristics of data collection situation"),
           tags$hr(),
           p('Description of noteworthy aspects of the data collection 
                 situation. Includes information on factors such as c
                 ooperativeness of respondents, duration of interviews, number 
                 of call-backs, etc.')
           )
}

collSitu_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$collSitu <- renderRHandsontable({
      req(dat())
      collSitu <- tibble(
        value = character(),
        lang = character()
      )
      for (c in dat()$stdyDscr$method$dataColl$collSitu) {
        collSitu <- add_row(collSitu, 
                            value = c$value, 
                            lang = c$lang)
      }
      rht <- rhandsontable(collSitu, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_collSitu, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_collSitu <- hot_to_r(input$collSitu)
          updatedData$stdyDscr$method$dataColl$collSitu <- NULL
          new_collSitu <- list()
          for(i in 1:length(updated_collSitu$value)) {
            new <- list(value = updated_collSitu$value[i],
                        lang  = updated_collSitu$lang[i]
            )
            new_collSitu <- c(new_collSitu, list(new))
          }
          updatedData$stdyDscr$method$dataColl$collSitu <- new_collSitu
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}