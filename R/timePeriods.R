timePeriods_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Time periods",
           rHandsontableOutput(ns("timePrd")),
           tags$br(),
           actionButton(ns("saveTimePrd_collDate"), "Save time periods"),
           tags$hr(),
           p('timePrd Definition: The time period to which the data refer. 
               This item reflects the time period covered by the data, not the 
               dates of coding or making documents machine-readable or the dates 
               the data were collected. Also known as span. Use the event 
               attribute to specify "start", "end", or "single" for each date 
               entered. The "cycle" attribute permits specification of the 
               relevant cycle, wave, or round of data. Maps to Dublin Core 
               Coverage element. Inclusion of this element is recommended.'),
           p('collDate Definition: Contains the date(s) when the data were 
               collected. Use the event attribute to specify "start", "end", or 
               "single" for each date entered. The "cycle" attribute permits 
               specification of the relevant cycle, wave, or round of data. 
               Maps to Dublin Core Coverage element. Inclusion of this element 
               in the codebook is recommended.')
  )
}

timePeriods_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
  
    output$timePrd <- renderRHandsontable({
      req(dat())
      fieldOptions <- c("timePrd", "collDate")
      eventOptions <- c("start", "end", "single")
      cycleOptions <- c("Wave1", "Wave2", "Wave3", "Wave4", "Wave5", "Wave6", "Wave7")
      prds <- tibble(
        field = factor(),
        date = as.Date(""),
        event = factor(),
        cycle = factor()
      )
      for (tp in dat()$stdyDscr$stdyInfo$sumDscr$timePrd) {
        prds <- add_row(prds,
                        field = "timePrd",
                        date = as.Date(tp$date), 
                        event = tp$event,
                        cycle = tp$cycle
        )
      }
      for (cd in dat()$stdyDscr$stdyInfo$sumDscr$collDate) {
        prds <- add_row(prds,
                        field = "collDate",
                        date = as.Date(cd$date), 
                        event = cd$event,
                        cycle = cd$cycle
        )
      }
      rht <- rhandsontable(prds, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("field", allowInvalid = FALSE, type = "dropdown", source = fieldOptions) %>% 
        hot_col("event", allowInvalid = FALSE, type = "dropdown", source = eventOptions) %>%
        hot_col("cycle", allowInvalid = FALSE, type = "dropdown", source = cycleOptions) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$saveTimePrd_collDate, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_prds <- hot_to_r(input$timePrd)
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- NULL
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- NULL
          new_timePrd <- list()
          new_collDate <- list()
          for(i in 1:length(updated_prds$field)) {
            if(updated_prds$field[i] == "timePrd") {
              new_tp <- list(date = as.character(updated_prds$date[i]),
                             event = updated_prds$event[i],
                             cycle = updated_prds$cycle[i]
              )
              new_timePrd <- c(new_timePrd, list(new_tp))
            } else {
              new_cd <- list(date = as.character(updated_prds$date[i]),
                             event = updated_prds$event[i],
                             cycle = updated_prds$cycle[i]
              )
              new_collDate <- c(new_collDate, list(new_cd))
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- new_timePrd
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- new_collDate
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}