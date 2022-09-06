timePeriods_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Time periods",
           rHandsontableOutput(ns("timePrd")),
           tags$br(),
           actionButton(ns("save_timePrd"), "Save time periods"),
           tags$hr(),
           p('timePrd Definition: The time period to which the data refer. 
               This item reflects the time period covered by the data, not the 
               dates of coding or making documents machine-readable or the dates 
               the data were collected. Also known as span. Use the event 
               attribute to specify "start", "end", or "single" for each date 
               entered. The "cycle" attribute permits specification of the 
               relevant cycle, wave, or round of data. Maps to Dublin Core 
               Coverage element. Inclusion of this element is recommended.'),
  )
}

timePeriods_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
  
    output$timePrd <- renderRHandsontable({
      req(dat())
      eventOptions <- c(NA_character_, "start", "end", "single")
      prds <- tibble(
        value = character(),
        date = as.Date(""),
        event = character(),
        cycle = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$timePrd) == 0) {
        prds <- add_row(prds, value = NA_character_)
      }
      
      for (tp in dat()$stdyDscr$stdyInfo$sumDscr$timePrd) {
        if(is.null(tp$date)) tp$date <- NA_character_
        if(is.null(tp$event)) tp$event <- NA_character_
        if(is.null(tp$cycle)) tp$cycle <- NA_character_
        if(is.null(tp$lang)) tp$lang <- NA_character_
        prds <- add_row(prds,
                        value = tp$value,
                        date = as.Date(tp$date), 
                        event = tp$event,
                        cycle = tp$cycle,
                        lang = tp$lang
        )
      }
      rht <- rhandsontable(prds, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("event", allowInvalid = FALSE, type = "dropdown", source = eventOptions) %>%
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_timePrd, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_prds <- hot_to_r(input$timePrd)
          updated_prds$event[is.na(updated_prds$event) | updated_prds$event == "" ] <- "single"
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- NULL
          new_timePrd <- list()
          for(i in 1:length(updated_prds$value)) {
            if(!is.na(updated_prds$value[i])) {
              if(updated_prds$value[i] != "") {
                new_tp <- list(value = updated_prds$value[i],
                               date = as.character(updated_prds$date[i]),
                               event = updated_prds$event[i],
                               cycle = updated_prds$cycle[i],
                               lang = stringr::str_extract(updated_prds$lang[i], "^[a-z]{2}")
                )
                new_timePrd <- c(new_timePrd, list(new_tp))
              }
            }
          } 
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- new_timePrd
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$timePrd)
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$timePrd,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}