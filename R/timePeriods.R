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
      eventOptions <- c(NA_character_, "start", "end", "single")
      prds <- tibble(
        field = factor(),
        value = character(),
        date = as.Date(""),
        event = factor(),
        cycle = factor(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$timePrd) == 0 & length(dat()$stdyDscr$stdyInfo$sumDscr$collDat) == 0) {
        prds <- add_row(prds, field = "timePrd", value = NA_character_)
      }
      
      for (tp in dat()$stdyDscr$stdyInfo$sumDscr$timePrd) {
        if(is.null(tp$date)) tp$date <- NA_character_
        if(is.null(tp$event)) tp$event <- NA_character_
        if(is.null(tp$cycle)) tp$cycle <- NA_character_
        if(is.null(tp$lang)) tp$lang <- NA_character_
        prds <- add_row(prds,
                        field = "timePrd",
                        value = tp$value,
                        date = as.Date(tp$date), 
                        event = tp$event,
                        cycle = tp$cycle,
                        lang = tp$lang
        )
      }
      for (cd in dat()$stdyDscr$stdyInfo$sumDscr$collDate) {
        if(is.null(cd$date)) cd$date <- NA_character_
        if(is.null(cd$event)) cd$event <- NA_character_
        if(is.null(cd$cycle)) cd$cycle <- NA_character_
        if(is.null(cd$lang)) cd$lang <- NA_character_
        prds <- add_row(prds,
                        field = "collDate",
                        value = cd$value,
                        date = as.Date(cd$date), 
                        event = cd$event,
                        cycle = cd$cycle,
                        lang = cd$lang
        )
      }
      rht <- rhandsontable(prds, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(20, 20, 20, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("field", allowInvalid = FALSE, type = "dropdown", source = fieldOptions) %>% 
        hot_col("event", allowInvalid = FALSE, type = "dropdown", source = eventOptions) %>%
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
              if(!is.na(updated_prds$value[i])) {
                if(updated_prds$value[i] != "") {
                  new_tp <- list(value = updated_prds$value[i],
                                 date = as.character(updated_prds$date[i]),
                                 event = updated_prds$event[i],
                                 cycle = updated_prds$cycle[i],
                                 lang = updated_prds$lang[i]
                  )
                  new_timePrd <- c(new_timePrd, list(new_tp))
                }
              }
            } else {
              if(!is.na(updated_prds$value[i])) {
                if(updated_prds$value[i] != "") {
                  new_cd <- list(value = updated_prds$value[i],
                                 date = as.character(updated_prds$date[i]),
                                 event = updated_prds$event[i],
                                 cycle = updated_prds$cycle[i],
                                 lang = updated_prds$lang[i]
                  )
                  new_collDate <- c(new_collDate, list(new_cd))
                }
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- new_timePrd
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$timePrd)
          updatedData$stdyDscr$stdyInfo$sumDscr$timePrd <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$timePrd,function(x) x[!is.na(x)])
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- new_collDate
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$collDate)
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$collDate,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}