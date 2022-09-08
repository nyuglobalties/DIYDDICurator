collDate_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Dates of Data Collection",
          rHandsontableOutput(ns("collDate")),
          tags$br(),
          actionButton(ns("save_collDate"), "Save data collection dates"),
          tags$hr(),
          p('collDate Definition: Contains the date(s) when the data were 
            collected. Use the event attribute to specify "start", "end", or 
            "single" for each date entered. The "cycle" attribute permits 
            specification of the relevant cycle, wave, or round of data. 
            Maps to Dublin Core Coverage element. Inclusion of this element 
            in the codebook is recommended.')
  )
}

collDate_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$collDate <- renderRHandsontable({
      req(dat())
      eventOptions <- c(NA_character_, "start", "end", "single")
      prds <- tibble(
        value = character(),
        date = as.Date(""),
        event = character(),
        cycle = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$collDate) == 0) {
        prds <- add_row(prds, value = NA_character_)
      }
      
      for (cd in dat()$stdyDscr$stdyInfo$sumDscr$collDate) {
        if(is.null(cd$date)) cd$date <- NA_character_
        if(is.null(cd$event)) cd$event <- NA_character_
        if(is.null(cd$cycle)) cd$cycle <- NA_character_
        if(is.null(cd$lang)) cd$lang <- NA_character_
        prds <- add_row(prds,
                        value = cd$value,
                        date = as.Date(cd$date), 
                        event = cd$event,
                        cycle = cd$cycle,
                        lang = cd$lang
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
      input$save_collDate, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_prds <- hot_to_r(input$collDate)
          updated_prds$event[is.na(updated_prds$event) | updated_prds$event == "" ] <- "single"
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- NULL
          new_collDate <- list()
          for(i in 1:length(updated_prds$value)) {
            if(!is.na(updated_prds$value[i])) {
              if(updated_prds$value[i] != "") {
                new_cd <- list(value = updated_prds$value[i],
                               date = as.character(updated_prds$date[i]),
                               event = updated_prds$event[i],
                               cycle = updated_prds$cycle[i],
                               lang = stringr::str_extract(updated_prds$lang[i], "^[a-z]{2}")
                )
                new_collDate <- c(new_collDate, list(new_cd))
              }
            }
          }
          
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- new_collDate
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$collDate)
          updatedData$stdyDscr$stdyInfo$sumDscr$collDate <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$collDate,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}