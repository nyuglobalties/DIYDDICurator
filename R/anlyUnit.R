anlyUnit_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Unit of Analysis",
           rHandsontableOutput(ns("anlyUnit")),
           tags$br(),
           actionButton(ns("save_anlyUnit"), "Save units of analysis"),
           tags$hr(),
           p("Definition: Basic unit of analysis or observation that the file 
               describes: individuals, families/households, groups, 
               institutions/organizations, administrative units, etc.")
           )
}

anlyUnit_server <- function(id, dat, filepth, lang) {
  moduleServer(id, function(input, output, session) {
    
    output$anlyUnit <- renderRHandsontable({
      req(dat())
      anlyUnit <- tibble(
        group = character(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) == 0) {
        anlyUnit <- add_row(anlyUnit, group = NA_character_, lang = "")
      }
      
      for (a in dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) {
        if(is.null(a$lang)) a$lang <- NA_character_
        anlyUnit <- add_row(anlyUnit, 
                            group = a$group, 
                            lang = a$lang)
      }
      rht <- rhandsontable(anlyUnit, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$save_anlyUnit, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_anlyUnit <- hot_to_r(input$anlyUnit)
          updatedData$stdyDscr$sumDscr$anlyUnit <- NULL
          new_anlyUnit <- list()
          for(i in 1:length(updated_anlyUnit$group)) {
            if(!is.na(updated_anlyUnit$group[i])) {
              if(updated_anlyUnit$group[i] != "") {
                new <- list(group = updated_anlyUnit$group[i],
                            lang  = stringr::str_extract(updated_anlyUnit$lang[i], "^[a-z]{2}")
                )
                new_anlyUnit <- c(new_anlyUnit, list(new))
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit <- new_anlyUnit
          updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit)
          updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$anlyUnit,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}