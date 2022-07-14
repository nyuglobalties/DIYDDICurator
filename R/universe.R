universe_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Universe",
           rHandsontableOutput(ns("universe")),
           tags$br(),
           actionButton(ns("saveUniverse"), "Save universe"),
           tags$hr(),
           tags$p('The group of persons or other elements that are the object 
                    of research and to which any analytic results refer. Age, 
                    nationality, and residence commonly help to delineate a 
                    given universe, but any of a number of factors may be 
                    involved, such as sex, race, income, veteran status, 
                    criminal convictions, etc. The universe may consist of 
                    elements other than persons, such as housing units, court 
                    cases, deaths, countries, etc. In general, it should be 
                    possible to tell from the description of the universe 
                    whether a given individual or element (hypothetical or real) 
                    is a member of the population under study.'),
           tags$p('The "clusion" attribute provides for specification of groups 
                    included (I) in or excluded (E) from the universe. This 
                    element may be repeated only to support multiple language 
                    expressions of the content.')
           )
}

universe_server <-  function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$universe <- renderRHandsontable({
      req(dat())
      clusionOptions <- c(NA_character_, "I", "E")
      universe <- tibble(
        group = character(),
        clusion = factor(),
        lang = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$universe) == 0) {
        universe <- add_row(universe, group = NA_character_)
      }
      for (u in dat()$stdyDscr$stdyInfo$sumDscr$universe) {
        if(is.null(u$lang)) u$lang <- NA_character_
        universe <- add_row(universe, 
                            group = u$group, 
                            clusion = u$clusion,
                            lang = u$lang)
      }
      rht <- rhandsontable(universe, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(30, 20, 20, 20),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("clusion", allowInvalid = FALSE, type = "dropdown", source = clusionOptions) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$saveUniverse, {
        isolate({
          req(dat())
          updatedData <- dat()
          updatedUniverse<- hot_to_r(input$universe)
          updatedData$stdyDscr$stdyInfo$sumDscr$universe <- NULL
          newUniverse <- list()
          for(i in 1:length(updatedUniverse$group)) {
            if(!is.na(updatedUniverse$group[i])) {
              if(updatedUniverse$group[i] != "") {
                new <- list(group = updatedUniverse$group[i],
                            level = "project",
                            clusion = updatedUniverse$clusion[i],
                            lang  = updatedUniverse$lang[i]
                )
                newUniverse <- c(newUniverse, list(new))
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$universe <- newUniverse
          updatedData$stdyDscr$stdyInfo$sumDscr$universe <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$universe)
          updatedData$stdyDscr$stdyInfo$sumDscr$universe <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$universe,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}