geography_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Geography",
           rHandsontableOutput(ns("geog")),
           tags$br(),
           actionButton(ns("saveGeog"), "Save geographic elements"),
           tags$hr(),
           p('nation definition: Indicates the country or countries covered in 
               the file. Attribute "abbr" may be used to list common 
               abbreviations; use of ISO country codes is recommended. Maps to 
               Dublin Core Coverage element. Inclusion of this element is recommended.'),
           p('geogCover definition: Information on the geographic coverage of 
               the data. Includes the total geographic scope of the data, and 
               any additional levels of geographic coding provided in the 
               variables. Maps to Dublin Core Coverage element. Inclusion of 
               this element in the codebook is recommended.'),
           p('geogUnit definition: Lowest level of geographic aggregation 
               covered by the data.')
           )
}

geography_server <- function(id, dat, filepth) {
  moduleServer(id, function(input, output, session) {
    
    output$geog <- renderRHandsontable({
      req(dat())
      fieldOptions <- c("nation", "geogCover", "geogUnit")
      geog <- tibble(
        field = factor(),
        value = character(),
        abbr = character(),
        lang = character()
      )
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$nation) {
        geog <- add_row(geog,
                        field = "nation",
                        value = v$value,
                        abbr = v$abbr,
                        lang = v$lang
        )
      }
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$geogCover) {
        geog <- add_row(geog,
                        field = "geogCover",
                        value = v$value, 
                        abbr = NA,
                        lang = v$lang
        )
      }
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) {
        geog <- add_row(geog,
                        field = "geogUnit",
                        value = v$value, 
                        abbr = NA,
                        lang = v$lang
        )
      }
      rht <- rhandsontable(geog, stretchH = "all", overflow = "visible") %>% # converts the R dataframe to rhandsontable object
        hot_cols(colWidths = c(40, 100, 40, 40),
                 manualColumnMove = FALSE,
                 manualColumnResize = FALSE) %>% 
        hot_rows(rowHeights = NULL) %>% 
        hot_col("field", allowInvalid = FALSE, type = "dropdown", source = fieldOptions) %>% 
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE) 
      htmlwidgets::onRender(rht, change_hook)
    })
    
    observeEvent(
      input$saveGeog, {
        isolate({
          req(dat())
          updatedData <- dat()
          updated_geog <- hot_to_r(input$geog)
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- NULL
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- NULL
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- NULL
          new_nation <- list()
          new_geogCover <- list()
          new_geogUnit <- list()
          for(i in 1:length(updated_geog$field)) {
            if(updated_geog$field[i] == "nation") {
              new_n <- list(value = updated_geog$value[i],
                            abbr = updated_geog$abbr[i],
                            lang = updated_geog$lang[i]
              )
              new_nation <- c(new_nation, list(new_n))
            } else if(updated_geog$field[i] == "geogCover"){
              new_gc <- list(value = updated_geog$value[i],
                             lang = updated_geog$lang[i]
              )
              new_geogCover <- c(new_geogCover, list(new_gc))
            } else {
              new_gu <- list(value = updated_geog$value[i],
                             lang = updated_geog$lang[i]
              )
              new_geogUnit <- c(new_geogUnit, list(new_gu))
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- new_nation
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- new_geogCover
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- new_geogUnit
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}