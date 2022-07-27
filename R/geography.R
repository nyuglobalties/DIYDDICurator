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

geography_server <- function(id, dat, filepth, lang) {
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
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$nation) == 0 &
         length(dat()$stdyDscr$stdyInfo$sumDscr$geogCover) == 0 &
         length(dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) == 0) {
        geog <- add_row(geog, field = "nation", value = NA_character_, abbr = "", lang = "")
      }
      
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$nation) {
        if(is.null(v$abbr)) v$abbr <- NA_character_
        if(is.null(v$lang)) v$lang <- NA_character_
        geog <- add_row(geog,
                        field = "nation",
                        value = v$value,
                        abbr = v$abbr,
                        lang = v$lang
        )
      }
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$geogCover) {
        if(is.null(v$lang)) v$lang <- NA_character_
        geog <- add_row(geog,
                        field = "geogCover",
                        value = v$value, 
                        abbr = NA,
                        lang = v$lang
        )
      }
      for (v in dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) {
        if(is.null(v$lang)) v$lang <- NA_character_
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
        hot_col("lang", allowInvalid = FALSE, type = "dropdown", source = lang) %>% 
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
            if(!is.na(updated_geog$value[i])) {
              if(updated_geog$value[i] != "") {
                if(updated_geog$field[i] == "nation") {
                  new_n <- list(value = updated_geog$value[i],
                                abbr = updated_geog$abbr[i],
                                lang = stringr::str_extract(updated_geog$lang[i], "^[a-z]{2}")
                  )
                  new_nation <- c(new_nation, list(new_n))
                } else if(updated_geog$field[i] == "geogCover"){
                  new_gc <- list(value = updated_geog$value[i],
                                 lang = stringr::str_extract(updated_geog$lang[i], "^[a-z]{2}")
                  )
                  new_geogCover <- c(new_geogCover, list(new_gc))
                } else {
                  new_gu <- list(value = updated_geog$value[i],
                                 lang = stringr::str_extract(updated_geog$lang[i], "^[a-z]{2}")
                  )
                  new_geogUnit <- c(new_geogUnit, list(new_gu))
                }
              }
            }
          }
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- new_nation
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$nation)
          updatedData$stdyDscr$stdyInfo$sumDscr$nation <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$nation,function(x) x[!is.na(x)])
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- new_geogCover
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$geogCover)
          updatedData$stdyDscr$stdyInfo$sumDscr$geogCover <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$geogCover,function(x) x[!is.na(x)])
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- new_geogUnit
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- recurse_write(updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit)
          updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit <- lapply(updatedData$stdyDscr$stdyInfo$sumDscr$geogUnit,function(x) x[!is.na(x)])
          yaml::write_yaml(updatedData, filepth())
        })
      })
    
  })
}