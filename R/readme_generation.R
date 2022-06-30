readme_generation_ui <- function(id) {
  ns <- NS(id)
  tabPanel("README",
           h1(textOutput(ns("title"))),
           h3(textOutput(ns("authors"))),
           htmlOutput(ns("contributors")),
           htmlOutput(ns("production")),
           htmlOutput(ns("funders")),
           tags$hr(),
           h4(textOutput(ns("series"))),
           tags$em(textOutput(ns("seriesInfo"))),
           tags$hr(),
           h3("Study Information"),
           tags$hr(),
           h4("Abstract"),
           htmlOutput(ns("abstract")),
           h4("Keywords"),
           htmlOutput(ns("keywords")),
           h4("Universe and Units of Analysis"),
           htmlOutput(ns("universes")),
           htmlOutput(ns("anlyUnits")),
           h4("Geography"),
           htmlOutput(ns("nation")),
           htmlOutput(ns("geogCoverage")),
           htmlOutput(ns("geogUnit")),
           h4("Time periods"),
           htmlOutput(ns("timePrd")),
           tags$hr(),
           h3("Data Collection"),
           tags$hr(),
           htmlOutput(ns("dataCollector")),
           htmlOutput(ns("collMode")),
           htmlOutput(ns("collSitu")),
           htmlOutput(ns("timeMeth")),
           h4("Collection Dates"),
           htmlOutput(ns("collDate")),
           h4("Research Instrument"),
           htmlOutput(ns("resInstru")),
           htmlOutput(ns("instrumentDevelopment")),
           h4("Sampling Procedure"),
           htmlOutput(ns("sampProc")),
           htmlOutput(ns("ConOps")),
           htmlOutput(ns("actMin")),
           htmlOutput(ns("deviat")),
           htmlOutput(ns("frequenc")),
           tags$hr(),
           h3("Variable Groups"),
           tags$hr(),
           htmlOutput(ns("varGrp"))
  )
}

readme_generation_server <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderText( {
      title <- dat()$stdyDscr$citation$titlStmt$titl$value
    })
    
    output$authors <- renderText( {
      authors <- character()
      if(length(dat()$stdyDscr$citation$rspStmt$AuthEnty) > 0) {
        for(a in dat()$stdyDscr$citation$rspStmt$AuthEnty) {
          authors <- paste0(authors, a$name, " (", a$affiliation, "), ")
        }
      }
      authors <- substr(authors, 1, nchar(authors)-2)
    })
    
    output$series <- renderText( {
      out <- ""
      if(length(dat()$stdyDscr$citation$serStmt$serName) > 0) {
        out <- paste0("A ", dat()$stdyDscr$citation$serStmt$serName[[1]]$value, 
                      " (", dat()$stdyDscr$citation$serStmt$serName[[1]]$abbr, 
                      ") project")
      } 
      out
    })
    
    output$seriesInfo <- renderText( {
      out <- ""
      if(length(dat()$stdyDscr$citation$serStmt$serInfo) > 0) {
        out <- dat()$stdyDscr$citation$serStmt$serInfo[[1]]$value
      } 
      out
    })
    
    output$contributors <- renderText( {
      contribs <- ""
      if(length(dat()$stdyDscr$citation$rspStmt$othId) > 0) {
        for(a in dat()$stdyDscr$citation$rspStmt$othId) {
          contribs <- paste0(contribs, "<b>", a$role, ":</b>", " ", 
                             a$name, " (", a$affiliation, ")</br>")
        }
      }
      contribs <- substr(contribs, 1, nchar(contribs)-5)
    })
    
    output$production <- renderText( {
      producers <- ""
      if(length(dat()$stdyDscr$citation$prodStmt$producer) > 0) {
        producers <- "<b>Produced by: </b>"
        for(a in dat()$stdyDscr$citation$prodStmt$producer) {
          producers <- paste0(producers, a$name, " (", a$abbr, "), ")
        }
      }
      producers <- substr(producers, 1, nchar(producers)-2)
      producers <- paste0(producers, "<br/>")
      
      if(length(dat()$stdyDscr$citation$prodStmt$prodPlac) > 0) {
        producers <- paste0(producers, "<b>Place of Production: </b>")
        for(a in dat()$stdyDscr$citation$prodStmt$prodPlac) {
          producers <- paste0(producers, a$value, ", ")
        }
        producers <- substr(producers, 1, nchar(producers)-2)
        producers <- paste0(producers, "<br/>")
      }
      
      if(length(dat()$stdyDscr$citation$prodStmt$prodDate) > 0) {
        producers <- paste0(producers, "<b>Date of Production: </b>")
        for(a in dat()$stdyDscr$citation$prodStmt$prodDate) {
          producers <- paste0(producers, a$value, ", ")
        }
        producers <- substr(producers, 1, nchar(producers)-2)
        producers <- paste0(producers, "<br/>")
      }
      HTML(producers)
    })
    
    output$funders <- renderText( {
      funders <- ""
      if(length(dat()$stdyDscr$citation$prodStmt$fundAg) > 0) {
        funders <- "<b>Funded by:</b><br/>"
        for(a in dat()$stdyDscr$citation$prodStmt$fundAg) {
          funders <- paste0(funders, "&nbsp; &nbsp;&nbsp; &nbsp;", a$name, 
                            " (", a$abbr, ") - ", a$role, "<br/>")
        }
      }
      funders <- substr(funders, 1, nchar(funders)-5)
      
      HTML(funders)
    })
    
    output$abstract <- renderUI( {
      abstract <- ""
      if(length(dat()$stdyDscr$stdyInfo$abstract) > 0) {
        for (a in dat()$stdyDscr$stdyInfo$abstract) {
          abstract <- paste0(abstract, "&nbsp; &nbsp;&nbsp; &nbsp;<b>", a$contentType, "</b>: ", a$value, "<br/><br/>")
        }
      }
      abstract <- substr(abstract, 1, nchar(abstract)-10)
      HTML(abstract)
    })
    
    output$keywords <- renderText( {
      keywords <- ""
      if(length(dat()$stdyDscr$stdyInfo$subject) > 0) {
        keywords <- "&nbsp; &nbsp;&nbsp; &nbsp;"
        for (k in dat()$stdyDscr$stdyInfo$subject) {
          keywords <- paste0(keywords, k$keyword, ", ")
        }
      }
      keywords <- substr(keywords, 1, nchar(keywords)-2)
      HTML(keywords)
    })
    
    output$universes <- renderText( {
      universe <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$universe) > 0) {
        universe <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Universe: </b>"  
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$universe) {
          universe <- paste0(universe, n$group, " (", n$clusion, "), ")
        }
      }
      universe <- substr(universe, 1, nchar(universe)-2)
      HTML(universe)
    })
    
    output$anlyUnits <- renderText( {
      anlyUnit <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) > 0) {
        if(length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) == 1) {
          anlyUnit <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Unit of Analysis: </b>"  
        } else {
          anlyUnit <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Units of Analysis: </b>"
        }
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) {
          anlyUnit <- paste0(anlyUnit, n$group, ", ")
        }
      }
      anlyUnit <- substr(anlyUnit, 1, nchar(anlyUnit)-2)
      HTML(anlyUnit)
    })
    
    output$nation <- renderText( {
      nation <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$nation) > 0) {
        if(length(dat()$stdyDscr$stdyInfo$sumDscr$nation) == 1) {
          nation <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Nation: </b>"  
        } else {
          nation <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Nations: </b>"
        }
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$nation) {
          nation <- paste0(nation, n$value, " (", n$abbr, "), ")
        }
      }
      nation <- substr(nation, 1, nchar(nation)-2)
      HTML(nation)
    })
    
    output$geogCoverage <- renderText( {
      geogCover <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$geogCover) > 0) {
        geogCover <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Geographic coverage: </b>"
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$geogCover) {
          geogCover <- paste0(geogCover, n$value, ", ")
        }
      }
      geogCover <- substr(geogCover, 1, nchar(geogCover)-2)
      HTML(geogCover)
    })
    
    output$geogUnit <- renderText( {
      geogUnit <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) > 0) {
        if(length(dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) == 1) {
          geogUnit <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Geographic Unit: </b>"  
        } else {
          geogUnit <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Geographic Units: </b>"
        }
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) {
          geogUnit <- paste0(geogUnit, n$value, ", ")
        }
      }
      geogUnit <- substr(geogUnit, 1, nchar(geogUnit)-2)
      HTML(geogUnit)
    })
    
    output$timePrd <- renderText( {
      timePrd <- ""
      ds <- tibble(date = character(),
                   event = character(),
                   cycle = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$timePrd) > 0) {
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$timePrd) {
          ds <- add_row(ds, date = n$date, event = n$event, cycle = n$cycle)
        }
      }
      cycles = unique(ds$cycle)
      for(c in cycles) {
        subset <- ds %>% filter(cycle == c)
        start <- subset %>% filter(event == "start") %>% select(date)
        end <- subset %>% filter(event == "end") %>% select(date)
        timePrd <- paste0(timePrd, "&nbsp; &nbsp;&nbsp; &nbsp;<b>", c, ": </b>", start, " to ", end, "<br/><br/>")
      }
      timePrd <- substr(timePrd, 1, nchar(timePrd)-10)
      HTML(timePrd)
    })
    
    output$dataCollector <- renderText( {
      collectors <- ""
      if(length(dat()$stdyDscr$method$dataColl$dataCollector) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$dataCollector) == 1) {
          collectors <- "<b>Data Collector: </b>"  
        } else {
          collectors <- "<b>Data Collectors: </b>"
        }
        for (n in dat()$stdyDscr$method$dataColl$dataCollector) {
          collectors <- paste0(collectors, n$value, ", ")
        }
      }
      collectors <- substr(collectors, 1, nchar(collectors)-2)
      HTML(collectors)
    })
    
    output$collMode <- renderText( {
      mode <- ""
      if(length(dat()$stdyDscr$method$dataColl$collMode) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$collMode) == 1) {
          mode <- "<b>Mode of collection: </b>"  
        } else {
          mode <- "<b>Modes of collection: </b>"
        }
        for (n in dat()$stdyDscr$method$dataColl$collMode) {
          mode <- paste0(mode, n$value, ", ")
        }
      }
      mode <- substr(mode, 1, nchar(mode)-2)
      HTML(mode)
    })
    
    output$collSitu <- renderText( {
      situ <- ""
      if(length(dat()$stdyDscr$method$dataColl$collSitu) > 0) {
        situ <- "<b>Characteristics of Data Collection Situation: </b>"
        for (n in dat()$stdyDscr$method$dataColl$collSitu) {
          situ <- paste0(situ, n$value, ", ")
        }
      }
      situ <- substr(situ, 1, nchar(situ)-2)
      HTML(situ)
    })

    output$collDate <- renderText( {
      collDate <- ""
      ds <- tibble(date = character(),
                   event = character(),
                   cycle = character()
      )
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$collDate) > 0) {
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$collDate) {
          ds <- add_row(ds, date = n$date, event = n$event, cycle = n$cycle)
        }
      }
      cycles = unique(ds$cycle)
      for(c in cycles) {
        subset <- ds %>% filter(cycle == c)
        start <- subset %>% filter(event == "start") %>% select(date)
        end <- subset %>% filter(event == "end") %>% select(date)
        collDate <- paste0(collDate, "&nbsp; &nbsp;&nbsp; &nbsp;<b>", c, ": </b>", start, " to ", end, "<br/><br/>")
      }
      collDate <- substr(collDate, 1, nchar(collDate)-10)
      HTML(collDate)
    })
    
    output$resInstru <- renderText( {
      resInstru <- ""
      if(length(dat()$stdyDscr$method$dataColl$resInstru) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$resInstru) == 1) {
          resInstru <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Research Instrument: </b>"  
        } else {
          resInstru <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Research Instruments: </b>"
        }
        for (n in dat()$stdyDscr$method$dataColl$resInstru) {
          resInstru <- paste0(resInstru, n$value, ", ")
        }
      }
      resInstru <- substr(resInstru, 1, nchar(resInstru)-2)
      HTML(resInstru)
    })
    
    output$instrumentDevelopment <- renderText( {
      instrumentDevelopment <- ""
      if(length(dat()$stdyDscr$method$dataColl$instrumentDevelopment) > 0) {
        instrumentDevelopment <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Instrument Development: </b>"
        for (n in dat()$stdyDscr$method$dataColl$instrumentDevelopment) {
          instrumentDevelopment <- paste0(instrumentDevelopment, n$value, ", ")
        }
      }
      instrumentDevelopment <- substr(instrumentDevelopment, 1, nchar(instrumentDevelopment)-2)
      HTML(instrumentDevelopment)
    })
    
    output$timeMeth <- renderText( {
      timeMethod <- ""
      if(length(dat()$stdyDscr$method$dataColl$timeMeth) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$timeMeth) == 1) {
          timeMethod <- "<b>Time method: </b>"  
        } else {
          timeMethod <- "<b>Time methods: </b>"
        }
        for (n in dat()$stdyDscr$method$dataColl$timeMeth) {
          timeMethod <- paste0(timeMethod, n$value, ", ")
        }
      }
      timeMethod <- substr(timeMethod, 1, nchar(timeMethod)-2)
      HTML(timeMethod)
    })
    
    output$sampProc <- renderText( {
      sampProc <- ""
      if(length(dat()$stdyDscr$method$dataColl$sampProc) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$sampProc) == 1) {
          sampProc <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Sampling Procedure: </b>"  
        } else {
          sampProc <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Sampling Procedure: </b>"
        }
        for (n in dat()$stdyDscr$method$dataColl$sampProc) {
          sampProc <- paste0(sampProc, n$value, ", ")
        }
      }
      sampProc <- substr(sampProc, 1, nchar(sampProc)-2)
      HTML(sampProc)
    })
    
    output$ConOps <- renderText( {
      ConOps <- ""
      if(length(dat()$stdyDscr$method$dataColl$ConOps) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$ConOps) == 1) {
          ConOps <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Control Operation: </b>"  
        } else {
          ConOps <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Control Operations: </b>"
        }
        for (n in dat()$stdyDscr$method$dataColl$ConOps) {
          ConOps <- paste0(ConOps, n$value, ", ")
        }
      }
      ConOps <- substr(ConOps, 1, nchar(ConOps)-2)
      HTML(ConOps)
    })
    
    output$actMin <- renderText( {
      actMin <- ""
      if(length(dat()$stdyDscr$method$dataColl$actMin) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$actMin) == 1) {
          actMin <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Action to Minimize Loss: </b>"  
        } else {
          actMin <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Actions to Minimize Loss: </b>"
        }
        for (n in dat()$stdyDscr$method$dataColl$actMin) {
          actMin <- paste0(actMin, n$value, ", ")
        }
      }
      actMin <- substr(actMin, 1, nchar(actMin)-2)
      HTML(actMin)
    })
    
    output$deviat <- renderText( {
      deviat <- ""
      if(length(dat()$stdyDscr$method$dataColl$deviat) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$deviat) == 1) {
          deviat <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Major Deviation: </b>"  
        } else {
          deviat <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Major Deviations: </b>"
        }
        for (n in dat()$stdyDscr$method$dataColl$deviat) {
          deviat <- paste0(deviat, n$value, ", ")
        }
      }
      deviat <- substr(deviat, 1, nchar(deviat)-2)
      HTML(deviat)
    })
    
    output$frequenc <- renderText( {
      frequenc <- ""
      if(length(dat()$stdyDscr$method$dataColl$frequenc) > 0) {
        frequenc <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Frequency of Collection: </b>"
        for (n in dat()$stdyDscr$method$dataColl$frequenc) {
          frequenc <- paste0(frequenc, n$value, ", ")
        }
      }
      frequenc <- substr(frequenc, 1, nchar(frequenc)-2)
      HTML(frequenc)
    })
    
    output$varGrp <- renderText( {
      groups <- ""
      for (d in dat()$dataDscr$varGrp) {
        groups <- paste0(groups, "<b>", d$name[[1]]$value, "</b><br/>")
        if(length(d$labl) > 0) {
          groups <- paste0(groups, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Label(s): </b>")
          for(l in d$labl) {
            groups <- paste0(groups, l$value, ", ")
          }
          groups <- substr(groups, 1, nchar(groups)-2)
        }
        groups <- paste0(groups, "<br/>")
        if(length(d$defntn) > 0) {
          groups <- paste0(groups, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Definition(s): </b>")
          for(n in d$defntn) {
            groups <- paste0(groups, n$value, ", ")
          }
          groups <- substr(groups, 1, nchar(groups)-2)
          groups <- paste0(groups, "<br/>")
        }
        if(length(d$concept) > 0) {
          groups <- paste0(groups, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Concept(s): </b>")
          for(c in d$concept) {
            groups <- paste0(groups, c$value, ", ")
          }
          groups <- substr(groups, 1, nchar(groups)-2)
          groups <- paste0(groups, "<br/>")
        }
        groups <- paste0(groups, "<br/>")
      }
      groups <- substr(groups, 1, nchar(groups)-10)
      HTML(groups)
    })
  })
}