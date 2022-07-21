readme_generation_ui <- function(id) {
  ns <- NS(id)
  tabPanel("README",
           h1(textOutput(ns("title"))),
           htmlOutput(ns("authors")),
           htmlOutput(ns("contributors")),
           htmlOutput(ns("production")),
           htmlOutput(ns("funders")),
           htmlOutput(ns("series")),
           htmlOutput(ns("seriesInfo")),
           
           htmlOutput(ns("abstract")),
           htmlOutput(ns("keywords")),
           htmlOutput(ns("universes")),
           htmlOutput(ns("anlyUnits")),
           htmlOutput(ns("kindOfData")),
           htmlOutput(ns("nation")),
           htmlOutput(ns("geogCoverage")),
           htmlOutput(ns("geogUnit")),
           htmlOutput(ns("timePrd")),
           
           htmlOutput(ns("dataCollector")),
           htmlOutput(ns("collMode")),
           htmlOutput(ns("collectorTraining")),
           htmlOutput(ns("collSitu")),
           htmlOutput(ns("timeMeth")),
           htmlOutput(ns("collDate")),
           htmlOutput(ns("resInstru")),
           htmlOutput(ns("instrumentDevelopment")),
           htmlOutput(ns("sampProc")),
           htmlOutput(ns("ConOps")),
           htmlOutput(ns("actMin")),
           htmlOutput(ns("deviat")),
           htmlOutput(ns("frequenc")),
           
           htmlOutput(ns("varGrp"))
  )
}

readme_generation_server <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    output$title <- renderText( {
      title <- dat()$stdyDscr$citation$titlStmt$titl[[1]]$value
    })
    
    output$authors <- renderText( {
      authors <- character()
      if(length(dat()$stdyDscr$citation$rspStmt$AuthEnty) > 0) {
        authors <- "<hr><h4>Authors:</h4><ul>"
        for(a in dat()$stdyDscr$citation$rspStmt$AuthEnty) {
          if(!is.null(a$affiliation)) {
            authors <- paste0(authors, "<li>", a$name, " (", a$affiliation, ")</li>")
          } else {
            authors <- paste0(authors, "<li>",a$name, "</li>")
          }
        }
      }
      authors <- paste0(authors, "</ul>")
    })
    
    output$series <- renderText( {
      out <- ""
      if(length(dat()$stdyDscr$citation$serStmt) > 0) {
        out <- "<hr>"
        if(length(dat()$stdyDscr$citation$serStmt) == 1) {
          if(!is.null(dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$abbr)) {
            out <- paste0(out, "<h4>A <b>", dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$value, 
                          " (", dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$abbr, 
                          ")</b> project</h4>")
          } else {
            out <- paste0(out, "<h4>A <b>", dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$value, 
                          "</b> project</h4>")
          }
        } else if(length(dat()$stdyDscr$citation$serStmt) == 2) {
          if(!is.null(dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$abbr) &
             !is.null(dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$abbr)) {
            out <- paste0(out, "<h4>A <b>", dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$value, 
                          " (", dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$abbr, 
                          ")</b> and <b>", dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$value, 
                          " (", dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$abbr, ")</b> project</h4>")
          } else if(is.null(dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$abbr) &
                    !is.null(dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$abbr)) {
            out <- paste0(out, "<h4>A <b>", dat()$stdyDscr$citation$serStmt[[1]]$serName$value, 
                          "</b> and <b>", dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$value, 
                          " (", dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$abbr, ")</b> project</h4>")
          } else if(!is.null(dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$abbr) &
                     is.null(dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$abbr)) {
            out <- paste0(out, "<h4>A <b>", dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$value, 
                          " (", dat()$stdyDscr$citation$serStmt[[1]]$serName[[1]]$abbr, 
                          ")</b> and <b>", dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$value, 
                          "</b> project</h4>")
          } else {
            out <- paste0(out, "<h4>A <b>", dat()$stdyDscr$citation$serStmt[[1]]$serName$value, 
                          "</b> and <b>", dat()$stdyDscr$citation$serStmt[[2]]$serName[[1]]$value, 
                          "</b> project</h4>")
          }
        } else {
          out <- "<h4>A "
          counter = 1
          for(s in dat()$stdyDscr$citation$serStmt) {
            if(counter == length(dat()$stdyDscr$citation$serStmt) - 1) {
              if(!is.null(s$serName[[1]]$abbr)) {
                out <- paste0(out, "<b>", s$serName[[1]]$value, "(", s$serName[[1]]$abbr, ")</b>, and ")
              } else {
                out <- paste0(out, "<b>", s$serName[[1]]$value, "</b>, and ")
              }
            } else if(counter == length(dat()$stdyDscr$citation$serStmt)) {
              if(!is.null(s$serName[[1]]$abbr)) {
                out <- paste0(out, "<b>", s$serName[[1]]$value, "(", s$serName[[1]]$abbr, ")</b>")
              } else {
                out <- paste0(out, "<b>", s$serName[[1]]$value, "</b>")
              }
            } else {
              if(!is.null(s$serName[[1]]$abbr)) {
                out <- paste0(out, "<b>", s$serName[[1]]$value, "(", s$serName[[1]]$abbr, ")</b>, ")
              } else {
                out <- paste0(out, "<b>", s$serName[[1]]$value, "</b>, ")
              }
            }
            counter <- counter + 1
          }
          out <- paste0(out, " project</h4>")
        }
      } 
      HTML(out)
    })
    
    output$seriesInfo <- renderText( {
      out <- ""
      if(length(dat()$stdyDscr$citation$serStmt) == 1) {
        for(i in dat()$stdyDscr$citation$serStmt[[1]]$serInfo) {
          out <- paste0(out, "<br>", i$value, "<br>")
        }
        out <- substr(out, 1, nchar(out)-5)
      } else if(length(dat()$stdyDscr$citation$serStmt) > 1) {
        for(series in dat()$stdyDscr$citation$serStmt) {
          if(!is.null(series$serInfo)) {
            out <- paste0(out, "<br><em>", series$serName[[1]]$value, ":</em><br>")
          }
          for(i in series$serInfo) {
            out <- paste0(out, "<br>", i$value, "<br>")
          }
        }
      }
      HTML(out)
    })
    
    output$contributors <- renderText( {
      contribs <- ""
      if(length(dat()$stdyDscr$citation$rspStmt$othId) > 0) {
        contribs <- "<em>Other support provided by:</em><br>"
        for(a in dat()$stdyDscr$citation$rspStmt$othId) {
          if(!is.null(a$role) & !is.null(a$affiliation)) {
            contribs <- paste0(contribs,  a$name, " (", a$affiliation, ") - ",
                               a$role, "</br>")
          } else if(!is.null(a$role) & is.null(a$affiliation)) {
            contribs <- paste0(contribs, a$name, " - ", a$role, "</br>")
          } else if(is.null(a$role) & !is.null(a$affiliation)) {
            contribs <- paste0(contribs,  a$name, " (", a$affiliation, ")</br>")
          } else {
            contribs <- paste0(contribs,  a$name, "</br>")
          }
        }
      }
      if(nchar(contribs) > 0) contribs <- substr(contribs, 1, nchar(contribs)-5)
    })
    
    output$production <- renderText( {
      producers <- "<br>"
      if(length(dat()$stdyDscr$citation$prodStmt$producer) > 0) {
        producers <- paste0(producers, "<b>Produced by: </b>")
        for(a in dat()$stdyDscr$citation$prodStmt$producer) {
          if(!is.null(a$abbr)) {
            producers <- paste0(producers, a$name, " (", a$abbr, "), ")
          } else {
            producers <- paste0(producers, a$name, ", ")
          }
        }
      }
      if(nchar(producers) > 0) {
        producers <- substr(producers, 1, nchar(producers)-2)
        producers <- paste0(producers, "<br/>")
      }
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
      if(producers == "<br>") producers <- ""
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
      if(length(dat()$stdyDscr$stdyInfo$abstract) > 0 | 
         length(dat()$stdyDscr$stdyInfo$subject) > 0 |
         length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) > 0 |
         length(dat()$stdyDscr$stdyInfo$sumDscr$universe) > 0 |
         length(dat()$stdyDscr$stdyInfo$sumDscr$nation) > 0 |
         length(dat()$stdyDscr$stdyInfo$sumDscr$geogCover) > 0 |
         length(dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) > 0 |
         length(dat()$stdyDscr$stdyInfo$sumDscr$timePrd) > 0 |
         length(dat()$stdyDscr$stdyInfo$sumDscr$dataKind) > 0) { 
        abstract <- paste0(abstract, "<hr><h3>Study Information</h3></hr>")
      }
      
      if(length(dat()$stdyDscr$stdyInfo$abstract) > 0) {
        abstract <- paste0(abstract, "<h4>Abstract or Purpose</h4>")
        for (a in dat()$stdyDscr$stdyInfo$abstract) {
          abstract <- paste0(abstract, a$value, "<br/><br/>")
        }
        abstract <- substr(abstract, 1, nchar(abstract)-10)
      }
      HTML(abstract)
    })
    
    output$keywords <- renderText( {
      keywords <- ""
      if(length(dat()$stdyDscr$stdyInfo$subject) > 0) {
        keywords <- "<h4>Keywords</h4>"
        keywords <- paste0(keywords, "&nbsp; &nbsp;&nbsp; &nbsp;")
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
        if(length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) > 0) {
          universe <- "<h4>Universes and Units of Analysis</h4> &nbsp; &nbsp;&nbsp; &nbsp;<b>Universe: </b>"
        } else {
          universe <- "<h4>Universes</h4> &nbsp; &nbsp;&nbsp; &nbsp;"
        }
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$universe) {
          if(!is.null(n$clusion)) {
            if(n$clusion == "E") {
              universe <- paste0(universe, n$group, " (excluding), ")
            } else {
              universe <- paste0(universe, n$group, ", ")
            }
          } else {
            universe <- paste0(universe, n$group, ", ")
          }
        }
      }
      universe <- substr(universe, 1, nchar(universe)-2)
      HTML(universe)
    })
    
    output$anlyUnits <- renderText( {
      anlyUnit <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) > 0) {
        if(length(dat()$stdyDscr$stdyInfo$sumDscr$universe) == 0) {
          if(length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) == 1) {
            anlyUnit <- "<h4>Unit of Analysis</h4> &nbsp; &nbsp;&nbsp; &nbsp;"  
          } else {
            anlyUnit <- "<h4>Units of Analysis</h4> &nbsp; &nbsp;&nbsp; &nbsp;"
          }
        } else {
          if(length(dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) == 1) {
            anlyUnit <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Unit of Analysis: </b>"  
          } else {
            anlyUnit <- "&nbsp; &nbsp;&nbsp; &nbsp;<b>Units of Analysis: </b>"
          }
        }
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$anlyUnit) {
          anlyUnit <- paste0(anlyUnit, n$group, ", ")
        }
      }
      anlyUnit <- substr(anlyUnit, 1, nchar(anlyUnit)-2)
      HTML(anlyUnit)
    })

    output$kindOfData <- renderText( {
      kindOfData <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$dataKind) > 0) {
        if(length(dat()$stdyDscr$stdyInfo$sumDscr$dataKind) == 1) {
          kindOfData <- "<h4>Kind of Data</h4> &nbsp; &nbsp;&nbsp; &nbsp;"  
        } else {
          kindOfData <- "<h4>Kinds of Data</h4> &nbsp; &nbsp;&nbsp; &nbsp;"            
        }
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$dataKind) {
          kindOfData <- paste0(kindOfData, n$value, ", ")
        }
      }
      kindOfData <- substr(kindOfData, 1, nchar(kindOfData)-2)
      HTML(kindOfData)
    })
    
    output$nation <- renderText( {
      nation <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$nation) > 0) {
        nation <- "<h4>Geography</h4>"
        if(length(dat()$stdyDscr$stdyInfo$sumDscr$nation) == 1) {
          nation <- paste0(nation, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Nation: </b>")  
        } else {
          nation <- paste0(nation, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Nations: </b>")
        }
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$nation) {
          if(!is.null(n$abbr)) {
            nation <- paste0(nation, n$value, " (", n$abbr, "), ")
          } else {
            nation <- paste0(nation, n$value, ", ")
          }
        }
      }
      nation <- substr(nation, 1, nchar(nation)-2)
      HTML(nation)
    })
    
    output$geogCoverage <- renderText( {
      geogCover <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$geogCover) > 0 &
         length(dat()$stdyDscr$stdyInfo$sumDscr$nation) == 0) {
        geogCover <- "<h4>Geography</h4>"
      } 
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$geogCover) > 0) {
        geogCover <- paste0(geogCover, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Geographic coverage: </b>")
        for (n in dat()$stdyDscr$stdyInfo$sumDscr$geogCover) {
          geogCover <- paste0(geogCover, n$value, ", ")
        }
      }
      geogCover <- substr(geogCover, 1, nchar(geogCover)-2)
      HTML(geogCover)
    })
    
    output$geogUnit <- renderText( {
      geogUnit <- ""
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) > 0 &
         length(dat()$stdyDscr$stdyInfo$sumDscr$geogCover) == 0 &
         length(dat()$stdyDscr$stdyInfo$sumDscr$nation) == 0) {
        geogUnit <- "<h4>Geography</h4>"
      } 
      if(length(dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) > 0) {
        if(length(dat()$stdyDscr$stdyInfo$sumDscr$geogUnit) == 1) {
          geogUnit <- paste0(geogUnit, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Geographic Unit: </b>")  
        } else {
          geogUnit <- paste0(geogUnit, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Geographic Units: </b>")
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
        timePrd <- "<h4>Time periods</h4>"
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
      if(length(dat()$stdyDscr$method$dataColl$dataCollector) > 0 |
         length(dat()$stdyDscr$method$dataColl$collMode) > 0 |
         length(dat()$stdyDscr$method$dataColl$collectorTraining) > 0 |
         length(dat()$stdyDscr$method$dataColl$collSitu) > 0 |
         length(dat()$stdyDscr$method$dataColl$timeMeth) > 0 |
         length(dat()$stdyDscr$stdyInfo$sumDscr$collDate) > 0 |
         length(dat()$stdyDscr$method$dataColl$resInstru) > 0 |
         length(dat()$stdyDscr$method$dataColl$instrumentDevelopment) > 0 |
         length(dat()$stdyDscr$method$dataColl$sampProc) > 0 |
         length(dat()$stdyDscr$method$dataColl$ConOps) > 0 |
         length(dat()$stdyDscr$method$dataColl$actMin) > 0 |
         length(dat()$stdyDscr$method$dataColl$deviat) > 0 |
         length(dat()$stdyDscr$method$dataColl$frequenc) > 0) {
        collectors <- paste0(collectors, "<hr><h3>Data Collection</h3><hr>")
      }
      if(length(dat()$stdyDscr$method$dataColl$dataCollector) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$dataCollector) == 1) {
          collectors <- paste0(collectors, "<b>Data Collector: </b>")  
        } else {
          collectors <- paste0(collectors, "<b>Data Collectors: </b>")
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

    output$collectorTraining <- renderText( {
      training <- ""
      if(length(dat()$stdyDscr$method$dataColl$collectorTraining) > 0) {
        training <- "<b>Collector Training: </b>"  
        for (n in dat()$stdyDscr$method$dataColl$collectorTraining) {
          training <- paste0(training, n$value, ", ")
        }
      }
      training <- substr(training, 1, nchar(training)-2)
      HTML(training)
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
        collDate <- "<h4>Collection Dates</h4>"
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
        resInstru <- "<h4>Research Instrument</h4>"
        if(length(dat()$stdyDscr$method$dataColl$resInstru) == 1) {
          resInstru <- paste0(resInstru, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Research Instrument: </b>")  
        } else {
          resInstru <- paste0(resInstru, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Research Instruments: </b>")
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
      if(length(dat()$stdyDscr$method$dataColl$instrumentDevelopment) > 0 &
         length(dat()$stdyDscr$method$dataColl$resInstru) == 0) {
        instrumentDevelopment <- "<h4>Research Instrument</h4>"
      }
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
        sampProc <- "<h4>Sampling Procedure</h4>"
        if(length(dat()$stdyDscr$method$dataColl$sampProc) == 1) {
          sampProc <- paste0(sampProc, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Sampling Procedure: </b>")  
        } else {
          sampProc <- paste0(sampProc, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Sampling Procedure: </b>")
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
      if(length(dat()$stdyDscr$method$dataColl$ConOps) > 0 &
         length(dat()$stdyDscr$method$dataColl$sampProc) == 0) {
        ConOps <- "<h4>Sampling Procedure</h4>"
      }
      if(length(dat()$stdyDscr$method$dataColl$ConOps) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$ConOps) == 1) {
          ConOps <- paste0(conOps, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Control Operation: </b>")  
        } else {
          ConOps <- paste0(conOps, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Control Operations: </b>")
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
      if(length(dat()$stdyDscr$method$dataColl$actMin) > 0 &
         length(dat()$stdyDscr$method$dataColl$ConOps) == 0 &
         length(dat()$stdyDscr$method$dataColl$sampProc) == 0) {
        actMin <- "<h4>Sampling Procedure</h4>"
      }
      if(length(dat()$stdyDscr$method$dataColl$actMin) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$actMin) == 1) {
          actMin <- paste0(actMin, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Action to Minimize Loss: </b>")  
        } else {
          actMin <- paste0(actMin, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Actions to Minimize Loss: </b>")
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
      if(length(dat()$stdyDscr$method$dataColl$deviat) > 0 &
         length(dat()$stdyDscr$method$dataColl$actMin) == 0 &
         length(dat()$stdyDscr$method$dataColl$ConOps) == 0 &
         length(dat()$stdyDscr$method$dataColl$sampProc) == 0) {
        deviat <- "<h4>Sampling Procedure</h4>"
      }
      if(length(dat()$stdyDscr$method$dataColl$deviat) > 0) {
        if(length(dat()$stdyDscr$method$dataColl$deviat) == 1) {
          deviat <- paste0(deviat, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Major Deviation: </b>")  
        } else {
          deviat <- paste0(deviate, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Major Deviations: </b>")
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
      if(length(dat()$stdyDscr$method$dataColl$frequenc) > 0 & 
         length(dat()$stdyDscr$method$dataColl$deviat) == 0 &
         length(dat()$stdyDscr$method$dataColl$actMin) == 0 &
         length(dat()$stdyDscr$method$dataColl$ConOps) == 0 &
         length(dat()$stdyDscr$method$dataColl$sampProc) == 0) {
        frequenc <- "<h4>Sampling Procedure</h4>"
      }
        
      if(length(dat()$stdyDscr$method$dataColl$frequenc) > 0) {
        frequenc <- paste0(frequenc, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Frequency of Collection: </b>")
        for (n in dat()$stdyDscr$method$dataColl$frequenc) {
          frequenc <- paste0(frequenc, n$value, ", ")
        }
      }
      frequenc <- substr(frequenc, 1, nchar(frequenc)-2)
      HTML(frequenc)
    })
    
    output$varGrp <- renderText( {
      groups <- ""
      if(length(dat()$dataDscr$varGrp) > 0) {
        groups <- "<hr><h3>Variable Groups</h3><hr>"
      }
      for (d in dat()$dataDscr$varGrp) {
        groups <- paste0(groups, "<b>", d$name[[1]], ":</b><br/>")
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
        if(length(d$universe) > 0) {
          groups <- paste0(groups, "&nbsp; &nbsp;&nbsp; &nbsp;<b>Universe(s): </b>")
          for(n in d$universe) {
            if(!is.null(n$clusion) & n$clusion == "E") {
              groups <- paste0(groups, n$group, " (excluding), ")
            } else {
              groups <- paste0(groups, n$group, ", ")
            }
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