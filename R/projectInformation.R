projectInformation_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Project Information", 
           h1(textOutput(ns("title"))),
           h3(textOutput(ns("authors"))),
           tags$hr(),
           h4(textOutput(ns("series"))),
           tags$em(textOutput(ns("seriesInfo"))),
           tags$hr(),
           h5(textOutput(ns("data_manager"))),
           h5(textOutput(ns("data_curator"))),
           tags$hr(),
           h6(textOutput(ns("producers"))),
           tags$br(), tags$br(), tags$br(), tags$br(), tags$hr(),
           p("This section holds all administrative data/Data Team data."),
           p("An important section in here will be quality control status and 
               data level (whether from a curation standpoint the data is suitable 
               for internal use, private sharing, replication/verification, or reuse."))
}

projectInformation_server <- function(id, file) {
  moduleServer(id, function(input, output, session) {
    
    output$title <- renderText( {
      title <- file()$stdyDscr$citation$titlStmt$titl
    })
    
    output$authors <- renderText( {
      authors <- character()
      if(length(file()$stdyDscr$citation$rspStmt$AuthEnty) > 0) {
        for(a in file()$stdyDscr$citation$rspStmt$AuthEnty) {
          authors <- paste0(authors, a$name, " (", a$affiliation, "), ")
        }
      }
      authors <- substr(authors, 1, nchar(authors)-2)
    })
    
    output$series <- renderText( {
      out <- ""
      if(length(file()$stdyDscr$citation$serStmt$serName) > 0) {
        out <- paste0("A ", file()$stdyDscr$citation$serStmt$serName[[1]]$value, 
                      " (", file()$stdyDscr$citation$serStmt$serName[[1]]$abbr, 
                      ") project")
      } 
      out
    })
    
    output$seriesInfo <- renderText( {
      out <- ""
      if(length(file()$stdyDscr$citation$serStmt$serInfo) > 0) {
        out <- file()$stdyDscr$citation$serStmt$serInfo[[1]]$value
      } 
      out
    })
    
    output$data_manager <- renderText( {
      manager <- "Data Manager: "
      if(length(file()$stdyDscr$citation$rspStmt$othId) > 0) {
        for(a in file()$stdyDscr$citation$rspStmt$othId) {
          if(a$role == "Data Manager") manager <- paste0(manager, a$name, 
                                                         " (", a$affiliation, "), ")
        }
      }
      manager <- substr(manager, 1, nchar(manager)-2)
    })
    
    output$data_curator <- renderText( {
      curator <- "Data Curator: "
      if(length(file()$stdyDscr$citation$rspStmt$othId) > 0) {
        for(a in file()$stdyDscr$citation$rspStmt$othId) {
          if(a$role == "Curator") curator <- paste0(curator, a$name, 
                                                    " (", a$affiliation, "), ")
        }
      }
      curator <- substr(curator, 1, nchar(curator)-2)
    })
    
    output$producers <- renderText( {
      producers <- ""
      if(length(file()$stdyDscr$citation$prodStmt$producer) > 0) {
        producers <- "Produced by: "
        for(a in file()$stdyDscr$citation$prodStmt$producer) {
          producers <- paste0(producers, a$name, " (", a$abbr, "), ")
        }
      }
      producers <- paste0(substr(producers, 1, nchar(producers)-2), " - ", 
                          file()$stdyDscr$citation$prodStmt$prodPlac)    
    })
  })
}