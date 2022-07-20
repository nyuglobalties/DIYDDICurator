library(shiny)
library(rhandsontable)
library(tidyverse)

loadSupport()

ui <- fluidPage(
  navbarPage(strong("TIES metadata curation tool"),
    tabPanel("Introduction", 
             p('This is the TIES metadata curation tool. It is designed for 
               you, the researcher, to edit, add, and delete descriptive metadata 
               for your projects using tables that allows you edit existing 
               metadata, add or delete metadata by right clicking on the table, 
               and choose appropriate attributes from drop down menus. Each 
               table has a "lang" (language) attribute for translations.'),
             p('In addition, each table has the element definitions below it. In 
               some cases a table may represent more than one element in which 
               case a field column is added to the table.'),
             p(strong('To begin, please pick the project you would like to edit below.')),
             tags$hr(), 
             radioButtons("proj",
                          "Select Project",
                          choices = list.files("data/"))
             ),
    navbarMenu(
      "Project Information",
      title_ui("titles"),
      authors_ui("authors"),
      series_ui("series"),
      producers_ui("producers"),
      funders_ui("funders")
    ),
    navbarMenu(
      "Study Information",
      abstract_ui("abstract"),
      subject_ui("subject"),
      universe_ui("universe"),
      anlyUnit_ui("anlyUnit"),
      dataKind_ui("dataKind"),
      geography_ui("geog"),
      timePeriods_ui("timePrd")
    ),
    navbarMenu(
      "Data Collection",
      timeMeth_ui("timeMeth"),
      frequenc_ui("frequenc"),
      dataCollector_ui("dataCollector"),
      collMode_ui("collMode"),
      collSitu_ui("collSitu"),
      collectorTraining_ui("collectorTraining"),
      resInstru_ui("resInstru"),
      instrumentDevelopment_ui("instrumentDevelopment"),
      ConOps_ui("ConOps"),
      actMin_ui("actMin"),
      sampProc_ui("sampProc"),
      deviat_ui("deviat")
    ),
    varGrp_ui("varGrp"),
        
    navbarMenu(
      "Evaluation/Export",
      readme_generation_ui("readme"),
      ddi_generation_ui("ddi")
    )
  )
)

server <- function(input, output, session) {
  
  filepth <- reactive({
    r <- paste0("data/", input$proj)
    return(r)
  })
  
  init_dat <- reactiveFileReader(intervalMillis = 1000, 
                            session, 
                            filePath = filepth,
                            readFunc = yaml::read_yaml
                          )
  
  dat <- reactive(recurse_read(init_dat()))

  
  # add projectinformation servers here
  title_server("titles", dat, filepth)
  authors_server("authors", dat, filepth)
  series_server("series", dat, filepth)
  producers_server("producers", dat, filepth)
  funders_server("funders", dat, filepth)
  
  abstract_server("abstract", dat, filepth)
  subject_server("subject", dat, filepth)
  universe_server("universe", dat, filepth)
  anlyUnit_server("anlyUnit", dat, filepth)
  dataKind_server("dataKind", dat, filepth)
  geography_server("geog", dat, filepth)
  timePeriods_server("timePrd", dat, filepth)

  timeMeth_server("timeMeth", dat, filepth)  
  frequenc_server("frequenc", dat, filepth)
  dataCollector_server("dataCollector", dat, filepth)  
  collectorTraining_server("collectorTraining", dat, filepth)
  collMode_server("collMode", dat, filepth)
  collSitu_server("collSitu", dat, filepth)
  resInstru_server("resInstru", dat, filepth)
  instrumentDevelopment_server("instrumentDevelopment", dat, filepth)
  ConOps_server("ConOps", dat, filepth)
  actMin_server("actMin", dat, filepth)
  sampProc_server("sampProc", dat, filepth)
  deviat_server("deviat", dat, filepth)
  
  varGrp_server("varGrp", dat, filepth)
  ddi_generation_server("ddi", dat, filepth)
  readme_generation_server("readme", dat)
}

shinyApp(server = server, ui = ui)