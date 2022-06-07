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
                          choices = c("Sample - Project 1" = "samp1", 
                                      "Sample - Project 2" = "samp2"),
                          selected = "samp1")
             ),
    projectInformation_ui("projInfo"),
    navbarMenu(
      "Study Information",
      abstract_ui("abstract"),
      subject_ui("subject"),
      universe_ui("universe"),
      anlyUnit_ui("anlyUnit"),
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
      resInstru_ui("resInstru"),
      instrumentDevelopment_ui("instrumentDevelopment"),
      ConOps_ui("ConOps"),
      actMin_ui("actMin"),
      sampProc_ui("sampProc"),
      deviat_ui("deviat")
    ),
    varGrp_ui("varGrp"),
        
    tabPanel("Evaluation/Export",
             p("This section will allow the researcher/s to export the existing 
               metadata into a file using a .Rmd template. I might want them to 
               be able to pick elements to include OR include elements from 
               multiple projects. This way the metadata can serve as a resource 
               for other writing."),
             p("In addition, this section will evaluate the metadata 
               programmatically according to a rubric so that we know the curation
               quality of the metadata.")
             )
  )
)

server <- function(input, output, session) {
  
  filepth <- reactive({
    r <- switch(input$proj,
                samp1 = "data/metadata_sample1.yml",
                samp2 = "data/metadata_sample2.yml")
    return(r)
  })
  
  dat <- reactiveFileReader(intervalMillis = 1000, 
                            session, 
                            filePath = filepth,
                            readFunc = yaml::read_yaml
                          )
  
  projectInformation_server("projInfo", dat)
  
  abstract_server("abstract", dat, filepth)
  subject_server("subject", dat, filepth)
  universe_server("universe", dat, filepth)
  anlyUnit_server("anlyUnit", dat, filepth)
  geography_server("geog", dat, filepth)
  timePeriods_server("timePrd", dat, filepth)

  timeMeth_server("timeMeth", dat, filepth)  
  frequenc_server("frequenc", dat, filepth)
  dataCollector_server("dataCollector", dat, filepth)  
  collMode_server("collMode", dat, filepth)
  collSitu_server("collSitu", dat, filepth)
  resInstru_server("resInstru", dat, filepth)
  instrumentDevelopment_server("instrumentDevelopment", dat, filepth)
  ConOps_server("ConOps", dat, filepth)
  actMin_server("actMin", dat, filepth)
  sampProc_server("sampProc", dat, filepth)
  deviat_server("deviat", dat, filepth)
  
  varGrp_server("varGrp", dat, filepth)
}

shinyApp(server = server, ui = ui)