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
             p(strong('To begin, please pick the project you would like to edit 
                      below or create a new file.')),
             tags$hr(), 
             uiOutput("project"),
             tags$hr(),
             textInput("newFileName", label = "New file name", placeholder = "Type new file name here"),
             tags$em('When creating a new file please use snake_case (dashes instead 
               of blanks) or camelCase (no blanks but uppercase the first letter 
               of the non-first word.'),
             tags$br(),
             tags$br(),
             actionButton("createNewFile", "Create new file")
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
    r <- paste0("data/", input$project)
    return(r)
  })
  
  init_dat <- reactiveFileReader(intervalMillis = 1000, 
                            session, 
                            filePath = filepth,
                            readFunc = yaml::read_yaml
                          )
  
  output$project <- renderUI(
    radioButtons("project", 
                 label = "Select Project",
                 choices = list.files("data/")
                 )
  )
  
  dat <- reactive(recurse_read(init_dat()))
  
  lang <- isolate(c("", "en - English", "fr - Français", "es - Español", 
                    "ar - عربى", "zh - 中国人", "ru - Русский"))
  
  observeEvent(
    input$createNewFile, {
      isolate({
        req(input$newFileName) 
        name <- str_replace_all(input$newFileName, " ", "_")
        if(!stringr::str_detect(input$newFileName, "[.]yml$")) {
          name <- paste0(name, ".yml")
        }
        file.copy("template.yml", "data/template.yml")
        file.rename("data/template.yml", paste0("data/", name))
        
        updateRadioButtons(session = session,
                           inputId = "project",
                           label = "Select Project",
                           choices = list.files("data/"))
      })
    })
  
  # add projectinformation servers here
  title_server("titles", dat, filepth, lang)
  authors_server("authors", dat, filepth)
  series_server("series", dat, filepth, lang)
  producers_server("producers", dat, filepth)
  funders_server("funders", dat, filepth)
  
  abstract_server("abstract", dat, filepth, lang)
  subject_server("subject", dat, filepth, lang)
  universe_server("universe", dat, filepth, lang)
  anlyUnit_server("anlyUnit", dat, filepth, lang)
  dataKind_server("dataKind", dat, filepth, lang)
  geography_server("geog", dat, filepth, lang)
  timePeriods_server("timePrd", dat, filepth, lang)

  timeMeth_server("timeMeth", dat, filepth, lang)  
  frequenc_server("frequenc", dat, filepth, lang)
  dataCollector_server("dataCollector", dat, filepth, lang)  
  collectorTraining_server("collectorTraining", dat, filepth, lang)
  collMode_server("collMode", dat, filepth, lang)
  collSitu_server("collSitu", dat, filepth, lang)
  resInstru_server("resInstru", dat, filepth, lang)
  instrumentDevelopment_server("instrumentDevelopment", dat, filepth, lang)
  ConOps_server("ConOps", dat, filepth, lang)
  actMin_server("actMin", dat, filepth, lang)
  sampProc_server("sampProc", dat, filepth, lang)
  deviat_server("deviat", dat, filepth, lang)
  
  varGrp_server("varGrp", dat, filepth, lang)
  ddi_generation_server("ddi", dat, filepth)
  readme_generation_server("readme", dat)
}

shinyApp(server = server, ui = ui)