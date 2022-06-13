ddi_generation_ui <- function(id) {
  ns <- NS(id)
  tabPanel("DDI Codebook",
           htmltidy::xmlviewOutput(ns("XML"))
  )
}

ddi_generation_server <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    output$XML <- htmltidy::renderXmlview({
      req(dat())
      htmltidy::xml_view(generate_ddi_codebook(dat()))
    })
  })
}