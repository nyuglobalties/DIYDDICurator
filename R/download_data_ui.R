download_data_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Download Data",
           downloadButton(ns("download_data"), "Download data")
  )
}

download_data_server <- function(id, dat) {
  moduleServer(id, function(input, output, session) {
    output$download_data <- downloadHandler(
      filename = paste0("dataDownload-", gsub("-", "", Sys.Date()), ".yml"),
      content = function(file) {
        yaml::write_yaml(dat(), file)
      },
      contentType = "text/yml"
    )
  })
}