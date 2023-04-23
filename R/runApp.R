#' Start the VCF shiny App
#'
#' @importFrom dplyr %>%
#' @importFrom shiny shinyApp runApp addResourcePath
#' @export
#'

startVCFshiny <- function(launch.browser = TRUE, port = getOption("shiny.port"), host = getOption("shiny.host", "127.0.0.1")) {

  # set upload file size limit as 100MB
  options(shiny.maxRequestSize = 1000 * 1024^2, warn = -1, shiny.sanitize.errors = TRUE)

  # addResourcePath(prefix = "Annovar", directoryPath = system.file("Annovar", ".", package = "VCFshiny"))

  shinyApp(ui = VCFui, server = VCFserver) %>% runApp(launch.browser = launch.browser, host = host, port = port)
}
