#' create the server functions
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @export

VCFserver <- function(input, output, session) {
  require(ggplot2)
  require(ggpubr)
  require(dplyr)
  require(circlize)
  require(tibble)
  require(pheatmap)

  source(system.file("shiny", "modules/Upload_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/1-1.summarise_barplot_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/1-2.SNP_Analysis_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/1-3.Indel_Analysis_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/1-4.Venn_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/1-5.circle_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/2-1.Distribution_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/2-2.VariantsGene_Summarise_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/2-3.VariantsGene_Heatmap_server.R", package = "VCFshiny"), local = T)
}
