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

  require(GenomicRanges)
  require(IRanges)
  require(TxDb.Hsapiens.UCSC.hg38.knownGene)
  require(TxDb.Hsapiens.UCSC.hg19.knownGene)
  require(org.Hs.eg.db)
  require(musicatk)

  source(system.file("shiny", "modules/1.Upload_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/2.Venn_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/3.summarise_barplot_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/4.SNP_Analysis_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/5.Indel_Analysis_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/6.circle_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/7.Distribution_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/8.VariantsGene_Summarise_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/9.VariantsGene_Heatmap_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/10.mutational_signature_server.R", package = "VCFshiny"), local = T)
  source(system.file("shiny", "modules/11.Annotation_server.R", package = "VCFshiny"), local = T)
}
