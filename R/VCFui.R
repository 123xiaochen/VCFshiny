#' Build shiny UI page
#'
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets useShinydashboard
#' @importFrom shiny shinyUI fluidPage navbarPage tabPanel icon includeHTML br hr navbarMenu fluidRow strong p tags
#' @importFrom shinydashboard box
#' @importFrom shinyBS bsModal
#'
#' @export
#'
VCFui <- function() {
  require(DT)
  require(shiny)
  require(shinyBS)
  require(shinyWidgets)
  require(shinydashboard)



  dashboardPage(
    # skin = "black",
    #Header -------------------------------------------------------------------------------------------------------------------------------
    dashboardHeader(title ="VCF Analyse" , titleWidth  = 250),
    #Sider---------------------------------------------------------------------------------------------------------------------------------
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        menuItem("Introduction",tabName = "introduction",icon = icon("home")),
        menuItem("Data Input", icon = icon("upload"), tabName = "datainput"),
        menuItem("Venn Diagram", icon = icon("chart-bar"), tabName = "venn_analyse"),
        menuItem("Variants Number", icon = icon("chart-bar"), tabName = "ALL_variants"),
        menuItem("SNVs Exploration", icon = icon("chart-bar"), tabName = "snp_analysis"),
        menuItem("Indels Exploration", icon = icon("chart-bar"), tabName = "Indel_analysis"),
        menuItem("Genome Circos Plot", icon = icon("chart-bar"), tabName = "circle"),
        menuItem("Genomic Features",icon = icon("chart-bar"), tabName = "distribution"),
        menuItem("Variants Relevant Genes", icon = icon("chart-bar"), tabName = "Variants_summarise"),
        menuItem("Cancer Driver Genes", icon = icon("chart-bar"), tabName = "Variants_heatmap"),
        menuItem("Data Annotation", icon = icon("book"), tabName = "dataAnno")
      )
    ),

    #BODY --------------------------------------------------------------------------------------------------------------------------------------------------
    dashboardBody(
      includeCSS(path = system.file("shiny", "www/custom.css", package = "VCFshiny")),
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
      ### 读入数据 ---------------------------------------------------------------------------------------------------------
      tabItems(
        source(system.file("shiny", "modules/0.Introduction.R", package = "VCFshiny"), local = T)$value,
        source(system.file("shiny", "modules/1.Upload_ui.R", package = "VCFshiny"), local = T)$value,
        ### 第一部分 ---------------------------------------------------------------------------------------------------------------------------------------------
        source(system.file("shiny", "modules/2.Venn_ui.R", package = "VCFshiny"), local = T)$value,
        source(system.file("shiny", "modules/3.summarise_barplot_ui.R", package = "VCFshiny"), local = T)$value,
        source(system.file("shiny", "modules/4.SNP_Analysis_ui.R", package = "VCFshiny"), local = T)$value,
        source(system.file("shiny", "modules/5.Indel_Analysis_ui.R", package = "VCFshiny"), local = T)$value,
        source(system.file("shiny", "modules/6.circle_ui.R", package = "VCFshiny"), local = T)$value,
        ### 第二部分 --------------------------------------------------------------------------------------------------------------------------------
        source(system.file("shiny", "modules/7.Distribution_ui.R", package = "VCFshiny"), local = T)$value,
        source(system.file("shiny", "modules/8.VariantsGene_Summarise_ui.R", package = "VCFshiny"), local = T)$value,
        source(system.file("shiny", "modules/9.VariantsGene_Heatmap_ui.R", package = "VCFshiny"), local = T)$value,
        source(system.file("shiny", "modules/10.Annotation_ui.R", package = "VCFshiny"), local = T)$value
      )
    )
  )
}
