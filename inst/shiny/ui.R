source("./library.R")
library(shinyBS)
ui <- dashboardPage(
  # skin = "black",
#Header -------------------------------------------------------------------------------------------------------------------------------
  dashboardHeader(title ="WGS Analyse" , titleWidth  = 250),
#Sider---------------------------------------------------------------------------------------------------------------------------------
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Introduction",tabName = "introduction",icon = icon(name = "glyphicon glyphicon-comment", lib = "glyphicon")),
      menuItem("Data Input", icon = icon(name = "glyphicon glyphicon-open",lib = "glyphicon"),tabName = "datainput"),
      menuItem(
        "General Analysis", icon = icon(name = "glyphicon glyphicon-option-vertical",lib = "glyphicon"), startExpanded = TRUE,
        menuItem("ALL Snp+Indel Summarise", tabName = "ALL_variants",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("SNP Snp Heatmap Analysis", tabName = "snp_heatmap",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("ALL Snp Type Analysis", tabName = "snp_all_type",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("Snp/Indel Cross-Analysis", tabName = "venn_analyse",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("Snp/Indel Circos Plot Analysis", tabName = "circle",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon"))
      ),
      menuItem(
        "Extension Analysis", icon = icon(name = "glyphicon glyphicon-option-vertical",lib = "glyphicon"),  startExpanded = TRUE,
        menuItem("Variants Position Analysis", tabName = "distribution",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("All Samples Variants Summarise", tabName = "Variants_summarise",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("Variants Heamap Analysis", tabName = "Variants_heatmap",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("Variants GO/KEGG Enrich Analysis", tabName = "DESeq",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon"))
      )
    )
  ),

#BODY --------------------------------------------------------------------------------------------------------------------------------------------------
  dashboardBody(
    includeCSS(path = "www/custom.css"),
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
  ### 读入数据 ---------------------------------------------------------------------------------------------------------
    tabItems(
      source("modules/Introduction.R")$value,
      source("modules/1-1.update_ui.R")$value,
  ### 第一部分 ---------------------------------------------------------------------------------------------------------------------------------------------
      source("modules/1-2.summarise_barplot_ui.R")$value,
      source("modules/1-3.snp_heatmap_ui.R")$value,
      source("modules/1-4.snp_All_Type_ui.R")$value,
      source("modules/1-5.Venn_ui.R")$value,
      source("modules/1-6.circle_ui.R")$value,
  ### 第二部分 ---------------------------------------------------------------------------------------------------------------------------------
      source("modules/2-1.Distribution_ui.R")$value,
      source("modules/2-2.VariantsGene_Summarise_ui.R")$value,
      source("modules/2-3.VariantsGene_Heatmap_ui.R")$value,
      source("modules/2-4.VariantsGene_GO.KEGG_ui.R")$value
    )
  )
)

