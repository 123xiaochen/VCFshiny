tabItem(
  tabName = "DESeq",
  fluidPage(
    box(
      title = "Set Enrich Data Paraments", width = 3, status = "info", collapsible = T,
      radioGroupButtons("DESeq_Type_ID", "Select Your Data Type:", c("snp", "indel"),
                        selected = "snp", justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
      selectInput("enrich_Species_Orgdb", "Select Your Enrich Species",
                  c("Human" = "org.Hs.eg.db",
                    "Rat" = "org.Rn.eg.db",
                    "Mouse" = "org.Mm.eg.db",
                    "Pig" = "org.Ss.eg.db",
                    "Zebrafish" = "org.Dr.eg.db",
                    "Cow" = "org.Bt.eg.db"),selected = "Human", multiple = F),
      uiOutput("DESeq_group_1"),
      uiOutput("DESeq_group_2"),
      uiOutput("DESeq_position_id"),
      h4("Display Plot:"),
      actionButton("plot_enrichPlot","Display Plot", icon = icon("bar-chart-o"), width = "100%")
    ),
    box(
      title = "Display Enrich Plot", width = 6, status = "primary", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("Enrich_plot_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
          sliderInput("Enrich_plot_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
          h4("Download Plot:"),
          downloadButton("Enrich_plot_Download","Download Plot")
        )
      ),
      div(
        uiOutput("enrich_PlotUI")
      )
    ),
    box(
      title = "Set Enrich Plot Paraments", width = 3, status = "info", collapsible = T,
      radioGroupButtons("enrich_Fun", "Select Your Enrich Function:", c("enrichGO","enrichKEGG"),
                        selected = "enrichGO", justified = T, checkIcon = list(yes = icon("ok", lib = "glyphicon")), width = "100%"),
      radioGroupButtons("enrich_geneID", "Select Your GeneID Ketype:", c("SYMBOL","ENSEMBL" , "ENTREZID" ),
                        selected = "SYMBOL", justified = T, checkIcon = list(yes = icon("ok", lib = "glyphicon")), width = "100%"),
      radioGroupButtons("enrich_ont", "Select Your Ont:", c("BP", "MF","CC"),
                        selected  = "BP", justified = T, checkIcon = list(yes = icon("ok", lib = "glyphicon")), width = "100%"),
      numericInput("showCategory_numbers","Choice Enrich ShowNumbers:", value = 15, width = "100%"),
      numericInput("enrichplot_fontSize", "Set Enrich Plot Font.size:", value = 10, width = "100%"),
      sliderInput("Enrich_plot_width", "Set Enrich Plot Width(%):", min = 0, max =100, value = 100, step = 2, width = "100%"),
      sliderInput("Enrich_plot_height", "Set Enrich Plot Height(px):", min = 0, max = 1000, value = 400, step = 20, width = "100%"),
      textAreaInput("Enrich_AddCodes", "Add ggplot2 Codes:", rows = 2, width = "100%")
    ),
    box(
      title = "Display Enrich Data",width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          width = "300px",icon = icon("cog", lib = "glyphicon"), right = T,
          radioGroupButtons("Enrich_Data_set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                            ,justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
        )
      ),
      div(
        DT::dataTableOutput("DESeq_data")
      )
    )
  )
)
