tabItem(
  tabName = "Variants_heatmap",
  fluidPage(
    box(
      title = "Set Variants Genes Heatmap Plot", width = 3, status = "info", collapsible = T,
      radioGroupButtons("Variants_heatmap_types", "Select Heatmap Data Types:", c("snp", "indel"), justified = T),
      uiOutput("Variants_position_id"),
      sliderInput("Variants_heatmap_numbers", "Select Gene Numbers:", min = 0, max = 200, value = 50, width = "100%"),
      h4("Set Basics Elements:"),
      actionButton("variants_heatmap_module_id", "Set Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:center",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      h4("Display Plot:"),
      actionButton("Variants_heatmap", "Variants Genes Heatmap", icon = icon("bar-chart-o"), width = "100%")
    ),
    bsModal(
      "variants_heatmap_module", "Set Additional Parameters", "variants_heatmap_module_id", size = "small",
      fluidPage(
        style = "padding:10px",
        radioGroupButtons("Variants_heatmap_show_rownames", "Show Rownames", c("TRUE", "FALSE"),
                          justified = T, checkIcon = list(yes = icon("ok", lib = "glyphicon")), width = "100%"),
        radioGroupButtons("Variants_heatmap_show_colnames", "Show Colnames", selected = "FALSE" ,c("TRUE","FALSE"),
                          justified = T, checkIcon = list(yes = icon("ok", lib = "glyphicon")), width = "100%"),
        sliderInput("Variants_heatmap_treeheight_row", "Treeheight Row", min = 0, max = 50, value = 20, step = 1, width = "100%"),
        sliderInput("Variants_heatmap_treeheight_col", "Treeheight Col", min = 0, max = 50, value = 20, step = 1, width = "100%")
      )
    ),
    box(
      title = "Display Heatmap Plot", width = 9, status = "primary", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("Variants_heatmap_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("Variants_heatmap_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot"),
          downloadButton("Variants_heatmap_Download","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("VariantsGenes_Heatmap_Plot"))
      )
    ),
    box(
      title = "Display Heatmap Data", width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
          radioGroupButtons("Variants_Genes_Heatmap_data", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                            , justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
        )
      ),
      div(
        DT::dataTableOutput("Variants_Heatmap_data")
      )
    )
  )
)
