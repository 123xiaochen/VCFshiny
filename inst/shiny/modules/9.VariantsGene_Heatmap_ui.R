tabItem(
  tabName = "Variants_heatmap",
  fluidRow(
    box(
      title = "Set Variants Genes Heatmap Plot", width = 3, status = "info", collapsible = T,
      prettyRadioButtons(inputId = "Variants_heatmap_types", label = "Select Heatmap Data Types:",
                         choices = c("all", "snp", "indel"), inline = T,
                         animation = "tada", shape = "square", bigger = T),
      uiOutput("Variants_heatmap_sampleID"),
      uiOutput("Variants_heatmap_feature_column"),
      uiOutput("Variants_heatmap_gene_column"),
      uiOutput("Variants_position_id"),
      sliderInput("Variants_heatmap_colorValue", "Max Color value:", min = 0, max = 50, value = 10, width = "100%"),
      actionButton("variants_heatmap_module_id", "Set Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:left;margin-top:15px; margin-bottom:8px",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      actionButton("Variants_heatmap", "Variants Genes Heatmap",
                   style = "background-color: #76a5af; border-radius: 28px;",  width = "100%")
    ),
    bsModal(
      "variants_heatmap_module", "Set Additional Parameters", "variants_heatmap_module_id", size = "small",
      fluidPage(
        style = "padding:10px",
        radioGroupButtons("Variants_heatmap_show_rownames", "Show Rownames", choices = c("TRUE", "FALSE"),
                          selected = "FALSE", justified = T, checkIcon = list(yes = icon("ok", lib = "glyphicon")), width = "100%"),
        radioGroupButtons("Variants_heatmap_show_colnames", "Show Colnames", choices = c("TRUE", "FALSE"),
                          selected = "TRUE", justified = T, checkIcon = list(yes = icon("ok", lib = "glyphicon")), width = "100%"),
      )
    ),
    box(
      title = "Display Heatmap Plot", width = 9, status = "primary", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("Variants_heatmap_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("Variants_heatmap_download_height", "Download Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot"),
          downloadButton("Variants_heatmap_Download","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Heatmap_plot_2", height = "528px"))
      )
    ),
    box(
      title = "Display Heatmap Data", width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        downloadButton('heatmap_gene_tab','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
      ),
      div(
        DT::dataTableOutput("Variants_Heatmap_data")
      )
    )
  )
)
