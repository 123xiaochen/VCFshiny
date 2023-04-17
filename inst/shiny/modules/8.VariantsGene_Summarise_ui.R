tabItem(
  tabName = "Variants_summarise",
  fluidRow(
    box(
      title = "Set Variants Genes Plot", width = 3, status = "info", collapsible = T,
      prettyRadioButtons(inputId = "Variants_type_id", label = "Variant type:", choices = c("all", "snp", "indel"), inline = T,
                         animation = "tada", shape = "square", bigger = T),
      uiOutput("Variants_sample_id"),
      uiOutput("Variants_feature_column"),
      uiOutput("Variants_gene_column"),
      uiOutput("Variants_bar_position_id"),
      actionButton("variants_modules_id", "Set Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:left;margin-top:15px; margin-bottom:8px",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      actionButton("plot_Variants_genes", "Plot Variants Genes",
                   style = "background-color: #76a5af; border-radius: 28px;",  width = "100%")
    ),
    bsModal(
      "variants_modules", "Set Additional Parameters", "variants_modules_id", size = "small",
      fluidPage(
        style = "padding:10px",
        column(
          width = 12,
          radioGroupButtons("Variants_genes_label", "Display Barplot Numbers Label:", c(TRUE, FALSE),
                            justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%"),
          sliderInput("Variants_genes_numbers", "Display Top N Gene:", min = 0, max = 50, value = 20, step = 1, width = "100%"),
          sliderInput("Variants_genes_title_text_size", "Title text size:", min = 0, max = 30, value = 13, step = 1, width = "100%"),
          sliderInput("Variants_genes_label_text_size", "Label text size:", min = 0, max = 30, value = 12, step = 1, width = "100%"),
          sliderInput("Variants_genes_bar_width", "Bar Width:", min = 0, max = 5, value = 0.8, step = 0.1, width = "100%"),
          textAreaInput("variants_genes_ggText", "Add ggplot Codes:", rows = 5, width = "100%")
        )
      )
    ),
    box(
      title = "Display Variants Genes Plot", width = 9, status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("Variants_Genes_download_width", "Download Plot width", min = 0, max = 15, value = 7, step = 0.5, width = "100%"),
          sliderInput("Variants_Genes_download_height", "Download Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot"),
          downloadButton("Variants_genes_Download","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Variants_Plots", height = "419px"))
      )
    ),
    box(
      title = "Display Variants Genes Data", width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        downloadButton('summary_gene_tab','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
      ),
      div(
        DT::dataTableOutput("Variants_df")
      )
    )
  )
)
