tabItem(
  tabName = "Indel_analysis",
  fluidRow(
    box(
      title = "Set Indel Analysis Plot", width = 3,  status = "info", collapsible = T,
      virtualSelectInput("Indel_Analyse_mode", "Analyse Mode:", choices = c("density", "heatmap"), multiple = F,
                         zIndex = 4, search = F, width = "100%"),
      uiOutput("Indel_analysis_group"),
      conditionalPanel(
        "input.Indel_Analyse_mode == 'density'",
        virtualSelectInput("Indel_density_position", "Density Position:", choices = c("stack","identity"), multiple = F,
                           zIndex = 4, search = F, width = "100%"),
        sliderInput("Indel_density_text_size","Text Size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
        sliderInput("Indel_density_alpha", "Alpha Size:", min = 0 , max = 1, value = 0.5, step = 0.1, width = "100%")
      ),
      conditionalPanel(
        "input.Indel_Analyse_mode == 'heatmap'",
        textInput("Indel_heatmap_color", "Indel Heatmap High Freq Color:", value = "white,red", width = "100%"),
        sliderInput("Indel_heatmap_Text_size", "Text size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
        sliderInput("Indel_heatmap_digits", "Snp Heatmap Significant Digits:", min = 0, max = 5, value = 2, width = "100%")
      ),
      actionButton("Indel_Analysis_action","SNP ALL Variants Type Plot",
                   style = "background-color: #76a5af; border-radius: 28px;margin-top: 14px", icon = icon("bar-chart-o"), width = "100%")
    ),
    box(
      title = "Display Indel Analysis Plot", width = 9,  status = "primary", collapsible = T,
    div(
      style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements",  width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save", lib = "glyphicon"),
          sliderInput("Indel_download_width", "Download Plot width:", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("Indel_download_height", "Download Plot Height:", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot:"),
          downloadButton("Indel_plot","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Display_Indel_plot", height = "442px"))
      )
     ),
    box(
      title = "Display Indel Analysis Data ", width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        downloadButton('indel_tab','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
      ),
      div(
        DT::dataTableOutput("Indel_Density_data")
      )
    )
  )
)
