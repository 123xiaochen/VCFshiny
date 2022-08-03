tabItem(
  tabName = "snp_heatmap",
  fluidPage(
    box(
      title = "Set SNP Heatmap Plot",width = 3, status = "info", collapsible = T,
      sliderInput("heatmap_text.size","Text Size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
      sliderInput("heatmap_ncol", "Snp Heatmap Cols:", min = 1, max = 6, value = 2, width = "100%"),
      sliderInput("heatmap_digits", "Snp Heatmap Significant Digits:", min = 0, max = 5, value = 2, width = "100%"),
      textInput("heatmap_color", "Snp Heatmap High Freq Color:", value = "#2A71AF", width = "100%"),
      h4("Display Plot:"),
      actionButton("Heatplot","SNP Data Heatmap Plot", icon = icon("bar-chart-o"), width = "100%")
    ),
    box(
      title = "Display SNP Heatmap Plot", width = 9,  status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements",  width = "100%", right = T,icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("heatmap_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("heatmap_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot"),
          downloadButton("heatmap_plot","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Heat_Plot"))
      )
    ),
    box(
      title = "Display SNP Heatmap Data", width = 12,  status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
          radioGroupButtons("Heatmap_data_Set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                            ,justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
        )
      ),
      div(
        DT::dataTableOutput("snp_heatmap_data")
      )
    )
  )
)
