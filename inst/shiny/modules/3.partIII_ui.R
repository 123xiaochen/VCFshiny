tabItem(
  tabName = "snp_heatmap",
  fluidPage(
    box(
      title = "Set SNP Heatmap Plot",width = 3, status = "info", collapsible = T,
      #h4("Set Basics Elements:"),
      # dropdown(
      #   label = "Set Basics Elements",  width = "100%", right = T,icon = icon("cog", lib = "glyphicon"),
      sliderInput("heatmap_text.size","Text Size:", min = 0, max = 30, value = 18),
      sliderInput("heatmap_ncol", "SNP Heatmap Cols:", min = 1, max = 6, value = 3),
      sliderInput("heatmap_digits", "SNP Heatmap Significant Digits:", min = 0, max = 5,value = 2),
      # ),
      # radioGroupButtons("heatmap_color", "Set Heatmap Color ?", c("No" = "FALSE", "Yes" = "TRUE"),
      #                   justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
      h4("Display Plot:"),
      actionButton("Heatplot","SNP Data Heatmap Plot", icon = icon("bar-chart-o"), width = "100%")
    ),
    box(
      title = "Display SNP Heatmap Plot", width = 9,  status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements",  width = "100%", right = T,icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          radioGroupButtons("heatmap_down_filetype","Download Data Type",c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"),
                            justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
          sliderInput("heatmap_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
          sliderInput("heatmap_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
          h4("Download Plot"),
          downloadButton("heatmap_plot","Heatmap Plot Download")
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
        DT::dataTableOutput("snp_heatmap")
      )
    )
  )
)
