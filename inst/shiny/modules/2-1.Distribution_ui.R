tabItem(
  tabName = "distribution",
  fluidRow(
    box(
      title = "Set Distribution Plot", width = 3, status = "info", collapsible = T,
      prettyRadioButtons(inputId = "distribution_type_id", label = "Variant type:", choices = c("snp", "indel"), inline = T,
                         animation = "tada", shape = "square", bigger = T),
      uiOutput("distribution_type_group"),
      uiOutput("distribution_feature_column"),
      virtualSelectInput("distribution_type_position", "Bar Position:", choices = c("dodge", "fill"),
                         multiple = F, zIndex = 4,search = F, width = "100%"),
      sliderInput("distribution_title_text_size", "Title Text Size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
      sliderInput("distribution_label_text_size", "Label Text Size:", min = 0, max = 30, value = 15, step = 1, width = "100%"),
      actionButton("distribution_module_id", "Set Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:left;margin-top:15px; margin-bottom:8px",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      actionButton("plot_distribution", "Variants Distribution Plot",
                   style = "background-color: #76a5af; border-radius: 28px;", icon = icon("bar-chart-o"), width = "100%")
    ),
    bsModal(
      "distribution_module", "Set Basics Elements", "distribution_module_id", size = "small",
      fluidPage(
       style = "padding:10px",
       column(
         width = 12,
         sliderInput("distribution_bar_width", "Bar Width:", min = 0, max = 5, value = 0.8, step = 0.1, width = "100%"),
         sliderInput("distribution_error_bar_width", "Error Bar Width:", min = 0, max = 1, value = 0.3, step = 0.05, width = "100%"),
         sliderInput("distribution_error_bar_size","Error Bar Size:", min = 0, max = 2, value = 0.5, step = 0.05, width = "100%"),
         textAreaInput("distribution_ggText", "Add ggplot2 Codes:", rows = 5, width = "100%")
       )
      )
    ),
    box(
      title = "Display Distribution Plot", width = 9, status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("Distribution_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("Distribution_download_height", "Download Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot"),
          downloadButton("Distribution_Download","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Distribution_Plot", height = "540px"))
      )
    ),
    box(
      title = "Display Distribution Data", width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        downloadButton('distribution_tab_download','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
      ),
      div(
        DT::dataTableOutput("distribution_data")
      )
    )
  )
)
