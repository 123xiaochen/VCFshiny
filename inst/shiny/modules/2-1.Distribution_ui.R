tabItem(
  tabName = "distribution",
  fluidPage(
    box(
      title = "Set Distribution Plot", width = 3, status = "info", collapsible = T,
      radioGroupButtons("distribution_type_id", "Select your type to analyse:", choices = c("snp","indel"),
                        justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%"),
      sliderInput("distribution_text_size", "Text Size", min = 0, max = 30, value = 12, step = 1, width = "100%"),
      sliderInput("distribution_bar_width", "Bar Width", min = 0, max = 5, value = 0.8, step = 0.1, width = "100%"),
      h4("Set Basics Elements:"),
      actionButton("distribution_module_id", "Set Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:center",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      h4("Display Plot:"),
      actionButton("plot_distribution", "Variants Distribution Plot", icon = icon("bar-chart-o"), width = "100%")
    ),
    bsModal(
      "distribution_module", "Set Basics Elements", "distribution_module_id", size = "small",
      fluidPage(
       style = "padding:10px",
       column(
         width = 12,
         sliderInput("distribution_error_bar_size","Error Bar Size", min = 0, max = 2, value = 0.5, step = 0.05, width = "100%"),
         sliderInput("distribution_error_bar_width", "Error Bar Width", min = 0, max = 1, value = 0.3, step = 0.05, width = "100%"),
         # sliderInput("distribution_jitter_size", "Distribution Jitter Size", min = 0, max = 5, value = 1, step = 0.5, width = "100%"),
         # sliderInput("distribution_jitter_width", "Distribution Jitter Width", min = 0, max = 1, value = 0.2,step = 0.01, width = "100%"),
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
          sliderInput("Distribution_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot"),
          downloadButton("Distribution_Download","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Distribution_Plot"))
      )
    ),
    box(
      title = "Display Distribution Data", width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
          radioGroupButtons("Distribution_data_Set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                            ,justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
        )
      ),
      div(
        DT::dataTableOutput("distribution_data")
      )
    )
  )
)
