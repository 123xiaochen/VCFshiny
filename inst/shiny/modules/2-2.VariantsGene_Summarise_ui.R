tabItem(
  tabName = "Variants_summarise",
  fluidPage(
    box(
      title = "Set Variants Genes Plot", width = 3, status = "info", collapsible = T,
      uiOutput("Variants_sample_id"),
      # radioGroupButtons("Variants_genes_select_position", "Select Position Data:", c(TRUE, FALSE), selected = FALSE,
      #                   justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
      # uiOutput("Variants_barplot_position_id"),

      radioGroupButtons("Variants_genes_label", "Display Barplot Numbers Label:", c(TRUE, FALSE),
                        justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%"),
      sliderInput("Variants_genes_numbers", "Choice Display Gene Numbers:", min = 0, max = 50, value = 20, step = 1, width = "100%"),

      h4("Set Basics Elements:"),
      actionButton("variants_modules_id", "Set Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:center",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      h4("Display Plot:"),
      actionButton("plot_Variants_genes", "Plot Variants Genes", icon = icon("bar-chart-o"), width = "100%")
    ),
    bsModal(
      "variants_modules", "Set Additional Parameters", "variants_modules_id", size = "small",
      fluidPage(
        style = "padding:10px",
        column(
          width = 12,
          sliderInput("Variants_genes_bar_width", "Bar Width:", min = 0, max = 5, value = 0.8, step = 0.1, width = "100%"),
          sliderInput("Variants_genes_text_size", "Text Size:", min = 0, max = 30, value = 12, step = 1, width = "100%"),
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
          sliderInput("Variants_Genes_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("Variants_Genes_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot"),
          downloadButton("Variants_genes_Download","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Variants_Plots"))
      )
    ),
    box(
      title = "Display Variants Genes Data", width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
          radioGroupButtons("Variants_Genes_data_Set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                            ,justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
        )
      ),
      div(
        DT::dataTableOutput("Variants_df")
      )
    )
  )
)
