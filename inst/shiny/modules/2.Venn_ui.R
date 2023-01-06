tabItem(
  tabName = "venn_analyse",
  fluidRow(
    box(
      title = "Set Venn Plot", width = 3, status = "info", collapsible = T,
      prettyRadioButtons(inputId = "venn_type_id",  label = "Variant type:", choices = c("snp", "indel"),
                        inline = T, animation = "tada", shape = "square", bigger = T),
      uiOutput("venn_group_id"),
      sliderInput("venn_ilcs", "Ilcs Size:", min = 0, max = 2, value = 1, step = 0.05, width = "100%",),
      sliderInput("venn_sncs", "Sncs Size:", min = 0, max = 2, value = 1, step = 0.05, width = "100%",),
      actionButton("venn_module_but", "Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:left;margin-top:15px; margin-bottom:8px",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      actionButton("venn_star", "Snp / Indel Venn Plot",
                   style = "background-color: #76a5af; border-radius: 28px;", icon = icon("bar-chart-o"), width = "100%")
    ),
    bsModal(
      "vennplot_module_but", "Additional Parameters", "venn_module_but", size = "small",
      fluidPage(
        style = "padding: 10px",
        column(
          width = 12,
          radioGroupButtons("venn_box", "Use Box:", c("FALSE", "TRUE"),justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%",),
          radioGroupButtons("venn_ellipse", "Use Ellipse:", choices = c("FALSE", "TRUE"),justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%"),
          radioGroupButtons("venn_borders", "Use borders", choices = c("FALSE", "TRUE"),justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%")
        )
      )
    ),
    box(
      title = "Display Venn Plot", width = 9,  status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements",  width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("VennPlot_download_width", "Download Plot width:", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("VennPlot_download_height", "Download Plot Height:", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot:"),
          downloadButton("Venn_download_plot","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Display_venn_plot", height = "413px"))
      )
    ),
    box(
      title = "Display Venn Data",
      width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        downloadButton('venn_tab_download','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
      ),
      div(
        DT::dataTableOutput("venn_Table")
      )
    )
  )
)
