tabItem(
  tabName = "venn_analyse",
  fluidPage(
    box(
      title = "Set Venn Plot", width = 3, status = "info", collapsible = T,
      radioGroupButtons("venn_type_id", "Select your type to analyse:", choices = c("snp","indel"),
                        justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%"),
      uiOutput("venn_group_id"),
      sliderInput("venn_ilcs", "Ilcs Size:", min = 0, max = 2, value = 0.8, step = 0.05, width = "100%",),
      sliderInput("venn_sncs", "Sncs Size:", min = 0, max = 2, value = 0.8, step = 0.05, width = "100%",),
      h4("Set Basics Elements:"),
      actionButton("venn_module_but", "Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:center",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      h4("Display Plot:"),
      actionButton("venn_star", "Snp / Indel Venn Plot", icon = icon("bar-chart-o"), width = "100%")
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
          sliderInput("VennPlot_download_height", "DOwnload Plot Height:", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot:"),
          downloadButton("Venn_download_plot","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("Display_venn_plot"))
      )
    ),
    box(
      title = "Display Venn Data",
      width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
          radioGroupButtons("Venn_data_Set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                            ,justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
        )
      ),
      div(
        DT::dataTableOutput("venn_Table")
      )
    )
  )
)
