tabItem(
  tabName = "circle",
  fluidRow(
    box(
      title = "Set Circle Plot", width = 3, status = "info", collapsible = T,
      selectInput("circle_type","Circle Type:",
                  choices = c("points", "lines", "rectangles"
                  )),
      radioGroupButtons("circle_type_ID", "Analyse Type:", choices = c("snp","indel"), justified = T),
      sliderInput("legend.x.position", "Legend plot X position:", min = -1, max = 1,value = 0.75, step = 0.05),
      sliderInput("legend.y.position", "Legend plot Y position:", min = -1, max = 1,value = -0.5, step = 0.05),
      h4("Set Basics Elements:"),
      actionButton("circle_module_but", "Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:center",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      h4("Display Plot:"),
      actionButton("circle_star","WGS Data Circle Plot", icon = icon("bar-chart-o"), width = "100%"),
    ),
    bsModal(
      "circleplot_module_but", "Additional Parameters", "circle_module_but", size = "small",
      fluidPage(
        style = "padding:10px",
          checkboxGroupButtons("chrom_plotType","Chrom plotType:",c("axis",  "labels", "ideogram"),selected = c("ideogram","labels"),
                               justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
          sliderInput("circle_text_size","Text Size:",min = 0, max = 5, value = 0.8, step = 0.1, width = "100%"),
          sliderInput("track.height","Track.height:",min = 0, max = 0.3,value = 0.10, step = 0.01, width = "100%"),
          sliderInput("start.degree", "Start.degree:", min = 0, max = 180,value = 90, step = 1, width = "100%"),
          sliderInput("track.margin1","Track.margin 1:",  min = 0, max = 0.1, value = 0.01, step = 0.01, width = "100%"),
          sliderInput("track.margin2","Track.margin 2:", min = 0, max = 0.1, value = 0.005, step = 0.01, width = "100%"),
          sliderInput("track.gap.degree","Track.gap.width:", min = 0, max = 5,value = 0.5, step = 0.1, width = "100%")
      )
    ),
    box(
      title = "Display Circle Plot", width = 6, status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Element",  width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("CirclePlot_download_width", "Download Plot width:", min = 0, max = 15, value = 10, step = 0.5),
          sliderInput("CirclePlot_download_height", "DOwnload Plot Height:", min = 0, max = 15, value = 10, step = 0.5),
          h4("Download Plot"),
          downloadButton("CirclePlot_download", "Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("cirecle_PlotOutput"))
      )
    ),
    box(
      title = "Set Circle Elements Plot", width = 3, status = "info", collapsible = T,
      uiOutput("circle_type_Basics_set")
    )
  )
)
