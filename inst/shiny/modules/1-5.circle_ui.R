tabItem(
  tabName = "circle",
  fluidRow(
    box(
      title = "Set Circle Plot", width = 3, status = "info", collapsible = T,
      prettyRadioButtons(inputId = "circle_type_ID", label = "Analyse Type:", choices = c("snp","indel"), inline = T,
                         animation = "tada", shape = "square", bigger = T),
      uiOutput("circle_group"),
      virtualSelectInput("circle_type","Circle Type:", choices = c("points", "lines", "heatmap"),
                         multiple = F, zIndex = 4,search = F, width = "100%"),
      sliderInput("circle_legend_text_size","Legend Text Size:",min = 0, max = 5, value = 0.8, step = 0.1, width = "100%"),
      sliderInput("circle_center_text_size","Center Text Size:",min = 0, max = 5, value = 1.8, step = 0.1, width = "100%"),
      sliderInput("track.height","Track.height:",min = 0, max = 0.3,value = 0.12, step = 0.01, width = "100%"),
      actionButton("circle_module_but", "Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:left;margin-top:15px; margin-bottom:8px",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      actionButton("circle_star","WGS Data Circle Plot",
                   style = "background-color: #76a5af; border-radius: 28px;", icon = icon("bar-chart-o"), width = "100%"),
    ),
    bsModal(
      "circleplot_module_but", "Additional Parameters", "circle_module_but", size = "small",
      fluidPage(
        style = "padding:10px",
          checkboxGroupButtons("chrom_plotType","Chrom plotType:", c("axis",  "labels", "ideogram"), selected = c("ideogram","labels"),
                               justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
          sliderInput("start.degree", "Start.degree:", min = 0, max = 180,value = 90, step = 1, width = "100%"),
          sliderInput("track.margin1","Track.margin 1:",  min = 0, max = 0.1, value = 0.02, step = 0.01, width = "100%"),
          sliderInput("track.margin2","Track.margin 2:", min = 0, max = 0.1, value = 0.02, step = 0.01, width = "100%"),
          sliderInput("track.gap.degree","Track.gap.width:", min = 0, max = 5,value = 0.6, step = 0.1, width = "100%")
      )
    ),
    box(
      title = "Display Circle Plot", width = 6, status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Element",  width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("CirclePlot_download_width", "Download Plot width:", min = 0, max = 15, value = 10, step = 0.5),
          sliderInput("CirclePlot_download_height", "Download Plot Height:", min = 0, max = 15, value = 10, step = 0.5),
          h4("Download Plot"),
          downloadButton("CirclePlot_download", "Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("cirecle_PlotOutput", height = "560px"))
      )
    ),
    box(
      title = "Set Circle Elements Plot", width = 3, status = "info", collapsible = T,
      selectInput(
        inputId = "Species",
        label = "Choose your Species:",
        c("Human" = "hg38",
          "Mouse" = "mm10",
          "Rat" = "rn6",
          "Sus scrofa" ="susScr11",
          "Dog" = "canFam4",
          "others" = "others"),
        selected = "Human", multiple = F, width = "100%"
      ),
      conditionalPanel(
        "input.Species == 'others'",
        fileInput("chrom_files", "Input your circle chrom data:", multiple = F, width = "100%")
      ),
      sliderInput("legend.x.position", "Legend plot X position:", min = -1, max = 1,value = 0.55, step = 0.05),
      sliderInput("legend.y.position", "Legend plot Y position:", min = -1, max = 1,value = -0.65, step = 0.05),
      conditionalPanel(
        "input.circle_type == 'points'",
        sliderInput("points.size", "Points Size:", min = 0, max = 1, value = 0.3, step = 0.05),
        selectInput("points.pch", "Points Pch:", choices = c(0, 1:25), selected = 16),
        sliderInput("points.alpha", "Points Alpha:", min = 0, max = 1, value = 0.5, step = 0.05)
      ),
      conditionalPanel(
        "input.circle_type == 'lines'",
        radioGroupButtons("lines_area", "Lines_Area", choices = c("TRUE", "FALSE")
                          ,justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
        selectInput("lines_type", "Lines_Type:", choices = c("l", "o", "h", "s")),
        selectInput("lines_lty", "Lines_Lty:", choices = c(1:6), selected = 1),
        sliderInput("lines_lwd", "Lines_Lwd:", min = 0, max = 5, value = 2, step = 0.1)
      )
    )
  )
)
