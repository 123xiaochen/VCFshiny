tabItem(
  tabName = "snp_analysis",
  fluidRow(
    box(
      title = "Set SNP Analysis Plot",width = 3, status = "info", collapsible = T,
      virtualSelectInput("SNP_Analyse_mode", "Analyse Mode:", choices = c("heatmap", "barplot"), multiple = F,
                         zIndex = 4, search = F, width = "100%"),
      uiOutput("snp_analyse_group"),
      conditionalPanel(
        "input.SNP_Analyse_mode == 'heatmap'",
        textInput("heatmap_color", "Snp Heatmap High Freq Color:", value = "white,#3d85c6", width = "100%"),
        sliderInput("heatmap_text.size","Text size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
        sliderInput("heatmap_ncol", "Snp heatmap cols:", min = 1, max = 6, value = 2, width = "100%")
      ),
      conditionalPanel(
        "input.SNP_Analyse_mode == 'barplot'",
        virtualSelectInput("SNP_barplot_mode", "Barplot Position:", choices = c("dodge", "fill"), multiple = F,
                           zIndex = 4, search = F, width = "100%"),
        sliderInput("SNV_title_Text_size", "Title text size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
        sliderInput("SNV_label_Text_size", "Label text size:", min = 0, max = 30, value = 15, step = 1, width = "100%"),
      ),
      actionButton("snp_modal_but", "Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:left; margin-top:15px; margin-bottom:8px",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      actionButton("SNP_Analysis_action", "SNP Analysis Plot", icon = icon("bar-chart-o"), width = "100%",
                    style = "background-color: #76a5af; border-radius: 28px;"),
    ),
    bsModal(
      "snp_modal", "Additional Parameters", "snp_modal_but", size = "large",
      fluidRow(
        style = "padding: 10px",
        column(
          12,
          conditionalPanel(
            "input.SNP_Analyse_mode == 'heatmap'",
            textAreaInput("snp_heatmap_ggText", "ggplot2 codes:", rows = 5, width = "100%")

          ),
          conditionalPanel(
            "input.SNP_Analyse_mode == 'barplot'",
            sliderInput("SNV_barplot_bar_width","Bar width:", min = 0, max = 1, value = 0.8, step = 0.1, width = "100%"),
            sliderInput("SNV_barplot_err_bar_width","Error bar width:", min = 0, max = 1, value = 0.6, step = 0.1, width = "100%"),
            sliderInput("SNV_barplot_err_bar_size","Error bar size:", min = 0, max = 1, value = 0.5, step = 0.1, width = "100%"),
            textAreaInput("snp_barplot_ggText", "ggplot2 codes:", rows = 5, width = "100%")
          )
        )
      )
    ),
    box(
      title = "Display SNP Analysis Plot", width = 9,  status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements",  width = "100%", right = T,icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
          sliderInput("SNP_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("SNP_download_height", "Download Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot"),
          downloadButton("SNP_plot","Download Plot")
        )
      ),
      div(
          shinycssloaders::withSpinner(plotOutput("Display_SNP_Plot", height = "481px"))
      )
    ),
    box(
      title = "Display SNP Analysis Data", width = 12,  status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        downloadButton('snp_tab','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
      ),
    div(
      DT::dataTableOutput("snp_analysis_data")
    )
    )
  )
)
