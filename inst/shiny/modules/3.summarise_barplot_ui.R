tabItem(
  tabName = "ALL_variants",
  fluidRow(
      box(
        title = "Set Summary Plot",width = 3,  status = "info", collapsible = T,
        prettyRadioButtons(inputId = "Summariseplot_label",  label = "Display numbers label:", choices =  c("TRUE","FALSE"),
                           selected = "FALSE", inline = T, animation = "tada", shape = "square", bigger = T),
        sliderInput("title_text_size","Title text size:", min = 0, max = 30, value = 18, step = 1, width = "100%"),
        sliderInput("label_text_size","Label text size:", min = 0, max = 30, value = 15, step = 1, width = "100%"),
        sliderInput("bar_width","Bar width:", min = 0, max = 1, value = 0.8, step = 0.1, width = "100%"),
        actionButton("summarise_modal_but", "Additional Parameters ...", width = "100%",
                     style = "background-color: rgb(255,255,255);text-align:left;margin-top:15px; margin-bottom:8px",
                     icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
        actionButton("summarise_plot","SNP+Indel ALL Variants Plot",
                     style = "background-color: #76a5af; border-radius: 28px;",  width = "100%")
      ),
      #设置bsModal控件
      bsModal(
        "summariseplot_modal_but", "Additional Parameters", "summarise_modal_but", size = "large",
        fluidRow(
          style = "padding: 10px",
          column(
            12,
            sliderInput("error_bar_width","Error bar width:", min = 0, max = 1, value = 0.3, step = 0.1, width = "100%"),
            sliderInput("error_bar_size","Error bar size:", min = 0, max = 2, value = 0.5, step = 0.1, width = "100%"),
            sliderInput("jitter_size","Jitter size:", min = 0, max = 5,value = 1,step = 0.1, width = "100%"),
            sliderInput("jitter_width","Jitter width:", min = 0, max = 0.5,value = 0.3,step = 0.01, width = "100%"),
            textAreaInput("summ_ggText", "Add ggplot2 Codes:", rows = 5, width = "100%")
          )
        )
      ),
      box(
        title = "Display Snp+Indel ALL Variants Plot",width = 9, status = "primary", collapsible = T,
        div(
          style = "position: absolute; right: 0.5em; top: 0.5em;",
          dropdown(
            label = "Set Download Plot Elements", width = "100%", right = T,icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
            sliderInput("summarise_download_width", "Download Plot width:", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
            sliderInput("summarise_download_height", "Download Plot Height:", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
            h4("Download Plot:"),
            downloadButton("summarise_plot","Download Plot")
          )
        ),
        div(
          shinycssloaders::withSpinner(plotOutput("SummarisePlot", height = "447px"))
        )
      ),

      box(
        title = "Display Snp+Indel ALL Variants Data", width = 12, status = "success", collapsible = T,
        div(
          style = "position: absolute; right:0.5em; top: 0.5em;",
          downloadButton('summary_tab','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
        ),
        div(
          DT::dataTableOutput("information")
        )
      )
  )
)
