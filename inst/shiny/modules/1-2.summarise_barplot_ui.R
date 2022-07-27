tabItem(
  tabName = "ALL_variants",
  fluidPage(
      box(
        title = "Set Summary Plot",width = 3,  status = "info", collapsible = T,
        #uiOutput("Summariseplot_stat.test.group"),
        sliderInput("text_size","Text Size:", min = 0, max = 30, value = 15, step = 1, width = "100%"),
        sliderInput("bar_width","Bar width:", min = 0, max = 2, value = 0.8, step = 0.1, width = "100%"),
        radioGroupButtons("Summariseplot_label", "Display Label:", c("FALSE","TRUE"),
                          justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%"),
        h4("Set Basics Elements:"),
        actionButton("summarise_modal_but", "Additional Parameters ...", width = "100%",
                     style = "background-color: rgb(255,255,255);text-align:center",
                     icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
        h4("Display Plot:"),
        actionButton("Data_summarise_plot","SNP+Indel ALL Variants Plot",icon = icon("bar-chart-o"), width = "100%")
      ),
      #设置bsModal控件
      bsModal(
        "summariseplot_modal_but", "Additional Parameters", "summarise_modal_but", size = "small",
        fluidRow(
          style = "padding: 10px",
          column(
            12,
            sliderInput("error_bar_width","Error bar width:", min = 0, max = 1, value = 0.3, step = 0.1, width = "100%"),
            sliderInput("error_bar_size","Error bar size:", min = 0, max = 2, value = 0.5, step = 0.1, width = "100%"),
            sliderInput("jitter_size","Jitter size:", min = 0, max = 5,value = 1,step = 0.1, width = "100%"),
            sliderInput("jitter_width","Jitter width:", min = 0, max = 0.5,value = 0.3,step = 0.01, width = "100%"),
            sliderInput("brack.size","Brack.size:", min = 0, max = 2, value = 0.8, step = 0.1, width = "100%"),
            sliderInput("tip.length","Tip.length:", min = 0, max = 1,value = 0.1,step = 0.05, width = "100%"),
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
            sliderInput("summarise_download_height", "DOwnload Plot Height:", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
            h4("Download Plot:"),
            downloadButton("summarise_plot","Download Plot")
          )
        ),
        div(
          shinycssloaders::withSpinner(plotOutput("SummarisePlot"))
        )
      ),

      box(
        title = "Display Snp+Indel ALL Variants Data", width = 12, status = "success", collapsible = T,
        div(
          style = "position: absolute; right:0.5em; top: 0.5em;",
          dropdown(
            width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
            radioGroupButtons("ALL_SNP_INDEL_data_Set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                              ,justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
          )
        ),
        div(
          DT::dataTableOutput("information")
        )
      )
  )
)
