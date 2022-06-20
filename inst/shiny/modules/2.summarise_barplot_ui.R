tabItem(
  tabName = "ALL_variants",
  fluidPage(
    fluidRow(
      box(
        title = "Summary Plot Parameters",width = 3,  status = "info", collapsible = T,
        uiOutput("Summariseplot_stat.test.group"),
        # h4("Set Basics Elements:"),
        # dropdown(
        #   label = "Set  Basics  Elements", width = "100%",icon = icon("cog", lib = "glyphicon"),
        numericInput("text_size","Text Size:", min = 0, max = 30, value = 10, step = 1),
        # sliderInput("text_size","Text Size:", min = 0, max = 30, value = 10, step = 1),
        sliderInput("bar_width","Bar width:", min = 0, max = 2, value = 0.8, step = 0.1),
        # ),
        radioGroupButtons("Summariseplot_label", "Display Label:", c("FALSE","TRUE"),
                          justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
        actionButton("summarise_modal_but", "Additional Parameters ...", width = "100%",
                     style = "background-color: rgb(255,255,255);text-align:left;margin-bottom:10px;margin-top:10px", icon = icon("plus-square")),
        actionButton("Data_summarise_plot","SNP+Indel ALL Variants Plot",icon = icon("bar-chart-o"), width = "100%")
      ),
      box(
        title = "Display Snp+Indel ALL Variants Plot",width = 9, status = "primary", collapsible = T,
        div(
          style = "position: absolute; right: 0.5em; top: 0.5em;",
          dropdown(
            label = "Set Download Plot Elements", width = "100%",height = 500, right = T,icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
            radioGroupButtons("summarise_down_filetype","Download Data Type",c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"),
                              justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
            sliderInput("summarise_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
            sliderInput("summarise_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
            h4("Download Plot:"),
            downloadButton("summarise_plot","Summarise Plot Download")
          )
        ),
        div(
          shinycssloaders::withSpinner(plotOutput("SummarisePlot"))
        )
      ),
      bsModal(
        "summariseplot_modal_but", "Additional Parameters", "summarise_modal_but", size = "small",
        fluidRow(
          style = "padding: 10px",
          column(
            12,
            # style = "margin:5px; text-align:justify;color:black;background-color:papayawhip;border-radius:10px;border:1px solid black;", br(),
            sliderInput("error_bar_width","Error bar width", min = 0, max = 1, value = 0.3, step = 0.1),
            sliderInput("error_bar_size","Error bar size", min = 0, max = 2, value = 0.5, step = 0.1),
            sliderInput("jitter_size","Jitter Size", min = 0, max = 5,value = 1,step = 0.1),
            sliderInput("jitter_width","Jitter Width", min = 0, max = 0.5,value = 0.3,step = 0.01),
            sliderInput("brack.size","Brack.size", min = 0, max = 2, value = 0.8, step = 0.1),
            sliderInput("tip.length","Tip.length", min = 0, max = 1,value = 0.1,step = 0.05),
            textAreaInput("summ_ggText", "ggplot2 Codes:", rows = 5, width = "100%")
          )
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
)
