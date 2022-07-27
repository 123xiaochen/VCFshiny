tabItem(
  tabName = "snp_all_type",
  fluidPage(
    box(
      title = "Set SNP All Variants Type Plot", width = 3,  status = "info", collapsible = T,
      sliderInput("ALL_Variants_plot_Text_size", "Text size:", min = 0, max = 30, value = 10, step = 1, width = "100%"),
      sliderInput("ALL_Variants_plot_bar.width","Bar Width:", min = 0, max = 5, value = 0.8, step = 0.1, width = "100%"),
      radioGroupButtons("ALL_Variants_plot_label", "Display Label:", c("FALSE","TRUE"),
                          justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")), width = "100%"),
      h4("Set Basics Elements:"),
      actionButton("snp_AllType_module_but", "Additional Parameters ...", width = "100%",
                   style = "background-color: rgb(255,255,255);text-align:center",
                   icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),
      h4("Display Plot:"),
      actionButton("ALL_Variantsplot","SNP ALL Variants Type Plot", icon = icon("bar-chart-o"), width = "100%")
    ),
    bsModal(
        "snp_All_Type_module_but", "Additional Parameters", "snp_AllType_module_but", size = "small",
        fluidPage(
          style = "padding: 10px",
          column(
            width = 12,
            sliderInput("ALL_Variants_plot_label_digits", "Label digits:", min = 0, max = 5, value = 3, step =1, width = "100%"),
            sliderInput("ALL_Variants_plot_Error_bar.size", "Error Bar Size", min = 0, max = 2, value = 0.5 , step = 0.05, width = "100%"),
            sliderInput("ALL_Variants_plot_Error_bar.width", "Error Bar Width", min = 0, max = 1, value = 0.3, step = 0.05, width = "100%"),
            textAreaInput("ALL_Variants_plot_ggText", "ggplot2 Codes:", rows = 5, width = "100%")
          )
        )
    ),
    box(
      title = "Display SNP All Variants Type Plot", width = 9,  status = "primary", collapsible = T,
      div(
        style = "position: absolute; right: 0.5em; top: 0.5em;",
        dropdown(
          label = "Set Download Plot Elements",  width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save", lib = "glyphicon"),
          sliderInput("ALL_Variants_download_width", "Download Plot width:", min = 0, max = 15, value = 6, step = 0.5, width = "100%"),
          sliderInput("ALL_Variants_download_height", "DOwnload Plot Height:", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
          h4("Download Plot:"),
          downloadButton("ALL_Variants_plot","Download Plot")
        )
      ),
      div(
        shinycssloaders::withSpinner(plotOutput("SNP_type_plot"))
      )
    ),
    box(
      title = "Display SNP All Variants Type Data ", width = 12, status = "success", collapsible = T,
      div(
        style = "position: absolute; right:0.5em; top: 0.5em;",
        dropdown(
          width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
          radioGroupButtons("ALL_Variants_data_Set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                            , justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon")))
        )
      ),
      div(
        DT::dataTableOutput("SNP_ALL_variants_data")
      )
    )
  )
)
