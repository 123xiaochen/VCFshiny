source("./library.R")
ui <- dashboardPage(
  # skin = "black",
#Header -------------------------------------------------------------------------------------------------------------------------------
  dashboardHeader(title ="WGS Analyse" , titleWidth  = 300),
#Sider---------------------------------------------------------------------------------------------------------------------------------
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Introduction",tabName = "introduction",icon = icon(name = "glyphicon glyphicon-comment", lib = "glyphicon")),
      menuItem("Data Input", icon = icon(name = "glyphicon glyphicon-open",lib = "glyphicon"),tabName = "datainput"),
      menuItem(
        "General Analysis", icon = icon(name = "glyphicon glyphicon-option-vertical",lib = "glyphicon"), startExpanded = TRUE,
        menuItem("ALL SNP+Indel Summarise", tabName = "ALL_variants",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("SNP Heatmap Analyse", tabName = "snp_heatmap",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("SNP ALL Type Analyse", tabName = "snp_all_type",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("Venn Analyse", tabName = "venn_analyse",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("Circle Analyse", tabName = "circle",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon"))
      ),
      menuItem(
        "Extension Analyse", icon = icon(name = "glyphicon glyphicon-option-vertical",lib = "glyphicon"),  startExpanded = TRUE,
        menuItem("Distribution Analyse", tabName = "distribution", 
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("Variants_Summarise", tabName = "Variants_summarise",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("Variants_Heatmap", tabName = "Variants_heatmap",
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon")),
        menuItem("GO/KEGG Analyse", tabName = "DESeq", 
                 icon = icon(name = "glyphicon glyphicon-triangle-right", lib = "glyphicon"))
      )
    )
  ),
  
#BODY --------------------------------------------------------------------------------------------------------------------------------------------------
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
  ### 第一部分 ---------------------------------------------------------------------------------------------------------
    tabItems(
      tabItem("introduction", "INTRODUCTION"),
      tabItem(
        tabName = "datainput",
        fluidPage(
          fluidRow(
            box(
              title = "Data Input", width = 3, status = "info", collapsible = T,
              selectInput(
                inputId = "Species",
                label = "Choose your Species:",
                 c("Human" = "hg38",
                   "Mouse" = "mm10",
                   "Rat" = "rn6",
                   "Sus scrofa" ="susScr11",
                   "Dog" = "canFam4",
                   "others" = "others"),
                 selected = "Human",
                 multiple = F
              ),
              uiOutput(outputId = "chrom_data"),
              fileInput(
                inputId = "file1",label = "choose your .gz/.tar.gz/.zip data:",
                multiple = F, accept = c("gz/zip")
              ),
              selectInput("separator", "TXT Separator:",
                          c("Tab" = "\t",
                            "Comma" = ",",
                            "Semicolon" = ";",
                            "Space" = " "),
                          selected = "Tab"),
              h4("Display Input Data:"),
              actionButton("sample_data","GO !",width = "100%")
            ),
            box(
              title = "Data Display", width = 9 ,status = "primary", collapsible = T,
              uiOutput("sample_id"),
              shinycssloaders::withSpinner(DT::dataTableOutput("sample"))
            )
          )
        )
      ),
  ### 第二部分 ---------------------------------------------------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "ALL_variants",
        fluidPage(
          fluidRow(
            box(
              title = "Set Snp+Indel ALL Variants Plot",width = 3,  status = "info", collapsible = T,
              uiOutput("Summariseplot_stat.test.group"),
              # h4("Set Basics Elements:"),
              # dropdown(
              #   label = "Set  Basics  Elements", width = "100%",icon = icon("cog", lib = "glyphicon"),
                sliderInput("text_size","Text Size:", min = 0, max = 30, value = 10, step = 1),
                sliderInput("bar_width","Bar width:", min = 0, max = 2, value = 0.8, step = 0.1),
              # ),
              h4("Set Addition Elements:"),
              dropdown(
                label = "Set Addition elements", width = "100%",icon = icon("cog", lib = "glyphicon"),
                radioGroupButtons("Summariseplot_label", "Display Label:", c("FALSE","TRUE"), 
                                  justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                sliderInput("error_bar_width","Error bar width", min = 0, max = 1, value = 0.3, step = 0.1),
                sliderInput("error_bar_size","Error bar size", min = 0, max = 2, value = 0.5, step = 0.1),
                sliderInput("jitter_size","Jitter Size", min = 0, max = 5,value = 1,step = 0.1),
                sliderInput("jitter_width","Jitter Width", min = 0, max = 0.5,value = 0.3,step = 0.01),
                sliderInput("brack.size","Brack.size", min = 0, max = 2, value = 0.8, step = 0.1),
                sliderInput("tip.length","Tip.length", min = 0, max = 1,value = 0.1,step = 0.05)
              ),
              h4("Display Plot:"),
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
      ),
      tabItem(
        tabName = "snp_heatmap",
        fluidPage(
          box(
            title = "Set SNP Heatmap Plot",width = 3, status = "info", collapsible = T,
            #h4("Set Basics Elements:"),
            # dropdown(
            #   label = "Set Basics Elements",  width = "100%", right = T,icon = icon("cog", lib = "glyphicon"),
            sliderInput("heatmap_text.size","Text Size:", min = 0, max = 30, value = 18),
            sliderInput("heatmap_ncol", "SNP Heatmap Cols:", min = 1, max = 6, value = 3),
            sliderInput("heatmap_digits", "SNP Heatmap Significant Digits:", min = 0, max = 5,value = 2),
            # ),
            # radioGroupButtons("heatmap_color", "Set Heatmap Color ?", c("No" = "FALSE", "Yes" = "TRUE"),
            #                   justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
            h4("Display Plot:"),
            actionButton("Heatplot","SNP Data Heatmap Plot", icon = icon("bar-chart-o"), width = "100%")
          ),
          box(
            title = "Display SNP Heatmap Plot", width = 9,  status = "primary", collapsible = T,
            div(
              style = "position: absolute; right: 0.5em; top: 0.5em;",
              dropdown(
                label = "Set Download Plot Elements",  width = "100%", right = T,icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
                radioGroupButtons("heatmap_down_filetype","Download Data Type",c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"),
                             justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                sliderInput("heatmap_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
                sliderInput("heatmap_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
                h4("Download Plot"),
                downloadButton("heatmap_plot","Heatmap Plot Download")
              )
            ),
            div(
              shinycssloaders::withSpinner(plotOutput("Heat_Plot"))
            )
          ),
          box(
            title = "Display SNP Heatmap Data", width = 12,  status = "success", collapsible = T,
            div(
              style = "position: absolute; right:0.5em; top: 0.5em;",
              dropdown(
                width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
                radioGroupButtons("Heatmap_data_Set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                             ,justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
              )
            ),
            div(
              DT::dataTableOutput("snp_heatmap")
            )
          )
        )
      ),
      tabItem(
        tabName = "snp_all_type",
        fluidPage(
          box(
            title = "Set SNP All Variants Type Plot", width = 3,  status = "info", collapsible = T,
            h4("Set Baiscs Elements:"),
            #uiOutput("snp_all_Variants_data"),
            sliderInput("ALL_Variants_plot_Text_size", "Text size", min = 0, max = 30, value = 10, step = 1),
            sliderInput("ALL_Variants_plot_bar.width","Bar Width", min = 0, max = 5, value = 0.8, step = 0.1),
            h4("Set Basics Elements:"),
            dropdown(
              label = "Set basics elements",  width = "100%", right = T, icon = icon("cog", lib = "glyphicon"),
              radioGroupButtons("ALL_Variants_plot_label", "Display Label:", c("FALSE","TRUE"), 
                                justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              sliderInput("ALL_Variants_plot_label_digits", "Label digits:", min = 0, max = 5, value = 3, step =1),
              sliderInput("ALL_Variants_plot_Error_bar.size", "Error Bar Size", min = 0, max = 2, value = 0.5 , step = 0.05),
              sliderInput("ALL_Variants_plot_Error_bar.width", "Error Bar Width", min = 0, max = 1, value = 0.3, step = 0.05)
            ),
            h4("Display Plot:"),
            actionButton("ALL_Variantsplot","SNP ALL Variants Type Plot", icon = icon("bar-chart-o"), width = "100%")
          ),
          box(
            title = "Display SNP All Variants Type Plot", width = 9,  status = "primary", collapsible = T,
            div(
              style = "position: absolute; right: 0.5em; top: 0.5em;",
              dropdown(
                label = "Set Download Plot Elements",  width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
                radioGroupButtons("ALL_Variants_down_filetype","Download Data Type",c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"),
                             justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                sliderInput("ALL_Variants_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
                sliderInput("ALL_Variants_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
                h4("Download Plot"),
                downloadButton("ALL_Variants_plot","ALL_Variants Plot Download")
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
      ),
      tabItem(
        tabName = "venn_analyse",
        fluidPage(
          box(
            title = "Set Venn Plot", width = 3, status = "info", collapsible = T,
            #radioButtons("venn_type_id", "Select your type to analyse:", choices = c("snp","indel"), inline = T),
            radioGroupButtons("venn_type_id", "Select your type to analyse:", choices = c("snp","indel"),
                              justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
            uiOutput("venn_group_id"),
            h4("Set Basics Elements:"),
            dropdown(
              label = "Set Basics Elements",  width = "100%", right = T, icon = icon("cog", lib = "glyphicon"),
              radioGroupButtons("venn_box", "Use Box:", c("FALSE", "TRUE"),justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              radioGroupButtons("venn_ellipse", "Use Ellipse:", choices = c("FALSE", "TRUE"),justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              radioGroupButtons("venn_borders", "Use borders", choices = c("FALSE", "TRUE"),justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              sliderInput("venn_ilcs", "Ilcs Size:", min = 0, max = 2, value = 0.8, step = 0.05),
              sliderInput("venn_sncs", "Sncs Size:", min = 0, max = 2, value = 0.8, step = 0.05)
            ),
            br(),
            h4("Display Plot:"),
            actionButton("venn_star", "Snp / Indel Venn Plot", icon = icon("bar-chart-o"), width = "100%")
          ),
          box(
            title = "Display Venn Plot",
            width = 9,  status = "primary", collapsible = T,
            div(
              style = "position: absolute; right: 0.5em; top: 0.5em;",
              dropdown(
                label = "Set Download Plot Elements",  width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
                radioGroupButtons("VennPlot_down_filetype", "Download Data Type", c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"),
                             justified = T, checkIcon = list(yes = icon("ok", lib = "glyphicon"))),
                sliderInput("VennPlot_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
                sliderInput("VennPlot_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
                h4("Download Plot"),
                downloadButton("Venn_download","Venn Plot Download")
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
      ),
      tabItem(
        tabName = "circle",
        fluidPage(
          box(
            title = "Set Circle Plot", width = 3, status = "info", collapsible = T,
            selectInput("circle_type","Circle Type:",
                        choices = c("points", "lines", "rectangles"
                        )),
            radioGroupButtons("circle_type_ID", "Analyse Type:", choices = c("snp","indel"), justified = T),
            h4("Set Basics Elements:"),
            dropdown(
              label = "Set Circle Basics Elements",  width = "100%",  icon = icon("cog", lib = "glyphicon"),
              checkboxGroupButtons("chrom_plotType","Chrom plotType",c("axis",  "labels", "ideogram"),selected = c("ideogram","labels"),
                                  justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              sliderInput("circle_text_size","Text Size",min = 0, max = 5, value = 1.5,step = 0.1),
              sliderInput("track.height","Track.height",min = 0, max = 0.5,value = 0.15, step = 0.01),
              sliderInput("start.degree", "Start.degree", min = 0, max = 180,value = 90, step = 1),
              sliderInput("track.margin1","Track.margin 1",  min = 0, max = 0.1, value = 0.01, step = 0.01),
              sliderInput("track.margin2","Track.margin 2", min = 0, max = 0.1, value = 0.005,step = 0.01),
              sliderInput("track.gap.degree","Track.gap.width", min = 0, max = 10,value = 0.5,step = 0.1)
            ),
            h4("Set Legend Position:"),
            dropdown(
              label = "Set Circle Legend Position ", width = "100%", icon = icon("cog", lib = "glyphicon"),
              sliderInput("legend.x.position", "Legend plot X position", min = -1, max = 1,value = 1, step = 0.05),
              sliderInput("legend.y.position", "Legend plot Y position", min = -1, max = 1,value = 0.5, step = 0.05)
            ),
            h4("Set Circle Type Elements:"),
            uiOutput("circle_type_Basics_set"),
            h4("Display Plot:"),
            actionButton("circle_star","WGS Data Circle Plot", icon = icon("bar-chart-o"), width = "100%"),
          ),
          box(
            title = "Display Circle Plot", width = 9, status = "primary", collapsible = T,
            div(
              style = "position: absolute; right: 0.5em; top: 0.5em;",
              dropdown(
                label = "Set Download Plot Element",  width = "300px", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
                radioGroupButtons("CirclePlot_down_filetype","Download Data Type",c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"),
                             justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                sliderInput("CirclePlot_download_width", "Download Plot width", min = 0, max = 15, value = 10, step = 0.5),
                sliderInput("CirclePlot_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 10, step = 0.5),
                h4("Download Plot"),
                downloadButton("CirclePlot_download","Circle Plot Download")
              )
            ),
            div(
              shinycssloaders::withSpinner(plotOutput("cirecle_PlotOutput")) 
            )
          )
        )
      ),
  ### 第三部分 ---------------------------------------------------------------------------------------------------------------------------------
      tabItem(
        tabName = "distribution",
        fluidPage(
          box(
            title = "Set Distribution Plot", width = 3, status = "info", collapsible = T, 
            radioButtons("distribution_type_id", "Select your type to analyse:", choices = c("snp","indel"), inline = T),
            sliderInput("distribution_text_size", "Text Size", min = 0, max = 30, value = 12, step = 1),
            sliderInput("distribution_bar_width", "Bar Width", min = 0, max = 5, value = 0.8, step = 0.1),
            h4("Set Basics Elements:"),
            dropdown(
              label = "Set Basics Elements", width = "100%", right = T, icon = icon("cog", lib = "glyphicon"), 
              sliderInput("distribution_error_bar_size","Error Bar Size", min = 0, max = 2, value = 0.5, step = 0.05),
              sliderInput("distribution_error_bar_width", "Error Bar Width", min = 0, max = 1, value = 0.3, step = 0.05),
              sliderInput("distribution_jitter_size", "Distribution Jitter Size", min = 0, max = 5, value = 1, step = 0.5),
              sliderInput("distribution_jitter_width", "Distribution Jitter Width", min = 0, max = 1, value = 0.2,step = 0.01)
            ),
            h4("Display Plot:"),
            actionButton("plot_distribution", "Variants Distribution Plot", icon = icon("bar-chart-o"), width = "100%")
          ),
          box(
            title = "Display Distribution", width = 9, status = "primary", collapsible = T,
            div(
              style = "position: absolute; right: 0.5em; top: 0.5em;",
              dropdown(
                label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
                radioGroupButtons("Distribution_down_filetype","Download Data Type",c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"),
                             justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                sliderInput("Distribution_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
                sliderInput("Distribution_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
                h4("Download Plot"),
                downloadButton("Distribution_Download","Distribution Plot Download")
              )
            ),
            div(
              shinycssloaders::withSpinner(plotOutput("Distribution_Plot"))
            )
          ),
          box(
            title = "Display Distribution Data", width = 12, status = "success", collapsible = T,
            div(
              style = "position: absolute; right:0.5em; top: 0.5em;",
              dropdown(
                width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
                radioGroupButtons("Distribution_data_Set", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                             ,justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
              )
            ),
            div(
              DT::dataTableOutput("distribution_data")
            )
          )
        )
      ),
      tabItem(
        tabName = "Variants_summarise", 
        fluidPage(
          box(
            title = "Set Variants Genes Plot", width = 3, status = "info", collapsible = T,
            uiOutput("Variants_sample_id"),
            # radioGroupButtons("Variants_genes_select_position", "Select Position Data:", c(TRUE, FALSE), selected = FALSE, 
            #                   justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
            # uiOutput("Variants_barplot_position_id"),
            h4("Set Basics Elements:"),
            # dropdown(
            #   label = "Set Basics Elements",width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
              radioGroupButtons("Variants_genes_label", "Display Barplot Numbers Label:", c(TRUE, FALSE),
                                justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              sliderInput("Variants_genes_numbers", "Choice Display Gene Numbers:", min = 0, max = 50, value = 20, step = 1),
              sliderInput("Variants_genes_bar_width", "Bar Width", min = 0, max = 5, value = 0.8, step = 0.1),
              sliderInput("Variants_genes_text_size", "Text Size", min = 0, max = 30, value = 12, step = 1),
             # ),
            h4("Display Plot:"),
            actionButton("plot_Variants_genes", "Plot Variants Genes", icon = icon("bar-chart-o"), width = "100%")
          ),
          box(
            title = "Display Variants Genes Plot", width = 9, status = "primary", collapsible = T,
            div(
              style = "position: absolute; right: 0.5em; top: 0.5em;",
              dropdown(
                label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
                radioGroupButtons("Variants_Genes_down_filetype","Download Data Type", c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"), 
                             justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                sliderInput("Variants_Genes_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
                sliderInput("Variants_Genes_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
                h4("Download Plot"),
                downloadButton("Variants_genes_Download","Variants Genes Plot Download")
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
      ),
      tabItem(
        tabName = "Variants_heatmap",
        fluidPage(
          box(
            title = "Set Variants Genes Heatmap Plot", width = 3, status = "info", collapsible = T,
            radioGroupButtons("Variants_heatmap_types", "Select Heatmap Data Types:", c("snp", "indel"), justified = T),
            uiOutput("Variants_position_id"),
            h4("Set Basics Elements:"),
            dropdown(
              label = "Set Basics Elements", width = "100%", right = T, icon = icon("cog", lib = "glyphicon"),
              radioGroupButtons("Variants_heatmap_show_rownames", "Show Rownames", c("TRUE", "FALSE"), 
                                justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              radioGroupButtons("Variants_heatmap_show_colnames", "Show Colnames", c("TRUE", "FALSE"), 
                                justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              sliderInput("Variants_heatmap_treeheight_row","Treeheight Row", min = 0, max = 50, value = 20, step = 1, width = "100%"),
              sliderInput("Variants_heatmap_treeheight_col", "Treeheight Col", min = 0, max = 50, value = 20, step = 1, width = "100%")
            ),
            br(),
            sliderInput("Variants_heatmap_numbers", "Select Gene Numbers:", min = 0, max = 200, value = 50, width = "100%"),
            h4("Display Plot:"),
            actionButton("Variants_heatmap", "Variants Genes Heatmap", icon = icon("bar-chart-o"), width = "100%")
          ),
          box(
            title = "Display Heatmap Plot", width = 9, status = "primary", collapsible = T,
            div(
              style = "position: absolute; right:0.5em; top: 0.5em;",
              dropdown(
                label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
                radioGroupButtons("Variants_heatmap_down_filetype","Download Data Type", c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"),
                                  justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                sliderInput("Variants_heatmap_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
                sliderInput("Variants_heatmap_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
                h4("Download Plot"),
                downloadButton("Variants_heatmap_Download","Variants Heatmap Plot Download")
              )
            ),
            div(
              shinycssloaders::withSpinner(plotOutput("Variants_Heatmap_Plot"))
            )
          ),
          box(
            title = "Display Heatmap Data", width = 12, status = "success", collapsible = T,
            div(
              style = "position: absolute; right:0.5em; top: 0.5em;",
              dropdown(
                width = "300px",icon = icon("cog", lib = "glyphicon"), height = 500, right = T,
                radioGroupButtons("Variants_Genes_Heatmap_data", "Select Part of Data", choices = c("Current Table" = "TRUE", "ALL Table" = "FALSE")
                             , justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon")))
              )
            ),
            div(
               DT::dataTableOutput("Variants_Heatmap_data")
            )
          )
        )
      ),
      tabItem(
        tabName = "DESeq",
        fluidPage(
            box(
              title = "Set Enrich Plot", width = 3, status = "info", collapsible = T,
              radioGroupButtons("DESeq_Type_ID", "Select Your Data Type:", c("snp", "indel"), 
                                selected = "snp", justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              selectInput("enrich_Species_Orgdb", "Select Your Enrich Species",
                          c("Human" = "org.Hs.eg.db",
                            "Rat" = "org.Rn.eg.db",
                            "Mouse" = "org.Mm.eg.db",
                            "Pig" = "org.Ss.eg.db",
                            "Zebrafish" = "org.Dr.eg.db",
                            "Cow" = "org.Bt.eg.db"),selected = "Human", multiple = F),
              uiOutput("DESeq_group_1"),
              uiOutput("DESeq_group_2"),
              uiOutput("DESeq_position_id"),
              radioGroupButtons("enrich_Fun", "Select Your Enrich Function:", c("enrichGO","enrichKEGG"), 
                                selected = "enrichGO", justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              radioGroupButtons("enrich_geneID", "Select Your GeneID Ketype:", c("SYMBOL","ENSEMBL" , "ENTREZID" ), 
                                selected = "SYMBOL", justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              radioGroupButtons("enrich_ont", "Select Your Ont:", c("BP", "MF","CC", "ALL"), 
                                selected  = "ALL", justified = T, checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
              sliderInput("showshowCategory_numbers","Choice EnrichGO ShowNumbers:", min = 0, max = 50,value = 30, step = 2),
              h4("Display Plot:"),
              actionButton("plot_enrichPlot","Display Plot", icon = icon("bar-chart-o"), width = "100%")
            ),
            box(
              title = "Display Enrich Plot", width = 9, status = "primary", collapsible = T,
              div(
                style = "position: absolute; right:0.5em; top: 0.5em;",
                dropdown(
                  label = "Set Download Plot Elements", width = "100%", right = T, icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
                  radioGroupButtons("Enrich_plot_down_filetype","Download Data Type", c("PDF" = "pdf","PNG" = "png", "JPEG" = "jpeg"), 
                               justified = T,checkIcon = list(yes = icon("ok",lib = "glyphicon"))),
                  sliderInput("Enrich_plot_download_width", "Download Plot width", min = 0, max = 15, value = 6, step = 0.5),
                  sliderInput("Enrich_plot_download_height", "DOwnload Plot Height", min = 0, max = 15, value = 4, step = 0.5),
                  h4("Download Plot:"),
                  downloadButton("Enrich_plot_Download","Enrich Plot Download")
                )
              ),
              div(
                shinycssloaders::withSpinner(plotOutput("enrich_Plot"))
              )
            ),
            box(
              title = "Display Enrich Data",width = 12, status = "success", collapsible = T,
              DT::dataTableOutput("DESeq_data")
            )
        )
      )
    )
  )

)

