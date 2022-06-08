source("./library.R")
navbarPage(
  "WGS Analyse",
  tabPanel(
    "数据读入",
    sidebarLayout(
      sidebarPanel(
        selectInput("Species",
                    label = "Choose your Species",
                    c("Human" = "hg38",
                      "Mouse" = "mm10",
                      "Rat" = "rn6",
                      "Sus scrofa" ="susScr11",
                      "Dog" = "canFam4",
                      "others" = "others"
                    ),
                    selected = "Human",
                    multiple = F
        ),
        uiOutput("chrom_data"),
        tags$hr(),
        h3("For example: "),
        h5("SampleNames-numbers.snp.vcf/vcf.gz"),
        h5("SampleNames-numbers.indel.vcf/vcf.gz"),
        h5("SampleNames-numbers.snp.txt"),
        h5("SampleNames-numbers.indel.txt"),
        tags$hr(),
        fileInput("file1","choose your .gz/.tar.gz/.zip data",
                  multiple = F,
                  accept = c("gz/zip")
        ),
        
        radioButtons("header","TXT Header",c("TRUE","FALSE"),selected = "TRUE", width = "60%",inline = T),
        #radioButtons("rownmames", "TXT Row.names", c("TRUE", "FALSE"), selected = "TRUE",width = "60%",inline = T),
        selectInput("separator", "TXT Separator",
                    c(
                      "Tab" = "\t",
                      "Comma" = ",",
                      "Semicolon" = ";",
                      "Space" = " "
                    ),selected = "Tab"),
        actionButton("sample_data","GO !"),
        width = 3
      ),
      mainPanel(
        h1("Sample_data"),
        uiOutput("sample_id"),
        shinycssloaders::withSpinner(DT::dataTableOutput("sample")),
      )
    )
  ),
  
  tabPanel(
    "数据处理",
    tabsetPanel(
      tabPanel(
        "SNP + Indel 数据统计柱状图",
        sidebarLayout(
          sidebarPanel(
            radioButtons("set_SNP_Indel_defaults_paramenters","Set Plot Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "80%",inline = T),
            uiOutput("set_snp_indel_data"),
            uiOutput("Summariseplot_stat.test.group"),
            #tags$hr(),
            h2("Plot Display"),
            actionButton("sample_information","Sample_Information",width = "60%"),
            tags$hr(),
            actionButton("Data_summarise_plot","SNP + Indel 数据柱状图"),
            h2("Plot Download"),
            radioButtons("set_SNP_Indel_download_paramenters","Set Download Default Parameters",c("TRUE","FALSE"),selected = "FALSE", width = "100%",inline = T),
            uiOutput("set_snp_indel_download"),
            downloadButton("summarise_plot","Summarise Plot Download"),
            width = 2
          ),
          mainPanel(
            fluidRow(
              column(
                6,
                verticalLayout(h1("data information"),DT::dataTableOutput("information",width = "80%", height = "600px")
                )
              ),
              column(
                6,
                verticalLayout(h1("Data_summarise_plot"),
                               tabPanel("SNP + Indel 数据柱状图",plotOutput("SummarisePlot", width = "100%", height = "600px"))
                ),
              )
            ),
            width = 10
          )
        )
      ),
      tabPanel(
        "SNP 突变热图统计",
        sidebarLayout(
          sidebarPanel(
            radioButtons("set_SNPHeat_defaults_paramenters","Set Plot Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "80%",inline = T),
            uiOutput("set_snp_heatmap_data"),
            h2("Plot Display"),
            actionButton("snp_heatmap_data","SNP 热图数据"),
            hr(),
            actionButton("Heatplot","SNP 突变热图"),
            h2("Plot Download"),
            radioButtons("set_SNPHeat_download_paramenters","Set Download Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "100%",inline = T),
            uiOutput("set_snp_heatmap_download"),
            downloadButton("heatmap_plot","Heatmap Plot Download"),
            width = 2
          ),
          mainPanel(
            fluidRow(
              column(
                6,
                verticalLayout(h1("SNP 热图数据"),
                               DT::dataTableOutput("snp_heatmap",width = "80%", height = "600px"))
              ),
              column(
                6,
                verticalLayout(h1("SNP 热图"),
                               tabPanel("SNP 突变热图",plotOutput("Heat_Plot",width = "80%", height = "400px")))
              )
            ),
            width = 10
          )
        )
      ),
      tabPanel(
        "SNP 各突变类型统计",
        sidebarLayout(
          sidebarPanel(
            radioButtons("set_SNP_ALL_variants_plot_paramenters","Set Plot Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "80%",inline = T),
            uiOutput("snp_all_Variants_data"),
            h2("Plot Display"),
            actionButton("SNP_ALL_variants_data","SNP 各突变类型数据"),
            tags$hr(),
            actionButton("ALL_Variantsplot","SNP 各突变类型柱状图"),
            h2("Plot Download"),
            radioButtons("set_SNP_ALL_variants_download_paramenters","Set Download Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "100%",inline = T),
            uiOutput("snp_all_Variants_download"),
            downloadButton("ALL_Variants_plot","ALL_Variants Plot Download"),
            width = 2
          ),
          mainPanel(
            fluidRow(
              column(
                6,
                verticalLayout(h1("SNP 各突变类型类型数据"),
                               DT::dataTableOutput("SNP_ALL_variants_data"))
              ),
              column(
                6,
                verticalLayout(h1("SNP ALLType plot"),
                               tabPanel("SNP 各突变类型柱状图",plotOutput("SNP_type_plot", width = "100%", height = "600px")))
              )
            ),
            width = 10
          )
        )
      ),
      tabPanel(
        "SNP/Indel 展示各组交叉情况",
        sidebarLayout(
          sidebarPanel(
            uiOutput("venn_group_id"),
            uiOutput("venn_type_id"),
            radioButtons("venn_box", "Use Box:", c("TRUE", "FALSE"),inline = T),
            tags$hr(),
            h2("Plot Display"),
            actionButton("venn_data","Venn Data"),
            tags$hr(),
            actionButton("venn_plot", "Venn Plot"),
            h2("Plot DOwnload"),
            radioButtons("set_Venn_plot_paramenters","Set Download Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "100%",inline = T),
            uiOutput("venn_download"),
            downloadButton("VennPlot_plot","Venn Plot Download"),
            width = 2
          ),
          mainPanel(
            fluidRow(
              column(
                6, 
                verticalLayout(h1("venn plot 交叉数据"),
                               DT::dataTableOutput("venn_Table", width = "100%", height = "600px"))
              ),
              column(
                6,
                verticalLayout(h1("Venn Plot"),
                               plotOutput("SNP_venn_plot", width = "100%", height = "600px"))
              )
            ),
            width = 10
          )
        )
      ),
      tabPanel(
        "圈图",
        sidebarLayout(
          sidebarPanel(
            selectInput("circle_type","Circle Type",
                        choices = c("points", "lines", "rectangles", "heatmap","density"
                        )),
            radioButtons("circle_type_ID", "Analyse Type", choices = c("snp","indel"), inline = T),
            radioButtons("set_circle_plot_paramenters","Set Plot Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "80%",inline = T),
            uiOutput("circle_set_plot_data"),
            radioButtons("set_circle_legend_position","Set Legend Position Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "100%",inline = T),
            uiOutput("circle_set_legend_position"),
            radioButtons("set_circle_Type_position","Set Circle Type Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "100%",inline = T),
            uiOutput("circle_type_set"),
            h2("Plot Display"),
            actionButton("circle_star","Circle_Plot"),
            h2("Plot Download"),
            radioButtons("set_circle_download_paramenters","Set Download Default Parameters",c("TRUE","FALSE"), selected = "FALSE", width = "100%",inline = T),
            uiOutput("circle_set_download"),
            
            downloadButton("CirclePlot_download","Circle Plot Download"),
            width = 2
          ),
          mainPanel(
            #shinycssloaders::withSpinner(DT::dataTableOutput("circle_Table", width = "100%", height = "600px")),
            shinycssloaders::withSpinner(plotOutput("cirecle_PlotOutput", width = "100%", height = "600px")),
            width = 10
          )
        )
      )
      
    )
  )
)

