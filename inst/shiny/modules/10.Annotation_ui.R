tabItem(
  tabName = "dataAnno",
  fluidRow(
    tabsetPanel(
      tabPanel(title = "Annovar Annotation",
        box(title = "Annotation Parameter Setting", width = 3, status = "info", collapsible = T,
            fileInput(inputId = "input_file_anno", label = "Upload Your VCF Data:", multiple = F, accept = c("gz/zip"), width = "100%"),
            selectInput("Annovar_species", "Select Download Species Version:", c("hg38", "hg19"), selected = "hg38", width = "100%"),
            selectInput(inputId = "Annovar_anno_type", label = "Select Annotation Type:",
                        choices = c("Gene-based Annotation" = "-geneanno", "Region-based Annotation" = "-regionanno", "Filter-based Annotation" = "-filter"),
                        selected = "Gene-based Annotation", multiple = T, width = "100%"),
            uiOutput("Annovar_anno_gene_Database"),
            uiOutput("Annovar_anno_region_Database"),
            uiOutput("Annovar_anno_Filter_Database"),
            actionButton("Annovar_section", "Annovar Annotation Data", width = "100%",
                         style = "background-color: #76a5af; border-radius: 28px;")
        ),
        box(title = "Annotation Results Display", width = 9, status = "primary", collapsible = T,
            div(
              style = "position: absolute; right:0.5em; top: 0.5em;",
              downloadButton('Annovar_tab_download','Download TXT', class = "btn", icon = icon("download", lib = "glyphicon"))
            ),
            div(
              uiOutput("Annovar_anno_id"),
              shinycssloaders::withSpinner(DT::dataTableOutput("Annovar_anno_table"))
            )
        )
      ),
      tabPanel(title = "VariantAnnotation Annotation",
        box(title = "Anntation Parameter Setting", width = 3, status = "info", collapsible = T,
            fileInput(inputId = "input_file_Variantanno",label = "Upload Your VCF Data:", multiple = F, accept = c("gz/zip"), width = "100%"),
            selectInput("Variantanno_species", "organism type", c("hg38", "hg19"), selected = "hg38", width = "100%"),
            actionButton("VariantAnnotation_section", "VariantAnnotation Annotation Data", width = "100%",
                         style = "background-color: #76a5af; border-radius: 28px;"),
        ),
        box(title = "Annotation Results Display", width = 9,  status = "primary", collapsible = T,
            div(
              style = "position: absolute; right:0.5em; top: 0.5em;",
              downloadButton('variantanno_tab_download','Download TXT', class = "btn", icon = icon("download", lib = "glyphicon"))
            ),
            div(
              uiOutput("VariantsAnno_anno_id"),
              shinycssloaders::withSpinner(DT::dataTableOutput("variantanno_anno_table"))
            )
        )
      ),
      tabPanel(title = "Download Annovar Datebase",
        uiOutput("Download_Annovar_Database_warning"),
        box(title = "Download Parameter Setting", width = 3,  status = "info", collapsible = T,
            selectInput("Annovar_download_species", "Select Download Species Version:", c("hg38", "hg19"), selected = "hg38", width = "100%"),
            selectInput("Annovar_download_database_type", "Select Download Database Type:", c("gene-based", "region-based", "filter-based"),
                        selected = "gene-based", width = "100%"),
            uiOutput("Annovar_Download_Name"),
            actionButton("Annovar_download_section", "Download Annovar Datebase", width = "100%",
                         style = "background-color: #76a5af; border-radius: 28px;"),
        ),
        box(title = "List of local datebase", width = 9,  status = "primary", collapsible = T,
            div(
              style = "position: absolute; right:0.5em; top: 0.5em;",
              downloadButton('Annovar_Download_Tab_Download','Download TXT', class = "btn", icon = icon("download", lib = "glyphicon"))
            ),
            div(
              DT::dataTableOutput("Annovar_Download_Tab")
            )
        ),
        box(title = "List of Datebase avaliable", width = 12, status = "success", collapsible = T,
            div(
              style = "position: absolute; right:0.5em; top: 0.5em;",
              downloadButton('Annovar_Database_Tab_Download','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
            ),
            div(
              DT::dataTableOutput("Annovar_Database_Tab")
            )
        )
      )
    )
  )
)
