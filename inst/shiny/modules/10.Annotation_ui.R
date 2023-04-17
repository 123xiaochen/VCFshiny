tabItem(
  tabName = "dataAnno",
  fluidRow(
    box(
      title = "Data Annotation", width = 3, status = "info", collapsible = T,
      tabsetPanel(

        tabPanel(
          title = "Download datebase",
          selectInput("Annovar_download_species", "Select Download Species Version:",
                      c("hg38", "hg19"),
                      selected = "hg38", width = "100%"),
          selectInput("Annovar_download_web", "Select Download Datebase Webfrom:",
                      choices = c("Annovar" = " -webfrom annovar ",
                                  "UCSC" = " "),
                      selected = "Annovar", width = "100%"),
          textAreaInput("Annovar_download_name", "Input Download Datebase Name:", width = "100%",
                        placeholder = "Please enter the name of the database! Ex: refGene"),
          actionButton("Annovar_download_section", "Download Annovar Datebase", width = "100%",
                       style = "background-color: #76a5af; border-radius: 28px;")
        ),
        tabPanel(
          title = "Annotation",
          fileInput(inputId = "input_file_anno",label = "Upload Your VCF Data:", multiple = F, accept = c("gz/zip")),
          prettyRadioButtons(inputId = "annotation_fun",  label = "Select Annotation Function:",
                             choices = c("Annovar", "VariantAnntation"),
                             selected = "Annovar", inline = T,  animation = "tada", shape = "square", bigger = T, width = "100%"),
          conditionalPanel("input.annotation_fun == 'Annovar'",
            uiOutput("Annovar_anno_Species"),
            uiOutput("Annovar_anno_Protocol"),
            textAreaInput(inputId = "Annovar_opreation", label ="Select Annotation Operation:", width = "100%",
                          placeholder = "Please enter the annotation mode in the order of the database! Ex: g,r,f (g,r is necessary)"),
            actionButton("Annovar_section", "Annovar Annotation Data", width = "100%",
                         style = "background-color: #76a5af; border-radius: 28px;"),
          ),
          conditionalPanel("input.annotation_fun == 'VariantAnntation'",
            uiOutput("VariantAnnotation_species"),
            selectInput("Variantanno_species", "organism type", c("hg38", "hg19"), selected = "hg38", width = "100%"),
            actionButton("VariantAnnotation_section", "VariantAnnotation Annotation Data", width = "100%",
                          style = "background-color: #76a5af; border-radius: 28px;"),

          )
        )
      )
    ),
    box(title = "Data Display", width = 9, status = "primary", collapsible = T,
        conditionalPanel(
          "input.annotation_fun == 'Annovar'",
          div(
            style = "position: absolute; right:0.5em; top: 0.5em;",
            downloadButton('Annovar_tab_download','Download TXT', class = "btn", icon = icon("download", lib = "glyphicon"))
          ),
          div(
            shinycssloaders::withSpinner(DT::dataTableOutput("variantanno_datebase_filename")),
            uiOutput("Annovar_anno_id"),
            shinycssloaders::withSpinner(DT::dataTableOutput("Annovar_anno_table"))
          )
        ),
        conditionalPanel(
          "input.annotation_fun == 'VariantAnntation'",
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
    box(title = "Annovar Datebase Display", width = 12,  status = "success", collapsible = T,
        div(
          style = "position: absolute; right:0.5em; top: 0.5em;",
          downloadButton('Annovar_download_datebase_download','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
        ),
        div(
          DT::dataTableOutput("Annovar_Datebase_Table")
        )
    )
  )
)
