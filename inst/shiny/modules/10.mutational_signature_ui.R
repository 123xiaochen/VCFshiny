tabItem(
  tabName = "cancer_signature",
  fluidRow(
      box(
        title = "Signatures Parameter Setting", width = 3, status = "info", collapsible = T,
        uiOutput("signature_sample_id"),
        selectInput(inputId = "signature_genome", label = "Select Data Genome:", choices = c("hg19", "hg38"), selected = "hg38", width = "100%"),
        selectInput(inputId = "Signature_Build_Table_Name", label = "Select Data Build:",
                    choices = c("SBS (SBS96)" = "SBS96", "DBS (DBS78)" = "DBS78", "INDEL (IND83)" = "IND83"), selected = "SBS (SBS96)", width = "100%"),
        selectInput(inputId = "signature_discover_algorithm", label = "Select Discover Algorithm：", choices = c("nmf", "lda"), selected = "nmf", width = "100%"),
        numericInput(inputId = "num_signatures", label =  "Numbers Signatures:", min = 1, max = 10, value = 3, step = 1, width = "100%"),
        conditionalPanel("input.Signature_Build_Table_Name == 'SBS96'",
                         selectInput(inputId = "signature_compare_cosmic", label = "Compare With Cosmic?", choices = c("TRUE", "FALSE"), selected = "FALSE", width = "100%")
        ),
        actionButton(inputId = "Signature_modal_but",label = "Set Additional Parameters ...", width = "100%",
                     style = "background-color: rgb(255,255,255);text-align:left; margin-top:15px; margin-bottom:8px",
                     icon = icon(lib ="glyphicon" ,"glyphicon glyphicon-cog")),

        actionButton(inputId = "analysis_signature", label =  "Analysis Signature Data", width = "100%", style = "background-color: #76a5af; border-radius: 28px;")
      ),
      bsModal(
          "Signature_modal", "Additional Parameters", "Signature_modal_but", size = "large",
          fluidRow(
            style = "padding: 10px",
            column(
              12,
              conditionalPanel("input.signature_compare_cosmic == 'TRUE'",
                        numericInput(inputId = "signature_discover_compare_threshold", label = "Compare With Cosmic Threshold:", min = 0.1, max = 1, value = 0.7,
                                     step = 0.05, width = "100%")
              ),
              conditionalPanel(condition = "input.signature_compare_cosmic == 'FALSE'",
                numericInput(inputId = "signature_discover_text_size", label = "Select Text Size:", min = 1, max = 20, value = 12, step = 1, width = "100%"),
                numericInput(inputId = "signature_discover_facet_size", label = "Select Facet Size:", min = 1, max = 20, value = 10, step = 1, width = "100%"),
              )
            )
          )
      ),
      box(
        title = "Signatures Plot Display", width = 9, status = "primary", collapsible = T,
        div(
          style = "position: absolute; right: 0.5em; top: 0.5em;",
          dropdown(
            label = "Set Download Plot Elements",  width = "100%", right = T,icon = icon(name = "glyphicon glyphicon-save",lib = "glyphicon"),
            sliderInput("Mutational_Signature_download_width", "Download Plot width", min = 0, max = 20, value = 8, step = 0.5, width = "100%"),
            sliderInput("Mutational_Signature_download_height", "Download Plot Height", min = 0, max = 15, value = 4, step = 0.5, width = "100%"),
            h4("Download Plot"),
            downloadButton("Download_mutational_signature_Plot","Download Plot")
          )
        ),
        div(
          shinycssloaders::withSpinner(plotOutput("Display_mutational_signature_plot"))
        ),
      ),
      box(
        title = "Signatures Results Display", width = 12, status = "success", collapsible = T,
        div(
          style = "position: absolute; right:0.5em; top: 0.5em;",
          downloadButton('Download_mutational_signature_data','Download CSV', class = "btn", icon = icon("download", lib = "glyphicon"))
        ),
        div(
          shinycssloaders::withSpinner(DT::dataTableOutput("Display_mutational_signature_data"))
        )
    )
  )
)
