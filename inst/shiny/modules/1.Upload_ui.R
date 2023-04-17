tabItem(
  tabName = "datainput",
  fluidRow(
      box(
        title = "Data Input", width = 3, status = "info", collapsible = T,
        fileInput(
          inputId = "input_file",label = "Upload your data:",
          multiple = F, accept = c("gz/zip")
        ),
        prettyRadioButtons(inputId = "use_example",  label = "Load example data ?", choices = c("TRUE", "FALSE"),
                           selected = "FALSE", inline = T,  animation = "tada", shape = "square", bigger = T, width = "100%"),
        actionButton("sample_data","Upload Data", width = "100%", style = "background-color: #76a5af; border-radius: 28px;"),
      ),
      box(title = "Data Display", width = 9, status = "primary", collapsible = T,
          uiOutput("sample_id"),
          shinycssloaders::withSpinner(DT::dataTableOutput("raw_sample"))
          ),
      column(
          12,
          wellPanel(
          tags$h3("Explanation of input data and example data !"),
          tags$h4("Requirements for input data:"),
          p("Before using the tool, please read the introduction, understand the required file naming format, and make sure that the input file is the annotated variant storage file.
            The format of the input file can be a TXT file, a VCF file, or a VCF.gz file, and make sure that the input file is a compressed folder where all the data is stored.."),
          tags$li("Requirements for VCF input data:"),
          p("The Variant Call Format (VCF) is used to record gene sequence variations.
             It is also the first file format to be understood for genome population correlation analysis.
             The file is divided into two main parts: the Header comment section, which begins with #, and the body section.
            "),
          tags$li("Requirements for TXT input data:"),
          p("The txt file is one of several output formats followed by tab-delimited comments.
            Since some users cannot annotate steps, we provide two annotation methods, Annovar and VariantAnnotation, which users can choose to annotate themselves or use this tool."),
          tags$h4("The source of example data:"),
          p("Sample data are reported from AGBE, published in Nucleic Acids Res:A dual deaminase-mediated base editor by fusing CGBE with ABE for creating a saturated mutant population with multiple editing patterns.
            The author has participated in the analysis of the relevant data in the paper, so four of the five data samples are selected as example data.")
        )
      )
  )
)
