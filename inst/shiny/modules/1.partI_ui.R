tabItem(
  tabName = "datainput",
  fluidPage(
    fluidRow(
      box(
        title = "Data Input", width = 3, collapsible = T, solidHeader = T,
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
        checkboxInput("use_example", "Use example data ?", value = F, width = "100%"),
        actionButton("sample_data","Upload Data !", width = "100%", class = "btn-upload")
      ),
      box(
        title = "Data Display", width = 9, collapsible = T,
        uiOutput("sample_id"),
        shinycssloaders::withSpinner(DT::dataTableOutput("sample"))
      ),
      column(
        12,
        wellPanel(
          tags$li("Registration is now open for the BioC22 "),
          p("Registration is now open for the BioC22 conference. You have two ticket options:

          Tickets for virtual attendance are now available on Eventbrite at https://bioc2022.eventbrite.com.
          Tickets for in-person attendance of the conference in Seattle, Washington (USA) are currently limited due to COVID capacity limits at the venue. As of 5/31/22, we have 20 in-person tickets available. You can register at https://bioc2022.eventbrite.com. If tickets have sold out, please email conference@bioconductor.org to be placed on a wait list.
          Both tickets provide access to all conference sessions. We will have a variety of in-person and virtual speakers. Anyone who does not get an in-person ticket will be able to secure a virtual ticket.

          Registration cost

          $400 academic in person
          $250 student in person
          $50 academic virtual
          $10 student virtual")
        )
      )
    )
  )
)
