tabItem(
  tabName = "introduction",
  includeHTML("home.html"),
  fluidPage(
    style = "margin-left:200px;margin-right:200px;",
    column(
      2,
      br(),
      tags$nav(
        id = "home_nav", class="affix",
        # style = "border-left: 1px solid black;",
        tags$ul(
          class="nav",
          tags$li(tags$a("Abstract", href="#abstract")),
          tags$li(tags$a("Features", href="#features")),
          tags$li(tags$a("Installing", href="#install")),
          tags$li(tags$a("Getting Start", href="#getting-start")),
          tags$li(tags$a("Documentation", href="#documentation")),
          tags$li(tags$a("Development", href="#development"))
        )
      )
    ),
    column(
      10,
      includeMarkdown("../markdown/installation.md")
    )
  )
)
