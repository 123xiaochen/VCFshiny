tabItem(
  tabName = "introduction",
  fluidPage(
    style = "margin-left:100px;margin-right:100px;",
    column(
      12,
      includeMarkdown(system.file("markdown", "installation.md", package = "VCFshiny"))
    )
  )
)
