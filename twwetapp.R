if (interactive()) {
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  
  shinyApp(
    ui = dashboardPage(
      dashboardHeader(),
      dashboardSidebar(),
      dashboardBody(
        box(
          title = "Social Media Share",
          status = NULL,
          socialButton(
            href = "https://twitter.com",
            icon = icon("twitter")
          ),
          socialButton(
            href = "https://facebook.com",
            icon = icon("facebook")
          ),
          socialButton(
            href = "https://linkedin.com",
            icon = icon("linkedin")
          )
        )
      ),
      title = "Social Media Share"
    ),
    server = function(input, output) { }
  )
}
