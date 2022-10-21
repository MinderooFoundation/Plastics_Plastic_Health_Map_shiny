library(shiny)
### include style for header
head.style <- "
         /* old shiny progress indicators */
         .shiny-progress-container {
           position: fixed;
           top: 0px;
           width: 100%;
           z-index: 4000;
     }
     .shiny-progress .progress-text {
       color: #020202;
       background-colort: #FF0000;
       width: 225px;
       left: calc(50% - 125px);
     }
     .progress-text {
       /* Copy the below to vertically center the progress bar text box in the shiny dashboard header */
       /* !important is crucial here otherwise it gets overridden by the dreaded element.style */
       top: 15px !important;
       text-align: center;
     }
     "

#initiate dashboard attributes and colors
ui <- dashboardPage(
  skin = "purple",
  
  dashboardHeader(
    title = HTML("Title"),
    dropdownMenu(type = "notifications",  icon = tagList(icon("question-circle"), "Help"),  badgeStatus = NULL,  headerText = "Links",
                 tags$li(a(icon("external-link"),  "XYZ",  href = "http://info.com", target = "blank")),
                 tags$li(a(icon("external-link"),  "ABC", href = "http://info.com", target = "blank")))
  ),
  
  dashboardSidebar(sidebarMenu(
    menuItem("Target Dashboard", tabName = "dashboard_tab", icon = icon("dashboard"))
  )),
  
  #################################################################################
  #################################################################################
  #################################################################################
  #################################################################################
  
  #Configure dashboard body.
  dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "favicon.ico"),
      tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.ico"),
      tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
      tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
      tags$style(head.style)
    ),
    
    
    #h1(paste0("<b>","Gene summary:","</b>")),
    titlePanel(div(HTML("<b>Gene summary</b>"), align = "left")),
    
    tabItems(
      tabItem(tabName = "dashboard_tab",
              tags$style(HTML("
                  #first {
                      border: 4px double red;
                  }
                  #second {
                      border: 2px dashed blue;
                  }
                ")),
              
              fluidRow(width=12,
                       tabBox(id = "tabset1", height = "2250px", width=12, title = " ",
                              tabPanel(
                                br(),br(),
                                fluidRow(h2(paste0("Header 0"), align="left")),
                                fluidRow(
                                  shinydashboard::valueBoxOutput("myvaluebox1", width=4),
                                  shinydashboard::valueBoxOutput("myvaluebox2", width=4),
                                  shinydashboard::valueBoxOutput("myvaluebox3", width=4)
                                ),
                                fluidRow(
                                  shinydashboard::valueBoxOutput("myvaluebox4", width=4),
                                  shinydashboard::valueBoxOutput("myvaluebox5", width=4),
                                  shinydashboard::valueBoxOutput("myvaluebox6", width=4)
                                ),
                                fluidRow(
                                  shinydashboard::infoBoxOutput("myvaluebox7", width=4),
                                  shinydashboard::valueBoxOutput("myvaluebox8", width=4)
                                ),
                                br(),
                                fluidRow(h2(paste0("Header 1"), align="left")),
                                #br(),
                                fluidRow(img(src='man_log.png', height="5%", width="5%", align="left")),
                                br(),
                                fluidRow(
                                  column(6,
                                         box( height="300px",  width=NULL,
                                              collapsible = TRUE,
                                              title = "Box 1",
                                              status = "primary",
                                              solidHeader = TRUE,
                                              plotOutput("plot1", height = "210px", width="350px")
                                         ),  style='width: 500px; height: 400px' ),
                                  column(6,
                                         box(height="300px", width="450px",
                                             title = "Box 2",
                                             status = "primary",
                                             solidHeader = TRUE,
                                             collapsible = TRUE,
                                             plotOutput("plot2", height = "230px", width="380px")
                                         ),  style='width: 500px; height: 400px') 
                                ),
                                br(),# br(),
                                img(src='mouse.png', height="10%", width="10%", align="left"),
                                br(),
                                h2(paste0("Header 2"), align="left"),
                                br(),
                                fluidRow(
                                  column(6,
                                         box(height="300px", width="450px",
                                             title = "Box 3",
                                             status = "primary",
                                             solidHeader = TRUE,
                                             collapsible = TRUE,
                                             plotOutput("plot3", height = "220px", width="350px")
                                         ),  style='width: 500px; height: 400px' ),
                                  column(6,
                                         box(height="300px", width="450px",
                                             title = "Box 4",
                                             status = "primary",
                                             solidHeader = TRUE,
                                             collapsible = TRUE,
                                             plotOutput("plot4", height = "220px", width="350px")
                                         ),  style='width: 500px; height: 400px')
                                )
                              )  ## end of tabPanel
                              
                       )  ## end of tabBox
              ) 
              
      )  ## end of tabItem
      
    )
  )
)


server <- function(input, output, session){
  output$plot1 <- renderPlot(qplot(rnorm(500),fill=I("red"),binwidth=0.2,title="plotgraph1"))
  output$plot2 <- renderPlot(qplot(rnorm(500),fill=I("green"),binwidth=0.2,title="plotgraph2"))
  output$plot3 <- renderPlot(qplot(rnorm(500),fill=I("blue"),binwidth=0.2,title="plotgraph3"))
  output$plot4 <- renderPlot(qplot(rnorm(500),fill=I("orange"),binwidth=0.2,title="plotgraph4"))
  
  output$myvaluebox1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox('2000',subtitle = "blah blah blah1",icon = icon("car"),
                             color = "green"
    )
  })
  output$myvaluebox2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox('2001',subtitle = "blah blah blah2",icon = icon("car"),
                             color = "green"
    )
  })
  output$myvaluebox3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox('2002',subtitle = "blah blah blah3",icon = icon("car"),
                             color = "green"
    )
  })
  output$myvaluebox4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox('2009',subtitle = "blah blah blah4",icon = icon("car"),
                             color = "red"
    )
  })
  output$myvaluebox5 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox('2010',subtitle = "XYZ1",icon = icon("car"),
                             color = "red"
    )
  })
  output$myvaluebox6 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox('2011',subtitle = "XYZ2",icon = icon("car"),
                             color = "green"
    )
  })
  output$myvaluebox7 <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox('2020',subtitle = "This is infobox",icon = icon("car"),
                            color = "blue"
    )
  })
  output$myvaluebox8 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox('2021',subtitle = "This is valuebox",icon = icon("car"),
                             color = "blue"
    )
  })
}

shinyApp(ui = ui, server = server)