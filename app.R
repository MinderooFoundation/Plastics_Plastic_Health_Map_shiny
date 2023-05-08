# Scoping review interactive plots #
# YM 25/05/2022 #
#AE 27/07/2022
source("helper.R")

#####################################################
# bugs:####
#
# - alpha log1p(n) reflected in legend bar - low priority
#
# It is Yannick and Ahmed repo
#
##### fixed bugs #### 
# - table in detailed heatmaps show only references for plot B, not plot A ...(AE) ---- Task done
#
# - Update the color scheme to be the same as the paper ...(YM)  DONE
#
#
# - add plot titles to chem and health heatmap tabs... (YM) DONE
# 
# - add filter for specific heatlh outcome measures in plot b for the health outcome tab... (AE) Task done
#     - needs to auto-update to only show levels in the selected health outcome in plot A ... DONE
#
# - worldmap has number of paper instead of articles ...(YM) DONE
# 
# - remove cut off percentage from detailed heatmaps ... (AE)  Task done
#
# - never show decimals for year in the chemicals over time graph (e.g. if the year range is from 2000 to 2022, there are years like 2012.5) (YM) DONE
#



#
####  features to do #### 
#  
# - new tab where someone can construct their own heatmap
#
#
#### features implemented ####


########################################################

# Define UI ---- 

# inactivity <- "function idleTimer() {
# var t = setTimeout(logout, 120000);
# window.onmousemove = resetTimer; // catches mouse movements
# window.onmousedown = resetTimer; // catches mouse movements
# window.onclick = resetTimer;     // catches mouse clicks
# window.onscroll = resetTimer;    // catches scrolling
# window.onkeypress = resetTimer;  //catches keyboard actions
# 
# function logout() {
# window.close();  //close the window
# }
# 
# function resetTimer() {
# clearTimeout(t);
# t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
# }
# }
# idleTimer();"
# 
# 
# # data.frame with credentials info
# credentials <- data.frame(
#   user = c("Minderoo_Guest"),
#   password = c("#AhmedElagali"),
#   # comment = c("alsace", "auvergne", "bretagne"), %>% 
#   stringsAsFactors = FALSE
# )
# 
# ui <- secure_app(head_auth = tags$script(inactivity),
#                  fluidPage(style = "padding: 0px;", # no gap in navbar

# 
 ui <- fluidPage(style = "padding: 0px;", # no gap in navbar
  theme = shinythemes::shinytheme("flatly"),
  # actionButton("logout", "Help", icon = icon("help"),
  #              style = "position: absolute; top: 5px; right: 5px; z-index:10000;",
  #              onclick="window.location.href='https://www.minderoo.org/plastics-and-human-health/'"),
  # actionButton("logout", "FAQs?", icon = icon(""),
  #              style = "position: absolute; top: 5px; right: 80px; z-index:10000;",
  #              onclick="window.location.href='https://www.minderoo.org/plastics-and-human-health/'"),
  actionButton("logout", "Home Page", icon = icon("home"),
               style = "position: absolute; top: 5px; right: 5px; z-index:10000;",
               onclick="window.location.href='https://osf.io/fhw7d/'"),
  navbarPage("Plastic Heat Map",
    
## Overview tab ====         
    tabPanel("Overview",
             
    sidebarLayout(
        sidebarPanel(
                div(img(src='logo2.svg', 
                         width = "80%"),
                    style="text-align: center;"),
            sliderInput("range", "Select the year",
                     min = 1961, 
                     max = 2022,
                     sep = "", 
                     ticks = F,
                     value = c(1961, 2022)
                     ),
            shinyWidgets::pickerInput("classInput", "Select chemical class",
                        choices  = levels(factor(df$class, levels = chem_order)),
                        options  = list('actions-box' = TRUE),
                        multiple = TRUE,
                        selected = levels(factor(df$class, levels = chem_order))
                        ),
            shinyWidgets::pickerInput("healthInput", "Select ICD categories",
                                      choices  = levels(factor(df$level0)),
                                      options  = list('actions-box' = TRUE),
                                      multiple = TRUE,
                                      selected = levels(factor(df$level0))
                                      ),
            shinyWidgets::pickerInput("studyInput", "Select study designs",
                                      options  = list('actions-box' = TRUE),
                                      choices  = levels(as.factor(df$study_design)),
                                      multiple = TRUE,
                                      selected = levels(as.factor(df$study_design))
                                      ),
            shinyWidgets::pickerInput("populationInput", "Select population",
                                      choices  = levels(as.factor(df$population)),
                                      options  = list('actions-box' = TRUE),
                                      multiple = TRUE,
                                      selected = levels(as.factor(df$population))
                                      ),
            shinyWidgets::pickerInput("age_exposureInput", "Select age exposure group",
                                      choices  = levels(factor(df$age_exp_comb, levels = age_order)),
                                      options  = list('actions-box' = TRUE),
                                      multiple = TRUE,
                                      selected = levels(factor(df$age_exp_comb))
            ),
            shinyWidgets::pickerInput("age_healthInput", "Select age health group",
                                      choices  = levels(factor(df$age_health_comb, levels = age_order)),
                                      options  = list('actions-box' = TRUE),
                                      multiple = TRUE,
                                      selected = levels(factor(df$age_health_comb))
            ),
           shinyWidgets::pickerInput("riskInput", "Select special risks",
                                      choices  = levels(as.factor(df$risk_spec)),
                                      options  = list('actions-box' = TRUE),
                                      multiple = TRUE,
                                      selected = levels(as.factor(df$risk_spec))
                                      ),    
           shinyWidgets::pickerInput("country_study", "Select Country of study",
                                     choices   = levels(as.factor(df$country_investigated)),
                                     options   = list('actions-box' = TRUE),
                                     multiple  = TRUE,
                                     selected  = levels(as.factor(df$country_investigated))
           ), 
            width = 2
                         ),

        mainPanel(tabsetPanel(
          tabPanel("Dashboard",
                   fluidRow(column(6, plotOutput("plot3", height = "900px")),
                            column(6, 
                                      column(12, plotOutput("plot1", height = "500px")),
                                      column(12, plotOutput("plot2", height = "500px"))
                                      )
                               )
                   ),
          tabPanel("References",
                   fluidRow(br(),
                            column(12, 
                                   column(9, 
                                          h4(uiOutput("moreControls"))
                                   ),
                                   column(3, 
                                          downloadButton("download", "Download reference list as CSV")
                                   )
                            )
                   ),
                   fluidRow(column(12,
                                   tableOutput("table1")
                   )
                   )
          )
          )
    )
)
),



## health heatmaps tab ====
tabPanel("Heatmap Health",
         sidebarLayout(
           sidebarPanel(
             div(img(src='logo2.svg', 
                     width = "80%"),
                 style="text-align: center;"),
             sliderInput("range2", "Select the year",
                         min   = 1961, 
                         max   = 2022,
                         sep   = "", 
                         ticks = F,
                         value = c(1961, 2022)
             ),
             shinyWidgets::pickerInput("healthInput2", "Select ICD categories",
                                       # choices  = levels(factor(df$level0, levels = hom_order_in)),
                                       choices  = levels(factor(df$level0)),
                                       multiple = FALSE
             ),
             shinyWidgets::pickerInput("healthoutcomeInput2", "Select Health Outcome",
                                       options  = list('actions-box' = TRUE),
                                       choices  = levels(as.factor(df$group)),
                                       multiple = TRUE,
                                       selected = levels(as.factor(df$group))
             ),
             shinyWidgets::pickerInput("classInput2", "Select chemical class",
                                       choices  = levels(factor(df$class, levels = chem_order)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(factor(df$class, levels = chem_order))
             ),
             shinyWidgets::pickerInput("studyInput2", "Select study designs",
                                       options  = list('actions-box' = TRUE),
                                       choices  = levels(as.factor(df$study_design)),
                                       multiple = TRUE,
                                       selected = levels(as.factor(df$study_design))
             ),
            
             shinyWidgets::pickerInput("populationInput2", "Select population",
                                       choices  = levels(as.factor(df$population)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(as.factor(df$population))
             ),
             shinyWidgets::pickerInput("age_exposureInput2", "Select age exposure group",
                                       choices  = levels(factor(df$age_exp_comb, levels = age_order)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(factor(df$age_exp_comb))
             ),
             shinyWidgets::pickerInput("age_healthInput2", "Select age health group",
                                       choices  = levels(factor(df$age_health_comb, levels = age_order)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(factor(df$age_health_comb))
             ),
             shinyWidgets::pickerInput("riskInput2", "Select special risks",
                                       choices  = levels(as.factor(df$risk_spec)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(as.factor(df$risk_spec))
             ),
             
             width = 2
           ),
           
           
           
           mainPanel(
             tabsetPanel(
             tabPanel("Heatmap",
                      fluidRow(column(12, plotOutput("plot4", height = "1000px")))),
             tabPanel("References",
                      fluidRow(br(),
                               column(12, 
                                      column(9, 
                                             h4(uiOutput("moreControls1"))
                                      ),
                                      column(3, 
                                             downloadButton("download2", "Download reference list as CSV")
                                      )
                               )
                      ),
                      fluidRow(column(12,
                                      tableOutput("table2")
                                      )
                               )
                      )
             )
             )
           )
         ),


## Chemical heatmaps tab ====
tabPanel("Heatmap Chemicals",
         sidebarLayout(
           sidebarPanel(
             div(img(src='logo2.svg', 
                     width = "80%"),
                 style="text-align: center;"),
             sliderInput("range3", "Select the year",
                         min   = 1961, 
                         max   = 2022,
                         sep   = "", 
                         ticks = F,
                         value = c(1961, 2022)
             ),
             shinyWidgets::pickerInput("classInput3", "Select chemical class",
                                       choices  = levels(factor(df$class, levels = chem_order)),
                                       multiple = FALSE
             ),
             shinyWidgets::pickerInput("congInput3", "Select specific congener",
                                       choices  = "",
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = ""
             ),
             shinyWidgets::pickerInput("healthInput3", "Select ICD categories",
                                       choices  = levels(factor(df$level0)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(factor(df$level0))
             ),
             shinyWidgets::pickerInput("studyInput3", "Select study designs",
                                       options  = list('actions-box' = TRUE),
                                       choices  = levels(as.factor(df$study_design)),
                                       multiple = TRUE,
                                       selected = levels(as.factor(df$study_design))
             ),
             shinyWidgets::pickerInput("populationInput3", "Select population",
                                       choices  = levels(as.factor(df$population)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(as.factor(df$population))
             ),
             shinyWidgets::pickerInput("age_exposureInput3", "Select age exposure group",
                                       choices  = levels(factor(df$age_exp_comb, levels = age_order)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(factor(df$age_exp_comb))
             ),
             shinyWidgets::pickerInput("age_healthInput3", "Select age health group",
                                       choices  = levels(factor(df$age_health_comb, levels = age_order)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(factor(df$age_health_comb))
             ),
             shinyWidgets::pickerInput("riskInput3", "Select special risks",
                                       choices  = levels(as.factor(df$risk_spec)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(as.factor(df$risk_spec))
             ),
             sliderInput("range3b", "Change cut off to expand ‘other’",
                           min   = 0, 
                           max   = 25,
                           value = 5,
                           step  = 1,
                           sep   = "", 
                           ticks = F
             ),
             width = 2
           ),
           
           
           
           mainPanel(tabsetPanel(
             tabPanel("Heatmap",
                      fluidRow(column(12, plotOutput("plot5", height = "1000px"))), 
                      ),
             tabPanel("References",
                      fluidRow(br(),
                               column(12, 
                                      column(9, 
                                             h4(uiOutput("moreControls3"))
                                      ),
                                      column(3, 
                                             downloadButton("download3", "Download reference list as CSV")
                                      )
                               )
                      ),
                      fluidRow(column(12,
                                      tableOutput("table3")
                      )
                      )
             )
            )
           )
         )

),

## Database Panel Chemical =====
tabPanel('Chemicals Search',   
         sidebarLayout(
           sidebarPanel(
             div(img(src='logo2.svg', 
                     width = "80%"),
                 style="text-align: center;"),
             
             textInput("search_input",
                       label = "Search Chemical or CAS",
                       value = "",
                       placeholder = "e.g. Bisphenol A"),
             sliderInput("fuzziness",
                         label = "Search Fuzziness",
                         min = 0,
                         max = 1,
                         value = 0.01
             ),
              shinyWidgets::pickerInput("ChemClasss", "Filter SEM Chemical Class",
                                        choices  = levels(as.factor(df_additives$...27)),
                                        options  = list('actions-box' = TRUE),
                                        multiple = TRUE,
                                        selected = levels(as.factor(df_additives$...27))
                                       ),
             checkboxGroupInput("display_classes", "Show chemical subclasses",
                                choices = c("Classification Level 2" = "Compound group / classification",
                                            "Classification Level 3" = "Compound group / classification2",
                                            "Classification Level 4" = "Compound group / classification3",
                                            "Classification Level 5" = "Compound group / classification4",
                                            "Classification Level 6" = "Compound group / classification5"),
                                selected = c("Compound group / classification",
                                             "Compound group / classification2"
                                             ),
                                inline = F
                                ),
             #uiOutput("headline"),
             selectizeInput("cas_number", "Type CAS number to add to reference list",
                            choices  = all.cas.numbers,
                            options  = list(maxOptions = 10),
                            multiple = TRUE,
                            width = "100%"
             ),
             # shinyWidgets::pickerInput("cas_number2", "Filter by CAS to See References",
             #                                               choices  = sort(all.cas.numbers),
             #                                               options  = list('actions-box' = TRUE),
             #                                               multiple = TRUE,
             #                                               selected = NULL
             #                    ),
             # shinyWidgets::pickerInput("AdditiveClasss", "Additive Classifications",
             #                           choices = levels(as.factor(df_additives$CAS)),
             #                           options = list('actions-box' = TRUE),
             #                           multiple = TRUE,
             #                           selected = levels(as.factor(df_additives$CAS))
             # ),
             # shinyWidgets::pickerInput("PolymerClasss", "Polymer Classifications",
             #                           choices = levels(as.factor(df_polymers$CAS)),
             #                           options = list('actions-box' = TRUE),
             #                           multiple = TRUE,
             #                           selected = levels(as.factor(df_polymers$CAS))
             #),
             width = 2
           ),
           mainPanel(tabsetPanel(
             
             tabPanel("Additives", DT::dataTableOutput("ex11")
                      ),
             tabPanel("Polymers",DT::dataTableOutput("ex111")
                      ),
             tabPanel("Patch Test Series",DT::dataTableOutput("ex1111")
                      ),
             tabPanel("References",
                      fluidRow(br(),
                               column(12, 
                                      column(9, 
                                             h4(uiOutput("moreControls5"))
                                      ),
                                      column(3, 
                                             downloadButton("download44", "Download reference list as CSV")
                                      )
                               )
                      ),
                      fluidRow(column(12,
                                      tableOutput("table44")
                                      )
                               )
                      ),
                      tags$head(tags$script(src="fuzzy.js")
                                )
                      )
           )
         )),
## Database Panel Health  =====
tabPanel('Health Outcomes',                  
         sidebarLayout(
           sidebarPanel(
             div(img(src='logo2.svg', 
                     width = "80%"),
                 style="text-align: center;"),
             
             
             textInput("search_input1",
                       label       = "Search Health Outcome",
                       value       = "",
                       placeholder = "e.g. ADHD"),
             sliderInput("fuzziness1",
                         label     = "Search Fuzziness",
                         min       = 0,
                         max       = 1,
                         value     = 0.01
             ),
             shinyWidgets::pickerInput("healthoutcomel1", "Filter ICD Category",
                                       choices  = levels(as.factor(df_healthoutcomes$level0)),
                                       options  = list('actions-box' = TRUE),
                                       multiple = TRUE,
                                       selected = levels(as.factor(df_healthoutcomes$level0))
             ),
             checkboxGroupInput("display_columns", "Show ICD levels",
                                choices = c("ICD Level 1" = "level1",
                                            "ICD Level 2" = "level2",
                                            "ICD Level 3" = "level3"),
                                selected = c("level1", "level2", "level3"),
                                inline = F
             ),
             width = 2),
           mainPanel(fluidRow(column(12,h2("Health Outcomes"),DT::dataTableOutput("ex2"))),
                     tags$head(tags$script(src="fuzzy.js"))
           )
         )),
box(
  title  = "",
  width  = 12,
  height = 12,
  status = NULL,style = "position: absolute; right: 20px",
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


)
)

    


    





# Server logic ####
server <- function(input, output, session) {
  # result_auth <- secure_server(check_credentials = check_credentials(credentials))
  # 
  # output$res_auth <- renderPrint({
  #   reactiveValuesToList(result_auth)
  # })
  
 
  
  # observeEvent (input$ex11_rows_selected,{
  # 
  # add.sel <- Additives_df()[input$ex11_rows_selected,"CAS"]
  # # sel <- rbind(sel, Additives_df()[input$ex11_rows_selected,"CAS"])
  # # 
  # # add.sel <- sel %>% 
  # #   count() %>% 
  # #   filter(lapply(n, "%%", 2) == 0) %>% 
  # #   select(-n)
  # 
  # updateSelectizeInput(session  = session,
  #                      inputId  = "cas_number",
  #                      selected = add.sel,
  #                      options  = list(maxOptions = 10))
  # })


  
  # observeEvent (input$ex11_cell_clicked$row,{
  # 
  # add.sel <- Additives_df()[input$ex11_cell_clicked$row,"CAS"]
  # 
  # sel <- c(sel, add.sel)
  # 
  # updateSelectizeInput(session = session,
  #                      inputId = "cas_number",
  #                      selected = sel,
  #                      options = list(maxOptions = 10))
  # })

  observeEvent(input$classInput3, {
    tdf <- df[df$class == input$classInput3, ]
    
    shinyWidgets::updatePickerInput(session  = session,
                                    inputId  = "congInput3",
                                    choices  = levels(as.factor(tdf$chem_name_shiny)),
                                    selected = levels(as.factor(tdf$chem_name_shiny))
    )
    
  }, ignoreInit = F)   
  
  observeEvent(input$healthInput2, {
    t2df <- df[df$level0 == input$healthInput2, ]
    
    shinyWidgets::updatePickerInput(session  = session,
                                    inputId  = "healthoutcomeInput2",
                                    choices  = levels(as.factor(t2df$group)),
                                    selected = levels(as.factor(t2df$group))
    )
    
  }, ignoreInit = F)  
  
  get_working_data_frame <- reactive({
    df %>%
      filter(as.numeric(year) >= input$range[1],
             as.numeric(year) <= input$range[2],
             class %in% input$classInput,
             level0 %in% input$healthInput,
             risk_spec %in% input$riskInput,
             population %in% input$populationInput,
             age_exp_comb %in% input$age_exposureInput,
             age_health_comb %in% input$age_healthInput,
             country_investigated %in% input$country_study,
             study_design %in% input$studyInput)
    
  })
   
        
### Overview tab ####  
    #### PLOT 1 ####  
    output$plot1 <- renderPlot({
      
      wdf <- get_working_data_frame()
      validate(need(nrow(wdf)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
      
      chem_order1 <- factor(input$classInput, level = c("Bisphenols",
                                                        "PFAS",
                                                        "PBBs", 
                                                        "Phthalates",
                                                        "Other mixed use",
                                                        "Other flame retardants",
                                                        "Other plasticizers",
                                                        "OPEs",
                                                        "PBDEs",
                                                        "PCBs",
                                                        "Polymers"))
                                 

# here we add the total number of references in each chemical class, and create a label string for use in the plot later
label_order <- wdf %>%
            select(refid, class, year) %>%
            drop_na(class) %>%
            unique() %>%
            group_by(class) %>%
            count() %>%
            arrange(factor(class, levels = chem_order1)) %>%
            mutate(label = paste0(class, " (n = ", n, ")", collapse = "")) %>%
            group_by(label) %>%
            select(label) %>%
            unlist() %>%
            paste()
        
# the actual plot
        wdf %>%
            select(refid, 
                   year, 
                   class) %>%
            drop_na(class) %>%
            unique() %>%
            complete(class, 
                     year = input$range[1]:input$range[2], 
                     fill = list(`n()` = 0)) %>%
            filter(year  != "2022") %>%
            group_by(class, 
                     year) %>%
            summarise(n = sum(!is.na(refid))) %>%
           ggplot(aes(x = as.integer(year),
                      y = n)
                 ) +
            scale_x_continuous(breaks = integer_breaks()) +
            geom_area(aes(fill = factor(class, level = chem_order1)), 
                      position = "stack", 
                      alpha    = 0.7,
                      color    = "Black"
                      ) +
            theme_classic()+
            scale_fill_manual(labels = label_order,
                              values = min_pal_qual[c(2:11,1)]
                              ) +
            labs(title = "D. Published articles over time",
                 x     = "Year", 
                 y     = "Published articles per year", 
                 fill  = "Chemical classes\n(number of articles)") +
            theme_sr +
            theme(legend.position   = c(.25,.7),
                  legend.background = element_rect(fill   = alpha("white", 0.7),
                                                   colour = "black"),
                  axis.title        = element_text(size   = 12)
                  )
            
    })
    #### PLOT 2 #### 
    output$plot2 <- renderPlot({
      
wdf <- get_working_data_frame()
validate(need(nrow(wdf)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))

      wdf %>% 
        # separate_rows(country_investigated, sep = "_") %>%
        # mutate(ci_code = to_code(country_investigated)) %>%
        distinct(refid, ci_code)  %>%
        group_by(ci_code) %>%
        unique() %>%
        count() %>% 
          
        ggplot() + 
          geom_map(aes(map_id = ci_code, 
                       #alpha  = log10(n),
                       fill   = n),
                   map = world_map
                   ) + 
          geom_polygon(data   = world_map,
                       aes(long, 
                           lat, 
                           group = group), 
                       fill   = "transparent", 
                       alpha  = 0.1, 
                       colour = "black", 
                       size   = 0.2
                       ) + 
          theme_void() + 
          coord_map(xlim = c(-190, 190), ylim = c(-60, 90)) + 
          
        scale_fill_stepsn(colours = min.col.test2(5),
                          breaks  = c(5, 25, 50, 100, 250),
                          limits  = c(0 , 1500),
                          values  = scales::rescale(c(2.5, 12.5, 37.5, 75, 175, 875),
                                                   from = c(0, 1500)),
                          #labels = c("0-5", "5-25", "25-50", "50-100", "100-250"),
                          show.limits = T
        ) +
        
          #scale_alpha_continuous(range = c(0.3, 1)) + 
          
          scale_x_continuous(expand = expansion(0)) +
          scale_y_continuous(expand = expansion(0)) +
          
          guides( alpha = "none") +
          
          labs(fill = "Number of\narticles",
               title = "E. Geographic distribution of studied populations\n "
               ) +
          theme_sr + 
          theme(legend.position  = c(.15,.15),
                legend.direction ="horizontal",
                legend.title     = element_text(hjust = 1,vjust = 1,  size = 12), 
                legend.text      = element_text(hjust = 1, angle = 45),
                panel.background = element_rect(fill  = NA, 
                                                colour = "black", 
                                                size  = 1),
                axis.text.x      = element_text(color = "transparent"),
                axis.text.y      = element_text(color = "transparent"),
                axis.title.y     = element_text(color = "transparent"),
                axis.title.x     = element_text(color = "transparent")
            )
      
     
      
  
    })
    
    #### PLOT 3 #### 
    output$plot3 <- renderPlot({
      
      wdf <- get_working_data_frame()
      validate(need(nrow(wdf)!=0, "There are no matches in the dataset. Try removing or relaxing one or more filters."))
      
      hom_order <- wdf %>%
        select(refid,
               level0) %>%
        unique() %>%
        drop_na() %>%
        group_by(level0)%>%
        count(sort = T) %>%
        select(level0)%>%
        unlist() %>%
        paste()
      
      max.val.hom <- wdf %>%
        select(refid,
               level0) %>%
        group_by(level0) %>%
        unique() %>%
        drop_na() %>%
        count() %>%
        ungroup() %>%
        select(n) %>%
        max() %>%
        round_any(50, f = ceiling)
      
max.val.exp <- wdf %>%
  select(refid,
         class) %>%
  group_by(class) %>%
  unique() %>%
  drop_na() %>%
  count() %>%
  ungroup() %>%
  select(n) %>%
  max() %>%
  round_any(50, f = ceiling)


max.heat <- wdf %>%
  select(refid,
         class,
         level0,
         year) %>%
  unique() %>%
  drop_na() %>%
  group_by(class,
           level0)%>%
  count() %>%
  ungroup() %>%
  select(n) %>%
  max() %>%
  round_any(50, f = ceiling)
        
max.val <- max(c(max.val.hom, max.val.exp))
      
main_plot <- wdf %>%
        select(refid,
               class,
               level0,
               year) %>%
        unique() %>%
        drop_na() %>%
        group_by(class,
                 level0)%>%
        count() %>%
        ggplot(aes(x    = factor(class, level = chem_order),
                   y    = factor(str_wrap(level0,30), level = rev(str_wrap(hom_order,30))),
                   fill = n)
               ) +
        geom_tile(aes(alpha = log10(n)
                      )
                  ) + 
        geom_text(aes(label =  n),
                  size = theme.size
                  )+
        scale_fill_gradientn(colours = min.col.test(50),
                             #trans = "sqrt",
                             limits = c(0,max.heat))+
        
       #scale_alpha_continuous(range = c(0.3, 1)) +
        scale_y_discrete(position = "left")+
        guides( alpha = "none", fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
        labs(x = "",
             y = "",
             fill = "Number of\narticles") +
        theme_sr +
        theme(legend.title     = element_text(hjust = 0,vjust = 1), 
              legend.position  = "bottom",
              legend.direction = "horizontal",
              legend.text      = element_text(hjust = 1, angle = 45),
              panel.background = element_rect(fill = NA, 
                                              colour = "black", 
                                              size = 1),
              axis.text.x      = element_text(hjust = 1,vjust = 0.5, angle = 90)
        )
      
bar_exp <- wdf %>%
        select(refid,
               class) %>%
        unique() %>%
        drop_na() %>%
        group_by(class)%>%
        count() %>% 
        ggplot(aes(x = factor(class, level = chem_order),
                   y = n)
               ) +
        geom_col(fill = min_pal_qual[1],
                 alpha = .5
                 ) + 
        geom_text(aes(label =  n),
                  vjust = 2,
                  size  = theme.size
                  )+
        scale_y_reverse(limits = c((1.3*max.val), 0)
                        )+
        scale_x_discrete(position = "top") +
        guides( alpha = "none", fill ="none") +
        labs(x = "",
             y = "Number of\narticles")+
        theme_sr +
        theme(axis.text.x = element_blank(),
              rect = element_rect(fill = "transparent")
        )
      
bar_hom <- wdf %>%
        select(refid,
               level0) %>%
        unique() %>%
        drop_na() %>%
        group_by(level0)%>%
        count() %>% 
        ggplot(aes(x = n,
                   y = factor(str_wrap(level0,30), level = rev(str_wrap(hom_order,30)))
                   )
               )+
        geom_col(fill  = min_pal_qual[1],
                 alpha = .5
                 ) + 
        geom_text(aes(label =  n),
                  hjust = -0.25,
                  size  = theme.size
                  )+
        scale_x_continuous(limits   = c(0, (1.3*max.val)),
                           position = "bottom"
                           ) + 
        
        guides( alpha = "none", 
                fill  = "none"
                ) +
        labs(x = "Number of\narticles",
             y = ""
             ) +
  
        theme_sr +
        theme(axis.text.y = element_blank(),
              axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
              )
      
      
Hom_exp_map <- (main_plot +  ggtitle("A. Articles combining chemical class exposures\nand health outcomes") + title_theme) +
  (bar_hom +  ggtitle("B. Articles including\nlisted health outcomes") + title_theme  + 
     theme(axis.title.x = element_text(margin = margin(t = -150, unit = "pt"))))+
  plot_spacer() + plot_spacer() +
  (bar_exp + ggtitle("C. Articles including listed\nchemical class exposure") + title_theme +
     theme(axis.title.y = element_text(margin = margin(r = -200, unit = "pt")),
           plot.title = element_text(vjust = -15))) + 
  guide_area()+
  plot_layout(ncol = 2, nrow = 3, 
              widths = c(2, 1),
              heights = c(4,-.5, 1),
              guides = 'collect') 

print(Hom_exp_map)
        
    })
    
    #### table 1 ####
  
      
      output$table1 <- renderTable({
     
     wdf <- df %>%
       filter(as.numeric(year) >= input$range[1],
              as.numeric(year) <= input$range[2],
              class %in% input$classInput,
              level0 %in% input$healthInput,
              risk_spec %in% input$riskInput,
              age_exp_comb %in% input$age_exposureInput,
              age_health_comb %in% input$age_healthInput,
              population %in% input$populationInput,
              country_investigated %in% input$country_study,
              study_design %in% input$studyInput)
     
     wdf %>%
       select(refid,
              citation) %>%
       mutate(refid = format(round(refid))) %>%
       unique()
     
   }
)
 
   datas1<- reactive({df <- df %>%
                       filter(as.numeric(year) >= input$range[1],
                              as.numeric(year) <= input$range[2],
                              class %in% input$classInput,
                              level0 %in% input$healthInput,
                              risk_spec %in% input$riskInput,
                              population %in% input$populationInput,
                              age_exp_comb %in% input$age_exposureInput,
                              age_health_comb %in% input$age_healthInput,
                              country_investigated %in% input$country_study,
                              study_design %in% input$studyInput)
                     
                     wdf<- df  %>% select(refid,
                              citation) %>%
                       mutate(refid = format(round(refid))) %>%
                       unique()
                    return(wdf)
                     })
  
  output$download <-
    downloadHandler(
      filename = function () {
        paste("MyData.csv", sep = "")
      },
      
      content = function(file) {
        write.csv(datas1(), file)
      }
    )
  
  
  output$moreControls <- renderUI({
    #outputArgs=paste0("Total number of articles found =",dim(datas1())[1])
    outputArgs =  str_glue("Number of articles found within current filter parameters = {dim(datas1())[1]}")
  })
    
### Heatmap Health  tab #### 
   
    #### PLOT 4 ####         
  
  #return(wdf)

    
    output$plot4 <- renderPlot({
     
      wdf <- df %>%
        filter(as.numeric(year) >= input$range2[1],
               as.numeric(year) <= input$range2[2],
               class %in% input$classInput2,
               level0 %in% input$healthInput2,
               risk_spec %in% input$riskInput2,
               population %in% input$populationInput2,
               age_exp_comb %in% input$age_exposureInput2,
               age_health_comb %in% input$age_healthInput2,
               study_design %in% input$studyInput2)
      
      validate(need(nrow(wdf %>% filter(group %in% input$healthoutcomeInput2))!=0, "There are no studies that match the selection. Try removing or relaxing one or more filters."))
      
      max.val <- wdf %>%
        select(refid,
               class,
               group) %>%
        filter(group %in% input$healthoutcomeInput2) %>% 
        select(-group) %>% 
        group_by(class) %>%
        unique() %>%
        drop_na() %>%
        count() %>%
        ungroup() %>%
        select(n) %>%
        max() %>%
        round_any(10, f = ceiling)
      
names_df <- wdf %>% 
        select(refid, 
               group
        ) %>%
        unique() %>%
        drop_na() %>%
        group_by(group) %>%
        summarise(n = n()) %>%
        mutate(percent = n / sum(n) * 100) %>%
        group_by(group) %>%
        unique()
      
wwdf <- wdf %>%
        select(refid,
               group) %>%
        mutate(group = case_when(group %!in% names_df$group ~ "Other",
                                           TRUE ~ as.character(group))) %>%
        unique() %>%
        group_by(group) %>%
        summarise(n = n())
      


hom.incl <- wwdf %>%
        arrange(desc(n)) %>%
        filter(group != "Other") %>%
        select(group) %>%
        unlist() %>%
        unique() %>%
        paste()
      
hom.order <- rev(c(hom.incl, "Other"))
      
hom.label <- wwdf %>%
        mutate(label = str_glue("{group} (n = {n})")) %>%
        pull(label, name = group)

icd.label <- wdf %>% 
  select(refid,
         group,
         level0) %>% 
  filter(group %in% input$healthoutcomeInput2) %>% 
  select(-group) %>% 
  group_by(level0) %>% 
  unique() %>% 
  count() %>% 
  mutate(label = str_glue("{level0} (n = {n})")) %>%
  pull(label, name = level0)

      
plotA <- wdf %>%
        select(refid,
               class,
               group,
               level0) %>%
        filter(group %in% input$healthoutcomeInput2) %>% 
        select(-group) %>% 
        unique() %>%
        drop_na() %>%
        group_by(class,
                 level0)%>%
        count() %>%
        ggplot(aes(x = factor(class, level = chem_order),
                   y = level0)
               ) +
        geom_tile(aes(x     = factor(class, level = chem_order),
                      y     = level0,
                      fill  = n,
                      alpha = log10(n)
                      )
                  ) + 
        geom_text(aes(label = n),
                      size  = theme.size
                        ) +

        scale_fill_gradientn(colours = min.col.test(50),
                             limits  = c(0,max.val))+
        scale_alpha_continuous(range = c(0.3, 1)) +
        scale_y_discrete(position    = "right",
                         label       = icd.label)+
        guides(alpha = "none") +
        labs(x       = "",
             y       = "",
             fill    = "Number of\narticles")+
        theme_sr +
        theme(legend.title = element_text(hjust = 1,vjust = 1,  size = 12), 
              legend.position = "left",
              panel.background = element_rect(fill = NA, 
                                              colour = "black", 
                                              size = 1),
              axis.text.x = element_blank()
              )
      
      
      plotB <- wdf %>%
        select(refid,
               class,
               level0, 
               group) %>%
        filter(group %in% input$healthoutcomeInput2) %>% 
        unique() %>%
        drop_na() %>%
        mutate(group = case_when(group %!in% wwdf$group ~ "Other",
                                 TRUE ~ as.character(group))) %>%
        group_by(class,
                 group) %>%
        unique() %>%
        count() %>%
        ggplot(aes(x = factor(class, level = chem_order),
                   y = reorder(group, n))) +
        geom_tile(aes(x     = factor(class, level = chem_order),
                      y     = factor(group, level = hom.order),
                      fill  = n,
                      alpha = log10(n)
                      )
                  ) + 
        geom_text(aes(label = n),
                      size  = theme.size
                        ) +
        scale_fill_gradientn(colours = min.col.test(50),
                             limits  = c(0 , max.val))+ 
        scale_alpha_continuous(range = c(0.3, 1)) +
        scale_y_discrete(position = "right",
                         label    = hom.label)+
        scale_x_discrete(drop = T) +
        guides( alpha = "none") +
        labs(x    = "",
             y    = "",
             fill = "Number of\narticles")  +
        theme_sr +
        theme(legend.title     = element_text(hjust = 1,vjust = 1,  size = 12), 
              legend.position  = "left",
              panel.background = element_rect(fill   = NA, 
                                              colour = "black", 
                                              size   = 1),
              axis.text.x      = element_text(hjust  = 1,
                                              vjust  = 1,  
                                              size   = 12, 
                                              angle  = 45)
              )
      
      
      
      plot4 <- (plotA + ggtitle(str_glue("A. Articles identified on {wdf$level0}")) + title_theme) +
               (plotB + ggtitle(str_glue("B. Sub-categories within {wdf$level0} for which articles were identified")) + title_theme) +
                    plot_layout(ncol = 1, nrow = 2, 
                    heights = c(1, 5),
                    guides = 'collect') & theme(legend.position = 'left') 
      
      print(plot4) 
      
    })
    
    
    #### table 2 ####
    output$table2 <- renderTable({
      
      wdf <- df %>%
        filter(as.numeric(year) >= input$range2[1],
               as.numeric(year) <= input$range2[2],
               class %in% input$classInput2,
               level0 %in% input$healthInput2,
               risk_spec %in% input$riskInput2,
               population %in% input$populationInput2,
               study_design %in% input$studyInput2,
               age_exp_comb %in% input$age_exposureInput2,
               age_health_comb %in% input$age_healthInput2,
               group %in% input$healthoutcomeInput2)
      
      wdf %>%
        select(refid,
               citation) %>%
        mutate(refid = format(round(refid))) %>%
        unique()
      
    })
    
  datas2<- reactive({
    
    df <- df %>%
      filter(as.numeric(year) >= input$range2[1],
             as.numeric(year) <= input$range2[2],
             class %in% input$classInput2,
             level0 %in% input$healthInput2,
             risk_spec %in% input$riskInput2,
             population %in% input$populationInput2,
             study_design %in% input$studyInput2,
             age_exp_comb %in% input$age_exposureInput2,
             age_health_comb %in% input$age_healthInput2,
             group %in% input$healthoutcomeInput2)
    
    wdf <- df  %>%
      select(refid,
             citation) %>%
      mutate(refid = format(round(refid))) %>%
      unique()
  
  return(wdf)
  })
  
  output$download2 <-
    downloadHandler(
      filename = function () {
        paste("MyData.csv", sep = "")
      },
      
      content = function(file) {
        write.csv(datas2(), file)
      }
    )
  
  output$moreControls1 <- renderUI({
    #outputArgs=paste0("Total number of articles found =",dim(datas2())[1])
    outputArgs =  str_glue("Number of articles found within current filter parameters = {dim(datas2())[1]}")
  })
    
### Heatmap Chem  tab ####
    
    #### PLOT 5 ####    
    
output$plot5 <- renderPlot({
  
  # observeEvent(input$classInput3, {
  #   shinyWidgets::updatePickerInput(session = session,
  #                                   inputId = "congInput3",
  #                                   choices = levels(as.factor(wdf$chem_name_shiny)),
  #                                   selected = levels(as.factor(wdf$chem_name_shiny))
  #   )
  #   
  # }, ignoreInit = F) 
  # 
  #     
wdf <- df %>%
        filter(as.numeric(year) >= input$range3[1],
               as.numeric(year) <= input$range3[2],
               class %in% input$classInput3,
               level0 %in% input$healthInput3,
               risk_spec %in% input$riskInput3,
               population %in% input$populationInput3,
               age_exp_comb %in% input$age_exposureInput3,
               age_health_comb %in% input$age_healthInput3,
               study_design %in% input$studyInput3)


 # wdf <- wdf2 %>%
 #   filter(chem_name_shiny %in% input$congInput3)
      
validate(need(nrow(wdf %>% filter(chem_name_shiny %in% input$congInput3))!=0, "There are no studies that match the selection. Try removing or relaxing one or more filters."))

      
max.val <- wdf %>%
  select(refid,
         chem_name_shiny,
         level0) %>%
  filter(chem_name_shiny %in% input$congInput3) %>%
  select(-chem_name_shiny) %>%
  group_by(level0) %>%
  unique() %>%
  drop_na() %>%
  count() %>%
  ungroup() %>%
  select(n) %>%
  max() %>%
  round_any(10, f = ceiling)
      
      
names_df <- wdf %>%
        select(refid,
               chem_name_shiny) %>%
        unique() %>%
        drop_na() %>%
        group_by(chem_name_shiny) %>%
        summarise(n = n()) %>%
        mutate(percent = n / sum(n) * 100) %>%
        mutate(chem_name_shiny = case_when(percent < input$range3b ~ "Other",
                                           TRUE ~ as.character(chem_name_shiny))) %>%
        group_by(chem_name_shiny) %>%
        unique()
      
wwdf <- wdf %>%
        select(refid,
               chem_name_shiny) %>%
        mutate(chem_name_shiny = case_when(chem_name_shiny %!in% names_df$chem_name_shiny ~ "Other",
                                           TRUE ~ as.character(chem_name_shiny))) %>%
        unique() %>%
        group_by(chem_name_shiny) %>%
        summarise(n = n())
        
chem.incl <- wwdf %>%
        arrange(desc(n)) %>%
        filter(chem_name_shiny != "Other") %>%
        select(chem_name_shiny) %>%
        unlist() %>%
        unique() %>%
        paste()


###%%%%%%%%%%%%%



###%%%%%%%%%%%%%


con.order <- c(chem.incl, "Other")
      
con.label <- wwdf %>%
        mutate(label = str_glue("{chem_name_shiny} (n = {n})")) %>%
        pull(label, name = chem_name_shiny)
      
cl.label <- wdf %>% 
  select(refid,
         chem_name_shiny,
         class) %>%
  filter(chem_name_shiny %in% input$congInput3) %>%
  select(-chem_name_shiny) %>%
  group_by(class) %>% 
  unique() %>% 
  count() %>% 
  mutate(label = str_glue("{class} (n = {n})")) %>%
  pull(label, name = class)

hom.order <- rev(wdf %>%
          select(refid,
                 class,
                 level0) %>%
          filter(class == input$classInput3) %>%
          unique() %>%
          drop_na() %>%
          group_by(level0) %>%
          count(sort = T) %>%
          select(level0) %>%
          unlist() %>%
          paste()
      )
      
      
plotA <- wdf %>%
        select(refid,
               class,
               chem_name_shiny,
               level0) %>%
        filter(chem_name_shiny %in% input$congInput3) %>%
        filter(class == input$classInput3) %>%
        select(-chem_name_shiny) %>% 
        unique() %>%
        drop_na() %>%
        group_by(level0,
                 class)%>%
        count() %>%
        ggplot(aes(x = factor(class, level = chem_order),
                   y = factor(str_wrap(level0, 30), level = str_wrap(hom.order, 30))
                   )
               )+
        geom_tile(aes(fill = n,
                      alpha = log10(n)
                      )
                  ) + 
        geom_text(aes(label = n),
                      size  = theme.size
                        ) +
        scale_fill_gradientn(colours = min.col.test(50),
                             limits  = c(0,(max.val+10)))+
        scale_alpha_continuous(range = c(0.3, 1)) +
        scale_y_discrete(position = "left") +
        scale_x_discrete(label    = cl.label) +
        guides( alpha = "none") +
        labs(x = "",
             y = "",
             fill = "Number of\narticles")+
        theme_sr +
        theme(legend.title     = element_text(hjust = 1, vjust = 1,  size = 12), 
              legend.position  = "left",
              panel.background = element_rect(fill = NA, 
                                              colour = "black", 
                                              size = 1),
              axis.text.x      = element_text(hjust = 1, angle = 45),
              axis.text.y      = element_text(size = 11)
        )
      
      
plotB <- wdf %>%
        select(refid,
               class,
               level0, 
               chem_name_shiny) %>%
        filter(class == input$classInput3) %>%
        filter(chem_name_shiny %in% input$congInput3) %>%
        unique() %>%
        mutate(chem_name_shiny = case_when(chem_name_shiny %!in% wwdf$chem_name_shiny ~ "Other",
                                           TRUE ~ as.character(chem_name_shiny))) %>%
        group_by(level0,
                 chem_name_shiny) %>%
        unique() %>%
        count() %>%
        
        ggplot(aes(x = factor(chem_name_shiny, level = con.order),
                   y = factor(level0, level = hom.order)
                   )
               ) +
        geom_tile(aes(fill = n,
                      alpha = log10(n)
                      )
                  ) + 
        geom_text(aes(label = n),
                        size = theme.size
                        ) +
        scale_fill_gradientn(colours = min.col.test(50),
                             limits  = c(0,(max.val+10)))+ 
        scale_alpha_continuous(range = c(0.3, 1)) +
        scale_x_discrete(label       = con.label) +
        scale_y_discrete(drop        = T) +
        guides( alpha = "none") +
        labs(x = "",
             y = "",
             fill = "Number of\narticles")  +
        theme_sr +
        theme(legend.title     = element_text(hjust = 1,vjust = 1,  size = 12), 
              legend.position  = "left",
              panel.background = element_rect(fill   = NA, 
                                              colour = "black", 
                                              size   = 1),
              axis.text.x      = element_text(hjust  = 1,
                                              vjust  = 1,  
                                              size   = 12, 
                                              angle  = 45),
              axis.text.y      = element_blank()
              )
      
      
      
plot5 <- (plotA + ggtitle(str_glue("A. Articles identified on {wdf$class}")) + title_theme) +
         (plotB + ggtitle(str_glue("B. Articles identified on individual chemicals within {wdf$class}")) + title_theme) +
              plot_layout(ncol = 2, nrow = 1, 
              widths = c(1, 5),
              guides = 'collect') & theme(legend.position = 'right') 
               #&
               # plot_annotation(tag_levels = "A",
               #          title = paste(input$classInput3)
               #          )

print(plot5) 
      
    })
    
    
    #### table 3 ####
    output$table3 <- renderTable({
      
      wdf <- df %>%
        filter(as.numeric(year) >= input$range3[1],
               as.numeric(year) <= input$range3[2],
               class %in% input$classInput3,
               level0 %in% input$healthInput3,
               risk_spec %in% input$riskInput3,
               population %in% input$populationInput3,
               study_design %in% input$studyInput3,
               age_exp_comb %in% input$age_exposureInput3,
               age_health_comb %in% input$age_healthInput3,
               chem_name_shiny %in% input$congInput3)
      
      wdf %>%
        select(refid,
               citation) %>%
        mutate(refid = format(round(refid))) %>%
        unique()
      
    })
    
  
  datas3<- reactive({
    
    df <- df %>%
      filter(as.numeric(year) >= input$range3[1],
             as.numeric(year) <= input$range3[2],
             class == input$classInput3,
             level0 %in% input$healthInput3,
             risk_spec %in% input$riskInput3,
             population %in% input$populationInput3,
             study_design %in% input$studyInput3,
             age_exp_comb %in% input$age_exposureInput3,
             age_health_comb %in% input$age_healthInput3,
             chem_name_shiny %in% input$congInput3)
    
    wdf <- df  %>%
      select(refid,
             citation) %>%
      mutate(refid = format(round(refid))) %>%
      unique()
    
    return(wdf)
  })
  
  output$download3 <-
    downloadHandler(
      filename = function () {
        paste("MyData.csv", sep = "")
      },
      
      content = function(file) {
        write.csv(datas3(), file)
      }
    )
  
  output$moreControls3 <- renderUI({
    #outputArgs=paste0("Total number of articles found =",dim(datas3())[1])
    outputArgs =  str_glue("Number of articles found within current filter parameters = {dim(datas3())[1]}")
  })

#Database Tables####
  
  ### Additives Table Chemical####
  
  
  
  Additives_df <- reactive({
    
    
    selected_chems <- set_names(c("Compound group / classification",
                                  "Compound group / classification2",
                                  "Compound group / classification3",
                                  "Compound group / classification4",
                                  "Compound group / classification5"), 
                                c("Classification Level 2",
                                  "Classification Level 3",
                                  "Classification Level 4",
                                  "Classification Level 5",
                                  "Classification Level 6")
                                )
    
    selected_cols <- selected_chems[selected_chems %in% c(input$display_classes)]
    
    head(df_additives <- df_additives %>%
           select("IUPAC Name" = IUPAC, 
                  "Extracted Name" = "Name in Shiny App",
                  Synonyms, 
                  CAS, 
                  Metabolites, 
                  "SEM Chemical Class" = "...27",
                  selected_cols) %>% 
           filter(`SEM Chemical Class` %in% input$ChemClasss))
    
    if (input$search_input == "") {
      return(df_additives)
    }
    
    else{
      searched_df <- search_dataframe(
      data = df_additives,
      cols = colnames(df_additives),
      search_terms = input$search_input,
      max.distance = input$fuzziness,
      match_all = TRUE
      )
      return(searched_df)}
      
  })
  
  
  
  
  output$ex11 <- DT::renderDataTable(
    DT::datatable(
      Additives_df(), plugins = c('natural'), extensions = 'Buttons', rownames  =FALSE, filter = 'top', 
      selection = list(target = 'cell'),
      #selection = "multiple",
      class = 'cell-border stripe', 
      options = list(searchHighlight = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE),
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 200, initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
          "}"),dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          )
        
      
    )
  )
  
  output$headline <- renderUI({
    
    h4(paste0(input$ex11_rows_selected))
    
  })
  
  Polymers_df <- reactive({
    head(df_polymers <- df_polymers %>%
           select("Name Extracted" = name,
                  CAS, 
                  "Abbreviation" = 'abb',
                  "Synonyms" = synonyms, 
                  "Tags" = tags)
    )
    if (input$search_input == "") {
      return(df_polymers)
    }
    else{
      searched_df1 <- search_dataframe(
        data = df_polymers,
        cols = colnames(df_polymers),
        search_terms = input$search_input,
        max.distance = input$fuzziness,
        match_all = TRUE
      )
      return(searched_df1)}   
  })
  
  
  
  output$ex111 <- DT::renderDataTable(
    DT::datatable(
      Polymers_df(),plugins = c('natural'), extensions = 'Buttons', rownames  =FALSE, filter = 'top', selection = list(target = 'cell'),class = 'cell-border stripe', 
      options = list( searchHighlight = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE),
                      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                      pageLength = 200, initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}"),dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  )
  
  
  
  Patch_df <- reactive({
    head(df_patch_test <- df_patch_test$data %>%
           select('Patch Test Series' = patch_test_series, 
                  'Classification'    = classification, 
                  'Product Number'    = product_number, 
                  'Included Substance'= included_substance, 
                  'Name in Shiny'     = name_in_shiny)
    )
    if (input$search_input == "") {
      return(df_patch_test)
    }
    else{
      searched_df2 <- search_dataframe(
        data = df_patch_test,
        cols = colnames(df_patch_test),
        search_terms = input$search_input,
        max.distance = input$fuzziness,
        match_all = TRUE
      )
      return(searched_df2)}  
  })
  
  
  
  output$ex1111 <- DT::renderDataTable(
    DT::datatable(
      Patch_df(),plugins = c('natural'), extensions = 'Buttons', rownames  =FALSE, filter = 'top', selection = list(target = 'cell'),class = 'cell-border stripe', 
      options = list(searchHighlight = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE),
                     lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                     pageLength = 200, initComplete = JS(
                       "function(settings, json) {",
                       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                       "}"),dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  )
  
  
  
  
  
  
  #### table 2 ####
  output$table44 <- renderTable({
    
    wdf <- df %>%
      filter(cas %in% input$cas_number)
    
    wdf %>%
      select(refid,
             citation) %>%
      mutate(refid = format(round(refid))) %>%
      unique()
    
  })
  
  datas44<- reactive({
    
    df <- df %>%
      filter(cas %in% input$cas_number)
    
    wdf <- df  %>%
      select(refid,
             citation) %>%
      mutate(refid = format(round(refid))) %>%
      unique()
    
    return(wdf)
  })
  
  output$download44 <-
    downloadHandler(
      filename = function () {
        paste("MyData.csv", sep = "")
      },
      
      content = function(file) {
        write.csv(datas44(), file)
      }
    )
  
  output$moreControls5 <- renderUI({
    #outputArgs=paste0("Total number of articles found =",dim(datas44())[1])
    outputArgs =  str_glue("Number of articles found within current filter parameters = {dim(datas44())[1]}")
  })
  
  
  
  ### database Health ####
  
  Houtcomes_df <- reactive({
    
    selected_names <- set_names(c("level1", "level2", "level3"), 
                                c("ICD level 1", "ICD level 2", "ICD level 3"))
    
    selected_columns <- selected_names[selected_names %in% c(input$display_columns)]
    
    head(df_healthoutcomes <- df_healthoutcomes %>%
           filter(level0 %in% input$healthoutcomel1)  %>%
           select("ICD Category"         = level0 ,
                   selected_columns,
                  "Search terms"         = search_terms,
                  "Extracted  Outcome"   = display,
                  "Health Outcome Group" = Group,)
    )
    
    if (input$search_input1 == "") {
      return(df_healthoutcomes)
    }
    
    else{
      searched_df3 <- search_dataframe(
        data = df_healthoutcomes,
        cols = colnames(df_healthoutcomes),
        search_terms = input$search_input1,
        max.distance = input$fuzziness1,
        match_all = TRUE
      )
      return(searched_df3)} 
  })
  
  
  output$ex2 <- DT::renderDataTable(
    DT::datatable(
      Houtcomes_df(),plugins = c('natural'),extensions = 'Buttons', rownames  =FALSE, filter = 'top', selection = list(target = 'cell'),class = 'cell-border stripe', 
      options = list( searchHighlight = TRUE, search = list(regex = TRUE, caseInsensitive = TRUE),
                      lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                      pageLength = 200, initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                        "}"),dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      )
    )
  )
  
  
  ##########################   End of Database parts
    

}




# Run the application 
shinyApp(ui = ui, server = server)
