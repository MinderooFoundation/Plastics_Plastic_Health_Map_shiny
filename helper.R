
  if (!require('maps')) install.packages('maps'); library('maps')
  if (!require('mapproj')) install.packages('mapproj'); library('mapproj')
  if (!require('shiny')) install.packages('shiny'); library('shiny')
  if (!require('here')) install.packages('here'); library('here')
  if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
  if (!require('patchwork')) install.packages('patchwork'); library('patchwork')
  if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
  if (!require('shinydashboardPlus')) install.packages('shinydashboardPlus'); library('shinydashboardPlus')
  if (!require('DT')) install.packages('DT'); library('DT')
  if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
  if (!require('shinydisconnect')) install.packages('shinydisconnect'); library('shinydisconnect')
  
  
  df_additives <- readxl::read_excel(here("additives.xlsx"))
  df_polymers <- readRDS(here("polymers.rds"))
  df_healthoutcomes <- readxl::read_xlsx(here("290922_Health_classes.xlsx"))
  df_patch_test <- readRDS(here("patch_test.rds"))
  
  
  # df <- read_rds(here("shiny_sr_figures","data", "shiny_df_condensed.rds")) 
  df <- readxl::read_excel(here("shiny_df_condensed.xlsx"))
  
  to_code <- function(x) {
    countrycode::countrycode(x, origin = "country.name", destination = "iso2c")
  }
  
  world_map <- map_data("world")
  world_map$region <- to_code(world_map$region)
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
  
  hom_order_in <- df %>%
    select(refid,
           level0) %>%
    unique() %>%
    drop_na() %>%
    group_by(level0)%>%
    count(sort = T) %>%
    select(level0)%>%
    unlist() %>%
    paste()
  
  chem_order <- c("PCBs",
                  "Phthalates",
                  "Bisphenols",
                  "PFAS",
                  "PBDEs",
                  "PBBs", 
                  "OPEs",
                  "Polymers",
                  "Other")
  

  
  geom.text.size = 12
  theme.size = 0.36 * geom.text.size
  
  min.col.test <- colorRampPalette(c("#FFCD00","#FF4F00"))
  min.col.test2 <- colorRampPalette(c("#fef5b3","#FFCD00","#FF8E00", "#FF4F00"))
  
  
  min_pal_qual <- rev(c(
                       "#a51930",
                       "#FF4F00",
                       "#FFCD00",
                       "#93af3e",
                       "#4d9d9d",
                       "#21453E",
                       "#95b5d1",
                       "#4f83b3",
                       "#511a89",
                       "#000000"
                     ))
  
  theme_set(theme_classic())
  
  title_theme<- theme(plot.title = element_text(hjust = 0, size = 12), 
                      plot.title.position = "plot")
  
  theme_sr<- theme(legend.background = element_blank(),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 12),
                   axis.text.x = element_text(size = 12),
                   axis.text.y = element_text(size = 12)
  )
  
  
