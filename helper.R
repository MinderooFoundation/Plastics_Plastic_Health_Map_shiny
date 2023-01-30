###Libraries####

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
  if (!require('shinymanager')) install.packages('shinymanager'); library('shinymanager')
  
  
###Functions####  

  to_code <- function(x) {
    countrycode::countrycode(x, origin = "country.name", destination = "iso2c")
  }
  
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
  
###Dataframes####  
  
  df <- readxl::read_excel(here("shiny_df_condensed_cas.xlsx"))
  
  df_additives      <- readxl::read_excel(here("additives.xlsx")) 
  df_polymers       <- readxl::read_excel(here("polymers.xlsx"))
  df_healthoutcomes <- readxl::read_xlsx(here("290922_Health_classes.xlsx"))
  df_patch_test     <- readRDS(here("patch_test.rds"))
  
  # df_additives$cas_n=cbind(df_additives$CAS, df_polymers$CAS)
 
  df_additives <- df_additives %>% 
    mutate(cas_names = ifelse(!is.na(CAS),
                              str_glue("{CAS} [{`Name in Shiny App`}]"),
                              CAS)
           )
  
  df_polymers <- df_polymers %>% 
    mutate(cas_names = ifelse(!is.na(CAS),
                              str_glue("{CAS} [{name}]"),
                              CAS)
           )
  
  df <- df %>% 
    mutate(cas_names = ifelse(!is.na(cas),
                              str_glue("{cas} [{chem_name_shiny}]"),
                              cas)
           )
  
  world_map <- map_data("world")
  world_map$region <- to_code(world_map$region)

###Values#### 
sel <- NULL
  
  all.cas.numbers = c(na.omit(df_additives$CAS), na.omit(df_polymers$CAS))
  all.cas.names   = c(na.omit(df_additives$cas_names), na.omit(df_polymers$cas_names))
  

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
  
age_order <- c("Prenatal (<0)",
               "Neonate (0-1 mnth)",
               "Infant (1-12 mnth)", 
               'Child (1-10 yrs)',
               'Adolescent (10-18 yrs)',
               'Unspecified pre-adult (<18 yrs)',
               'Adult', 
               'Older adult/Elderly',
               'Unspecified')
  
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
  
  
  
########################### Search function####
  
  
  #' Search a vector with a seach_term.
  #'
  #' @description Searches a given vector with a given search term. If
  #'   `max.distance > 0` then tolerates some mismatches to the given
  #'   `search_term`. Ignores capitalisation.
  #'   
  #' @param vector vector of strings to be searched.
  #' @param search_term String to be matched against vector.
  #' @param max.distance Maximum 'distance' between strings that result in a match
  #'   (values between 0 and 1).
  #'
  #' @return Logical vector of matches.
  #' @export
  #' @examples 
  #' # search vector with a term
  #' search_single_term(
  #'   vector = c("some", "terms", "in", "this", "vector"), 
  #'   search_term = "terms", 
  #'   max.distance = 0
  #' )
  #' 
  #' # fail to match because of spelling error
  #' search_single_term(
  #'   vector = c("some", "terms", "in", "this", "vector"), 
  #'   search_term = "teems", 
  #'   max.distance = 0
  #' )
  #' 
  #' # match the slightly different term, using max.distance > 0
  #' search_single_term(
  #'   vector = c("some", "terms", "in", "this", "vector"), 
  #'   search_term = "teems", 
  #'   max.distance = 0.1
  #' )
  
  
  search_single_term <- function(vector, search_term, max.distance) {
    
    if (max.distance == 0) {
      # should search without worrying about case but also ignoring regex symbols
      stringr::str_detect(
        string = stringr::str_replace_na(vector, ""), 
        pattern = stringr::fixed(search_term, 
                                 ignore_case = TRUE)
      )
      
    } else {
      agrepl(
        pattern = search_term, 
        x = vector, 
        max.distance = max.distance, 
        ignore.case = TRUE, 
        # do not use regex, just doing literal string matching as-is
        fixed = TRUE
      )
    }
    
  }
  
  #' Search a Vector With Multiple Terms
  #'
  #' @description takes multiple search terms and a single vector. Uses
  #'   `single_search()` to search the vector for each individual search term. If
  #'   `match_all = TRUE` returns `TRUE` only if the value in a vector matched all
  #'   of the search terms, if `match_all = FALSE` then it will return TRUE if the
  #'   value matched _any_ of the search terms.
  #'
  #' @param vector vector of strings to be searched
  #' @param search_terms vector of strings to perform a search with
  #' @param max.distance Maximum 'distance' between strings that results in a
  #'   match (values between 0 and 1).
  #' @param match_all Logical. If `TRUE`, only returns `TRUE` if a value matches
  #'   every search term. If `FALSE` returns `TRUE` if a value matches any of the
  #'   search terms.
  #'
  #' @return Logical vector of matches.
  #' @export
  #'
  #' @examples
  #' # matches only where one of the search terms appear exactly
  #' search_multiple_terms(
  #'   vector = c("something", "another", "here", "a-value", "some-another-here"), 
  #'   search_terms = c("seme", "here", "another"), 
  #'   max.distance = 0, 
  #'   match_all = FALSE
  #' )
  #' 
  #' # matches where one of the search terms appear, even with a slight mismatch
  #' search_multiple_terms(
  #'   vector = c("something", "another", "here", "a-value", "some-another-here"), 
  #'   search_terms = c("seme", "here", "another"), 
  #'   max.distance = 0.1, 
  #'   match_all = FALSE
  #' )
  #' # matches matches only where _all_ of the search terms appear
  #' search_multiple_terms(
  #'   vector = c("something", "another", "here", "a-value", "some-another-here"), 
  #'   search_terms = c("seme", "here", "another"), 
  #'   max.distance = 0.1, 
  #'   match_all = TRUE
  #' )
  search_multiple_terms <- function(vector, 
                                    search_terms, 
                                    max.distance = 0.1, 
                                    match_all = FALSE) {
    multiple_term_matches <- lapply(search_terms, function(x) {
      search_single_term(
        vector = vector,
        search_term = x,
        max.distance = max.distance
      )
    })
    
    if (match_all) {
      purrr::reduce(multiple_term_matches, `&`)
    } else {
      purrr::reduce(multiple_term_matches, `|`)
    }
  }
  
  #' Search Multiple Columns in a dataframe
  #' 
  #' @description Search multiple columns in a dataframe using the
  #'   `search_multiple_terms()` function. Returns a row if there was a match in
  #'   any of the specified columns.  The same rules for `match_all` and
  #'   `max.distance` apply.
  #'   
  #'
  #' @param df Dataframe to be subset based on search terms.
  #' @param cols Vector of strings, specifying the columns to search.
  #' @param search_terms Vector of strings, giving the terms to be searched.
  #' @param max.distance Maximum 'distance' between strings that results in a match
  #'   (values between 0 and 1).
  #' @param match_all Logical. If `TRUE`, only returns a row that has a match from
  #'   each search term, in any of the columns. If `FALSE` returns a row if any of
  #'   the search terms were matched in any of the columns.
  #'
  #' @return
  #' @export
  #'
  #' @examples
  search_multiple_columns <-
    function(df,
             cols,
             search_terms,
             max.distance = 0.1,
             match_all = FALSE) {
      multiple_column_matches <- lapply(cols, function(x) {
        search_multiple_terms(
          vector = df[, x],
          search_terms = search_terms,
          max.distance = max.distance,
          match_all = match_all
        )
      })
      
      # return TRUE for the row if matched the search in at least one column If
      # match_all = TRUE then it must match all search terms in at least one
      # column If match_all = FALSE then it must match at least one term in at
      # least one column
      
      purrr::reduce(multiple_column_matches, `|`)
      
      
    }
  
  
  #' Subset dataframe rows
  #' 
  #' @description Returns rows
  #'
  #' @param data Dataframe to subset
  #' @param cols Vector of strings of column names to search through.
  #' @param search_terms Vector of strings of search terms.
  #' @param max.distance 
  #' @param match_all 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  search_dataframe <- function(data, 
                               cols, 
                               search_terms, 
                               max.distance = 0.1, 
                               match_all = FALSE) {
    data <- as.data.frame(data)
    
    data[search_multiple_columns(
      data, 
      cols = cols, 
      search_terms = search_terms, 
      max.distance = max.distance, 
      match_all = match_all
    ), ]
  }
  
  #' Remove nested entries from a column.
  #' 
  #' @description Takes a single column that contains multiple entries seprated by
  #'   a something. It first splits the entries by this separator, filters those
  #'   that don't match, and returns a dataframe that replaces the column with
  #'   only those entries that contain the search terms.
  #'
  #' @param data Data frame that contains a column that needs cleaning.
  #' @param col Column name (tidy-select) to be cleaned.
  #' @param search_terms Search terms to match for cleaning (should be the same as
  #'   the other searches).
  #' @param sep String separator to split the nested entries by.
  #' @param max.distance Fuzziness for pattern matching.
  #' @param match_all Logical, whether or not each item must be matched by all
  #'   search terms (`TRUE`) or just at least one search term (`FALSE`).
  #'
  #' @return
  #' @export
  #'
  #' @examples
  strip_unmatched_entries <- function(data, 
                                      col, 
                                      search_terms,
                                      max.distance = 0.1, 
                                      sep = "\n", 
                                      match_all = FALSE) {
    column_as_string <- deparse(substitute(col))
    
    # if there have been no matches, just return the data 
    # without attempting to do anything further
    if (nrow(data) == 0) return(data)
    
    if (!("id" %in% colnames(data))) {
      data <- data %>% 
        tibble::rownames_to_column("id")
    }
    
    split_rows <- data %>% 
      dplyr::select("id", {{ col }}) %>% 
      tidyr::separate_rows({{ col }}, sep = sep)
    
    selected_rows <-
      minchem::search_dataframe(
        data = split_rows, 
        cols = column_as_string, 
        search_terms = search_terms,
        max.distance = max.distance, 
        match_all = match_all
      )
    
    combined_rows <- selected_rows %>% 
      dplyr::group_by(id) %>% 
      dplyr::summarise(collapsed = paste({{ col }}, collapse = ",\n"))
    
    colnames(combined_rows)[2] <- column_as_string
    
    data %>% 
      dplyr::select(-{{ col }}) %>% 
      dplyr::left_join(combined_rows, by = "id")
  }
  
  
