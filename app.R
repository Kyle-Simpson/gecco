#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(data.table); library(dplyr); library(tidyverse); library(ggplot2); library(plotly); library(zip); library(DT);
library(scales); library(shiny); library(tools); library(readstata13); library(shinythemes); library(chron); library(feather)

# Increase max filesize
options(shiny.maxRequestSize = 8000*1024^2)
# Helpful functions
`%ni%` <- Negate(`%in%`)
create_upper_vars <- function(inputData, varlist) {
  
  inputData <- setDT(inputData)
  
  # for each input column
  for (i in 1:length(varlist)) {
    # Make vars for column names
    var <- varlist[i]
    uppervar <- paste0("upper_", var)
    
    # Create uppervar column
    inputData[, eval(uppervar) := get(var)]
    
    #---# First pass at special charater removal #---# ####
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'ÿ', replacement = "Y")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'Ÿ', replacement = "Y")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'æ', replacement = "AE")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'Æ', replacement = "AE")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'œ', replacement = "OE")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'Œ', replacement = "OE")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'ç', replacement = "C")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'Ç', replacement = "C")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'ñ', replacement = "N")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'Ñ', replacement = "N")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = 'ß', replacement = "SS")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '/', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '\\', replacement = " ", fixed = T)]
    
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = ':', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = ',', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = ';', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '.', replacement = " ", fixed = T)]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '-', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '(', replacement = " ", fixed = T)]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = ')', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '\"', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = "'", replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '«', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '»', replacement = " ")]
    inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = '·', replacement = " ")]
    inputData[, eval(uppervar) := str_squish(inputData[[uppervar]])] # Remove all instances of consecutive spaces in strings
    
    inputData[, eval(uppervar) := str_trim(inputData[[uppervar]], side = "both")]
    # data[, eval(uppervar) := paste0(" ", data[[uppervar]], " ")]
    #------------------------------------------------# ####
    
    #---# Second pass at special character removal #---# ####
    # Replace A's
    letters <- c("á", "Á", "à", "À", "ã", "Ã", "â", "Â", "å", "Å", "ä", "Ä")
    for (l in 1:length(letters)) {
      letter <- letters[l]
      inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = letter, replacement = "A")]
    }
    
    # Replace E's
    letters <- c("é", "É", "ê", "Ê", "è", "È", "ë", "Ë")
    for (l in 1:length(letters)) {
      letter <- letters[l]
      inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = letter, replacement = "E")]
    }
    
    # Replace I's
    letters <- c("í", "Í", "ì", "Ì", "î", "Î", "ï", "Ï")
    for (l in 1:length(letters)) {
      letter <- letters[l]
      inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = letter, replacement = "I")]
    }
    
    # Replace O's
    letters <- c("ó", "Ó", "ò", "Ò", "õ", "Õ", "ô", "Ô", "ø", "Ø", "ö", "Ö")
    for (l in 1:length(letters)) {
      letter <- letters[l]
      inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = letter, replacement = "O")]
    }
    
    # Replace U's
    letters <- c("ú", "Ú", "ù", "Ù", "û", "Û", "ü", "Ü")
    for (l in 1:length(letters)) {
      letter <- letters[l]
      inputData[, eval(uppervar) := gsub(inputData[[uppervar]], pattern = letter, replacement = "U")]
    }
    #--------------------------------------------------# ####
    
    # Make every row uppercase
    inputData[, eval(uppervar) := toupper(get(uppervar))]
    inputData[, eval(var) := NULL]
    setnames(inputData, eval(uppervar), eval(var))
    
  }
  
  return(inputData)
}

ui <- fluidPage(
  theme=shinytheme('yeti'),
  
  # Application title
  navbarPage('GeCCo'),
  tagList('', a('Documentation', href="https://stash.ihme.washington.edu/projects/SUR/repos/gecco/browse")),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(type='tabs',
                  tabPanel('Intake',
                           fileInput("old_data", "Previous Dataset", 
                                     accept = c(".csv", ".dta", ".feather")),
                           fileInput("new_data", "Current Dataset", 
                                     accept = c(".csv", ".dta", ".feather")),
                           # Tab panel for merge column selection
                           p(strong('Select Columns to Merge on:')),
                           tabsetPanel(type='tabs',
                                       tabPanel('Prev Colnames',
                                                uiOutput('prev_colnames')),
                                       tabPanel('Curr Colnames',
                                                uiOutput('curr_colnames'))),
                           # Dropdown of column to compare
                           br(),
                           p(strong('Select Column to Compare')),
                           tabsetPanel(type='tabs',
                                       tabPanel('Prev Colnames',
                                                uiOutput('prev_colname_merge')),
                                       tabPanel('Curr Colnames',
                                                uiOutput('curr_colname_merge'))),
                           # Outlier percentage input
                           uiOutput("outlier_input")
                  ),
                  tabPanel('Advanced Manipulations',
                           checkboxInput('group_by_check', 'Aggregate data by column?', value=F),
                           uiOutput('group_by_col'),
                           uiOutput('aggregation_type'),
                           checkboxInput('draw_check', 'Data contains draws', value=F),
                           uiOutput('draw_col')
                  )),
      # Button to execute merge
      uiOutput('testMerge')
    ),
    
    
    mainPanel(
      tabsetPanel(type='tabs',
                  tabPanel('Overview',
                           br(),
                           tabsetPanel(type='tabs',
                                       tabPanel('Summary Statistics',
                                                br(),
                                                fluidRow(
                                                  column(tableOutput('prev_summary_stats'), width = 6),
                                                  column(tableOutput('curr_summary_stats'), width = 6)),
                                                br(),
                                                h4(textOutput('merge_nrow')),
                                                h4(textOutput('merge_inconsistencies'))
                                       ),
                                       tabPanel('Browse Data',
                                                dataTableOutput("data_browse"),
                                                br(),
                                                p(strong('Notes:')),
                                                p('1. All calculations are performed as (previous_dataset - current_dataset) as previous_dataset is assumed to be the baseline data.'),
                                                p('2. The `equate_check` column == 0 if there are no differences and == 1 if there are any differences.  This means you can sort this column descending to see inconsistencies first.'))
                           )
                  ),
                  tabPanel('Scatter Plots',
                           br(),
                           fluidRow(column(checkboxInput('is_log_vis', 'View in Log Space'), width=3),
                                    column(checkboxInput('should_facet', 'Facet graph by column?'), width=3),
                                    column(uiOutput('facet_by'), width=6)),
                           
                           tabsetPanel(type='tabs',
                                       tabPanel('Plot',
                                                plotlyOutput('scatter')),
                                       tabPanel('Outlier Table',
                                                dataTableOutput('outliers')))),
                  tabPanel('Distribution Charts',
                           br(),
                           tabsetPanel(type='tabs',
                                       tabPanel('Box Plot',
                                                plotlyOutput('boxplot')
                                       ),
                                       tabPanel('Density Plot',
                                                br(),
                                                fluidRow(column(strong(textOutput('prev_density_stat')), width=6, align='center'),
                                                         column(strong(textOutput('curr_density_stat')), width=6, align='center')),
                                                plotlyOutput('density')
                                       )
                           )
                  )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #----# Sidebar Functions #----# ####
  ### Column Utils ###
  # Function to extract unique columns
  UID_cols <- reactive({
    req(input$prev_colnames)
    req(input$curr_colnames)
    # Construct list of both sets of colnames
    names <- list('prev' = input$prev_colnames, 'curr' = input$curr_colnames)
    # Return list
    return(names)
  })
  # Function to extract column to compare
  compare_col <- reactive({
    req(input$prev_colname_merge)
    req(input$curr_colname_merge)
    
    col <- list('prev' = input$prev_colname_merge, 'curr' = input$curr_colname_merge)
    
    return(col)
  })
  # Function to extract outlier percentage
  outlier_pc <- reactive({
    req(input$outlier_input)
    
    return(input$outlier_input)
  })
  #-----------------------------#
  ### Data Utils ###
  # Pull historical data
  old_data <- reactive({
    req(input$old_data)
    
    inFile <- input$old_data
    # Read in data
    if(file_ext(inFile)[1] == "csv") {
      data <- setDT(fread(inFile$datapath))
    } else if (file_ext(inFile)[1] == "dta") {
      data <- setDT(read.dta13(inFile$datapath))
    } else if (file_ext(inFile)[1] == "feather") {
      data <- setDT(read_feather(inFile$datapath))
    }
    
    return(data)
  })
  # Pull new data
  new_data <- reactive({
    req(input$new_data)
    
    inFile <- input$new_data
    
    if(file_ext(inFile)[1] == "csv") {
      data <- setDT(fread(inFile$datapath))
    } else if (file_ext(inFile)[1] == "dta") {
      data <- setDT(read.dta13(inFile$datapath))
    } else if (file_ext(inFile)[1] == "feather") {
      data <- setDT(read_feather(inFile$datapath))
    }
    
    return(data)
  })
  # Present previous column names
  output$prev_colnames <- renderUI({
    colnames <- colnames(old_data())
    selectInput('prev_colnames', "Previous columns:", choices = as.list(sort(colnames)), multiple=TRUE, selectize=TRUE)
  })
  # Present current column names
  output$curr_colnames <- renderUI({
    colnames <- colnames(new_data())
    selectInput('curr_colnames', "Current columns:", choices = as.list(sort(colnames)), multiple=TRUE, selectize=TRUE)
  })
  # Present merge test button
  output$testMerge <- renderUI({
    req(input$old_data)
    req(input$new_data)
    
    # Popup notification if datasets are identical
    if (isTRUE(all.equal(old_data(), new_data()))) {
      showModal(modalDialog(title='SUCCESS!', 'Datasets are identical! No need to proceed with comparisons!'))
    } 
    
    actionButton('doMerge', 'Update Tables')
  })
  # Present comparison column dropdown
  output$prev_colname_merge <- renderUI({
    prev_names <- UID_cols()
    old_data <- old_data()
    colnames <- names(old_data)[names(old_data) %ni% prev_names$prev]
    
    selectInput('prev_colname_merge', 'Previous column:', choices=as.list(sort(colnames)), multiple=FALSE, selectize=TRUE)
  })
  output$curr_colname_merge <- renderUI({
    curr_names <- UID_cols()
    new_data <- new_data()
    colnames <- names(new_data)[names(new_data) %ni% curr_names$curr]
    
    selectInput('curr_colname_merge', 'Current column:', choices=as.list(sort(colnames)), multiple=FALSE, selectize=TRUE)
  })
  # Present outlier numeric input
  output$outlier_input <- renderUI({
    req(old_data())
    req(new_data())
    numericInput("outlier_input", "Outlier Percentage", 
                 min = 1, max = 100, value = 20)
  })
  
  full_data <- eventReactive(input$doMerge, {
    # Required inputs
    UID_cols <- UID_cols()
    compare_col <- compare_col()
    old_process <- c()
    new_process <- c()
    old <- old_data()
    new <- new_data()
    
    # Clean merging columns
    for (col in UID_cols$prev) {
      if(typeof(old[[col]]) == 'character') {
        old_process <- append(old_process, col)
      }
    }
    for (col in UID_cols$curr) {
      if(typeof(new[[col]]) == 'character') {
        new_process <- append(new_process, col)
      }
    }
    if (length(old_process) > 0) {
      old <- create_upper_vars(old, old_process)
    }
    if (length(new_process) > 0) {
      new <- create_upper_vars(new, new_process)
    }
    
    # Calculate draws if requested
    if (input$draw_check) {
      # Calculate & rename
      old <- calc_draws(old, T)
      setnames(old, c('mean', 'upper', 'lower'), c(input$draw_val_prev, paste0(input$draw_val_prev, '_upper'), paste0(input$draw_val_prev, '_lower')))
      new <- calc_draws(new, F)
      setnames(new, c('mean', 'upper', 'lower'), c(input$draw_val_curr, paste0(input$draw_val_curr, '_upper'), paste0(input$draw_val_curr, '_lower')))
      # Fix UID_cols if the draw name column was listed
      if (input$draw_col_prev %in% UID_cols$prev) { UID_cols$prev <- UID_cols$prev[UID_cols$prev %ni% input$draw_col_prev] }
      if (input$draw_col_curr %in% UID_cols$curr) { UID_cols$curr <- UID_cols$curr[UID_cols$curr %ni% input$draw_col_curr] }
      # Fix compare_col if the draw value column was listed
      if (compare_col$prev == input$draw_val_prev) {
        compare_col <- compare_col()    
      }
    }
    
    # Aggregate data if requested
    if (input$group_by_check) {
      old <- aggregate_data(old, T)
      new <- aggregate_data(new, F)
    }
    
    # Merging
    data <- merge(old, new, by.x=sort(UID_cols$prev), by.y=sort(UID_cols$curr), all=T)
    
    ### Calculations where the comarison columns are not name matched (merge does not result in .x and .y)
    if (compare_col$prev != compare_col$curr) {
      # Ensure compare_col has no NA values (for numeric columns)
      if (is.numeric(data[, get(compare_col$prev)])) {
        data[is.na(get(compare_col$prev)), eval(compare_col$prev) := 0]
        data[is.na(get(compare_col$curr)), eval(compare_col$curr) := 0]
      }
      # basic equate check
      data[, equate_check := 1]
      data[get(compare_col$prev) == get(compare_col$curr), equate_check := 0]
      # Absolute, percent, and Log difference check (for numeric columns)
      if (is.numeric(data[, get(compare_col$prev)])) {
        # Absolute difference
        data[, abs_diff := abs(get(compare_col$prev) - get(compare_col$curr))]
        data[is.na(abs_diff), abs_diff := 0]
        # Percent difference
        data[, pct_diff := (100 * (get(compare_col$prev) - get(compare_col$curr)) / get(compare_col$curr))]
        data[is.na(pct_diff), pct_diff := 0]
        # Log percent difference
        data[, log_prev := log(get(compare_col$prev))]
        data[, log_curr := log(get(compare_col$curr))]
        data[, log_pct_diff := (100 * (log(log_prev) - log(log_curr)))]
        data[is.na(log_pct_diff), log_pct_diff := 0]
        data[, `:=`(log_prev = NULL, log_curr = NULL)]
      }
    }
    ### Calculations where the comparison columns are name matched (merge results in .x and .y)
    else {
      # Ensure compare_col has no NA values (for numeric columns)
      if (is.numeric(data[, get(paste0(compare_col$prev, '.x'))])) {
        data[is.na(get(paste0(compare_col$prev, '.x'))), eval(paste0(compare_col$prev, '.x')) := 0]
        data[is.na(get(paste0(compare_col$curr, '.y'))), eval(paste0(compare_col$curr, '.y')) := 0]
      }
      # basic equate check
      data[, equate_check := 1]
      data[get(paste0(compare_col$prev, '.x')) == get(paste0(compare_col$curr, '.y')), equate_check := 0]
      # Absolute, percent, and Log difference check (for numeric columns)
      if (is.numeric(data[, get(paste0(compare_col$prev, '.x'))])) {
        # Absolute difference
        data[, abs_diff := abs(get(paste0(compare_col$prev, '.x')) - get(paste0(compare_col$curr, '.y')))]
        data[is.na(abs_diff), abs_diff := 0]
        # Percent difference
        data[, pct_diff := (100 * (get(paste0(compare_col$prev, '.x')) - get(paste0(compare_col$curr, '.y'))) / get(paste0(compare_col$curr, '.y')))]
        data[is.na(pct_diff), pct_diff := 0]
        # Log percent difference
        data[, log_prev := log(get(paste0(compare_col$prev, '.x'))),]
        data[, log_curr := log(get(paste0(compare_col$curr, '.y'))),]
        data[, log_pct_diff := (100 * (log(log_prev) - log(log_curr)))]
        data[is.na(log_pct_diff), log_pct_diff := 0]
        data[, `:=`(log_prev = NULL, log_curr = NULL)]
      }
    }
    
    return(data)
  })
  
  aggregate_data <- function(data, is_old) {
    funct <- input$aggregation_type
    
    # If we're aggregating the previous data
    if (is_old) {
      uid_cols <- UID_cols()$prev
      compare_col <- compare_col()$prev
      gb <- input$group_by_col_prev
      cols <- c(uid_cols, gb)
    }
    else {
      uid_cols <- UID_cols()$curr
      compare_col <- compare_col()$curr
      gb <- input$group_by_col_curr
      cols <- c(uid_cols, gb)
    }
    
    data <- data[, lapply(.SD, get(funct), na.rm=T), by=c(cols), .SDcols=c(compare_col)]
    
    return(data)
  }
  
  calc_draws <- function(data, is_old) {
    if (is_old) {
      uid_cols <- UID_cols()$prev
      dc <- input$draw_col_prev
      vc <- input$draw_val_prev
      if (dc %in% uid_cols) {uid_cols <- uid_cols[uid_cols %ni% dc]}
    }
    else {
      uid_cols <- UID_cols()$curr
      dc <- input$draw_col_curr
      vc <- input$draw_val_curr
      if (dc %in% uid_cols) {uid_cols <- uid_cols[uid_cols %ni% dc]}
    }
    
    data <- data[, .(mean = mean(get(vc), na.rm=T), lower = quantile(get(vc), 0.025, na.rm=T), upper = quantile(get(vc), 0.975, na.rm=T)), by=c(uid_cols)]
    
    return(data)
  }
  #-----------------------------#
  ### Advanced Manipulations ###
  # Group by functions
  output$group_by_col <- renderUI({
    if (input$group_by_check) {
      tabsetPanel(type='tabs',
                  tabPanel('Prev Colnames',
                           selectInput('group_by_col_prev',
                                       'Previous column:',
                                       choices=as.list(sort(names(old_data()))),
                                       multiple=TRUE, selectize=TRUE)),
                  tabPanel('Curr Colnames',
                           selectInput('group_by_col_curr',
                                       'Current column:',
                                       choices=as.list(sort(names(new_data()))),
                                       multiple=TRUE, selectize=TRUE)))
    }
  })
  output$aggregation_type <- renderUI({
    if (input$group_by_check) {
      selectInput('aggregation_type', 'Type of aggregation to perform:', choices = c('sum', 'mean'), selected = 'sum')
    }
  })
  # Presents list of columns to select draw column name
  output$draw_col <- renderUI({
    if (input$draw_check) {
      tabsetPanel(type='tabs',
                  tabPanel('Prev Colnames',
                           selectInput('draw_col_prev',
                                       'Name of previous column containing draw labels:',
                                       choices=as.list(sort(colnames(old_data()))),
                                       selected = as.list(sort(colnames(old_data()))[1]),),
                           selectInput('draw_val_prev',
                                       'Name of previous column containing draw values:',
                                       choices=as.list(sort(colnames(old_data()))),
                                       selected = as.list(sort(colnames(old_data())))[1])
                  ),
                  tabPanel('Curr Colnames',
                           selectInput('draw_col_curr',
                                       'Name of current column containing draw labels:',
                                       choices=as.list(sort(colnames(new_data()))),
                                       selected = as.list(sort(colnames(new_data())))[1]),
                           selectInput('draw_val_curr',
                                       'Name of current column containing draw values:',
                                       choices=as.list(sort(colnames(new_data()))),
                                       selected = as.list(sort(colnames(new_data())))[1])
                  ))
    }
  })
  #-----------------------------# ####
  
  #----# Main Panel Functions #----# ####
  ### Overview Tab
  ## Summary Statistics
  output$prev_summary_stats <- renderTable({
    compare_col <- compare_col()
    old_data <- old_data()
    
    dt <- data.table('Summary Statistics - Previous' = c("Number of rows in the 'previous' dataset", 
                                                         paste0('Mean ', compare_col$prev),
                                                         paste0('Median ', compare_col$prev),
                                                         paste0('Min ', compare_col$prev),
                                                         paste0('Max ', compare_col$prev)),
                     'Values' = c(nrow(old_data), 
                                  mean(old_data[, get(compare_col$prev)], na.rm=T),
                                  median(old_data[, get(compare_col$prev)], na.rm=T),
                                  min(old_data[, get(compare_col$prev)], na.rm=T),
                                  max(old_data[, get(compare_col$prev)], na.rm=T)))
    return(dt)
  }, rownames=F)
  output$curr_summary_stats <- renderTable({
    compare_col <- compare_col()
    new_data <- new_data()
    dt <- data.table('Summary Statistics - Current' = c("Number of rows in the 'current' dataset",
                                                        paste0('Mean ', compare_col$curr),
                                                        paste0('Median ', compare_col$curr),
                                                        paste0('Min ', compare_col$curr),
                                                        paste0('Max ', compare_col$curr)),
                     'Values' = c(nrow(new_data),
                                  mean(new_data[, get(compare_col$curr)], na.rm=T),
                                  median(new_data[, get(compare_col$curr)], na.rm=T),
                                  min(new_data[, get(compare_col$curr)], na.rm=T),
                                  max(new_data[, get(compare_col$curr)], na.rm=T)))
    return(dt)
  }, rownames=F)
  output$merge_nrow <- renderText({
    return(paste0('Number of rows in merged dataset: ', nrow(full_data())))
  })
  output$merge_inconsistencies <- renderText({
    dt <- full_data()
    compare_col <- compare_col()
    return(paste0('Number of rows in merged dataset with differences by [', compare_col$prev, '/', compare_col$curr, ']: ', nrow(dt[dt$equate_check == 1,])))
  })
  ## Browse Data
  output$data_browse <- renderDataTable({
    data <- full_data()
    compare_col <- compare_col()
    uid_cols <- UID_cols()
    draw_cols <- c()
    
    # Remove draw column name from UID_cols if it's been aggregated
    if (input$draw_check) {
      if (input$draw_col_prev %in% uid_cols$prev) {
        uid_cols$prev <- uid_cols$prev[uid_cols$prev %ni% input$draw_col_prev]
        uid_cols$curr <- uid_cols$curr[uid_cols$curr %ni% input$draw_col_curr]
      }
      if (input$draw_val_prev == input$draw_val_curr) {
        draw_cols <- c(paste0(input$draw_val_prev, '_lower.x'), paste0(input$draw_val_curr, '_lower.y'),
                       paste0(input$draw_val_prev, '_upper.x'), paste0(input$draw_val_curr, '_upper.y'))
      } else {
        draw_cols <- c(paste0(input$draw_val_prev, '_lower'), paste0(input$draw_val_curr, '_lower'),
                       paste0(input$draw_val_prev, '_upper'), paste0(input$draw_val_curr, '_upper'))
      }
    }
    
    # Return just the UID columns, comparison column, and the calculation columns
    if (compare_col$prev != compare_col$curr) {
      data <- data[, c(uid_cols$prev, uid_cols$curr, compare_col$prev, compare_col$curr, draw_cols, 'equate_check', 'abs_diff', 'pct_diff', 'log_pct_diff'), with=F]
      setnames(data, c(compare_col$prev, compare_col$curr), 
               c(paste0('previous.', compare_col$prev), paste0('current.', compare_col$curr)))
    } 
    else {
      data <- data[, c(uid_cols$prev, uid_cols$curr, paste0(compare_col$prev, '.x'), paste0(compare_col$curr, '.y'), draw_cols, 'equate_check', 'abs_diff', 'pct_diff', 'log_pct_diff'), with=F]
      setnames(data, c(paste0(compare_col$prev, '.x'), paste0(compare_col$curr, '.y')), 
               c(paste0('previous.', compare_col$prev), paste0('current.', compare_col$curr)))
    }
    
    # Rename all UID columns to have 'previous' or 'current' prefix
    setnames(data, uid_cols$prev, paste0('previous.', uid_cols$prev))
    setnames(data, uid_cols$curr, paste0('current.', uid_cols$curr))
    
    # Rename draw_cols if present
    if (input$draw_check) {
      if (input$draw_val_prev == input$draw_val_curr) {
        setnames(data, c(paste0(input$draw_val_prev, '_lower.x'), paste0(input$draw_val_curr, '_lower.y'),
                         paste0(input$draw_val_prev, '_upper.x'), paste0(input$draw_val_curr, '_upper.y')),
                 c(paste0('previous.', input$draw_val_prev, '_lower'), paste0('current.', input$draw_val_curr, '_lower'),
                   paste0('previous.', input$draw_val_prev, '_upper'), paste0('current.', input$draw_val_curr, '_upper')))
      } else {
        setnames(data, c(paste0(input$draw_val_prev, '_lower'), paste0(input$draw_val_curr, '_lower'),
                         paste0(input$draw_val_prev, '_upper'), paste0(input$draw_val_curr, '_upper')),
                 c(paste0('previous.', input$draw_val_prev, '_lower'), paste0('current.', input$draw_val_curr, '_lower'),
                   paste0('previous.', input$draw_val_prev, '_upper'), paste0('current.', input$draw_val_curr, '_upper')))
      }
    }
    
    return(data)
  })
  
  ### Scatter Plots
  output$facet_by <- renderUI({
    if (input$should_facet) {
      selectInput('facet_by', 'Column to facet by:', choices=c(UID_cols()$prev), selected = UID_cols()$prev[1])
    }
  })
  ## Scatter plot
  output$scatter <- renderPlotly({
    data <- full_data()
    compare_col <- compare_col()
    outlier_pc <- outlier_pc()
    is_log_vis <- input$is_log_vis
    
    if (compare_col$prev != compare_col$curr) {
      data[, prev := get(compare_col$prev)]
      data[, curr := get(compare_col$curr)]
      if (input$draw_check) {
        data[, prev_lower := get(paste0(compare_col$prev, '_lower'))]
        data[, prev_upper := get(paste0(compare_col$prev, '_upper'))]
        data[, curr_lower := get(paste0(compare_col$curr, '_lower'))]
        data[, curr_upper := get(paste0(compare_col$curr, '_upper'))]
      }
    }
    else {
      data[, prev := get(paste0(compare_col$prev, '.x'))]
      data[, curr := get(paste0(compare_col$curr, '.y'))]
      if (input$draw_check) {
        data[, prev_lower := get(paste0(compare_col$prev, '_lower.x'))]
        data[, prev_upper := get(paste0(compare_col$prev, '_upper.x'))]
        data[, curr_lower := get(paste0(compare_col$curr, '_lower.y'))]
        data[, curr_upper := get(paste0(compare_col$curr, '_upper.y'))]
      }
    }
    
    # Scatter plot
    if (is_log_vis) {
      scatter <- ggplot(data = data) +
        geom_point(mapping=aes(x=prev, y=curr, 
                               text = paste0('Previous ', compare_col$prev, ': ', prev,
                                             '\nCurrent ', compare_col$curr, ': ', curr)),
                   color = ifelse(abs(data$log_pct_diff) >= log(outlier_pc), "red", "blue")) +
        geom_abline(slope = 1, intercept = 0, color="red", linetype="dashed") +
        scale_x_log10(labels = comma) +
        scale_y_log10(labels = comma) +    
        labs(title="Previous vs Current: Log Space",
             x=paste0('previous.', compare_col$prev), y=paste0('current.', compare_col$curr)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    else {
      scatter <- ggplot(data = data) +
        geom_point(mapping=aes(x=prev, y=curr, 
                               text=paste0('Previous ', compare_col$prev, ': ', prev,'\nCurrent ', compare_col$curr, ': ', curr)
        ),
        color=ifelse(abs(data$pct_diff) >= outlier_pc, 'red', 'blue')) +
        geom_abline(slope = 1, intercept = 0, color="red", linetype="dashed") +
        scale_x_continuous(labels = comma) +
        scale_y_continuous(labels = comma) +
        labs(title="Previous vs Current: Raw Comparison",
             x=paste0('previous.', compare_col$prev), y=paste0('current.', compare_col$curr)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    if (input$should_facet) {
      scatter <- scatter + facet_wrap(input$facet_by)
    }
    
    scatter <- ggplotly(scatter, height = 800, width = 1000, tooltip='text')
    
    return(scatter)
  })
  ## Plot outliers
  output$outliers <- renderDataTable({
    data <- full_data()
    uid_cols <- UID_cols()
    outlier_pc <- outlier_pc()
    compare_col <- compare_col()
    is_log_vis <- input$is_log_vis
    draw_cols <- c()
    
    # Subset to outliers
    if (is_log_vis) {
      data <- data[abs(log_pct_diff) >= log(outlier_pc), ]
    }
    else {
      data <- data[abs(pct_diff) >= outlier_pc, ]
    }
    
    # Remove draw column name from UID_cols if it's been aggregated
    if (input$draw_check) {
      if (input$draw_col_prev %in% uid_cols$prev) {
        uid_cols$prev <- uid_cols$prev[uid_cols$prev %ni% input$draw_col_prev]
        uid_cols$curr <- uid_cols$curr[uid_cols$curr %ni% input$draw_col_curr]
      }
      if (input$draw_val_prev == input$draw_val_curr) {
        draw_cols <- c(paste0(input$draw_val_prev, '_lower.x'), paste0(input$draw_val_curr, '_lower.y'),
                       paste0(input$draw_val_prev, '_upper.x'), paste0(input$draw_val_curr, '_upper.y'))
      } else {
        draw_cols <- c(paste0(input$draw_val_prev, '_lower'), paste0(input$draw_val_curr, '_lower'),
                       paste0(input$draw_val_prev, '_upper'), paste0(input$draw_val_curr, '_upper'))
      }
    }
    
    # Keep only relevant columns
    if (compare_col$prev != compare_col$curr) {
      data <- data[, c(uid_cols$prev, uid_cols$curr, compare_col$prev, compare_col$curr, draw_cols, 'equate_check', 'abs_diff', 'pct_diff', 'log_pct_diff'), with=F]
      setnames(data, c(compare_col$prev, compare_col$curr), 
               c(paste0('previous.', compare_col$prev), paste0('current.', compare_col$curr)))
    } 
    else {
      data <- data[, c(uid_cols$prev, uid_cols$curr, paste0(compare_col$prev, '.x'), paste0(compare_col$curr, '.y'), draw_cols, 'equate_check', 'abs_diff', 'pct_diff', 'log_pct_diff'), with=F]
      setnames(data, c(paste0(compare_col$prev, '.x'), paste0(compare_col$curr, '.y')), 
               c(paste0('previous.', compare_col$prev), paste0('current.', compare_col$curr)))
    }
    
    # Rename all UID columns to have 'previous' or 'current' prefix
    setnames(data, uid_cols$prev, paste0('previous.', uid_cols$prev))
    setnames(data, uid_cols$curr, paste0('current.', uid_cols$curr))
    
    # Rename draw_cols if present
    if (input$draw_check) {
      if (input$draw_val_prev == input$draw_val_curr) {
        setnames(data, c(paste0(input$draw_val_prev, '_lower.x'), paste0(input$draw_val_curr, '_lower.y'),
                         paste0(input$draw_val_prev, '_upper.x'), paste0(input$draw_val_curr, '_upper.y')),
                 c(paste0('previous.', input$draw_val_prev, '_lower'), paste0('current.', input$draw_val_curr, '_lower'),
                   paste0('previous.', input$draw_val_prev, '_upper'), paste0('current.', input$draw_val_curr, '_upper')))
      } else {
        setnames(data, c(paste0(input$draw_val_prev, '_lower'), paste0(input$draw_val_curr, '_lower'),
                         paste0(input$draw_val_prev, '_upper'), paste0(input$draw_val_curr, '_upper')),
                 c(paste0('previous.', input$draw_val_prev, '_lower'), paste0('current.', input$draw_val_curr, '_lower'),
                   paste0('previous.', input$draw_val_prev, '_upper'), paste0('current.', input$draw_val_curr, '_upper')))
      }
    }
    
    return(data)
  })
  
  ### Distribution Charts
  ## Box Plot
  output$boxplot <- renderPlotly({
    data <- full_data()
    compare_col <- compare_col()
    
    if (compare_col$prev == compare_col$curr) {
      data <- melt(data, measure.vars=c(paste0(compare_col$prev, '.x'), paste0(compare_col$curr, '.y')))
      data$variable <- ifelse(data$variable == paste0(compare_col$prev, '.x'), paste0('previous.', compare_col$prev), paste0('current.', compare_col$curr))
    } else {
      data <- melt(data, measure.vars=c(compare_col$prev, compare_col$curr))
      data$variable <- ifelse(data$variable == compare_col$prev, paste0('previous.', compare_col$prev), paste0('current.', compare_col$curr))
    }
    
    hist <- ggplot(data = data) +
      geom_boxplot(mapping=aes(x=variable, y=value)) +
      labs(x='')
    
    hist <- ggplotly(hist, height = 800, width = 1000)
    
    return(hist)
  })
  ## Density plots
  output$density <- renderPlotly({
    data <- full_data()
    compare_col <- compare_col()
    
    if (compare_col$prev == compare_col$curr) {
      prev_mean <- mean(data[, get(paste0(compare_col$prev, '.x'))], na.rm=T)
      curr_mean <- mean(data[, get(paste0(compare_col$curr, '.y'))], na.rm=T)
      
      data <- melt(data, measure.vars=c(paste0(compare_col$prev, '.x'), paste0(compare_col$curr, '.y')))
      data$variable <- ifelse(data$variable == paste0(compare_col$prev, '.x'), paste0('previous.', compare_col$prev), paste0('current.', compare_col$curr))
    } else {
      prev_mean <- mean(data[, get(compare_col$prev)], na.rm=T)
      curr_mean <- mean(data[, get(compare_col$curr)], na.rm=T)
      
      data <- melt(data, measure.vars=c(compare_col$prev, compare_col$curr))
      data$variable <- ifelse(data$variable == compare_col$prev, paste0('previous.', compare_col$prev), paste0('current.', compare_col$curr))
    }
    
    density <- ggplot(data = data) +
      geom_density(mapping=aes(x=value, fill=variable), alpha=0.4) +
      geom_vline(mapping=aes(xintercept=prev_mean), linetype='dashed') +
      geom_vline(mapping=aes(xintercept=curr_mean), linetype='dotted') +
      labs(fill='')
    
    density <- ggplotly(density, height = 800, width = 1000)
    
    return(density)
  })
  output$prev_density_stat <- renderText({
    compare_col <- compare_col()
    old <- old_data()
    return(paste0('Mean density of previous.', compare_col$prev, ': ', mean(old[, get(compare_col$prev)], na.rm=T)))
  })
  output$curr_density_stat <- renderText({
    compare_col <- compare_col()
    new <- new_data()
    return(paste0('Mean density of current.', compare_col$curr, ': ', mean(new[, get(compare_col$curr)], na.rm=T)))
  })
  #--------------------------------# ####
}

# Run the application 
shinyApp(ui = ui, server = server)