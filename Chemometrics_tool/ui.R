##################################################
## Title: ui.R
## Description: This script is a part of the a Shiny web application to generate different chemometric plots from your data. 
## This is the user-interface definition of a Shiny web application.
## You can run the application by clicking the 'Run App' button above.
## Date: Wed Nov 28 11:09:20 2018
## Author: Purva Kulkarni
##################################################

options(shiny.maxRequestSize = 30*1024^2)

ui <- bootstrapPage(
  mainPanel(
    titlePanel("Chemometrics tool"),
    
    tabsetPanel(
      
      tabPanel("Data input", 
               tags$h2("Upload data file"),
               tags$hr(),
               p("Before uploading the data file, check that it is clean, especially ensure that the the numeric variables contain only the digits 0-9 or NA (to indicate missing data)."),
               p("Rows that contain one or more NAs will be excluded from the PCA."),
               p("Columns that contain a mixture of numbers and text will not be included in the computation of the PCA results."),
               tags$hr(),
               p("Select the following options that match your data file format and then upload:"),
               
               
               radioButtons(inputId = 'header',  
                            label = 'Header',
                            choices = c('Columns have headers'='Yes',
                                        'Columns do not have headers'='No'), 
                            selected = 'Yes'),
               
               radioButtons('sep', 'Separator',
                            c(None='',
                              Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ','),
               
               radioButtons('quote', 'Quote',
                            c(None='',
                              'Double Quote'='"',
                              'Single Quote'="'"),
                            '"'),
               
               
               tags$hr(),
               textInput('Sheetname', 'Sheet name', value = "", width = NULL, placeholder = NULL),
               textInput('StartColumn', 'Enter start column aplhabet', value = "", width = NULL, placeholder = NULL),
               textInput('EndColumn', 'Enter end column aplhabet', value = "", width = NULL, placeholder = NULL),
               
               tags$hr(),
               
               fileInput('file1', 'Choose a .csv/xlsx file to upload (max. 30 MB):',
                         accept = c(
                           'text/csv',
                           'text/comma-separated-values',
                           'text/tab-separated-values',
                           'text/plain',
                           '.csv',
                           '.tsv'
                         )),
               p("After uploading your CSV file, click on the 'Inspect the data' tab")
               
      ), # end file  tab
      
      tabPanel("Summary",
               tags$h2("Data summary"),
               tags$hr(),
               tableOutput('summary'),
               tags$hr(),
               p("Here is the raw data from the CSV file"),
               DT::dataTableOutput('contents')
      ), # end  tab
      
      tabPanel("Compute PCA",
               
               p("Choose the columns of your data to include in the PCA."),
               p("Only columns containing numeric data are shown here because PCA doesn't work with non-numeric data."),
               p("The PCA is automatically re-computed each time you change your selection."),
               p("Observations (ie. rows) are automatically removed if they contain any missing values."),
               p("Variables with zero variance have been automatically removed because they're not useful in a PCA."),
               uiOutput("choose_columns_pca"),
               tags$hr(),
               p("Select options for the PCA computation (we are using the prcomp function here)"),
               radioButtons(inputId = 'center',  
                            label = 'Center',
                            choices = c('Shift variables to be zero centered'='Yes',
                                        'Do not shift variables'='No'), 
                            selected = 'Yes'),
               
               radioButtons('scale.', 'Scale',
                            choices = c('Scale variables to have unit variance'='Yes',
                                        'Do not scale variables'='No'), 
                            selected = 'Yes')
               
      ), # end  tab
      
      
      
      tabPanel("PC Plots",
               h2("Scree plot"),
               p("The scree plot shows the variances of each PC, and the cumulative variance explained by each PC (in %) "),
               plotOutput("plot2", height = "300px"),
               tags$hr(),
               h2("PC plot: zoom and select points"),
               p("Select the grouping variable."),
               p("Only variables where the number of unique values is less than 10% of the total number of observations are shown here (because seeing groups with 1-2 observations is usually not very useful)."),
               uiOutput("the_grouping_variable"),
               tags$hr(),
               p("Select the PCs to plot"),
               uiOutput("the_pcs_to_plot_x"),
               uiOutput("the_pcs_to_plot_y"),
               tags$hr(),
               
               p("Click and drag on the first plot below to zoom into a region on the plot. Or you can go directly to the second plot below to select points to get more information about them."),
               p("Then select points on zoomed plot below to get more information about the points."),
               p("You can click on the 'Compute PCA' tab at any time to change the variables included in the PCA, and then come back to this tab and the plots will automatically update."),
               plotOutput ("z_plot1", height = 400,
                           brush = brushOpts(
                             id = "z_plot1Brush",
                             resetOnNew = TRUE)),
               tags$hr(),
               
               p("Click and drag on the plot below to select points, and inspect the table of selected points below"),
               
               plotOutput("z_plot2", height = 400,
                          brush = brushOpts(
                            id = "plot_brush_after_zoom",
                            resetOnNew = TRUE)),
               tags$hr(),
               p("Details of the brushed points"),
               tableOutput("brush_info_after_zoom")
      ), # end  tab 
      
      
      
      tabPanel("PCA output",
               verbatimTextOutput("pca_details")
               
      ) # end  tab 
      
    )))