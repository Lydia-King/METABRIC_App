# Allow large file uploads and sanitize errors
options(shiny.maxRequestSize=30*1024^3) 
options(spinner.size=1.2)
options(spinner.type = 5)
options(spinner.color="#2cdeeb")

    
# Load up libraries 
library(shiny) # Build shiny app
library(shinydashboard) # For shiny dashboard layout
library(dashboardthemes) # For shiny dashboard layout
library(DT) # Used to create scrollable datatables 
library(dplyr) # Used for data subsetting, score calculations etc.
library(fabricatr) # Used for splitting into scores quartiles 
library(ggplot2) # Used for ggplots (Exploratory plots and survival plots)
library(ggthemes) #  Used for ggplots (Exploratory plots and survival plots)
library(reshape) # Used for facetwrap plots -> melt data
library(survminer) # Used for Survival Analysis
library(survival)# Used for Survival Analysis 
library(maftools) # Used for mutation analysis 
library(shinycssloaders) # Used for loading screen
library(RColorBrewer) # Used for Segmented Density Plots
library(car) # For Levenes Test
library(DescTools) # For Dunns Test


# tweaks_levels: a list object to set up multicols for checkboxGroupInput within factor recoding tab
# tweaks_Cox: a list object to set up multicols for checkboxGroupInput for variable selection Cox models 

tweaks_Levels <- list(tags$head(tags$style(HTML(".multicol { height: 180px; -webkit-column-count: 6; /* Chrome, Safari, Opera */ 
                               -moz-column-count: 6;    /* Firefox */ column-count: 6; -moz-column-fill: auto; -column-fill: auto;"))))

tweaks_Cox <- list(tags$head(tags$style(HTML(".multicol { height: 280px; -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                               -moz-column-count: 5;    /* Firefox */ column-count: 5; -moz-column-fill: auto; -column-fill: auto;"))))
    
# Data control: the checkboxes will control the data values plotted
controls_Uni <- list(tags$div(align = 'left', class = 'multicol', checkboxGroupInput('show_vars', 'Columns to Test:', choices= "", selected = ""))) # Univariate Cox variable selection
controls_Multi <- list(tags$div(align = 'left', class = 'multicol', checkboxGroupInput('show_vars1', 'Columns to Test:', choices= "", selected = ""))) # Multivariate Cox variable selection
controls_Num <- list(tags$div(align = 'left', class = 'multicol', checkboxGroupInput('show_vars3', 'Columns to Test:', choices= "", selected = ""))) # Recode variable to numeric 
controls_Cat <- list(tags$div(align = 'left', class = 'multicol', checkboxGroupInput('show_vars4', 'Columns to Test:', choices= "", selected = ""))) # Recode varibale to caegorical with n factor levels 

#css <- ".shiny-output-error { visibility: hidden; }.shiny-output-error:before {visibility: visible; content: 'An error has occured. Make sure the appropriate inputs have been selected and try again. If the error persists please contact the app author for clarification'; }}"


# Define UI for application
ui <- dashboardPage(
    
    dashboardHeader(tags$li(class = "dropdown", tags$style(".main-header {max-height: 40px}"), tags$style(".main-header .logo {height: 40px}"),  tags$style(".sidebar-toggle {height: 40px; padding-top: 1px !important;}"),
                            tags$style(".navbar {min-height:40px !important}")), # Title row margin settings
                    
                    title = h4(id ='head', "Metabric App"), titleWidth = 195), # Set title and title width 
    
    # Create side-bar menu with all tab options: 
    dashboardSidebar(tags$style(".left-side, .main-sidebar {padding-top: 60px}"), width = 195,
                     sidebarMenu(menuItem("Input Files", tabName = "input_files", icon = icon("file-alt")),
                                 menuItem("Exploratory Tables", tabName = "tables", icon = icon("table")),
                                 menuItem("Recode/Subset Data", tabName = "Recode1", icon = icon("sort-alpha-down"),
                                          menuSubItem("Recode and Subset", tabName = "Recode"),
                                          menuSubItem("Variable Levels", tabName = "FactorLevels")),
                                 menuItem("Exploratory Plots", tabName = "plots", icon = icon("chart-bar"),
                                          menuSubItem("Boxplots", tabName = "boxplot", icon = icon("align-left")),
                                          menuSubItem("Scatter Plots", tabName = "scatterplot", icon = icon("braille")),
                                          menuSubItem("Barplots", tabName = "Explor", icon = icon("chart-bar")),
                                          menuSubItem("Density Plots", tabName = "Dist", icon = icon("chart-area"))),
                                 menuItem("Survival Analysis", tabName = "survival", icon = icon("heartbeat"),
                                          menuSubItem("KM-Plot Clinical Variables", tabName = "KMplot"),
                                          menuSubItem("KM-Plot CNA Quartiles", tabName = "KMOver"),
                                          menuSubItem("KM-Plot by Treatment", tabName = "KMplotRadio")),
                                 menuItem("Cox Regression", tabName = "Cox", icon = icon("file-medical-alt"),
                                          menuSubItem("Univariate Analysis", tabName = "UniVar"),
                                          menuSubItem("Multivariate Analysis", tabName = "MultiVar"),
                                          menuSubItem("Cox OS Assumptions", tabName = "AssumptionsOS"),
                                          menuSubItem("Cox DSS Assumptions", tabName = "AssumptionsDSS")),
                                 menuItem("Association Tests", tabName = "ASTest", icon = icon("connectdevelop")),
                                 menuItem("Maftools Summary", tabName = "MAFplots", icon = icon("exclamation-triangle"),
                                          menuSubItem("MAF Text Summary", tabName = "MAFText"),
                                          menuSubItem("MAF Visual Summary", tabName = "MAFVis")))),
    
    dashboardBody(
        tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Bree+Serif|Cabin:400,400'); #head{color: #ffffff; font-family: 'Bree Serif';}")), # Fonts
        shinyDashboardThemes(theme = "blue_gradient"), # Dashboard layout
        
        # Create Generic users error 
       # tags$style(type="text/css", css),
        
       # Input File Tab: Space to upload desired files (Clinical patient, sample, CNA and MAF)
        tabItems(tabItem(tabName = "input_files",
                         tabBox(id = "tabset1", height = "890px", width = "800px", 
                                # Clinical Pateint Data Tab 
                                tabPanel("Clinical Patient Data", 
                                         box(title = "Input Patient Data File", width = 3, status = "primary", solidHeader = TRUE, br(),
                                             fileInput("fileClinical1", "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(), 
                                             checkboxInput("header", "Header", TRUE),  # Input: Checkbox if file has header 
                                             fluidRow(column(5, radioButtons("sep", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t")), # Input: Select separator 
                                                      column(7, radioButtons("quote", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'))), tags$hr(), # Input: Select quotes 
                                             numericInput(inputId = "skipypatient", label = "Number of Lines to Skip:", value = 4, min = 0, max = 10),  tags$hr(), # Input: Choose number of lines to skip
                                          
                                             h5(strong("Total Number of Clinical Variables:")), verbatimTextOutput('TotalC', placeholder = T),  # Space to display number of rows/columns
                                             h5(strong("Total Number of Patients:")), verbatimTextOutput('TotalR', placeholder = T),
                                             tags$hr(),br(), actionButton("goButtonClin", "Show Preview", icon = icon("table"))), # Action Button to display preview 
                                         
                                         box(title = "Preview" , width = 9, status = "primary", solidHeader = TRUE, height = "720px", withSpinner(DT::dataTableOutput("ClinicalP")), style = "height:675px; overflow-y: scroll;overflow-x: scroll;")),
                                
                                # Clinical Sample Data Tab 
                                tabPanel("Clinical Sample Data",
                                         box(title = "Input Sample Data File", width = 3, status = "primary", solidHeader = TRUE, br(),
                                             fileInput("fileClinical2", "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(),
                                             checkboxInput("header1", "Header", TRUE), # Input: Checkbox if file has header
                                             fluidRow(column(5, radioButtons("sep1", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t")), # Input: Select separator 
                                                      column(7, radioButtons("quote1", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'))), tags$hr(), # Input: Select quotes 
                                             numericInput(inputId = "skipysample", label = "Number of Lines to Skip:", value = 4, min = 0, max = 10),  tags$hr(), # Input: Choose number of lines to skip
                                             
                                             h5(strong("Total Number of Clinical Variables:")), verbatimTextOutput('TotalC1', placeholder = T), # Space to display number of rows/columns
                                             h5(strong("Total Number of Patients:")), verbatimTextOutput('TotalR2', placeholder = T),
                                             tags$hr(),br(), actionButton("goButtonClin", "Show Preview", icon = icon("table"))),  # Action Button to display preview 
                                         
                                         box(title = "Preview" , width = 9, status = "primary", solidHeader = TRUE, height = "720px", withSpinner(DT::dataTableOutput("ClinicalS")), style = "height:675px; overflow-y: scroll;overflow-x: scroll;")),
                                
                                # View Merged Dataframe (Clinical Sample and  Patient Data)
                                tabPanel("All Clinical Data", box(title = "Preview of Merged Clinical Files" , width = 12, status = "primary", solidHeader = TRUE, height = "730px",
                                             DT::dataTableOutput("ClinicalAll"), style = "height:686px; overflow-y: scroll;overflow-x: scroll;")),
                                
                                # Input CNA Data Tab 
                                tabPanel("CNA Data",  box(title = "Input CNA File", status = "primary", width = 3, solidHeader = TRUE,
                                                          fileInput("fileCNA", "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),  tags$hr(),
                                                          fluidRow(column(5, checkboxInput("headerCNA", "Header", TRUE)), column(5, checkboxInput("NACNA", "Ignore NAs", FALSE))), # Input: Checkbox if file has header 
                                                          fluidRow(column(5, radioButtons("sepCNA", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t")),  # Input: Select separator 
                                                                   column(7, radioButtons("quoteCNA", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'))), tags$hr(),  # Input: Select quotes
                                                          numericInput(inputId = "skipyCNA", label = "Number of Lines to Skip:", value = 0, min = 0, max = 10),  numericInput(inputId = "StartCNA", label = "Indicate CNA Start Column:", value = 3), tags$hr(), # Input: Choose number of lines to skip
                                                          
                                                          h5(strong("Total Number of Columns:")), verbatimTextOutput('TotalCCNA', placeholder = T), # Space to display number of rows/columns
                                                          h5(strong("Total Number of Genes:")), verbatimTextOutput('TotalRCNA', placeholder = T),
                                                          tags$hr(), br(), actionButton("goButtonCNA", "Show Preview", icon = icon("table"))), # Action Button to display preview 
                                         
                                         box(title = "Preview", width = 9, status = "primary", solidHeader = TRUE, height = "720px", withSpinner(DT::dataTableOutput("CNA")), style = "height:675px; overflow-y: scroll;overflow-x: scroll;")),
                                
                                # Input Mutation Annotation Format (MAF) File
                                tabPanel("Mutation Data", 
                                         box(title = "Input MAF File", width = 3, status = "primary", solidHeader = TRUE,
                                             br(), fileInput("fileMAF", "Choose File:", multiple = TRUE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), tags$hr(),
                                             checkboxInput("headerMAF", "Header", TRUE), # Input: Checkbox if file has header
                                             fluidRow(column(5, radioButtons("sepMAF", "Separator", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = "\t")),  # Input: Select separator 
                                                      column(7, radioButtons("quoteMAF", "Quote", choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'))), tags$hr(), # Input: Select quotes
                                             numericInput(inputId = "skipyMAF", label = "Number of Lines to Skip:", value = 1, min = 0, max = 10), tags$hr(), # Input: Choose number of lines to skip
                                             
                                             h5(strong("Total Number of Columns:")), verbatimTextOutput('TotalCMAF', placeholder = T), # Space to display number of rows/columns
                                             h5(strong("Total Number of Mutations:")), verbatimTextOutput('TotalRMAF', placeholder = T),
                                             tags$hr(), br(), actionButton("goButtonMAF", "Show Preview", icon = icon("table"))), # Action Button to display preview 
                                         
                                         box(title = "Preview", width = 9, status = "primary", solidHeader = TRUE, height = "720px", withSpinner(DT::dataTableOutput("MAF"), type = 4), style = "height:675px; overflow-y: scroll;overflow-x: scroll;")))),
                         
                # Exploratory Table Tab: Space to explore uploaded data -> choose 5 columns to study 
                tabItem(tabName = "tables",
                                 tabBox(id = "tabset2", height = "800px", width = "1000px",
                                        tabPanel("All Clinical Data",  box(title = "Column Selection", width = 3, status = "primary", solidHeader = TRUE, # Box design
                                                     br(), selectInput("column1", "Select Column 1:", choices = ""), selectInput("column2", "Select Column 2:", choices = ""), selectInput("column3", "Select Column 3:", choices = ""),
                                                     selectInput("column4", "Select Column 4:", choices = ""), selectInput("column5", "Select Column 5:", choices = ""), # Select Input: Display desired columns 
                                                     br(), br(), br(), actionButton("goButton", "Show Table", icon = icon("table"))), 
                                                 box(title = "Data Table", height = "720px", width = 9, status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("Clinical_Table")),style = "height:675px; overflow-y: scroll;overflow-x: scroll;")), # Display Data Table 
                                        
                                        tabPanel("CNA Data", box(title =  "Column Selection", width = 3, status = "primary", solidHeader = TRUE, # Box design 
                                                     br(), selectInput("columnCNA", "Select Column 1:", choices = ""), selectInput("columnCNA1", "Select Column 2:", choices = ""),
                                                     selectInput("columnCNA2", "Select Column 3:", choices = ""), selectInput("columnCNA3", "Select Column 4:", choices = ""), # Select Input: Display desired columns 
                                                     selectInput("columnCNA4", "Select Column 5:", choices = ""), br(), br(), br(), actionButton("goButtonCNA1", "Show Table", icon = icon("table"))), 
                                                 box(title = "Data Table",  width = 9, height = "720px", status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("CNA_Table")), style = "height:675px; overflow-y: scroll;overflow-x: scroll;")), # Display Data Table 
                                        
                                        tabPanel("Mutation Data", box(title = "Column Selection", width = 3, status = "primary", solidHeader = TRUE, # Box design
                                                     br(), selectInput("columnMAF", "Select Column 1:", choices = ""), selectInput("columnMAF1", "Select Column 2:", choices = ""), selectInput("columnMAF2", "Select Column 3:", choices = ""),
                                                     selectInput("columnMAF3", "Select Column 4:", choices = ""), selectInput("columnMAF4", "Select Column 5:", choices = ""), # Select Input: Display desired columns 
                                                     br(), br(), br(), actionButton("goButtonMAF1", "Show Table", icon = icon("table"))), 
                                                 box(title = "Data Table", width = 9, height = "720px", status = "primary", solidHeader = TRUE, withSpinner(DT::dataTableOutput("MAF_Table")), style = "height:675px; overflow-y: scroll;overflow-x: scroll;")))), # Display Data Table
                             
                 # Recode and Subset Data -> OS and DSS recoding, indicate starting CNA and quartile value and subset based on vairiables 
                 tabItem(tabName = "Recode",
                         box(solidHeader = TRUE, width = 2, status = "primary", title = ("Select Survival Columns to Recode"), # Select Options for recoding survival, CNA calculations start column and number of splits in data desired
                             checkboxInput("calcOS", "Recode Survival Columns", FALSE), selectInput("columnOS", "Select OS Column:", choices = ""), selectInput("columnDSS", "Select DSS Column:", choices = ""), tags$hr(),
                             textInput("caption", "OS Event:", "DECEASED"), verbatimTextOutput("value"), textInput("captionDSS", "DSS Event:", "Died of Disease"), verbatimTextOutput("Died of Disease"), tags$hr(),
                             numericInput(inputId = "number", label = "Number of _tiles:", value = 4)),
                                                                                 
                         box(solidHeader = TRUE, width = 2, title = ("Data Subsetting"), status = "primary", # Select Variable to split data on, then select desired variable levels i.e. Varibale = PAM50, desired levels = LumA, LumB.
                             selectInput("Select1", "Subset Based on Variable:", choices = "PATIENT_ID"), selectInput("Variable1", label = "Choose Variable Level:", choices = "", selected = NULL, multiple = TRUE),
                             selectInput("Select2", "Subset Based on Variable:", choices = "PATIENT_ID"), selectInput("Variable2", label = "Choose Variable Level:", choices = "", selected = NULL, multiple = TRUE),
                             selectInput("Select3", "Subset Based on Variable:", choices = "PATIENT_ID"), selectInput("Variable3", label = "Choose Variable Level:", choices = "", selected = NULL, multiple = TRUE)),
                         box(solidHeader = TRUE, height = "390px", title = "Check Variable Levels:", width = 8, status = "primary", style = "height:330px; overflow-y: scroll;", withSpinner(verbatimTextOutput("TableLevels"))),
                         
                         tabBox(id = "tabset3", height = "420px", width = 8,
                             tabPanel("Check Variable Types:", withSpinner(verbatimTextOutput("TableType")), style = "height:370px; overflow-y: scroll;"), # Display the types of variables present in the data 
                             tabPanel("Check Recoding and CNA Calculations:", withSpinner(DT::dataTableOutput("TableRecode")), style = "height:370px; overflow-y: scroll;overflow-x: scroll;"), # Make sure survival recoding has been implemented 
                             tabPanel("Check Subsetted Data:", withSpinner(DT::dataTableOutput("TableSubset")), style = "height:370px; overflow-y: scroll;overflow-x: scroll;"))), # Make sure subsetting has been implemented effectively
                        
                # Data Prep: Make sure columns are in right format (numeric/factor)
                tabItem(tabName = "FactorLevels", box(height = "380px", solidHeader = TRUE, width = 12, status = "primary", title = ("Select Numeric Variables:"), fluidPage(tweaks_Levels, controls_Num)),
                        box(height = "380px", solidHeader = TRUE, width = 12, status = "primary", title = ("Select Categorical Variables:"), fluidPage(tweaks_Levels, controls_Cat))),
                 
                # Exploratory Plots:  1) Boxplot
                tabItem(tabName = "boxplot", box(width = 2, status = "primary", title = ("Select Variables of Interest"), solidHeader = T,
                             selectInput("BoxVariable1", "Select Variable (x):", choices = ""), selectInput("BoxVariable2", "Select Variable (y):", choices = ""), # X and Y variable selection
                             checkboxInput("legend", "Display Legend", TRUE), checkboxInput("sampsize", "Boxplot by Sample Size", FALSE), checkboxInput("NAview", "Display NA Values", FALSE)), # Options for boxplot aesthetics 
                         box(title = "Boxplot", solidHeader = T, width = 10, height = "650px", status = "primary", withSpinner(plotOutput("plot3", height = "590px")))), 
                         
                 # 2) Scatterplots 
                 tabItem(tabName = "scatterplot", box(title = ("Select Variables of Interest"), width = 2, status = "primary", solidHeader = TRUE, 
                             selectInput("scattervar1", "Select Variable (x):", choices = ""), selectInput("scattervar2", "Select Variable (y):", choices = ""), selectInput("scattervar3", "Select Variable (colour):", choices = ""), # X and Y variable selection
                             checkboxInput("legend1", "Display Legend", TRUE), checkboxInput("NAview1", "Display NA Values", FALSE)),  # Options for scatterplot aesthetics 
                         box(title = ("Scatterplots"), solidHeader = T, width = 10, height= "650px", status = "primary", withSpinner(plotOutput("Scatterplot1", height = "590px")))),
                 
                 # 3) Barplots
                 tabItem(tabName = "Explor",box(title = ("Select Variables of Interest"), width = 2, status = "primary", solidHeader = TRUE, selectInput("First__Variable", "Select Variable (x):", choices = ""), # X and Y variable selection
                             selectInput("Second__Variable", "Select Variable (y):", choices = ""), checkboxInput("legend2", "Display Legend", TRUE), checkboxInput("NAview2", "Display NA Values", FALSE)),  # Options for scatterplot aesthetics 
                         box(title = ("Barplots"), solidHeader = T, width = 10, height= "650px", status = "primary", withSpinner(plotOutput("Association3", height = "590px")))),
                 
                 # 4) Density plot of CNA scores (3 options -> Plain density plot, segmented density plot and faceted density plots)
                 tabItem(tabName = "Dist", 
                         box(title = ("Select Options"), solidHeader = T, width = 2, status = "primary", selectInput("columnDist", "Select Continuous Variable:", choices = ""), 
                             sliderInput("bins", "Number of Bins:", min = 1000, max = 2000, value = 1350), tags$hr(), selectInput("ConPanel", "Select Plot Type:", c(Plain = "Plain", Segmented = "Segmented", Facetwrap = "Facet_Wrap")), 
                             selectInput("columnDist1", "Select Discrete Variable:", choices=""), tags$hr(),  checkboxInput("legend3", "Display Legend", TRUE), checkboxInput("NAview3", "Display NA Values", FALSE), numericInput(inputId = "number1", label = "Number of _tiles:", value = 4)), # Options
                         
                         tabBox(width = 10, height = "650px", 
                                tabPanel(title = "Histogram", width = 10, status = "primary", br(), plotOutput("CNAHist", height = "590px")),
                                tabPanel(title = "Density Plot", 
                                         conditionalPanel(condition = "input.ConPanel == 'Plain'", width = 10, br(), withSpinner(plotOutput("CNADist", height = "590px"))), # Plain Density Plot
                                         conditionalPanel(condition = "input.ConPanel == 'Segmented'",  width = 10, br(), withSpinner(plotOutput("CNADist1", height="590px"))), # Segmented Density Plot 
                                         conditionalPanel(condition = "input.ConPanel == 'Facet_Wrap'", width = 10, br(), withSpinner(plotOutput("CNADist2", height = "650px")))))), # Faceted Density Plot
                 
                 # Survival Analysis KM plots -> 1) KM Survival curves and log rank tests (Clinical Variables)
                 tabItem(tabName = "KMplot",
                         fluidRow(box(width = 2, status = "primary", solidHeader = TRUE, title = ("Select Survival Variables"),
                             selectInput("Time1", "Survival Time Column:", choices = ""), selectInput("Event1", "Event Status Column:", choices = ""), tags$hr(),
                             selectInput("Variable", "Select Clinical Variable:", choices = ""), tags$hr(), # Select survival variables i.e. time to event, event status Aand variable of interest
                             checkboxInput("confI", "Display CI", FALSE), checkboxInput("riskT", "Display RT", TRUE), # KM Plot options 
                             selectInput("legendpos", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right")), 
                         
                         box(title = ("Kaplan-Meier Plot of Clinical Variables"), solidHeader=T, width = 10, height = "560px", status = "primary", withSpinner(plotOutput("KM1", height = "505px"))), # KM Curve
                         box(title = ("Log Rank Test"),  solidHeader = T, br(), width = 12, status = "primary", withSpinner(verbatimTextOutput("KMlogrank"))))), # Logrank Test
                 
                 # 2) Segmented (Quartiled) Survival Plots  
                 tabItem(tabName = "KMOver",  fluidRow(box(title = ("Select Survival Variables"), width = 2, status = "primary", solidHeader = T,
                         selectInput("Time11", "Survival Time Column:", choices = ""), selectInput("Event11", "Event Status Column:", choices = ""),  tags$hr(), selectInput("Variable11", "Select Clinical Variable:", choices = ""), tags$hr(), # Variables of Interest
                         checkboxInput("confI1", "Display CI", FALSE), checkboxInput("riskT1", "Display RT", TRUE), # Options: Display confidance intervale, risk table and legend.
                         selectInput("legendpos1", "Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right")),
                         
                         box(title = ("Survival Plot Based on CNA Scores"), width = 10, status = "primary", solidHeader = T,  height = "535px", withSpinner(plotOutput("PercentSurv", height="480px"))), # KM Curve
                         box(title = ("Log Rank Test"),width = 12, status = "primary", solidHeader = T, withSpinner(verbatimTextOutput("KMlogrank1"))))), # Logrank Test

                # 3) Survival Analysis- Split on Specific Variable (Allows comparison between groups (only binary outcomes))
                tabItem(tabName = "KMplotRadio", fluidPage(fluidRow(box(title = "Select Survival Variables", width = 12, status = "primary", solidHeader = TRUE, 
                        column(2, selectInput("TimeTreat", "Survival Time Column:", choices = "")), column(2, selectInput("EventTreat", "Event Status Column:", choices = "")),
                        column(2, selectInput("VariableTreat", "Select Clinical Variable:", choices = "")), column(2, selectInput("SplittingVariable", "Select Treatment Variable:", choices = "")), 
                        column(2, selectInput("legendposyes", "Select Legend Position:", choices = c("top", "bottom", "left", "right", "none"), selected = "right")), # Selection Option
                        column(2, checkboxInput("confIyes", "Display CI", FALSE), checkboxInput("riskTyes", "Display RT", TRUE))), # KM Plot options  
                        
                        box(title = ("Kaplan-Meier Plot Treatment - Yes"), solidHeader=T, width = 12, height = "500px", status = "primary", withSpinner(plotOutput("KMR1", height = "445px"))), # KM Plot Group 1 
                        box(title = ("Kaplan-Meier Plot Treatment - No"), solidHeader=T, width = 12, height = "500px", status = "primary", withSpinner(plotOutput("KMR2", height =  "445px"))), # KM Plot Group 2
                        box(title = ("Log Rank Test Treatment - Yes"), solidHeader=T, width = 6, status = "primary", withSpinner(verbatimTextOutput("KMlogrankYes"))), # LRT Group 1
                        box(title = ("Log Rank Test Treatment - No"), solidHeader=T, width = 6, status = "primary", withSpinner(verbatimTextOutput("KMlogrankNo")))))),  #LRT Group 2
                        
            # Univariate and Multivariate Cox Tab with Assumptions: 1) Univariate OS and DSS models with variable selection 
                tabItem(tabName = "UniVar", fluidRow(box(title = "Select Clinical Variables", width = 12, status = "primary", solidHeader = T, fluidPage(tweaks_Cox, controls_Uni)), # Variable selection for model
                        box(title = ("Variable Selection"), solidHeader = T,  width = 2, status = "primary", selectInput("Timecox1", "Time to Event:", choices = ""), selectInput("OSEventcox1", "OS Event Status:", choices = ""), selectInput("DSSEventcox1", "DSS Event Status:", choices = ""), tags$hr(), checkboxInput("terms", "Display by Covariate", TRUE)), # Survival options
                        box(title = ("Univariate Cox Models for OS"), solidHeader = T,  width = 5, status = "primary", withSpinner(verbatimTextOutput("UniCoxOS", placeholder = T)), h5(strong("Wald Test P-value:")), verbatimTextOutput("UniCoxOS1", placeholder = T), h5(strong("LRT P-value:")), verbatimTextOutput("UniCoxOS2", placeholder = T)), # Cox models OS
                        box(title = ("Univariate Cox Models for DSS"), solidHeader = T, width = 5, status = "primary", withSpinner(verbatimTextOutput("UniCoxDSS", placeholder = T)), h5(strong("Wald Test P-value:")), verbatimTextOutput("UniCoxDSS1", placeholder = T), h5(strong("LRT P-value:")), verbatimTextOutput("UniCoxDSS2", placeholder = T)))), # Cox models DSS
                
                # 2) Multivariate Cox
                tabItem(tabName = "MultiVar", fluidRow(box(title = ("Select Clinical Variables"), width = 12, status = "primary", solidHeader = T, fluidPage(tweaks_Cox, controls_Multi)),
                        box(title = ("Multivariate Cox Models for OS"), width = 6, solidHeader = T, status = "primary", withSpinner(verbatimTextOutput("MultiCoxOS", placeholder = T)), h5(strong("Wald Test P-value:")), verbatimTextOutput("MultiCoxOS1", placeholder = T), h5(strong("LRT P-value:")), verbatimTextOutput("MultiCoxOS2", placeholder = T)),
                        box(title = ("Multivariate Cox Models for DSS"), width = 6, solidHeader = T, status = "primary", withSpinner(verbatimTextOutput("MultiCoxDSS", placeholder = T)), h5(strong("Wald Test P-value:")), verbatimTextOutput("MultiCoxDSS1", placeholder = T), h5(strong("LRT P-value:")), verbatimTextOutput("MultiCoxDSS2", placeholder = T)))),
                
                # 3) Model  Assumptions 
                tabItem(tabName = "AssumptionsOS", fluidRow(box(title = ("OS Cox Models Assumptions"), solidHeader = T, width = 12, status = "primary", height = "1200px",  withSpinner(plotOutput("AssumptionsCoxOS"))))),
                tabItem(tabName = "AssumptionsDSS", fluidRow(box(title = ("DSS Cox Models Assumptions"), solidHeader = T, width = 12, status = "primary", height = "1200px", withSpinner(plotOutput("AssumptionsCoxDSS"))))),
                
                # Association Tests: Fishers Exact Test, Chi-Squared, ANOVA and KW along with relevant assumptions 
                tabItem(tabName = "ASTest", fluidPage(fluidRow(box(title = ("Select Clinical Variables"), width = 12, status = "primary", solidHeader = TRUE, 
                        column(3,selectInput("CategoricalV1", "Select Categorical Variable 1:", choices = "")), column(3,selectInput("CategoricalV2", "Select Categorical Variable 2:", choices = "")), # Conditional Panel to display appropriate association tests 
                        column(3,selectInput("ContinuousV1", "Select Continuous Variable:", choices = "")), column(3,selectInput("plotType", "Data Type:", c(Categorical = "Categorical", Categorical_Continuous = "Both", Pairwise = "Pairwise")))))),
                        
                        conditionalPanel(condition = "input.plotType == 'Categorical'", box(title = ("Chi-Square Test"), solidHeader = T, width = 4, status = "primary", verbatimTextOutput("Cat1")),
                                         box(title = ("Fishers Exact Test"), solidHeader = T,  width = 4, status = "primary", verbatimTextOutput("Cat3")), box(title = ("Simulated Fishers Exact Test"), solidHeader = T, width = 4, status = "primary", verbatimTextOutput("Cat2"))),
                        conditionalPanel(condition = "input.plotType == 'Both'", box(title = "ANOVA Assumptions", width=6, status = "primary", solidHeader = T, h4("Test for Equal Variance 1"), verbatimTextOutput("ANOVAAss1"), h4("Test for Equal Variance 2"), verbatimTextOutput("ANOVAAss2"), h4("Test for Normality"), verbatimTextOutput("ANOVAAss3")), 
                                         box(title = ("ANOVA or Kruskal-Wallis Test"), width = 6, status = "primary", solidHeader = T, h4("ANOVA Test"), verbatimTextOutput("ANOVA"), h4("Kruskal-Wallis Test"), verbatimTextOutput("KW"))),
                        conditionalPanel(condition = "input.plotType == 'Pairwise'", box(title = ("Pairwise Comparisons: t-test"), width = 6, status = "primary", solidHeader = T, verbatimTextOutput("PC")), box(title = ("Pairwise Comparisons: Dunn's Test"), width = 6, status = "primary", solidHeader = T, verbatimTextOutput("Dunn")))),
               
                # MAF Tabs: For exploration of mutation annotation format files 
                tabItem(tabName = "MAFText", tabBox(id = "tabset3", height = "800px", width = 12, # MAF summaries (Text)
                                                    tabPanel("MAF Summary", width = 12, status = "primary", solidHeader = TRUE, verbatimTextOutput("MAF1")), tabPanel("Sample Summary", width = 12, status = "primary", solidHeader = TRUE, verbatimTextOutput("MAF2")), 
                                                    tabPanel("Gene Summary", width = 12, status = "primary", solidHeader = TRUE, verbatimTextOutput("MAF3")), tabPanel("All Fields Summary", width = 12, status = "primary", solidHeader = TRUE, verbatimTextOutput("MAF4")))),
             
                tabItem(tabName = "MAFVis", fluidRow(tabBox(width = 12, id = "tabsetMAF1", height = "1900px", # MAF summaries (Visual Plots)
                                                   tabPanel("MafSummary", box(title=("MafSummary Plots"), width = 12, solidHeader = T, status = "primary", plotOutput("summaryMAF", height = "500px"))),
                                                   tabPanel("OncoPlot/OncoStrip", box(title = "OncoPlot", width = 12, solidHeader = T, status = "primary", plotOutput("oncoplotMAF", height = "500px")),
                                                            box(title = "OncoStrip Of Selected Genes", width  = 12, solidHeader = T, status = "primary", plotOutput("oncostripMAF", height = "480px")),
                                                            box(title = "Transition and Transversions", width = 12, solidHeader = T, status = "primary", plotOutput("TandT", height = "480px"))),
                                                   tabPanel("Lollipop Plots", fluidRow(box(title = ("Choose Genes to Analyse"), width = 12, status = "primary",  solidHeader = TRUE,
                                                        column(3, numericInput(inputId = "MAFgenecol", label = "Gene Name Column:", value = 1)), column(3,selectInput("genename1lol", "Gene of Interest 1:", choices = "")),
                                                        column(3, selectInput("genename2lol", "Gene of Interest 2:", choices = "")), column(3,selectInput("genename3lol", "Gene of Interest 3:", choices = ""))),
                                                        box(title = "Lollipop Plot 1", width = 12, solidHeader = T, status = "primary", plotOutput("lol1", height = "480px")),
                                                        box(title = "Lollipop Plot 2", width  = 12, solidHeader = T, status = "primary", plotOutput("lol2", height = "480px")),
                                                        box(title = "Lollipop Plot 3", width = 12, solidHeader = T, status = "primary", plotOutput("lol3", height = "480px")))),
                        # Other Plots  
                        tabPanel("Other Plots", id = "tabsetMAFother", box(title = "Mutation Load Plot", width = 12, solidHeader = T, status = "primary", plotOutput("Mutload", height = "500px")),
                         box(title = "Somatic Interaction Plot", width  = 12, solidHeader = T, status = "primary", plotOutput("VAF1", height = "500px")), 
                         box(title = "Genecloud", width = 12, solidHeader = T, status = "primary", plotOutput("Gcloud", height = "500px")))))
        ))))
                 


# Server:
server <- function(input, output, session) {
# Input Files server -> Clinical patient file 
dataInputClinicalP <- reactive({req(input$fileClinical1, cancelOutput = FALSE)  # Input Clinical Files: can specify header,sep, quote and number of lines to skip (default = 4)
    df <- read.csv(input$fileClinical1$datapath, header = input$header, sep = input$sep, quote = input$quote, na.strings=c(""," ","NA"), skip = input$skipypatient)})

output$ClinicalP <- renderDataTable({input$goButtonClin 
    isolate((dataInputClinicalP()))}, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30))

output$TotalC<-renderPrint({ncol(dataInputClinicalP())})
output$TotalR<-renderPrint({nrow(dataInputClinicalP())})

# Clinical File 2 -> Sample file Tab
dataInputClinicalS <- reactive({req(input$fileClinical2, cancelOutput = FALSE)  # Input Clinical Files: can specify header,sep, quote and number of lines to skip (default = 4)
    df <- read.csv(input$fileClinical2$datapath, header = input$header1, sep = input$sep1, quote = input$quote1, na.strings=c(""," ","NA"), skip = input$skipysample)})

output$ClinicalS <- renderDataTable({input$goButtonClin
    isolate((dataInputClinicalS()))}, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30))

output$TotalC1<-renderPrint({ncol(dataInputClinicalS())})
output$TotalR2<-renderPrint({nrow(dataInputClinicalS())})

# Merge two of them (patient and sample -> clinical data) -> Make sure PATIENT ID column exists 
dataClinical <- reactive({if(is.null(input$fileClinical2)) {return(dataInputClinicalP())
        } else if (is.null(input$fileClinical1)){return(dataInputClinicalS())
        } else {x <- merge(dataInputClinicalP(), dataInputClinicalS(), by.x = "PATIENT_ID", by.y="PATIENT_ID")
        return(x)}})

output$ClinicalAll <- renderDataTable(dataClinical(), options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30))

# 3) CNA File Tab 
dataInputCNA <- reactive({req(input$fileCNA) # Input CNA Files: can specify header,sep, quote and number of lines to skip (default = 0)
    df <- read.csv(input$fileCNA$datapath, header = input$headerCNA, sep = input$sepCNA, quote = input$quoteCNA, check.names = F, na.strings=c(""," ","NA"), skip=input$skipyCNA)})

output$CNA = renderDataTable({input$goButtonCNA
    isolate((dataInputCNA()))}, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30))

output$TotalCCNA<-renderPrint({ncol(dataInputCNA())})
output$TotalRCNA<-renderPrint({nrow(dataInputCNA())})

# 4) MAF File Tab 
dataInputMAF <- reactive({req(input$fileMAF) # Input CNA Files: can specify header,sep, quote and number of lines to skip (default = 1)
    df <- read.csv(input$fileMAF$datapath, header = input$headerMAF, sep = input$sepMAF, quote = input$quoteMAF, na.strings=c(""," ","NA"), skip=input$skipyMAF)})

output$MAF = renderDataTable({input$goButtonMAF
    isolate((dataInputMAF()))}, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30))

output$TotalCMAF<-renderPrint({ncol(dataInputMAF())})
output$TotalRMAF<-renderPrint({nrow(dataInputMAF())})

## Tab 2: 1) Comparison Tables -> Output of comaprison tables 
observe({vchoices <- names(dataClinical())
updateSelectInput(session, "column1", choices = vchoices, selected = vchoices[1])
updateSelectInput(session, "column2", choices = vchoices, selected = vchoices[2])
updateSelectInput(session, "column3", choices = vchoices, selected = vchoices[3])
updateSelectInput(session, "column4", choices = vchoices, selected = vchoices[4]) 
updateSelectInput(session, "column5", choices = vchoices, selected = vchoices[5])})

output$Clinical_Table = renderDataTable({input$goButton  # renderDataTable is reactive to a change in the input data or the selected columns
    isolate((dataClinical()[,c(input$column1, input$column2, input$column3, input$column4, input$column5)]))}, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30))

# 2) Table CNA 
observe({vchoicesCNA <- names(dataInputCNA())
updateSelectInput(session, "columnCNA", choices = vchoicesCNA, selected = vchoicesCNA[2])
updateSelectInput(session, "columnCNA1", choices = vchoicesCNA, selected = vchoicesCNA[4])
updateSelectInput(session, "columnCNA2", choices = vchoicesCNA, selected = vchoicesCNA[5])
updateSelectInput(session, "columnCNA3", choices = vchoicesCNA, selected = vchoicesCNA[6])
updateSelectInput(session, "columnCNA4", choices = vchoicesCNA, selected = vchoicesCNA[7])})

output$CNA_Table = renderDataTable({input$goButtonCNA1
    isolate((dataInputCNA()[,c(input$columnCNA, input$columnCNA1, input$columnCNA2, input$columnCNA3, input$columnCNA4)]))}, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30))

# 3) Table - Mutation (MAF)
observe({vchoicesMAF <- names(dataInputMAF())
updateSelectInput(session, "columnMAF", choices = vchoicesMAF, selected = vchoicesMAF[1])
updateSelectInput(session, "columnMAF1", choices = vchoicesMAF, selected = vchoicesMAF[2])
updateSelectInput(session, "columnMAF2", choices = vchoicesMAF, selected = vchoicesMAF[3])
updateSelectInput(session, "columnMAF3", choices = vchoicesMAF, selected = vchoicesMAF[4])
updateSelectInput(session, "columnMAF4", choices = vchoicesMAF, selected = vchoicesMAF[5])})

output$MAF_Table = renderDataTable({input$goButtonMAF1
    isolate((dataInputMAF()[,c(input$columnMAF, input$columnMAF1, input$columnMAF2, input$columnMAF3, input$columnMAF4)]))}, options = list(lengthMenu = c(10, 30, 50, 100), pageLength = 30))

# Tab 3: 1) CNA Score calculation (behind the scenes) -> Calculates CNA Score, Amp, Del etc. 
CNA_Metrics1 <- reactive({
    if(is.null(input$fileClinical2) & is.null(input$fileClinical1)){
        # CNA Score CCA
        PATIENT_ID <- colnames(dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())]) 
        Scores <- as.data.frame(PATIENT_ID)
        Scores$CNA_Score <- colSums(abs(dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())]), na.rm = input$NACNA)
        Scores <- na.omit(Scores) # remove NAs (Could have All and CCA)
        
        # CNA Score Amp and Del for Each Patient CCA (Apply)
        Scores$Amp_Score <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x[x > 0], na.rm= input$NACNA)))
        Scores$Del_Score <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x[x < 0], na.rm= input$NACNA)))
        Scores$Del_Score <- abs(Scores$Del_Score)
        
        # CNA Burden Calculations
        Scores$Altered_Genes <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x != 0, na.rm= input$NACNA)))
        Scores <- Scores %>% mutate(CNA_Burden = (Altered_Genes/nrow(dataInputCNA()))*100)
        
        # Amp Burden, Del Burden, No Change
        Scores$Amp_Genes <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x > 0, na.rm= input$NACNA)))
        Scores$Del_Genes <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x < 0, na.rm= input$NACNA)))
        Scores <- Scores %>% mutate(Amplification_Burden = (Amp_Genes/nrow(dataInputCNA()))*100, Deletion_Burden = (Del_Genes/nrow(dataInputCNA()))*100)
        
       # Scores$Score_Quartile <- split_quantile(x=Scores$CNA_Score, type = input$number) # Segment Scores 
        #Scores$Burden_Quartile <- split_quantile(x=Scores$CNA_Burden, type = input$number)
        
        a_ranks <- rank(Scores$CNA_Score, ties.method = "first")
        a_ranks1 <- rank(Scores$CNA_Burden, ties.method = "first")
        Scores$Score_Quartile <- cut(a_ranks, unique(quantile(a_ranks, probs=0:input$number/input$number)), include.lowest=TRUE, labels=FALSE) 
        Scores$Burden_Quartile <- cut(a_ranks1, unique(quantile(a_ranks1, probs=0:input$number/input$number)), include.lowest=TRUE, labels=FALSE) 
        Scores$Score_Quartile <- as.factor(Scores$Score_Quartile)
        Scores$Burden_Quartile <- as.factor(Scores$Burden_Quartile)

        CNA_Metrics <- Scores %>% select(c("PATIENT_ID", "CNA_Score", "Amp_Score", "Del_Score", "Altered_Genes", "CNA_Burden", "Amp_Genes","Del_Genes","Amplification_Burden","Deletion_Burden", "Score_Quartile", "Burden_Quartile"))
        return(CNA_Metrics)}
        else if (is.null(input$fileCNA)){return(dataClinical())
        } else {
        # CNA Score CCA
        PATIENT_ID <- colnames(dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())]) 
        Scores <- as.data.frame(PATIENT_ID)
        Scores$CNA_Score <- colSums(abs(dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())]), na.rm= input$NACNA)
        Scores <- na.omit(Scores) # remove NAs (Could have All and CCa)
        
        # CNA Score Amp and Del for Each Patient CCA (Apply)
        Scores$Amp_Score <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x[x > 0], na.rm= input$NACNA)))
        Scores$Del_Score <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x[x < 0], na.rm= input$NACNA)))
        Scores$Del_Score <- abs(Scores$Del_Score)
        
        # CNA Burden Calculations 
        Scores$Altered_Genes <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x != 0, na.rm= input$NACNA)))
        Scores <- Scores %>% mutate(CNA_Burden = (Altered_Genes/nrow(dataInputCNA()))*100)
        
        # Amp Burden, Del Burden, No Change
        Scores$Amp_Genes <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x > 0, na.rm= input$NACNA)))
        Scores$Del_Genes <- na.omit(apply(X = dataInputCNA()[,input$StartCNA:ncol(dataInputCNA())], MARGIN = 2, function(x) sum(x < 0, na.rm= input$NACNA)))
        Scores <- Scores %>% mutate(Amplification_Burden = (Amp_Genes/nrow(dataInputCNA()))*100, Deletion_Burden = (Del_Genes/nrow(dataInputCNA()))*100)
        
        CNA_Metrics <- Scores %>% select(c("PATIENT_ID", "CNA_Score", "Amp_Score", "Del_Score", "Altered_Genes", "CNA_Burden", "Amp_Genes","Del_Genes","Amplification_Burden","Deletion_Burden"))
        CNA_Metrics_All <- merge(dataClinical(), CNA_Metrics, by.x = "PATIENT_ID", by.y = "PATIENT_ID") # Make sure both files have PATIENT_ID 
        
        a_ranks <- rank(CNA_Metrics_All$CNA_Score, ties.method = "first")
        a_ranks1 <- rank(CNA_Metrics_All$CNA_Burden, ties.method = "first")
        CNA_Metrics_All$Score_Quartile <- cut(a_ranks, unique(quantile(a_ranks, probs=0:input$number/input$number)), include.lowest=TRUE, labels=FALSE) 
        CNA_Metrics_All$Burden_Quartile <- cut(a_ranks1, unique(quantile(a_ranks1, probs=0:input$number/input$number)), include.lowest=TRUE, labels=FALSE) 
        CNA_Metrics_All$Score_Quartile <- as.factor(CNA_Metrics_All$Score_Quartile)
        CNA_Metrics_All$Burden_Quartile <- as.factor(CNA_Metrics_All$Burden_Quartile)
        
        return(CNA_Metrics_All)}
})

# Check Recoding and CNA calculations and carry out survival recoding
# 1) Carry out Survival recoding
observe({vchoicesRecode <- names(CNA_Metrics1())
updateSelectInput(session, "columnOS", choices = vchoicesRecode, selected = vchoicesRecode[14])
updateSelectInput(session, "columnDSS", choices = vchoicesRecode, selected = vchoicesRecode[17])})

dataClinicalSurv <- reactive({if(input$calcOS == TRUE){Surv_Data = CNA_Metrics1()
        Surv_Data$OS <- ifelse(Surv_Data[,input$columnOS] == input$caption, 1, 0)
        Surv_Data$DSS <- ifelse(Surv_Data[,input$columnDSS] == input$captionDSS, 1, 0) 
        return(Surv_Data)} else {return(CNA_Metrics1())}})

# 2) Subset data based on variable factor levels
observe({vchoicesSub <- names(dataClinicalSurv())
updateSelectInput(session, "Select1", choices = vchoicesSub, selected = vchoicesSub[4])
updateSelectInput(session, "Select2", choices = vchoicesSub, selected = vchoicesSub[5])
updateSelectInput(session, "Select3", choices = vchoicesSub, selected = vchoicesSub[7])})

observe({vchoicesSub1 <- c(levels(dataClinicalSurv()[,input$Select1]), "None Selected")
updateSelectInput(session, "Variable1", choices = vchoicesSub1, selected = "None Selected")})

observe({vchoicesSub2 <- c(levels(dataClinicalSurv()[,input$Select2]), "None Selected")
updateSelectInput(session, "Variable2", choices = vchoicesSub2, selected = "None Selected")})

observe({vchoicesSub3 <- c(levels(dataClinicalSurv()[,input$Select3]), "None Selected")
updateSelectInput(session, "Variable3", choices = vchoicesSub3, selected = "None Selected")})

CNA_Metrics_Sub1  <- reactive({if(input$Variable1 == "None Selected" & input$Variable2 == "None Selected" & input$Variable3 == "None Selected"){
        return(dataClinicalSurv())
    } else if (input$Variable1 != "None Selected" & input$Variable2 == "None Selected" & input$Variable3 == "None Selected"){ 
        Subset <- filter(dataClinicalSurv(), dataClinicalSurv()[,input$Select1] %in% input$Variable1)
        Subset[,input$Select1] <- as.factor(as.character(Subset[,input$Select1]))
        return(Subset)
    } else if (input$Variable1 == "None Selected" & input$Variable2 != "None Selected" & input$Variable3 == "None Selected"){
        Subset <- filter(dataClinicalSurv(), dataClinicalSurv()[,input$Select2] %in% input$Variable2)
        return(Subset)
    } else if (input$Variable1 == "None Selected" & input$Variable2 == "None Selected" & input$Variable3 != "None Selected"){
        Subset <- filter(dataClinicalSurv(), dataClinicalSurv()[,input$Select3] %in% input$Variable3)
        return(Subset)
    } else if (input$Variable1 != "None Selected" & input$Variable2 != "None Selected" & input$Variable3 == "None Selected"){
        Subset <- filter(dataClinicalSurv(), dataClinicalSurv()[,input$Select1] %in% input$Variable1, dataClinicalSurv()[,input$Select2] %in% input$Variable2)
        return(Subset)
    } else if (input$Variable1 != "None Selected" & input$Variable2 == "None Selected" & input$Variable3 != "None Selected"){
        Subset <- filter(dataClinicalSurv(), dataClinicalSurv()[,input$Select1] %in% input$Variable1, dataClinicalSurv()[,input$Select3] %in% input$Variable3)
        return(Subset)
    } else if (input$Variable1 == "None Selected" & input$Variable2 != "None Selected" & input$Variable3 != "None Selected"){
        Subset <- filter(dataClinicalSurv(), dataClinicalSurv()[,input$Select2] %in% input$Variable2, dataClinicalSurv()[,input$Select3] %in% input$Variable3)
        return(Subset)
    } else {
        Subset <- filter(dataClinicalSurv(), dataClinicalSurv()[,input$Select1] %in% input$Variable1, dataClinicalSurv()[,input$Select2] %in% input$Variable2, dataClinicalSurv()[,input$Select3] %in% input$Variable3)
        return(Subset)} })

# Carry out another segmentation of data -> Quartile after subsetitng
CNA_Metrics_Sub2<- reactive({
    if(is.null(input$fileCNA)){return(CNA_Metrics_Sub1())} 
    else {subset <- CNA_Metrics_Sub1()
    a_ranks <- rank(subset$CNA_Score, ties.method = "first")
    a_ranks1 <- rank(subset$CNA_Burden, ties.method = "first")
    subset$Score_Quartile_Subset <- cut(a_ranks, quantile(a_ranks, probs=0:input$number/input$number), include.lowest=TRUE, labels=FALSE) 
    subset$Burden_Quartile_Subset <- cut(a_ranks1, quantile(a_ranks1, probs=0:input$number/input$number), include.lowest=TRUE, labels=FALSE) 
    subset$Score_Quartile_Subset <- as.factor(subset$Score_Quartile_Subset)
    subset$Burden_Quartile_Subset <- as.factor(subset$Burden_Quartile_Subset)
    return(subset) } })

# Recode numeric/categorical varibales 
observe({vchoicesCheckNum <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateCheckboxGroupInput(session, "show_vars3", choices = vchoicesCheckNum)
updateCheckboxGroupInput(session, "show_vars4", choices = vchoicesCheckNum)})

CNA_Metrics_Sub <- reactive({ x <- CNA_Metrics_Sub2()
    cols <- c(input$show_vars3)
    cols1 <- c(input$show_vars4)
    x %<>% mutate_at(cols, funs(as.numeric(.)))  # Convert columns to numeric 
    x %<>% mutate_at(cols1, funs(factor(.))) # Convert columns to factor
    return(x) })

# Check Levels - No NA factor levels and all right 'Type' etc. 
output$TableLevels = renderPrint({sapply(CNA_Metrics_Sub(), levels)})
output$TableType = renderPrint({str(CNA_Metrics_Sub())})
output$TableRecode = renderDataTable({CNA_Metrics_Sub()})
output$TableSubset = renderDataTable({CNA_Metrics_Sub()})

# Exploratory Plots:  (1. Boxplot, 2. Barplot, 3. Histogram, 4. Density Plot) 
# Box plots 
observe({vchoicesbox1 <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "BoxVariable1", choices = vchoicesbox1, selected = vchoicesbox1[15])
updateSelectInput(session, "BoxVariable2", choices = vchoicesbox1, selected = vchoicesbox1[33])})

give.n <- function(x){return(c(y = median(x)*1.05, label = length(x)))}     # experiment with the multiplier to find the perfect position

output$plot3 <- renderPlot({ggplot(data = CNA_Metrics_Sub(), mapping = aes(x = CNA_Metrics_Sub()[,input$BoxVariable1], y = CNA_Metrics_Sub()[,input$BoxVariable2], color = CNA_Metrics_Sub()[,input$BoxVariable1])) +
    geom_boxplot(varwidth = input$sampsize, na.rm = TRUE, aes(fill = CNA_Metrics_Sub()[,input$BoxVariable1]), alpha = 0.35, show.legend = input$legend) + geom_jitter(alpha = 0.3) + ggtitle("Boxplot of Clinical Variables and Scores") + 
    ylab("Clinical Variable/Score 1") + xlab("Clinical Variable/Score 2") + theme(plot.title = element_text(hjust = 0.5, size =15)) + theme(axis.title.x = element_text(hjust = 0.5, size=15)) + theme(axis.title.y = element_text(hjust = 0.5, size=15)) + 
    theme(axis.text.x=element_text(size=14)) +  theme(axis.text.y=element_text(size=14)) + theme(legend.title = element_text(colour="black", size=15, face="bold")) + theme(strip.text = element_text(size=15)) + theme(legend.text=element_text(size=15)) + 
    stat_summary(fun.data = give.n, geom = "text", fun = median, col = "black", size=4) + labs(fill="Variable Levels") + scale_color_discrete(guide = FALSE) + scale_x_discrete(na.translate = input$NAview) })

# Scatterplots 
observe({vchoicesASS <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "scattervar1", choices = vchoicesASS, selected = vchoicesASS[34])
updateSelectInput(session, "scattervar2", choices = vchoicesASS, selected = vchoicesASS[35]) 
updateSelectInput(session, "scattervar3", choices = vchoicesASS, selected = vchoicesASS[15]) })

output$Scatterplot1 <- renderPlot({ggplot(CNA_Metrics_Sub()) + geom_point(aes(x=CNA_Metrics_Sub()[,input$scattervar1], y=CNA_Metrics_Sub()[,input$scattervar2], colour = CNA_Metrics_Sub()[,input$scattervar3]), size = 1.5, show.legend = input$legend1) + 
ggtitle("Scatterplot of Clinical Variables and Scores") + ylab("Clinical Variable/Score (y)") + xlab("Clinical Variable/Score (x)") + theme(plot.title = element_text(hjust = 0.5, size =18)) + theme(axis.title.x = element_text(hjust = 0.5, size=18)) +   
theme(axis.title.y = element_text(hjust = 0.5, size=18)) + theme(axis.text.x=element_text(size=15)) +  theme(axis.text.y=element_text(size=15)) + theme(legend.title = element_text(colour="black", size=13, face="bold")) + 
theme(strip.text = element_text(size=15)) + theme(legend.text=element_text(size=13)) + labs(col="Variable Levels") + scale_colour_discrete(na.translate = input$NAview1) })

# Barplots 
observe({vchoicesASS <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "First__Variable", choices = vchoicesASS, selected = vchoicesASS[8])
updateSelectInput(session, "Second__Variable", choices = vchoicesASS, selected = vchoicesASS[15]) })

output$Association3 <- renderPlot({ggplot(CNA_Metrics_Sub(), aes(x=factor(CNA_Metrics_Sub()[,input$First__Variable]), fill=factor(CNA_Metrics_Sub()[,input$Second__Variable]))) + geom_bar(position="fill", show.legend = input$legend2) + 
ggtitle("Barplot of Selected Clinical Variables") + ylab("Count") + xlab("Clinical Variable") + theme(plot.title = element_text(hjust = 0.5, size =18)) + theme(axis.title.x = element_text(hjust = 0.5, size=18)) +   
theme(axis.title.y = element_text(hjust = 0.5, size=18)) + theme(axis.text.x=element_text(size=15)) +  theme(axis.text.y=element_text(size=15)) + theme(legend.title = element_text(colour="black", size=13, face="bold")) + 
scale_x_discrete(na.translate = input$NAview2) + theme(strip.text = element_text(size=15)) + theme(legend.text=element_text(size=13)) + labs(fill="Variable Levels")})

# Density Plots -> CNA Score Density 
observe({vchoicesDist <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "columnDist", choices = vchoicesDist, selected = vchoicesDist[33])})

# Histogram 
output$CNAHist <- renderPlot({ggplot(CNA_Metrics_Sub(), aes(CNA_Metrics_Sub()[,input$columnDist], fill = cut(CNA_Metrics_Sub()[,input$columnDist], 100))) + geom_histogram(show.legend = FALSE, binwidth = input$bins) + 
ggtitle("Histogram of Selected Continuous Clinical Variable") + ylab("Density") + xlab("Continuous Clinical Variable/Score") + theme(plot.title = element_text(hjust = 0.5, size =17)) + theme(axis.title.x = element_text(hjust = 0.5, size=17)) +
theme(axis.title.y = element_text(hjust = 0.5, size=17)) + theme(axis.text.x=element_text(size=14)) +  theme(axis.text.y=element_text(size=14)) + scale_fill_discrete(h = c(180, 360), c = 150, l = 80) })

# Plain Density Plot 
output$CNADist <- renderPlot({n <- density(CNA_Metrics_Sub()[,input$columnDist], na.rm = T)
ggplot(CNA_Metrics_Sub()) + geom_density(aes(x=CNA_Metrics_Sub()[,input$columnDist], color = 'Color')) + geom_histogram(aes(x=CNA_Metrics_Sub()[,input$columnDist], y = ..density.., color = "Color", alpha = 0.05), fill="#2ac0db", 
alpha = 0.3, position = "identity") + xlab("CNA Score Distribution") + ylab("Density") + ggtitle("Density Plots of Selected Continuouse Variables") + theme(plot.title = element_text(hjust = 0.5, size=17)) + 
theme(axis.title.x = element_text(hjust = 0.5, size=17)) + theme(axis.title.y = element_text(hjust = 0.5, size=17)) + xlim(range(n$x)) + theme(axis.text.x=element_text(size=14)) +  theme(axis.text.y=element_text(size=14)) + 
scale_color_manual(values = c("Color" = "#2ac0db")) + theme(legend.position = "none") })

# Segmented Density Plot (Works for half, tertiled, quartiles and quintiled data)
output$CNADist1 <- renderPlot({
    if(input$legend3 == TRUE){
        colourCount = input$number1
        getPalette = colorRampPalette(brewer.pal(9, "Blues"))
        lab <- as.character(1:input$number1)
        
        dt <- data.frame(x=c(1:length(CNA_Metrics_Sub()[,input$columnDist])), y=CNA_Metrics_Sub()[,input$columnDist])
        dt <- na.omit(dt)
        dens <- density(dt$y)
        df <- data.frame(x=dens$x, y=dens$y)
        probs1 = c(0:input$number1/input$number1)
        probs <- probs1[-c(1,length(probs1))]
        quantiles <- quantile(dt$y, prob=probs)
        df$quant <- factor(findInterval(df$x,quantiles))
        ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + xlab("Distribution") + ylab("Density") + ggtitle("Segmented Density Plots of Selected Continuouse Variables") + 
            theme(plot.title = element_text(hjust = 0.5, size=17)) + theme(axis.title.x = element_text(hjust = 0.5, size=17)) + theme(axis.title.y = element_text(hjust = 0.5, size=17)) + theme(axis.text.x=element_text(size=14)) +  
            theme(axis.text.y=element_text(size=14)) + theme(legend.position = c(0.9, 0.5)) + theme(legend.key.size = unit(0.9, "cm")) + theme(legend.title = element_text(colour="black", size=15, face="bold")) +
            theme(strip.text = element_text(size=15)) + theme(legend.text=element_text(size=15)) + scale_fill_manual(values = getPalette(colourCount), labels = lab, name="Segment")
} else {
    colourCount = input$number1
    getPalette = colorRampPalette(brewer.pal(9, "Blues"))
    
    dt <- data.frame(x=c(1:length(CNA_Metrics_Sub()[,input$columnDist])), y=CNA_Metrics_Sub()[,input$columnDist])
    dt <- na.omit(dt)
    dens <- density(dt$y)
    df <- data.frame(x=dens$x, y=dens$y)
    probs1 = c(0:input$number1/input$number1)
    probs <- probs1[-c(1,length(probs1))]
    quantiles <- quantile(dt$y, prob=probs)
    df$quant <- factor(findInterval(df$x,quantiles))
    ggplot(df, aes(x,y)) + geom_line() + geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + scale_x_continuous(breaks=quantiles) + xlab("Distribution") + ylab("Density") + ggtitle("Segmented Density Plots of Selected Continuouse Variables") + 
        theme(plot.title = element_text(hjust = 0.5, size=17)) + theme(axis.title.x = element_text(hjust = 0.5, size=17)) + theme(axis.title.y = element_text(hjust = 0.5, size=17)) + theme(axis.text.x=element_text(size=14)) +  
        theme(axis.text.y=element_text(size=14)) + theme(legend.position = c(0.9, 0.5)) + theme(legend.key.size = unit(0.9, "cm")) + theme(legend.title = element_text(colour="black", size=15, face="bold")) +
        theme(strip.text = element_text(size=15)) + theme(legend.text=element_text(size=15)) + scale_fill_manual(values = getPalette(colourCount), name="Segment") + guides(fill=FALSE)
    } })

# Facet Wrap -> Make new dataset (melt)
observe({vchoicesDistfacet <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "columnDist1", choices = vchoicesDistfacet, selected = vchoicesDistfacet[4])})

long_file <- reactive({if(input$NAview3 == FALSE){
        CNA_Scores_Plot <- melt(CNA_Metrics_Sub()[,c(input$columnDist, input$columnDist1)])
        CNA_Scores_Plot <- CNA_Scores_Plot[!is.na(CNA_Scores_Plot[,input$columnDist1]), ]
        return(CNA_Scores_Plot) } else {CNA_Scores_Plot <- melt(CNA_Metrics_Sub()[,c(input$columnDist, input$columnDist1)])
return(CNA_Scores_Plot)}})

output$CNADist2 <- renderPlot({n <- density(CNA_Metrics_Sub()[,input$columnDist], na.rm = T)
ggplot(long_file(), aes(x=value)) + geom_histogram(aes(x=value, y=..density.., fill=factor(long_file()[,input$columnDist1])), position = "identity", alpha = 0.45, color="grey30", show.legend = input$legend3) +
geom_density(aes(x=value, color = factor(long_file()[,input$columnDist1])), show.legend = input$legend3) + facet_grid(factor(long_file()[,input$columnDist1])~.) + ggtitle("Exploartion of Density Plots by Variable") + 
ylab("Density") + xlab("Selected Continuous Variable") + theme(plot.title = element_text(hjust = 0.5, size =18)) + theme(axis.title.x = element_text(hjust = 0.5, size=18)) +   theme(axis.title.y = element_text(hjust = 0.5, size=18)) + 
theme(axis.text.x=element_text(size=13)) +     theme(axis.text.y=element_text(size=13)) + theme(legend.title = element_text(colour="black", size=15, face="bold")) + scale_fill_discrete(name = "Variable Levels") + xlim(range(n$x))  + 
guides(color = FALSE) + theme(strip.text = element_text(size=15)) + theme(legend.text=element_text(size=13)) })

# Survival Analysis Tab: 1) Clinical KM Plots

observe({vchoicesSurv1 <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "Time1", choices = vchoicesSurv1, selected = vchoicesSurv1[13])
updateSelectInput(session, "Event1", choices = vchoicesSurv1, selected = vchoicesSurv1[44])
updateSelectInput(session, "Variable", choices = vchoicesSurv1, selected = vchoicesSurv1[18])})

surv_data <- reactive({raw_surv <- CNA_Metrics_Sub()
data.frame(Time = raw_surv[[input$Time1]], Group = raw_surv[[input$Variable]], Cen  = raw_surv[[input$Event1]]) })

datafit <- reactive({sfit <- survfit(Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~ Group, data = surv_data()) })

output$KM1 <- renderPlot({ggsurvplot(datafit(), censor.shape="", xlab="Survival time (months)", ylab="Survival probability", data = surv_data(), size = 1, conf.int = input$confI, pval = T, risk.table = input$riskT, legend = c(input$legendpos),
legend.labs = rownames(summary(datafit()$table)), pval.size = 7, risk.table.height = 0.25, ggtheme = theme_gray(), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, title = ("Breast cancer patients in METABRIC data"), 
font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black")) }) 

output$KMlogrank <- renderPrint({survdiff(Surv(CNA_Metrics_Sub()[,input$Time1], as.numeric(CNA_Metrics_Sub()[,input$Event1])) ~ CNA_Metrics_Sub()[,input$Variable]) })

# 2) Quartiles Survival Plot 2 -> Survival plot based on CNA score (Quartiles) 
observe({vchoicesSurv2 <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "Time11", choices = vchoicesSurv2, selected = vchoicesSurv2[13])
updateSelectInput(session, "Event11", choices = vchoicesSurv2, selected = vchoicesSurv2[44])
updateSelectInput(session, "Variable11", choices = vchoicesSurv2, selected = vchoicesSurv2[42]) })

surv_data_1 <- reactive({raw_surv_var <- CNA_Metrics_Sub()
data.frame(Time = raw_surv_var[[input$Time11]], Quartile = raw_surv_var[[input$Variable11]], cen  = raw_surv_var[[input$Event11]]) })

SurvfitCNA <- reactive({sfit <- survfit(Surv(as.numeric(Time), as.numeric(as.character(cen))) ~ Quartile, data = surv_data_1())
return(sfit) })

output$PercentSurv <- renderPlot({ggsurvplot(SurvfitCNA(), censor.shape="", xlab="Survival time (months)", ylab="Survival probability", data = surv_data_1(), size = 1, conf.int = input$confI1, pval = T, risk.table = input$riskT1, 
legend = c(input$legendpos1), legend.labs = rownames(summary(SurvfitCNA()$table)), risk.table.height = 0.25, pval.size = 7, ggtheme = theme_gray(), break.time.by =50, risk.table.y.text.col = T, risk.table.y.text = FALSE, 
title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))})

output$KMlogrank1 <- renderPrint({survdiff(Surv(CNA_Metrics_Sub()[,input$Time11], as.numeric(CNA_Metrics_Sub()[,input$Event11])) ~ CNA_Metrics_Sub()[,input$Variable11]) })

# 3) Survival Curves - Treatment (Yes or No) 
observe({vchoicesSurvsplit <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "TimeTreat", choices = vchoicesSurvsplit, selected = vchoicesSurvsplit[13])
updateSelectInput(session, "EventTreat", choices = vchoicesSurvsplit, selected = vchoicesSurvsplit[44]) 
updateSelectInput(session, "VariableTreat", choices = vchoicesSurvsplit, selected = vchoicesSurvsplit[42]) 
updateSelectInput(session, "SplittingVariable", choices = vchoicesSurvsplit, selected = vchoicesSurvsplit[19])})

dataRYes <- reactive({CNA_Metrics_Sub()[CNA_Metrics_Sub()[,input$SplittingVariable] == "YES",]})
dataRNo <- reactive({CNA_Metrics_Sub()[CNA_Metrics_Sub()[,input$SplittingVariable] == "NO",]})

surv_data_R <- reactive({raw_surv_R <- dataRYes()
data.frame(Time = raw_surv_R[[input$TimeTreat]], Group = raw_surv_R[[input$VariableTreat]], Cen  = raw_surv_R[[input$EventTreat]]) })

datafit_R <- reactive({sfit <- survfit(Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~ Group, data = surv_data_R()) })

output$KMR1 <- renderPlot({ggsurvplot(datafit_R(), censor.shape="", xlab="Survival time (months)", ylab="Survival probability", data = surv_data_R(), size = 1, conf.int = input$confIyes, pval = T, risk.table = input$riskTyes, 
legend = c(input$legendposyes), legend.labs = rownames(summary(datafit_R()$table)), pval.size = 7, risk.table.height = 0.25, ggtheme = theme_gray(), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, 
title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))})

output$KMlogrankYes <- renderPrint({survdiff(Surv(dataRYes()[,input$TimeTreat], as.numeric(dataRYes()[,input$EventTreat])) ~ dataRYes()[,input$VariableTreat]) })
output$KMlogrankNo <- renderPrint({survdiff(Surv(dataRNo()[,input$TimeTreat], as.numeric(dataRNo()[,input$EventTreat])) ~ dataRNo()[,input$VariableTreat]) })

# KM Plot 2 (NO)
surv_data_NR <- reactive({raw_surv_NR <- dataRNo()
data.frame(Time = raw_surv_NR[[input$TimeTreat]], Group = raw_surv_NR[[input$VariableTreat]], Cen  = raw_surv_NR[[input$EventTreat]]) })

datafit_NR <- reactive({sfit <- survfit(Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~ Group, data = surv_data_NR()) })

output$KMR2 <- renderPlot({ggsurvplot(datafit_NR(), censor.shape="", xlab="Survival time (months)", ylab="Survival probability", data = surv_data_NR(), size = 1, conf.int = input$confIyes, pval = T, risk.table = input$riskTyes, 
legend = c(input$legendposyes), legend.labs = rownames(summary(datafit_NR()$table)), pval.size = 7, risk.table.height = 0.25, ggtheme = theme_gray(), break.time.by = 50, risk.table.y.text.col = T, risk.table.y.text = FALSE, 
title = ("Breast cancer patients in METABRIC data"), font.main = c(18, "plain", "black"), font.x = c(15, "plain", "black"), font.y = c(15, "plain", "black"), font.legend = c(14, "plain", "black"), font.tickslab = c(12, "plain", "black"))}) 


# Univariate Cox choices 
observe({ vchoicesSurvcox <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "Timecox1", choices = vchoicesSurvcox, selected = vchoicesSurvcox[13])
updateSelectInput(session, "OSEventcox1", choices = vchoicesSurvcox, selected = vchoicesSurvcox[44])
updateSelectInput(session, "DSSEventcox1", choices = vchoicesSurvcox, selected = vchoicesSurvcox[45])
updateCheckboxGroupInput(session, "show_vars", choices = vchoicesSurvcox, selected = vchoicesSurvcox[3])})

# OS 
surv_data_OSCox <- reactive({raw_surv_Cox <- CNA_Metrics_Sub()
data.frame(Time = raw_surv_Cox[[input$Timecox1]], Cen  = raw_surv_Cox[[input$OSEventcox1]], Covariate = raw_surv_Cox[, c(input$show_vars)]) })

UnivarOS  <- reactive({a <- "Covariate"
f <- as.formula(paste( "Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~", noquote(paste(a, collapse="+"))))
res.cox <- coxph(formula = f, data = surv_data_OSCox())
return(summary(res.cox))})

output$UniCoxOS <- renderPrint({UnivarOS()})
output$UniCoxOS1 <- renderPrint({UnivarOS()$waldtest})
output$UniCoxOS2 <- renderPrint({UnivarOS()$logtest})

# DSS
surv_data_DSSCox <- reactive({raw_surv_Cox <- CNA_Metrics_Sub()
data.frame(Time = raw_surv_Cox[[input$Timecox1]], Cen  = raw_surv_Cox[[input$DSSEventcox1]], Covariate = raw_surv_Cox[, c(input$show_vars)]) })

UnivarDSS <- reactive({a <- "Covariate"
f <- as.formula(paste( "Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~", noquote(paste(a, collapse="+"))))
res.cox <- coxph(formula = f, data = surv_data_DSSCox())
return(summary(res.cox))})

output$UniCoxDSS <- renderPrint({UnivarDSS()})
output$UniCoxDSS1 <- renderPrint({UnivarDSS()$waldtest})
output$UniCoxDSS2 <- renderPrint({UnivarDSS()$logtest})

# Multivariate Cox
observe({ vchoicesSurvcox <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateCheckboxGroupInput(session, "show_vars1", choices = vchoicesSurvcox, selected = c(vchoicesSurvcox[3],vchoicesSurvcox[4]))})

surv_data_OSCoxMulti <- reactive({raw_surv_Cox <- CNA_Metrics_Sub()
data.frame(Time = raw_surv_Cox[[input$Timecox1]], Cen  = raw_surv_Cox[[input$OSEventcox1]], Covariate = raw_surv_Cox[, c(input$show_vars1)]) })

MultireactiveOS <- reactive({a <- as.character(c(colnames(surv_data_OSCoxMulti()[,3:ncol(surv_data_OSCoxMulti())])))
f <- as.formula(paste( "Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~", noquote(paste(a, collapse="+"))))
res.cox <- coxph(formula = f, data = surv_data_OSCoxMulti())
return(summary(res.cox))})

output$MultiCoxOS <- renderPrint({MultireactiveOS()})
output$MultiCoxOS1 <- renderPrint({MultireactiveOS()$waldtest})
output$MultiCoxOS2 <- renderPrint({MultireactiveOS()$logtest})

surv_data_DSSCoxMulti <- reactive({raw_surv_Cox <- CNA_Metrics_Sub()
data.frame(Time = raw_surv_Cox[[input$Timecox1]], Cen  = raw_surv_Cox[[input$DSSEventcox1]], Covariate = raw_surv_Cox[, c(input$show_vars1)]) })

MultireactiveDSS <- reactive({a <- as.character(c(colnames(surv_data_DSSCoxMulti()[,3:ncol(surv_data_DSSCoxMulti())])))
f <- as.formula(paste( "Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~", noquote(paste(a, collapse="+"))))
res.cox <- coxph(formula = f, data = surv_data_DSSCoxMulti())
return(summary(res.cox))})

output$MultiCoxDSS <- renderPrint({MultireactiveDSS()})
output$MultiCoxDSS1 <- renderPrint({MultireactiveDSS()$waldtest})
output$MultiCoxDSS2 <- renderPrint({MultireactiveDSS()$logtest})

# Cox Models Assumptions
MultireactiveOSAssump <- reactive({a <- as.character(c(colnames(surv_data_OSCoxMulti()[,3:ncol(surv_data_OSCoxMulti())])))
f <- as.formula(paste( "Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~", noquote(paste(a, collapse="+"))))
res.cox <- coxph(formula = f, data = surv_data_OSCoxMulti())
return(res.cox)})

MultireactiveDSSAssump <- reactive({a <- as.character(c(colnames(surv_data_DSSCoxMulti()[,3:ncol(surv_data_DSSCoxMulti())])))
f <- as.formula(paste( "Surv(as.numeric(Time), as.numeric(as.character(Cen))) ~", noquote(paste(a, collapse="+"))))
res.cox <- coxph(formula = f, data = surv_data_DSSCoxMulti())
return(res.cox)})

AssumpOS <- reactive({test.ph <- cox.zph(MultireactiveOSAssump(), terms = input$terms)
return(test.ph) })
output$AssumptionsCoxOS <- renderPlot({ggcoxzph(AssumpOS())}, height = 1100)

AssumpDSS <- reactive({test.ph <- cox.zph(MultireactiveDSSAssump(), terms = input$terms)
return(test.ph) })
output$AssumptionsCoxDSS <- renderPlot({ggcoxzph(AssumpDSS())}, height=1100)

# Association analysis: Fishers exact test, chi-squared, ANOVA and KW
observe({vchoicesASS <- c(names(dataClinicalSurv()), "Score_Quartile_Subset", "Burden_Quartile_Subset")
updateSelectInput(session, "CategoricalV1", choices = vchoicesASS, selected = vchoicesASS[15])
updateSelectInput(session, "CategoricalV2", choices = vchoicesASS, selected = vchoicesASS[8])
updateSelectInput(session, "ContinuousV1", choices = vchoicesASS, selected = vchoicesASS[43])
})

data_Association2 <- reactive({tbl = table(CNA_Metrics_Sub()[,input$CategoricalV1], CNA_Metrics_Sub()[,input$CategoricalV2])
return(tbl)})

output$Cat1 <- renderPrint({chisq.test(data_Association2())})
output$Cat2 <- renderPrint({fisher.test(data_Association2(), simulate.p.value = T)})
output$Cat3 <- renderPrint({fisher.test(data_Association2())})

output$ANOVAAss1 <- renderPrint({leveneTest(CNA_Metrics_Sub()[,input$ContinuousV1] ~ CNA_Metrics_Sub()[,input$CategoricalV1], CNA_Metrics_Sub(), center=mean) })
output$ANOVAAss2 <- renderPrint({fligner.test(CNA_Metrics_Sub()[,input$ContinuousV1] ~ CNA_Metrics_Sub()[,input$CategoricalV1], CNA_Metrics_Sub()) })
output$ANOVAAss3 <- renderPrint({shapiro.test(CNA_Metrics_Sub()[,input$ContinuousV1]) }) # It tests the null hypothesis that the population variances are equal
output$ANOVA <- renderPrint({res.aov <- aov(CNA_Metrics_Sub()[,input$ContinuousV1] ~ CNA_Metrics_Sub()[,input$CategoricalV1], data = CNA_Metrics_Sub())
summary(res.aov)})

output$KW <- renderPrint({kruskal.test(CNA_Metrics_Sub()[,input$ContinuousV1] ~ CNA_Metrics_Sub()[,input$CategoricalV1], data = CNA_Metrics_Sub()) })
output$PC <- renderPrint({pairwise.t.test(CNA_Metrics_Sub()[,input$ContinuousV1], CNA_Metrics_Sub()[,input$CategoricalV1], p.adjust.method = "BH") })
output$Dunn <- renderPrint({DunnTest(CNA_Metrics_Sub()[,input$ContinuousV1], CNA_Metrics_Sub()[,input$CategoricalV1], method = "BH", out.list = F) })

# Maftools Summary input
dataInputMAFPLOT <- reactive({if(is.null(input$fileClinical) == "TRUE") {
        df <- read.maf(maf = dataInputMAF())} else{df <- read.maf(maf = input$fileMAF$datapath, clinicalData = dataClinical())} })

output$MAF1 <- renderPrint({dataInputMAFPLOT()})
output$MAF2 <- renderPrint({getSampleSummary(dataInputMAFPLOT())})
output$MAF3 <- renderPrint({getGeneSummary(dataInputMAFPLOT())})
output$MAF4 <- renderPrint({getFields(dataInputMAFPLOT())})

output$summaryMAF <- renderPlot({
plotmafSummary(dataInputMAFPLOT(), rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)})
output$oncoplotMAF <- renderPlot({oncoplot(dataInputMAFPLOT(), top = 5)})

observe({vchoices1 <- unique(dataInputMAF()[,input$MAFgenecol])
    updateSelectInput(session, "genename1", choices = vchoices1, selected = vchoices1[1])
    updateSelectInput(session, "genename2", choices = vchoices1, selected = vchoices1[2])
    updateSelectInput(session, "genename3", choices = vchoices1, selected = vchoices1[3])})

output$oncostripMAF <- renderPlot({oncostrip(dataInputMAFPLOT(), genes = c(input$genename1, input$genename2, input$genename3))})
output$TandT <- renderPlot({laml.titv = titv(maf = dataInputMAFPLOT(), plot = FALSE, useSyn = TRUE)
plotTiTv(res = laml.titv) })
output$Mutload <- renderPlot({laml.mutload = tcgaCompare(maf = dataInputMAFPLOT(), cohortName = 'Our Data')})
output$VAF1 <- renderPlot({somaticInteractions(maf = dataInputMAFPLOT(), top =5, pvalue = c(0.05, 0.1))})
output$Gcloud <- renderPlot({geneCloud(input = dataInputMAFPLOT(), minMut = 3)})

# Lollipop plot 
observe({vchoiceslol <- unique(dataInputMAF()[,input$MAFgenecol])
    updateSelectInput(session, "genename1lol", choices = vchoiceslol, selected = vchoiceslol[1])
    updateSelectInput(session, "genename2lol", choices = vchoiceslol, selected = vchoiceslol[2])
    updateSelectInput(session, "genename3lol", choices = vchoiceslol, selected = vchoiceslol[3])})

output$lol1 <- renderPlot({lollipopPlot(dataInputMAFPLOT(), gene = input$genename1lol, AACol = 'HGVSp_Short', showMutationRate = TRUE)})
output$lol2 <- renderPlot({lollipopPlot(dataInputMAFPLOT(), gene = input$genename2lol, AACol = 'HGVSp_Short', showMutationRate = TRUE)})
output$lol3 <- renderPlot({lollipopPlot(dataInputMAFPLOT(), gene = input$genename3lol, AACol = 'HGVSp_Short', showMutationRate = TRUE)})
}

# Run the application 
shinyApp(ui = ui, server = server)


