#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#
shinyUI(pageWithSidebar(
  # Application title
  headerPanel(HTML("<h2>CycloStratiGRAPHICS: Shiny User Interface (v0.1)</h2>")),
  ########################################################################################
  #### Panel 'About' (right hand side)
  ########################################################################################
  sidebarPanel(
    #
    #tags$head(
    #  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")
    #),
    #
    imageOutput("uptc.logo", inline=TRUE),
    p(HTML("<h5>Welcome to CycloStratiGRAPHICS, the open-source on-line GUI for
      cyclostratigraphy.</h5> This interface helps in the identification of the cycles
      present in a stratigraphic signal (grain size, well log, abundance data, etc)
      using the <strong>R</strong> language. Identify the periodic components of your
      stratigraphic data and visualize the results using the panels on the right
      using spectral and wavelet analysis."))
  ),
  mainPanel(
    tabsetPanel(
      #########################################################################
      ####                    Panel 'Import Data'
      #########################################################################
      tabPanel("Import Data",
               h3("First step: import data"),
               br(),
               checkboxInput('header', ' Header?', TRUE),
               checkboxInput('rownames', ' Row names?', FALSE),
               selectInput('sep', 'Separator:',
                           c("Comma","Semicolon","Tab","Space"), 'Comma'),
               selectInput('quote', 'Quote:',
                           c("None","Double Quote","Single Quote"),
                           'Double Quote'),
               selectInput('dec', 'Decimal mark', c("Period", "Comma"),
                           'Period'),
               numericInput('nrow.preview','Number of rows in the preview:',20),
               numericInput('ncol.preview', 'Number of columns in the preview:',
                            10),
               helpText("Note: Even if the preview only shows a restricted number of
                        observations, the map will be based on the full dataset."),
               fileInput('file1', 'Choose CSV/TXT File'),
               tableOutput("view")),
      #########################################################################
      ####                    Panel 'Import Data'
      #########################################################################
      tabPanel("Exploratory Data Analysis",
               icon = icon("bar-chart-o"),
               h3("Second Step: Start to look at our data"),
               br(),
               p(HTML("In this step, a set of tools is used to gain insight into the data,
                      uncover the underlying structure, define important variables, detect
                      outliers and anomalies, test underlying assumptions, develop
                      parsimonious models.")),
               br(),
               h4("Filter Well Log"),
               wellPanel(
                 uiOutput("EDA.filter1"),
                 uiOutput("EDA.filter2"),
                 uiOutput("EDA.filter3"),
                 br(),
                 uiOutput("EDA.filter4")
               ),
               br(),
               uiOutput("EDAplottype"),
               br(),
               uiOutput("EDAtitle1"),
               uiOutput("EDAoption1"),
               uiOutput("EDAoption2"),
               uiOutput("EDAoption3"),
               br(),
               actionButton(inputId = "EDA.plot.results", label = "Plot Results"),
               br(),
               br(),
               plotOutput("EDA.plot")),
      #########################################################################
      ####                    Panel 'Spectral Analysis'
      #########################################################################
      tabPanel("Spectral Analysis",
               icon = icon("clock-o"),
               h3("Third step: Identify significant frequencies/cycles"),
               br(),
               br(),
               p(HTML("In this step all the significant frequencies/cycles present in the
                       stratigraphic signal are identified using spectral analysis.")),
               br(),
               uiOutput("SPEC.selectvar"),
               uiOutput("SPEC.ar"),
               uiOutput("SPEC.demean"),
               uiOutput("SPEC.detrend"),
               uiOutput("SPEC.xlim"),
               uiOutput("SPEC.ylim"),
               uiOutput("SPEC.run"),
               br(),
               br(),
               plotOutput("SPEC.plot"),
               br(),
               h4('Significant Frequencies/Cycles'),
               br(),
               tableOutput("SPEC.view")),
      #########################################################################
      ####                    Panel 'Wavelet Data'
      #########################################################################
      tabPanel("Wavelet Analysis",
               icon = icon("location-arrow"),
               h3("Fourth step: Localize significant cycles in space"),
               br(),
               uiOutput("WAVE.selectvar"),
               uiOutput("WAVE.type"),
               uiOutput("WAVE.par"),
               uiOutput("WAVE.lag"),
               uiOutput("WAVE.dosig"),
               uiOutput("WAVE.siglvl"),
               uiOutput("WAVE.sigtest"),
               uiOutput("WAVE.run"),
               br(),
               plotOutput("WAVE.plot"))
    ))
  )
)

