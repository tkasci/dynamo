library(shiny)
library(shinyFiles)
library(plotly)
library(qgraph)

shinyUI(fluidPage(

  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
    div(img(src = "DynAMo.png", align = "center"), style = "text-align: center;"),

  sidebarLayout(
    sidebarPanel("Dataset",
                 width=3,
                 shinyFilesButton('file', 'Load Dataset', 'Please select a dataset', FALSE),
                 uiOutput("rawControls"), 
                 uiOutput("rawDateSelection"),
                 htmlOutput("patinfo"),
                 checkboxInput("showRaw", "Raw Plot", TRUE),
                 checkboxInput("showRecurr", "Recurrence Plot", FALSE),
                 checkboxInput("showComplexity", "Complexity Plot", TRUE),
                 checkboxInput("makeScale", "Make scale", FALSE)
                 ),
    mainPanel(
      tabsetPanel(
        tabPanel("Time Series",
                 splitLayout(cellWidths = c("33%","33%","33%"), 
                             htmlOutput("rawPlotInfo")),
                  conditionalPanel(condition = "input.showRaw==true",
                    plotlyOutput("altRawPlot", width = "100%")),
                  conditionalPanel(condition = "input.showComplexity==true",
                    plotlyOutput("complexityPlot", width = "100%")),
                  conditionalPanel(condition = "input.showRecurr==true",
                    plotOutput("recurrencePlot", width = "110%"))
                 ),
        tabPanel("Overview",
                 checkboxInput("rawOverview", "Show Raw Values", TRUE),
                 plotlyOutput("heatmapOverview", width = "110%")),
        tabPanel("Network Analysis",
                plotOutput("networkPlot", "auto", "auto")
                 ),

        tabPanel("Help",
                 includeMarkdown("hilfe.md"))
                )
              )
            )
                ))
