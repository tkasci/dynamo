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
    sidebarPanel("Datensatz",
                 width=3,
                 shinyFilesButton('file', 'Datensatz laden', 'Bitte Datensatz wählen', FALSE),
                 uiOutput("rawControls"), 
                 uiOutput("rawDateSelection"),
                 tableOutput("patinfo")
                 ),
    mainPanel(
      tabsetPanel(
        tabPanel("Verläufe",
                 splitLayout(cellWidths = c("75%","25%"), 
                  plotlyOutput("altRawPlot", width = "100%"),
                  div(h4("Hinweis"), "Wenn Sie zwei",
                      br("oder mehr Variablen"), "ausgewählt haben,", 
                      br("können Sie die Fragen"), "zusammenfügen lassen",
                      checkboxInput("makeScale", "Variablen Zusammenfügen", TRUE)))
                 ),
        tabPanel("Offene Antworten",
                 fluidPage(
                   fluidRow(DT::dataTableOutput("tbl.offene")))
                   ),
        tabPanel("Faktor Analyse",
                 splitLayout(cellWidths = c("75%","25%"),
                             fluidRow(
                               tableOutput("out.fa"), align="center", br(),br(),
                               imageOutput("factorPlot", width = "500", height = "480")
                             ),
                             
                             div(h4("Hinweis"), p("Die Faktoranalyse gruppiert Variablen,",br(),
                                                  "die einen ähnlichen zeitlichen Verlauf", br(), 
                                                  "aufweisen.", br(), br(),
                                                  "Die Berechnung kann einige Augenblicke", br(), 
                                                  "in Anspruch nehmen", br(), br(),
                                                  "Untersucht werden die gesamten ", br(),
                                                  "Variablen. Die Auswahl auf der linken", br(),
                                                  "Seite wird hierbei nicht beachtet.")
                             ))                 
        ),
        tabPanel("Netzwerk Analyse",
                 splitLayout(cellWidths = c("75%","25%"),
                             imageOutput("VAR", width = "500", height = "480"),
                 div(h4("Hinweis"),"Bitte wählen Sie",br("links mindestens"), "zwei Variablen aus.",
                     p(br("Es kann vorkommen,"), "dass kein Netzwerk", br("berechnet werden kann."), 
                       br("Bitte wählen Sie"), "in einem solchen", br("Fall eine andere"), "Kombination aus")))                 
                 ), 
        
        tabPanel("Hilfe",
                 includeMarkdown("www/hilfe.md"))
                )
              )
            )
                ))
