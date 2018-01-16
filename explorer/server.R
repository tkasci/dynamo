# require(Cairo)
require(shiny)
require(ggplot2)
require(plotly)
require(pastecs)
require(dplyr)
require(TTR)
require(shinyFiles)
require(reshape2)
require(vars)
require(psych)
require(scales)
require(tseriesChaos)
require(fNonlinear)
require(fBasics)
require(purrr)
require(qgraph)
require(dyncomp)
source("/srv/shiny-server/explorer/functions.R")
source("https://raw.githubusercontent.com/tkaiser86/r-scripts/master/VARtoEdges.R")
pdf(NULL)


shinyServer(function(input, output, session) {
  roots <- c(wd = '/srv/shiny-server/data/')
  shinyFileChoose(input, 'file', roots = roots, session = session)
  
  values <- reactiveValues()
  
  observeEvent(input$file, {
    rm(list = ls())
    inFile <- parseFilePaths(roots = roots, input$file)
    load(as.character(inFile$datapath), envir = .GlobalEnv)
    values$loaded <- list(
      patinfo = patinfo,
      answers = answers,
      questions = questions,
      questions.short = questions.short,
      df.answers = df.answers,
      mat.answers = mat.answers
      #scaleMax=scaleMax,
      #scaleMin=scaleMin
    )
  })
  
  showComplexityGraphs <- reactive({
    input$showComplexity
  })
  
  # Patient Info for Sidebar, also item text
  output$patinfo <- renderUI({
    str1 <-
      paste("<br/><pre>Patient Code:",
            values$loaded$patinfo[1],
            "</pre>")
    str2 <-
      paste("<pre>Age (years):", values$loaded$patinfo[2], "</pre>")
    str3 <-
      paste("<pre>Gender (M/F):", values$loaded$patinfo[3], "</pre>")
    str4 <-
      paste("<pre>Diagnosis:", values$loaded$patinfo[4], "</pre>")
    str5 <-
      values$loaded$questions[which(values$loaded$questions.short == input$plotselection)]
    HTML(paste(str5, str1, str2, str3, str4, sep = "<br/>"))
  })
  
  output$heatMap <- renderPlot({
    min <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[1])
    max <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[2])
    
  })
  
  # Raw Plots
  output$altRawPlot <- renderPlotly({
    min <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[1])
    max <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[2])
    df <-
      melt(values$loaded$df.answers[min:max, ], id.vars = "date") %>% purrr::keep(is.numeric)
    
    
    #scaleLow <- values$loaded$scaleMin[grep(input$plotselection, colnames(values$loaded$df.answers))]
    #scaleHi <- values$loaded$scaleMax[grep(input$plotselection, colnames(values$loaded$df.answers))]
    
    scaleLow <- 0
    scaleHi <- 100
    
    print(scaleLow) #For Debugging
    print(scaleHi)
    
    yvar <- melt(values$loaded$df.answers[min:max, ], id.vars = "date")
    yvar$date <- yvar$date
    yvar$value <- as.numeric(yvar$value)
    yvarSelected <- yvar[yvar$variable == input$plotselection, ]
    yvarComp <- yvarSelected
    yvarComp$value <-
      scales::rescale(
        complexity(
          yvarSelected$value,
          scaleMin = scaleLow,
          scaleMax = scaleHi,
          width = 5
        ),
        to = c(scaleLow, scaleHi)
      )
    session <-
      data.frame(x = as.numeric(as.Date(
        subset(
          values$loaded$df.answers,
          values$loaded$df.answers[, ncol(values$loaded$df.answers)] == "Ja"
        )$date
      )))
    
    ggplotly(
      ggplot(yvarSelected, aes(x = date, y = value)) +
        ggtitle("Raw Values") +
        geom_point() +
        #geom_smooth() +
        #geom_vline(data = session, aes(xintercept = x), linetype = "longdash", colour= "black") +
        geom_line() +
        expand_limits(y = scaleLow:scaleHi) +
        theme(legend.title = element_blank()) +
        scale_x_datetime()
    )
    
  })
  
  output$complexityPlot <- renderPlotly({
    min <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[1])
    max <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[2])
    df <-
      melt(values$loaded$df.answers[min:max, ], id.vars = "date") %>% purrr::keep(is.numeric)
    
    
    #scaleLow <- values$loaded$scaleMin[grep(input$plotselection, colnames(values$loaded$df.answers))]
    #scaleHi <- values$loaded$scaleMax[grep(input$plotselection, colnames(values$loaded$df.answers))]
    
    scaleLow <- 0
    scaleHi <- 100
    
    print(scaleLow) #For Debugging
    print(scaleHi)
    
    yvar <- melt(values$loaded$df.answers[min:max, ], id.vars = "date")
    yvar$date <- yvar$date
    yvar$value <- as.numeric(yvar$value)
    yvarSelected <- yvar[yvar$variable == input$plotselection, ]
    yvarComp <- yvarSelected
    yvarComp$value <-
      scales::rescale(
        complexity(
          yvarSelected$value,
          scaleMin = scaleLow,
          scaleMax = scaleHi,
          width = 5
        ),
        to = c(scaleLow, scaleHi)
      )
    session <-
      data.frame(x = as.numeric(as.Date(
        subset(
          values$loaded$df.answers,
          values$loaded$df.answers[, ncol(values$loaded$df.answers)] == "Ja"
        )$date
      )))
    
    ggplotly(
      ggplot(yvarComp, aes(x = date, y = value)) +
        ggtitle("Dynamic Complexity") +
        geom_point() +
        geom_smooth() +
        geom_hline(
          aes(
            yintercept = qnorm(.95) * sd(yvarComp$value[1:length(yvarComp$value - 5)]) + mean(yvarComp$value[1:length(yvarComp$value -
                                                                                                                        5)])
          ),
          linetype = "longdash",
          colour = "red"
        ) +
        geom_vline(
          data = session,
          aes(xintercept = x),
          linetype = "longdash",
          colour = "red"
        ) +
        geom_line() +
        expand_limits(y = scaleLow:scaleHi) +
        theme(legend.title = element_blank()) +
        scale_x_datetime()
    )
  })
  
  
  output$rawControls <- renderUI({
    options <- values$loaded$questions.short
    selectInput("plotselection",
                "Choose Variables to Plot",
                multiple = FALSE,
                options)
  })
  
  output$rawDateSelection <- renderUI({
    start = min(as.Date(values$loaded$df.answers$date))
    min =  min(as.Date(values$loaded$df.answers$date))
    end = max(as.Date(values$loaded$df.answers$date))
    max = max(as.Date(values$loaded$df.answers$date))
    format = "dd.mm.yyyy"
    separator = "to"
    dateRangeInput(
      "rawdaterange",
      "Select date range",
      separator = separator,
      min = min,
      max = max,
      start = start,
      end = end,
      format = format
    )
  })
  
  output$data.item.table <- renderDataTable({
    table  <-
      as.data.frame((sort(
        round(values$loaded$current_model$data.scores, 3),
        decreasing = TRUE
      ) * 100))
    table <-
      cbind(table, values$loaded$questions[rownames(table) %in% values$loaded$questions.short])
    colnames(table) <- c("Normalized Score", "Item Text")
    table
  })
  
  # Recurrence Plot
  output$recurrencePlot <- renderPlot({
    min <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[1])
    max <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[2])
    var <- melt(values$loaded$df.answers[min:max, ], id.vars = "date")
    varSelected <-
      var[which(var$variable == input$plotselection), ]$value
    layout(rbind(1, 2), heights = c(7, 1))
    recurr(
      varSelected,
      m = 3,
      d = 1,
      col = heat.colors(n = 20, alpha = 1)
    )
    #title(sub = input$plotselection)
  })
  
  output$heatmapOverview <- renderPlotly({
    max <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[2])
    min <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[1])
    
    var <- values$loaded$df.answers[min:max, ]
    varLong <- melt(var, id.vars = "date")
    
    varComp <- df.complexity(var, 0, 100, 7)
    varComp <- cbind(date = var$date, varComp)
    varComp <- varComp %>% dplyr::select(-time)
    varCompLong <-
      melt(varComp, id.vars = "date", value.name = "Complexity")
    
    if (input$rawOverview) {
      heatmapOverview <- ggplotly(
        ggplot(data = varCompLong, aes(x = date, y = variable)) +
          geom_raster(aes(fill = Complexity), interpolate = FALSE) +
          geom_text(
            data = varLong,
            colour = "white",
            size = 2,
            aes(label = value)
          ) +
          scale_x_datetime()
      )
    } else {
      heatmapOverview <- ggplotly(
        ggplot(data = varCompLong, aes(x = date, y = variable)) +
          geom_raster(aes(fill = Complexity), interpolate = FALSE) +
          scale_x_datetime()
      )
    }
    
    heatmapOverview
  })
  
  output$networkPlot <- renderPlot({
    dat <- values$loaded$df.answers[min:max, ]
    dat$tdif <- diff(dat$date)
    dat$tdif[is.na(dat$tdif)] <- 0
    dat$sumT <- cumsum(dat$tdif)
    
    datcub <- data.frame(matrix(ncol = ncol(dat) - 2, nrow = nrow(data)))
    for (i in 1:ncol(datcub)) {
      datcub[, i] <-
        (spline(
          x = dat$sumT,
          y = dat[, i],
          nrow(dat),
          method = 'fmm'
        )$y)
    }
    colnames(datcub) <- colnames(dat[, 1:ncol(dat - 2)])
    VARmodel <-
      VAR(
        datcub,
        type = "both",
        p = 1,
        exogen = cbind(session = factor(values$loaded$df.answers$session))
      )
    edgelist <- VARtoEdges(VARmodel)
    
    qgraph(edgelist$temporal, edge.labels = T)
  })
  
})
