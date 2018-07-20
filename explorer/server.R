# require(Cairo)
require(car)
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
require(DT)
library(forecast)
source("./functions.R")
#source("https://raw.githubusercontent.com/tkaiser86/r-scripts/master/VARtoEdges.R")
pdf(NULL)


shinyServer(function(input, output, session) {
  roots <- c(wd = 'data')
  shinyFileChoose(input, 'file', roots = roots, session = session)
  
  values <- reactiveValues()
  
  observeEvent(input$file, {
    rm(list = ls())
    inFile <- parseFilePaths(roots = roots, input$file)
    load(as.character(inFile$datapath), envir = .GlobalEnv)
    values$loaded <- list(
      answers = answers,
      questions = questions,
      questions.short = questions.short,
      questions.load = questions.load,
      df.answers = df.answers,
      offene.cut = offene.cut
    )
  })

  output$rawControls <- renderUI({
    options <- values$loaded$questions
    selectInput("plotselection",
                "Variable auswählen",
                choices = c("Wählen" = "", options),
                multiple = TRUE,
                selectize = T
                )
  })
  
  output$rawDateSelection <- renderUI({
    start = min(as.Date(values$loaded$df.answers$date))
    min =  min(as.Date(values$loaded$df.answers$date))
    end = max(as.Date(values$loaded$df.answers$date))
    max = max(as.Date(values$loaded$df.answers$date))
    format = "dd.mm.yyyy"
    separator = "bis"
    dateRangeInput(
      "rawdaterange",
      "Zeitauswahl",
      separator = separator,
      min = min,
      max = max,
      start = start,
      end = end,
      format = format
    )
  })
  
  
  # Patient Info for Sidebar, also item text
  output$patinfo <- renderTable({ #renderUI

    selectionTable  <- data.frame(values$loaded$questions[values$loaded$questions %in% input$plotselection],
                                  values$loaded$questions.short[values$loaded$questions %in% input$plotselection])
    selectionTable[order(selectionTable[,2]),]
  }, spacing = 'xs', striped = TRUE, colnames = FALSE)
  

  # Table Offene Fragen
  output$tbl.offene <- DT::renderDT({
    names(offene.cut) <- c("Datum", "Uhrzeit", "Beschwerden", "Offen", "Gedanken", "Notizen")
    DT::datatable(offene.cut)
  })
  
  
  # Raw Plots
  output$altRawPlot <- renderPlotly({
    min <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[1])
    max <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[2])

    yvar <- values$loaded$df.answers[min:max, c(values$loaded$questions.short[values$loaded$questions %in% input$plotselection],"date")]
    yvar.m <- melt(yvar, id.vars = "date")

    yvar.m$value <- as.numeric(yvar.m$value)

    if(input$makeScale){
    	scale.df.m <- aggregate(yvar.m$value, by = list(date = yvar.m$date), FUN = mean)
    	colnames(scale.df.m) <- c("date", "value")
      }
   
    if(!input$makeScale){ 
    ggplotly(
      ggplot(yvar.m, aes(x = date, y = value)) +
        ggtitle("") +
        geom_point() +
        #geom_smooth() +
        #geom_vline(data = session, aes(xintercept = x), linetype = "longdash", colour= "black") +
        geom_line() +
        theme(legend.title = element_blank()) +
        scale_x_datetime() +
        labs(x = "", y = "")
    )} else {
	    ggplotly(
      ggplot(scale.df.m, aes(x = date, y = value)) +
        geom_point() +
        #geom_smooth() +
        #geom_vline(data = session, aes(xintercept = x), linetype = "longdash", colour= "black") +
        geom_line() +
        theme(legend.title = element_blank()) +
        scale_x_datetime() +
        labs(x = "", y = ""))
   }

  })
  
  
 #  # Durchschnitt-Plots  ### Not working jet
 #   output$Durchschnitt <- renderPlot({
 #     min <-
 #       which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[1])
 #     max <-
 #       which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[2])
 # 
 #     yvar <- values$loaded$df.answers[min:max, c(values$loaded$questions.short[values$loaded$questions %in% input$plotselection],"date")]
 #     #yvar.m <- melt(yvar, id.vars = "date")
 #     #yvar.m$value <- as.numeric(yvar.m$value)
 # 
 # 
 #     varAllDates=NULL
 #     for (i in 1:length(unique(yvar$date))) {
 #       # gather data of that day
 #       x <- yvar$date[yvar$date == unique(yvar$date)[i]]
 #       y <- yvar[yvar$date == unique(yvar$date)[i],input$plotselection]
 #       varAllDates<-rbind(varAllDates,data.frame(x,y)) #store data in a dataframe
 #     }
 #    plot(varAllDates$y)
 # })

  
  
  #### VAR
  
  output$VAR <- renderPlot({
    
    #detrend with helper function
    detrend.man(data)
      
    #selected variables only
    min <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[1])
    max <-
      which(as.Date(values$loaded$df.answers$date) == input$rawdaterange[2])
 
    yvar <- data[, c(values$loaded$questions.short[values$loaded$questions %in% input$plotselection])]
    
    ###
    
    yvar <- scale(yvar)
    yvar <- yvar + 200 #introduce arbitrary intercept
    var1 <- VAR(yvar, p = 1)
    VAR.sig <- restrict(var1, method = "ser", thresh = 2) # think about t-thresh
    

    serial.out <- serial.test(VAR.sig, type = "BG") #Breusch-Godfrey 
    #serial.out <- serial.test(VAR.sig, type = "PT.asymptotic") #Portmanteau
    
    sig = 0.05
    #if(serial.out$serial$p.value > sig){ #### needs to be uncomment.
      #get edge list
      nVariables <- ncol(yvar)
      temporal.edges <- matrix(data = 0, nrow = nVariables, ncol = nVariables)
      
      colnames(temporal.edges) <- colnames(yvar)
      row.names(temporal.edges) <- colnames(yvar)
      
      
      for(n in 1:nVariables){
        sigEdges <- names(VAR.sig$varresult[[n]]$coefficients)
        sigEdges <- sub(".l1", "", sigEdges)[1:(length(sigEdges)-1)]
        temporal.edges[rownames(temporal.edges) %in% sigEdges, n] <- VAR.sig$varresult[[n]]$coefficients[1:(length(VAR.sig$varresult[[n]]$coefficients)-1)]
      } 
      
      qgraph(temporal.edges, edge.labels = F, threshold = 0.2, labels = colnames(yvar))
      
    #} #### else needs to be included.
      
  })
  
  
  #### Factor analysis
  
  output$factorPlot <- renderPlot({
  
    data <- detrend.man(data)
    
    sig.lvl <- 0.05
    for(i in 1:ncol(data)){
      box.out <- Box.test(data[,i], 20, "Ljung-Box")
      if(box.out$p.value<= sig.lvl){
        auto.arima.out <- auto.arima(data[,i], stationary = T)
        data[,i] <- as.numeric(auto.arima.out$residuals)
      }
    }
    
    data.fa <- data
    #FA
    faParallel <- fa.parallel(data.fa, fm = "ml", plot = F)
    nFAeigen <- range(which(faParallel$fa.values > 1))[2]
    nFAparallel <- faParallel$nfact
    
    #choose criterium
    nFA <- nFAparallel
    
    #fa
    result_fa <- fa(data.fa, nfactors = nFA, rotate = "varimax", fm ="ml") #fm ="gls"
    #print(result_fa, digits = 2, sort = T)
    
    ### Automatic FA - Chi^2
    data.fa.scale <-scale(data.fa)
    pChi <- NULL
    pVal = 0.0001
    while(pVal < 0.05) {

      #faParallel <- fa.parallel(data.fa.scale, fm = "ml", plot = F)
      result_fa <- fa(data.fa.scale, nfactors = nFA, rotate = "varimax", fm ="ml") #fm ="gls"
      pChi <- c(pChi, result_fa$PVAL)
      pVal <- result_fa$PVAL
      #print(pVal)
      
      if(pVal > 0.05) break
      data.fa.scale <- subset(data.fa.scale, select = ! (result_fa$complexity == max(result_fa$complexity) |        #max complexit item out
                                                           result_fa$communality == min(result_fa$communality)) #|    #lowest h2 item out
                              #result_fa$uniquenesses == min(result_fa$uniquenesses))    #low uniqueness out
      )
      #print(paste("items left", ncol(data.fa.scale)))
    }
    #print(result_fa, digits = 2, sort = T)
    
    ## Item overview
    cutoff = 0.4
    faNames <- NULL
    faText <- NULL
    faTable <- NULL
    faMerge <- NULL
    fa.out <- NULL
    for(i in 1:nFA){
      faNames[[i]] <- data.frame(Var = names(fa.sort(result_fa)$loadings[,i][sqrt(fa.sort(result_fa)$loadings[,i]^2) > cutoff]),
                                 Score = round(fa.sort(result_fa)$loadings[sqrt(fa.sort(result_fa)$loadings[,i]^2) > cutoff, i], digits = 2))
      faNames[[i]] <- faNames[[i]][order(abs(faNames[[i]]$Score), decreasing = T),]
      
      faTable[[i]] <- data.frame(questions.short,questions)
      faTable[[i]] <- faTable[[i]][faTable[[i]][,1] %in% names(fa.sort(result_fa)$loadings[,i][sqrt(fa.sort(result_fa)$loadings[,i]^2) > cutoff]),]
      faMerge[[i]] <- merge.data.frame(x = faTable[[i]], y = faNames[[i]], by.x = "questions.short", by.y = "Var")
      faMerge[[i]] <- faMerge[[i]][order(abs(faMerge[[i]]$Score), decreasing = T),]
      faMerge[[i]]$Sign <- faMerge[[i]]$Score
      faMerge[[i]]$Sign[faMerge[[i]]$Score >= 0] <- "(+)"
      faMerge[[i]]$Sign[faMerge[[i]]$Score < 0] <- "(-)"
      faMerge[[i]]$Faktor <- rep(paste("Faktor", i), times = nrow(faMerge[[i]]))
      faMerge[[i]]$Faktor[-1] <- ""
      fa.out[[i]] <- faMerge[[i]][,c(F,T,F,T,T)]
      #show(faTable[[i]])
      #show(paste("________________________________________________________________________"))
    }
    
    faRbind <- do.call(rbind.data.frame, fa.out)
    fa.finTable <<- data.frame(Faktor = faRbind$Faktor, Frage = faRbind$questions, Zeichen = faRbind$Sign) # global variable
    
    VAR.fa.out <- VAR(result_fa$scores+200) #introduce arbitrary intercept
    VAR.fa.out.res <- restrict(VAR.fa.out)
    
    serial.out <- serial.test(VAR.fa.out.res, type = "BG") #Breusch-Godfrey 
    #serial.out <- serial.test(VAR.fa.out.res, type = "PT.asymptotic") #Portmanteau
    
    fa.scores.out <- result_fa$scores
    
    sig = 0.1
    #if(serial.out$serial$p.value > sig){ #### needs to be uncomment.
      #get edge list
      nVariables <- ncol(fa.scores.out)
      temporal.edges <- matrix(data = 0, nrow = nVariables, ncol = nVariables)
      
      colnames(temporal.edges) <- colnames(fa.scores.out)
      row.names(temporal.edges) <- colnames(fa.scores.out)
      
      
      for(n in 1:nVariables){
        sigEdges <- names(VAR.fa.out.res$varresult[[n]]$coefficients)
        sigEdges <- sub(".l1", "", sigEdges)[1:(length(sigEdges)-1)]
        temporal.edges[rownames(temporal.edges) %in% sigEdges, n] <- VAR.fa.out.res$varresult[[n]]$coefficients[1:(length(VAR.fa.out.res$varresult[[n]]$coefficients)-1)]
      } 
      
      colnames(temporal.edges) <- c(sub("ML", "FA", colnames(temporal.edges)))
      rownames(temporal.edges) <- c(sub("ML", "FA", rownames(temporal.edges)))
      
      qgraph(temporal.edges, edge.labels = F, threshold = 0.2, labels = colnames(temporal.edges))
    #}  #### else needs to be included.
    
  })
  
  # Table Factor Analysis
  output$out.fa <- renderTable({
    head(fa.finTable, n =  nrow(fa.finTable))
  })
  
})
