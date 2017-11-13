#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(astrochron)
library(biwavelet)
#
shinyServer(function(input, output, session) {

  ########################################################################################
  ###                              Server variables
  ########################################################################################
  server.env <- environment() # used to allocate in functions
  current.table <- NULL
  current.table.original <- NULL
  res.mtm <- NULL
  res.mtm3 <- NULL
  wt.res <- NULL
  ########################################################################################
  ####                 Panel 'About' (left hand side)
  ########################################################################################
  # Output the uptc logo :
  output$uptc.logo <- renderImage(list(src="uptc_jpg.jpg"),
                                      deleteFile=FALSE)
  ########################################################################################
  ####                    Panel 'Import data'
  ########################################################################################
  dInput <- reactive({
    in.file <- input$file1

    if (is.null(in.file))
      return(NULL)

    the.sep <- switch(input$sep, "Comma"=",", "Semicolon"=";", "Tab"="\t",
                      "Space"="")
    the.quote <- switch(input$quote, "None"="","Double Quote"='"',
                        "Single Quote"="'")
    the.dec <- switch(input$dec, "Period"=".", "Comma"=",")
    if (input$rownames) {
      the.table <- read.table(in.file$datapath, header=input$header,
                              sep=the.sep, quote=the.quote, row.names=1,
                              dec=the.dec)
    } else {
      the.table <- read.table(in.file$datapath, header=input$header,
                              sep=the.sep, quote=the.quote, dec=the.dec)
    }
    # return the table
    server.env$current.table <- the.table
    the.table
  })
  #
  # data preview table
  output$view <- renderTable({
    d.input <- dInput()
    if (is.null(d.input))
      return(NULL)
    if (ncol(d.input)>input$ncol.preview)
      d.input <- d.input[,1:input$ncol.preview]
    head(d.input, n=input$nrow.preview)
  }, rownames = TRUE)
  ########################################################################################
  ####                    Panel 'Exploratory Data Analysis'
  ########################################################################################
  output$EDAvarnames <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    current.names <-  c("None", names(server.env$current.table))
    selectInput(inputId= "EDAvaornames1", label = "Current Variable",
                choices = current.names)
  })
  #
  output$EDA.filter1 <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    tmp <- selectInput(inputId = "EDA.filter1a", label = "Filter Well Log by",
                       choices = c("None", "Depth", "Geological Unit"))
    return(tmp)
  })
  #
  output$EDA.filter2 <- renderUI({
    tmp <- NULL
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$EDA.filter1a) || input$EDA.filter1a == "None")
      return(NULL)
    if(input$EDA.filter1a == "Depth"){
      mny <- min(current.table$DEPTH)
      mxy <- max(current.table$DEPTH)
      value <- paste(as.character(mny), as.character(mxy), sep = ",")
      tmp <- textInput(inputId = "EDA.filter.depth", label = "Depth(min, max)= ",
                       value = value)
    }
    if(input$EDA.filter1a == "Geological Unit"){
      fms <- as.character(unique(current.table$Unit))
      print(c("None", fms))
      tmp <- selectInput(inputId = "EDA.filter.fm", label = "Geological Unit= ",
                         choices = c("None", fms), selected = "None", multiple = TRUE)
    }
    return(tmp)
  })
  #
  output$EDA.filter3 <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$EDA.filter1a) || input$EDA.filter1a == "None")
      return(NULL)
    tmp <- actionButton(inputId = "EDARunFilter", label = "Apply Filter", icon = icon("bullseye"))
    return(tmp)
  })
  #
  output$EDA.filter4 <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$EDA.filter1a) || input$EDA.filter1a == "None")
      return(NULL)
    tmp <- actionButton(inputId = "EDARestore", label = "Restore Original Logs", icon = icon("help"))
    return(tmp)
  })
  #
  observeEvent(input$EDARunFilter, {
    filter_well_log()
  })
  #
  observeEvent(input$EDARestore, {
    restore_well_log()
  })
  #
  filter_well_log <- function(){
    current.table <- server.env$current.table
    input$EDARunFilter
    if(is.null(current.table))
      return(NULL)
    if(input$EDA.filter1a == "Depth"){
      depthrng <- as.numeric(unlist(strsplit(input$EDA.filter.depth,",")))
      depthmn <- min(depthrng)
      depthmx <- max(depthrng)
      server.env$current.table.original <- current.table
      current.table <- current.table %>% filter(DEPTH <= depthmx & DEPTH >= depthmn)
      server.env$current.table <- current.table
    }
    if(input$EDA.filter1a == "Geological Unit"){
      chosen.fms <- input$EDA.filter.fm
      print(chosen.fms)
      server.env$current.table.original <- current.table
      current.table <- current.table %>% filter(Unit %in% chosen.fms)
      server.env$current.table <- current.table
      updateSelectInput(session,inputId = "EDAplottypes1", selected = "None")
    }
  }
  #
  restore_well_log <- function(){
    current.table <- server.env$current.table
    input$EDARestore
    if(is.null(current.table))
      return(NULL)
    server.env$current.table <- server.env$current.table.original
    updateSelectInput(session,inputId = "EDAplottypes1", selected = "None")
  }
  #
  output$EDAplottype <- renderUI({
    if(!is.null(server.env$current.table)){
      selectInput(inputId = "EDAplottypes1", label = "Plot Type",
                  choices = c("None", "Well Logs", "Histogram"))
    }
  })
  #
  output$EDAoption1 <- renderUI({
    tmp <- NULL
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$EDAplottypes1) || input$EDAplottypes1 == "None")
      return(NULL)
#    if(input$EDAplottypes1 == "Well Logs"){
#      bot <- max(current.table$DEPTH)
#      tmp <- textInput(inputId = "EDAtop", label = "Depth Bottom(ft)",
#                       value = as.character(bot))
#    }
    if(input$EDAplottypes1 == "Histogram"){
      tmp <- textInput(inputId = "EDAnbins", label = "Number Bins", value = "30")
    }
    return(tmp)
  })
  #
  output$EDAtitle1 <- renderUI({
    tmp <- NULL
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$EDAplottypes1) || input$EDAplottypes1 == "None")
      return(NULL)
    if(input$EDAplottypes1 == "Well Logs"){
      tmp <- h4("Well Logs")
    }
    if(input$EDAplottypes1 == "Histogram"){
      tmp <- h4("Histogram")
    }
    return(tmp)
  })
  #
  output$EDAoption2 <- renderUI({
    tmp <- NULL
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$EDAplottypes1) || input$EDAplottypes1 == "None")
      return(NULL)
#    if(input$EDAplottypes1 == "Well Logs"){
#        top <- min(current.table$DEPTH)
#        tmp <- textInput(inputId = "EDAbottom", label = "Depth Top(ft)",
#                         value = as.character(top))
#    }
    if(input$EDAplottypes1 == "Histogram"){
      tmp <- selectInput(inputId = "histovar", label = "Variable",
                         choices = c("None", names(current.table)), selected = "None")
    }
    return(tmp)
  })
  #
  output$EDAoption3 <- renderUI({
    tmp <- NULL
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$EDAplottypes1) || input$EDAplottypes1 == "None")
      return(NULL)
    if(input$EDAplottypes1 == "Histogram"){
      tmp <- checkboxInput(inputId = "groupvar", label = "Grouped by Geological Unit",
                           value = FALSE)
    }
    return(tmp)
  })
  #
  output$EDA.plot <- renderPlot({
    res <- NULL
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    #
    if(is.null(input$EDAplottypes1) || input$EDAplottypes1 == "None")
      return(NULL)
    #
    #if(is.null(input$histovar))
    #  return(NULL)
    if(input$EDAplottypes1 == "Well Logs"){
      # Resistivity plot
      p1 <- ggplot() + geom_line(aes(x = DEPTH, y = RPS2), data = current.table,
                                 color = "black") +
        scale_y_log10() +
        scale_x_reverse() +
        coord_flip() +
        xlab('Depth(ft)') +
        ggtitle('Resistivity') +
        theme_bw(base_size = 16) +
        theme(aspect.ratio = 3,
              plot.margin = unit(rep(.01,4),"cm"))
      # Gamma Ray
      p2 <- ggplot() + geom_line(aes(x= DEPTH, y = GR), data = current.table,
                                 color = "black") +
        scale_x_reverse() +
        coord_flip() +
        xlab('Depth(ft)') +
        ggtitle('Gamma Ray') + theme_bw(base_size = 16) +
        theme(aspect.ratio = 3,
              plot.margin = unit(rep(.01,4),"cm"))
      # Neutron Porosity
      p3 <- ggplot() + geom_line(aes(x = DEPTH, y = NPHI), data = current.table, col = "black") +
        scale_x_reverse() +
        scale_y_reverse() +
        coord_flip() + xlab('Depth(ft)') +
        ggtitle("Neutron Porosity") +
        theme_bw(base_size = 16) +
        theme(aspect.ratio = 3,
              plot.margin = unit(rep(.01,4),"cm"))
      #
      res <- grid.arrange(p2, p1, p3, ncol = 3)
    }
    if(input$EDAplottypes1 == "Histogram"){
      if(is.null(input$EDAnbins)){
        nbins <- 30
      }
      else{
        nbins <- as.numeric(input$EDAnbins)
      }
      #print(nbins)
      h1 <- ggplot(aes(x = GR), data = current.table) + geom_histogram(bins = nbins,
                                                                       fill="green") +
        theme_bw() +
        ggtitle('Gamma Ray')
      #print(h1)
      h2 <- ggplot(aes(x = RPS2), data = current.table) + geom_histogram(bins = nbins,
                                                                         fill="red") +
        scale_x_log10() +
        theme_bw() +
        ggtitle('Resistivity')
      #print(h2)
      h3 <- ggplot(aes(x = NPHI), data = current.table) + geom_histogram(bins = nbins,
                                                                         fill = "blue") +
        theme_bw() +
        ggtitle('Neutron Porosity')
      #print(h3)
      #
      if(is.null(input$histovar) || input$histovar == "None"){
        res <- grid.arrange(h1, h2, h3, ncol = 2)
      }
      else {
        if(input$histovar == "GR")
          res <- h1
        if(input$histovar == "NPHI")
          res <- h3
        if(input$histovar == "RPS2")
          res <- h2
      }
      #
      if(is.null(input$groupvar)){
        res <- grid.arrange(h1, h2, h3, ncol = 2)
      }
      else if(input$groupvar){
        current.table$Unit <- factor(current.table$Unit,  levels=unique(current.table$Unit))
        nbins <- as.numeric(input$EDAnbins)
        if(input$histovar == "None"){
          res <- grid.arrange(h1, h2, h3, ncol = 2)
        }
        else{
          if(input$histovar == "GR"){
            res <- ggplot(aes(x = GR), data = current.table) +
              geom_histogram(bins=nbins, fill="green") +
              facet_wrap(~Unit) +
              theme_bw() +
              ggtitle('Gamma Ray')
          }
          if(input$histovar == "RPS2"){
            res <- ggplot(aes(x = RPS2), data = current.table) +
              geom_histogram(bins=nbins, fill = "red") +
              facet_wrap(~Unit) +
              scale_x_log10() +
              theme_bw() +
              ggtitle('Resistivity')
          }
          if(input$histovar == "NPHI"){
            res <- ggplot(aes(x = NPHI), data = current.table) +
              geom_histogram(bins=nbins, fill="blue") +
              facet_wrap(~Unit) +
              theme_bw() +
              ggtitle('Neutron Porosity')
          }
        }
      }
    }
    return(res)
  })
  ########################################################################################
  ####                    Panel 'Spectral Analysis'
  ########################################################################################
  output$SPEC.selectvar <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    current.names <-  c("None", names(server.env$current.table))
    selectInput(inputId= "SPEC.selectvar1", label = "Current Variable",
                choices = current.names)
  })
  #
  output$SPEC.ar <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    checkboxInput(inputId = "SPEC.ar1", label = "Estimate AR(1) Spectrum", value = FALSE)
  })
  #
  output$SPEC.demean <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    checkboxInput(inputId = "SPEC.demean1", label = "Remove Mean?", value = FALSE)
  })
  #
  output$SPEC.detrend <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    checkboxInput(inputId = "SPEC.detrend1", label = "Remove Linear Trend?", value = FALSE)
  })
  #
  output$SPEC.xlim <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    textInput(inputId = "SPEC.xlim1", label = "Freq limits: ", value = paste("0.0", "0.5",
                                                                              sep=","))
  })
  #
  output$SPEC.ylim <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    textInput(inputId = "SPEC.ylim1", label = "Power limits: ", value = paste("0.0", "100",
                                                                               sep = ","))
  })
  #
  output$SPEC.run <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$SPEC.selectvar1) ||  input$SPEC.selectvar1 == "None")
      return(NULL)
    tmp <- actionButton(inputId = "SPEC.run1", label = "Run Spectral Analysis", icon = icon("play"))
  })
  #
  observeEvent(input$SPEC.run1, {
    run_spectral_analysis()
  })
  #
  output$SPEC.plot <- renderPlot({
    input$SPEC.run1
    current.table <- server.env$current.table
    res.mtm <- server.env$res.mtm
    res.mtm3 <- server.env$res.mtm3
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$SPEC.selectvar1) ||  input$SPEC.selectvar1 == "None")
      return(NULL)
    if(is.null(res.mtm))
      return(NULL)
    if(is.null(res.mtm3))
      return(NULL)
    #
    #print(names(res.mtm))
    xrng <- as.numeric(unlist(strsplit(input$SPEC.xlim1, ",")))
    yrng <- as.numeric(unlist(strsplit(input$SPEC.ylim1, ",")))
    current.title <- NULL
    if(input$SPEC.selectvar1 == "GR"){
      current.title <- "Gamma Ray: MTM"
    }
    else if(input$SPEC.selectvar1 == "NPHI"){
      current.title <- "Neutron Porosity: MTM"
    }
    else if(input$SPEC.selectvar1 == "RPS2"){
      current.title <- "Resistivity: MTM"
    }
    p1 <- ggplot() + geom_line(aes(x = Frequency, y = Power), data= res.mtm) +
      geom_line(aes(x = Frequency, y = AR1_99_power), data = res.mtm, col = "red") +
      xlim(xrng) +
      scale_y_log10(limits = yrng) +
      xlab("Frequency (cycles/ft)") +
      ggtitle(current.title) +
      theme_bw()
    return(p1)
  })
  #
  run_spectral_analysis <- function(){
    current.table <- server.env$current.table
    input$SPEC.run1
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$SPEC.selectvar1) ||  input$SPEC.selectvar1 == "None")
      return(NULL)
    current.var <- input$SPEC.selectvar1
    current.var1 <- NULL
    if(current.var == "RPS2"){
      current.var1 <- log10(current.table[current.var])
    }
    else{
      current.var1 <- current.table[current.var]
    }
    #print(current.var)
    ar <- input$SPEC.ar1
    demean <- input$SPEC.demean1
    detrend <- input$SPEC.detrend1
    #
    res.mtm <- mtm(cbind(current.table$DEPTH, current.var1),
                   tbw = 2, ar = ar, pl= 2, demean = demean, detrend = detrend,
                   output = 1, genplot = F)
    #
    res.mtm3 <- mtm(cbind(current.table$DEPTH, current.var1),
                    tbw = 2, ar = ar, pl= 2, demean = demean, detrend = detrend,
                    output = 3, genplot = F)
    #
    res.mtm3 <- res.mtm3 %>% mutate(Period.ft = 1/Frequency,
                                    Period.m = 0.3048*Period.ft,
                                    Period.Ratio = Period.m / min(Period.m))
    #
    server.env$res.mtm <- res.mtm
    server.env$res.mtm3 <- res.mtm3
  }
  #
  output$SPEC.view <- renderTable({
    input$SPEC.run1
    current.table <- server.env$current.table
    res.mtm3 <- server.env$res.mtm3
    if(is.null(current.table))
      return(NULL)
    if(is.null(res.mtm3))
      return(NULL)
    return(res.mtm3)
  })

  ########################################################################################
  ####                    Panel 'Wavelet Analysis'
  ########################################################################################
  output$WAVE.selectvar <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    current.names <-  c("None", names(server.env$current.table))
    selectInput(inputId= "WAVE.selectvar1", label = "Current Variable",
                choices = current.names)
  })
  #
  output$WAVE.type <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    selectInput(inputId = "WAVE.type1", label = "Mother Wavelet Type:",
                choices = c("None", "paul", "morlet"), selected = "None")
  })
  #
  output$WAVE.par <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    textInput(inputId = "WAVE.par1", label = "Wavelet Parameter: ", value = 0.0)
  })
  #(
  output$WAVE.lag <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    textInput(inputId = "WAVE.lag1", label = "AR(1) Lag: ", value = 0.0)
  })
  #
  output$WAVE.dosig <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    checkboxInput(inputId = "WAVE.dosig1", label = "Perform Significante Test?", value = TRUE)
  })
  #
  output$WAVE.siglvl <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    textInput(inputId = "WAVE.siglvl1", label = "Wavelet Significance Level: ", value = 0.95)
  })
  #
  output$WAVE.sigtest <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    selectInput(inputId = "WAVE.sigtest1", label = "Significance Test Type: ",
                choices = c("None", "0", "1", "2"), selected = "None")
  })
  #
  output$WAVE.run <- renderUI({
    current.table <- server.env$current.table
    if(is.null(current.table))
      return(NULL)
    actionButton(inputId = "WAVE.run1", label = "Run Wavelet Analysis", icon = icon("play"))
  })
  #
  observeEvent(input$WAVE.run1, {
    run_wavelet_analysis()
  })
  #
  run_wavelet_analysis <- function(){
    current.table <- server.env$current.table
    input$WAVE.run1
    if(is.null(current.table))
      return(NULL)
    if(is.null(input$WAVE.selectvar1) ||  input$WAVE.selectvar1 == "None")
      return(NULL)
    current.var <- input$WAVE.selectvar1
    if(input$WAVE.sigtest1 == "None")
      return(NULL)
    #print(current.var)
    mother <- input$WAVE.type1
    par <- as.numeric(input$WAVE.par1)
    lag <- as.numeric(input$WAVE.lag1)
    dosig <- input$WAVE.dosig1
    sig.level <- as.numeric(input$WAVE.siglvl1)
    sig.test <- as.integer(input$WAVE.sigtest1)
    #print(c(mother, par, lag, dosig, sig.level, sig.test))
    #print(mother)
    #if(mother == 2)
    #  mother = "paul"
    #if(mother == 3)
    #  mother <- "morlet"
    current.depth <- current.table$DEPTH
    current.value <-  log10(current.table[current.var])
    current.value1 <- loess.smooth(current.depth, current.value, span = .025, degree = 2,
                                   evaluation = length(current.depth))
    #print(cbind(current.depth, current.value))
    #print(cbind(current.value1$x, current.value1$y))
    #
    wt.res <- wt(cbind(current.value1$x, current.value1$y),
                 mother = mother, param = par, lag1 = lag,
                 sig.level = sig.level, sig.test = sig.test, do.sig = dosig)
    #
    server.env$wt.res <- wt.res
  }
  #
  output$WAVE.plot <- renderPlot({
    input$WAVE.run1
    current.table <- server.env$current.table
    wt.res <- server.env$wt.res
    if(is.null(current.table))
      return(NULL)
    if(is.null(wt.res))
      return(NULL)
    current.title <- paste0(input$WAVE.selectvar1,': ', input$WAVE.type1, ' Wavelet')
    plot(wt.res, main = current.title, plot.cb = F, plot.phase = F,
         type = "power.corr.norm", lwd.sig = 2, xlab = "Depth(ft)",
         ylab = "Cycle(ft)")
  })
})

