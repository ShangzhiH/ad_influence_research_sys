## extract the residual of regression
observeEvent(input$GetResidu, {
  
  
  
})

## if the residu is extracted
output$ResiduSuccess <- renderText({
  if(input$GetResidu == 0) {
    
    return("Calculating the residual")
    
  }
  else if(!is.null(ResiduValue$data)) {
    
    return("Residual has been extracted!")
    
  }
  else{
    return("Residual has not been extracted!")
  }
})

## display the residu
output$Residu <- renderChart2({
  
  if(!is.null(ResiduValue$data) && input$GetResidu != 0) {
    browser()
    date = ResiduValue$data$date
    
    h = Highcharts$new()
    h$set(height=600, width=1500)
    h$chart(zoomType = 'x')
    h$title(text = "residual")
    
    
    
    h$xAxis(title = list(text = 'date'), categories = date, labels=list(enabled = TRUE, rotation = -45))
    h$yAxis(title = list(text ='Residual'))
    
    h$series(name = 'Residual', data = unname(unlist(ResiduValue$data$residual)), color = '#F32525')
    h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    
    return(h)
  }
  return(Highcharts$new())})


## parameter to time series
output$ParamTSMode <- renderUI({
  switch(input$TSMode,
         "ARIMA" = list(h5("ARIMA:"),
                        textInput("TS1", label = "d", value = "0")
                        ,textInput("TS2", label = "p", value = "0")
                        ,textInput("TS3", label = "q", value = "0")),
         "SARIMA" = list(h5("SARIMA:"),
                         textInput("TS1", label = "d", value = "0"),
                         textInput("TS2", label = "p", value = "0"),
                         textInput("TS3", label = "q", value = "0"),
                         textInput("TS4", label = "D", value = "0"),
                         textInput("TS5", label = "P", value = "0"),
                         textInput("TS6", label = "Q", value = "0"),
                         textInput("TS7", label = "s", value = "7")))
  
  
})


## different situation
EventTS <- eventReactive(input$TSOK, {
  if(input$TSMode == "ARIMA" && input$TS2 == "0" && input$TS3 == "0") {
    return(0)
  }
  else if(input$TSMode == "ARIMA") {
    return(1)
  }
  else if(input$TSMode == "AUTO-ARIMA") {
    return(2)
  }
  else if(input$TSMode == "SARIMA" && input$TS2 == "0" && input$TS3 == "0" && input$TS5 == "0" && input$TS6 == "0") {
    return(3)
  }
  else if(input$TSMode == "SARIMA") {
    return(4)
  }
  else if(input$TSMode == "AUTO-SARIMA") {
    return(5)
  }
})


## get the input parameter and do some differenciation to see the acf and pacf 
observeEvent(input$TSOK,{
  
  if(EventTS() == 0) {
    
    diff = as.numeric(input$TS1)
    if(diff == 0) {
      ResiduApresTransformation$data = unname(unlist(ResiduValue$data$residual))
    }
    else {
      ResiduApresTransformation$data = diff(unname(unlist(ResiduValue$data$residual)), differences = diff)
    }
  }
  else if(EventTS() == 3) {
    diff1 = as.numeric(input$TS1)
    diff2 = as.numeric(input$TS4)
    
    if(diff1 == 0) {
      ResiduApresTransformation$data = unname(unlist(ResiduValue$data$residual))
    }
    else {
      ResiduApresTransformation$data = diff(unname(unlist(ResiduValue$data$residual)), differences = diff1)
    }
    
    if(diff2 == 0) {
      ResiduApresTransformation$data = unname(unlist(ResiduApresTransformation$data))
    }
    else {
      ResiduApresTransformation$data = diff(unname(unlist(ResiduApresTransformation$data)), differences = diff2*as.numeric(input$TS7))
    }
  }
})

## reset the diffrenciation
TSReset <- observeEvent(input$TSReset, {
  ResiduApresTransformation$data = unname(unlist(ResiduValue$data$residual))
  
})

## diffrenciation time series
output$ARIMAPLOT1 <- renderChart2({
  if(!is.null(ResiduApresTransformation$data)) {
    if(EventTS() == 0) {
      data = ResiduApresTransformation$data
      
      date = ResiduValue$data$date
      
      h1 = Highcharts$new()
      h1$set(height=600, width=1500)
      h1$chart(zoomType = 'x')
      h1$title(text = "residual")
      
      
      
      h1$xAxis(title = list(text = 'date'), categories = date, labels=list(enabled = TRUE, rotation = -45))
      h1$yAxis(title = list(text ='Differentiated residual'))
      
      h1$series(name = 'Residuals', data = data, color = '#1E61E6')
      h1$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
      
      
      
      return(h1)
    }
    else if(EventTS() == 3) {
      data = ResiduApresTransformation$data
      
      date = ResiduValue$data$date
      
      h1 = Highcharts$new()
      h1$set(height=600, width=1500)
      h1$chart(zoomType = 'x')
      h1$title(text = "Residual")
      
      
      
      h1$xAxis(title = list(text = 'Date'), categories=date, labels=list(enabled = TRUE, rotation = -45))
      h1$yAxis(title = list(text ='Differentiated residual'))
      
      h1$series(name = 'Residual', data = data, color = '#1E61E6')
      h1$exporting(enabled=T)
      
      
      
      return(h1)
    }
  }
  
  return(Highcharts$new())
})

## acf
output$ARIMAPLOT2 <- renderChart2({
  if(!is.null(ResiduApresTransformation$data)) {
    if(EventTS() == 0) {
      data = ResiduApresTransformation$data
      
      
      
      data_acf = acf(data, plot = FALSE, lag.max = 50)
      hacf = Highcharts$new()
      hacf$chart(type = 'column', zoomType = 'x')
      
      #hacf$set(height=400, width=600)
      
      hacf$title(text = "The ACF plot for differentiated residual")
      hacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
      
      
      
      hacf$xAxis(categories = data_acf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
      hacf$yAxis(title = list(text ='ACF value'), min = -1, max = 1)
      
      hacf$series(name = 'Residual', data = unlist(data_acf$acf), color = '#F32525')
      
      hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(2/sqrt(length(data)), length(unlist(data_acf$acf))))
      hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(-2/sqrt(length(data)), length(unlist(data_acf$acf))))
      
      
      hacf$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
      
      
      
      
      
      return(hacf)
    }
    else if(EventTS() == 3) {
      data = ResiduApresTransformation$data
      
      
      
      data_acf = acf(data, plot = FALSE, lag.max = 50)
      hacf = Highcharts$new()
      hacf$chart(type = 'column', zoomType = 'x')
      
      hacf$set(height=600, width=1500)
      
      hacf$title(text = "The ACF plot for differentiated residual")
      hacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
      
      
      
      hacf$xAxis(categories = data_acf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
      hacf$yAxis(title = list(text ='ACF value'), min = -1, max = 1)
      
      hacf$series(name = 'Residual', data = unlist(data_acf$acf), color = '#F32525')
      
      hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(2/sqrt(length(data)), length(unlist(data_acf$acf))))
      hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(-2/sqrt(length(data)), length(unlist(data_acf$acf))))
      
      
      hacf$exporting(enabled=T)
      
      
      
      
      
      return(hacf)
    }
    else {
      return(Highcharts$new())
    }
  }
  
  return(Highcharts$new())
})

## pacf
output$ARIMAPLOT3 <- renderChart2({
  if(!is.null(ResiduApresTransformation$data)) {
    if(EventTS() == 0) {
      data = ResiduApresTransformation$data
      
      
      
      data_pacf = pacf(data, plot = FALSE, lag.max = 50)
      hpacf = Highcharts$new()
      hpacf$chart(type = 'column', zoomType = 'x')
      
      hpacf$set(height=600, width=1500)
      
      hpacf$title(text = "The PACF plot for differentiated residual")
      hpacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
      
      
      
      hpacf$xAxis(categories = data_pacf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
      hpacf$yAxis(title = list(text ='Diiferentiated residual'), min = -1, max = 1)
      
      hpacf$series(name = 'Residual', data = unlist(data_pacf$acf), color = '#F32525')
      
      hpacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(2/sqrt(length(data)), length(unlist(data_pacf$acf))))
      hpacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(-2/sqrt(length(data)), length(unlist(data_pacf$acf))))
      
      
      hpacf$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
      
      
      
      
      
      return(hpacf)
    }
    else if(EventTS() == 3) {
      data = ResiduApresTransformation$data
      
      
      
      data_pacf = pacf(data, plot = FALSE, lag.max = 50)
      hpacf = Highcharts$new()
      hpacf$chart(type = 'column', zoomType = 'x')
      
      hpacf$set(height=600, width=1500)
      
      hpacf$title(text = "The PACF plot for differentiated residual")
      hpacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
      
      
      
      hpacf$xAxis(categories = data_pacf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
      hpacf$yAxis(title = list(text ='Differentiated residual'), min = -1, max = 1)
      
      hpacf$series(name = 'Residual', data = unlist(data_pacf$acf), color = '#F32525')
      
      hpacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(2/sqrt(length(data)), length(unlist(data_pacf$acf))))
      hpacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(-2/sqrt(length(data)), length(unlist(data_pacf$acf))))
      
      
      hpacf$exporting(enabled=T)
      
      
      
      
      
      return(hpacf)
    }
    else {
      return(Highcharts$new())
    }
  }
  
  return(Highcharts$new())
})


## calculate the time series model and display the fitted values
output$ModeleTS <- renderChart2({
  if(!is.null(ResiduValue$data)) {
    if(EventTS() == 1) {
      d = as.numeric(input$TS1)
      p = as.numeric(input$TS2)
      q = as.numeric(input$TS3)
      
      modele = arima(ResiduValue$data$residual, order = c(p,d,q))
      
      TS_Model_Table$data = modele
      date = ResiduValue$data$date
      
      
      h = Highcharts$new()
      h$set(height=600, width=1500)
      h$chart(zoomType = 'x')
      h$title(text = "Residual")
      
      
      
      h$xAxis(title = list(text = 'date'), categories = date, labels=list(enabled = TRUE, rotation = -45))
      h$yAxis(title = list(text ='Value'))
      
      h$series(name = 'Real value', data = unname(unlist(ResiduValue$data$residual)), color = '#1E61E6')
      h$series(name = 'Predict value', data = unname(unlist(ResiduValue$data$residual)) - as.numeric(modele$residuals), color = '#F81954')          
      h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
      return(h)
    }
    else if(EventTS() == 2) {
      
      modele = auto.arima(ResiduValue$data$residual, stepwise = FALSE, parallel = TRUE)
      
      TS_Model_Table$data = modele
      
      date = ResiduValue$data$date
      
      
      h = Highcharts$new()
      h$set(height=600, width=1500)
      h$chart(zoomType = 'x')
      h$title(text = "Residual")
      
      
      
      h$xAxis(title = list(text = 'date'), categories = date, labels=list(enabled = TRUE, rotation = -45))
      h$yAxis(title = list(text ='Value'))
      
      h$series(name = 'Real value', data = unname(unlist(ResiduValue$data$residual)), color = '#1E61E6')
      h$series(name = 'Predict value', data = unname(unlist(ResiduValue$data$residual))-as.numeric(modele$residuals), color = '#F81954')          
      h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
      h$tooltip(shared = TRUE)
      return(h)
    }
    else if(EventTS() == 4) {
      d = as.numeric(input$TS1)
      p = as.numeric(input$TS2)
      q = as.numeric(input$TS3)
      D = as.numeric(input$TS4)
      P = as.numeric(input$TS5)
      Q = as.numeric(input$TS6)
      s = as.numeric(input$TS7)
      
      modele = arima(ResiduValue$data$residual, order = c(p,d,q), seasonal = list(order = c(P,D,Q), period = s))
      
      TS_Model_Table$data = modele
      date = ResiduValue$data$date
      
      
      h = Highcharts$new()
      h$set(height=600, width=1500)
      h$chart(zoomType = 'x')
      h$title(text = "Residual")
      
      
      
      h$xAxis(title = list(text = 'date'), categories = date, labels=list(enabled = TRUE, rotation = -45))
      h$yAxis(title = list(text ='Value'))
      
      h$series(name = 'Real value', data = unname(unlist(ResiduValue$data$residual)), color = '#1E61E6')
      h$series(name = 'Predict value', data = unname(unlist(ResiduValue$data$residual))-as.numeric(modele$residuals), color = '#F81954')          
      h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
      return(h)
    }
    else if(EventTS() == 5) {
      
      modele = auto.arima(ResiduValue$data$residual, D = 1, stepwise = FALSE, parallel = TRUE, seasonal.test = "ocsb")
      
      TS_Model_Table$data = modele
      
      date = ResiduValue$data$date
      
      
      h = Highcharts$new()
      h$set(height=600, width=1500)
      h$chart(zoomType = 'x')
      h$title(text = "Residual")
      
      
      
      h$xAxis(title = list(text = 'date'), categories = date, labels=list(enabled = TRUE, rotation = -45))
      h$yAxis(title = list(text ='Value'))
      
      h$series(name = 'Real value', data = unname(unlist(ResiduValue$data$residual)), color = '#1E61E6')
      h$series(name = 'Predict value', data = unname(unlist(ResiduValue$data$residual))-as.numeric(modele$residuals), color = '#F81954')          
      h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
      h$tooltip(shared = TRUE)
      return(h)
    }
  }
  return(Highcharts$new())
})



## information about the used time series 
output$CalculTSMode <- renderText({
  if(EventTS() == 0) {
    
    return(paste("The difference of", input$TS1, "is calculated"))
    
  }
  else if(EventTS() == 1) {
    
    return(paste("The ARIMA (", input$TS2,",",input$TS1,",",input$TS3,") is calculated"))
    
  }
  else if(EventTS() == 2) {
    return("AUTO-ARIMA model is calculated")
  }
  else if(EventTS() == 3) {
    return(paste("The difference of SARIMA d = ",input$TS1,", D = ",input$TS4, ", s = ", input$TS7, "is calculated"))
  }
  else if(EventTS() == 4) {
    return(paste("The SARIMA (",input$TS2,",",input$TS1,",",input$TS3, ")*(", input$TS5, ",", input$TS4,",",input$TS6,") with season of", input$TS7))
  }
  else if(EventTS() == 5) {
    return("AUTO-SARIMA model is calculated")
  }
})




## time series coefficient table and download widget
output$TStable <- DT::renderDataTable({
  if(!is.null(TS_Model_Table$data)) {
    
    m = TS_Model_Table$data
    
    data = formatC(t(m$coef)[1,], digits = 4)
    
    
    
    
    data['d'] = m$arma[length(m$arma)-1]
    data['D'] = m$arma[length(m$arma)]
    data['s'] = m$arma[length(m$arma)-2]
    
    
    data['logLikelihood'] = formatC(m$loglik, digits = 4)
    data['sigma2'] = formatC(m$sigma2, digits = 4)
    data['aic'] = formatC(m$aic, digits = 4)
    
    as.data.frame(data)
  }
},options = list(paging = FALSE))

output$TSdownloadtable <- downloadHandler(
  filename = function() { paste('DataTable', '.csv', sep='') },
  content = function(file) {
    write.csv(TS_Model_Table, file)
  }
)

output$TSdownload_table <- renderUI({
  
  downloadButton('TSdownloadtable', 'Download')
})




## time series model residual
output$ModeleTS_Residu <- renderChart2({
  
  if(!is.null(TS_Model_Table$data)) {
    date = ResiduValue$data$date
    
    data = TS_Model_Table$data
    
    h = Highcharts$new()
    h$set(height=600, width=1500)
    h$chart(zoomType = 'x')
    h$title(text = "Residual")
    
    
    
    h$xAxis(title = list(text = 'date'), categories = date, labels=list(enabled = TRUE, rotation = -45))
    h$yAxis(title = list(text ='Value'))
    
    residu = as.numeric(unname(unlist(data$residuals)))
    h$series(name = 'Residual', data = residu, color = '#1E61E6')
    
    h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h)
  }
  else {
    return(Highcharts$new())
  }
})









