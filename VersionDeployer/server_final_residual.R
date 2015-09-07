## ACF of residual 
output$ModeleTS_Residu_ACF <- renderChart2({
  if(!is.null(TS_Model_Table$data)) {
    
    #data = unname(unlist(ResiduValue$data)) - TS_Model_Table$data$residuals
    
    
    data = TS_Model_Table$data$residuals
    data_acf = acf(data, plot = FALSE, lag.max = 50)
    hacf = Highcharts$new()
    hacf$chart(type = 'column', zoomType = 'x')
    
    hacf$set(height=600, width=1500)
    
    hacf$title(text = "The ACF Plot of residual")
    hacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
    
    
    
    hacf$xAxis(categories = data_acf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
    hacf$yAxis(title = list(text ='ACF value'), max = 1, min = -1)
    
    hacf$series(name = 'Residual', data = unlist(data_acf$acf), color = '#F32525')
    
    hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(2/sqrt(length(data)), length(unlist(data_acf$acf))))
    hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(-2/sqrt(length(data)), length(unlist(data_acf$acf))))
    
    
    hacf$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(hacf)
  }
  return(Highcharts$new())
})


## QQ plot of residual
output$ModeleTS_Residu_QQ <- renderChart2({
  if(!is.null(TS_Model_Table$data)) {
    
    #Model = unname(unlist(ResiduValue$data)) - TS_Model_Table$data$residuals
    Model = TS_Model_Table$data$residuals
    Model = Model/sd(Model)
    
    h = Highcharts$new()
    h$set(height=600, width=1500)
    h$chart(zoomType = 'xy')
    h$title(text = "Normal Q-Q")
    
    
    
    h$xAxis(title = list(text = 'Theorical quantiles'), labels=list(enabled = TRUE, rotation = -45))
    h$yAxis(title = list(text ='Std.deviance.residual'))
    
    script = "combinelist = list("
    
    l = length(Model) 
    data_quantile = qnorm((seq(1,l,1)-0.375)/(l+0.25))
    
    
    data = cbind(unname(data_quantile), sort(unlist(unname(Model))))
    
    script = paste(script, "list(data[1,1], data[1,2])")
    
    for(i in c(2:dim(data)[1])) {
      script = paste(script, ',list(data[',i,',1],data[',i,',2])')
    }
    
    script = paste(script, ")")
    eval(parse(text = script))
    
    
    
    h$series(name = "Actual value", type = 'scatter', data = combinelist)
    
    h$series(name = 'Theorical line', type = 'line', data = list(list(-3.5,-3.5), list(3.5,3.5)), marker = list(enabled = FALSE), enableMouseTracking = FALSE, dashStyle = 'shortdot')
    h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h)
  }
  return(Highcharts$new())
})


## ljungbox test
output$ModeleTS_Residu_LjungBox <- renderChart2({
  if(!is.null(TS_Model_Table$data)) {
    
    #data = as.numeric(unname(unlist(ResiduValue$data) - TS_Model_Table$data$residuals))
    data = as.numeric(unname(TS_Model_Table$data$residuals))
    pvalue = numeric(50)
    for(i in c(1:50)) {
      pvalue[i] = Box.test(data, lag = i)$p.value
    }
    
    h = Highcharts$new()
    h$set(height=600, width=1500)
    h$chart(zoomType = 'x')
    h$title(text = "Ljung Box Test")
    h$xAxis(title = list(text = 'lag'), labels=list(enabled = TRUE, rotation = 0))
    h$yAxis(title = list(text ='p-value'), max = 1, min = 0)
    
    data = cbind(c(1:50), unname(pvalue))
    
    
    script = "combinelist = list("
    script = paste(script, "list(data[1,1], data[1,2])")
    
    for(i in c(2:dim(data)[1])) {
      script = paste(script, ',list(data[',i,',1],data[',i,',2])')
    }
    
    script = paste(script, ")")
    eval(parse(text = script))
    
    
    
    h$series(name = "Ljung Box Test Value", type = 'scatter', data = combinelist)
    
    h$series(name = 'Theorical line', type = 'line', data = list(list(0, 0.05), list(50, 0.05)), marker = list(enabled = FALSE), enableMouseTracking = FALSE, dashStyle = 'shortdot')
    h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h)
    
  }
  return(Highcharts$new())
  
})





## fitted result in the last page
output$Result_Final <- renderChart2({
  
  if(!is.null(TS_Model_Table$data)) {
    
    dataoriginial = unname(unlist(ResiduValue$data$reponse))
    
    dataajuste = unname(unlist(ResiduValue$data$regressionfitted)) + as.numeric(unname(unlist(ResiduValue$data$residual) - TS_Model_Table$data$residuals))
    
    h = Highcharts$new()
    
    date = ResiduValue$data$date
    h$set(height=600, width=1500)
    h$chart(zoomType = 'x')
    h$title(text = "Residual")
    
    
    
    h$xAxis(title = list(text = 'date'), categories = date, labels=list(enabled = TRUE, rotation = -45))
    h$yAxis(title = list(text ='Value'))
    
    
    h$series(name = 'Real value', data = dataoriginial, color = '#1E61E6')
    h$series(name = 'Predict value', data = dataajuste, color = '#F81942')
    
    h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h)
  }
  return(Highcharts$new())
})




## residual displayed in the last page
output$ModeleTS_Residu_Final <- renderChart2({
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


## pie chart contribution
output$Contribution_Final <- renderChart2({
  if(!is.null(TS_Model_Table$data)) {
    Contributions = ResiduValue$data$Contributions
    
    
    
    h = Highcharts$new()
    h$set(height=600, width=1500)
    
    h$chart(options3d=list(enabled= TRUE, alpha= 45, beta= 0), type = 'pie')
    
    
    h$title(text = 'Contribution of different part')
    
    
    h$plotOptions(pie=list(allowPointSelect= TRUE,cursor= 'pointer',
                           dataLabels=list(
                             enabled= TRUE,
                             format= '<b>{point.name}</b>: {point.percentage:.1f} %'
                             
                           )
    ))
    
    
    
    
    script1 = paste("data1 = list(list(name='Baseline', y=", abs(sum(Contributions[,1])),"),",sep = "")
    script2 = "data2 = list("
    allmedia = 0
    
    for(i in c(2:dim(Contributions)[2])) {
      value = sum(Contributions[,i])
      allmedia = allmedia + value
      if(value<0) {
        name = paste(colnames(Contributions)[i],"(-)",sep="")
      }
      else {
        name = paste(colnames(Contributions)[i],"(+)",sep="")
      }
      
      if(i != dim(Contributions)[2]) {
        
        script2 = paste(script2, "list(name='",name,"',y=",abs(value),"),",sep="")
      }
      else {
        script2 = paste(script2, "list(name='",name,"',y=",abs(value),"))",sep="")
      }
    }
    
    script1 = paste(script1, "list(name='allmedia',y=", abs(allmedia), "),",sep="")
    script1 = paste(script1, "list(name='time series',y=",abs(sum(as.numeric(unname(unlist(ResiduValue$data$residual) - TS_Model_Table$data$residuals)))), "),",sep="")
    script1 = paste(script1, "list(name='UNKNOWN',y=", abs(sum(as.numeric(TS_Model_Table$data$residuals))), "))",sep="")
    
    eval(parse(text = script1))
    eval(parse(text =script2))
    
    h$series(size = 400, center= list(300, 300), name = "Contribution", colorByPoint=TRUE, data=data1)
    h$series(size = 400, center= list(1000, 300), name = "Contribution1", colorByPoint=TRUE, data=data2)
    h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    
    return(h)
  }
  return(Highcharts$new())
})


## final precision 
output$PrecisionErrorFinal <- renderText({
  if(!is.null(TS_Model_Table$data)) {
    dataoriginial = unname(unlist(ResiduValue$data$reponse))
    dataajuste = unname(unlist(ResiduValue$data$regressionfitted)) + as.numeric(unname(unlist(ResiduValue$data$residual) - TS_Model_Table$data$residuals))
    
    error = Precision(dataoriginial, dataajuste)
    
    error = formatC(error*100, digits=3, format = "f")
    
    script = paste("The MAP(mean absolute precison) is <b>", error, "%</b>", sep = '')
    
    
    
    HTML(script)
  }  
})