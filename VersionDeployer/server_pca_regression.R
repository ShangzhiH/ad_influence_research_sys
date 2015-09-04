## to get the name of chosen independent variable for PCA
VarExplicativeInputPCA <- eventReactive(input$CalculatePCA, {
  
  return(input$var_explicative)
  
})

## to get the name of chosen dependent variable for PCA
VarReponseInputPCA <- eventReactive(input$CalculatePCA, { 
  
  return(input$var_reponse)
})

## to get chosen PCA principal composant
VarInputPCA <- eventReactive(input$PCA_RegressionOK, {
  
  return(input$PCA_var)
  
})


## use the chosen variables to do the analysis PCA 
AnalysePCA <- observeEvent(input$CalculatePCA, {
  
  data1 = Data_Tempo$data[VarExplicativeInputPCA()]
  data2 = Data_Tempo$data[VarReponseInputPCA()]
  
  
  Data_PCA$data$PCA = prcomp(x = data1)
  Data_PCA$data$RegressionData = cbind(Data_PCA$data$PCA$x, data2)
  
  
  
})


## PCA proportion of each composant
output$PCA_Proportion <- renderChart2({
  VarAll = Data_PCA$data$PCA$sdev
  h = Highcharts$new()
  h$set(height = 600, width = 1500)
  n = length(VarAll)
  h$title(text = "Proportion of each principal composant")
  h$xAxis(categories = c(1:n))
  h$yAxis(title = list(text = "Proportion(%)"))
  
  h$series(name = "composant", data = cumsum(VarAll^2/(sum(VarAll^2)))*100)
  h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
  return(h)
})


##we can choose what principal variable you want 
output$PCA_VariableGenere <- renderUI({
  tryCatch({
    
    script = "choice = list('1'"
    for(i in c(2:length(VarExplicativeInputPCA()))) {
      script = paste(script, ",'",i,"'", sep = "")
    }
    script = paste(script, ")")
    eval(parse(text = script))
    
    checkboxGroupInput("PCA_var", 
                       label = h3(""), 
                       choices = choice)},error = function(e){})
  
})



## regression for pca
observeEvent(input$PCA_RegressionOK, {
  
  var = VarInputPCA()
  
  AllNames = names(Data_PCA$data$RegressionData)
  
  PCA_Regression_Model$data = glm(as.formula(paste(AllNames[length(AllNames)],'~', paste(AllNames[as.numeric(var)], collapse = '+'))), data = Data_PCA$data$RegressionData)
  

  
  
  ResiduValue$data$residual = unlist(unname(PCA_Regression_Model$data$residuals))
  ResiduValue$data$reponse = unlist(unname(Data_Tempo$data[VarReponseInputPCA()]))
  ResiduValue$data$regressionfitted = unlist(unname(PCA_Regression_Model$data$fitted.values))
  ResiduValue$data$date = as.character(unlist(unname(Data_Tempo$data$Date)))
  
  
})


## fitted result pca regression
output$PCA_Regression_Result <- renderChart2({
  if(!is.null(PCA_Regression_Model$data)){
    x = Data_Tempo$data$Date
    
    
    h = Highcharts$new()
    h$chart(zoomType = 'x')
    h$title(text = "PCA Regression result")
    h$set(height=600, width=1500)
    
    h$xAxis(title = list(text = 'Date'), categories = x,labels=list(enabled = TRUE, rotation = -90))
    h$yAxis(title = list(text ='Value'))
    
    
    h$series(name = 'Real value', data =  PCA_Regression_Model$data$data[,dim(PCA_Regression_Model$data$data)[2]])
    h$series(name = 'Fitted value', color = '#F32525', data = unname(PCA_Regression_Model$data$fitted.values))
    h$tooltip(shared = TRUE)
    h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h)
  }
  return(Highcharts$new())
})



## siginification of regressors, the same as normal regression
output$summary_table_PCA_Regression <- DT::renderDataTable({
  
  
  
  
  return(TableCoefDataPCA())
  # create an empty FlexTable
  #coef_ft = xtable(data, caption = "Coefficients du modele", label = NULL, align = NULL, digits = NULL,
  #                 display = NULL)
  
  
  
  
},options = list(lengthMenu = c(15, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500, scrollX = 500))

TableCoefDataPCA <- reactive({
  if(input$PCA_RegressionOK != 0) {
    Model = summary(PCA_Regression_Model$data)
    
    data = Model$coefficients
    data = as.data.frame(data)
    
    # get signif codes
    signif.codes = cut( data[,4]
                        , breaks = c( -Inf, 0.001, 0.01, 0.05, Inf)
                        , labels= c("xxx", "xx", "x", "" ) )
    
    
    # format the data values
    data[, 1] = formatC( data[, 1], digits=5, format = "f")
    data[, 2] = formatC( data[, 2], digits=5, format = "f")
    data[, 3] = formatC( data[, 3], digits=5, format = "f")
    data[, 4] = ifelse( data[, 4] < 0.001, "< 0.001", formatC( data[, 4], digits=5, format = "f"))
    # add signif codes to data
    data$Signif = signif.codes
    return(data)
  }
})
output$downloadtablePCA <- downloadHandler(
  filename = function() { paste('DataTable', '.csv', sep='') },
  content = function(file) {
    write.csv(TableCoefDataPCA(), file)
  }
)
output$download_tableCoef_PCA_Regression <- renderUI({
  
  downloadButton('downloadtablePCA', 'Download')
})


## precision, R2 for pca regression
output$PrecisionError_PCA_Regression <- renderText({
  model = PCA_Regression_Model$data
  
  error = Precision(unlist(unname(model$data[,dim(model$data)[2]])), unlist(unname(model$fitted.values)))
  
  error = formatC(error*100, digits=3, format = "f")
  
  script = paste("The MAP(mean absolute precison) is <b>", error, "%</b>", sep = '')
  
  
  R2 = 1 - (sum((unlist(unname(model$data[,dim(model$data)[2]]))-model$fitted.values)^2)/sum((unlist(unname(model$data[,dim(model$data)[2]]))-mean(unlist(unname(model$data[,dim(model$data)[2]]))))^2))
  R2 = formatC(R2, digits = 4, format = "f")
  script = paste(script, "</br>The R2 value is <b>", R2, "</b>")
  HTML(script)
  
})



## plot of diagnostic residu vs predict
output$Diagnostic1_PCA_Regression <- renderChart2({
  
  
  if(!is.null(PCA_Regression_Model$data)) {
    Model = PCA_Regression_Model$data
    
    h_diag1 = Highcharts$new()
    h_diag1$set(height=600, width=1500)
    h_diag1$chart(zoomType = 'xy')
    h_diag1$title(text = "Residual vs predicted y")
    
    
    
    h_diag1$xAxis(title = list(text = 'Predicted y'), labels=list(enabled = TRUE, rotation = -45))
    h_diag1$yAxis(title = list(text ='Residual'))
    
    script = "combinelist = list("
    
    data = cbind(unname(Model$fitted.values), unname(Model$residuals))
    script = paste(script, "list(data[1,1], data[1,2])")
    
    for(i in c(2:dim(data)[1])) {
      script = paste(script, ',list(data[',i,',1],data[',i,',2])')
    }
    
    script = paste(script, ")")
    eval(parse(text = script))
    
    
    
    h_diag1$series(name = "Residual vs predicted y", type = 'scatter', data = combinelist)
    h_diag1$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h_diag1)
  }
})



## plot of diagnostic residuStd vs predict
output$Diagnostic2_PCA_Regression <- renderChart2({
  
  
  if(!is.null(PCA_Regression_Model$data)) {
    
    Model = PCA_Regression_Model$data
    h_diag2 = Highcharts$new()
    h_diag2$set(height=600, width=1500)
    h_diag2$chart(zoomType = 'xy')
    h_diag2$title(text = "Scale Location")
    
    
    
    h_diag2$xAxis(title = list(text = 'Predicted y'), labels=list(enabled = TRUE, rotation = -45))
    h_diag2$yAxis(title = list(text ='Std.deviance.residual'))
    
    script = "combinelist = list("
    
    data = cbind(unname(Model$fitted.values), unname(sqrt(abs(rstandard(Model)))))
    script = paste(script, "list(data[1,1], data[1,2])")
    
    for(i in c(2:dim(data)[1])) {
      script = paste(script, ',list(data[',i,',1],data[',i,',2])')
    }
    
    script = paste(script, ")")
    eval(parse(text = script))
    
    
    
    h_diag2$series(name = "Scale Location", type = 'scatter', data = combinelist)
    h_diag2$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h_diag2)
  }
}) 

## plot of diagnostic QQplot
output$Diagnostic3_PCA_Regression <- renderChart2({
  
  
  if(!is.null(PCA_Regression_Model$data)) {
    
    Model = unlist(unname(rstandard(PCA_Regression_Model$data)))
    Model = Model[which(!is.na(Model))]
    
    h_diag3 = Highcharts$new()
    h_diag3$set(height=600, width=1500)
    h_diag3$chart(zoomType = 'xy')
    h_diag3$title(text = "Normal Q-Q")
    
    
    
    h_diag3$xAxis(title = list(text = 'Theorical quantiles'), labels=list(enabled = TRUE, rotation = -45))
    h_diag3$yAxis(title = list(text ='Std.deviance.residual'))
    
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
    
    
    
    h_diag3$series(name = "Actual value", type = 'scatter', data = combinelist)
    
    h_diag3$series(name = 'Theorical line', type = 'line', data = list(list(-3.5,-3.5), list(3.5,3.5)), marker = list(enabled = FALSE), enableMouseTracking = FALSE, dashStyle = 'shortdot')
    h_diag3$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h_diag3)
  }
})


## plot of diagnostic residu vs date
output$Diagnostic4_PCA_Regression <- renderChart2({
  
  
  if(!is.null(PCA_Regression_Model$data)) {
    Model = PCA_Regression_Model$data
    
    h_diag4 = Highcharts$new()
    h_diag4$set(height=600, width=1500)
    h_diag4$chart(zoomType = 'x')
    h_diag4$title(text = "residual vs date")
    
   
    
    h_diag4$xAxis(title = list(text = 'date'), categories = ResiduValue$data$date,labels=list(enabled = TRUE, rotation = -45))
    h_diag4$yAxis(title = list(text ='Value'))
    
    
    
    
    
    
    
    
    h_diag4$series(name = 'Residual', data = unlist(unname(Model$residuals)))
    h_diag4$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    
    return(h_diag4)
  }
})
