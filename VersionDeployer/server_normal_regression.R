## get all variable name of Data_Tempo
varnumber <- reactive({
  if(input$Extract != 0) {
    name = names(GetData())
    return(name)
  }
})


## use thess names to generate the choosing independent variable part
## then we can choose what variable you want 
output$VariableGenere <- renderUI({
  tryCatch({
    name = varnumber()
    
    
    script = "choices = list("
    for(i in name) {
      if(!(i %in% c("annonceur","modelevehicule","Date"))){##,"ConfigCompleted","ConfigStarted","UniqueVisitor","UtileVisitor","Nombre_inscription_S","Nombre_inscription_Non_S","Nombre_inscription"))) {
        script = paste(script,'"', i,'"', ",", sep = '')
      }
    }
    script = paste(substr(script, 1, nchar(script) - 1), ')')
    eval(parse(text = script))
    
    checkboxGroupInput("var_explicative", 
                       label = h3(""), 
                       choices = choices, selected = c(                                   
                         
                         
                         "PRESSE_InvestissementsEnEuros"        
                         
                         
                         , "RADIO_InvestissementsEnEuros"                   
                         , "TV_InvestissementsEnEuros"                    
                         
                         
                         
                         
                         , "MOBILE_Net_Budget_LC"                            
                         , "Investissement_Affichage"))},error = function(e){})
  
})


## here we can control the regression parameters
output$ParamModel <- renderUI({
  if(input$Modele_Statistique == 'GLM') {
    list(h5("Generalized linear model"),
         selectInput("Family", label = "family", choices = list('gaussian','Gamma','poisson','quasi'), selected = 'gaussian'),
         selectInput("Criterion", label = "criterion", choices = list('AIC', 'BIC', 'NONE'), selected = 'NONE'),
         selectInput("Stepwise", label = "stepwise", choices = list('forward', 'backward', 'both'), selected = 'both')
    )
  }
})




## generate the choosing dependent variable part
output$VariableReponseGenere <- renderUI({
  tryCatch({
    name = varnumber()
    
    
    script = "choices = list("
    for(i in name) {
      if(i %in% c("ConfigStarted","ConfigCompleted","UniqueVisitor","UtileVisitor","Nombre_inscription_S","Nombre_inscription_Non_S","Nombre_inscription")) {
        script = paste(script,"'", i,"'", ",", sep = '')
        
      }
    }
    script = paste(substr(script, 1, nchar(script) - 1), ')')
    eval(parse(text = script))
    
    return(radioButtons("var_reponse", 
                        label = h3(""), 
                        choices = choices, selected = choices[1]))},error = function(e){})
  
})


## when you click ok, the functon can get the name of chosen independent variable 
VarExplicativeInput <- eventReactive(input$calculate, {
  
  
  return(input$var_explicative)
  
})
## when you click ok, the functon can get the name of chosen dependent variable
VarReponseInput <- eventReactive(input$calculate, { 
  
  return(input$var_reponse)
})


## data is ready, regression is ready, predictor is ready, then caculate the regression model
observeEvent(input$calculate, {
  
  Famille = switch(input$Family,
                   "gaussian" = gaussian(),
                   "Gamma" = Gamma(),
                   "poisson" = poisson(),
                   "quasi" = quasi()
  )
  
  
  
  
  startrow = which(apply(Data_Tempo$data, MARGIN = 1, WhichRowHasNA))
  if(length(startrow) > 0) {
    Model = Stat_GLM(Data_Tempo$data[-(1:max(startrow)),], VarExplicativeInput(), VarReponseInput(), Famille = Famille, Intercept = TRUE, TypeSelect = input$Stepwise, Critere = input$Criterion)
  }
  else {
    Model = Stat_GLM(Data_Tempo$data, VarExplicativeInput(), VarReponseInput(), Famille = Famille, Intercept = TRUE, TypeSelect = input$Stepwise, Critere = input$Criterion)
  }
  
  Normal_Regression_Model$data = Model
  
  ResiduValue$data$residual = unlist(unname(Model$residual))
  ResiduValue$data$reponse = unlist(unname(Model$reponse))
  ResiduValue$data$regressionfitted = unlist(unname(Model$predict))
  ResiduValue$data$date = as.character(unlist(unname(Data_Tempo$data$Date)))
})



## normal regression, plot between the fitted value and original value 
output$resultchart <- renderChart2({ 
  
  
  if(!is.null(Normal_Regression_Model$data)) {
    
    h = Affichage_Ajuste(Normal_Regression_Model$data)
    h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h)
  }
  return(Highcharts$new())
  
  
})


## normal regression, contribution of different media
output$proportionchart <- renderChart2({
  
  if(!is.null(Normal_Regression_Model$data)) {
    h = Affichage_Proportion(Normal_Regression_Model$data)
    h$h$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    return(h$h)
  }
  return(Highcharts$new())
})

## normal regression, daily contribution of media 
output$proportionchartParDate <- renderChart2({
  
  if(!is.null(Normal_Regression_Model$data)) {
    h = Affichage_Proportion(Normal_Regression_Model$data)
    h$h1$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
      
    ResiduValue$data$Contributions = h$Contributions
    return(h$h1)
  }
  return(Highcharts$new())
})

## normal regression, model coefficient
TableCoefData <- reactive({
  if(input$calculate != 0) {
    
    Model = summary(Normal_Regression_Model$data$model)
    
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

## put the coefficent in a table to display
output$summary_table1 <- DT::renderDataTable({
  
  
  
  
  return(TableCoefData())
  # create an empty FlexTable
  #coef_ft = xtable(data, caption = "Coefficients du modele", label = NULL, align = NULL, digits = NULL,
  #                 display = NULL)
  
  
  
  
},options = list(lengthMenu = c(15, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500, scrollX = 500))

## function to download the table
output$downloadtable1 <- downloadHandler(
  filename = function() { paste('DataTable', '.csv', sep='') },
  content = function(file) {
    write.csv(TableCoefData(), file)
  }
)


output$download_tableCoef <- renderUI({
  
  downloadButton('downloadtable1', 'Download')
})



## same funtion, but for the table of correlation
output$correlation_table <- DT::renderDataTable({
  
  
  return(TableCorrData())
},filter = 'top',options = list(searchable = TRUE,lengthMenu = c(15, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500, scrollX = 500))

TableCorrData <- reactive({
  if(input$calculate != 0) {
    Data = cbind(Normal_Regression_Model$data$explicative,Normal_Regression_Model$data$reponse)
    
    
    for(i in names(Data)) {
      if(is.numeric(unlist(Data[i]))) {
        if(exists("CorData")) {
          CorData = cbind(CorData, Data[i])
        }
        else {
          CorData = Data[i]
        }
      }
    }
    
    Cor = cor(CorData)
    Cor = as.data.frame(Cor)
    return(Cor)
  }
})

output$downloadtable2 <- downloadHandler(
  filename = function() { paste('DataTable', '.csv', sep='') },
  content = function(file) {
    write.csv(TableCorrData(), file)
  }
)


output$download_tableCorr <- renderUI({
  
  downloadButton('downloadtable2', 'Download')
})


## calculate the precision and the R2 value
output$PrecisionError <- renderText({
  model = Normal_Regression_Model$data
  
  error = Precision(unlist(unname(model$reponse)), unlist(unname(model$predict)))
  
  error = formatC(error*100, digits=3, format = "f")
  
  script = paste("The MAP(mean absolute precison) is <b>", error, "%</b>", sep = '')
  
  
  R2 = 1 - (sum((model$reponse-model$predict)^2)/sum((model$reponse-mean(unlist(model$reponse)))^2))
  R2 = formatC(R2, digits = 4, format = "f")
  script = paste(script, "</br>The R2 value is <b>", R2, "</b>")
  HTML(script)
  
})


## plot of diagnostic residu vs predict
output$Diagnostic1 <- renderChart2({
  
  
  if(!is.null(Normal_Regression_Model$data)) {
    Model = Normal_Regression_Model$data$model
    
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
output$Diagnostic2 <- renderChart2({
  
  
  if(!is.null(Normal_Regression_Model$data)) {
    Model = Normal_Regression_Model$data
    h_diag2 = Highcharts$new()
    h_diag2$set(height=600, width=1500)
    h_diag2$chart(zoomType = 'xy')
    h_diag2$title(text = "Scale Location")
    
    
    
    h_diag2$xAxis(title = list(text = 'Predicted y'), labels=list(enabled = TRUE, rotation = -45))
    h_diag2$yAxis(title = list(text ='Std.deviance.residual'))
    
    script = "combinelist = list("
    
    data = cbind(unname(Model$predict), sqrt(abs(unname(Model$standardresidual))))
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
output$Diagnostic3 <- renderChart2({
  
  
  if(!is.null(Normal_Regression_Model$data)) {
    Model = Normal_Regression_Model$data$standardresidual
    Model = Model[which(!is.na(Model)),]
    
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
output$Diagnostic4 <- renderChart2({
  
  
  if(!is.null(Normal_Regression_Model$data)) {
    Model = Normal_Regression_Model$data
    browser()
    h_diag4 = Highcharts$new()
    h_diag4$set(height=600, width=1500)
    h_diag4$chart(zoomType = 'x')
    h_diag4$title(text = "residual vs date")
    
    
    
    h_diag4$xAxis(title = list(text = 'date'), categories = ResiduValue$data$date,labels=list(enabled = TRUE, rotation = -45))
    h_diag4$yAxis(title = list(text ='Value'))
    
    
    
    
    
    
    
    
    h_diag4$series(name = 'Residual', data = unlist(unname(Model$residual)))
    h_diag4$exporting(sourceWidth = 1500, sourceHeight = 600, scale = 15, enabled=T)
    
    return(h_diag4)
  }
  return(Highcharts$new())
})



## datatable to explorer and download
output$data_table <- renderDataTable({
  Data_Tempo$data
  
},filter = 'top',options = list(lengthMenu = c(15, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500, scrollX = 500))

output$downloadtable <- downloadHandler(
  filename = function() { paste('DataTable', '.csv', sep='') },
  content = function(file) {
    write.csv(Data_Tempo$data, file)
  }
)

## widget to download the table
output$download_table <- renderUI({
  
  downloadButton('downloadtable', 'Download')
})
