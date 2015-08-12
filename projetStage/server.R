shinyServer(
  function(input, output, session) {
  
    
    
    
    ## recuperer les variables explicative
    VarExplicativeInput <- eventReactive(input$calculate, {
   
        return(input$var_explicative)
      
    })
    
    
    ## recuperer la variable reponse
    VarReponseInput <- eventReactive(input$calculate, { 

        return(input$var_reponse)
    })
    
    Data_Tempo <- reactiveValues(data = NULL)
    
    
    ## recupere les donnees de la base de donnees
    GetData <- eventReactive(input$Extract,{



        Data = GET_DATA_FROM_BBD(TableName = c('Table_Complet','Audi_Complet_Final')
                          ,AdvertiserName = input$Marque, YearBegin = input$Annee[1], YearEnd = input$Annee[2])
        
        l = dim(Data)[1]
        
        Data$JourNormalise = (c(0:(l-1))%%365+1)/365
        
        Data$DateMonth = as.factor(Data$DateMonth)
        Data$DateDay = as.factor(Data$DateDay)
        Data$DateYear = as.factor(Data$DateYear)
        
        
        
        if(is.null(Data_Tempo$data)) {
          Data_Tempo$data = Data
        }
        
      
        return(Data)
      
    })
    
    
    ## si les donnees sont bien recuperees
    output$Success <- renderText({
      if(input$Extract == 0) {
        
        return("Choisir les parametres pour extraire les donnees")
        
      }
      else if(!is.null(GetData())) {
        
        return("Les donnees sont bien extraites!")
        
      }
    })
    
    
    ## l'affichage des choix des modeles de transformations
    output$param <- renderUI({
      
      
      
      if(!is.null(GetData())) {
        tryCatch({
        if(input$Transformation_Type %in% c("Linear")) {
          list(h5("Linear: f(x) = a0 + a1*x"),
              textInput("Param1", label = "a0", value = "0")
              ,textInput("Param2", label = "a1", value = "1"))
        }
        else if(input$Transformation_Type %in% c("Logged")) {
          
          list(h5("Logged: f(x) = a0 + a1*ln(x)"),textInput("Param1", label = "a0", value = "Entrez une valeur")
                 ,textInput("Param2", label = "a1", value = "Entrez une valeur"))
        }
          
        else if(input$Transformation_Type %in% c("Power")) {
          list(h5("Power: f(x) = a0 + a1*x^a2"),textInput("Param1", label = "a0", value = "Entrez une valeur")
               ,textInput("Param2", label = "a1", value = "Entrez une valeur")
               ,textInput("Param3", label = "a2", value = "Entrez une valeur"))
        }
        else if(input$Transformation_Type %in% c("Reciprocal")) {
          list(h5("Reciprocal: f(x) = a0 + a1/(x + a2)"),textInput("Param1", label = "a0", value = "Entrez une valeur")
                 ,textInput("Param2", label = "a1", value = "Entrez une valeur")
                 ,textInput("Param3", label = "a2", value = "Entrez une valeur"))
        }
        else if(input$Transformation_Type %in% c("Dimishing")) {
          list(h5("Dimishing: f(x) = a0 + a1*(1-exp(-x/a2))"), textInput("Param1", label = "a0", value = "Entrez une valeur")
                 ,textInput("Param2", label = "a1", value = "Entrez une valeur")
                 ,textInput("Param3", label = "a2", value = "Entrez une valeur"))
        }
          
        else if(input$Transformation_Type %in% c("AdBudg")) {
          list(h5("AdBudg: f(x) = a0 + a1*x^a3/(x^a3 + a2^a3)"),textInput("Param1", label = "a0", value = "Entrez une valeur")
               ,textInput("Param2", label = "a1", value = "Entrez une valeur")
               ,textInput("Param3", label = "a2", value = "Entrez une valeur")
               ,textInput("Param4", label = "a3", value = "Entrez une valeur"))
        }
      },error = function(e) {})}
        
    })
    
    
    ## si la transformation est comfirmee
    IsOK <- observeEvent(input$bOK, {
      param = NULL
      tryCatch({
        param = c(param, input$Param1)
      }, error = function(e){param = c(param, NULL)})
      tryCatch({
        param = c(param, input$Param2)
      }, error = function(e){param = c(param, NULL)})
      tryCatch({
        param = c(param, input$Param3)
      }, error = function(e){param = c(param, NULL)})
      tryCatch({
        param = c(param, input$Param4)
      }, error = function(e){param = c(param, NULL)})
      
      
      
      
      Data = GetData()
      
      x = Data$date
      y = unname(unlist(Data[input$Transformations]))
     
      y_t = Transform(input$Transformation_Type, y, as.numeric(param))$result
      
      #if(input$Memorisation_Type != "pas de memorisation") {
      #  y_t = Memorisation(input$Transformation_Type, y, c(0.5,0.5))
      #}

      Data_Tempo$data[input$Transformations] = y_t
      
     
    })
    
    
    ## reset la transformation ou pas
    IsReset <- observeEvent(input$bReset, {
      
      
      DataOrigine = GetData()
      
      Data_Tempo$data[input$Transformations] = DataOrigine[input$Transformations]
    
      
    })
    
    
    observe({
      if (input$Extract == 0) {
        shinyjs::disable("bOK")
        shinyjs::disable("bReset")
      } else {
        shinyjs::enable("bOK")
        shinyjs::enable("bReset")
      }
    })
    
    observe({
      if (input$bOK == 0) {
        shinyjs::disable("datedebut")
        shinyjs::disable("datefin")
      } else {
        shinyjs::enable("datedebut")
        shinyjs::enable("datefin")
        
      }
    })
    


    
    ## afficher le plot pour les donnees transformees
    output$Transformation_PLOT <- renderChart2({
      tryCatch({if(input$bOK == 0){
        
        return(Highcharts$new())
      }
      else{
        
        Data = GetData()
        
        x = Data$date
        
        y = unname(unlist(Data[input$Transformations]))
        
        y_t = unname(unlist(Data_Tempo$data[input$Transformations]))
        

        h = Highcharts$new()
        h$chart(zoomType = 'x')
        h$title(text = "A remplir")
        h$set(height=400, width=1000)
        
        
        
        h$xAxis(title = list(text = 'Date'), labels=list(enabled = TRUE, rotation = -45), type = 'datetime')
        h$yAxis(title = list(text ='A remplir'))
        
        
        
        
        
        
        h$series(name = 'Valeur reelle',  data = unname(y), pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
        h$series(name = 'Valeur transformee', color = '#F32525', data = unname(y_t),pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
        h$exporting(enabled=T)
        h$tooltip(shared = TRUE)
        return(h)
      }
    },error = function(e) {return(Highcharts$new())})
    })
    
    ## histo avant la transformation
    output$hist1 <- renderChart2({
      tryCatch({if(input$bOK == 0){
        
        return(Highcharts$new())
      }
      else{
        Data = GetData()
        
        x = Data$date
        
        y = unname(unlist(Data[input$Transformations]))
        
        
        
        
        histgramOld = Highcharts$new()
        histgramOld$chart(type = 'column')
        histgramOld$title(text = input$Transformations)
        histgramOld$set(height = 400, width = 600)
        histgramOld$plotOptions(column = list(pointPadding=0,borderWidth=0,groupPadding=0,shadow=TRUE))
        histdata = hist(y, plot = FALSE)
        
        
        histgramOld$xAxis(categories = histdata$mids)
        histgramOld$series(name = 'data original', data = histdata$density)
        
         
        
        histgramOld$series(color = '#F32525', enableMouseTracking = FALSE, dashStyle = 'shortdot',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = dnorm(histdata$mids, mean = mean(y), sd = sd(y)))
        histgramOld$exporting(enable=TRUE)
        return(histgramOld)
      }
      },error = function(e) {return(Highcharts$new())})
      
    })
    
    
    ## histo apres la transformation
    output$hist2 <- renderChart2({
      tryCatch({if(input$bOK == 0){
        
        return(Highcharts$new())
      }
        else{
          Data = GetData()
          
          x = Data$date
          
          
          
          y_t = unname(unlist(Data_Tempo$data[input$Transformations]))
          
          
          histgramNew = Highcharts$new()
          histgramNew$chart(type = 'column')
          histgramNew$title(text = input$Transformations)
          histgramNew$set(height = 400, width = 600)
          histgramNew$plotOptions(column = list(pointPadding=0,borderWidth=0,groupPadding=0,shadow=TRUE))
          histdata = hist(y_t, plot = FALSE)
          
          
          histgramNew$xAxis(categories = histdata$mids)
          histgramNew$series(name = 'data transforme', data = histdata$density)
          histgramNew$series(color = '#F32525', enableMouseTracking = FALSE, dashStyle = 'shortdot',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = dnorm(histdata$mids, mean = mean(y_t), sd = sd(y_t)))
          histgramNew$exporting(enable=TRUE)
          return(histgramNew)
        }
      },error = function(e) {return(Highcharts$new())})
      
    })
    
    
    ## si la memorisation est comfirmee
    output$paramMemo <- renderUI({
      
      
      
      if(!is.null(GetData())) {
        tryCatch({
          if(input$Memorisation_Type %in% c("Linear")) {
            list(h5("Linear: moyen mobile"),
                 textInput("ParamMemo1", label = "a0", value = 10))
          }
          else if(input$Memorisation_Type %in% c("LinearW")) {
            list(h5("Linear: moyen mobile pondere"),
                 textInput("ParamMemo1", label = "a0", value = 10))
          }
          else if(input$Memorisation_Type %in% c("Exponentiel")) {
            
            list(h5("Exponentiel: f(x) = x0*a0^(t)"),textInput("ParamMemo1", label = "a0", value = "0.5"))
          }
          else if(input$Memorisation_Type %in% c("Loess")) {
            list(h5("Loess: span"), textInput("ParamMemo1", label = "span", value = "0.66"))
          }
        },error = function(e) {})}
      
    })
    
    observe({
      if (input$Extract == 0) {
        shinyjs::disable("MemoOK")
        shinyjs::disable("MemoReset")
      } else {
        shinyjs::enable("MemoOK")
        shinyjs::enable("MemoReset")
      }
    })
    
    IsMemoOK <- observeEvent(input$MemoOK, {
      param = NULL
      tryCatch({
        param = c(param, input$ParamMemo1)
      }, error = function(e){param = c(param, NULL)})
      tryCatch({
        param = c(param, input$ParamMemo2)
      }, error = function(e){param = c(param, NULL)})
      tryCatch({
        param = c(param, input$ParamMemo3)
      }, error = function(e){param = c(param, NULL)})
      tryCatch({
        param = c(param, input$ParamMemo4)
      }, error = function(e){param = c(param, NULL)})
  
      Data = GetData()
      
      x = Data$date
      y = Data[input$Memorisation]
      
      y_t = Memorisation(input$Memorisation_Type, y, as.numeric(param))
      
      
      name = paste(input$Memorisation,"Memo", sep = '')
      Data_Tempo$data[name] = y_t
      
      
    })
    
    
    ## reset la transformation ou pas
    IsMemoReset <- observeEvent(input$MemoReset, {

      string = paste(input$Memorisation, 'Memo', sep = '')
      
      Data_Tempo$data[string] = NULL
      
    })
    
    
    output$Memorisation_PLOT <- renderChart2({
      tryCatch({if(input$MemoOK == 0){
        
        return(Highcharts$new())
      }
        else{
          
          x = Data_Tempo$data$date
          
          y = unname(unlist(Data_Tempo$data[input$Memorisation]))
          
          string = paste(input$Memorisation, "Memo", sep = '')
          y_t = unname(unlist(Data_Tempo$data[string]))
          
          
          h = Highcharts$new()
          h$chart(zoomType = 'x')
          h$title(text = "A remplir")
          h$set(height=400, width=1000)
          
          
          
          h$xAxis(title = list(text = 'Date'), labels=list(enabled = TRUE, rotation = -45), type = 'datetime')
          h$yAxis(title = list(text ='A remplir'))
          
          
          
          
          
          
          h$series(name = 'Valeur reelle',  data = unname(y), pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
          h$series(name = 'Valeur de la memorisation', color = '#F32525', data = unname(y_t),pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
          h$exporting(enabled=T)
          h$tooltip(shared = TRUE)
          return(h)
        }
      },error = function(e) {return(Highcharts$new())})
    })
    
    
    varnumber <- reactive({
      if(input$MemoOK != 0 || input$Extract != 0) {
        name = names(Data_Tempo$data)
        return(name)
      }
    })
    
    ## generer les variables explicatives a choisir
    
    output$VariableGenere <- renderUI({
      tryCatch({
      name = varnumber()
      possiblename = c("DateYear"                                        
                       , "DateMonth"                                       
                       , "DateDay"
                       , "JourNormalise"
                       , "NomJour"
                       , "EstFerie"                                         
                       , "SpecificationJour"  
                       , "TTC_Gazole"
                       , "Impressions_BRANDING"  
                       , "Impressions_ROI"                                 
                       , "Clicks_BRANDING"   
                       , "Clicks_ROI"                                      
                       , "Budget_Depense_NON_Cappe_BRANDING" 
                       , "Budget_Depense_NON_Cappe_ROI"                   
                       , "INTERNET_DISPLAY_InvestissementsEnEuros" 
                       , "PRESSE_InvestissementsEnEuros_QUOT_AUTO"        
                       , "PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO"  
                       , "PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO"    
                       , "PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO"
                       , "RADIO_InvestissementsEnEuros"                   
                       , "TV_NAT_InvestissementsEnEuros"                    
                       , "TV_NAT_GRP"                                    
                       , "TV_TNT_InvestissementsEnEuros" 
                       , "TV_TNT_GRP"
                       , "TV_InvestissementsEnEuros"
                       , "TV_GRP"
                       , "MOBILE_Volume_Achete"
                       , "MOBILE_Net_Budget_LC"                            
                       , "Investissement_Affichage"
                       , "TTC_GazoleMemo"
                       , "Impressions_BRANDINGMemo"  
                       , "Impressions_ROIMemo"                                 
                       , "Clicks_BRANDINGMemo"   
                       , "Clicks_ROIMemo"                                      
                       , "Budget_Depense_NON_Cappe_BRANDINGMemo" 
                       , "Budget_Depense_NON_Cappe_ROIMemo"                   
                       , "INTERNET_DISPLAY_InvestissementsEnEurosMemo" 
                       , "PRESSE_InvestissementsEnEuros_QUOT_AUTOMemo"        
                       , "PRESSE_InvestissementsEnEuros_NON_QUOT_AUTOMemo"  
                       , "PRESSE_InvestissementsEnEuros_QUOT_NON_AUTOMemo"    
                       , "PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTOMemo"
                       , "RADIO_InvestissementsEnEurosMemo"                   
                       , "TV_NAT_InvestissementsEnEurosMemo"                    
                       , "TV_NAT_GRPMemo"                                    
                       , "TV_TNT_InvestissementsEnEurosMemo" 
                       , "TV_TNT_GRPMemo"
                       , "TV_InvestissementsEnEurosMemo"
                       , "TV_GRPMemo"
                       , "MOBILE_Volume_AcheteMemo"
                       , "MOBILE_Net_Budget_LCMemo"                            
                       , "Investissement_AffichageMemo")
      script = "choices = list("
      for(i in name) {
        if(i %in% possiblename) {
          script = paste(script,"'", i,"'", ",", sep = '')
        }
      }
      script = paste(substr(script, 1, nchar(script) - 1), ')')
      eval(parse(text = script))
      
      checkboxGroupInput("var_explicative", 
                         label = h3(""), 
                         choices = choices, selected = c(                                   
                                                                                      "SpecificationJour"                               
                                                                                    , "Impressions_BRANDING"  
                                                                                    , "Impressions_ROI"                                 
                                                                                                                       
                                                                                    , "Budget_Depense_NON_Cappe_BRANDING" 
                                                                                    , "Budget_Depense_NON_Cappe_ROI"                   
                                                                                    , "INTERNET_DISPLAY_InvestissementsEnEuros" 
                                                                                    , "PRESSE_InvestissementsEnEuros_QUOT_AUTO"        
                                                                                    , "PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO"  
                                                                                    , "PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO"    
                                                                                    , "PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO"
                                                                                    , "RADIO_InvestissementsEnEuros"                   
                                                                                    , "TV_NAT_InvestissementsEnEuros"                    
                                                                                    , "TV_NAT_GRP"                                    
                                                                                    , "TV_TNT_InvestissementsEnEuros" 
                                                                                    , "TV_TNT_GRP"                                      
                                                                                    , "MOBILE_Volume_Achete"
                                                                                    , "MOBILE_Net_Budget_LC"                            
                                                                                    , "Investissement_Affichage"))},error = function(e){})
      
    })
    
    
    

    output$ParamModel <- renderUI({
      if(input$Modele_Statistique == 'GLM') {
        list(h5("Modele lineaire generalise"),
             selectInput("Family", label = "family", choices = list('gaussian','Gamma','poisson','quasi'), selected = 'gaussian')
        )
      }
    })
    ## calculer le modele de regression
    AnalyseStatistique <- eventReactive(input$calculate, {
      
          Famille = switch(input$Family,
                           "gaussian" = gaussian(),
                           "Gamma" = Gamma(),
                           "poisson" = poisson(),
                           "quasi" = quasi()
          )
        
          
          Model = Stat_GLM(Data_Tempo$data, VarExplicativeInput(), VarReponseInput(), Famille = Famille, Intercept = TRUE, TypeSelect = 'forward', Critere = '')
          
          return(Model)
       
      
    })
    
    
    ## plot entre la reponse ajustee et la reponse d'origine 
    output$resultchart <- renderChart2({ 
      
      
      if(!is.null(AnalyseStatistique())) {
        
        h = Affichage_Ajuste(AnalyseStatistique())
        h$exporting(enabled=T)
        return(h)
      }
      
      
    })
    
    ## contribution des media
    output$proportionchart <- renderChart2({
      
      if(!is.null(AnalyseStatistique())) {
        h = Affichage_Proportion(AnalyseStatistique())
        
        return(h$h)
      }
      
    })
    
    ## contribution des media par date
    output$proportionchartParDate <- renderChart2({
      
      if(!is.null(AnalyseStatistique())) {
        h = Affichage_Proportion(AnalyseStatistique())
        
        return(h$h1)
      }
      
    })
    
    
    
    ## siginification des regresseurs
    output$summary_table1 <- DT::renderDataTable({
      
      
      
      
      return(TableCoefData())
      # create an empty FlexTable
      #coef_ft = xtable(data, caption = "Coefficients du modele", label = NULL, align = NULL, digits = NULL,
      #                 display = NULL)
      
      
      
      
    },options = list(lengthMenu = c(15, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500, scrollX = 500))

    
    TableCoefData <- reactive({
      if(input$calculate != 0) {
        Model = summary(AnalyseStatistique()$model)
        
        data = Model$coefficients
        data = as.data.frame(data)
        
        # get signif codes
        signif.codes = cut( data[,4]
                            , breaks = c( -Inf, 0.001, 0.01, 0.05, Inf)
                            , labels= c("***", "**", "*", "" ) )
        
        
        # format the data values
        data[, 1] = formatC( data[, 1], digits=3, format = "f")
        data[, 2] = formatC( data[, 2], digits=3, format = "f")
        data[, 3] = formatC( data[, 3], digits=3, format = "f")
        data[, 4] = ifelse( data[, 4] < 0.001, "< 0.001", formatC( data[, 4], digits=5, format = "f"))
        # add signif codes to data
        data$Signif = signif.codes
        return(data)
      }
    })
    
    
    ## table de l'information de ecart_type
    #output$summary_table2 <- renderDataTable({
    #  Model = summary(AnalyseStatistique()$model)
    #  Dev = c(min(Model$deviance.resid), quantile(Model$deviance.resid, prob = c(0.25)), median(Model$deviance.resid), quantile(Model$deviance.resid, prob = c(0.75)), max(Model$deviance.resid))
    #  Dev = as.data.frame(Dev, row.names = c("Min", "1Q", "Median", "3Q", "Max"))
    #  colnames(Dev) = "Deviance Residuals"
    #  return(Dev)
    #  #DevRes = xtable(Dev, caption = "Info des Deviance Residuals")
    #},options = list(lengthMenu = c(15, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500, scrollX = 500))
    
    
    output$downloadtable1 <- downloadHandler(
      filename = function() { paste('DataTable', '.csv', sep='') },
      content = function(file) {
        write.csv(TableCoefData(), file)
      }
    )
    
    
    output$download_tableCoef <- renderUI({
      
      downloadButton('downloadtable1', 'Telecharger la table')
    })
    
    
    
    ## table de la correlation
    output$correlation_table <- DT::renderDataTable({
      
      
      return(TableCorrData())
    },options = list(searchable = FALSE,lengthMenu = c(15, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500, scrollX = 500))
    
    TableCorrData <- reactive({
      if(input$calculate != 0) {
        Data = cbind(AnalyseStatistique()$explicative,AnalyseStatistique()$reponse)
        
        
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
    
    ## wiget pour telecharger la table
    output$download_tableCorr <- renderUI({
      
      downloadButton('downloadtable2', 'Telecharger la table')
    })
    
    ## calculer la precision et la valeur de R2
    output$PrecisionError <- renderText({
      model = AnalyseStatistique()
      
      error = Precision(unlist(unname(model$reponse)), unlist(unname(model$predict)))
      
      error = formatC(error*100, digits=3, format = "f")
      
      script = paste("The MAP(mean absolute precison) is <b>", error, "%</b>", sep = '')
      
      
      R2 = 1 - (sum((model$reponse-model$predict)^2)/sum((model$reponse-mean(unlist(model$reponse)))^2))
      R2 = formatC(R2, digits = 4, format = "f")
      script = paste(script, "</br>The R2 value is <b>", R2, "</b>")
      HTML(script)
      
    })
    
    
    ## plot de diagnostic residu vs predict
    output$Diagnostic1 <- renderChart2({
      
   
      if(!is.null(AnalyseStatistique())) {
        Model = AnalyseStatistique()$model
        
        h_diag1 = Highcharts$new()
        h_diag1$set(height=500, width=700)
        h_diag1$chart(zoomType = 'xy')
        h_diag1$title(text = "residual vs predicted y")
        
       
        
        h_diag1$xAxis(title = list(text = 'predicted y'), labels=list(enabled = TRUE, rotation = -45))
        h_diag1$yAxis(title = list(text ='residual'))
        
        script = "combinelist = list("
        
        data = cbind(unname(Model$fitted.values), unname(Model$residuals))
        script = paste(script, "list(data[1,1], data[1,2])")
        
        for(i in c(2:dim(data)[1])) {
          script = paste(script, ',list(data[',i,',1],data[',i,',2])')
        }
        
        script = paste(script, ")")
        eval(parse(text = script))
        
        
        
        h_diag1$series(name = "observation", type = 'scatter', data = combinelist)
        h_diag1$exporting(enabled=T)
        return(h_diag1)
      }
    })
    
    ## plot de diagnostic residuStd vs predict
    output$Diagnostic2 <- renderChart2({
      
      
      if(!is.null(AnalyseStatistique())) {
        Model = AnalyseStatistique()
        h_diag2 = Highcharts$new()
        h_diag2$set(height=500, width=700)
        h_diag2$chart(zoomType = 'xy')
        h_diag2$title(text = "Scale Location")
        
        
        
        h_diag2$xAxis(title = list(text = 'predicted y'), labels=list(enabled = TRUE, rotation = -45))
        h_diag2$yAxis(title = list(text ='Std.deviance.residual'))
        
        script = "combinelist = list("
        
        data = cbind(unname(Model$predict), sqrt(abs(unname(Model$standardresidual))))
        script = paste(script, "list(data[1,1], data[1,2])")
        
        for(i in c(2:dim(data)[1])) {
          script = paste(script, ',list(data[',i,',1],data[',i,',2])')
        }
        
        script = paste(script, ")")
        eval(parse(text = script))
        
        
        
        h_diag2$series(name = "observation", type = 'scatter', data = combinelist)
        h_diag2$exporting(enabled=T)
        return(h_diag2)
      }
    }) 
    
    ## plot de diagnostic QQplot
    output$Diagnostic3 <- renderChart2({
      
      
      if(!is.null(AnalyseStatistique())) {
        Model = AnalyseStatistique()$standardresidual
        Model = Model[which(!is.na(Model)),]
       
        h_diag3 = Highcharts$new()
        h_diag3$set(height=500, width=700)
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
        
        
        
        h_diag3$series(name = "observation", type = 'scatter', data = combinelist)
        
        h_diag3$series(name = 'theorical line', type = 'line', data = list(list(-3.5,-3.5), list(3.5,3.5)), marker = list(enabled = FALSE), enableMouseTracking = FALSE, dashStyle = 'shortdot')
        h_diag3$exporting(enabled=T)
        return(h_diag3)
      }
    })
    
    
    ## plot de diagnostic residu vs date
    output$Diagnostic4 <- renderChart2({
      
      
      if(!is.null(AnalyseStatistique())) {
        Model = AnalyseStatistique()
        
        h_diag4 = Highcharts$new()
        h_diag4$set(height=500, width=700)
        h_diag4$chart(zoomType = 'x')
        h_diag4$title(text = "residual vs date")
        
        
        
        h_diag4$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
        h_diag4$yAxis(title = list(text ='residual'))
        
        
        
        
        
        
        
        
        h_diag4$series(name = 'Residuals', data = unlist(unname(Model$residual)),pointStart = as.numeric(Model$input$date[1])*86400000, pointInterval=24 * 3600 * 1000)
        h_diag4$exporting(enabled=T)
        
        return(h_diag4)
      }
    })
    
    
    ## datatable a exploiter et telecharger
    output$data_table <- renderDataTable({
      Data_Tempo$data
      
      },options = list(lengthMenu = c(15, 30, 50),  pageLength = 5, paging = FALSE, scrollY = 500, scrollX = 500))
    
    output$downloadtable <- downloadHandler(
      filename = function() { paste('DataTable', '.csv', sep='') },
      content = function(file) {
        write.csv(Data_Tempo$data, file)
      }
    )
    
    ## wiget pour telecharger la table
    output$download_table <- renderUI({
      
      downloadButton('downloadtable', 'Telecharger la table')
    })
    
    
    
    
    
    
    
    
    
    
    
    
    ResiduValue <- reactiveValues(data = NULL) 
    ResiduApresTransformation <- reactiveValues(data = NULL)
    
    
    observeEvent(input$GetResidu, {
       
        
        ResiduValue$data = AnalyseStatistique()$residual
      })
    
    output$ResiduSuccess <- renderText({
      if(input$GetResidu == 0) {
        
        return("En train de calculer le residu")
        
      }
      else if(!is.null(ResiduValue$data)) {
        
        return("Le residu est bien extrait!")
        
      }
    })
    
    output$Residu <- renderChart2({
      
      if(!is.null(ResiduValue$data)) {
        
        date = Data_Tempo$data$date
        
        h = Highcharts$new()
        h$set(height=400, width=600)
        h$chart(zoomType = 'x')
        h$title(text = "residual")
        
        
        
        h$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
        h$yAxis(title = list(text ='residual'))

        h$series(name = 'Residuals', data = unname(unlist(ResiduValue$data)), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#F32525')
        h$exporting(enabled=T)
        
        return(h)
      }
      return(Highcharts$new())})
    
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
    
    
    
    observeEvent(input$TSOK,{
      
     if(EventTS() == 0) {
        
        diff = as.numeric(input$TS1)
        if(diff == 0) {
          ResiduApresTransformation$data = unname(unlist(ResiduValue$data))
        }
        else {
          ResiduApresTransformation$data = diff(unname(unlist(ResiduValue$data)), differences = diff)
        }
     }
     else if(EventTS() == 3) {
       diff1 = as.numeric(input$TS1)
       diff2 = as.numeric(input$TS4)
       
       if(diff1 == 0) {
         ResiduApresTransformation$data = unname(unlist(ResiduValue$data))
       }
       else {
         ResiduApresTransformation$data = diff(unname(unlist(ResiduValue$data)), differences = diff1)
       }
       
       if(diff2 == 0) {
         ResiduApresTransformation$data = unname(unlist(ResiduApresTransformation$data))
       }
       else {
         ResiduApresTransformation$data = diff(unname(unlist(ResiduApresTransformation$data)), differences = diff2*as.numeric(input$TS7))
       }
     }

      
    })
    

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
    
    
    
    TSReset <- observeEvent(input$TSReset, {
      ResiduApresTransformation$data = unname(unlist(ResiduValue$data))
      
    })
    
    output$ARIMAPLOT1 <- renderChart2({
      if(!is.null(ResiduApresTransformation$data)) {
        if(EventTS() == 0) {
          data = ResiduApresTransformation$data
        
          date = Data_Tempo$data$date
      
          h1 = Highcharts$new()
          h1$set(height=400, width=600)
          h1$chart(zoomType = 'x')
          h1$title(text = "residual")
      
      
      
          h1$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
          h1$yAxis(title = list(text ='residual apres diff'))
      
          h1$series(name = 'Residuals', data = data, pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
          h1$exporting(enabled=T)
        
          
        
          return(h1)
        }
        else if(EventTS() == 3) {
          data = ResiduApresTransformation$data
          
          date = Data_Tempo$data$date
          
          h1 = Highcharts$new()
          h1$set(height=400, width=600)
          h1$chart(zoomType = 'x')
          h1$title(text = "residual")
          
          
          
          h1$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
          h1$yAxis(title = list(text ='residual apres diff'))
          
          h1$series(name = 'Residuals', data = data, pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
          h1$exporting(enabled=T)
          
          
          
          return(h1)
        }
      }
      
      return(Highcharts$new())
    })
    
    output$ARIMAPLOT2 <- renderChart2({
      if(!is.null(ResiduApresTransformation$data)) {
        if(EventTS() == 0) {
          data = ResiduApresTransformation$data
          
          
          
          data_acf = acf(data, plot = FALSE, lag.max = 50)
          hacf = Highcharts$new()
          hacf$chart(type = 'column', zoomType = 'x')
          
          hacf$set(height=400, width=600)
          
          hacf$title(text = "acf de residual apres diff")
          hacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
          
          
          
          hacf$xAxis(categories = data_acf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
          hacf$yAxis(title = list(text ='residual apres diff'), min = -1, max = 1)
          
          hacf$series(name = 'Residuals', data = unlist(data_acf$acf), color = '#F32525')
          
          hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(2/sqrt(length(data)), length(unlist(data_acf$acf))))
          hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(-2/sqrt(length(data)), length(unlist(data_acf$acf))))
          
          
          hacf$exporting(enabled=T)
          
          
          
          
          
          return(hacf)
        }
        else if(EventTS() == 3) {
          data = ResiduApresTransformation$data
          
          
          
          data_acf = acf(data, plot = FALSE, lag.max = 50)
          hacf = Highcharts$new()
          hacf$chart(type = 'column', zoomType = 'x')
          
          hacf$set(height=400, width=600)
          
          hacf$title(text = "acf de residual apres diff")
          hacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
          
          
          
          hacf$xAxis(categories = data_acf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
          hacf$yAxis(title = list(text ='residual apres diff'), min = -1, max = 1)
          
          hacf$series(name = 'Residuals', data = unlist(data_acf$acf), color = '#F32525')
          
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
    
    output$ARIMAPLOT3 <- renderChart2({
      if(!is.null(ResiduApresTransformation$data)) {
        if(EventTS() == 0) {
          data = ResiduApresTransformation$data
          
          
          
          data_pacf = pacf(data, plot = FALSE, lag.max = 50)
          hpacf = Highcharts$new()
          hpacf$chart(type = 'column', zoomType = 'x')
          
          hpacf$set(height=400, width=600)
          
          hpacf$title(text = "acf de residual apres diff")
          hpacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
          
          
          
          hpacf$xAxis(categories = data_pacf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
          hpacf$yAxis(title = list(text ='residual apres diff'), min = -1, max = 1)
          
          hpacf$series(name = 'Residuals', data = unlist(data_pacf$acf), color = '#F32525')
          
          hpacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(2/sqrt(length(data)), length(unlist(data_pacf$acf))))
          hpacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(-2/sqrt(length(data)), length(unlist(data_pacf$acf))))
          
          
          hpacf$exporting(enabled=T)
          
          
          
         
          
          return(hpacf)
        }
        else if(EventTS() == 3) {
          data = ResiduApresTransformation$data
          
          
          
          data_pacf = pacf(data, plot = FALSE, lag.max = 50)
          hpacf = Highcharts$new()
          hpacf$chart(type = 'column', zoomType = 'x')
          
          hpacf$set(height=400, width=600)
          
          hpacf$title(text = "acf de residual apres diff")
          hpacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
          
          
          
          hpacf$xAxis(categories = data_pacf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
          hpacf$yAxis(title = list(text ='residual apres diff'), min = -1, max = 1)
          
          hpacf$series(name = 'Residuals', data = unlist(data_pacf$acf), color = '#F32525')
          
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
    
   
    
    output$ModeleTS <- renderChart2({
      if(!is.null(ResiduValue$data)) {
        if(EventTS() == 1) {
          d = as.numeric(input$TS1)
          p = as.numeric(input$TS2)
          q = as.numeric(input$TS3)
          
          modele = arima(ResiduValue$data, order = c(p,d,q))
          
          TS_Model_Table$data = modele
          date = Data_Tempo$data$date
          
          
          h = Highcharts$new()
          h$set(height=400, width=600)
          h$chart(zoomType = 'x')
          h$title(text = "residual")
          
        
          
          h$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
          h$yAxis(title = list(text ='residual apres diff'))
          
          h$series(name = 'Origine', data = unname(unlist(ResiduValue$data)), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
          h$series(name = 'Predicted', data = as.numeric(modele$residuals), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#F81954')          
          h$exporting(enabled=T)
          return(h)
        }
        else if(EventTS() == 2) {
         
          modele = auto.arima(ResiduValue$data, stepwise = FALSE, parallel = TRUE)
          
          TS_Model_Table$data = modele
          
          date = Data_Tempo$data$date
          
          
          h = Highcharts$new()
          h$set(height=400, width=600)
          h$chart(zoomType = 'x')
          h$title(text = "residual")
          
          
          
          h$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
          h$yAxis(title = list(text ='residual apres diff'))
          
          h$series(name = 'Origine', data = unname(unlist(ResiduValue$data)), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
          h$series(name = 'Predicted', data = as.numeric(modele$residuals), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#F81954')          
          h$exporting(enabled=T)
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
          
          modele = arima(ResiduValue$data, order = c(p,d,q), seasonal = list(order = c(P,D,Q), period = s))
          
          TS_Model_Table$data = modele
          date = Data_Tempo$data$date
          
          
          h = Highcharts$new()
          h$set(height=400, width=600)
          h$chart(zoomType = 'x')
          h$title(text = "residual")
          
          
          
          h$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
          h$yAxis(title = list(text ='residual apres diff'))
          
          h$series(name = 'Origine', data = unname(unlist(ResiduValue$data)), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
          h$series(name = 'Predicted', data = as.numeric(modele$residuals), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#F81954')          
          h$exporting(enabled=T)
          return(h)
        }
        else if(EventTS() == 5) {
          
          modele = auto.arima(ResiduValue$data, D = 1, stepwise = FALSE, parallel = TRUE, seasonal.test = "ocsb")
          
          TS_Model_Table$data = modele
          
          date = Data_Tempo$data$date
          
          
          h = Highcharts$new()
          h$set(height=400, width=600)
          h$chart(zoomType = 'x')
          h$title(text = "residual")
          
          
          
          h$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
          h$yAxis(title = list(text ='residual apres diff'))
          
          h$series(name = 'Origine', data = unname(unlist(ResiduValue$data)), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
          h$series(name = 'Predicted', data = as.numeric(modele$residuals), pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#F81954')          
          h$exporting(enabled=T)
          h$tooltip(shared = TRUE)
          return(h)
        }
      }
      return(Highcharts$new())
    })
    
    TS_Model_Table <- reactiveValues(data = NULL)
    
    
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
      
      downloadButton('TSdownloadtable', 'Telecharger la table')
    })
    
    output$ModeleTS_Residu <- renderChart2({
     
      if(!is.null(TS_Model_Table$data)) {
        date = Data_Tempo$data$date
      
        data = TS_Model_Table$data
      
        h = Highcharts$new()
        h$set(height=400, width=600)
        h$chart(zoomType = 'x')
        h$title(text = "residual")
      
      
      
        h$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
        h$yAxis(title = list(text ='residual apres diff'))
      
        residu = as.numeric(unname(unlist(ResiduValue$data)) - unname(unlist(data$residuals)))
        h$series(name = 'Origine', data = residu, pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
      
        h$exporting(enabled=T)
        return(h)
      }
      else {
        return(Highcharts$new())
      }
    })
    
    output$ModeleTS_Residu_Final <- renderChart2({
      if(!is.null(TS_Model_Table$data)) {
        date = Data_Tempo$data$date
        
        data = TS_Model_Table$data
        
        h = Highcharts$new()
        h$set(height=400, width=600)
        h$chart(zoomType = 'x')
        h$title(text = "residual")
        
        
        
        h$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
        h$yAxis(title = list(text ='residual apres diff'))
        
        residu = as.numeric(unname(unlist(ResiduValue$data)) - unname(unlist(data$residuals)))
        h$series(name = 'Origine', data = residu, pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
        
        h$exporting(enabled=T)
        return(h)
      }
      else {
        return(Highcharts$new())
      }
    })
    
    output$ModeleTS_Residu_ACF <- renderChart2({
      if(!is.null(TS_Model_Table$data)) {
        
        data = unname(unlist(ResiduValue$data)) - TS_Model_Table$data$residuals
        
        data_acf = acf(data, plot = FALSE, lag.max = 50)
        hacf = Highcharts$new()
        hacf$chart(type = 'column', zoomType = 'x')
        
        hacf$set(height=400, width=600)
        
        hacf$title(text = "acf de residual apres diff")
        hacf$plotOptions(column = list(pointPlacement = "on",  pointWidth = 0, pointPadding=0,borderWidth=0,groupPadding=1,shadow=FALSE))
        
        
        
        hacf$xAxis(categories = data_acf$lag[,1,1], labels=list(align = 'left', enabled = TRUE))
        hacf$yAxis(title = list(text ='residual apres diff'), max = 1, min = -1)
        
        hacf$series(name = 'Residuals', data = unlist(data_acf$acf), color = '#F32525')
        
        hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(2/sqrt(length(data)), length(unlist(data_acf$acf))))
        hacf$series(color = "#A5BFF2", enableMouseTracking = FALSE, dashStyle = 'shortdash',marker = list(enabled = FALSE),type = 'spline', name = 'distribution normale', data = rep(-2/sqrt(length(data)), length(unlist(data_acf$acf))))
        
        
        hacf$exporting(enabled=T)
        return(hacf)
      }
      return(Highcharts$new())
    })
    
    output$ModeleTS_Residu_QQ <- renderChart2({
      if(!is.null(TS_Model_Table$data)) {
        
        Model = unname(unlist(ResiduValue$data)) - TS_Model_Table$data$residuals
        Model = Model/sd(Model)
        
        h = Highcharts$new()
        h$set(height=500, width=700)
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
        
        
        
        h$series(name = "observation", type = 'scatter', data = combinelist)
        
        h$series(name = 'theorical line', type = 'line', data = list(list(-3.5,-3.5), list(3.5,3.5)), marker = list(enabled = FALSE), enableMouseTracking = FALSE, dashStyle = 'shortdot')
        h$exporting(enabled=T)
        return(h)
      }
      return(Highcharts$new())
    })
    
    output$ModeleTS_Residu_LjungBox <- renderChart2({
      if(!is.null(TS_Model_Table$data)) {
        
        data = as.numeric(unname(unlist(ResiduValue$data) - TS_Model_Table$data$residuals))
        pvalue = numeric(50)
        for(i in c(1:50)) {
          pvalue[i] = Box.test(data, lag = i)$p.value
        }
        
        h = Highcharts$new()
        h$set(height=500, width=700)
        h$chart(zoomType = 'x')
        h$title(text = "Ljung Box")
        h$xAxis(title = list(text = 'lag'), labels=list(enabled = TRUE, rotation = 0))
        h$yAxis(title = list(text ='pvalue'), max = 1, min = 0)
        
        data = cbind(c(1:50), unname(pvalue))
        
        
        script = "combinelist = list("
        script = paste(script, "list(data[1,1], data[1,2])")
        
        for(i in c(2:dim(data)[1])) {
          script = paste(script, ',list(data[',i,',1],data[',i,',2])')
        }
        
        script = paste(script, ")")
        eval(parse(text = script))
        
        
        
        h$series(name = "observation", type = 'scatter', data = combinelist)
        
        h$series(name = 'theorical line', type = 'line', data = list(list(0, 0.05), list(50, 0.05)), marker = list(enabled = FALSE), enableMouseTracking = FALSE, dashStyle = 'shortdot')
        h$exporting(enabled=T)
        return(h)
        
      }
      return(Highcharts$new())
      
    })
    
    output$Result_Final <- renderChart2({
      
      if(!is.null(TS_Model_Table$data)) {
        
        dataoriginial = unname(unlist(AnalyseStatistique()$reponse))
        dataajuste = unname(unlist(AnalyseStatistique()$predict + as.numeric(TS_Model_Table$data$residuals)))
        
        h = Highcharts$new()
        
        date = Data_Tempo$data$date
        h$set(height=400, width=600)
        h$chart(zoomType = 'x')
        h$title(text = "residual")
        
        
        
        h$xAxis(type = "datetime", title = list(text = 'date'), labels=list(enabled = TRUE, rotation = -45))
        h$yAxis(title = list(text ='residual apres diff'))
        
        
        h$series(name = 'Origine', data = dataoriginial, pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#1E61E6')
        h$series(name = 'predi', data = dataajuste, pointStart = as.numeric(date[1])*86400000, pointInterval=24 * 3600 * 1000, color = '#F81942')
        
        h$exporting(enabled=T)
        return(h)
      }
      return(Highcharts$new())
    })
    
    output$PrecisionErrorFinal <- renderText({
      if(!is.null(TS_Model_Table$data)) {
        dataoriginial = unname(unlist(AnalyseStatistique()$reponse))
        dataajuste = unname(unlist(AnalyseStatistique()$predict + as.numeric(TS_Model_Table$data$residuals)))
      
        error = Precision(dataoriginial, dataajuste)
      
        error = formatC(error*100, digits=3, format = "f")
      
        script = paste("The MAP(mean absolute precison) is <b>", error, "%</b>", sep = '')
      
      
  
        HTML(script)
      }  
    })
    
    
})
    
    
    
    
    
    
