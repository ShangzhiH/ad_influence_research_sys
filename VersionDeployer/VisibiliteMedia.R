

library(MASS)
library(shiny)
library(shinyjs)
library(rCharts)
library(DT)
#library(shinyapps)
library(forecast)

## récupérer les données de la base de données
GET_DATA_FROM_BBD <- function(TableName = c('Table_Complet_Modele','Audi_Complet_Final')
                              ,AdvertiserName = 'Audi', ModeleVehicule = 'TOTAL', YearBegin = '2014', YearEnd = '2015') {
  
    library(RODBC)
    
    get_data <- function(table) { 
      #cette fonction donne les noms de variables et leur contenu 
      # d'un ficher importer.
      name = colnames(table)
      get_data = list(name = name, data = table[[1]])
    }
    
    
    CONNEXTION <- odbcConnect(dsn = "Shangzhi",uid = 'ShHuang', pwd = 'a19910707B')
    on.exit(odbcClose(CONNEXTION))
    
    if(ModeleVehicule != 'TOTAL') {
      SQuery = "SELECT a.annonceur,a.modelevehicule, a.date,a.DateYear,a.DateMonth,a.DateDay,a.NomJour,a.EstFerie,a.SpecificationJour,a.TTC_Gazole, a.Impressions_BRANDING,a.Impressions_ROI,a.Clicks_BRANDING,a.Clicks_ROI,a.Budget_Depense_NON_Cappe_BRANDING,a.Budget_Depense_NON_Cappe_ROI,a.INTERNET_DISPLAY_InvestissementsEnEuros,a.PRESSE_InvestissementsEnEuros_QUOT_AUTO,a.PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO,a.PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO,a.PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO,a.RADIO_InvestissementsEnEuros"
      SQuery = paste(SQuery, ",a.TV_NAT_InvestissementsEnEuros,a.TV_TNT_InvestissementsEnEuros")
      SQuery = paste(SQuery, ",(a.TV_NAT_InvestissementsEnEuros + a.TV_TNT_InvestissementsEnEuros) AS TV_InvestissementsEnEuros")

      SQuery = paste(SQuery, ",a.TV_NAT_GRP, a.TV_TNT_GRP")
      SQuery = paste(SQuery, ",(a.TV_NAT_GRP + a.TV_TNT_GRP) AS TV_GRP")
    
      SQuery = paste(SQuery, ", a.MOBILE_Volume_Achete, a.MOBILE_Net_Budget_LC, a.Investissement_Affichage")
    
      SQuery = paste(SQuery, ", a.INTERNET_DISPLAY_InvestissementsEnEuros_BMW") 
      SQuery = paste(SQuery, ", a.PRESSE_InvestissementsEnEuros_QUOT_AUTO_BMW, a.PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO_BMW, a.PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO_BMW, a.PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO_BMW,a.RADIO_InvestissementsEnEuros_BMW,a.TV_NAT_InvestissementsEnEuros_BMW,a.TV_NAT_GRP_BMW,a.TV_TNT_InvestissementsEnEuros_BMW,a.TV_TNT_GRP_BMW,a.Investissement_Affichage_BMW")	
      SQUERY = paste(SQuery, ", a.INTERNET_DISPLAY_InvestissementsEnEuros_MERCEDES, a.PRESSE_InvestissementsEnEuros_QUOT_AUTO_MERCEDES, a.PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO_MERCEDES, a.PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO_MERCEDES, a.PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO_MERCEDES, a.RADIO_InvestissementsEnEuros_MERCEDES, a.TV_NAT_InvestissementsEnEuros_MERCEDES, a.TV_NAT_GRP_MERCEDES, a.TV_TNT_InvestissementsEnEuros_MERCEDES, a.TV_TNT_GRP_MERCEDES, a.Investissement_Affichage_MERCEDES")


      SQuery = paste(SQuery, ",b.Nombre_inscription_S, b.Nombre_inscription_Non_S")
      SQuery = paste(SQuery, ",(b.Nombre_inscription_S + b.Nombre_inscription_Non_S) AS Nombre_inscription")
    
    
      SQuery = paste(SQuery, " FROM DM_1259_GroupMFrance.projetStage.", TableName[1],'a',sep = ' ')
      SQuery = paste(SQuery, " LEFT OUTER JOIN DM_1259_GroupMFrance.projetStage.", TableName[2], 'b', sep = ' ')
      SQuery = paste(SQuery, "ON (a.date = b.Date and a.modelevehicule = b.modelevehicule)")
    
      OrderBy = 'ORDER BY annonceur, modelevehicule, date'
    
    
      Where = paste("WHERE annonceur = '", AdvertiserName, "'",sep = '') 
    
      Where = paste(Where, "AND DateYear >=", YearBegin, "AND DateYear <= ",YearEnd, "AND a.modelevehicule = '", ModeleVehicule, "'", sep = '')
    
      SQuery = paste(SQuery, Where, OrderBy,  sep = ' ')
      
      DataSet =  sqlQuery(CONNEXTION, paste(SQuery))
      DataSet[which(is.na(DataSet[,45])),45] = 0
      DataSet[which(is.na(DataSet[,44])),44] = 0
      DataSet[which(is.na(DataSet[,43])),43] = 0
      

      
    }
    else if(ModeleVehicule == 'TOTAL') {
      SQuery = "SELECT a.annonceur,a.date,a.DateYear,a.DateMonth,a.DateDay,a.NomJour,a.EstFerie,a.SpecificationJour,a.TTC_Gazole, a.Impressions_BRANDING, a.Impressions_ROI, a.Clicks_BRANDING,a.Clicks_ROI,a.Budget_Depense_NON_Cappe_BRANDING,a.Budget_Depense_NON_Cappe_ROI, sum(a.INTERNET_DISPLAY_InvestissementsEnEuros) as INTERNET_DISPLAY_InvestissementsEnEuros, sum(a.PRESSE_InvestissementsEnEuros_QUOT_AUTO) as PRESSE_InvestissementsEnEuros_QUOT_AUTO,sum(a.PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO) as PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO,sum(a.PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO) as PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO,sum(a.PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO) as PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO,sum(a.RADIO_InvestissementsEnEuros) as RADIO_InvestissementsEnEuros"
      SQuery = paste(SQuery, ",sum(a.TV_NAT_InvestissementsEnEuros) as TV_NAT_InvestissementsEnEuros, sum(a.TV_TNT_InvestissementsEnEuros) as TV_TNT_InvestissementsEnEuros")
      SQuery = paste(SQuery, ",sum(a.TV_NAT_InvestissementsEnEuros + a.TV_TNT_InvestissementsEnEuros) AS TV_InvestissementsEnEuros")
      
      SQuery = paste(SQuery, ",sum(a.TV_NAT_GRP) as TV_NAT_GRP, sum(a.TV_TNT_GRP) as TV_TNT_GRP")
      SQuery = paste(SQuery, ",sum(a.TV_NAT_GRP + a.TV_TNT_GRP) AS TV_GRP")
      
      SQuery = paste(SQuery, ", avg(a.MOBILE_Volume_Achete) as MOBILE_Volume_Achete, avg(a.MOBILE_Net_Budget_LC) as MOBILE_Net_Budget_LC, sum(a.Investissement_Affichage) as Investissement_Affichage")
      
      SQuery = paste(SQuery, ", avg(a.INTERNET_DISPLAY_InvestissementsEnEuros_BMW) as INTERNET_DISPLAY_InvestissementsEnEuros_BMW") 
      SQuery = paste(SQuery, ", avg(a.PRESSE_InvestissementsEnEuros_QUOT_AUTO_BMW) as PRESSE_InvestissementsEnEuros_QUOT_AUTO_BMW, avg(a.PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO_BMW) as PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO_BMW, avg(a.PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO_BMW) as PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO_BMW, avg(a.PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO_BMW) as PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO_BMW, avg(a.RADIO_InvestissementsEnEuros_BMW) as RADIO_InvestissementsEnEuros_BMW, avg(a.TV_NAT_InvestissementsEnEuros_BMW) as TV_NAT_InvestissementsEnEuros_BMW, avg(a.TV_NAT_GRP_BMW) as TV_NAT_GRP_BMW, avg(a.TV_TNT_InvestissementsEnEuros_BMW) as TV_TNT_InvestissementsEnEuros_BMW, avg(a.TV_TNT_GRP_BMW) as TV_TNT_GRP_BMW, avg(a.Investissement_Affichage_BMW) as Investissement_Affichage_BMW")	
      SQuery = paste(SQuery, ", avg(a.INTERNET_DISPLAY_InvestissementsEnEuros_MERCEDES) as INTERNET_DISPLAY_InvestissementsEnEuros_MERCEDES, avg(a.PRESSE_InvestissementsEnEuros_QUOT_AUTO_MERCEDES) as PRESSE_InvestissementsEnEuros_QUOT_AUTO_MERCEDES, avg(a.PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO_MERCEDES) as PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO_MERCEDES, avg(a.PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO_MERCEDES) as PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO_MERCEDES, avg(a.PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO_MERCEDES) as PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO_MERCEDES, avg(a.RADIO_InvestissementsEnEuros_MERCEDES) as RADIO_InvestissementsEnEuros_MERCEDES, avg(a.TV_NAT_InvestissementsEnEuros_MERCEDES) as TV_NAT_InvestissementsEnEuros_MERCEDES, avg(a.TV_NAT_GRP_MERCEDES) as TV_NAT_GRP_MERCEDES, avg(a.TV_TNT_InvestissementsEnEuros_MERCEDES) as TV_TNT_InvestissementsEnEuros_MERCEDES, avg(a.TV_TNT_GRP_MERCEDES) as TV_TNT_GRP_MERCEDES, avg(a.Investissement_Affichage_MERCEDES) as Investissement_Affichage_MERCEDES")
      
      SQuery = paste(SQuery, ", avg(b.ConfigStarted) as ConfigStarted, avg(b.ConfigCompleted) as ConfigCompleted,avg(b.UniqueVisitor) as UniqueVisitor, avg(b.UtileVisitor) as UtileVisitor")
      SQuery = paste(SQuery, ",sum(b.Nombre_inscription_S) as Nombre_inscription_S, sum(b.Nombre_inscription_Non_S) as Nombre_inscription_Non_S")
      SQuery = paste(SQuery, ",sum(b.Nombre_inscription_S + b.Nombre_inscription_Non_S) AS Nombre_inscription")
      
      
      SQuery = paste(SQuery, " FROM DM_1259_GroupMFrance.projetStage.", TableName[1],'a',sep = ' ')
      SQuery = paste(SQuery, " LEFT OUTER JOIN DM_1259_GroupMFrance.projetStage.", TableName[2], 'b', sep = ' ')
      SQuery = paste(SQuery, "ON (a.date = b.Date and a.modelevehicule = b.modelevehicule)")
      
      OrderBy = 'ORDER BY annonceur, date'
      GroupBy = 'GROUP BY a.annonceur,a.date,a.DateYear,a.DateMonth,a.DateDay,a.NomJour,a.EstFerie,a.SpecificationJour,a.TTC_Gazole, a.Impressions_BRANDING, a.Impressions_ROI, a.Clicks_BRANDING,a.Clicks_ROI,a.Budget_Depense_NON_Cappe_BRANDING,a.Budget_Depense_NON_Cappe_ROI'
      
      Where = paste("WHERE annonceur = '", AdvertiserName, "'",sep = '') 
      
      Where = paste(Where, "AND DateYear >=", YearBegin, "AND DateYear <= ",YearEnd,  sep = '')
      
      SQuery = paste(SQuery, Where,GroupBy, OrderBy,  sep = ' ')
      
      DataSet =  sqlQuery(CONNEXTION, paste(SQuery))
      
    }
      
    #write.csv(DataSet, file = "Data_Audi.csv")
  
  
  #DataSet = read.csv("Data_Audi.csv")
  #DataSet = DataSet[,-1]
  #DataSet$date = sapply(DataSet$date, as.Date)
  
  return(DataSet)
}



## Analyse de régression
Stat_GLM <- function(DataSet, Explicatives, Reponses, Famille = gaussian(), Intercept = TRUE, TypeSelect = 'forward', Critere = 'AIC') {
  
  #ahistgrm = NULL
  #script = 'histgrm=list('
  #for(i in Explicatives) {
  #  if(is.numeric(DataSet[i][1,1])) {
  #    sous_script = paste(i, '=hist(plot = FALSE, DataSet$',i,')',sep='')
  #    
  #    eval(parse(text = sous_script))
  #  
  #    script = paste(script, i, '=', i, ',', sep = '')
  #  }
  #}
  
  #script = paste(substr(script, 1, nchar(script)-1), ')', sep = '')
  #eval(parse(text = script))
  
  input = DataSet
  explicative = DataSet[Explicatives]
  reponse = DataSet[Reponses]
  
  predict = data.frame(matrix(ncol = length(Reponses), nrow = dim(reponse)[1]))
  names(predict) = Reponses
  
  residual = data.frame(matrix(ncol = length(Reponses), nrow = dim(reponse)[1]))
  names(residual) = Reponses
  
  standardresidual = data.frame(matrix(ncol = length(Reponses), nrow = dim(reponse)[1]))
  names(standardresidual) = Reponses
  
  
  if(Intercept == TRUE) {
    intercept = ''
    coeff = data.frame(matrix(ncol = length(Reponses), nrow = length(Explicatives)+1))
    names(coeff) = Reponses
  }
  else {
    intercept = '0+'
    coeff = data.frame(matrix(ncol = length(Reponses), nrow = length(Explicatives)))
    names(coeff) = Reponses
  }
  
  
  for(i in Reponses) {
    
    model = glm(as.formula(paste(i,'~', intercept, paste(Explicatives, collapse = '+'), sep = '')), family = Famille, data = DataSet)
    
    if(Critere == 'AIC') {
      model1 = stepAIC(model, trace = 0, direction = TypeSelect)
    }
    else if(Critere == 'BIC') {
      model1 = stepBIC(model, trace = 0, direction = TypeSelect)
    }
    else {
      model1 = model;
    }
    
    
    predict[i] = model1$fitted.values
    residual[i] = residuals(model1)
    
    
    standardresidual[i] = rstandard(model1)
    
    k = 1
    if(Intercept == TRUE) {
      for(j in c("(Intercept)", Explicatives)) {
        if(sum(which(names(model1$coefficients) == j))) {
          coeff[i][k,1] = model1$coefficients[which(names(model1$coefficients) == j)]
          k = k+1
        }
        else {
          coeff[i][k,1] = 0
          k = k+1
        }
      }
      
    }
    else {
      for(j in Explicatives) {
        if(sum(which(names(model1$coefficients) == j))) {
          coeff[i][k,1] = model1$coefficients[which(names(model1$coefficients) == j)]
          k = k+1
        }
        else {
          coeff[i][k,1] = 0
          k = k+1
        }
      }
    }
    
  }
  
  
  list(model = model1, input = input, explicative = explicative, reponse = reponse, predict = predict, residual = residual, standardresidual = standardresidual, coeff = coeff)#, histgrm = histgrm)
}

## precision 
Precision <- function(DataOriginal, DataModele) {
  
  
  return(mean(abs((DataModele[which(DataOriginal>0)] - DataOriginal[which(DataOriginal>0)])/DataOriginal[which(DataOriginal>0)])))
}

## plot valeur ajuste vs valeur d'origine
Affichage_Ajuste <- function(Model) {
  
  x = Model$input$date
  
  
  
  h = Highcharts$new()
  h$chart(zoomType = 'x')
  h$title(text = "Regression result")
  #h$set(height=800, width=1000)
  
  h$xAxis(title = list(text = 'Date'), labels=list(enabled = TRUE, rotation = -45),type = 'datetime')
  h$yAxis(title = list(text ='Value'))
  h$series(name = 'Real value', data = Model$reponse[,1],pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
  h$series(name = 'Fitted value', color = '#F32525', data = Model$predict[,1],pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
  h$tooltip(shared = TRUE)
  
  
  
  return(h)
  
}


## plot contribution des medias
Affichage_Proportion <- function(Model) {
  
  namelist = c("annonceur"
               , "modelevehicule"
               , "date"
               , "DateYear"
               , "DateMonth"
               , "semaine"
               , "DateDay"
               , "NomJour"
               , "EstFerie"
               , "SpecificationJour")
  x = Model$input$date
  
  h1 = Highcharts$new()
  h1$chart(zoomType = 'x')
  #h1$plotOptions(area = list(stacking = 'percent',lineColor = '#ffffff', lineWidth = 2, marker = list(lineWidth = 1, lineColor = '#ffffff')))
  
  h1$tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.1f}</b><br/>', shared = TRUE)
  
  h1$title(text = "Media contribution for everyday")
  #h1$set(height=800, width=1000)
  
  
  h1$xAxis(title = list(text = "Date"),labels=list(enabled = TRUE, rotation = -45),type='datetime')
  h1$yAxis(list(list(title = list(text ="Contribution value")), list(title = list(text = "SUM"), opposite = TRUE)))
  
  
  
  
  h = Highcharts$new()
  
  h$chart(zoomType = 'x')
  h$plotOptions(area = list(stacking = 'percent',lineColor = '#ffffff', lineWidth = 2, marker = list(lineWidth = 1, lineColor = '#ffffff')))
  
  h$tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.1f}</b><br/>', shared = TRUE)
  
  h$title(text = "Media contribution by predict y")
  #h$set(height=800, width=1000)
  
  categories = sort(Model$predict[,1])
  
  
  h$xAxis(title = list(text = "predict y"), categories = categories, labels=list(enabled = FALSE))
  
  
  h$yAxis(title = list(text ='Contribution value'))
  
  
  if(dim(Model$explicative)[2] < dim(Model$coeff)[1]) {## there is an intercept
    intercept = 1
  }
  else {
    intercept = 0
  }
  
  for(i in names(Model$explicative)) {
    if(!(i %in% namelist)) {
      h$series(name = i, data = unname(unlist(Model$explicative[i])*Model$coeff[which(names(Model$explicative) == i)+intercept,])[order(Model$predict[,1])])
      h1$series(yAxis = 0, name = i, data = unname(unlist(Model$explicative[i])*Model$coeff[which(names(Model$explicative) == i)+intercept,]),pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
    }
  }
  #if(intercept == 1) {
  # 
  #    h$series(name = "Intercept", data = rep(x = Model$coeff[1,], each = length(categories)))
  #    h1$series(yAxis = 0, name = "Intercept", data = rep(x = Model$coeff[1,], each = length(categories)),pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
  #              
  #    
  #}
  
  h1$series(yAxis = 1, name = names(Model$reponse), data = unname(unlist(Model$predict)),pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
  
  
  h$exporting(enabled=T)
  h1$exporting(enabled=T)
  
  list(h=h, h1=h1)
} 


DEMO <- function(Tendance = FALSE) {
  
  ## Données
  Data_Stat = GET_DATA_FROM_BBD(TableName = c('Table_Complet','Audi_Complet_Final')
                                ,AdvertiserName = 'Audi', YearBegin = '2014', YearEnd = '2015')
  name = names(Data_Stat)
  ## Matrice de corrélation
  Corr = cor(Data_Stat[,c(-1,-2,-3,-4,-5)])
  Corr
  plot(Data_Stat[,c(-1,-2,-3,-4,-5)])
  
  Reponses = name[29]
  output = Stat_GLM(Data_Stat, Explicatives = c(name[c(seq(6,26,1))]), Critere = 'AIC',  Intercept = TRUE, TypeSelect = 'both', Reponses = Reponses, Famille = gaussian())
  summary(output$model)  
  plot(output$input$date, t(output$input[Reponses]), type = 'l')
  lines(output$input$date, t(output$predict[Reponses]), col ='red')
  text =paste("Precision(Data_Stat$",Reponses,", t(output$predict[Reponses]))", sep = '')
  eval(parse(text = text))
  
  plot(output$input$date, t(output$residual[Reponses]), type = 'l')
  
  plot(unlist(output$residual[Reponses])~unlist(output$predict[Reponses]))
}



