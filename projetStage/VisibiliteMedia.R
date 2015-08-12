getwd()
setwd('C:/Users/Shangzhi.Huang/Documents/Projet/projetStage/')

library(MASS)
library(shiny)
library(shinyjs)
library(rCharts)
library(DT)
library(forecast)

## récupérer les données de la base de données
GET_DATA_FROM_BBD <- function(TableName = c('Table_Complet','Audi_Complet_Final')
                              ,AdvertiserName = 'Audi', YearBegin = '2014', YearEnd = '2015') {
  function(){
  library(RODBC)
  
  get_data <- function(table) { 
    #cette fonction donne les noms de variables et leur contenu 
    # d'un ficher importer.
    name = colnames(table)
    get_data = list(name = name, data = table[[1]])
  }
  
  
  CONNEXTION <- odbcConnect(dsn = "Shangzhi",uid = 'ShHuang', pwd = 'a19910707B')
  on.exit(odbcClose(CONNEXTION))
  
  SQuery = "SELECT a.AdvertiserName,a.date,a.DateYear,a.DateMonth,a.DateDay,a.NomJour,a.EstFerie,a.SpecificationJour,a.TTC_Gazole, a.Impressions_BRANDING,a.Impressions_ROI,a.Clicks_BRANDING,a.Clicks_ROI,a.Budget_Depense_NON_Cappe_BRANDING,a.Budget_Depense_NON_Cappe_ROI,a.INTERNET_DISPLAY_InvestissementsEnEuros,a.PRESSE_InvestissementsEnEuros_QUOT_AUTO,a.PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO,a.PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO,a.PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO,a.RADIO_InvestissementsEnEuros"
  SQuery = paste(SQuery, ",a.TV_NAT_InvestissementsEnEuros,a.TV_TNT_InvestissementsEnEuros")
  SQuery = paste(SQuery, ",(a.TV_NAT_InvestissementsEnEuros + a.TV_TNT_InvestissementsEnEuros) AS TV_InvestissementsEnEuros")
  
  
  
                 
                 
  SQuery = paste(SQuery, ",a.TV_NAT_GRP, a.TV_TNT_GRP")
  SQuery = paste(SQuery, ",(a.TV_NAT_GRP + a.TV_TNT_GRP) AS TV_GRP")
  
  SQuery = paste(SQuery, ", a.MOBILE_Volume_Achete, a.MOBILE_Net_Budget_LC, a.Investissement_Affichage")
  
 
  
                  
                 
                 
  SQuery = paste(SQuery, ",b.ConfigCompleted, b.ConfigStarted, b.UniqueVisitor, b.UtileVisitor")
  
  SQuery = paste(SQuery, ",b.Nombre_inscription_S, b.Nombre_inscription_Non_S")
  SQuery = paste(SQuery, ",(b.Nombre_inscription_S + b.Nombre_inscription_Non_S) AS Nombre_inscription")
  
  
  
  
  
  
                 
  SQuery = paste(SQuery, " FROM DM_1259_GroupMFrance.projetStage.", TableName[1],'a',sep = ' ')
  SQuery = paste(SQuery, " INNER JOIN DM_1259_GroupMFrance.projetStage.", TableName[2], 'b', sep = ' ')
  SQuery = paste(SQuery, "ON (a.date = b.Date)")
  
  OrderBy = 'ORDER BY AdvertiserName, date'
  
 
  


  Where = paste("WHERE AdvertiserName = '", AdvertiserName, "'",sep = '') 
  
  Where = paste(Where, "AND DateYear >=", YearBegin, "AND DateYear <= ",YearEnd)
 
  SQuery = paste(SQuery, Where, OrderBy,  sep = ' ')
  
  DataSet =  sqlQuery(CONNEXTION, paste(SQuery))
  write.csv(DataSet, file = "Data_Audi.csv")
  }
  DataSet = read.csv("Data_Audi.csv")
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
  h$title(text = "A remplir")
  h$set(height=800, width=1000)
  
  h$xAxis(title = list(text = 'Date'), labels=list(enabled = TRUE, rotation = -45),type = 'datetime')
  h$yAxis(title = list(text ='A remplir'))
  h$series(name = 'Valeur reelle', data = Model$reponse[,1],pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
  h$series(name = 'Valeur ajuste', color = '#F32525', data = Model$predict[,1],pointStart = as.numeric(x[1])*86400000, pointInterval=24 * 3600 * 1000)
  h$tooltip(shared = TRUE)
  


  return(h)
          
}


## plot contribution des medias
Affichage_Proportion <- function(Model) {
  
  namelist = c("Impressions_BRANDING"  
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
               , "TV_TNT_InvestissementsEnEuros" 
               , "TV_InvestissementsEnEuros"
               , "TV_NAT_GRP"                                    
               , "TV_TNT_GRP"
               , "TV_GRP"                              
               , "MOBILE_Volume_Achete"
               , "MOBILE_Net_Budget_LC"                            
               , "Investissement_Affichage")
  x = Model$input$date
  
  h1 = Highcharts$new()
  h1$chart(zoomType = 'x')
  #h1$plotOptions(area = list(stacking = 'percent',lineColor = '#ffffff', lineWidth = 2, marker = list(lineWidth = 1, lineColor = '#ffffff')))
  
  h1$tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.1f}</b><br/>', shared = TRUE)
  
  h1$title(text = "Impact des medias par jour")
  h1$set(height=800, width=1000)
  
  
  h1$xAxis(title = list(text = "Date"),labels=list(enabled = TRUE, rotation = -45),type='datetime')
  h1$yAxis(list(list(title = list(text ="valeur de chaque partie")), list(title = list(text = "SUM"), opposite = TRUE)))
  
  
  
  
  h = Highcharts$new()
  
  h$chart(zoomType = 'x')
  h$plotOptions(area = list(stacking = 'percent',lineColor = '#ffffff', lineWidth = 2, marker = list(lineWidth = 1, lineColor = '#ffffff')))
  
  h$tooltip(pointFormat = '<span style="color:{series.color}">{series.name}</span>: <b>{point.y:.1f}</b><br/>', shared = TRUE)
  
  h$title(text = "Impact des medias")
  h$set(height=800, width=1000)
  
  categories = sort(Model$predict[,1])
  
  
  h$xAxis(title = list(text = "y'"), categories = categories, labels=list(enabled = FALSE))
  
  
  h$yAxis(title = list(text ='A remplir'))
  
  
  if(dim(Model$explicative)[2] < dim(Model$coeff)[1]) {## there is an intercept
    intercept = 1
  }
  else {
    intercept = 0
  }
  
  for(i in names(Model$explicative)) {
    if(i %in% namelist) {
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



