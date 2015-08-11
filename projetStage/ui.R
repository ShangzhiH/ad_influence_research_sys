source("VisibiliteMedia.R")
source("FonctionTransform.R")

shinyUI( 
  
   
  navbarPage("Analyse statistique",
    
    
    tabPanel("Exploitation des donnees",
      fluidPage(
        shinyjs::useShinyjs(), 
        
        fluidRow(
          column(2,
                 selectInput("Marque", 
                             label = "Choisir un marque",
                             choices = list("Audi"),
                             selected = "Audi")),
          column(2,
                 sliderInput("Annee", label = "Annee", step = 1, min = 2014, max = 2015, value = c(2014,2015)))
        ),
        
        fluidRow(
          column(1,
                 actionButton("Extract", label = "Extract")),
          column(11,
                 verbatimTextOutput("Success"))
        ),
    
          
        
        fluidRow(
          column(2,
                 selectInput("Transformations", 
                             label = "Choisir une variable a transformer",
                             choices = list("Impressions_BRANDING"  
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
                                            , "Investissement_Affichage"
                                            , "ConfigStarted"
                                            , "ConfigCompleted"                               
                                            , "UniqueVisitor"                                  
                                            , "UtileVisitor" 
                                            , "Nombre_inscription_S"
                                            , "Nombre_inscription_Non_S"
                                            , "Nombre_inscription")),
                 fluidRow(
                   column(12,
                          selectInput("Transformation_Type", label = "Choisir la transformation",
                                      choices = list('Linear', 'Logged', 'AdBudg', 'Power', 'Reciprocal', 'Dimishing'), selected = "Linear"))
                   
                 ),
                 
                 fluidRow(
                   column(12,
                          uiOutput("param"),
                          column(6,
                            actionButton("bOK", label = "OK")),
                          column(6,
                            actionButton("bReset", label = "Reset"))
                  )),
                 fluidRow(
                    column(12,
                           selectInput("Memorisation", 
                                       label = "Choisir une variable a calculer sa memorisation",
                                       choices = list("Impressions_BRANDING"  
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
                                                      , "Investissement_Affichage")),
                           fluidRow(
                             column(12,
                                    selectInput("Memorisation_Type", label = "Choisir la memorisation",
                                                choices = list('Loess','Linear','LinearW'), selected = "Linear"))
                             
                           ),
                           fluidRow(
                             column(12,
                                    uiOutput("paramMemo"),
                                    column(6,
                                           actionButton("MemoOK", label = "OK")),
                                    column(6,
                                           actionButton("MemoReset", label = "Reset"))
                                    
                             ))
                    ))),
          
            column(10,
                   #includeHTML("C:/Users/Shangzhi.Huang/Documents/Projet/projetStage/test.html"),
                   fluidRow(
                     column(12,showOutput("Transformation_PLOT", "highcharts"),
                            fluidRow(
                              
                              column(3,
                                     showOutput("hist1","highcharts")),
                              column(3,offset = 4,
                                     showOutput("hist2","highcharts"))
                    ))),
                   fluidRow(
                     column(12, showOutput("Memorisation_PLOT", "highcharts"))
                   )))
                
        )),
        
    
    
    tabPanel("Modelisation",
      fluidPage(
        fluidRow(
          column(3,
            h3("Choisir les variables explicatives"),
            fluidRow(
              column(12,
        
                uiOutput("VariableGenere"),     
      
        
                      fluidRow(
                        column(12, h3("Choisir les variables reponses")),
                          fluidRow(
                            column(12,
                              radioButtons("var_reponse", label = h3(""), choices = list("ConfigStarted"
                                                         , "ConfigCompleted"                               
                                                         , "UniqueVisitor"                                  
                                                         , "UtileVisitor"
                                                         , "Nombre_inscription_S"
                                                         , "Nombre_inscription_Non_S"
                                                         , "Nombre_inscription"), selected = 'ConfigStarted'),
                              fluidRow(
                                  column(12,
                                         actionButton("calculate", label = "OK"))))))))),
          column(9,tabsetPanel(tabPanel("Result Fitted", showOutput("resultchart", "highcharts")),
                                tabPanel("Media Impact dans le modele", 
                                         fluidRow(
                                           column(12,
                                                  showOutput("proportionchart", "highcharts"))),
                                         fluidRow(
                                           column(12,
                                                  showOutput("proportionchartParDate", "highcharts")))
                                           ),
                                         
                                tabPanel("Summary",
                                         fluidRow(
                                           column(12,
                                                  htmlOutput("PrecisionError"),
                                                  uiOutput('download_tableCoef'),
                                                  fluidRow(column(8,
                                                                  DT::dataTableOutput("summary_table1"))),
                                                  uiOutput('download_tableCorr'),
                                                  fluidRow(column(12,
                                                                  DT::dataTableOutput("correlation_table")))))),
                                
                               tabPanel("Diagnostic" , 
                                         fluidRow(
                                           column(5, showOutput("Diagnostic1", "highcharts"))
                                           ,column(5, offset = 2, showOutput("Diagnostic2", "highcharts"))),
                                          fluidRow(  
                                            column(5,
                                                   showOutput("Diagnostic4", "highcharts"))
                                            ,column(5, offset = 2, showOutput("Diagnostic3","highcharts")))),
                               
                                tabPanel("Tableau", fluidRow(
                                                      column(12,uiOutput('download_table'),
                                                             fluidRow(
                                                               column(12, dataTableOutput('data_table'))))))
                                
                                
          ))))
          
        
        ),
    tabPanel("Modelisation de serie chronologique sur le residu",
             fluidPage(
               
                
                
                
                
              tabsetPanel(tabPanel("Exploiter le residu", 
                                   
                            column(2,
                                  actionButton("GetResidu", label = "Get Residu"),
                                      fluidRow(
                                          column(12,
                                                verbatimTextOutput("ResiduSuccess"),
                                                  fluidRow(
                                                    column(12,
                                                        selectInput("TSMode", label = "Modele de serie chronologique", choices = list("NONE","ARIMA","AUTO-ARIMA","SARIMA","AUTO-SARIMA"), selected = "NONE"),
                                                        fluidRow(
                                                          column(12,
                                                                uiOutput("ParamTSMode"),
                                                                  fluidRow(
                                                                    column(6,
                                                                            actionButton("TSOK", label = "OK")),
                                                                    column(6,
                                                                            actionButton("TSReset", label = "Reset"))
                                                                       ),
                                                                fluidRow(
                                                                  column(12,
                                                                         verbatimTextOutput("CalculTSMode"))
                                                                ))
                                                              )
                                                       ))
                                              ))),
                                   column(10,
                                             showOutput("Residu",'highcharts'),
                                             fluidRow(
                                               column(3, showOutput("ARIMAPLOT1", "highcharts"),
                                                      fluidRow(column(3, showOutput("ARIMAPLOT3", "highcharts")))),
                                               column(3, offset = 2,showOutput("ARIMAPLOT2", "highcharts")
                                               )
                                             ))),
                  tabPanel("Modele de serie",
                                             fluidRow(
                                               column(4,showOutput("ModeleTS", 'highcharts')),
                                               column(4, offset = 2, uiOutput("TSdownload_table"),
                                                      fluidRow(DT::dataTableOutput("TStable")))),
                                             fluidRow(
                                               column(4, showOutput("ModeleTS_Residu", "highcharts")),
                                               column(4, offset = 2, showOutput("ModeleTS_Residu_ACF", 'highcharts'))
                                             ),
                                              fluidRow(
                                                column(4, showOutput("ModeleTS_Residu_QQ", 'highcharts')),
                                                column(4, offset = 2, showOutput("ModeleTS_Residu_LjungBox", "highcharts"))
                                  
                                              )),
                  tabPanel("Resultat final",
                           htmlOutput("PrecisionErrorFinal"),
                           
                             showOutput("Result_Final", "highcharts")
                            
                           ))))
                      
               
               
              
                        
              
                        
              
                
                
               
                 
               
      
      
        
        
          
        
        

    
))
    

