source("VisibiliteMedia.R")
source("FonctionTransform.R")

shinyUI( 
  
   
  navbarPage("Statistic Analysis",
    
    
    tabPanel("Data exploitation",
      fluidPage(
        shinyjs::useShinyjs(), 
        
        fluidRow(
          column(2,
                 selectInput("Marque", 
                             label = "Choose a model",
                             choices = list("TOTAL"
                                            ,"Audi S1"
                                            ,"Audi S3"
                                            ,"Audi Q7"
                                            ,"Audi TT"
                                            ,"Audi A6"
                                            ,"Audi A5"
                                            ,"Audi Q5"
                                            ,"Audi A4"
                                            ,"Audi Q3"
                                            ,"Audi A1"
                                            ,"Audi A3"),
                             selected = "TOTAL")),
          column(2,
                 sliderInput("Annee", label = "Year", step = 1, min = 2014, max = 2015, value = c(2014,2015)))
        ),
        
        fluidRow(
          column(1,
                 actionButton("Extract", label = "Extract")),
          column(11,
                 verbatimTextOutput("Success"))
        ),
    
          
        
        fluidRow(
          column(3,
                 selectInput("Transformations", 
                             label = "Choose a variable to transform",
                             choices = list("TTC_Gazole"
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
                                            , "TV_TNT_InvestissementsEnEuros" 
                                            , "TV_InvestissementsEnEuros"
                                            , "TV_NAT_GRP"                                    
                                            , "TV_TNT_GRP"
                                            , "TV_GRP"                              
                                            , "MOBILE_Volume_Achete"
                                            , "MOBILE_Net_Budget_LC"                            
                                            , "Investissement_Affichage"
                                            
                                            , "INTERNET_DISPLAY_InvestissementsEnEuros_BMW" 
                                            , "PRESSE_InvestissementsEnEuros_QUOT_AUTO_BMW"        
                                            , "PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO_BMW"  
                                            , "PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO_BMW"    
                                            , "PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO_BMW"
                                            , "RADIO_InvestissementsEnEuros_BMW"                   
                                            , "TV_NAT_InvestissementsEnEuros_BMW"
                                            , "TV_TNT_InvestissementsEnEuros_BMW" 
                                            , "TV_InvestissementsEnEuros_BMW"
                                            , "TV_NAT_GRP_BMW"                                    
                                            , "TV_TNT_GRP_BMW"
                                            , "TV_GRP_BMW"                              
                                            , "Investissement_Affichage_BMW"
                                            
                                            , "INTERNET_DISPLAY_InvestissementsEnEuros_MERCEDES" 
                                            , "PRESSE_InvestissementsEnEuros_QUOT_AUTO_MERCEDES"        
                                            , "PRESSE_InvestissementsEnEuros_NON_QUOT_AUTO_MERCEDES"  
                                            , "PRESSE_InvestissementsEnEuros_QUOT_NON_AUTO_MERCEDES"    
                                            , "PRESSE_InvestissementsEnEuros_NON_QUOT_NON_AUTO_MERCEDES"
                                            , "RADIO_InvestissementsEnEuros_MERCEDES"                   
                                            , "TV_NAT_InvestissementsEnEuros_MERCEDES"
                                            , "TV_TNT_InvestissementsEnEuros_MERCEDES" 
                                            , "TV_InvestissementsEnEuros_MERCEDES"
                                            , "TV_NAT_GRP_MERCEDES"                                    
                                            , "TV_TNT_GRP_MERCEDES"
                                            , "TV_GRP_MERCEDES"                              
                                            , "Investissement_Affichage_MERCEDES"
                                            
                                            , "ConfigStarted"
                                            , "ConfigCompleted"                               
                                            , "UniqueVisitor"                                  
                                            , "UtileVisitor" 
                                            , "Nombre_inscription_S"
                                            , "Nombre_inscription_Non_S"
                                            , "Nombre_inscription"),width = '100%'),
                 fluidRow(
                   column(12,
                          selectInput("Transformation_Type", label = "Choose a transform function",
                                      choices = list('Linear', 'Logged', 'AdBudg', 'Power', 'Reciprocal', 'Dimishing'), selected = "Linear"))
                   
                 ),
                 
                 fluidRow(
                   column(12,
                          uiOutput("param"),
                          fluidRow(
                          column(6,
                            actionButton("bOK", label = "OK")),
                          column(6,
                            actionButton("bReset", label = "Reset"))
                  ))),
                 fluidRow(
                    column(12,
                           selectInput("Memorisation", 
                                       label = "Choose a variable to calcul its lasting effect",
                                       choices = list("TTC_Gazole"
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
                                                      , "TV_TNT_InvestissementsEnEuros" 
                                                      , "TV_InvestissementsEnEuros"
                                                      , "TV_NAT_GRP"                                    
                                                      , "TV_TNT_GRP"
                                                      , "TV_GRP"                              
                                                      , "MOBILE_Volume_Achete"
                                                      , "MOBILE_Net_Budget_LC"                            
                                                      , "Investissement_Affichage"),width = '100%'),
                           fluidRow(
                             column(12,
                                    selectInput("Memorisation_Type", label = "Choose a smoothing function",
                                                choices = list('Loess','Linear','LinearW'), selected = "Linear"),
                             
                           
                           fluidRow(
                             column(12,
                                    uiOutput("paramMemo"),
                                    fluidRow(
                                    column(6,
                                           actionButton("MemoOK", label = "OK")),
                                    column(6,
                                           actionButton("MemoReset", label = "Reset"))
                                    
                             ))
                    )))))),
          
            column(9,showOutput("Transformation_PLOT", "highcharts"),
                            fluidRow(
                              
                              column(3,
                                     showOutput("hist1","highcharts")),
                              column(3,offset = 4,
                                     showOutput("hist2","highcharts"))
                    ),
                   fluidRow(
                     column(12, showOutput("Memorisation_PLOT", "highcharts"))
                   )))
                
        )),
        
    
    
    tabPanel("Modelisation",
      fluidPage(
        fluidRow(
          column(3,
            h3("Choose the independent variable"),
            fluidRow(
              column(12,
        
                uiOutput("VariableGenere"),     
      
        
                      fluidRow(
                        column(12, h3("Choose the dependent variable")),
                          fluidRow(
                            column(12,
                              
                              uiOutput('VariableReponseGenere'),
                              fluidRow(
                                  column(12, 
                                         selectInput("Modele_Statistique", label = h3(""), choices = list("GLM","GAM"), selected = "GLM"),
                                  fluidRow(
                                    column(12, 
                                      uiOutput("ParamModel"))
                                  ),
                                  fluidRow(
                                  column(2,
                                         actionButton("calculate", label = "OK"))))))))))),
          column(9,tabsetPanel(tabPanel("Fitting result", showOutput("resultchart", "highcharts")),
                                tabPanel("Media contributions", 
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
                                
                               tabPanel("Regression Diagnostics" , 
                                         fluidRow(
                                           column(5, showOutput("Diagnostic1", "highcharts"))
                                           ,column(5, offset = 2, showOutput("Diagnostic2", "highcharts"))),
                                          fluidRow(  
                                            column(5,
                                                   showOutput("Diagnostic4", "highcharts"))
                                            ,column(5, offset = 2, showOutput("Diagnostic3","highcharts")))),
                               
                                tabPanel("Data table", fluidRow(
                                                      column(12,uiOutput('download_table'),
                                                             fluidRow(
                                                               column(12, dataTableOutput('data_table'))))))
                                
                                
          ))))
          
        
        ),
    tabPanel("Time series modeling for residual",
             fluidPage(
               
                
                
                
                
              tabsetPanel(tabPanel("Residual exploitation", 
                                   
                            column(2,
                                  actionButton("GetResidu", label = "Get Residual"),
                                      fluidRow(
                                          column(12,
                                                verbatimTextOutput("ResiduSuccess"),
                                                  fluidRow(
                                                    column(12,
                                                        selectInput("TSMode", label = "Time series model", choices = list("NONE","ARIMA","AUTO-ARIMA","SARIMA","AUTO-SARIMA"), selected = "ARIMA"),
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
                                               column(3, showOutput("ARIMAPLOT1", "highcharts")),
                                                      
                                               column(3, offset = 4,showOutput("ARIMAPLOT2", "highcharts")
                                               )
                                             ),
                                          fluidRow(column(3, showOutput("ARIMAPLOT3", "highcharts"))))),
                  tabPanel("Time series model",
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
                  tabPanel("Final residual",
                           htmlOutput("PrecisionErrorFinal"),
                            fluidRow(
                               column(5, showOutput("Result_Final", "highcharts")),
                               column(5, showOutput("ModeleTS_Residu_Final", "highcharts")))
                            
                           ))))
                      
               
               
              
                        
              
                        
              
                
                
               
                 
               
      
      
        
        
          
        
        

    
))
    

