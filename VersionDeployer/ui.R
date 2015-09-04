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
                 selectInput("Memorisation", 
                             label = "Choose a variable to calcul its ADSTOCK",
                             choices = list("TTC_Gazole" 
                                            ,"Impressions_BRANDING"                  
                                            ,"Impressions_ROI"
                                            ,"Clicks_BRANDING"                       
                                            ,"Clicks_ROI"
                                            ,"Budget_Depense_NON_Cappe_BRANDING"     
                                            ,"Budget_Depense_NON_Cappe_ROI"
                                            ,"PRESSE_InvestissementsEnEuros_QUOT"    
                                            ,"PRESSE_InvestissementsEnEuros_NON_QUOT"
                                            ,"PRESSE_InvestissementsEnEuros"
                                            ,"RADIO_InvestissementsEnEuros"          
                                            ,"TV_InvestissementsEnEuros"
                                            ,"TV_GRP"                                
                                            ,"MOBILE_Volume_Achete"
                                            ,"MOBILE_Net_Budget_LC"                  
                                            ,"Investissement_Affichage"
                                            ,"PRESSE_InvestissementsEnEuros_BMW"     
                                            ,"RADIO_InvestissementsEnEuros_BMW"
                                            ,"TV_InvestissementsEnEuros_BMW"         
                                            ,"TV_GRP_BMW"
                                            ,"Investissement_Affichage_BMW"          
                                            ,"PRESSE_InvestissementsEnEuros_MERCEDES" 
                                            ,"RADIO_InvestissementsEnEuros_MERCEDES" 
                                            ,"TV_InvestissementsEnEuros_MERCEDES"     
                                            ,"TV_NAT_GRP_MERCEDES"                   
                                            ,"Investissement_Affichage_MERCEDES"),width = '100%'),
                 fluidRow(
                   column(12,
                          h5("ADSTOCK: AD(t) = I(t) + Level*AD(t-1)"),textInput("ParamMemo1", label = "Level(0~1)", value = "0.1"),
                                   fluidRow(
                                     column(6,
                                            actionButton("MemoOK", label = "OK")),
                                     column(6,
                                            actionButton("MemoReset", label = "Reset"))
                                   ))),
                 fluidRow(
                    column(12,
                           selectInput("Transformations", 
                                       label = "Choose a variable to transform",
                                       choices = list("TTC_Gazole" 
                                                      ,"Impressions_BRANDING"                  
                                                      ,"Impressions_ROI"
                                                      ,"Clicks_BRANDING"                       
                                                      ,"Clicks_ROI"
                                                      ,"Budget_Depense_NON_Cappe_BRANDING"     
                                                      ,"Budget_Depense_NON_Cappe_ROI"
                                                      ,"PRESSE_InvestissementsEnEuros_QUOT"    
                                                      ,"PRESSE_InvestissementsEnEuros_NON_QUOT"
                                                      ,"PRESSE_InvestissementsEnEuros"
                                                      ,"RADIO_InvestissementsEnEuros"          
                                                      ,"TV_InvestissementsEnEuros"
                                                      ,"TV_GRP"                                
                                                      ,"MOBILE_Volume_Achete"
                                                      ,"MOBILE_Net_Budget_LC"                  
                                                      ,"Investissement_Affichage"
                                                      ,"PRESSE_InvestissementsEnEuros_BMW"     
                                                      ,"RADIO_InvestissementsEnEuros_BMW"
                                                      ,"TV_InvestissementsEnEuros_BMW"         
                                                      ,"TV_GRP_BMW"
                                                      ,"Investissement_Affichage_BMW"          
                                                      ,"PRESSE_InvestissementsEnEuros_MERCEDES" 
                                                      ,"RADIO_InvestissementsEnEuros_MERCEDES" 
                                                      ,"TV_InvestissementsEnEuros_MERCEDES"     
                                                      ,"TV_NAT_GRP_MERCEDES"                   
                                                      ,"Investissement_Affichage_MERCEDES"),width = '100%'),
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
                                    ))))),
                 fluidRow(
                   column(12,
                          selectInput("Lag", 
                                      label = "Choose a variable to calculate the lag effect",
                                      choices = list("TTC_Gazole" 
                                                     ,"Impressions_BRANDING"                  
                                                     ,"Impressions_ROI"
                                                     ,"Clicks_BRANDING"                       
                                                     ,"Clicks_ROI"
                                                     ,"Budget_Depense_NON_Cappe_BRANDING"     
                                                     ,"Budget_Depense_NON_Cappe_ROI"
                                                     ,"PRESSE_InvestissementsEnEuros_QUOT"    
                                                     ,"PRESSE_InvestissementsEnEuros_NON_QUOT"
                                                     ,"PRESSE_InvestissementsEnEuros"
                                                     ,"RADIO_InvestissementsEnEuros"          
                                                     ,"TV_InvestissementsEnEuros"
                                                     ,"TV_GRP"                                
                                                     ,"MOBILE_Volume_Achete"
                                                     ,"MOBILE_Net_Budget_LC"                  
                                                     ,"Investissement_Affichage"
                                                     ,"PRESSE_InvestissementsEnEuros_BMW"     
                                                     ,"RADIO_InvestissementsEnEuros_BMW"
                                                     ,"TV_InvestissementsEnEuros_BMW"         
                                                     ,"TV_GRP_BMW"
                                                     ,"Investissement_Affichage_BMW"          
                                                     ,"PRESSE_InvestissementsEnEuros_MERCEDES" 
                                                     ,"RADIO_InvestissementsEnEuros_MERCEDES" 
                                                     ,"TV_InvestissementsEnEuros_MERCEDES"     
                                                     ,"TV_NAT_GRP_MERCEDES"                   
                                                     ,"Investissement_Affichage_MERCEDES"),width = '100%'),
                          fluidRow(
                            column(12,
                                   textInput("ParamLag", label = "Lag(days)", value = "1")
                            )
                          ),
                          fluidRow(
                            column(6,
                                   actionButton("LagOK", label = "OK")),
                            column(6,
                                   actionButton("LagReset", label = "Reset"))
                          ))
                 ),
                 fluidRow(
                   column(12,
                          selectInput("Display", 
                                      label = "Choose a variable to display",
                                      choices = list("TTC_Gazole" 
                                                     ,"Impressions_BRANDING"                  
                                                     ,"Impressions_ROI"
                                                     ,"Clicks_BRANDING"                       
                                                     ,"Clicks_ROI"
                                                     ,"Budget_Depense_NON_Cappe_BRANDING"     
                                                     ,"Budget_Depense_NON_Cappe_ROI"
                                                     ,"PRESSE_InvestissementsEnEuros_QUOT"    
                                                     ,"PRESSE_InvestissementsEnEuros_NON_QUOT"
                                                     ,"PRESSE_InvestissementsEnEuros"
                                                     ,"RADIO_InvestissementsEnEuros"          
                                                     ,"TV_InvestissementsEnEuros"
                                                     ,"TV_GRP"                                
                                                     ,"MOBILE_Volume_Achete"
                                                     ,"MOBILE_Net_Budget_LC"                  
                                                     ,"Investissement_Affichage"
                                                     ,"PRESSE_InvestissementsEnEuros_BMW"     
                                                     ,"RADIO_InvestissementsEnEuros_BMW"
                                                     ,"TV_InvestissementsEnEuros_BMW"         
                                                     ,"TV_GRP_BMW"
                                                     ,"Investissement_Affichage_BMW"          
                                                     ,"PRESSE_InvestissementsEnEuros_MERCEDES" 
                                                     ,"RADIO_InvestissementsEnEuros_MERCEDES" 
                                                     ,"TV_InvestissementsEnEuros_MERCEDES"     
                                                     ,"TV_NAT_GRP_MERCEDES"                   
                                                     ,"Investissement_Affichage_MERCEDES"
                                                     
                                                     , "ConfigStarted"
                                                     , "ConfigCompleted"                               
                                                     , "UniqueVisitor"                                  
                                                     , "UtileVisitor" 
                                                     , "Nombre_inscription_S"
                                                     , "Nombre_inscription_Non_S"
                                                     , "Nombre_inscription"),width = '100%'),
                          fluidRow(
                            column(6,
                                   actionButton("DisplayOK", label = "OK")),
                            column(6,
                                   actionButton("DisplayReset", label = "Reset"))
                          ))
                 
                 
                 ),
                 fluidRow(
                   column(12,
                          selectInput("MoreFunction", 
                                      label = "Choose what you want to implement",
                                      choices = list("Rien pour l'instant"),width = '100%'),
                          fluidRow(
                            column(6,
                                   actionButton("FunctionOK", label = "OK"))
                            
                          ))
                   
                   
                 )
                 
                 ),
          
            column(9,
                   fluidRow(column(12,
                            showOutput("Transformation_PLOT","highcharts"))),
                   fluidRow(column(12,
                            showOutput("hist1","highcharts"))),
                   fluidRow(column(12,
                            showOutput("hist2","highcharts")))
            )  
                
        ))),
        
    
    
    tabPanel("Regression Modelisation",
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
                                         actionButton("calculate", label = "OK"),
                                         actionButton("CalculatePCA", label = "PCA_OK"))))))))))),
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
                                           column(12, showOutput("Diagnostic1", "highcharts"))),
                                        fluidRow(
                                          column(12,  showOutput("Diagnostic2", "highcharts"))),
                                        
                                        fluidRow(  
                                            column(12,
                                                   showOutput("Diagnostic4", "highcharts"))),
                                        fluidRow(
                                            column(12, showOutput("Diagnostic3","highcharts")))),
                               
                                tabPanel("Data table", fluidRow(
                                                           column(12,uiOutput('download_table'))),
                                                      fluidRow(
                                                            column(12, dataTableOutput('data_table'))))))
                                
                                
          ))),
    
    
    tabPanel("Regression Modelisation with PCA",
             fluidPage(
               fluidRow(
                showOutput("PCA_Proportion", "highcharts")
               ),
               fluidRow(
                 column(2,
                        uiOutput("PCA_VariableGenere"),
                        actionButton("PCA_RegressionOK", "OK")
                        ),
                 column(10,
                        tabsetPanel(tabPanel("Fitting result", showOutput("PCA_Regression_Result", "highcharts")),
                                    tabPanel("Media contributions", 
                                             fluidRow(
                                               column(12,
                                                      showOutput("PCA_proportionchart", "highcharts"))),
                                             fluidRow(
                                               column(12,
                                                      showOutput("PCA_proportionchartParDate", "highcharts")))
                                    ),
                                    
                                    tabPanel("Summary",
                                             fluidRow(
                                               column(12,
                                                      htmlOutput("PrecisionError_PCA_Regression"),
                                                      uiOutput('download_tableCoef_PCA_Regression'),
                                                      fluidRow(column(8,
                                                                      DT::dataTableOutput("summary_table_PCA_Regression")))
                                                      ))),
                                    
                                    tabPanel("Regression Diagnostics" , 
                                             fluidRow(
                                               column(12, showOutput("Diagnostic1_PCA_Regression", "highcharts"))),
                                             fluidRow(
                                               column(12,  showOutput("Diagnostic2_PCA_Regression", "highcharts"))),
                                             
                                             fluidRow(  
                                               column(12,
                                                      showOutput("Diagnostic4_PCA_Regression", "highcharts"))),
                                             fluidRow(
                                               column(12, showOutput("Diagnostic3_PCA_Regression","highcharts"))))
                                    
                                    ))
               )
             )
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
                                             fluidRow(
                                               column(12,showOutput("Residu",'highcharts'))),
                                             fluidRow(
                                               column(12, showOutput("ARIMAPLOT1", "highcharts"))),
                                            fluidRow(        
                                               column(12, showOutput("ARIMAPLOT2", "highcharts"))),
                                          fluidRow(
                                            column(12, showOutput("ARIMAPLOT3", "highcharts"))))),
                  tabPanel("Time series model",
                                             fluidRow(
                                               column(12,showOutput("ModeleTS", 'highcharts'))),
                                            fluidRow(
                                               column(12, uiOutput("TSdownload_table"),
                                                      fluidRow(DT::dataTableOutput("TStable")))),
                                             fluidRow(
                                               column(12, showOutput("ModeleTS_Residu", "highcharts"))),
                                            fluidRow(
                                               column(12, showOutput("ModeleTS_Residu_ACF", 'highcharts'))
                                             ),
                                              fluidRow(
                                                column(12, showOutput("ModeleTS_Residu_QQ", 'highcharts'))),
                                              fluidRow(
                                                column(12, showOutput("ModeleTS_Residu_LjungBox", "highcharts"))
                                              )),
                  tabPanel("Final residual",
                           tags$head(includeScript(system.file('www', 'pie3d.js'))),
                           htmlOutput("PrecisionErrorFinal"),
                            fluidRow(
                               column(12, showOutput("Result_Final", "highcharts"))),
                           fluidRow(
                               column(12, showOutput("ModeleTS_Residu_Final", "highcharts"))),
                           fluidRow(
                             column(12, showOutput("Contribution_Final", "highcharts"))
                           )
                            
                      )
                  )
              )
             )
  ))
                      
               
               
              
                        
              
                        
              
                
                
               
                 
               
      
      
        
        
          
        
        

    

    

