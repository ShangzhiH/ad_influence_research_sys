## this is a variable to stock the intermediat data
## the data is used to do the normal regression
Data_Tempo <- reactiveValues(data = NULL)

## this is a variable to stock the intermediat data for PCA
## the data calculated by PCA Algo
Data_PCA <- reactiveValues(data = NULL)


## Normal regression model
Normal_Regression_Model <- reactiveValues(data = NULL)

## Regression model on the PCA predictors
PCA_Regression_Model <- reactiveValues(data = NULL)



## this is intermediate variable to stock the residus
## the residus is caculated by the regression model
ResiduValue <- reactiveValues(data = NULL) 


## this is transformed residual
## a copy of residus for exploitation of PACF and ACF
ResiduApresTransformation <- reactiveValues(data = NULL)

## Time series model
TS_Model_Table <- reactiveValues(data = NULL)