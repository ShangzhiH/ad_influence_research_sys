shinyServer(
  function(input, output, session) {
  
    source("server_variable.R",local = TRUE)
    source("server_exploitation.R",local = TRUE)
    source("server_normal_regression.R",local = TRUE)
    source("server_pca_regression.R",local = TRUE)
    source("server_timeseries.R",local = TRUE)
    source("server_final_residual.R",local = TRUE)
})
    
    
    
    
    
    
