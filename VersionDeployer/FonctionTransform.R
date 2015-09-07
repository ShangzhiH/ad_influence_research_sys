Transform <- function(nom, data, parametre) {
  if(nom == 'Linear') {
    result = parametre[1] + data*parametre[2]
  }
  else if(nom == 'Logged') {
    result = data
    
    for(i in c(1:length(result))) {
      if(result[i] > 0) {
        result[i] = parametre[1] + log(result[i])*parametre[2]
      }
    }
  }
  else if(nom == 'AdBudg') {
    result = parametre[1] + parametre[2]*(data^parametre[4]
                                          /(data^parametre[4] + parametre[3]^parametre[4]))
  }
  else if(nom == 'Power') {
    result = parametre[1] + (data^parametre[3])*parametre[2]
  }
  else if(nom == 'Reciprocal') {
    result = parametre[1] + parametre[2]/(data + parametre[3])
  }
  else if(nom == 'Dimishing') {
    
    result = parametre[1] + parametre[2]*(1-exp(-data/parametre[3]))
  }
  else {
    print("Merci de taper une fonction valide: 'Linear', 'Logged', 'AdBudg', 'Power', 'Reciprocal', 'Dimishing'")
    return()
  }
    
  list(nom = nom, data = data, result = result)
}

Memorisation <- function(data, parametre) {
 
  glisse = data[,1]
  for(i in 2:length(glisse)) {
    glisse[i] = glisse[i] + parametre*glisse[i-1]
  }
  
  
  return(glisse)
}


WhichRowHasNA <- function(rowdata) {
  if(sum(is.na(unlist(rowdata))) >= 1)
    return(TRUE)
  else
    return(FALSE) 
}



