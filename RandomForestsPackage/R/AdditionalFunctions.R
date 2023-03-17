####################################################################################
#                                CSV-Datei lesen
####################################################################################

readCSV <- function(file, type) {
  data <- as_tibble(read.csv(file$datapath))
  
  # Regression
  if (type == 0) {
    X <- c()
    
    for (col in 1:(ncol(data) - 1)) {
      XNew <- data %>% pull(colnames(data)[col])
      X <- c(X, XNew)
    }
    
    Y <- data %>% pull(colnames(data)[ncol(data)])
    
    return (list(x = matrix(X, nrow = ncol(data) - 1), y = Y))
  }
  
  # Klassifikation
  if (type == 1) {
    X <- c()
    
    for (col in 1:(ncol(data) - 1)) {
      XNew <- data %>% pull(colnames(data)[col])
      X <- c(X, XNew)
    }
    
    Y <- data %>% pull(colnames(data)[ncol(data)])
    
    return(list(x = matrix(X, nrow = ncol(data) - 1, byrow = TRUE), y = Y))
  }
}

####################################################################################
#                   Zufallsdaten Gieriges-Verfahren Regression
####################################################################################

create_random_sample_data_reg <- function(n, m){
  set.seed(n)
  X <- runif(m,0,1)
  e <- rnorm(m,0,0.2)
  Y <- sin(2*pi*X) + e
  return (list(x = matrix(X, nrow = 1), y = Y))
}

####################################################################################
#                 Zufallsdaten Gieriges-Verfahren Klassifikation
####################################################################################

create_random_sample_data_class <- function(n, m){
  set.seed(n)
  X1 <- runif(m,0,1)
  X2 <- runif(m,0,1)
  e <- rnorm(m,0,0.2)
  kappa <- function(x,y) y - 0.5 - 0.3*sin(2*pi*x)
  f <- function(x,y,e){
    Y <- c()
    for(i in seq_along(x)){
      if(kappa(X1[i],X2[i]) - e[i] <= 0){
        Y[i] <- 1
      } else{
        Y[i] <- 2
      }
    }
    Y
  }
  data <- list(x = matrix(c(X1,X2),nrow = 2, byrow = TRUE), y = f(X1,X2,e))
  return(data)
}
