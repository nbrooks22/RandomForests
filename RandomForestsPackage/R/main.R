library(tidyverse)

# Greedy CART

#CREATE RANDOM SAMPLE DATA FOR REGRESSION (As Matrix) - function(SEED n, #VALUES m)
create_random_sample_data_reg <- function(n, m){
  set.seed(n)
  X <- runif(m,0,1)
  e <- rnorm(m,0,0.2)
  Y <- sin(2*pi*X) + e
  return (list(x = matrix(X, nrow = 1), y = Y))
}

#CREATE (Semi)REALISTIC SAMPLE DATA FOR REGRESSION (As Matrix)
create_realistic_sample_data_reg <- function(){
  return (data_realistic <- list(x = matrix(c(2, 4, 6, 8, 11.5, 14.5, 16, 18.5, 21, 23.5, 26, 27, 28, 29, 30, 32, 33, 35, 37), nrow = 1),
                         y = c(0, 0, 0, 0, 5, 20, 100, 100, 100, 100, 65, 63, 60, 58, 56, 11, 0, 0, 0)))
}

#CREATE SIMPLIFIED SAMPLE DATA FOR REGRESSION (As Matrix)
create_simplified_sample_data_reg <- function(){
  return (data_simplified <- list(x = matrix(c(1,3,5), nrow = 1), y = c(0,0,100)))
}

#CREATE SAMPLE DATA FOR CLASSIFICATION (As Matrix) - function(SEED n (NOT ADDED CURRENTLY: #VALUES m))
create_Sample_data_class <- function(n){
  set.seed(n)
  X1 <- runif(100,0,1)
  X2 <- runif(100,0,1)
  e <- rnorm(100,0,0.2)
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

#FIND LEAF - function(TREE (Tibble) tree)
find_leaf1 <- function(tree){
  tree %>%
    filter(name == "leaf") -> leafs
  return(leafs$node)
}



