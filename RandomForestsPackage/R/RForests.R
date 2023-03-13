
#' RANDOM FOREST ALGORITHM (Regression)
#'
#' Condition to End: The Tree has t leaves - 6.52
#'
#' @param data Tibble that contains Regression Data
#' @param m Number of Samples that should be aquired
#' @param t Condition to End: The Tree has t leaves (insert tea leaf pun here)
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
#' @examples random_forest_regression(create_random_sample_data_reg(10, 100), 30, 40)
random_forest_regression <- function(data, B, A, m, num_leaf = NULL, depth = 0, num_split = 2, min_num = 1){
  
  # Datenüberprüfung
  # Rest der Datenüberprüfung findet in greedy_cart statt
  d <- nrow(data$x)

  if(m == d){
    warning("m is equal to the dimension of data: Bagging is used. To use Random Forest enter a value less than d")
  }
  
  stopifnot("depth must be greater than or equal to 0" = depth >= 0)
  stopifnot("num_split must be greater than or equal to 2" = num_split >= 2)
  stopifnot("min_num must be greater than or equal to 1" = min_num >= 1)
  
  
  tree <- vector("list", B)
  n <- length(data$y)
  for(i in 1:B){
    # ziehe n Stichproben aus data
    if(A < n){
      sam <- sample(1:n, A)
    } else{
      sam <- sample(1:n, n, replace = TRUE)
    }
    new_data1 <- data$y[sam]
    new_data2 <- data$x[, sam, drop = FALSE]
    new_data <- list(x = new_data2, y = new_data1)
    # wenn d = 1: data$x[,sam] Problem, da new_data2 keine matrix mehr ist
    # Lösung: drop = FALSE
    
    tree[[i]] <- greedy_cart_regression(new_data, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, m = m)
    
  }
  tree  
}




#' RANDOM FOREST ALGORITHM (Classification)
#'
#' Condition to End: The Tree has t leaves - 6.52
#'
#' @param data Tibble that contains Regression Data
#' @param m Number of Samples that should be aquired
#' @param t Condition to End: The Tree has t leaves
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
#' @examples random_forest_classification(create_random_sample_data_class(10, 100), 30, 40)
random_forest_classification <- function(data, B, A, m, num_leaf = NULL, depth = 0, num_split = 2, min_num = 1){
  
  # Datenüberprüfung
  # Rest der Datenüberprüfung findet in greedy_cart statt
  d <- nrow(data$x)
  
  if(m == d){
    warning("m is equal to the dimension of data: Bagging is used. To use Random Forest enter a value less than d")
  }
  
  stopifnot("depth must be greater than or equal to 0" = depth >= 0)
  stopifnot("num_split must be greater than or equal to 2" = num_split >= 2)
  stopifnot("min_num must be greater than or equal to 1" = min_num >= 1)
  

  tree <- vector("list", B)
  n <- length(data$y)
  for(i in 1:B){
    # ziehe n Stichproben aus data
    if(A < n){
      sam <- sample(1:n, A)
    } else{
      sam <- sample(1:n, n, replace = TRUE)
    }
    new_data1 <- data$y[sam]
    new_data2 <- data$x[, sam, drop = FALSE]
    new_data <- list(x = new_data2, y = new_data1)
    # wenn d = 1: data$x[,sam] Problem, da new_data2 keine matrix mehr ist
    # Lösung: drop = FALSE
    
    tree[[i]] <- greedy_cart_classification(new_data, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, m = m)
    
  }
  tree  
}

#' RANDOM FOREST ALGORITHM
#'
#' Condition to End: The Tree has t leaves - 6.52
#'
#' @param data Tibble that contains Regression Data
#' @param m Number of Samples that should be aquired
#' @param t Condition to End: The Tree has t leaves
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
random_forest <- function(x,y,data, B, A, m, type = NULL, num_leaf = NULL, depth = 0, num_split = 2, min_num = 1){
  x1 <- enexpr(x)
  y1 <- enexpr(y)
  data1 <- eval_tidy(x1,data)
  data2 <- eval_tidy(y1,data)
  
  # Typüberprüfung:
  # Daten (X) sollten Zahlen sein
  stopifnot("x must be numeric" = is.numeric(data1))
  
  stopifnot("y must be numeric" = is.numeric(data2))
  
  # y muss eindimensional sein
  stopifnot("y must be one-dimensional" = NCOL(data2) == 1)
  
  # x und y sollen richtige Länge haben
  # x ist ein vielfaches der Länge von y
  stopifnot("x and y don't have compatible length" = as.integer(length(data1)/length(data2))*length(data2) == length(data1))
  
  # Überprüfung der Argumente
  stopifnot("depth must be greater than or equal to 0" = depth >= 0)
  stopifnot("num_split must be greater than or equal to 2" = num_split >= 2)
  stopifnot("min_num must be greater than or equal to 1" = min_num >= 1)
  
  
  mat <- matrix(data1, nrow = length(data1)/length(data2), byrow = TRUE)
  dat <- list(x = mat, y = data2)
  
  # wenn kein Typ angegeben wurde -> versuche Typ zu erraten
  if(is.null(type)){
    y_int <- as.integer(data2)
    # wenn y in 1,...,K liegt -> classification
    if(all(y_int == data2) & all(y_int >= 1)){
      type <- "class"
      warning("Type was forgotten. Type was set to classification")
    } else{
      type <- "reg"
      warning("Type was forgotten. Type was set to regression")
      
    }

  }
  
  if(type == "reg"){
    return(random_forest_regression(dat, B = B, A = A, m = m, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num))
  } else if(type == "class"){
    return(random_forest_classification(dat, B = B, A = A, m = m, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num))
  } else{
    stop("Invalid type!")
  }
}










