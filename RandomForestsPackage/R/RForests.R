
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
random_forest_regression <- function(data, B, A = NULL, m = 0, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1){
  
  # Datenüberprüfung
  # Rest der Datenüberprüfung findet in greedy_cart statt
  d <- nrow(data$x)

  len <- length(data$y)
  if(is.null(A)) A <- len
  if(missing(m)) m <- d
  if(m == d & A == len){
    warning("Bagging is used. To use Random Forest enter a value less than ", d, " for m or a value less than ", len," for A")
  }
  
  if(m > d) {warning("m is too big. m is set to " , d); m <- d}
  if(m <= 0) {warning("m must be greater than 0. m is set to ", d); m <- d}
  if(is.null(num_leaf)) num_leaf <- length(data$y)
  if (!is.null(depth)){
    if(depth < 0) {warning("depth must be greater than or equal to 0. depth is set to the maximal depth"); depth <- NULL}
  }
  if (num_split < 2) {warning("num_split must be greater than or equal to 2. num_split is set to 2"); num_split <- 2}
  if (min_num < 1) {warning("min_num must be greater than or equal to 1. min_num is set to 1"); min_num <- 1}
  if (num_leaf < 1) {warning("num_leaf must be greater than or equal to 1. num_leaf is set to ", length(data$y)); num_leaf <- length(data$y)}

  if(as.integer(num_leaf) != num_leaf) {warning("num_leaf is not an integer. The value is set to ", ceiling(num_leaf)); num_leaf <- ceiling(num_leaf)}
  if(!is.null(depth)){
     if(as.integer(depth) != depth) {warning("depth is not an integer. The value is set to ", ceiling(depth)); depth <- ceiling(depth)}
  }
  if(as.integer(num_split) != num_split) {warning("num_split is not an integer. The value is set to ", ceiling(num_split)); num_split <- ceiling(num_split)}
  if(as.integer(min_num) != min_num) {warning("min_num is not an integer. The value is set to ", ceiling(min_num)); min_num <- ceiling(min_num)}

  
  
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
random_forest_classification <- function(data, B, A = NULL, m = 0, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1){
  
  # Datenüberprüfung
  # Rest der Datenüberprüfung findet in greedy_cart statt
  d <- nrow(data$x)
  
  len <- length(data$y)
  if(is.null(A)) A <- len
  if(missing(m)) m <- d
  if(m == d & A == len){
    warning("Bagging is used. To use Random Forest enter a value less than ", d, " for m or a value less than ", len," for A")
  }
  
  if(m > d) {warning("m is too big. m is set to " , d); m <- d}
  if(m <= 0) {warning("m must be greater than 0. m is set to ", d); m <- d}
  if(is.null(num_leaf)) num_leaf <- length(data$y)
  if (!is.null(depth)){
    if(depth < 0) {warning("depth must be greater than or equal to 0. depth is set to the maximal depth"); depth <- NULL}
  }
  if (num_split < 2) {warning("num_split must be greater than or equal to 2. num_split is set to 2"); num_split <- 2}
  if (min_num < 1) {warning("min_num must be greater than or equal to 1. min_num is set to 1"); min_num <- 1}
  if (num_leaf < 1) {warning("num_leaf must be greater than or equal to 1. num_leaf is set to ", length(data$y)); num_leaf <- length(data$y)}

  if(as.integer(num_leaf) != num_leaf) {warning("num_leaf is not an integer. The value is set to ", ceiling(num_leaf)); num_leaf <- ceiling(num_leaf)}
  if(!is.null(depth)){
     if(as.integer(depth) != depth) {warning("depth is not an integer. The value is set to ", ceiling(depth)); depth <- ceiling(depth)}
  }
  if(as.integer(num_split) != num_split) {warning("num_split is not an integer. The value is set to ", ceiling(num_split)); num_split <- ceiling(num_split)}
  if(as.integer(min_num) != min_num) {warning("min_num is not an integer. The value is set to ", ceiling(min_num)); min_num <- ceiling(min_num)}

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
random_forest <- function(x,y,data, B, A = NULL, m = 0, type = NULL, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1){
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
  
  # Überprüfung der Argumente findet in Unterfunktionen statt
  
  mat <- matrix(data1, nrow = length(data1)/length(data2), byrow = TRUE)
  dat <- list(x = mat, y = data2)
  
  d <- nrow(mat)
  if(missing(m)) m <- d
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

