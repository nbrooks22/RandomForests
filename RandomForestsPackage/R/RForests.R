
#' Random Forest Algorithm (Regression)
#'
#' Random Forest Algorithm for regression data
#'
#' @param data a named list that contains regression data\cr the x values have the name x and are in
#' the form of a matrix where the row number gives the dimension of the data\cr the y
#' values have the name y and are in the form of a vector
#' @param B number of Bags to create
#' @param A sample size we want to use
#' \cr must be greater than 0 and less than or equal to the number of observations
#' \cr the default value is the size of the data (so all observations are used)
#' @param m positive number of coordinates which we want to use in each iteration
#' \cr the default value is the dimension of the data
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves \cr must be greater than or equal to 1
#' \cr the default value is the maximal achievable number of leaves (the number of data points)
#' @param depth Condition to end: the tree has depth `depth`\cr must be greater than or equal to 0
#' \cr the default value is the maximal achievable depth
#' @param num_split split only nodes which contain at least `num_split` elements \cr must be greater than or equal to 2
#' @param min_num only split a node, if both child nodes have at least `min_num` elements \cr must be greater than or equal to 1
#'
#' @return a list of `B` trees in tibble form (description for a tree see ?greedy_cart)
#' @export
#'
#' @examples
#' X1 <- runif(50,0,1)
#' X2 <- runif(50,0,1)
#' X3 <- runif(50,0,1)
#' Y <- X1 + X2 + X3
#' data <- list(x = matrix(c(X1,X2,X3), nrow = 3, byrow = TRUE), y = Y)
#' random_forest_regression(data, B = 5, A = 25, m = 2, num_leaf = 10)
#'
#'
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

  if(as.integer(A) != A) {warning("A is not an integer. The value is set to ", ceiling(A)); A <- ceiling(A)}
  if(A > len) {warning("A is too big. A is set to ", len); A <- len}
  if(A <= 0) {warning("A must be greater than 0. A is set to ", len); A <- len}

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

    tree[[i]] <- greedy_cart_regression(new_data, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, m = m)$tree

  }
  tree
}




#' Random Forest Algorithm (Classification)
#'
#' Random Forest Algorithm for classification data
#'
#' @param data a named list that contains classification data\cr the x values have the name x and are in
#' the form of a matrix where the row number gives the dimension of the data\cr the y
#' values have the name y and are in the form of a vector
#' @param B number of Bags to create
#' @param A sample size we want to use
#' \cr must be greater than 0 and less than or equal to the number of observations
#' \cr the default value is the size of the data (so all observations are used)
#' @param m positive number of coordinates which we want to use in each iteration
#' \cr the default value is the dimension of the data
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves \cr must be greater than or equal to 1
#' \cr the default value is the maximal achievable number of leaves (the number of data points)
#' @param depth Condition to end: the tree has depth `depth`\cr must be greater than or equal to 0
#' \cr the default value is the maximal achievable depth
#' @param num_split split only nodes which contain at least `num_split` elements \cr must be greater than or equal to 2
#' @param min_num only split a node, if both child nodes have at least `min_num` elements \cr must be greater than or equal to 1
#' @param unique if `unique` is set to TRUE we don't split nodes where all data points in this node have the same class (y value)
#' \cr the default value is FALSE
#'
#' @return a list of `B` trees in tibble form (description for a tree see ?greedy_cart)
#' @export
#'
#' @examples
#' X1 <- runif(50,0,1)
#' X2 <- runif(50,0,1)
#' e <- rnorm(50,0,0.2)
#' kappa <- function(x,y) y - 0.5 - 0.3*sin(2*pi*x)
#' f <- function(x,y,e){
#'   Y <- c()
#'   for(i in seq_along(x)){
#'     if(kappa(X1[i],X2[i]) - e[i] <= 0){
#'       Y[i] <- 1
#'     } else{
#'       Y[i] <- 2
#'     }
#'   }
#'   Y
#' }
#' data <- list(x = matrix(c(X1,X2), nrow = 2, byrow = TRUE), y = f(X1,X2,e))
#' random_forest_classification(data, B = 5, A = 25, m = 1, depth = 4)
#'
#'
#'
random_forest_classification <- function(data, B, A = NULL, m = 0, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, unique = FALSE){

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
  if(as.integer(A) != A) {warning("A is not an integer. The value is set to ", ceiling(A)); A <- ceiling(A)}
  if(A > len) {warning("A is too big. A is set to ", len); A <- len}
  if(A <= 0) {warning("A must be greater than 0. A is set to ", len); A <- len}

  if(!is.logical(unique)) {warning("unique must be logical. unique is set to FALSE"); unique <- FALSE}

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

    tree[[i]] <- greedy_cart_classification(new_data, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, m = m, unique = unique)$tree

  }
  tree
}

#' Random Forest Algorithm
#'
#' Random Forest Algorithm for either regression or classification data
#'
#' @param x column/list name(s) of the x value(s)
#' @param y column/list name of the y value
#' @param data tibble or named list with data
#' @param type "reg" for regression tree\cr "class" for classification tree
#' \cr if `type` is missing the function tries to "guess" the type
#' @param B number of Bags to create
#' @param A sample size we want to use
#' \cr must be greater than 0 and less than or equal to the number of observations
#' \cr the default value is the size of the data (so all observations are used)
#' @param m positive number of coordinates which we want to use in each iteration
#' \cr the default value is the dimension of the data
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves \cr must be greater than or equal to 1
#' \cr the default value is the maximal achievable number of leaves (the number of data points)
#' @param depth Condition to end: the tree has depth `depth`\cr must be greater than or equal to 0
#' \cr the default value is the maximal achievable depth
#' @param num_split split only nodes which contain at least `num_split` elements \cr must be greater than or equal to 2
#' @param min_num only split a node, if both child nodes have at least `min_num` elements \cr must be greater than or equal to 1
#' @param unique parameter for classification data: if `unique` is set to TRUE we don't split nodes where all data points in this node have the same class (y value)
#' \cr the default value is FALSE
#'
#' @return a list of `B` trees in tibble form (description for a tree see ?greedy_cart)
#'
#' @export
#'
#' @examples
#' X1 <- runif(50,0,1)
#' X2 <- runif(50,0,1)
#' X3 <- runif(50,0,1)
#' Y <- X1 + X2 + X3
#' data_reg <- tibble(a = X1, b = X2, c = X3, y = Y)
#' random_forest_regression(x = c(a,b,c), y = y, data = data_reg, type = "reg", B = 5, A = 25, m = 2, num_leaf = 10)
#'
#' X1 <- runif(50,0,1)
#' X2 <- runif(50,0,1)
#' e <- rnorm(50,0,0.2)
#' kappa <- function(x,y) y - 0.5 - 0.3*sin(2*pi*x)
#' f <- function(x,y,e){
#'   Y <- c()
#'   for(i in seq_along(x)){
#'     if(kappa(X1[i],X2[i]) - e[i] <= 0){
#'       Y[i] <- 1
#'     } else{
#'       Y[i] <- 2
#'     }
#'   }
#'   Y
#' }
#' data_class <- list(x1 = X1, x2 = X2, y = f(X1,X2,e))
#' random_forest_classification(x = c(x1, x2), y = y, data = data_class, type = "class", B = 5, A = 25, m = 1, depth = 4)

random_forest <- function(x,y,data, type = NULL, B, A = NULL, m = 0, num_leaf = NULL, depth = NULL, num_split = 2, min_num = 1, unique = FALSE){
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
    return(random_forest_classification(dat, B = B, A = A, m = m, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, unique = unique))
  } else{
    stop("Invalid type!")
  }
}

