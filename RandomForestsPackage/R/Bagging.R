#' RANDOM FOREST BAGGING ALGORITHM (Regression)
#'
#' Condition to End: The Tree has t leaves - 6.52
#'
#' @param data Tibble that contains Regression Data
#' @param b Number of Bags
#' @param x OPTIONAL: If wanting to predict a Value
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
#' @examples bagging_regression(create_random_sample_data_reg_dim(5, 100, 4), 3, c(0.1, 0.2, 0.9, 0.4))
bagging_regression <- function(data, b, pred_val){

  list_of_bags <- list()
  list_of_trees <- list()
  dims <- length(data$x[,1])
  tibble_of_values <- as_tibble(rbind(matrix(data$x, nrow = dims), data$y))


  #Data: All Data(Replacement), X: percentage of data to sample
  create_bag <- function(data, b, list_of_bags){
    bagOfData <- sample(data, length(data[1,]), replace = TRUE)
    return(bagOfData)
  }

  #For Each Bag, make a corresponding Tree
  for (i in 1:b) {
    list_of_bags[[length(list_of_bags)+1]] <- create_bag(tibble_of_values, length(data$y), list_of_bags)
    temp_tibble <- as_tibble(t(list_of_bags[[i]]))

    x <- matrix(temp_tibble$V1, nrow = 1)
    if(length(data$x[,1]) > 1){
      for (j in 2:length(data$x[,1])) {
        x <- rbind(x, temp_tibble[[j]])
      }
    }
    list_of_trees[[length(list_of_trees)+1]] <- greedy_cart_regression(list(x = x, y = temp_tibble[[dims+1]]))
  }

  #For each Tree, make a Prediction (if provided). Then Average them
  if(hasArg(pred_val)){
    if(as_integer(length(pred_val)) != as_integer(dims)){
      stop("The dimensions of the prediction are not equal to the dimensions of the given data x-values!")
    }

    c <- c()

    for (i in 1:b) {
     c  <- append(c, make_prediction(list_of_trees[[i]]$tree, pred_val))
    }

    avg <- mean(c)
    return(avg)
  }
}


#' RANDOM FOREST BAGGING ALGORITHM (Classification)
#'
#' Condition to End: The Tree has t leaves - 6.52
#'
#' @param data Tibble that contains Regression Data
#' @param m Number of Samples that should be aquired
#' @param x OPTIONAL: If wanting to predict a Value
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
#' @examples bagging_classification(create_Sample_data_class(1), 4, c(0.1,0.5, 0.6))
bagging_classification <- function(data, b, pred_val){
  list_of_bags <- list()
  list_of_trees <- list()
  dims <- length(data$x[,1])
  tibble_of_values <- as_tibble(rbind(matrix(data$x, nrow = dims), data$y))


  #Data: All Data(Replacement), X: percentage of data to sample
  create_bag <- function(data, b, list_of_bags){
    bagOfData <- sample(data, length(data[1,]), replace = TRUE)
    return(bagOfData)
  }

  #For Each Bag, make a corresponding Tree
  for (i in 1:b) {
    list_of_bags[[length(list_of_bags)+1]] <- create_bag(tibble_of_values, length(data$y), list_of_bags)
    temp_tibble <- as_tibble(t(list_of_bags[[i]]))

    x <- matrix(temp_tibble$V1, nrow = 1)
    if(length(data$x[,1]) > 1){
      for (j in 1:length(data$x[,1])) {
        x <- rbind(x, temp_tibble[[j]])
      }
    }
    list_of_trees[[length(list_of_trees)+1]] <- greedy_cart_classification(list(x = x, y = temp_tibble[[dims+1]]))
  }


  #For each Tree, make a Prediction. Then Average them
  if(hasArg(pred_val)){
    if(as_integer(length(pred_val)) != as_integer(dims+1)){
      stop("The dimensions of the prediction are not equal to the dimensions of the given data x-values!")
    }
    c <- c()
    i <- 0
    for (i in 1:b) {
      c  <- append(c, make_prediction(list_of_trees[[i]]$tree, pred_val))
    }
    avg <- sort(table(c),decreasing=TRUE)[1]
    return(c[avg])
  }
}









