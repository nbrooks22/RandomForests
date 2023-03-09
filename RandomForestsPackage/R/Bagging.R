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
#' @examples
bagging_regression <- function(data, b, x){

  list_of_bags <- list()
  list_of_trees <- list()
  tibble_of_values <- as_tibble(rbind(matrix(data$x, nrow = 1), data$y))
  amount_per_bag <- round((length(tibble_of_values)/b))



  #Data: All Data(Replacement), X: percentage of data to sample
  create_bag <- function(data, b, list_of_bags){
    bagOfData <- sample(data, amount_per_bag, replace = TRUE)
    return(bagOfData)
  }

  #For Each Bag, make a corresponding Tree
  for (i in 1:b) {
    list_of_bags[[length(list_of_bags)+1]] <- create_bag(tibble_of_values, amount_per_bag, list_of_bags)
    temp_tibble <- rename(as_tibble(t(list_of_bags[[i]])),x = V1, y = V2)
    list_of_trees[[length(list_of_trees)+1]] <- greedy_cart_regression(list(x = matrix((temp_tibble$x), nrow = 1), y = temp_tibble$y))
  }

  #For each Tree, make a Prediction. Then Average them
  if(hasArg(x)){
    c <- c()

    for (i in 1:b) {
     c  <- append(c, make_prediction(list_of_trees[[i]], x))
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
#'
bagging_classification <- function(data, b, x){
  list_of_bags <- list()
  list_of_trees <- list()
  tibble_of_values <- as_tibble(rbind(matrix(data$x, nrow = 1), data$y))
  amount_per_bag <- (length(tibble_of_values)/b)



  #Data: All Data(Replacement), X: percentage of data to sample
  create_bag <- function(data, b, list_of_bags){
    bagOfData <- sample(data, amount_per_bag, replace = TRUE)
    return(bagOfData)
  }

  #For Each Bag, make a corresponding Tree
  for (i in 1:b) {
    list_of_bags[[length(list_of_bags)+1]] <- create_bag(tibble_of_values, amount_per_bag, list_of_bags)
    temp_tibble <- rename(as_tibble(t(list_of_bags[[i]])), x = V1, y = V2)
    list_of_trees[length(list_of_trees)+1] <- greedy_cart_classification(list(x = matrix((temp_tibble$x), nrow = 1), y = temp_tibble$y))
  }


  #For each Tree, make a Prediction. Then Average them
  if(hasArg(x)){
    c <- c()

    for (i in 1:b) {
      c  <- append(c, make_prediction(list_of_trees[[i]], x))
    }
    avg <- sort(table(c),decreasing=TRUE)[1]
    return(avg)
  }
}


#############################
# Alina's Code
bagging <- function(data, B, type = "reg"){
  tree <- vector("list", B)
  n <- length(data$y)
  for(i in 1:B){
    # ziehe n Stichproben aus data
    sam <- sample(1:n, n, replace = TRUE)
    new_data1 <- data$y[sam]
    new_data2 <- data$x[,sam]
    new_data <- list(x = new_data2, y = new_data1)
    
    # nicht immer nur ein Element in Blatt
    # auch möglich, dass mehrere Elemente im Blatt sind -> sind dann die gleichen Elemente
    tree[[i]] <- greedy_cart(x = x, y = y, data = new_data, type = type)
    # TODO: Muss man hier pruning verwenden anstatt greedy_cart?
  }
  tree
}






