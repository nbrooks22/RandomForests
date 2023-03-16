#' Bagging Algorithm (Regression or Classification)
#'
#' Bagging Algorithm for Regression or Classification Data
#'
#' @param data a named list that contains Regression or Classification data\cr the x values have the name x and are in
#' the form of a matrix \cr where the row number gives the dimension of the data\cr the y
#' values have the name y and are in the form of a vector
#' @param b Number of Bags to create. The Default Value is 5.
#' @param type Whether Classification or Regression is being called
#' @param pred_val OPTIONAL: If wanting to predict some values in a matrix, each of length \cr equal one less than
#' the number of dimensions in our data set. \cr Else, input null.
#'
#' @return A named list with the elements `Bagged_Trees` and `Prediction`.\cr
#' `Bagged_Trees` is an unnamed list with a number of tree \cr elements (see ?greedy_cart_regression) equal to the input value `b`
#' `Prediction` is the predicted value resulting from the `pred_val` data point. \cr If no `pred_val` was provided, this value is NULL
#' @export
#'
#' @examples bagging(create_random_sample_data_reg_dim(5, 100, 3), 3, type = "reg", matrix(c(0.1, 0.4, 0.6, 0.2, 0.3, 0.6), ncol = 2))
#' @examples bagging(create_Sample_data_class(1), 4, type = "class", matrix(c(0.1,0.4,0.6, 0.2, 0.4, 0, 1, 1), ncol = 4))
bagging <- function(data, b = 5, type = NULL, pred_val = NULL){


  if(is.null(type)){
    stop("Please specify whether you wish to Bag Regression or Classification data!")
  }

  if(type == "reg"){
    if(class(data) != class(list())){
      stop("Please input data in the correct format - see ?bagging_regression !")
    }
    if (b == 0){
      stop("Please input a non-Zero number of Bags!")
    }
    dims <- length(data$x[,1])

    if(!is.null(pred_val)){
      if(as_integer(length(pred_val[,1])) != as_integer(dims)){
        stop("The dimensions of the prediction are not equal to the dimensions of the given data x-values!")
      }
    }

    list_of_bags <- list()
    list_of_trees <- list()
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
      list_of_trees[[length(list_of_trees)+1]] <- greedy_cart_regression(list(x = x, y = temp_tibble[[dims+1]]))$tree
    }

    #For each Tree, make a Prediction (if provided). Then Average them
    if(!is.null(pred_val)){
      c  <- make_prediction(list_of_trees, pred_val, "reg")

      return(list("Bagged_Trees" = list_of_trees, "Prediction" = c))
    } else {
      return(list("Bagged_Trees" = list_of_trees, "Prediction" = NULL))
    }
  }
  if(type == "class"){
    if(class(data) != class(list())){
      stop("Please input data in the correct format - see ?bagging_regression !")
    }
    if (b == 0){
      stop("Please input a non-Zero number of Bags!")
    }
    dims <- length(data$x[,1])
    if(!is.null(pred_val)){
      if(as_integer(length(pred_val[,1])) != as_integer(dims)){
        stop("The dimensions of the prediction are not equal to the dimensions of the given data x-values!")
      }
    }

    list_of_bags <- list()
    list_of_trees <- list()
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
      list_of_trees[[length(list_of_trees)+1]] <- greedy_cart_classification(list(x = x, y = temp_tibble[[dims+1]]))$tree
    }


    #For each Tree, make a Prediction. Then Average them
    if(!is.null(pred_val)){
      if(as_integer(length(pred_val[,1])) != as_integer(dims)){
        stop("The dimensions of the prediction are not equal to the dimensions of the given data x-values!")
      }
      c <- make_prediction(list_of_trees, pred_val, "class")

      return(list("Bagged_Trees" = list_of_trees, "Prediction" = c))
    } else {
      return(list("Bagged_Trees" = list_of_trees, "Prediction" = NULL))
    }
  }
}




