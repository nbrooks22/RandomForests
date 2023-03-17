library(tidyverse)
library(rlang)

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

#CREATE RANDOM SAMPLE DATA FOR REGRESSION with n dimensions (As Matrix) - function(SEED n, #VALUES m, DIM dim > 1)
create_random_sample_data_reg_dim <- function(n, m, dim){
  set.seed(n)
  e <- rnorm(m,0,0.2)
  X <- c()
  for (i in 1:dim) {
    X <- append(X, runif(m,0,1))
    if(i == 1){
      Y <- sin(2*pi*X) + e
    }
  }

  return (list(x = matrix(X, nrow = dim), y = Y))
}

#CREATE SAMPLE DATA FOR CLASSIFICATION (As Matrix) - function(SEED n (NOT ADDED CURRENTLY: #VALUES m))
create_Sample_data_class <- function(n){
  set.seed(n)
  X1 <- runif(20,0,1)
  X2 <- runif(20,0,1)
  e <- rnorm(20,0,0.2)
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


#' Make Prediction
#' Make a Prediction of a datapoint or set of datapoints on a tree or set of trees.
#'
#'
#'
#' @param tree_list A list containing any number >=1 of trees (in tibble form) \cr as produced by `greedy_cart()`, `pruning()`, `bagging()`
#' or `random_forest()`
#' @param x_list A matrix of values to be predicted, where each column of `x_list` corresponds \cr to one datapoint.
#' \cr The number of rows corresponds to the dimension of the x values
#' @param type "reg" for regression trees\cr "class" for classification trees
#'
#' @return
#' @export
#'
#' @examples
#' treelist <- bagging(create_random_sample_data_reg_dim(5, 100, 3), 3, type = "reg")
#' xlist <- matrix(c(0.1, 0.4, 0.6, 0.2, 0.3, 0.6), ncol = 2)
#' predictions <- make_prediction(treelist$Bagged_Trees, xlist, "reg")
make_prediction <- function(tree_list, x_list, type = NULL){
  if(any(class(tree_list) != class(list()))){
    stop("The Tree you are making a prediction for must be in a List!")
  }
  if(any(class(x_list) != class(matrix()))){
    stop("The x Values you are making a prediction for must be in a List (of Vectors)!")
  }
  if(is.null(type)){
    stop("Please set the type to either `reg` or `class`!")
  }
  if(length(tree_list[[1]]$A[[1]][[1]]) != nrow(x_list)){
    stop("The dimensions of the prediction are not equal to the dimensions of the given data x-values!")
  }

  y_s <- c()


  is_not_leaf <- function(tree, this_node_number){
    this_node <- tree$A[tree$node == this_node_number]
    if(length(this_node[[1]]) > 1){
      return(TRUE)
    }
    return(FALSE)
  }

  node_exists <- function(tree, this_node){
    for (i in 1:length(tree$node)) {
      if(this_node == tree$node[[i]]){
        return(TRUE)
      }
    }
    return(FALSE)
  }

  make_pred <- function(tree, x){
    #current_node is an integer whose value is equal to the nodes position in the full_Tree
    current_node <- tree$node[tree$name == "root"]
    while(is_not_leaf(tree, current_node) && ((node_exists(tree, current_node*2)) || node_exists(tree, (current_node*2)+1))){
      if(node_exists(tree, current_node*2)){
        current_dim <- tree$split_index[tree$node == current_node*2]
        if(x[current_dim] < tree$split_point[tree$node == current_node*2]){
          newNode <- (tree$node[tree$node == current_node])*2
          current_node <- tree$node[tree$node == newNode]
        } else {
          newNode <- ((tree$node[tree$node == current_node])*2)+1
          current_node <- tree$node[tree$node == newNode]
        }
      } else if (node_exists(tree, (current_node*2)+1)){
        current_dim <- tree$split_index[tree$node == current_node*2+1]

        if(x[current_dim] < tree$split_point[tree$node == (current_node*2)+1]){
          newNode <- (tree$node[tree$node == current_node])*2
          current_node <- tree$node[tree$node == newNode]
        } else {
          newNode <- ((tree$node[tree$node == current_node])*2)+1
          current_node <- tree$node[tree$node == newNode]
        }
      }
    }
    return(tree$c_value[tree$node == current_node])
  }

  #For each value to predict, for each tree

  #For each value to predict
  y_s <- c()
  for(j in 1:ncol(x_list)){
    y_list <- c()
    for (i in 1:length(tree_list)) {
      y_list <- append(y_list, make_pred(tree_list[[i]], x_list[,j]))
    }

    if(type == "reg"){
      y_s <- append(y_s, mean(y_list))
    } else if(type == "class"){
      y_s <- append(y_s, as.integer(tail(names(sort(table(y_list))), 1)))
    }
  }
  return(y_s);
}




