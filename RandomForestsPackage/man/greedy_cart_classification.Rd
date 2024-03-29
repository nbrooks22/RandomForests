% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GierigesVerfahren.R
\name{greedy_cart_classification}
\alias{greedy_cart_classification}
\title{Greedy Algorithm (Classification)}
\usage{
greedy_cart_classification(
  data,
  num_leaf = NULL,
  depth = NULL,
  num_split = 2,
  min_num = 1,
  m = 0,
  unique = FALSE
)
}
\arguments{
\item{data}{a named list that contains classification data\cr the x values have the name x and are in
the form of a matrix where the rownumber gives the dimension of the data\cr the y
values have the name y and are in the form of a vector}

\item{num_leaf}{Condition to end: the tree has \code{num_leaf} leaves \cr must be greater than or equal to 1
\cr the default value is the maximal achievable number of leaves (the number of data points)}

\item{depth}{Condition to end: the tree has depth \code{depth}\cr must be greater than or equal to 0
\cr the default value is the maximal achievable depth}

\item{num_split}{split only nodes which contain at least \code{num_split} elements \cr must be greater than or equal to 2}

\item{min_num}{only split a node, if both child nodes have at least \code{min_num} elements \cr must be greater than or equal to 1}

\item{m}{parameter for Random Forest algorithm: positive number of coordinates which we want to use in each iteration
\cr the default value is the dimension of the data}

\item{unique}{if \code{unique} is set to TRUE we don't split nodes where all data points in this node have the same class (y value)
\cr the default value is FALSE}
}
\value{
An environment with the elements \code{dim}, \code{values} and \code{tree}.\cr
\code{dim}: the dimension of the data. \cr
\code{values}: data in a tibble. \cr
\code{tree}: decision tree in the form of a tibble. It has the following columns \cr
\itemize{
\item \code{node}: node n has the child nodes 2n and 2n + 1
\item \code{name}: type of the node (root, inner node, leaf)
\item \code{split_index}: at which index we split the data
\item \code{split_point}: where we split the data. If node 2n has the split point s
then all data in this node is less than s. Analog if the node 2n + 1 has the split point s
then all data in this node is greater than or equal to s.
\item \code{y}: y values of data contained in this node
\item \code{A}: x values of data contained in this node
\item \code{c_value}: the approximate value for the data elements in a node
}
}
\description{
Greedy algorithm for classification data
}
\examples{
X1 <- runif(200,0,1)
X2 <- runif(200,0,1)
e <- rnorm(200,0,0.2)
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
data <- list(x = matrix(c(X1,X2), nrow = 2, byrow = TRUE), y = f(X1,X2,e))
val <- greedy_cart_classification(data, num_split = 10)
val$values
val$tree
}
