% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Pruning.R
\name{pruning_regression}
\alias{pruning_regression}
\title{Cost-complexity Pruning (Regression)}
\usage{
pruning_regression(trees, lambda)
}
\arguments{
\item{trees}{A tibble containing the information of the tree.}

\item{lambda}{Parameter to weigh the amount of leaves per subtree}
}
\value{
The optimal subtree
}
\description{
Chooses optimal subtree from CART for parameter \code{lambda}
}
\details{
Uses \code{tree_seq_reg} to create a sequence of subtrees and calculates their risk. Pruning chooses the subtree, which
minimizes the term \code{risk_tree_reg(subtree)+ lambda * count_leaves(subtree)}.\cr
Will return error if \code{lambda} is smaller than 0. \code{lambda} can be estimated through cross validation
}
\examples{
trees <- greedy_cart_regression(create_realistic_sample_data_reg())$tree
print(pruning_regression(trees,1/80),n=40)

}
