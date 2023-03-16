
#' Count leaves for a given CART
#'
#' @param data A tibble containing the information of each node type of the CART in the colum "name".
#' @return The number of nodes titled leaf of the CART \code{data}
#' @examples
#' tib <- tibble(node=c(1,2,3,4,5), name=c("root","inner node","leaf","leaf","leaf"))
#' count_leaves(tib)
#' @export
count_leaves <- function(data){
  count <- data %>% filter(name=="leaf") %>% nrow()
  return(count)
}


#' Cut tree
#'
#' @description Cut a tree (CART) at a specific inner node value
#'
#'@param trees  A tibble containing the information of each node type of the CART in the colum "name".
#'@param new_leaf The node number where the tree will be cut and which will be turned from inner node into a leaf.
#'
#'@return A tibble in which all cut leaves are named "old leaf" and the former inner node \code{new leaf}, where the tree was cut, is named "leaf". \cr
#' All nodes, which where cut and hence turned into "old leaf", inherited the c_value from node number \code{new leaf}.
#'@examples
#' tib <- tibble(node=c(1,2,3,4,5,6,7), name=c("root","inner node", "inner node", "leaf","leaf","leaf","leaf"), c_value=c(1,2,3,4,5,6,7))
#' tib
#' cut_tree(tib,3)
#'
#' @details
#' \code{new leaf} has to be a positive whole number of an inner node of the tree. If \code{new leaf} is not a positive whole number
#' an error message will be returned. If \code{new leaf} is not an inner node, e.g. it is the node number of a leaf, or if it
#' is greater than he maximum node number in the tree, the function has no effect on the tree and will return the tree unchanged.
#'
#'@export
cut_tree <- function(trees,new_leaf)
{

  cut_leaves <- c(new_leaf*2, new_leaf*2 + 1)
  for (m in 1:ceiling(log(max(trees$node))/log(2))) # Alle Kindknoten ab der aktuellen Knotennummer auflisten
  {
    if( max(cut_leaves) <= max(trees$node))
    {
      cut_leaves <- unique(sort(c(cut_leaves,cut_leaves *2,cut_leaves*2+1)))
      cut_leaves <- cut_leaves[ cut_leaves %in% trees$node]
    }
  }
  cut_leaves
  trees %>% filter( node %in% cut_leaves) %>%  mutate(name="old leaf") -> old_leaves
  old_leaves
  trees
  trees %>% filter(node==new_leaf) %>% mutate(name=c("leaf")) -> make_leaf # Inneren Knoten isolieren
  make_leaf

  c <- make_leaf$c_value
  c

  trees %>% filter(! node %in% old_leaves$node) -> uncut_tree
  bind_rows(anti_join(make_leaf,old_leaves,by="node"),old_leaves) %>% mutate(c_value=c) -> subtree
  subtree
  bind_rows(anti_join(uncut_tree,subtree,by="node"),subtree) %>% arrange(node) -> subtree
  subtree


  return(subtree)

}

#' Risk Tree Regression
#' @description Calculate the risk of a tree (CART) using the risk function for regression
#'
#'@param trees  A tibble containing the information of the tree, e.g. y values of training data and c values for the estimations.
#'
#'@return The risk of the algorithm, which corresponds to the information in the tree.
#'
#'@details The risk function for regression sums the square of the difference of each y value of the training data and its corresponding
#' c value estimation and divides that term by the number of training points.
#'@examples
#' trees <- greedy_cart_regression(create_realistic_sample_data_reg())$tree
#' trees
#' risk_tree_reg(trees)
#'
#'
#'
#'@export
risk_tree_reg <- function(trees)
{
  leaves <- filter(trees, name=="leaf")   # Erzeuge Vektor aus allen y-Werten der Trainingsdaten
  a <- 0
  n <- 0
  for (i in seq_along(leaves$y))
  {
    for ( j in seq_along(leaves$y[[i]]))
    {
      a <- a + ( leaves$y[[i]][[j]] - leaves$c_value[[i]] )^2
      n <- n + 1
    }
  }

  risk <- a / n
  return(risk)
}

#' Risk Tree Classification
#' @description Calculate the risk of a tree (CART) using the risk function for classification
#'
#'@param trees  A tibble containing the information of the tree, e.g. y values of training data and c values for the estimations.
#'
#'@return The risk of the algorithm, which corresponds to the information in the tree.
#'
#'@details The risk function for classification adds ones if the y value and its corresponding
#' c value estimation are not identical and divides that term by the number of training points.
#'@examples
#' trees <- greedy_cart_classification(create_Sample_data_class())$tree
#' trees
#' risk_tree_class(trees)
#'
#'
#'
#'@export
risk_tree_class <- function(trees)
{

  leaves <- filter(trees, name=="leaf")   # Erzeuge Vektor aus allen y-Werten der Trainingsdaten
  a <- 0
  n <- 0
  for (i in seq_along(leaves$y))
  {
    for ( j in seq_along(leaves$y[[i]]))
    {
       a <- a + !( identical( leaves$y[[i]][[j]], leaves$c_value[[i]] ))
       n <- n + 1
    }
  }

   risk <- a / n # Riskowert des Baums für die Verlustfunktion im Klassifikationsproblem
  return(risk)
}



#' Tree Sequencing (Regression)
#' @description Tree Sequencing used in cost-complexity pruning for regression CARTs
#'
#'@param trees  A tibble containing the information of the tree.
#'
#'@return A subtree which minimizes the risk of cutting the tree in relation to the number of leafs left.
#'
#'@details For each risk calculation of a tree the \code{risk_tree_reg} function is used. The subtree, which will be returned,
#'minimizes the quotient of \code{(risk_tree_reg(trees) - riks_tree_reg(subtree)) / (count_leaves(subtree) - count_leaves(trees))} \cr
#'A column containing the risk of the subtree will be added to the tibble.
#'
#'trees <- greedy_cart_regression(create_realistic_sample_data_reg())$tree
#'print(tree_seq_reg(trees),n=37)
#'
#'@export
tree_seq_reg <- function(trees)
  {
  trees %>% filter(name=="old leaf") %>% mutate(name="older leaf") -> older_leaves
  older_leaves
  bind_rows(anti_join(trees,older_leaves,by="node"),older_leaves) %>% arrange(node) -> trees
  risk <- risk_tree_reg(trees)
  trees <- mutate(trees,risk=risk)
  #print(trees,n=39)

  leaves <- count_leaves(trees)
  leaves

  inner_nodes <- c(trees$node[trees$name=="root"],trees$node[trees$name=="inner node"])
  inner_nodes

  a <- vector(mode="numeric",length=0L)
  for (n in seq_along(inner_nodes))
  {
    new_leaf <- inner_nodes[n]    # Wähle inneren Knoten an dem abgeschnitten wird
    new_leaf
    subtree <- cut_tree(trees,new_leaf) # Neuer Teilbaum bei dem an new_leaf abgeschnitten wurde
    subtree %>% mutate(risk=risk_tree_reg(subtree)) -> subtree


    risk2 <- (risk_tree_reg(subtree)-risk)/(leaves-count_leaves(subtree))
    risk2

    a <- c(a,risk2)
    a
  }
  min_subtree <- cut_tree(trees,inner_nodes[which.min(a)])
  min_subtree %>% mutate(risk=risk_tree_reg(min_subtree)) -> min_subtree
  min_subtree
  return(min_subtree)
}


#' Tree Sequencing (Classification)
#' @description Tree Sequencing used in cost-complexity pruning for classification CARTs
#'
#'@param trees  A tibble containing the information of the tree.
#'
#'@return A subtree which minimizes the risk of cutting the tree in relation to the number of leafs left.
#'
#'@details For each risk calculation of a tree the \code{risk_tree_class} function is used. The subtree, which will be returned,
#'minimizes the quotient of \code{(risk_tree_class(trees) - riks_tree_class(subtree)) / (count_leaves(subtree) - count_leaves(trees))} \cr
#'A column containing the risk of the subtree will be added to the tibble.
#'
#'@examples
#'trees <- greedy_cart_classification(create_Sample_data_class())$tree
#'print(tree_seq_class(trees),n=40)
#'
#'@export
tree_seq_class <- function(trees)
{
  trees %>% filter(name=="old leaf") %>% mutate(name="older leaf") -> older_leaves
  older_leaves
  bind_rows(anti_join(trees,older_leaves,by="node"),older_leaves) %>% arrange(node) -> trees
  risk <- risk_tree_class(trees)
  trees <- mutate(trees,risk=risk)
  #print(trees,n=39)

  leaves <- count_leaves(trees)
  leaves

  inner_nodes <- c(trees$node[trees$name=="root"],trees$node[trees$name=="inner node"])
  inner_nodes

  a <- vector(mode="numeric",length=0L)
  for (n in seq_along(inner_nodes))
  {
    new_leaf <- inner_nodes[n]    # Wähle inneren Knoten an dem abgeschnitten wird
    new_leaf
    subtree <- cut_tree(trees,new_leaf) # Neuer Teilbaum bei dem an new_leaf abgeschnitten wurde
    subtree %>% mutate(risk=risk_tree_class(subtree)) -> subtree


    risk2 <- (risk_tree_class(subtree)-risk)/(leaves-count_leaves(subtree))
    risk2

    a <- c(a,risk2)
    a
  }
  min_subtree <- cut_tree(trees,inner_nodes[which.min(a)])
  min_subtree %>% mutate(risk=risk_tree_class(min_subtree)) -> min_subtree
  min_subtree
  return(min_subtree)
}

#' Cost-complexity Pruning (Regression)
#' @description Chooses optimal subtree from CART for parameter \code{lambda}
#'
#'@param trees  A tibble containing the information of the tree.
#'@param lambda Parameter to weigh the amount of leaves per subtree
#'
#'@return The optimal subtree
#'
#'@details Uses \code{tree_seq_reg} to create a sequence of subtrees and calculates their risk. Pruning chooses the subtree, which
#'minimizes the term \code{risk_tree_reg(subtree)+ lambda * count_leaves(subtree)}.\cr
#'Will return error if \code{lambda} is smaller than 0. \code{lambda} can be estimated through cross validation
#'
#'@examples
#'trees <- greedy_cart_regression(create_realistic_sample_data_reg())$tree
#'print(pruning_regression(trees,1/80),n=40)
#'
#'@export
pruning_regression <- function(trees,lambda)
{
  if (lambda<=0)
  {stop("Lambda must be greater than 0")} # Gewichtung der Blätter muss größer Null sein
  risk <- risk_tree_reg(trees)
  trees <- mutate(trees,risk=risk)
  trees <- list(trees)
  trees
  while( count_leaves(trees[[length(trees)]])>1 )
  {
    trees <- c(trees,list(tree_seq_reg(trees[[length(trees)]])))
  }

  a <- vector(mode="numeric",length=0L)
  for (i in seq_along(trees))
  {
      a <- c(a, trees[[i]]$risk[1] + lambda * count_leaves(trees[[i]]))
  }
  trees[[which.min(a)]] %>% filter(name %in% c("root","inner node","leaf"))  -> prun_tree
  return(prun_tree)
}


#' Cost-complexity Pruning (Classification)
#' @description Chooses optimal subtree from CART for parameter \code{lambda}
#'
#'@param trees  A tibble containing the information of the tree.
#'@param lambda Parameter to weigh the amount of leaves per subtree
#'
#'@return The optimal subtree.
#'
#'@details Uses \code{tree_seq_class} to create a sequence of subtrees and calculates their risk. Pruning chooses the subtree, which
#'minimizes the term \code{risk_tree_class(subtree)+ lambda * count_leaves(subtree)}.\cr
#'Will return error if \code{lambda} is smaller than 0. \code{lambda} can be estimated through cross validation
#'
#'@examples
#'trees <- greedy_cart_classification(create_Sample_data_class())$tree
#'print(pruning_classification(trees,1/80),n=40)
#'
#'@export
pruning_classification <- function(trees,lambda)
{
  if (lambda<=0)
  {stop("Lambda must be greater than 0")} # Gewichtung der Blätter muss größer Null sein
  risk <- risk_tree_class(trees)
  trees <- mutate(trees,risk=risk)
  trees <- list(trees)
  trees
  while( count_leaves(trees[[length(trees)]])>1 )
  {
    trees <- c(trees,list(tree_seq_class(trees[[length(trees)]])))
  }

  a <- vector(mode="numeric",length=0L)
  for (i in seq_along(trees))
  {
    a <- c(a, trees[[i]]$risk[1] + lambda * count_leaves(trees[[i]]))
  }
  trees[[which.min(a)]] %>% filter(name %in% c("root","inner node","leaf")) -> prun_tree
  return(prun_tree)
}



#' Cost-complexity Pruning
#' @description Chooses optimal subtree from CART for parameter \code{lambda}
#'
#'@param trees  A tibble containing the information of the tree.
#'@param lambda Parameter to weigh the amount of leaves per subtree
#'@param type Either \code{"reg"} or \code{"class"}
#'
#'@return The optimal subtree.
#'
#'@details Choose the type of pruning (regression or classification) which should be used.
#'See \code{pruning_regression} or \code{pruning_classification} for further details.\cr
#'Return error if no valid type is chosen.
#'
#'@examples
#'trees <- greedy_cart_classification(create_Sample_data_class())$tree
#'print(pruning(trees,1/80,type="class"),n=40)
#'
#'trees <- greedy_cart_regression(create_random_sample_data_reg(10,20))$tree
#'print(pruning(trees,1/100,type="reg"),n=40)
#'@export
pruning <- function(trees, lambda, type=NULL)
{
  if(type == "reg"){
    return(pruning_regression(trees,lambda))
  } else if(type == "class"){
    return(pruning_classification(trees,lambda))
  } else{
    stop("Invalid type!")
  }
}


partition <- function(data,m=2)
{
  if(length(data$y)< m) stop("Partition cannot have more subsets then set has elements")
  #if(m>=3) warning("Process may cause long runtime")
 m1 <-  floor(length(data$y)/m) # Wieviele Elemente enthält eine Teilmenge der Partition
 m2 <- length(data$y) - m1*m # Falls m n nicht teilt, erhalten m2 Teilmengen ein Element mehr als die anderen Teilmengen

 elements <- rep(m1,length=m)

 if(m2 >0)
 {
   for (i in seq(m2))
   {
     elements[i] <- elements[i]+1
   }

 }
 elements # Anzahl der Elemente pro Teilmenge der Partition
 new_data <- list(data)
 new_data
 idx_vec <- 1:length(data$y)
 for(i in seq_along(elements))
 {
   part_idx <- sample(idx_vec,elements[i],replace=FALSE) # Zufällige Auswahl an sovielen Indizes wie in Teilmenge
   new_data[[i+1]] <- list(x=matrix(data$x[,-part_idx],nrow=nrow(data$x)),y=data$y[-part_idx]) # Auswahl des zu den Indizes gehörigen Komplements in den Trainingsdaten
   idx_vec <- idx_vec[!(idx_vec %in% part_idx)] # Entfernung der ausgewählten Indizes aus Indexvektor
 }
 return(new_data)
}




#' Cross Validation (Regression)
#' @description Chooses optimal parameter lambda from vector \code{Lambda}
#'
#'@param data  A list of training data.
#'@param m Integer
#'@param Lambda Vector of possible lambda values
#'@param type Either \code{"reg"} or \code{"class"}
#'
#'@return The optimal lambda estimation
#'
#'@details The integer \code{m} lets you choose how many subsets the partition of the training data \code{data} should have.
#' For each complement of the subset and every component of \code{Lambda} pruning will be conducted. The function
#' returns the entry of \code{Lambda}, which minimizes the risk of the trees given through the pruninig process.
#'
#'
#'@examples
#'data <- create_random_sample_data_reg(10,10)
#'Lambda <- c(1,2,3)/100
#'cross_validation_reg(data, Lambda, m=3)
#'
#'@export
cross_validation_reg <- function(data, Lambda,m){
  list <- partition(data,m)
  cv <- vector(mode="numeric",length=0L)
  for (la in Lambda)
  {
    d <- 0
    for (i in 1:m)
    {
      d <- d + pruning_regression(greedy_cart_regression(list[[i+1]])$tree, la)$risk[1]

    }
    cv <- c(cv,d)
  }

  return(Lambda[which.min(cv)])
}



#' Cross Validation (Classification)
#' @description Chooses optimal parameter lambda from vector \code{Lambda}
#'
#'@param data  A list of training data.
#'@param m Integer
#'@param Lambda Vector of possible lambda values
#'@param type Either \code{"reg"} or \code{"class"}
#'
#'@return The optimal lambda estimation
#'
#'@details The integer \code{m} lets you choose how many subsets the partition of the training data \code{data} should have.
#' For each complement of the subset and every component of \code{Lambda} pruning will be conducted. The function
#' returns the entry of \code{Lambda}, which minimizes the risk of the trees given through the pruninig process.
#'
#'
#'@examples
#'data <- create_Sample_data_class(11)
#'Lambda <- c(1,2,3)/100
#'cross_validation_class(data, Lambda, m=3)
#'
#'@export
cross_validation_class <- function(data, Lambda,m){
  list <- partition(data,m)
  cv <- vector(mode="numeric",length=0L)
  for (la in Lambda)
  {
    d <- 0
    for (i in 1:m)
    {
      d <- d + pruning_classification(greedy_cart_classification(list[[i+1]])$tree, la)$risk[1]
    }
    cv <- c(cv,d)
  }
  cv
  return(Lambda[which.min(cv)])
}



#' Cross Validation
#' @description Chooses optimal parameter lambda from vector \code{Lambda}
#'
#'@param data  A list of training data.
#'@param m Integer
#'@param Lambda Vector of possible lambda values
#'@param type Either \code{"reg"} or \code{"class"}
#'
#'@return The optimal lambda estimation
#'
#'@details Choose the type of cross validation (regression or classification) which should be used.
#'See \code{cross_validation_reg} or \code{cross_validation_class} for further details.\cr
#'Return error if no valid type is chosen.
#'
#'@examples
#'data <- create_random_sample_data_reg(10,10)
#'Lambda <- c(1,2,3)/100
#'cross_validation(data, Lambda, m=3, type="reg")
#'
#'@export
cross_validation <- function(data,Lambda,m,type=NULL)
{
  if(type == "reg"){
    return(cross_validation_reg(data,Lambda,m))
  } else if(type == "class"){
    return(cross_validation_class(data,Lambda,m))
  } else{
    stop("Invalid type!")
  }
}



