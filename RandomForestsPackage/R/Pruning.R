
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

  #a <- bind_rows(make_leaf,old_leaves)$y
  #b <- a[[!a==0]]
  #c <- sum(a) / length(a)
  #c

  c <- make_leaf$c_value
  c

  trees %>% filter(! node %in% old_leaves$node) -> uncut_tree
  bind_rows(anti_join(make_leaf,old_leaves,by="node"),old_leaves) %>% mutate(c_value=c) -> subtree
  subtree
  bind_rows(anti_join(uncut_tree,subtree,by="node"),subtree) %>% arrange(node) -> subtree
  subtree


  return(subtree)

}

risk_tree <- function(trees)
{
  y_vec <- filter(trees, !(trees$y == 0))$y
  c_vec <- filter(trees, !(trees$y == 0))$c_value
  y_vec
  c_vec

  risk <- sum( (y_vec-c_vec)^2,na.rm = TRUE)/length(y_vec)
  risk
  return(risk)
}


tree_seq <- function(trees)
  {
  trees %>% filter(name=="old leaf") %>% mutate(name="older leaf") -> older_leaves
  older_leaves
  bind_rows(anti_join(trees,older_leaves,by="node"),older_leaves) %>% arrange(node) -> trees
  risk <- risk_tree(trees)
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
    subtree %>% mutate(risk=risk_tree(subtree)) -> subtree


    risk2 <- (risk_tree(subtree)-risk)/(leaves-count_leaves(subtree))
    risk2

    a <- c(a,risk2)
    a
  }
  min_subtree <- cut_tree(trees,inner_nodes[which.min(a)])
  min_subtree %>% mutate(risk=risk_tree(min_subtree)) -> min_subtree
  min_subtree
  return(min_subtree)
}



pruning_regression <- function(trees,lambda)
{
  if (lambda<=0)
  {stop("Lambda must be greater than 0")} # Gewichtung der Blätter muss größer Null sein
  risk <- risk_tree(trees)
  trees <- mutate(trees,risk=risk)
  trees <- list(trees)
  trees
  while( count_leaves(trees[[length(trees)]])>1 )
  {
    trees <- c(trees,list(tree_seq(trees[[length(trees)]])))
  }

  a <- vector(mode="numeric",length=0L)
  for (i in seq_along(trees))
  {
      a <- c(a, trees[[i]]$risk[1] + lambda * count_leaves(trees[[i]]))
  }
  trees[[which.min(a)]] %>% filter(name %in% c("root","leaf")) -> prun_tree
  return(prun_tree)
}
