library(tidyverse)


count_leaves <- function(data){            # Funktion um für gegebenen Baum die Blätter zu zählen
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
  
  subtree <- filter(trees, ! node %in% cut_leaves) # Kindknoten aus dem Baum entfernen
  
  subtree %>%  filter(node==new_leaf) %>% mutate(name=c("leaf")) -> make_leaf # Inneren Knoten isolieren
  
  bind_rows(anti_join(subtree, make_leaf, by="node"), make_leaf) %>% arrange(desc(node)) -> subtree # Teilbaum mit innerem Knoten als Blatt
  
  return(subtree)
  
}

tree_seq <- function(trees)
  {
  risk <- vector(mode="numeric",length=0L)
  
  #Risiko Baum
  y1<-filter(trees,name=="leaf")$y
  c1<-filter(trees,name=="leaf")$c_value
  
  risk1 <- sum( (y1- c1)^2, na.rm=TRUE) / length(y1)
  mutate(trees,risk=risk1)
  risk1
  leaves <- count_leaves(trees)
  leaves 
  
  inner_nodes <- c(trees$node[trees$name=="root"],trees$node[trees$name=="inner node"])
  inner_nodes 
  
  for (n in seq_along(inner_nodes))
  {
    new_leaf <- inner_nodes[n]
    subtree <- cut_tree(trees,new_leaf)
      
      #Risiko Teilbaum
      y2 <- filter(subtree, name=="leaf")$y
      c2 <- filter(subtree, name=="leaf")$c_value
      y2
      c2
      
      risk2 <- sum( (y2-c2)^2,na.rm = TRUE)/length(y2)
      
      # Zu minimierende Funktion (Satz 6.19)
      temp_risk <- (risk2 - risk1) / (leaves - count_leaves(subtree))
    
      if (temp_risk < risk) 
      {  
        risk <- temp_risk
        min_subtree <- mutate(subtree,risk=risk2)  # Teilbaum abspeichern, falls der Quotient kleiner ist als der Quotient des letzten Teilbaums
      }
  }
  
  return(min_subtree)
}



pruning_regression <- function(trees,lambda)
{
  if (lambda<=0)
  {stop("Lambda must be greater than 0")} # Gewichtung der Blätter muss größer Null sein
  trees <- list(trees)
  trees
  while( nrow(trees[[length(trees)]])>1 ) 
  {
    trees <- c(trees,list(tree_seq(trees[[length(trees)]])))
  }
  
  a <- vector(mode="numeric",length=0L)
  for (i in seq_along(trees))
  {
      a <- c(a, trees[[i]]$risk[1] + lambda * count_leaves(trees[[i]]))
  }
  p <- which.min(a)
  return(trees[[p]])
}

