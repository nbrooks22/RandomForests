
library(tidyverse)


count_leaves <- function(data){            # Funktion um für gegebenen Baum die Blätter zu zählen
  count <- data %>% filter(name=="leaf") %>% nrow()
  return(count)
} 


tree_seq <- function(trees){
  
  risk <- 0
  
  #Risiko Baum
  y1<-filter(trees,name=="leaf")$y
  c1<-filter(trees,name=="leaf")$c_value
 
  risk1 <- sum( (y1- c1)^2, na.rm=TRUE) / length(y1)
  risk1
  leaves <- count_leaves(trees)
  leaves 
  
  for (n in 1:length(trees$node)){
  old_leaves <- arrange(trees,desc(node))$node[n]   # Blatt mit höchster Knotennummer
  
  if(!old_leaves%%2==0) {
    old_leaves <- c(old_leaves-1,old_leaves)
  } else { old_leaves <- c(old_leaves, old_leaves+1)}
  old_leaves
  for (m in 1:ceiling(log(max(trees$node))/log(2))) # Alle Kindknoten ab der aktuellen Knotennummer auflisten
  { 
    if( max(old_leaves) <= max(trees$node))
    {
      old_leaves <- unique(sort(c(old_leaves,old_leaves *2,old_leaves*2+1))) 
      old_leaves <- old_leaves[ old_leaves %in% trees$node]
    }
  }
  old_leaves
  
  while(min(old_leaves) > 1) {
    # Höchste Blattnummer im Teilbaum
    
    if (identical(min(old_leaves)%%2,0)) {
      new_leaf <- min(old_leaves)/2 
      old_leaves <- sort(c(min(old_leaves)+1, old_leaves))
      old_leaves
    }  else {
      new_leaf <- (min(old_leaves)-1)/2  # Innerer Knoten der zum Blatt wird 
      old_leaves <- sort(c(min(old_leaves)-1, old_leaves))
    } 
    old_leaves
    new_leaf
    
    
    subtree <- filter(trees, ! node %in% old_leaves) # Blätter mit hoher Knotennummer aus dem Baum entfernen
    #print(subtree)
    
    subtree %>%  filter(node==new_leaf) %>% mutate(name=c("leaf")) -> make_leaf # Inneren Knoten isolieren
    make_leaf
    
    bind_rows(anti_join(subtree, make_leaf, by="node"), make_leaf) %>% arrange(desc(node)) -> subtree # Teilbaum mit innerem Knoten als Blatt
    # print(subtree,n=38)
    
    
    #Risiko Teilbaum
    y2 <- filter(subtree, name=="leaf")$y
    c2 <- filter(subtree, name=="leaf")$c_value
    y2
    c2
    
    risk2 <- sum( (y2-c2)^2,na.rm = TRUE)/length(y2)
    (risk2 - risk1)/(leaves- count_leaves(subtree))
    count_leaves(subtree)
    
    # Zu minimierende Funktion (Satz 6.19)
    temp_risk <- (risk2 - risk1) / (leaves - count_leaves(subtree))
    temp_risk
    risk
    temp_risk < risk
    
    if (temp_risk < risk) 
    {  
      risk <- temp_risk
      min_subtree <- subtree  # Teilbaum abspeichern, falls der Quotient kleiner ist als der Quotient des letzten Teilbaums
    }
  
    old_leaves <- sort(c(old_leaves, new_leaf))
    old_leaves
  }
  }
  return(min_subtree)
}



pruning_regression <- function(trees)
{
  trees <- list(trees)
  trees
  while( nrow(trees[[length(trees)]])>1 ) 
  {
    trees <- c(trees,list(tree_seq(trees[[length(trees)]])))
  }
    return(trees)
}


# Example
# trees <- greedy_cart_regression(create_realistic_sample_data_reg())
# trees 
# pruning_regression(trees)



