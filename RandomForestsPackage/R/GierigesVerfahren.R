
#' GREEDY ALGORITHM (Regression)
#'
#' Condition to End: Every Leaf contains one Value - 6.15
#'
#' @param data Tibble that contains Regression Data
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
#' @examples greedy_cart_regression(create__random_sample_data_reg(12, 50))
greedy_cart_regression <- function(data, depth = 0, num_split = 2, min_num = 1){
  # depth = Tiefe des Baumes die wir haben wollen
  # num_split = minimale Anzahl an Trainingsdaten die in einem Blatt sein sollen, damit noch gesplittet wird
  # bei num_split wird noch gesplittet, bei num_split - 1 nicht mehr
  # min_num = splitte nur, wenn die darauffolgenden leafs eine gewisse Größe haben
  # z.B nur splitten, wenn die daraus entstehenden leafs mind. 5 Elemente besitzen (min_num = 5)
  
  
  # schreibe Beobachtungen von X in Liste (Elemente sind die Spalten)
  X <- lapply(seq_len(ncol(data$x)), function(i) data$x[,i])
  tree <- tibble(node = 1, name = "leaf", split_index = NA, split_point = NA, y = NA, A = list(NULL), c_value = NA)

  ##### Algorithmus
  tree[tree$node == 1,]$A[[1]] <- X # A = list of length(X)
  # Zugriff auf Daten von A: A[[1]]


  # Schritt 1
  # schreibe Funktion, die A(v) berechnet
  # für gegebenes Blatt v
  A1 <- function(j,s,v){
    set <- tree[tree$node == v, ]$A[[1]]
    A_1 <- list()
    for(i in seq_along(set)){
      if(set[[i]][j] < s){
        A_1[[length(A_1) + 1]] <- set[[i]]
      }
    }
    # j = welche Dimension
    A_1
  }

  A2 <- function(j,s,v){
    set <- tree[tree$node == v, ]$A[[1]]
    A_2 <- list()
    for(i in seq_along(set)){
      if(set[[i]][j] >= s){
        A_2[[length(A_2) + 1]] <- set[[i]]
      }
    }
    A_2
  }

  n <- length(data$y)
  c1 <- function(j,s,v){
    Y <- 0
    A_1 <- A1(j,s,v)
    for(i in seq_along(X)){
      # schauen, ob X_i in der Liste ist
      if(Position(function(x) identical(x, X[[i]]), A_1, nomatch = 0) > 0) Y <- Y + data$y[i]
      #if(X[[i]] %in% A1(j,s)) Y <- Y + data$y[i]
    }
    1/length(A_1) * Y
  }
  c2 <- function(j,s,v){
    Y <- 0
    A_2 <- A2(j,s,v)
    for(i in seq_along(X)){
      # schauen, ob X_i in der Liste ist
      if(Position(function(x) identical(x, X[[i]]), A_2, nomatch = 0) > 0) Y <- Y + data$y[i]
    }
    1/length(A_2) * Y
  }

  # mache das hier so lange bis jedes Blatt nur noch einen Datenpunkt hat
  # D.h alle A(v) sind entweder leer oder haben nur ein Element
  # für v Blatt

  cond <- sapply(tree$A, length) # gibt an, wie viele Elemente jeweils in A(v) sind
  
  # wenn depth nicht 0, zähle die Tiefe des Baumes
  # wenn 0: verändere dept_count nicht mehr
  if(depth == 0){
    depth_count <- -1
  } else{
    depth_count <- 0
  }                
            
  while(!all(cond %in% 0:(num_split - 1)) & depth_count < depth){
    # speichere den tree. Ist wichtig für num_split
    # -> Abbruchbedingung: wenn sich der Tree nicht geändert hat, d.h keine neuen Blätter hinzugekommen sind -> abbrechen
    tree1 <- tree
    
    # finde Blätter
    leafs <- find_leaf1(tree)

    # Mache die nächsten Schritte für alle v in leafs
    for(v in leafs){
      # nur wenn length(A(v)) > 1
      if(length(tree[tree$node == v,]$A[[1]]) %in% 0:(num_split - 1)) next
      # wenn Anzahl im Leaf < num_split ist, splitte nicht mehr
      

      # Schritt 3
      # löse das Minimierungsproblem

      # minimiere diese Funktion
      minimize <- function(s){
        Y <- 0
        A <- A1(j,s,v)
        c_1 <- c1(j,s,v)
        c_2 <- c2(j,s,v)
        for(i in seq_along(X)){
          # schauen, ob X_i in der Liste ist
          if(Position(function(x) identical(x, X[[i]]), A, nomatch = 0) > 0){
            Y <- Y + (data$y[i] - c_1)^2
          } else{
            Y <- Y + (data$y[i] - c_2)^2
          }
        }
        Y
      }

      # j in {1,..,d} d = Dimension von X = nrow(data$x)
      # s in IR

      # suche für alle j das Minimum: und minimiere dann darüber
      op <- c()
      value <- c()
      t <- tree[tree$node == v,]$A[[1]]
      # objective = minimaler Wert
      # minimum = argmin
      # minimum nicht in den ganzen Daten von x (data$x) suchen, nur in A(v)
      for(k in 1:nrow(data$x)){
        j <- k
        # nehme von jedem Listenelement die j-te Komponente
        idx <- sapply(t, function(x) x[j])
        # schauen, ob idx nur gleiche Elemente hat
        if(length(unique(idx)) == 1){
          optimum <- minimize(idx[[1]])
          op[k] <- optimum
          value[k] <- idx[[1]]
        } else{
          optimum <- optimize(minimize, c(min(idx), max(idx)))
          op[k] <- optimum$objective
          value[k] <- optimum$minimum
        }
      }

      opt <- c() # c(j,s)
      opt[1] <- which.min(op) # j
      opt[2] <- value[which.min(op)] # s
      
      c_1 <- c1(opt[1],opt[2],v)
      c_2 <- c2(opt[1],opt[2],v)
                      
      # füge die neuen Zeilen (Blätter) in tree ein
      # nur, wenn # A1 und # A2 >= min_num sind
      A_1 <- A1(opt[1],opt[2],v)
      A_2 <- A2(opt[1],opt[2],v)
      if(length(A_1) >= min_num & length(A_2) >= min_num){
        tree %>%
          add_row(node = 2*v, split_index = opt[1], split_point = opt[2], c_value = c_1) %>%
          add_row(node = 2*v + 1, split_index = opt[1], split_point = opt[2], c_value = c_2) -> tree
        tree[tree$node == 2*v,]$A[[1]] <- A1(opt[1],opt[2],v)
        tree[tree$node == 2*v + 1,]$A[[1]] <- A2(opt[1],opt[2],v)
        # benenne leafs in leaf um (im Tibble)
        tree %>% 
          mutate(name = ifelse(node == v, "inner node", ifelse(node == 2*v, "leaf", ifelse(node == 2*v + 1, "leaf", name)))) -> tree
        
      }
    }

    tree %>%
      filter(name == "leaf") -> leaf_tree
    cond <- sapply(leaf_tree$A, length) # solange wie es noch mind. zwei Elemente gibt
    if(depth != 0){
      depth_count <- depth_count + 1
    }
                      
    # wenn sich das Tibble nicht geändert hat -> while Schleife verlassen
    # alter tree: tree1
    # neuer tree: tree
    if(isTRUE(all.equal(tree, tree1))) break

  }

  # wenn alle leafs nur noch ein Punkt enthalten:
  # y_m einfügen
  leafs <- find_leaf1(tree)
  n <- length(data$y)
  for(leaf in leafs){
    tr <- tree[tree$node == leaf,]$A[[1]]
    Y <- 0
    for(i in seq_along(X)){
      # schauen, ob X_i in der Liste ist
      if(Position(function(x) identical(x, X[[i]]), tr, nomatch = 0) > 0) Y <- Y + data$y[i]
      #if(X[[i]] %in% A1(j,s)) Y <- Y + data$y[i]
    }
    
    # hier y verändern
    tree %>%
      mutate(y = ifelse(node == leaf, 1/length(tr)*Y, y)) -> tree
  }




  # füge split_point/ split_index hinzu
  tree %>%
    mutate(y = ifelse(name == "inner node", 0, y)) %>%  # inner Knoten: setze y = 0
    mutate(name = ifelse(node == 1, "root", name)) -> tree # benenne erstes Element in root um

  return(tree)
}


#' GREEDY ALGORITHM (Classification)
#'
#' Condition to End: Every Leaf contains one Value - 6.16
#'
#' @param data Tibble that contains Classification Data
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
#' @examples greedy_cart_classification(create__random_sample_data_class(10, 50))
greedy_cart_classification <- function(data, depth = 0, num_split = 2, min_num = 1){
  # analog wie regression
  # schreibe Beobachtungen von X in Liste (Elemente sind die Spalten)
  X <- lapply(seq_len(ncol(data$x)), function(i) data$x[,i])
  tree <- tibble(node = 1, name = "leaf", split_index = NA, split_point = NA, y = NA, A = list(NULL), c_value = NA)

  ##### Algorithmus
  tree[tree$node == 1,]$A[[1]] <- X # A = list of length(X)

  K <- length(unique(data$y))
  # Schritt 3
  # minimiere anders:

  A1 <- function(j,s,v){
    set <- tree[tree$node == v, ]$A[[1]]
    A_1 <- list()
    for(i in seq_along(set)){
      if(set[[i]][j] < s){
        A_1[[length(A_1) + 1]] <- set[[i]]
      }
    }
    # j = welche Dimension
    A_1
  }

  A2 <- function(j,s,v){
    set <- tree[tree$node == v, ]$A[[1]]
    A_2 <- list()
    for(i in seq_along(set)){
      if(set[[i]][j] >= s){
        A_2[[length(A_2) + 1]] <- set[[i]]
      }
    }
    A_2
  }


  # p
  # A = Liste
  # k = Klassifikation
  p <- function(k,A){
    idx <- 0
    for(i in seq_along(X)){
      if(Position(function(x) identical(x, X[[i]]), A, nomatch = 0) > 0 & data$y[[i]] == k){
        idx <- idx + 1
      }
    }
    return(idx/length(A))
  }

  # c1
  c1 <- function(j,s,v){
    obj <- rep(NA,K)
    for(k in 1:K){
      obj[[k]] <- p(k, A1(j,s,v))
    }
    which.max(obj)
  }

  # c2
  c2 <- function(j,s,v){
    obj <- rep(NA,K)
    for(k in 1:K){
      obj[[k]] <- p(k, A2(j,s,v))
    }
    which.max(obj)
  }



  cond <- sapply(tree$A, length) # gibt an, wie viele Elemente jeweils in A(v) sind
  if(depth == 0){
    depth_count <- -1
  } else{
    depth_count <- 0
  }
                  
  while(!all(cond %in% 0:(num_split - 1)) & depth_count < depth){
    
    tree1 <- tree
    # finde Blätter
    leafs <- find_leaf1(tree)

    # Mache die nächsten Schritte für alle v in leafs
    for(v in leafs){
      # nur wenn length(A(v)) > 1
      if(length(tree[tree$node == v,]$A[[1]]) %in% 0:(num_split - 1)) next


      # Schritt 3
      # löse das Minimierungsproblem

      # minimiere diese Funktion
      minimize <- function(s){
        length(A1(j,s,v))*(1-p(c1(j,s,v),A1(j,s,v))) + length(A2(j,s,v))*(1-p(c2(j,s,v), A2(j,s,v)))
      }


      # schauen, ob das funktioniert
      # j in {1,..,d} d = Dimension von X = nrow(data$x)
      # s in IR

      # suche für alle j das Minimum: und minimiere dann darüber
      op <- c()
      value <- c()
      t <- tree[tree$node == v,]$A[[1]]
      # objective = minimaler Wert
      # minimum = argmin
      # minimum nicht in den ganzen Daten von x (data$x) suchen, nur in A(v)
      for(k in 1:nrow(data$x)){
        j <- k
        # nehme von jedem Listenelement die j-te Komponente
        idx <- sapply(t, function(x) x[j])
        optimum <- optimize(minimize, c(min(idx), max(idx)))
        op[k] <- optimum$objective
        value[k] <- optimum$minimum
      }

      opt <- c() # c(j,s)
      opt[1] <- which.min(op) # j
      opt[2] <- value[which.min(op)] # s

      c_1 <- c1(opt[1],opt[2],v)
      c_2 <- c2(opt[1],opt[2],v)
                      
      # füge die neuen Zeilen (Blätter) in tree ein
      # nur, wenn # A1 und # A2 >= min_num sind
      A_1 <- A1(opt[1],opt[2],v)
      A_2 <- A2(opt[1],opt[2],v)
      if(length(A_1) >= min_num & length(A_2) >= min_num){
        tree %>%
          add_row(node = 2*v, split_index = opt[1], split_point = opt[2], c_value = c_1) %>%
          add_row(node = 2*v + 1, split_index = opt[1], split_point = opt[2], c_value = c_2) -> tree
        tree[tree$node == 2*v,]$A[[1]] <- A1(opt[1],opt[2],v)
        tree[tree$node == 2*v + 1,]$A[[1]] <- A2(opt[1],opt[2],v)
        # benenne leafs in leaf um (im Tibble)
        tree %>% 
          mutate(name = ifelse(node == v, "inner node", ifelse(node == 2*v, "leaf", ifelse(node == 2*v + 1, "leaf", name)))) -> tree
        
      }
    }

    tree %>%
      filter(name == "leaf") -> leaf_tree
    cond <- sapply(leaf_tree$A, length) # solange wie es noch mind. zwei Elemente gibt
    if(depth != 0){
      depth_count <- depth_count + 1
    }
    
    # wenn sich das Tibble nicht geändert hat -> while Schleife verlassen
    # alter tree: tree1
    # neuer tree: tree
    if(isTRUE(all.equal(tree, tree1))) break
                      
  }


  # y_m verändern
  leafs <- find_leaf1(tree)
  n <- length(data$y)
  for(leaf in leafs){
    tr <- tree[tree$node == leaf,]$A[[1]]
    obj <- rep(NA,K)
    for(k in 1:K){
      obj[[k]] <- p(k, tr)*length(tr)
    }
    tree %>%
      mutate(y = ifelse(node == leaf, which.max(obj), y)) -> tree
  }

  # füge split_point/ split_index hinzu
  tree %>%
    mutate(y = ifelse(name == "inner node", 0, y)) %>% # inner Knoten: setze y = 0
    mutate(name = ifelse(node == 1, "root", name)) -> tree # benenne erstes Element in root um

  return(tree)
}
                      
######## benötigen library(rlang) für das hier                    
# regression und classification in eine Funktion schreiben                      
greedy_cart <- function(x,y,data, type = "reg", depth = 0, num_split = 2, min_num = 1){
  # Daten umformatieren
  # hier kann man auch schauen, ob die Daten 
  # in der richtigen Struktur sind
  
 
  x1 <- enexpr(x)
  y1 <- enexpr(y)
  data1 <- eval_tidy(x1,data)
  data2 <- eval_tidy(y1,data)  

  # Typüberprüfung:
  # Daten (X) sollten Zahlen sein
  stopifnot("x must be numeric" = is.numeric(data1))
  # bei classification kann Y auch TRUE/FALSE sein 
  # (wird dann dementsprechend mit 1/0 übersetzt)
  if(type == "reg") stopifnot("y must be numeric" = is.numeric(data2))
  if(type == "class") stopifnot("y must be numeric or logical" = is.numeric(data2)|is.logical(data2))
  # x und y sollen richtige Länge haben
  # x ist ein vielfaches der Länge von y
  stopifnot("x and y don't have compatible length" = as.integer(length(data1)/length(data2))*length(data2) == length(data1))
  
  
  m <- matrix(data1, nrow = length(data1)/length(data2), byrow = TRUE)
  dat <- list(x = m, y = data2)
  if(type == "reg"){
    return(greedy_cart_regression(dat, depth = depth, num_split = num_split, min_num = min_num))
  } else if(type == "class"){
    return(greedy_cart_classification(dat, depth = depth, num_split = num_split, min_num = min_num))
  } else{
    stop("Invalid type!")
  }
}


### Beispiel Dateneingabe:
## Regression (eindimensional)                      
# data1 <- list(a = X, b = Y)    
# greedy_cart(x = a, y = b, data = data1, type = "reg")
## Regression (zweidimensional)
# data2 <- list(a = X, b = -X, c = Y) a ist x1 Koordinate, b ist x2 Koordinate
# greedy_cart(x = c(a,b), y = c, data = data2, type = "reg")
## Klassifikation
# data3 <- tibble(x1 = X1, x2 = X2, y = f(X1,X2,e)) mit X1, X2, f,... aus vorherigem Beispiel
# greedy_cart(x = c(x1,x2), y = y, data = data3, type = "class")                      
                                   
