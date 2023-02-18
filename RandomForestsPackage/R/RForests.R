
#' RANDOM FOREST ALGORITHM (Regression)
#'
#' Condition to End: The Tree has t leaves - 6.52
#'
#' @param data Tibble that contains Regression Data
#' @param m Number of Samples that should be aquired
#' @param t Condition to End: The Tree has t leaves (insert tea leaf pun here)
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
#' @examples random_forest_regression(create_random_sample_data_reg(10, 100), 30, 40)
random_forest_regression <- function(data,m,t){
  # verwende greedy_cart mit zusätzlichem Schritt
  # wähle Teilmenge aus und mache Minimierung nur über die Teilmenge

  d <- nrow(data$x)
  # m < d
  S <- sample(1:d,m)

  #####################################################
  # greedy cart #######################################
  #####################################################

  # schreibe Beobachtungen von X in Liste (Elemente sind die Spalten)
  X <- lapply(seq_len(ncol(data$x)), function(i) data$x[,i])
  tree <- tibble(node = 1, name = "leaf", split_index = NA, split_point = NA, y = NA, A = list(NULL))

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
    for(i in seq_along(X)){
      # schauen, ob X_i in der Liste ist
      if(Position(function(x) identical(x, X[[i]]), A1(j,s,v), nomatch = 0) > 0) Y <- Y + data$y[i]
      #if(X[[i]] %in% A1(j,s)) Y <- Y + data$y[i]
    }
    1/n * Y
  }
  c2 <- function(j,s,v){
    Y <- 0
    for(i in seq_along(X)){
      # schauen, ob X_i in der Liste ist
      if(Position(function(x) identical(x, X[[i]]), A2(j,s,v), nomatch = 0) > 0) Y <- Y + data$y[i]
    }
    1/n * Y
  }

  # mache das hier so lange bis jedes Blatt nur noch einen Datenpunkt hat
  # D.h alle A(v) sind entweder leer oder haben nur ein Element
  # für v Blatt


  cond <- sapply(tree$A, length) # gibt an, wie viele Elemente jeweils in A(v) sind
  while(!all(cond %in% c(0,1)) & length(find_leaf1(tree)) <= t - 1){

    # finde Blätter
    leafs <- find_leaf1(tree)

    # Mache die nächsten Schritte für alle v in leafs
    for(v in leafs){
      # nur wenn length(A(v)) > 1
      if(length(tree[tree$node == v,]$A[[1]]) == 1) next
      if(length(tree[tree$node == v,]$A[[1]]) == 0) next

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
      t1 <- tree[tree$node == v,]$A[[1]]
      # objective = minimaler Wert
      # minimum = argmin
      # minimum nicht in den ganzen Daten von x (data$x) suchen, nur in A(v)

      #############################################
      # hier Änderung #############################
      #############################################
      # Minimierung nur über j in S ###############
      #############################################

      for(k in S){
        j <- k
        # nehme von jedem Listenelement die j-te Komponente
        idx <- sapply(t1, function(x) x[j])
        optimum <- optimize(minimize, c(min(idx), max(idx)))
        op[k] <- optimum$objective
        value[k] <- optimum$minimum
      }

      opt <- c() # c(j,s)
      opt[1] <- which.min(op) # j
      opt[2] <- value[which.min(op)] # s


      # füge die neuen Zeilen (Blätter) in tree ein


      #################################################
      # nur Durchführen bis Baum t Blätter hat ########
      #################################################


      # Füge ersten Knoten an, und schaue ob wir jetzt t Blätter haben
      tree %>%
        add_row(node = 2*v, split_index = opt[1], split_point = opt[2]) -> tree
      tree[tree$node == 2*v,]$A[[1]] <- A1(opt[1],opt[2],v)

      tree %>%
        mutate(name = ifelse(node == v, "inner node", ifelse(node == 2*v, "leaf", name))) -> tree


      # schaue, ob wir t Blätter haben:
      num_leafs <- find_leaf1(tree)
      if(length(num_leafs) == t) break

      tree %>%
        add_row(node = 2*v + 1, split_index = opt[1], split_point = opt[2]) -> tree
      tree[tree$node == 2*v + 1,]$A[[1]] <- A2(opt[1],opt[2],v)


      # benenne leafs in leaf um (im Tibble)
      tree %>%
        mutate(name = ifelse(node == v, "inner node", ifelse(node == 2*v, "leaf", ifelse(node == 2*v + 1, "leaf", name)))) -> tree





    }

    tree %>%
      filter(name == "leaf") -> leaf_tree
    cond <- sapply(leaf_tree$A, length) # solange wie es noch mind. zwei Elemente gibt



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
    tree %>%
      mutate(y = ifelse(node == leaf, 1/n*Y, y)) -> tree
  }


  # füge split_point/ split_index hinzu
  tree %>%
    mutate(y = ifelse(name == "inner node", 0, y)) %>%  # inner Knoten: setze y = 0
    mutate(split_index = ifelse(name == "leaf", 0, split_index),
           split_point = ifelse(name == "leaf", 0, split_point)) -> tree

  return(tree)

}


#' RANDOM FOREST ALGORITHM (Classification)
#'
#' Condition to End: The Tree has t leaves - 6.52
#'
#' @param data Tibble that contains Regression Data
#' @param m Number of Samples that should be aquired
#' @param t Condition to End: The Tree has t leaves
#'
#' @return tree in format: Tibble (BFS)
#' @export
#'
#' @examples random_forest_classification(create_random_sample_data_class(10, 100), 30, 40)
random_forest_classification <- function(data,m,t){
  d <- nrow(data$x)
  # m < d
  S <- sample(1:d,m)

  #####################################################
  # greedy cart #######################################
  #####################################################

  # analog wie regression
  # schreibe Beobachtungen von X in Liste (Elemente sind die Spalten)
  X <- lapply(seq_len(ncol(data$x)), function(i) data$x[,i])
  tree <- tibble(node = 1, name = "leaf", split_index = NA, split_point = NA, y = NA, A = list(NULL))

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
  while(!all(cond %in% c(0,1)) & length(find_leaf1(tree)) <= t - 1){

    # finde Blätter
    leafs <- find_leaf1(tree)

    # Mache die nächsten Schritte für alle v in leafs
    for(v in leafs){
      # nur wenn length(A(v)) > 1
      if(length(tree[tree$node == v,]$A[[1]]) == 1) next
      if(length(tree[tree$node == v,]$A[[1]]) == 0) next

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
      t1 <- tree[tree$node == v,]$A[[1]]
      # objective = minimaler Wert
      # minimum = argmin
      # minimum nicht in den ganzen Daten von x (data$x) suchen, nur in A(v)
      for(k in S){
        j <- k
        # nehme von jedem Listenelement die j-te Komponente
        idx <- sapply(t1, function(x) x[j])
        optimum <- optimize(minimize, c(min(idx), max(idx)))
        op[k] <- optimum$objective
        value[k] <- optimum$minimum
      }

      opt <- c() # c(j,s)
      opt[1] <- which.min(op) # j
      opt[2] <- value[which.min(op)] # s


      # füge die neuen Zeilen (Blätter) in tree ein
      # Füge ersten Knoten an, und schaue ob wir jetzt t Blätter haben
      tree %>%
        add_row(node = 2*v, split_index = opt[1], split_point = opt[2]) -> tree
      tree[tree$node == 2*v,]$A[[1]] <- A1(opt[1],opt[2],v)

      tree %>%
        mutate(name = ifelse(node == v, "inner node", ifelse(node == 2*v, "leaf", name))) -> tree


      # schaue, ob wir t Blätter haben:
      num_leafs <- find_leaf1(tree)
      if(length(num_leafs) == t) break

      tree %>%
        add_row(node = 2*v + 1, split_index = opt[1], split_point = opt[2]) -> tree
      tree[tree$node == 2*v + 1,]$A[[1]] <- A2(opt[1],opt[2],v)



      # benenne leafs in leaf um (im Tibble)
      tree %>%
        mutate(name = ifelse(node == v, "inner node", ifelse(node == 2*v, "leaf", ifelse(node == 2*v + 1, "leaf", name)))) -> tree

    }

    tree %>%
      filter(name == "leaf") -> leaf_tree
    cond <- sapply(leaf_tree$A, length) # solange wie es noch mind. zwei Elemente gibt

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
    mutate(y = ifelse(name == "inner node", 0, y)) %>%  # inner Knoten: setze y = 0
    mutate(split_index = ifelse(name == "leaf", 0, split_index),
           split_point = ifelse(name == "leaf", 0, split_point)) -> tree

  return(tree)
}
