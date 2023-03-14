
#' Greedy Algorithm (Regression)
#'
#' Greedy algorithm for regression data
#'
#' @param data a named list that contains regression data\cr the x values have the name x and are in
#' the form of a matrix where the row number gives the dimension of the data\cr the y
#' values have the name y and are in the form of a vector
#' @param depth Condition to end: the tree hast depth `depth`\cr must be greater than 0
#' @param num_split split only nodes which contain at least `num_split` elements \cr must be greater than or equal to 2
#' @param min_num only split a node, if both child nodes have at least `min_num` elements \cr must be greater than or equal to 1
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves \cr must be greater than or equal to 1
#' @param m parameter for Random Forest algorithm: positive number of coordinates which we want to use in each iteration \cr
#' must be smaller than the dimension of the data (if dimension is \eqn{>=} 2) or must be equal to the dimension of the data (if dimension is \eqn{=} 1) \cr
#' the default value is the dimension of the data
#'
#' @return An environment with the elements `dim`, `values` and `tree`.\cr
#' `dim` gives the dimension of the data. \cr
#' `values` is the regression data in a tibble. \cr
#' `tree` is the tree in the form of a tibble. It has the following columns \cr
#' \itemize{
#'    \item node: node n has the child nodes 2n and 2n + 1
#'    \item name: type of the node (root, inner node, leaf)
#'    \item split_index: at which index we split the data
#'    \item split_point: where we split the data. If node 2n has the split point s
#'    then all data in this node is less than s. Analog if the node 2n + 1 has the split point s
#'    then all data in this node is greater than or equal to s.
#'    \item y: only for leafs; the approximate value for the data in a leaf
#'    \item A: elements of the data in this node
#'    \item c_value: the approximate value for the data in a node
#' }
#'
#' @export
#'
#' @examples
#' X <- runif(100,0,1)
#' e <- rnorm(100,0,0.2)
#' Y <- sin(2*pi*X) + e
#' data <- list(x = matrix(X, nrow = 1), y = Y)
#' val <- greedy_cart_regression(data, depth = 3)
#' val$values
#' val$tree

greedy_cart_regression <- function(data, num_leaf = NULL, depth = 0, num_split = 2, min_num = 1, m = 0){
  # depth = Tiefe des Baumes die wir haben wollen
  # num_split = minimale Anzahl an Trainingsdaten die in einem Blatt sein sollen, damit noch gesplittet wird
  # bei num_split wird noch gesplittet, bei num_split - 1 nicht mehr
  # min_num = splitte nur, wenn die darauffolgenden leafs eine gewisse Größe haben
  # z.B nur splitten, wenn die daraus entstehenden leafs mind. 5 Elemente besitzen (min_num = 5)

  if(is.null(num_leaf)) num_leaf <- length(data$y)
  t <- num_leaf
  d <- nrow(data$x)


  stopifnot("depth must be greater than or equal to 0" = depth >= 0)
  stopifnot("num_split must be greater than or equal to 2" = num_split >= 2)
  stopifnot("min_num must be greater than or equal to 1" = min_num >= 1)
  stopifnot("num_leaf must be greater than or equal to 1" = num_leaf >= 1)

  row <- nrow(data$x)
  stopifnot("m is too big" = m <= row)
  if(m == 0) m <- row
  stopifnot("m must be greater than 0" = m > 0)

  greedyReg <- new.env()


  dat <- t(data$x)
  tb <- as_tibble(dat)
  if(row == 1){
    greedyReg$values <- bind_cols(as_tibble_col(as.vector(data$x), column_name = "x"), as_tibble_col(as.vector(data$y), column_name = "y"))
  } else{
    greedyReg$values <- bind_cols(tb, as_tibble_col(as.vector(data$y), column_name = "y"))
  }

  greedyReg$dim <- row
  # schreibe Beobachtungen von X in Liste (Elemente sind die Spalten)
  X <- lapply(seq_len(ncol(data$x)), function(i) data$x[,i])
  n <- length(data$y)
  mean <- 1/n*sum(data$y)
  tree <- tibble(node = 1, name = "leaf", split_index = NA, split_point = NA, y = NA, A = list(NULL), c_value = mean)

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

  while(!all(cond %in% 0:(num_split - 1)) & length(find_leaf1(tree)) <= t - 1 & depth_count < depth){
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
      op <- rep(NA, d)
      value <- rep(NA, d)
      t1 <- tree[tree$node == v,]$A[[1]]
      # objective = minimaler Wert
      # minimum = argmin
      # minimum nicht in den ganzen Daten von x (data$x) suchen, nur in A(v)

      S <- sample(1:d,m)
      S <- sort(S)
      for(k in S){
        j <- k
        # nehme von jedem Listenelement die j-te Komponente
        idx <- sapply(t1, function(x) x[j])
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
      num_leafs <- length(find_leaf1(tree))
      # Füge ersten Knoten an, und schaue ob wir jetzt t Blätter haben

      if(length(A_1) >= min_num & length(A_2) >= min_num){
        if(num_leafs == t){
          break
        } else{
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

  greedyReg$tree <- tree

  return(greedyReg)
}


#' Greedy Algorithm (Classification)
#'
#' Greedy algorithm for classification data
#'
#' @param data a named list that contains classification data\cr the x values have the name x and are in
#' the form of a matrix where the rownumber gives the dimension of the data\cr the y
#' values have the name y and are in the form of a vector
#' @param depth Condition to end: the tree hast depth `depth`\cr must be greater than 0
#' @param num_split split only nodes which contain at least `num_split` elements \cr must be greater than or equal to 2
#' @param min_num only split a node, if both child nodes have at least `min_num` elements \cr must be greater than or equal to 1
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves \cr must be greater than or equal to 1
#' @param m parameter for Random Forest algorithm: positive number of coordinates which we want to use in each iteration \cr
#' must be smaller than the dimension of the data (if dimension is \eqn{>=} 2) or must be equal to the dimension of the data (if dimension is \eqn{=} 1) \cr
#' the default value is the dimension of the data
#'
#'
#' @return An environment with the elements `dim`, `values` and `tree`.\cr
#' `dim` gives the dimension of the data. \cr
#' `values` gives back the classification data in a tibble. \cr
#' `tree` is the tree in the form of a tibble. It has the following columns \cr
#' \itemize{
#'    \item node: node n has the child nodes 2n and 2n + 1
#'    \item name: type of the node (root, inner node, leaf)
#'    \item split_index: at which index we split the data
#'    \item split_point: where we split the data. If node 2n has the split point s
#'    then all data in this node is less than s. Analog if the node 2n + 1 has the split point s
#'    then all data in this node is greater than or equal to s.
#'    \item y: only for leafs; the approximate value for the data in a leaf
#'    \item A: elements of the data in this node
#'    \item c_value: the approximate value for the data in a node
#' }
#'
#' @export
#'
#' @examples
#' X1 <- runif(200,0,1)
#' X2 <- runif(200,0,1)
#' e <- rnorm(200,0,0.2)
#' kappa <- function(x,y) y - 0.5 - 0.3*sin(2*pi*x)
#' f <- function(x,y,e){
#'   Y <- c()
#'   for(i in seq_along(x)){
#'     if(kappa(X1[i],X2[i]) - e[i] <= 0){
#'       Y[i] <- 1
#'     } else{
#'       Y[i] <- 2
#'     }
#'   }
#'   Y
#' }
#' data <- list(x = matrix(c(X1,X2), nrow = 2, byrow = TRUE), y = f(X1,X2,e))
#' val <- greedy_cart_classification(data, num_split = 10)
#' val$values
#' val$tree
greedy_cart_classification <- function(data, num_leaf = NULL, depth = 0, num_split = 2, min_num = 1, m = 0){

  if(is.null(num_leaf)) num_leaf <- length(data$y)
  t <- num_leaf
  d <- nrow(data$x)



  stopifnot("depth must be greater than or equal to 0" = depth >= 0)
  stopifnot("num_split must be greater than or equal to 2" = num_split >= 2)
  stopifnot("min_num must be greater than or equal to 1" = min_num >= 1)
  stopifnot("num_leaf must be greater than or equal to 1" = num_leaf >= 1)

  row <- nrow(data$x)
  stopifnot("m is too big" = m <= row)
  if(m == 0) m <- row
  stopifnot("m must be greater than 0" = m > 0)

  greedyCla <- new.env()

  greedyCla$dim <- row

  dat <- t(data$x)
  tb <- as_tibble(dat)
  if(row == 2){
    greedyCla$values <- bind_cols(as_tibble_col(as.vector(data$x[1, ]), column_name = "x"),
                                  as_tibble_col(as.vector(data$x[2, ]), column_name = "y"))
    greedyCla$values <- bind_cols(greedyCla$values, as_tibble_col(data$y, column_name = "classes"))
  } else{
    greedyCla$values <- bind_cols(tb, as_tibble_col(as.vector(data$y), column_name = "classes"))
  }



  # analog wie regression
  # schreibe Beobachtungen von X in Liste (Elemente sind die Spalten)
  X <- lapply(seq_len(ncol(data$x)), function(i) data$x[,i])
  n <- length(data$y)
  mean <- 1/n*sum(data$y)
  tree <- tibble(node = 1, name = "leaf", split_index = NA, split_point = NA, y = NA, A = list(NULL), c_value = mean)

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
    # nicht durch 0 teilen
    if(length(A) == 0) return(0)
    return(idx/length(A))
  }

  # c1
  c1 <- function(j,s,v){
    obj <- rep(NA,K)
    A_1 <- A1(j,s,v)
    for(k in 1:K){
      obj[[k]] <- p(k, A_1)
    }
    which.max(obj)
  }

  # c2
  c2 <- function(j,s,v){
    obj <- rep(NA,K)
    A_2 <- A2(j,s,v)
    for(k in 1:K){
      obj[[k]] <- p(k, A_2)
    }
    which.max(obj)
  }



  cond <- sapply(tree$A, length) # gibt an, wie viele Elemente jeweils in A(v) sind
  if(depth == 0){
    depth_count <- -1
  } else{
    depth_count <- 0
  }

  while(!all(cond %in% 0:(num_split - 1)) & length(find_leaf1(tree)) <= t - 1 & depth_count < depth){

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
        A_1 <- A1(j,s,v)
        A_2 <- A2(j,s,v)
        length(A_1)*(1-p(c1(j,s,v),A_1)) + length(A_2)*(1-p(c2(j,s,v), A_2))
      }


      # schauen, ob das funktioniert
      # j in {1,..,d} d = Dimension von X = nrow(data$x)
      # s in IR

      # suche für alle j das Minimum: und minimiere dann darüber
      op <- rep(NA, d)
      value <- rep(NA, d)
      t1 <- tree[tree$node == v,]$A[[1]]
      # objective = minimaler Wert
      # minimum = argmin
      # minimum nicht in den ganzen Daten von x (data$x) suchen, nur in A(v)
      S <- sample(1:d,m)
      S <- sort(S)

      for(k in S){
        j <- k
        # nehme von jedem Listenelement die j-te Komponente
        idx <- sapply(t1, function(x) x[j])
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


      # Es kann häufiger mal das selbe Minimum auftreten
      min <- which(op == min(op))
      if(length(min) >= 2){
        x <- sample(min, 1)
        opt[1] <- x
        opt[2] <- value[x]
      } else{
        opt[1] <- which.min(op) # j
        opt[2] <- value[which.min(op)] # s
      }



      c_1 <- c1(opt[1],opt[2],v)
      c_2 <- c2(opt[1],opt[2],v)

      # füge die neuen Zeilen (Blätter) in tree ein
      # nur, wenn # A1 und # A2 >= min_num sind
      A_1 <- A1(opt[1],opt[2],v)
      A_2 <- A2(opt[1],opt[2],v)
      num_leafs <- length(find_leaf1(tree))

      if(length(A_1) >= min_num & length(A_2) >= min_num){
         if(num_leafs == t){
          break
        } else{
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

  greedyCla$tree <- tree

  return(greedyCla)
}


#' Greedy Algorithm
#'
#' Greedy algorithm for either regression or classification data
#'
#' @param x column/list name(s) of the x value(s)
#' @param y column/list name of the y value
#' @param data tibble or named list with data
#' @param type "reg" for regression tree\cr "class" for classification tree
#' @param depth Condition to end: the tree hast depth `depth`\cr must be greater than 0
#' @param num_split split only nodes which contain at least `num_split` elements \cr must be greater than or equal to 2
#' @param min_num only split a node, if both child nodes have at least `min_num` elements \cr must be greater than or equal to 1
#' @param num_leaf Condition to end: the tree has `num_leaf` leaves \cr must be greater than or equal to 1
#' @param m parameter for Random Forest algorithm: positive number of coordinates which we want to use in each iteration \cr
#' must be smaller than the dimension of the data (if dimension is \eqn{>=} 2) or must be equal to the dimension of the data (if dimension is \eqn{=} 1) \cr
#' the default value is the dimension of the data
#'
#'
#' @return An environment with the elements `dim`, `values` and `tree`.\cr
#' `dim` gives the dimension of the data. \cr
#' `values` gives back the data in a tibble. \cr
#' `tree` is the tree in the form of a tibble. It has the following columns \cr
#' \itemize{
#'    \item node: node n has the child nodes 2n and 2n + 1
#'    \item name: type of the node (root, inner node, leaf)
#'    \item split_index: at which index we split the data
#'    \item split_point: where we split the data. If node 2n has the split point s
#'    then all data in this node is less than s. Analog if the node 2n + 1 has the split point s
#'    then all data in this node is greater than or equal to s.
#'    \item y: only for leafs; the approximate value for the data in a leaf
#'    \item A: elements of the data in this node
#'    \item c_value: the approximate value for the data in a node
#' }
#'
#' @export
#' @examples
#' X <- runif(100,0,1)
#' e <- rnorm(100,0,0.2)
#' Y <- sin(2*pi*X) + e
#' data <- list(a = X, b = Y)
#' val <- greedy_cart(x = a, y = b, data = data, type = "reg")
#' val$values
#' val$tree
#'
#' X1 <- runif(200,0,1)
#' X2 <- runif(200,0,1)
#' e <- rnorm(200,0,0.05)
#' k <- function(x,y) (x-0.5)*(y-0.5)
#' g <- function(x,y,e){
#'   Y <- c()
#'   for(i in seq_along(x)){
#'    if(k(X1[i],X2[i]) - e[i] <= 0){
#'      Y[i] <- 1
#'     } else{
#'       Y[i] <- 2
#'     }
#'   }
#'   Y
#' }
#' tbl <- tibble(x1 = X1, x2 = X2, y = g(X1,X2,e))
#' val <- greedy_cart(x = c(x1,x2), y = y, data = tbl, type = "class", depth = 3)
#' val$values
#' val$tree


greedy_cart <- function(x,y,data, type = NULL, num_leaf = NULL ,depth = 0, num_split = 2, min_num = 1, m = 0){
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

  stopifnot("y must be numeric" = is.numeric(data2))
  # y muss eindimensional sein
  stopifnot("y must be one-dimensional" = NCOL(data2) == 1)

  # x und y sollen richtige Länge haben
  # x ist ein vielfaches der Länge von y
  stopifnot("x and y don't have compatible length" = as.integer(length(data1)/length(data2))*length(data2) == length(data1))

  # Überprüfung der Argumente
  stopifnot("depth must be greater than or equal to 0" = depth >= 0)
  stopifnot("num_split must be greater than or equal to 2" = num_split >= 2)
  stopifnot("min_num must be greater than or equal to 1" = min_num >= 1)


  mat <- matrix(data1, nrow = length(data1)/length(data2), byrow = TRUE)
  dat <- list(x = mat, y = data2)

  # wenn kein Typ angegeben wurde -> versuche Typ zu erraten
  if(is.null(type)){
    y_int <- as.integer(data2)
    # wenn y natürliche Zahl -> classification
    if(all(y_int == data2) & all(y_int >= 1)){
      type <- "class"
      warning("Type was forgotten. Type was set to classification")
    } else{
      type <- "reg"
      warning("Type was forgotten. Type was set to regression")
    }
  }


  if(type == "reg"){
    return(greedy_cart_regression(dat, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, m = m))
  } else if(type == "class"){
    return(greedy_cart_classification(dat, num_leaf = num_leaf, depth = depth, num_split = num_split, min_num = min_num, m = m))
  } else{
    stop("Invalid type!")
  }
}



