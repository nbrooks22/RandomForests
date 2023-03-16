####################################################################################
#                         Plot Gieriges-Verfahren Regression
####################################################################################

printRegression <- function(data, plotname) {
  # Plot Daten
  plot(data$values$x, data$values$y, xlab = "x1", ylab = "y", main=plotname)

  # Trennlinien
  trennlinien <- data$tree %>% drop_na() %>% select(split_point) %>% unique() %>% arrange(split_point)

  # Plot Trennlinien
  for(linie in trennlinien) {
    abline(v = linie, lty=2, lwd=1)
  }

  # Schätzer
  bayesRegelDaten <- data$tree %>% filter(name == "leaf")
  
  # Max-Min
  minValue <- sort(data$values$x)[1] - 1
  maxValue <- sort(data$values$x, decreasing = T)[1] + 1
  
  # Plot Schätzer
  for (row in 1:nrow(bayesRegelDaten)) {
    value <- bayesRegelDaten$A[[row]][[1]]
    bayesRegel <- bayesRegelDaten$c_value[[row]]
    
    rightSideBorder <- trennlinien %>% filter(value <= split_point) %>% min()
    leftSideBorder <- trennlinien %>% filter(value >= split_point) %>% max()

    # Es existiert keine linke Trennlinie
    if (is.infinite(leftSideBorder)) {
      segments(minValue, bayesRegel, rightSideBorder, bayesRegel)
    }

    # Es existiert keine rechte Trennlinie
    if (is.infinite(rightSideBorder)) {
      segments(leftSideBorder, bayesRegel, maxValue, bayesRegel)
    }

    # Es existiert sowohl eine linke als auch eine rechte Trennlinie
    if (!is.infinite(leftSideBorder) && !is.infinite(rightSideBorder)) {
      segments(leftSideBorder, bayesRegel, rightSideBorder, bayesRegel)
    }
  }
  
  # Legende
  legend("topright", legend=c("Trainingsdaten", "Schätzer", "Trennlinien"), pch=c(1, NA, NA), lty=c(NA, 1, 2), cex=1.5)
}

####################################################################################
#                     Plot Gieriges-Verfahren Klassifikation
####################################################################################

printClassification <- function(data, plotname) {
  pal <- palette(c("red", "blue"))
  
  # Plot Daten
  plot(data$values$x[1], data$values$y[1], xlim=c(0, 1), ylim=c(0, 1), xlab = "x1", ylab = "x2", main=plotname, col = pal[data$values$classes[1]])

  for (point in 2:nrow(data$values)) {
    points(data$values$x[point], data$values$y[point], col = pal[data$values$classes[point]])
  }
  
  # Legende
  legend("topright", legend=c("Trennlinien", "Schätzung Klasse 1", "Schätzung Klasse 2"), col=c("black", pal[1], pal[2]), pch=c(NA, 1, 1), lty=c(1, NA, NA), cex=1.5)
}

####################################################################################
#                                  Plot Tree
####################################################################################

moveSplit <- function(data) {
  for (row in 1:nrow(data$tree)) {
    currentNode <- data$tree[row, ]
    leftChild <- data$tree %>% filter(node == currentNode$node * 2)
    
    # Überprüfe, ob linkes Kind existiert
    if (nrow(leftChild) > 0) {
      data$tree[row, ]$split_point <- leftChild$split_point
      data$tree[row, ]$split_index <- leftChild$split_index
    }
  }
  
  data$tree[data$tree$name == "leaf", ]$split_point <- NA 
  data$tree[data$tree$name == "leaf", ]$split_index <- NA
  
  return(data)
}

plotTree <- function(data) {
  data <- moveSplit(data)
  
  parentVector <- c()
  childVector <- c()
  
  for (row in 1:nrow(data$tree)) {
    currentNode <- data$tree[row, ]
    if (nrow(currentNode) > 0) {
      leftChild <- data$tree %>% filter(node == currentNode$node * 2)
      rightChild <- data$tree %>% filter(node == currentNode$node * 2 + 1)

      # Überprüfe, ob linkes Kind existiert
      if (nrow(leftChild) > 0) {
        # Füge Eltern-Knoten hinzu
        parentVector <- c(parentVector, paste0("Position: ",
                                               currentNode$node,
                                               "\n", "j = ",
                                               currentNode$split_index,
                                               ", s = ",
                                               round(currentNode$split_point, digits = 2)
                                               )
                          )
        # Füge Kind-Knoten/Leaf hinzu
        if (leftChild$name == "leaf") {
          childVector <- c(childVector, paste0("Position: ",
                                               leftChild$node,
                                               "\n", "y = ",
                                               round(leftChild$c_value, digits = 2)
                                               )
                           )
        } else {
          childVector <- c(childVector, paste0("Position: ",
                                               leftChild$node,
                                               "\n", "j = ",
                                               leftChild$split_index,
                                               ", s = ",
                                               round(leftChild$split_point, digits = 2)
                                               )
                           )
        }
      }

      # Überprüfe, ob rechtes Kind existiert
      if (nrow(rightChild) > 0) {
        # Füge Eltern-Knoten hinzu
        parentVector <- c(parentVector, paste0("Position: ",
                                               currentNode$node,
                                               "\n", "j = ",
                                               currentNode$split_index,
                                               ", s = ",
                                               round(currentNode$split_point, digits = 2)
                                               )
                          )
        # Füge Kind-Knoten/Leaf hinzu
        if (rightChild$name == "leaf") {
          childVector <- c(childVector, paste0("Position: ",
                                               rightChild$node,
                                               "\n", "y = ",
                                               round(rightChild$c_value, digits = 2)
                                               )
                           )
        } else {
          childVector <- c(childVector, paste0("Position: ",
                                               rightChild$node,
                                               "\n", "j = ",
                                               rightChild$split_index,
                                               ", s = ",
                                               round(rightChild$split_point, digits = 2)
                                               )
                           )
        }
      }
    }
  }
  
  df <- data.frame(parent = parentVector,
                   child = childVector)

  tree <- data.tree::FromDataFrameNetwork(df)
  data.tree::SetGraphStyle(tree, rankdir = "TB")
  data.tree::SetEdgeStyle(tree, arrowhead="normal", arrowsize=0.3)
  data.tree::SetNodeStyle(tree, style = "box", shape = "box", height=0.05, fontsize=8)
  plot(tree)
}
