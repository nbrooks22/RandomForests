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

plotLinesColor <- function(data, currentNodePosition = 1, leftBelowPoint = NULL, rightAbovePoint = NULL, colorpal) {
  currentNode <- data$tree %>% filter(node == currentNodePosition)

  if (nrow(currentNode) > 0 && currentNode$name != "leaf") {
    # Klasse 1
    if (currentNode$split_index == 1) {
      if (currentNodePosition == 1) {
        # Current Node ist Root-Node
        leftBelowPoint <- c()
        rightAbovePoint <- c()

        # Finde Zone
        for (point in 1:length(data$tree$A[[1]])) {
          # Setze Standardpunkt
          if (is.null(leftBelowPoint)) {leftBelowPoint <- data$tree$A[[1]][[point]]}
          if (is.null(rightAbovePoint)) {rightAbovePoint <- data$tree$A[[1]][[point]]}

          # Vergleiche ob der linke untere Punkt wirklich der letzte ist
          if (data$tree$A[[1]][[point]][1] <= leftBelowPoint[1]) {
            leftBelowPoint[1] <- data$tree$A[[1]][[point]][1]
          }

          if (data$tree$A[[1]][[point]][2] <= leftBelowPoint[2]) {
            leftBelowPoint[2] <- data$tree$A[[1]][[point]][2]
          }

          # Vergleiche ob der rechte obere Punkt wirklich der letzte ist
          if (data$tree$A[[1]][[point]][1] >= rightAbovePoint[1]) {
            rightAbovePoint[1] <- data$tree$A[[1]][[point]][1]
          }

          if (data$tree$A[[1]][[point]][2] >= rightAbovePoint[2]) {
            rightAbovePoint[2] <- data$tree$A[[1]][[point]][2]
          }
        }

        # Zeichne erste Linie
        segments(currentNode$split_point, leftBelowPoint[2], currentNode$split_point, rightAbovePoint[2])
      } else {
        segments(currentNode$split_point, leftBelowPoint[2], currentNode$split_point, rightAbovePoint[2])
      }
      # Linkes Kind
      plotLinesColor(data, currentNodePosition * 2, leftBelowPoint, c(currentNode$split_point, rightAbovePoint[2]), colorpal)
      # Rechtes Kind
      plotLinesColor(data, currentNodePosition * 2 + 1, c(currentNode$split_point, leftBelowPoint[2]), rightAbovePoint, colorpal)
    }

    # Klasse 2
    if (currentNode$split_index == 2) {
      if (currentNodePosition == 1) {
        # Current Node ist Root-Node
        leftBelowPoint <- c()
        rightAbovePoint <- c()

        # Finde Zone
        for (point in 1:length(data$tree$A[[1]])) {
          # Setze Standardpunkt
          if (is.null(leftBelowPoint)) {leftBelowPoint <- data$tree$A[[1]][[point]]}
          if (is.null(rightAbovePoint)) {rightAbovePoint <- data$tree$A[[1]][[point]]}

          # Vergleiche ob der linke untere Punkt wirklich der letzte ist
          if (data$tree$A[[1]][[point]][1] <= leftBelowPoint[1]) {
            leftBelowPoint[1] <- data$tree$A[[1]][[point]][1]
          }

          if (data$tree$A[[1]][[point]][2] <= leftBelowPoint[2]) {
            leftBelowPoint[2] <- data$tree$A[[1]][[point]][2]
          }

          # Vergleiche ob der rechte obere Punkt wirklich der letzte ist
          if (data$tree$A[[1]][[point]][1] >= rightAbovePoint[1]) {
            rightAbovePoint[1] <- data$tree$A[[1]][[point]][1]
          }

          if (data$tree$A[[1]][[point]][2] >= rightAbovePoint[2]) {
            rightAbovePoint[2] <- data$tree$A[[1]][[point]][2]
          }
        }

        # Zeichne erste Linie
        segments(leftBelowPoint[1], currentNode$split_point, rightAbovePoint[1], currentNode$split_point)
      } else {
        segments(leftBelowPoint[1], currentNode$split_point, rightAbovePoint[1], currentNode$split_point)
      }
      # Linkes Kind
      plotLinesColor(data, currentNodePosition * 2, leftBelowPoint, c(rightAbovePoint[1], currentNode$split_point), colorpal)
      # Rechtes Kind
      plotLinesColor(data, currentNodePosition * 2 + 1, c(leftBelowPoint[1], currentNode$split_point), rightAbovePoint, colorpal)
    }
  }

  if (currentNode$name == "leaf") {
    rect(leftBelowPoint[1], leftBelowPoint[2], rightAbovePoint[1], rightAbovePoint[2], col = colorpal[currentNode$c_value])
  }

}

printClassification <- function(data, plotname) {
  color <- c("red", "blue")
  data1 <- list()
  # Plot Daten
  x_min <- min(data$values$x) - 0.01
  x_max <- max(data$values$x) + 0.01
  y_max <- max(data$values$y) + 0.01
  y_min <- min(data$values$y) - 0.01
  plot(data$values$x[1], data$values$y[1], xlim = c(x_min,x_max), ylim = c(y_min,y_max), xlab = "x1", ylab = "x2", main=plotname, col = color[data$values$classes[1]])

  for (point in 2:nrow(data$values)) {
    points(data$values$x[point], data$values$y[point], col = color[data$values$classes[point]])
  }

  # Plot Linien und Color
  data1 <- moveSplit(data)
  plotLinesColor(data1, colorpal = c(rgb(1.0, 0, 0, alpha=0.4), rgb(0, 0, 1.0, alpha=0.4)))

  # Legende
  legend("topright", legend=c("Trennlinien", "Schätzung Klasse 1", "Schätzung Klasse 2"), col=c("black", color[1], color[2]), pch=c(NA, 1, 1), lty=c(1, NA, NA), cex=1.5)
}

####################################################################################
#                                  Plot Tree
####################################################################################

moveSplit <- function(data) {
  data1 <- list()
  data1$tree <- data$tree
  for (row in 1:nrow(data1$tree)) {
    currentNode <- data1$tree[row, ]
    leftChild <- data1$tree %>% filter(node == currentNode$node * 2)

    # Überprüfe, ob linkes Kind existiert
    if (nrow(leftChild) > 0) {
      data1$tree[row, ]$split_point <- leftChild$split_point
      data1$tree[row, ]$split_index <- leftChild$split_index
    }
  }

  data1$tree[data$tree$name == "leaf", ]$split_point <- NA
  data1$tree[data$tree$name == "leaf", ]$split_index <- NA

  return(data1)
}

plotTree <- function(data) {
  data1 <- moveSplit(data)

  parentVector <- c()
  childVector <- c()

  for (row in 1:nrow(data1$tree)) {
    currentNode <- data1$tree[row, ]
    if (nrow(currentNode) > 0) {
      leftChild <- data1$tree %>% filter(node == currentNode$node * 2)
      rightChild <- data1$tree %>% filter(node == currentNode$node * 2 + 1)

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



