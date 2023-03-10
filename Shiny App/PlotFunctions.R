####################################################################################
#                         Plot Gieriges-Verfahren Regression
####################################################################################

printGreedyCartRegression <- function(data) {
  # Plot Daten
  plot(data$values$x, data$values$y, xlab = "x1", ylab = "y", main="Gieriges Verfahren Regressionsproblem")

  # Trennlinien
  trennlinien <- data$tree %>% drop_na() %>% filter(split_point > 0) %>% select(split_point) %>% unique() %>% arrange(split_point)

  # Plot Trennlinien
  for(linie in trennlinien) {
    abline(v = linie, lty=2, lwd=1)
  }

  # Schätzer
  bayesRegelDaten <- data$tree %>% filter(name == "leaf")

  # Plot Schätzer
  for (row in 1:nrow(bayesRegelDaten)) {
    value <- bayesRegelDaten$A[[row]][[1]]
    bayesRegel <- bayesRegelDaten$y[[row]]

    rightSideBorder <- trennlinien %>% filter(value <= split_point) %>% first() %>% .[[1]]
    leftSideBorder <- trennlinien %>% filter(value >= split_point) %>% last() %>% .[[1]]

    # Es existiert keine linke Trennlinie
    if (is.na(leftSideBorder)) {
      segments(-1, bayesRegel, rightSideBorder, bayesRegel)
    }

    # Es existiert keine rechte Trennlinie
    if (is.na(rightSideBorder)) {
      segments(leftSideBorder, bayesRegel, 2, bayesRegel)
    }

    # Es existiert sowohl eine linke als auch eine rechte Trennlinie
    if (!is.na(leftSideBorder) && !is.na(rightSideBorder)) {
      segments(leftSideBorder, bayesRegel, rightSideBorder, bayesRegel)
    }
  }
  
  # Legende
  legend("topright", legend=c("Trainingsdaten", "Schätzer", "Trennlinien"), pch=c(1, NA, NA), lty=c(NA, 1, 2), cex=1.5)
}

####################################################################################
#                     Plot Gieriges-Verfahren Klassifikation
####################################################################################

printGreedyCartClassification <- function(data) {
  pal <- palette(c("red", "blue"))
  
  # Plot Daten
  plot(data$values$x[1], data$values$y[1], xlim=c(0, 1), ylim=c(0, 1), xlab = "x1", ylab = "x2", main="Gieriges Verfahren Klassifikationsproblem", col = pal[data$values$classes[1]])

  for (point in 2:nrow(data$values)) {
    points(data$values$x[point], data$values$y[point], col = pal[data$values$classes[point]])
  }
  
  # Legende
  legend("topright", legend=c("Trennlinien", "Schätzung Klasse 1", "Schätzung Klasse 2"), col=c("black", pal[1], pal[2]), pch=c(NA, 1, 1), lty=c(1, NA, NA), cex=1.5)
}

####################################################################################
#                                  Plot Tree
####################################################################################

plotTree <- function(data) {
  parentVector <- c()
  childVector <- c()
  values <- c()

  countNodes <- data$tree$node %>% last()
  for (nodePosition in 1:countNodes) {
    currentNode <- data$tree %>% filter(node == nodePosition)
    if (nrow(currentNode) > 0) {
      leftChild <- data$tree %>% filter(node == nodePosition * 2)
      rightChild <- data$tree %>% filter(node == nodePosition * 2 + 1)

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
                                               "\n", "j = ",
                                               leftChild$split_index,
                                               ", s = ",
                                               round(leftChild$split_point, digits = 2),
                                               "\n", "y = ",
                                               round(leftChild$y, digits = 2)
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
                                               "\n", "j = ",
                                               rightChild$split_index,
                                               ", s = ",
                                               round(rightChild$split_point, digits = 2),
                                               "\n", "y = ",
                                               round(rightChild$y, digits = 2)
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
