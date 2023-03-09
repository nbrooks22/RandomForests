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
}

####################################################################################
#                     Plot Gieriges-Verfahren Klassifikation
####################################################################################

# Daten
forestsDataClassification <- greedy_cart_classification(create_Sample_data_class(10))
forestsDataClassification <- forestsDataClassification %>% select(nodePosition = node, nodeType = name, split_index, split_point, classes = y, value = A)

# Genutzte Funktion
eq <- function(x) {
  0.5 + 0.3*sin(2*pi*x)
}

X <- seq(0, 1, by=0.01)
plot(eq(X)~X, xlim=c(0, 1), ylim=c(0, 1), type="l", xlab="x1", ylab="x2", main = "Gieriges Verfahren Klassifikationsproblem")

p <- forestsDataClassification %>% filter(nodeType == "leaf") %>% select(classes, value)
pal <- palette(rainbow(max(p$classes)))

# Zufällige Werte plotten
for (i in 1:nrow(p)) {
  if (length(p$value[[i]]) > 1) {
    for (j in 1:length(p$value[[i]])) {
      points(p$value[[i]][[j]][1], p$value[[i]][[j]][2], col=pal[p$classes[i]])
    }
  } else {
    points(p$value[[i]][[1]][1], p$value[[i]][[1]][2], col=pal[p$classes[i]])
  }
}

#Linien plotten
minFromYAxes = -1
maxFromYAxes = 1.1
minFromXAxes = -1
maxFromXAxes = 1.1

plot.lines.recursive()
plot.lines.recursive <- function(currentNodePosition = 1, previousNodePosition = NA, prePreviousNodePosition = NA, above = F, below = F) {
  if (currentNodePosition == 2) {
    below <- currentNodePosition == 2
  }

  if (currentNodePosition == 3) {
    above <- currentNodePosition == 3
  }

  currentNode  <- forestsDataClassification %>% filter(nodePosition == currentNodePosition)
  previousNode <- forestsDataClassification %>% filter(nodePosition == previousNodePosition)
  prePreviousNode <- forestsDataClassification %>% filter(nodePosition == prePreviousNodePosition)

  if (nrow(currentNode) > 0) {
    if (!is.na(currentNode$split_index) && currentNode$split_index == 1) {
      # Befinde mich oberhalb der Mittellinie
      if (above) {
        # Kleiner als Wert (NodePosition ist eine gerade Zahl)
        if (currentNodePosition %% 2 == 0) {
          # Root abfangen
          if (!is.na(prePreviousNode$split_index) && prePreviousNode$split_index == 2) {
            segments(currentNode$split_point, prePreviousNode$split_point, currentNode$split_point, maxFromYAxes)
          } else {
            belowLimes <- forestsDataClassification %>% filter(nodePosition == 2) %>% .$split_point
            segments(currentNode$split_point, belowLimes, currentNode$split_point, maxFromYAxes)
          }
        }
      }

      # Befinde mich unterhalb der Mittellinie
      if (below) {
        # Kleiner als Wert (NodePosition ist eine gerade Zahl)
        if (currentNodePosition %% 2 == 0) {
          # Root abfangen
          if (!is.na(prePreviousNode$split_index) && prePreviousNode$split_index == 2) {
            segments(currentNode$split_point, minFromYAxes, currentNode$split_point, prePreviousNode$split_point)
          } else {
            belowLimes <- forestsDataClassification %>% filter(nodePosition == 2) %>% .$split_point
            segments(currentNode$split_point, minFromYAxes, currentNode$split_point, belowLimes)
          }
        }
      }

    }

    if (!is.na(currentNode$split_index) && currentNode$split_index == 2) {
      # Kleiner als Wert (NodePosition ist eine gerade Zahl)
      if (currentNodePosition %% 2 == 0) {
        # Root abfangen
        if (is.na(previousNode$split_point)) {
          segments(minFromXAxes, currentNode$split_point, maxFromXAxes, currentNode$split_point)
        } else {
          segments(previousNode$split_point, currentNode$split_point, prePreviousNode$split_point, currentNode$split_point)
        }
      }
    }

    # Left Child
    plot.lines.recursive(currentNodePosition * 2, currentNodePosition, previousNodePosition, above, below)
    # Right Child
    plot.lines.recursive(currentNodePosition * 2 + 1, currentNodePosition, previousNodePosition, above, below)
  }
}

####################################################################################
#                                  Plot Tree
####################################################################################

library(data.tree)

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
