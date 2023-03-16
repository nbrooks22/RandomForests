source("PlotFunctions.R")
source("AdditionalFunctions.R")

# Für Bagging und Randomforests, um anschließend eine Prediction zu machen
listOfTrees <- list()
method <- ""

server <- function(input, output, session) {
  # Button-Style
  runjs("$('#file').parent().removeClass('btn-default').addClass('btn-info');")
  runjs("$('#fileMakePrediciton1').parent().removeClass('btn-default').addClass('btn-info');")
  
  # Beispiele
  observeEvent(input$update1, {
    # Wahl des Algorithmus
    algorithm <- input$algorithm1
    # Gieriges Verfahren
    countElements <- input$numberOfElements
    depth <- input$depth1
    minNum <- input$minNum1
    numSplit <- input$numSplit1
    numLeaf <- input$numLeaf1
    notSplitBySameClass <- input$unique1
    # Pruning
    lambda <- input$lambdaVar1
    # Random Forests
    numberOfDataFromTotal <- input$numberOfDataFromTotal1
    numberOfCoordinates <- input$numberOfCoordinates1
    # Random Forests und Bagging
    numberOfBags <- input$numberOfBags1
    
    output$caption1 <- renderText({
      return("Plot")
    })
    
    output$caption2 <- renderText({
      return("Tree")
    })
    
    switch (algorithm,
      "Gieriges Verfahren - Regressionsproblem" = {
        data <- create_random_sample_data_reg(1, countElements)
        data <- RandomForestsPackage::greedy_cart_regression(data, numLeaf, depth, numSplit, minNum)
        
        output$plot <- renderPlot({
            printRegression(data, "Gieriges Verfahren - Regressionsproblem")
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
        
        # Falls es zuvor deaktiviert wurde, wird es hier wieder aktiviert
        shinyjs::showElement("caption1")
        shinyjs::showElement("plot")
        shinyjs::showElement("caption2")
        shinyjs::showElement("tree")
      },
      "Gieriges Verfahren - Klassifikationsproblem" = {
        data <- create_random_sample_data_class(1, countElements)
        data <- RandomForestsPackage::greedy_cart_classification(data, numLeaf, depth, numSplit, minNum, unique = notSplitBySameClass)

        output$plot <- renderPlot({
          printClassification(data, "Gieriges Verfahren - Klassifikationsproblem")
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
        
        # Falls es zuvor deaktiviert wurde, wird es hier wieder aktiviert
        shinyjs::showElement("caption1")
        shinyjs::showElement("plot")
        shinyjs::showElement("caption2")
        shinyjs::showElement("tree")
      },
      "Pruning - Regressionsproblem" = {
        data <- create_random_sample_data_reg(1, countElements)
        data <- RandomForestsPackage::greedy_cart_regression(data, numLeaf, depth, numSplit, minNum)
        data$tree <- RandomForestsPackage:::pruning(data$tree, lambda, type = "reg")
        
        output$plot <- renderPlot({
          printRegression(data, "Pruning - Regressionsproblem")
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
      },
      "Pruning - Klassifikationsproblem" = {
        data <- create_random_sample_data_class(1, countElements)
        data <- RandomForestsPackage::greedy_cart_classification(data, numLeaf, depth, numSplit, minNum, unique = notSplitBySameClass)
        data$tree <- RandomForestsPackage:::pruning(data$tree, lambda, type = "class")
        
        output$plot <- renderPlot({
          printClassification(data, "Pruning - Klassifikationsproblem")
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
      },
      "Bagging - Regressionsproblem" = {
        data <- create_random_sample_data_reg(1, countElements)
        data <- RandomForestsPackage::bagging(data, numberOfBags, "reg")
        
        # Trees abspeichern
        listOfTrees <<- data$Bagged_Trees
        method <<- "reg"
        
        # Vorhersagen einlesen
        shinyjs::showElement("makePrediction1")
        shinyjs::showElement("fileMakePrediciton1")
        shinyjs::showElement("makePredictionButton1")
        
        # Plot und Tree deaktivieren
        shinyjs::hideElement("caption1")
        shinyjs::hideElement("plot")
        shinyjs::hideElement("caption2")
        shinyjs::hideElement("tree")
      },
      "Bagging - Klassifikationsproblem" = {
        data <- create_random_sample_data_class(1, countElements)
        data <- RandomForestsPackage::bagging(data, numberOfBags, "class")
        
        # Trees abspeichern
        listOfTrees <<- data$Bagged_Trees
        method <<- "class"
        
        # Vorhersagen einlesen
        shinyjs::showElement("makePrediction1")
        shinyjs::showElement("fileMakePrediciton1")
        shinyjs::showElement("makePredictionButton1")
        
        # Plot und Tree deaktivieren
        shinyjs::hideElement("caption1")
        shinyjs::hideElement("plot")
        shinyjs::hideElement("caption2")
        shinyjs::hideElement("tree")
      },
      "Random Forests - Regressionsproblem" = {
        data <- create_random_sample_data_reg(1, countElements)
        result <- RandomForestsPackage::random_forest_regression(data, numberOfBags, numberOfDataFromTotal, numberOfCoordinates, num_leaf = numLeaf, depth = depth, num_split = numSplit, min_num = minNum)
        
        resultListOfTree <- list()
        for (i in 1:length(result)) {
          resultListOfTree <- append(resultListOfTree, list(result[[i]]$tree))
        }
        
        # Trees abspeichern
        listOfTrees <<- resultListOfTree
        method <<- "reg"
        
        # Vorhersagen einlesen
        shinyjs::showElement("makePrediction1")
        shinyjs::showElement("fileMakePrediciton1")
        shinyjs::showElement("makePredictionButton1")
        
        # Plot und Tree deaktivieren
        shinyjs::hideElement("caption1")
        shinyjs::hideElement("plot")
        shinyjs::hideElement("caption2")
        shinyjs::hideElement("tree")
      },
      "Random Forests - Klassifikationsproblem" = {
        data <- create_random_sample_data_class(1, countElements)
        result <- RandomForestsPackage::random_forest_classification(data, numberOfBags, numberOfDataFromTotal, numberOfCoordinates, numLeaf, depth, numSplit, minNum, notSplitBySameClass)
        
        resultListOfTree <- list()
        for (i in 1:length(result)) {
          resultListOfTree <- append(resultListOfTree, list(result[[i]]$tree))
        }
        
        # Trees abspeichern
        listOfTrees <<- result
        method <<- "class"
        
        # Vorhersagen einlesen
        shinyjs::showElement("makePrediction1")
        shinyjs::showElement("fileMakePrediciton1")
        shinyjs::showElement("makePredictionButton1")
        
        # Plot und Tree deaktivieren
        shinyjs::hideElement("caption1")
        shinyjs::hideElement("plot")
        shinyjs::hideElement("caption2")
        shinyjs::hideElement("tree")
      }
    )
  })
  
  observeEvent(input$makePredictionButton1, {
    # Handeingabe
    handPrediction <- input$makePrediction1
    # File
    filePrediction <- input$fileMakePrediciton1
    
    if (handPrediction != "" || !is.null(filePrediction)) {
      # Händische Vorhersage
      if (handPrediction != "") {
        prediction <- as.matrix(as.double(unlist(strsplit(handPrediction, ","))), row=1)
        result <- RandomForestsPackage:::make_prediction(listOfTrees, prediction, method)
        
        output$textForPrediction1 <- renderText({ return(paste0("Die Schätzung beträgt ", result)) })
        shinyjs::show("textForPrediction1")
      }
      
      # File-Vorhersage
      if (!is.null(filePrediction)) {
        prediction <- as.matrix(read.csv(filePreditcion$datapath))
        result <- RandomForestsPackage:::make_prediction(listOfTrees, prediction, method)
        
        output$textForPrediction1 <- renderText({ return(paste0("Die Schätzung beträgt ", result)) })
        shinyjs::show("textForPrediction1")
      }
    } else {
      warning("Es wurde keine Vorhersage eingegeben!")
    }
  })
  
  # Nutzerdaten
  observeEvent(input$update2, {
    file <- input$file
    # Wahl des Algorithmus
    algorithm <- input$algorithm2
    # Gieriges Verfahren
    depth <- input$depth2
    minNum <- input$minNum2
    numSplit <- input$numSplit2
    numLeaf <- input$numLeaf2
    # Pruning
    lambda <- input$lambdaVar2
    # Random Forests
    numberOfDataFromTotal <- input$numberOfDataFromTotal2
    # Random Forests und Bagging
    numberOfBags <- input$numberOfBags2
    
    if (!is.null(file)) {
      switch (algorithm,
              "Gieriges Verfahren - Regressionsproblem" = {
                data <- readCSV(file, type = 0)
                data <- RandomForestsPackage::greedy_cart_regression(data, numLeaf, depth, numSplit, minNum)
                
                # Plot wird nur in der ersten Dimension
                if (data$dim == 1) {
                  shinyjs::showElement("caption1")
                  shinyjs::showElement("plot")
                  
                  output$caption1 <- renderText({
                    return("Plot")
                  })
                  
                  output$plot <- renderPlot({
                    printRegression(data, "Gieriges Verfahren - Regressionsproblem")
                  })
                } else {
                  shinyjs::hideElement("caption1")
                  shinyjs::hideElement("plot")
                }
                
                output$caption2 <- renderText({
                  return("Tree")
                })
                
                output$tree <- renderGrViz({
                  plotTree(data)
                })
              },
              "Gieriges Verfahren - Klassifikationsproblem" = {
                data <- readCSV(file, type = 1)
                data <- RandomForestsPackage::greedy_cart_classification(data, numLeaf, depth, numSplit, minNum)
                
                # Plot wird nur in der zweiten Dimension
                if (data$dim == 2) {
                  shinyjs::showElement("caption1")
                  shinyjs::showElement("plot")
                  
                  output$caption1 <- renderText({
                    return("Plot")
                  })
                  
                  output$plot <- renderPlot({
                    printClassification(data, "Gieriges Verfahren - Klassifikationsproblem")
                  })
                } else {
                  shinyjs::hideElement("caption1")
                  shinyjs::hideElement("plot")
                }
                
                output$tree <- renderGrViz({
                  plotTree(data)
                })
              }
      )
    } else {
      warning("Es wurde keine Datei eingelesen!")
    }
  })
  
  observeEvent(input$makePredictionButton2, {
    # Handeingabe
    handPrediction <- input$makePrediction2
    # File
    filePrediction <- input$fileMakePrediciton2
    
    if (handPrediction != "" || !is.null(filePrediction)) {
      # Händische Vorhersage
      if (handPrediction != "") {
        prediction <- as.matrix(as.double(unlist(strsplit(handPrediction, ","))), row=1)
        result <- RandomForestsPackage:::make_prediction(listOfTrees, prediction, method)
        
        output$textForPrediction2 <- renderText({ return(paste0("Die Schätzung beträgt ", result)) })
        shinyjs::show("textForPrediction2")
      }
      
      # File-Vorhersage
      if (!is.null(filePrediction)) {
        prediction <- as.matrix(read.csv(filePreditcion$datapath))
        result <- RandomForestsPackage:::make_prediction(listOfTrees, prediction, method)
        
        output$textForPrediction2 <- renderText({ return(paste0("Die Schätzung beträgt ", result)) })
        shinyjs::show("textForPrediction2")
      }
    } else {
      warning("Es wurde keine Vorhersage eingegeben!")
    }
  })

  # Zeige Lambda, M ect. bei den Beispielen
  observeEvent(input$algorithm1, {
    algorithm1 <- input$algorithm1
    
    switch (algorithm1,
            "Gieriges Verfahren - Klassifikationsproblem" = {
              shinyjs::showElement("unique1")
              
              shinyjs::hideElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::hideElement("lambdaVar1")
              
              shinyjs::hideElement("numberOfBags1")
              shinyjs::hideElement("numberOfDataFromTotal1")
              shinyjs::hideElement("numberOfCoordinates1")
              
              shinyjs::hideElement("makePrediction1")
              shinyjs::hideElement("fileMakePrediciton1")
              shinyjs::hideElement("makePredictionButton1")
            },
            "Pruning - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("lambdaVar1")
              
              shinyjs::hideElement("unique1")
              
              shinyjs::hideElement("numberOfBags1")
              shinyjs::hideElement("numberOfDataFromTotal1")
              shinyjs::hideElement("numberOfCoordinates1")
              
              shinyjs::hideElement("makePrediction1")
              shinyjs::hideElement("fileMakePrediciton1")
              shinyjs::hideElement("makePredictionButton1")
            },
            "Pruning - Klassifikationsproblem" = {
              shinyjs::showElement("unique1")
              
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("lambdaVar1")
              
              shinyjs::hideElement("numberOfBags1")
              shinyjs::hideElement("numberOfDataFromTotal1")
              shinyjs::hideElement("numberOfCoordinates1")
              
              shinyjs::hideElement("makePrediction1")
              shinyjs::hideElement("fileMakePrediciton1")
              shinyjs::hideElement("makePredictionButton1")
            },
            "Bagging - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Bagging-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("numberOfBags1")
              
              shinyjs::hideElement("unique1")
              
              shinyjs::hideElement("lambdaVar1")
              shinyjs::hideElement("numberOfDataFromTotal1")
              shinyjs::hideElement("numberOfCoordinates1")
              
              shinyjs::hideElement("makePrediction1")
              shinyjs::hideElement("fileMakePrediciton1")
              shinyjs::hideElement("makePredictionButton1")
            },
            "Bagging - Klassifikationsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Bagging-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("numberOfBags1")
              
              shinyjs::hideElement("unique1")
              
              shinyjs::hideElement("lambdaVar1")
              shinyjs::hideElement("numberOfDataFromTotal1")
              shinyjs::hideElement("numberOfCoordinates1")
              
              shinyjs::hideElement("makePrediction1")
              shinyjs::hideElement("fileMakePrediciton1")
              shinyjs::hideElement("makePredictionButton1")
            },
            "Random Forests - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("numberOfBags1")
              shinyjs::showElement("numberOfDataFromTotal1")
              shinyjs::showElement("numberOfCoordinates1")
              
              shinyjs::hideElement("unique1")
              
              shinyjs::hideElement("lambdaVar1")
              
              shinyjs::hideElement("makePrediction1")
              shinyjs::hideElement("fileMakePrediciton1")
              shinyjs::hideElement("makePredictionButton1")
            },
            "Random Forests - Klassifikationsproblem" = {
              shinyjs::showElement("unique1")
              
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("numberOfBags1")
              shinyjs::showElement("numberOfDataFromTotal1")
              shinyjs::showElement("numberOfCoordinates1")
              
              shinyjs::hideElement("lambdaVar1")
              
              shinyjs::hideElement("makePrediction1")
              shinyjs::hideElement("fileMakePrediciton1")
              shinyjs::hideElement("makePredictionButton1")
            },
            {
              shinyjs::hideElement("helpTextForPruningAndRandomForests1")
              shinyjs::hideElement("unique1")
              shinyjs::hideElement("lambdaVar1")
              shinyjs::hideElement("numberOfBags1")
              shinyjs::hideElement("numberOfDataFromTotal1")
              shinyjs::hideElement("numberOfCoordinates1")
              shinyjs::hideElement("makePrediction1")
              shinyjs::hideElement("fileMakePrediciton1")
              shinyjs::hideElement("makePredictionButton1")
            }
    )
  })
  
  # Zeige Lambda, M ect. bei Nutzerdaten
  observeEvent(input$algorithm2, {
    algorithm2 <- input$algorithm2
    switch (algorithm2,
            "Gieriges Verfahren - Klassifikationsproblem" = {
              shinyjs::showElement("unique2")
              
              shinyjs::hideElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::hideElement("lambdaVar2")
              
              shinyjs::hideElement("numberOfBags2")
              shinyjs::hideElement("numberOfDataFromTotal2")
              shinyjs::hideElement("numberOfCoordinates2")
              
              shinyjs::hideElement("makePrediction2")
              shinyjs::hideElement("fileMakePrediciton2")
              shinyjs::hideElement("makePredictionButton2")
            },
            "Pruning - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("lambdaVar2")
              
              shinyjs::hideElement("unique2")
              
              shinyjs::hideElement("numberOfBags2")
              shinyjs::hideElement("numberOfDataFromTotal2")
              shinyjs::hideElement("numberOfCoordinates2")
              
              shinyjs::hideElement("makePrediction2")
              shinyjs::hideElement("fileMakePrediciton2")
              shinyjs::hideElement("makePredictionButton2")
            },
            "Pruning - Klassifikationsproblem" = {
              shinyjs::showElement("unique2")
              
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("lambdaVar2")
              
              shinyjs::hideElement("numberOfBags2")
              shinyjs::hideElement("numberOfDataFromTotal2")
              shinyjs::hideElement("numberOfCoordinates2")
              
              shinyjs::hideElement("makePrediction2")
              shinyjs::hideElement("fileMakePrediciton2")
              shinyjs::hideElement("makePredictionButton2")
            },
            "Bagging - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Bagging-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("numberOfBags2")
              
              shinyjs::hideElement("unique2")
              
              shinyjs::hideElement("lambdaVar2")
              shinyjs::hideElement("numberOfDataFromTotal2")
              shinyjs::hideElement("numberOfCoordinates2")
              
              shinyjs::hideElement("makePrediction2")
              shinyjs::hideElement("fileMakePrediciton2")
              shinyjs::hideElement("makePredictionButton2")
            },
            "Bagging - Klassifikationsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Bagging-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("numberOfBags2")
              
              shinyjs::hideElement("unique2")
              
              shinyjs::hideElement("lambdaVar2")
              shinyjs::hideElement("numberOfDataFromTotal2")
              shinyjs::hideElement("numberOfCoordinates2")
              
              shinyjs::hideElement("makePrediction2")
              shinyjs::hideElement("fileMakePrediciton2")
              shinyjs::hideElement("makePredictionButton2")
            },
            "Random Forests - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("numberOfBags2")
              shinyjs::showElement("numberOfDataFromTotal2")
              shinyjs::showElement("numberOfCoordinates2")
              
              shinyjs::hideElement("unique2")
              
              shinyjs::hideElement("lambdaVar2")
              
              shinyjs::hideElement("makePrediction2")
              shinyjs::hideElement("fileMakePrediciton2")
              shinyjs::hideElement("makePredictionButton2")
            },
            "Random Forests - Klassifikationsproblem" = {
              shinyjs::showElement("unique2")
              
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("numberOfBags2")
              shinyjs::showElement("numberOfDataFromTotal2")
              shinyjs::showElement("numberOfCoordinates2")
              
              shinyjs::hideElement("lambdaVar2")
              
              shinyjs::hideElement("makePrediction2")
              shinyjs::hideElement("fileMakePrediciton2")
              shinyjs::hideElement("makePredictionButton2")
            },
            {
              shinyjs::hideElement("helpTextForPruningAndRandomForests2")
              shinyjs::hideElement("unique2")
              shinyjs::hideElement("lambdaVar2")
              shinyjs::hideElement("numberOfBags2")
              shinyjs::hideElement("numberOfDataFromTotal2")
              shinyjs::hideElement("numberOfCoordinates2")
              shinyjs::hideElement("makePrediction2")
              shinyjs::hideElement("fileMakePrediciton2")
              shinyjs::hideElement("makePredictionButton2")
            }
    )
  })
}
