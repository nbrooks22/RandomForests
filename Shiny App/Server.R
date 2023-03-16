source("PlotFunctions.R")
source("AdditionalFunctions.R")

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
    # Pruning
    lambda <- input$lambdaVar1
    # Random Forests
    numberOfDataFromTotal <- input$numberOfDataFromTotal1
    # Random Forests und Bagging
    numberOfBags <- input$numberOfBags1
    
    # Falls es zuvor deaktiviert wurde, wird es hier wieder aktiviert
    shinyjs::showElement("caption1")
    shinyjs::showElement("plot")
    
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
      },
      "Gieriges Verfahren - Klassifikationsproblem" = {
        data <- create_random_sample_data_class(1, countElements)
        data <- RandomForestsPackage::greedy_cart_classification(data, numLeaf, depth, numSplit, minNum)
        
        output$plot <- renderPlot({
          printClassification(data, "Gieriges Verfahren - Klassifikationsproblem")
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
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
        data <- RandomForestsPackage::greedy_cart_classification(data, numLeaf, depth, numSplit, minNum)
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
        data <- RandomForestsPackage::bagging_regression(data, numberOfBags)
        
        # Vorhersagen einlesen
        shinyjs::showElement("makePrediction1")
        shinyjs::showElement("fileMakePrediciton1")
        shinyjs::showElement("makePredictionButton1")
      },
      "Bagging - Klassifikationsproblem" = {
        data <- create_random_sample_data_class(1, countElements)
        data <- RandomForestsPackage::bagging_classification(data, numberOfBags)
        
        # Vorhersagen einlesen
        shinyjs::showElement("makePrediction1")
        shinyjs::showElement("fileMakePrediciton1")
      },
      "Random Forests - Regressionsproblem" = {
        data <- create_random_sample_data_reg(1, countElements)
        data <- RandomForestsPackage::random_forest_regression(data, numberOfBags, numberOfDataFromTotal, num_leaf = numLeaf, depth = depth, num_split = numSplit, min_num = minNum)
      },
      "Random Forests - Klassifikationsproblem" = {
        data <- create_random_sample_data_class(1, countElements)
        data <- RandomForestsPackage::random_forest_classification(data, numberOfBags, numberOfDataFromTotal, num_leaf = numLeaf, depth = depth, num_split = numSplit, min_num = minNum)
      }
    )
  })
  
  
  # Nutzerdaten
  observeEvent(input$update2, {
    if (is.null(input$file) == FALSE) {
      file <- input$file
      # Wahl des Algorithmus
      algorithm <- input$algorithm2
      # Gieriges Verfahren
      countElements <- input$numberOfElements
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
  
  
  # Zeige Lambda, M oder T bei den Beispielen
  observeEvent(input$algorithm1, {
    algorithm <- input$algorithm1
    
    switch (algorithm,
            "Pruning - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("lambdaVar1")
              
              shinyjs::hideElement("numberOfBags1")
              shinyjs::hideElement("numberOfDataFromTotal1")
            },
            "Pruning - Klassifikationsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("lambdaVar1")
              
              shinyjs::hideElement("numberOfBags1")
              shinyjs::hideElement("numberOfDataFromTotal1")
            },
            "Bagging - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Bagging-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("numberOfBags1")
              
              shinyjs::hideElement("lambdaVar1")
              shinyjs::hideElement("numberOfDataFromTotal1")
            },
            "Bagging - Klassifikationsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Bagging-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("numberOfBags1")
              
              shinyjs::hideElement("lambdaVar1")
              shinyjs::hideElement("numberOfDataFromTotal1")
            },
            "Random Forests - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("numberOfBags1")
              shinyjs::showElement("numberOfDataFromTotal1")
              
              shinyjs::hideElement("lambdaVar1")
            },
            "Random Forests - Klassifikationsproblem" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("numberOfBags1")
              shinyjs::showElement("numberOfDataFromTotal1")
              
              shinyjs::hideElement("lambdaVar1")
            },
            {
              shinyjs::hideElement("helpTextForPruningAndRandomForests1")
              shinyjs::hideElement("lambdaVar1")
              shinyjs::hideElement("numberOfBags1")
              shinyjs::hideElement("numberOfDataFromTotal1")
            }
    )
  })
  
  # Zeige Lambda, M oder T bei Nutzerdaten
  observeEvent(input$algorithm2, {
    algorithm <- input$algorithm2
    
    switch (algorithm,
            "Pruning - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests2 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("lambdaVar2")
              
              shinyjs::hideElement("numberOfBags2")
              shinyjs::hideElement("numberOfDataFromTotal2")
            },
            "Pruning - Klassifikationsproblem" = {
              output$helpTextForPruningAndRandomForests2 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("lambdaVar2")
              
              shinyjs::hideElement("numberOfBags2")
              shinyjs::hideElement("numberOfDataFromTotal2")
            },
            "Bagging - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests2 <- renderText({ "Nachfolgende Parameter werden für das Bagging-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("numberOfBags2")
              
              shinyjs::hideElement("lambdaVar2")
              shinyjs::hideElement("numberOfDataFromTotal2")
            },
            "Bagging - Klassifikationsproblem" = {
              output$helpTextForPruningAndRandomForests2 <- renderText({ "Nachfolgende Parameter werden für das Bagging-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("numberOfBags2")
              
              shinyjs::hideElement("lambdaVar2")
              shinyjs::hideElement("numberOfDataFromTotal2")
            },
            "Random Forests - Regressionsproblem" = {
              output$helpTextForPruningAndRandomForests2 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("numberOfBags2")
              shinyjs::showElement("numberOfDataFromTotal2")
              
              shinyjs::hideElement("lambdaVar2")
            },
            "Random Forests - Klassifikationsproblem" = {
              output$helpTextForPruningAndRandomForests2 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("numberOfBags2")
              shinyjs::showElement("numberOfDataFromTotal2")
              
              shinyjs::hideElement("lambdaVar2")
            },
            {
              shinyjs::hideElement("helpTextForPruningAndRandomForests2")
              shinyjs::hideElement("lambdaVar2")
              shinyjs::hideElement("numberOfBags2")
              shinyjs::hideElement("numberOfDataFromTotal2")
            }
    )
  })
}
