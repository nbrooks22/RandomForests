source("PlotFunctions.R")
source("AdditionalFunctions.R")

server <- function(input, output, session) {
  # Button-Style
  runjs("$('#file').parent().removeClass('btn-default').addClass('btn-info');")

  
  # Beispiele
  observeEvent(input$update1, {
    algorithm <- input$algorithm1
    countElements <- input$numberOfElements
    depth <- input$depth1
    minNum <- input$minNum1
    numSplit <- input$numSplit1
    numLeaf <- input$numLeaf1
    
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
            printGreedyCartRegression(data)
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
      },
      "Gieriges Verfahren - Klassifikationsproblem" = {
        data <- create_random_sample_data_class(1, countElements)
        data <- RandomForestsPackage::greedy_cart_classification(data, numLeaf, depth, numSplit, minNum)
        
        output$plot <- renderPlot({
          printGreedyCartClassification(data)
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
      },
      "Pruning" = {
        data <- create_random_sample_data_reg(1, countElements)
        data <- RandomForestsPackage::greedy_cart_regression(data, numLeaf, depth, numSplit, minNum)
      }
    )
  })
  
  
  # Nutzerdaten
  observeEvent(input$update2, {
    if (is.null(input$file) == FALSE) {
      file <- input$file
      algorithm <- input$algorithm2
      depth <- input$depth2
      minNum <- input$minNum2
      numSplit <- input$numSplit2
      numLeaf <- input$numLeaf2
      
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
                    printGreedyCartRegression(data)
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
                    printGreedyCartRegression(data)
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
            "Pruning" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("lambdaVar1")
              
              shinyjs::hideElement("mVar1")
              shinyjs::hideElement("tVar1")
            },
            "Random Forests" = {
              output$helpTextForPruningAndRandomForests1 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests1")
              
              shinyjs::showElement("mVar1")
              shinyjs::showElement("tVar1")
              
              shinyjs::hideElement("lambdaVar1")
            },
            {
              shinyjs::hideElement("helpTextForPruningAndRandomForests1")
              shinyjs::hideElement("lambdaVar1")
              shinyjs::hideElement("mVar1")
              shinyjs::hideElement("tVar1")
            }
    )
  })
  
  # Zeige Lambda, M oder T bei Nutzerdaten
  observeEvent(input$algorithm2, {
    algorithm <- input$algorithm2
    
    switch (algorithm,
            "Pruning" = {
              output$helpTextForPruningAndRandomForests2 <- renderText({ "Nachfolgende Parameter werden für das Pruning-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("lambdaVar2")
              
              shinyjs::hideElement("mVar2")
              shinyjs::hideElement("tVar2")
            },
            "Random Forests" = {
              output$helpTextForPruningAndRandomForests2 <- renderText({ "Nachfolgende Parameter werden für das Random-Forests-Verfahren berücksichtigt:" })
              shinyjs::showElement("helpTextForPruningAndRandomForests2")
              
              shinyjs::showElement("mVar2")
              shinyjs::showElement("tVar2")
              
              shinyjs::hideElement("lambdaVar2")
            },
            {
              shinyjs::hideElement("helpTextForPruningAndRandomForests2")
              shinyjs::hideElement("lambdaVar2")
              shinyjs::hideElement("mVar2")
              shinyjs::hideElement("tVar2")
            }
    )
  })
}
