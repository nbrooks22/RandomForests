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
    
    output$caption1 <- renderText({
      return("Plot")
    })
    
    output$caption2 <- renderText({
      return("Tree")
    })
    
    switch (algorithm,
      "Gieriges Verfahren - Regressionsproblem" = {
        data <- create_random_sample_data_reg(1, countElements)
        data <- RandomForestsPackage::greedy_cart_regression(data, depth, numSplit, minNum)
        
        output$plot <- renderPlot({
            printGreedyCartRegression(data)
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
      },
      "Gieriges Verfahren - Klassifikationsproblem" = {
        data <- create_Sample_data_class(1, countElements)
        data <- RandomForestsPackage::greedy_cart_classification(data, depth, numSplit, minNum)
        
        output$plot <- renderPlot({
          printGreedyCartClassification(data)
        })
        
        output$tree <- renderGrViz({
          plotTree(data)
        })
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
      
      output$caption1 <- renderText({
        return("Plot")
      })
      
      output$caption2 <- renderText({
        return("Tree")
      })
      
      switch (algorithm,
              "Gieriges Verfahren - Regressionsproblem" = {
                data <- readCSV(file, type = 0)
                data <- RandomForestsPackage::greedy_cart_regression(data, depth, numSplit, minNum)
                
                output$plot <- renderPlot({
                  printGreedyCartRegression(data)
                })
                
                output$tree <- renderGrViz({
                  plotTree(data)
                })
              },
              "Gieriges Verfahren - Klassifikationsproblem" = {
                data <- readCSV(file, type = 1)
                data <- RandomForestsPackage::greedy_cart_classification(data, depth, numSplit, minNum)
                
                output$plot <- renderPlot({
                  printGreedyCartClassification(data)
                })
                
                output$tree <- renderGrViz({
                  plotTree(data)
                })
              }
      )
    } else {
      warning("Es wurde keine Datei eingelesen!")
    }
  })
}
