#' Shiny App
#' @import shiny
#' @import shinyjs
#' @import bslib
#' @import DiagrammeR
#' @import data.tree
#' @export

App <- function() {
  # Ui
  ui <- fluidPage(
    # Black-Theme
    theme = bslib::bs_theme(version = 4, bootswatch = "darkly"),
    useShinyjs(),

    # App-Titel
    fluidRow(column(12, align = "center", titlePanel("Random Forests"))),

    # Sidebar_Panels...
    fluidRow(column(width=6,
                    # ...für Beispiele
                    sidebarPanel(
                      selectInput("algorithm1",
                                  label = "Wähle ein Beispiel",
                                  choices = list("Gieriges Verfahren - Regressionsproblem",
                                                 "Gieriges Verfahren - Klassifikationsproblem",
                                                 "Pruning - Regressionsproblem",
                                                 "Pruning - Klassifikationsproblem",
                                                 "Bagging - Regressionsproblem",
                                                 "Bagging - Klassifikationsproblem",
                                                 "Random Forests - Regressionsproblem",
                                                 "Random Forests - Klassifikationsproblem"),
                                  selected = 1),

                      tags$div(title = "Bestimmt die Anzahl der Trainingsdaten zufällig.",
                               sliderInput("numberOfElements",
                                           label = "Anzahl der zufälligen Daten",
                                           min = 1, max = 500, value = 50)
                      ),

                      tags$div(title = "Die eingegebene Zahl bestimmt die Tiefe des Baumes. -1 entspricht, dass er die maximale Größe besitzen wird.",
                               numericInput("depth1",
                                            "Maximale Tiefe des Baumes",
                                            value = -1,
                                            min = -1)
                      ),

                      tags$div(title = "Minimale Anzahl an Trainingsdaten die in einem Blatt sein sollen, damit noch gesplittet wird. Bei n wird noch gesplittet, bei n - 1 nicht mehr.",
                               numericInput("numSplit1",
                                            "Minimale Anzahl an Daten der Blätter",
                                            value = 2,
                                            min = 2)
                      ),

                      tags$div(title="Splitte nur, wenn die darauffolgenden gesplitteten Blätter die eingestellte Anzahl haben.",
                               numericInput("minNum1",
                                            "Mindestgröße der Blätter",
                                            value = 1,
                                            min = 1)
                      ),

                      tags$div(title="Beende das Verfahren, sobald n Blätter berechnet wurden. 0 bedeutet, dass die maximal mögliche Anzahl berechnet wird.",
                               numericInput("numLeaf1",
                                            "Maximale Anzahl an Blätter",
                                            value = 0,
                                            min = 0)
                      ),

                      tags$div(title="Wenn alle Datenpunkte einer Node die selbe Klasse angehören, wir kein Split mehr vorgenommen.",
                               hidden(
                                 checkboxInput("unique1", "Keine Splittung durchführen, wenn Klasse identisch", value = TRUE)
                               )
                      ),

                      hidden(
                        textOutput("helpTextForPruningAndRandomForests1"),
                        numericInput("lambdaVar1",
                                     "Lambda",
                                     value = 0,
                                     min = 0)
                      ),

                      tags$div(title="Anzahl der Taschen.",
                               hidden(
                                 numericInput("numberOfBags1",
                                              "Anzahl der Taschen",
                                              value = 1,
                                              min = 1)
                               )
                      ),

                      tags$div(title="Anzahl der Daten, die aus der Gesamtanzahl gezogen wird.",
                               hidden(
                                 numericInput("numberOfDataFromTotal1",
                                              "Anzahl der Daten",
                                              value = 0)
                               )
                      ),

                      tags$div(title="Anzahl der Koordinaten.",
                               hidden(
                                 numericInput("numberOfCoordinates1",
                                              "Anzahl der Koordinaten",
                                              value = 0)
                               )
                      ),

                      fluidRow(column(12, align = "center",
                                      actionButton("update1", "Update", icon("rotate"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                      ),

                      tags$div(title="Ein n-dimensionaler Vektor. Beispiel bei Klassifikation: 1,2",
                               hidden(
                                 textInput("makePrediction1",
                                           "Schätzung")
                               )
                      ),

                      tags$div(title="Die CSV-Datei besitzt m Spalten mit n Zeilen, wobei m die Anzahl der Vorhersagen sind und n die Dimension.",
                               hidden(
                                 fileInput("fileMakePrediciton1", "CSV-Datei einlesen", accept = ".csv")
                               )
                      ),

                      fluidRow(
                        column(12, align="center",
                               hidden(
                                 actionButton("makePredictionButton1", "Berechne Schätzung", icon("rotate"),
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 textOutput("textForPrediction1")
                               )
                        )
                      ),


                      width = 100,
                    )),


             column(width=6,
                    # ...für Nutzereingaben

                    sidebarPanel(
                      tags$div(title="Regression: Die CSV Datei besitzt m Spalten mit n x-Werte. Die letzte Spalte besitzt n y-Werte, wobei n eine natürliche Zahl ist.\n\nKlassifikation: Die CSV Dateibesitzt m >= 2 Spalten mit n x-Werte und in der letzten Spalte n y-Werte, wobei n eine natürliche Zahl ist und y die zugehörige Klasse.",
                               fileInput("file", "CSV-Datei einlesen", accept = ".csv"),
                      ),

                      selectInput("algorithm2",
                                  label = "Wähle ein Algorithmus",
                                  choices = list("Gieriges Verfahren - Regressionsproblem",
                                                 "Gieriges Verfahren - Klassifikationsproblem",
                                                 "Pruning - Regressionsproblem",
                                                 "Pruning - Klassifikationsproblem",
                                                 "Bagging - Regressionsproblem",
                                                 "Bagging - Klassifikationsproblem",
                                                 "Random Forests - Regressionsproblem",
                                                 "Random Forests - Klassifikationsproblem"),
                                  selected = 1),

                      tags$div(title = "Die eingegebene Zahl bestimmt die Tiefe des Baumes. -1 entspricht, dass er die maximale Größe besitzen wird.",
                               numericInput("depth2",
                                            "Maximale Tiefe des Baumes",
                                            value = -1,
                                            min = -1)
                      ),

                      tags$div(title = "Minimale Anzahl an Trainingsdaten die in einem Blatt sein sollen, damit noch gesplittet wird. Bei n wird noch gesplittet, bei n - 1 nicht mehr.",
                               numericInput("numSplit2",
                                            "Minimale Anzahl an Daten der Blätter",
                                            value = 2,
                                            min = 2)
                      ),

                      tags$div(title="Splitte nur, wenn die darauffolgenden gesplitteten Blätter die eingestellte Anzahl haben.",
                               numericInput("minNum2",
                                            "Mindestgröße der Blätter",
                                            value = 1,
                                            min = 1)
                      ),

                      tags$div(title="Beende das Verfahren, sobald n Blätter berechnet wurden. 0 bedeutet, dass die maximal mögliche Anzahl berechnet wird.",
                               numericInput("numLeaf2",
                                            "Maximale Anzahl an Blätter",
                                            value = 0,
                                            min = 0)
                      ),

                      tags$div(title="Wenn alle Datenpunkte einer Node die selbe Klasse angehören, wir kein Split mehr vorgenommen.",
                               hidden(
                                 checkboxInput("unique2", "Keine Splittung durchführen, wenn Klasse identisch", value = TRUE)
                               )
                      ),

                      hidden(
                        textOutput("helpTextForPruningAndRandomForests2"),
                        numericInput("lambdaVar2",
                                     "Lambda",
                                     value = 0,
                                     min = 0)
                      ),

                      tags$div(title="Anzahl der Taschen.",
                               hidden(
                                 numericInput("numberOfBags2",
                                              "Anzahl der Taschen",
                                              value = 1,
                                              min = 1)
                               )
                      ),

                      tags$div(title="Anzahl der Daten, die aus der Gesamtanzahl gezogen wird.",
                               hidden(
                                 numericInput("numberOfDataFromTotal2",
                                              "Anzahl der Daten",
                                              value = 0)
                               )
                      ),

                      tags$div(title="Anzahl der Koordinaten.",
                               hidden(
                                 numericInput("numberOfCoordinates2",
                                              "Anzahl der Koordinaten",
                                              value = 0)
                               )
                      ),

                      fluidRow(column(12, align = "center",
                                      actionButton("update2", "Update", icon("rotate"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                      ),

                      tags$div(title="Ein n-dimensionaler Vektor. Beispiel bei Klassifikation: 1,2",
                               hidden(
                                 textInput("makePrediction2",
                                           "Schätzung")
                               )
                      ),

                      tags$div(title="Die CSV-Datei besitzt m Spalten mit n Zeilen, wobei m die Anzahl der Vorhersagen sind und n die Dimension.",
                               hidden(
                                 fileInput("fileMakePrediciton2", "CSV-Datei einlesen", accept = ".csv")
                               )
                      ),

                      fluidRow(
                        column(12, align="center",
                               hidden(
                                 actionButton("makePredictionButton1", "Berechne Schätzung", icon("rotate"),
                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                 textOutput("textForPrediction2")
                               )
                        )
                      ),

                      width = 100,
                    ))
    ),

    # Main-Panel
    mainPanel(
      fluidRow(column(12, align = "center", h2(textOutput("caption1")))),
      fluidRow(
        column(12, align="center",
               plotOutput("plot", width = "95%", height = "850px")
        )
      ),
      fluidRow(column(12, align = "center", h2(textOutput("caption2")))),
      fluidRow(
        column(12, align="center",
               grVizOutput("tree", width = "95%", height = "100%")
        )
      ),
      width = 100
    )
  )

  # Für Bagging und Randomforests, um anschließend eine Prediction zu machen
  listOfTrees <- list()
  method <- ""

  # Server
  server <- function(input, output, session) {
    # Button-Style
    shinyjs::runjs("$('#file').parent().removeClass('btn-default').addClass('btn-info');")
    shinyjs::runjs("$('#fileMakePrediciton1').parent().removeClass('btn-default').addClass('btn-info');")

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

                # Falls es zuvor deaktiviert wurde, wird es hier wieder aktiviert
                shinyjs::showElement("caption1")
                shinyjs::showElement("plot")
                shinyjs::showElement("caption2")
                shinyjs::showElement("tree")
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

                # Falls es zuvor deaktiviert wurde, wird es hier wieder aktiviert
                shinyjs::showElement("caption1")
                shinyjs::showElement("plot")
                shinyjs::showElement("caption2")
                shinyjs::showElement("tree")
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
                data <- RandomForestsPackage::random_forest_regression(data, numberOfBags, numberOfDataFromTotal, numberOfCoordinates, num_leaf = numLeaf, depth = depth, num_split = numSplit, min_num = minNum)

                # Trees abspeichern
                listOfTrees <<- data
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
                data <- RandomForestsPackage::random_forest_classification(data, numberOfBags, numberOfDataFromTotal, numberOfCoordinates, numLeaf, depth, numSplit, minNum, notSplitBySameClass)

                # Trees abspeichern
                listOfTrees <<- data
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
      notSplitBySameClass <- input$unique2
      # Pruning
      lambda <- input$lambdaVar2
      # Random Forests
      numberOfDataFromTotal <- input$numberOfDataFromTotal2
      numberOfCoordinates <- input$numberOfCoordinates2
      # Random Forests und Bagging
      numberOfBags <- input$numberOfBags2

      output$caption1 <- renderText({
        return("Plot")
      })

      output$caption2 <- renderText({
        return("Tree")
      })

      if (!is.null(file)) {
        switch (algorithm,
                "Gieriges Verfahren - Regressionsproblem" = {
                  data <- readCSV(file, type = 0)
                  data <- RandomForestsPackage::greedy_cart_regression(data, numLeaf, depth, numSplit, minNum)

                  # Plot wird nur in der ersten Dimension
                  if (data$dim == 1) {
                    output$caption1 <- renderText({
                      return("Plot")
                    })

                    output$plot <- renderPlot({
                      printRegression(data, "Gieriges Verfahren - Regressionsproblem")
                    })

                    shinyjs::showElement("caption1")
                    shinyjs::showElement("plot")
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

                  shinyjs::showElement("caption2")
                  shinyjs::showElement("tree")
                },
                "Gieriges Verfahren - Klassifikationsproblem" = {
                  data <- readCSV(file, type = 1)
                  data <- RandomForestsPackage::greedy_cart_classification(data, numLeaf, depth, numSplit, minNum, unique = notSplitBySameClass)

                  # Plot wird nur in der zweiten Dimension
                  if (data$dim == 2) {
                    output$caption1 <- renderText({
                      return("Plot")
                    })

                    output$plot <- renderPlot({
                      printClassification(data, "Gieriges Verfahren - Klassifikationsproblem")
                    })
                    shinyjs::showElement("caption1")
                    shinyjs::showElement("plot")
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

                  shinyjs::showElement("caption2")
                  shinyjs::showElement("tree")
                },
                "Pruning - Regressionsproblem" = {
                  data <- readCSV(file, type = 0)
                  data <- RandomForestsPackage::greedy_cart_regression(data, numLeaf, depth, numSplit, minNum)
                  data$tree <- RandomForestsPackage:::pruning(data$tree, lambda, type = "reg")

                  # Plot wird nur in der ersten Dimension
                  if (data$dim == 1) {
                    output$caption1 <- renderText({
                      return("Plot")
                    })

                    output$plot <- renderPlot({
                      printRegression(data, "Gieriges Verfahren - Regressionsproblem")
                    })

                    shinyjs::showElement("caption1")
                    shinyjs::showElement("plot")
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

                  shinyjs::showElement("caption2")
                  shinyjs::showElement("tree")
                },
                "Pruning - Klassifikationsproblem" = {
                  data <- readCSV(file, type = 1)
                  data <- RandomForestsPackage::greedy_cart_classification(data, numLeaf, depth, numSplit, minNum, unique = notSplitBySameClass)
                  data$tree <- RandomForestsPackage:::pruning(data$tree, lambda, type = "class")

                  # Plot wird nur in der zweiten Dimension
                  if (data$dim == 2) {
                    output$caption1 <- renderText({
                      return("Plot")
                    })

                    output$plot <- renderPlot({
                      printRegression(data, "Gieriges Verfahren - Regressionsproblem")
                    })

                    shinyjs::showElement("caption1")
                    shinyjs::showElement("plot")
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

                  shinyjs::showElement("caption2")
                  shinyjs::showElement("tree")
                },
                "Bagging - Regressionsproblem" = {
                  data <- readCSV(file, type = 0)
                  data <- RandomForestsPackage::bagging(data, numberOfBags, "reg")

                  # Trees abspeichern
                  listOfTrees <<- data$Bagged_Trees
                  method <<- "reg"

                  # Vorhersagen einlesen
                  shinyjs::showElement("makePrediction2")
                  shinyjs::showElement("fileMakePrediciton2")
                  shinyjs::showElement("makePredictionButton2")

                  # Plot und Tree deaktivieren
                  shinyjs::hideElement("caption1")
                  shinyjs::hideElement("plot")
                  shinyjs::hideElement("caption2")
                  shinyjs::hideElement("tree")
                },
                "Bagging - Klassifikationsproblem" = {
                  data <- readCSV(file, type = 1)
                  data <- RandomForestsPackage::bagging(data, numberOfBags, "class")

                  # Trees abspeichern
                  listOfTrees <<- data$Bagged_Trees
                  method <<- "class"

                  # Vorhersagen einlesen
                  shinyjs::showElement("makePrediction2")
                  shinyjs::showElement("fileMakePrediciton2")
                  shinyjs::showElement("makePredictionButton2")

                  # Plot und Tree deaktivieren
                  shinyjs::hideElement("caption1")
                  shinyjs::hideElement("plot")
                  shinyjs::hideElement("caption2")
                  shinyjs::hideElement("tree")
                },
                "Random Forests - Regressionsproblem" = {
                  data <- data <- readCSV(file, type = 0)
                  data <- RandomForestsPackage::random_forest_regression(data, numberOfBags, numberOfDataFromTotal, numberOfCoordinates, num_leaf = numLeaf, depth = depth, num_split = numSplit, min_num = minNum)

                  # Trees abspeichern
                  listOfTrees <<- data
                  method <<- "reg"

                  # Vorhersagen einlesen
                  shinyjs::showElement("makePrediction2")
                  shinyjs::showElement("fileMakePrediciton2")
                  shinyjs::showElement("makePredictionButton2")

                  # Plot und Tree deaktivieren
                  shinyjs::hideElement("caption1")
                  shinyjs::hideElement("plot")
                  shinyjs::hideElement("caption2")
                  shinyjs::hideElement("tree")
                },
                "Random Forests - Klassifikationsproblem" = {
                  data <- readCSV(file, type = 1)
                  data <- RandomForestsPackage::random_forest_classification(data, numberOfBags, numberOfDataFromTotal, numberOfCoordinates, numLeaf, depth, numSplit, minNum, notSplitBySameClass)

                  # Trees abspeichern
                  listOfTrees <<- data
                  method <<- "class"

                  # Vorhersagen einlesen
                  shinyjs::showElement("makePrediction2")
                  shinyjs::showElement("fileMakePrediciton2")
                  shinyjs::showElement("makePredictionButton2")

                  # Plot und Tree deaktivieren
                  shinyjs::hideElement("caption1")
                  shinyjs::hideElement("plot")
                  shinyjs::hideElement("caption2")
                  shinyjs::hideElement("tree")
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
                shinyjs::hideElement("textForPrediction1")
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
                shinyjs::hideElement("textForPrediction1")
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
                shinyjs::hideElement("textForPrediction1")
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
                shinyjs::hideElement("textForPrediction1")
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
                shinyjs::hideElement("textForPrediction1")
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
                shinyjs::hideElement("textForPrediction1")
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
                shinyjs::hideElement("textForPrediction1")
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
                shinyjs::hideElement("textForPrediction1")
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
                shinyjs::hideElement("textForPrediction2")
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
                shinyjs::hideElement("textForPrediction2")
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
                shinyjs::hideElement("textForPrediction2")
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
                shinyjs::hideElement("textForPrediction2")
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
                shinyjs::hideElement("textForPrediction2")
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
                shinyjs::hideElement("textForPrediction2")
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
                shinyjs::hideElement("textForPrediction2")
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
                shinyjs::hideElement("textForPrediction2")
              }
      )
    })
  }


  shinyApp(ui, server,
           onStart = function() {
             onStop(function() {
               rm("server", "ui",
                  "printRegression", "printClassification", "plotLinesColor", "plotTree", "moveSplit",
                  "readCSV", "create_random_sample_data_reg", "create_random_sample_data_class",
                  "listOfTrees", "method",
                  envir = .GlobalEnv)
             })
           })
}
