ui <- fluidPage(
  # Black-Theme
  theme = bs_theme(version = 4, bootswatch = "darkly"),
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
                    
                    tags$div(title = "Die eingegebene Zahl bestimmt die Tiefe des Baumes. 0 entspricht, dass er die maximale Größe besitzen wird.",
                             numericInput("depth1",
                                          "Maximale Tiefe des Baumes",
                                          value = 0,
                                          min = 0)
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
                    
                    tags$div(title="Gewichtung der Blätter.",
                             hidden(
                               textOutput("helpTextForPruningAndRandomForests1"),
                               numericInput("lambdaVar1",
                                            "Lambda",
                                            value = 0,
                                            min = 0)
                             )
                    ),
                    
                    tags$div(title="Anzahl der Taschen.",
                             hidden(
                               numericInput("numberOfBags1",
                                            "Anzahl der Taschen",
                                            value = 0)
                             )
                    ),
                    
                    tags$div(title="Anzahl der Daten, die aus der Gesamtanzahl gezogen wird.",
                             hidden(
                               numericInput("numberOfDataFromTotal1",
                                            "Anzahl der Daten",
                                            value = 0)
                             )
                    ),
                    
                    fluidRow(column(12, align = "center",
                                    actionButton("update1", "Update", icon("rotate"),
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
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
                    
                    tags$div(title = "Die eingegebene Zahl bestimmt die Tiefe des Baumes. 0 entspricht, dass er die maximale Größe besitzen wird.",
                             numericInput("depth2",
                                          "Maximale Tiefe des Baumes",
                                          value = 0,
                                          min = 0)
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
                    
                    tags$div(title="Gewichtung der Blätter.",
                             hidden(
                               textOutput("helpTextForPruningAndRandomForests2"),
                               numericInput("lambdaVar2",
                                            "Lambda",
                                            value = 0,
                                            min = 0)
                             )
                    ),
                    
                    tags$div(title="Anzahl der Taschen.",
                             hidden(
                               numericInput("numberOfBags2",
                                            "Anzahl der Taschen",
                                            value = 0)
                             )
                    ),
                    
                    tags$div(title="Anzahl der Daten, die aus der Gesamtanzahl gezogen wird.",
                             hidden(
                               numericInput("numberOfDataFromTotal2",
                                            "Anzahl der Daten",
                                            value = 0)
                             )
                    ),
                    
                    fluidRow(column(12, align = "center",
                                    actionButton("update2", "Update", icon("rotate"),
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                             width = 100
                             ),
                    
                    width = 100
                    )
                  )
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
