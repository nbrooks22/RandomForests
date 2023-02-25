ui <- fluidPage(
  # black theme
  theme = bs_theme(version = 4, bootswatch = "darkly"),
  useShinyjs(),

  # app title
  fluidRow(column(12, align = "center", titlePanel("Random Forests"))),

  # sidebar panels...
  fluidRow(column(width=6,
                  # ...for examples
                  sidebarPanel(
                    selectInput("example",
                                label = "Wähle ein Beispiel",
                                choices = list("Gieriges Verfahren"),
                                selected = 1),

                    sliderInput("numberOfElements",
                                label = "Anzahl der zufälligen Daten",
                                min = 1, max = 100, value = 20),

                    fluidRow(column(12, align = "center",
                                    actionButton("update", "Update", icon("rotate"),
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                             ),
                    width = 100,
                    )),
           column(width=6,
                  # ...for user inputs
                  sidebarPanel(
                    fileInput("file", "CSV-Datei einlesen", accept = ".csv"),
                    selectInput("algorithm",
                                label = "Wähle ein Algorithmus",
                                choices = list("Gieriges Verfahren - Regression",
                                               "Gieriges Verfahren - Klassifikationsfall",
                                               "Cost-Complexity Pruning",
                                               "Bagging-Algorithmus auf Regressionsbäume",
                                               "Bagging-Algorithmus auf Klassifikationsbäume"),
                                selected = 1),
                    fluidRow(column(12, align = "center",
                                    actionButton("show", "Show",
                                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                             ),
                    width = 100,
                    ),
                  )),

  # main panel to show data
  mainPanel(
    fluidRow(column(12, align = "center", h3("Test"))),
    plotOutput("plot"),
    width = 100
  )
)
