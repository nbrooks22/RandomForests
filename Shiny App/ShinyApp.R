library(bslib)
library(DiagrammeR) # for plotting tree in shiny-app
library(data.tree)
library(tidyverse)
library(shiny)
library(shinyjs)

library(RandomForestsPackage)

source("Ui.R")
source("Server.R")

shinyApp(ui, server,
         onStart = function() {
           onStop(function() {
             rm("server", "ui",
                "printGreedyCartRegression", "printGreedyCartClassification", "plotTree", "moveSplit",
                "readCSV", "create_random_sample_data_reg", "create_random_sample_data_class",
                envir = .GlobalEnv)
           })
         })

# RandomForests: argument "m" is missing, with no default
# Bagging: return NULL
# Pruning: Nur aufrufbar mit :::, Klassifikation fehlt und Warning: Error in if: argument is of length zero

# Plotting fehlt bei Beispiel (Pruning, Bagging, Randomforests)
# Plotting fehlt bei Nutzerdaten (Pruning, Bagging, Randomforests)
# Plotting für Klassifikation fehlerhaft