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
                "printRegression", "printClassification", "plotTree", "moveSplit",
                "readCSV", "create_random_sample_data_reg", "create_random_sample_data_class",
                "listOfTrees", "method",
                envir = .GlobalEnv)
           })
         })
