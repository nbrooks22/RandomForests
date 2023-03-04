library(shiny)
library(shinyjs)
library(bslib)

library(RandomForestsPackage)

source("Ui.R")
source("Server.R")


shinyApp(ui, server,
         onStart = function() {
           onStop(function() {
             rm("server", "ui", envir = .GlobalEnv)
           })
         })
