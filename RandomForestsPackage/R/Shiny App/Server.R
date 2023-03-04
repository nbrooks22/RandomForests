server <- function(input, output, session) {
  # change file-button style
  runjs("$('#file').parent().removeClass('btn-default').addClass('btn-info');")

  output$plot <- renderPlot({
    x    <- faithful[, 2]  # Old Faithful Geyser data
    bins <- seq(min(x), max(x), length.out = input$numberOfElements + 1)

    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}
