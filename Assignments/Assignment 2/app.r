library(lubridate); library(tidyverse); library(shiny); library(nycflights13); library(ggthemes)

ui <- bootstrapPage( ## Define the UI
  sliderInput(inputId = 'hr', label = 'Scheduled departure hour: ', min = 0, max = 23, value = 12),
  sliderInput(inputId = 'min', label = 'Scheduled departure minute: ', min = 0, max = 59, value = 30),
  plotOutput('plot')
)



server <- function(input, output) { ## Define the server code
    output$plot <- renderPlot({
    plot(x = rep(round(input$hr * 100 +input$min, 1), 3),
         y = c(100/(1+exp(-(0.279603750 - (0.001999519 - 0.0000148) * abs(input$hr * 60 + input$min - (19 * 60 + 35))))),
               100/(1+exp(-(0.279603750 - 0.001999519 * abs(input$hr * 60 + input$min - (19 * 60 + 35))))),
               100/(1+exp(-(0.279603750 - (0.001999519 + 0.0000148) * abs(input$hr * 60 + input$min - (19 * 60 + 35)))))),
         xlim = range(input$hr * 100 +input$min - 15, input$hr * 100 + input$min + 15),
         type = 'b',
         lwd = 3,
         lty = 2,
         width = 0,
         tck = .02,
         xlab = 'Scheduled departure time',
         ylab = 'Percent chance of delay',
         pch = 3,
         main = paste('Plan around a chance of delay of ', round(100/(1+exp(-(0.279603750 - (0.001999519) * abs(input$hr * 60 + input$min - (19 * 60 + 35)))))), '%'))
      
  })
}

shinyApp(ui = ui, server = server) ## Return a Shiny app object
