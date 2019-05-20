library(lubridate); library(tidyverse); library(shiny); library(nycflights13); library(ggthemes)

ui <- bootstrapPage( ## Define the UI
  sliderInput(inputId = 'hr', label = 'Scheduled departure hour: ', min = 0, max = 23, value = 12),
  sliderInput(inputId = 'min', label = 'Scheduled departure minute: ', min = 0, max = 59, value = 30),
  plotOutput('plot')
)

library(lubridate); library(tidyverse); library(shiny); library(nycflights13); library(ggthemes)

server <- function(input, output) { ## Define the server code
    output$plot <- renderPlot({
    plot(x = c(as.POSIXct(as.character(if_else(input$min >= 15, input$hr * 100 + input$min - 15, (input$hr - 1) * 100 + input$min + 45)), format = '%H%M'),
               as.POSIXct(as.character(input$hr * 100 +input$min), format = '%H%M'),
               as.POSIXct(as.character(if_else(input$min <= 44, input$hr * 100 + input$min + 15, (input$hr + 1) * 100 + input$min - 45)), format = '%H%M')),
         y = tibble(diff_1935h = abs(c(input$hr * 60 + input$min - 15 - 1175, input$hr * 60 + input$min - 1175, input$hr * 60 + input$min + 15 - 1175))) %>%
                  mutate(pred_prob = 100/(1+exp(-(0.279603750 - 0.001999519 * diff_1935h)))) %>%
           pull(pred_prob), 
         type = 'l',
         col = 'red',
         lwd = 3,
         tck = .01,
         xlab = 'Scheduled departure time',
         ylab = 'Percent chance of delay',
         main = paste('Plan around a chance of delay of ', input$hr * 100 + input$min))
  })
}

shinyApp(ui = ui, server = server) ## Return a Shiny app object



#x = c(input$hr * 100 + input$min * 5 / 3,
#      if(input$min >= 15) input$hr * 100 + (input$min - 15) * 5 / 3 else (input$hr - 1) * 100 + (input$min + 45) * 5 /3,
#      if(input$min <= 45) input$hr * 100 + (input$min - 15) * 5 / 3 else (input$hr + 1) * 100 + (input$min - 45) * 5 /3)