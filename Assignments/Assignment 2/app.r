library(lubridate); library(tidyverse); library(shiny); library(nycflights13); library(ggthemes)

ui <- bootstrapPage( ## Define the UI
  sliderInput(inputId = 'hr', label = 'Scheduled departure hour: ', min = 0, max = 23, value = 12),
  sliderInput(inputId = 'min', label = 'Scheduled departure minute: ', min = 0, max = 59, value = 30),
  plotOutput('plot')
)

library(lubridate); library(tidyverse); library(shiny); library(nycflights13); library(ggthemes)

server <- function(input, output) { ## Define the server code
    output$plot <- renderPlot({
    plot(x = c(if_else(input$min >= 15, hm(paste(input$hr, input$min - 15)), hm(paste(input$hr - 1, input$min + 45))),
               hm(paste(input$hr, input$min)),
               if_else(input$min <= 44, hm(paste(input$hr, input$min + 15)), hm(paste(input$hr + 1, input$min - 45)))),
         y = tibble(sched_dep_time_metric = c(if_else(input$min >= 15, input$hr * 100 + (input$min - 15) * 5 / 3, (input$hr - 1) * 100 + (input$min + 45) * 5 /3),
                                              input$hr * 100 + input$min * 5 / 3,
                                              if_else(input$min <= 44, input$hr * 100 + (input$min + 15) * 5 / 3, (input$hr + 1) * 100 + (input$min - 45) * 5 /3))) %>%
           mutate(diff_1935h = abs(sched_dep_time_metric - 1958.33),
                  pred_prob = as.integer(100/(1+exp(-(0.279603750 - 0.001999519 * diff_1935h))))) %>%
           pull(pred_prob), 
         type = 'l',
         col = 'red',
         lwd = 3,
         xlab = 'Scheduled departure time',
         ylab = 'Percent chance of delay',
         main = paste('Plan around a chance of delay of ', ))
  })
}

shinyApp(ui = ui, server = server) ## Return a Shiny app object



#x = c(input$hr * 100 + input$min * 5 / 3,
#      if(input$min >= 15) input$hr * 100 + (input$min - 15) * 5 / 3 else (input$hr - 1) * 100 + (input$min + 45) * 5 /3,
#      if(input$min <= 45) input$hr * 100 + (input$min - 15) * 5 / 3 else (input$hr + 1) * 100 + (input$min - 45) * 5 /3)