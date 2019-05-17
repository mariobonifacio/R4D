library(lubridate); library(tidyverse); library(shiny); library(nycflights13); library(ggthemes)

server <- function(input, output, session) {
  
  # Combine the selected variables into a new data frame

  user_sched_dep <- reactive({
    hm(paste(input[1], input[2]))
    })
  
  user_sched_dep_lag15 <- if_else(as.numeric(user_sched_dep - minutes(15)) > 0, user_sched_dep - minutes(15), hm('0 00'))
  user_sched_dep_lead15 <- if_else(as.numeric(user_sched_dep + minutes(15)) < 86400, user_sched_dep + minutes(15), hm('23 59'))
  
  user_diff_1935h_lag15 <- abs(as.numeric(user_sched_dep_lag15 - hm('19:35')) / 60)
  user_diff_1935h <- abs(as.numeric(user_sched_dep - hm('19:35')) / 60)   
  user_diff_1935h_lead15 <- abs(as.numeric(user_sched_dep_lead15 - hm('19:35')) / 60)
  
  pred_prob_lag15 <- 1/(1+exp(-(0.279603750 - 0.001999519 * (user_diff_1935h - 15))))
  pred_prob <- 1/(1+exp(-(0.279603750 - 0.001999519 * user_diff_1935h)))
  pred_prob_lead15 <- 1/(1+exp(-(0.279603750 - 0.001999519 * (user_diff_1935h + 15))))
  
  output$plot1 <- renderPlot({
    ggplot() +
      geom_smooth(aes(x = user_diff_1935h, y = pred_prob))
    ggtitle(paste('Your flight has a', pred_prob, 'percent probability of delay')) +
      theme_fivethirtyeight() +
      scale_color_fivethirtyeight()
  })
  
}

ui <- fluidPage(
  
  pageWithSidebar(
    
    headerPanel('Probability of delay by scheduled departure time'),
    sidebarPanel(
      selectInput('user_hr', 'Scheduled departure hour', 0:24, selected = 12),
      selectInput('user_min', 'Scheduled departure minute', 0:59, selected = 30)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
)


shinyApp(ui = ui, server = server)
