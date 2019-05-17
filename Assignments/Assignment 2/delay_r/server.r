function(input, output, session) {
  
  # Combine the selected variables into a new data frame
library(lubridate); library(tidyverse)
    user_sched_dep <- reactive({
      hm(paste(input$user_hr, input$user_min))
          
  })
  
    user_sched_dep_lag15 <- if_else(as.numeric(user_sched_dep - minutes(15)) > 0, user_sched_dep - minutes(15), hm('0 00'))
    user_sched_dep_lead15 <- if_else(as.numeric(user_sched_dep + minutes(15)) < 86400, user_sched_dep + minutes(15), hm('23 59'))

    user_diff_1935h_lag15 <- abs(as.numeric(user_sched_dep_lag15 - hm('19:35')) / 60)
    user_diff_1935h <- abs(as.numeric(user_sched_dep - hm('19:35')) / 60)   
    user_diff_1935h_lead15 <- abs(as.numeric(user_sched_dep_lead15 - hm('19:35')) / 60)

    pred_prob_lag15 <- 1/(1+exp(-(0.279603750 - 0.001999519 * (user_diff_1935h - 15))))
    pred_prob_0 <- 1/(1+exp(-(0.279603750 - 0.001999519 * user_diff_1935h)))
    pred_prob_lead15 <- 1/(1+exp(-(0.279603750 - 0.001999519 * (user_diff_1935h + 15))))
  

      
  output$plot1 <- renderPlot({
      ggplot() +
      geom_smooth(aes(x = ))
    ggtitle(paste('Your flight has a', pred_prob, 'percent probability of delay')) +
      theme_fivethirtyeight() +
      scale_color_fivethirtyeight()
  })
  
}
