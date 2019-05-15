pageWithSidebar(
  headerPanel('Probability of delay by scheduled departure time'),
  sidebarPanel(
    selectInput('user_hr', 'Scheduled departure hour', 0:24),
    selectInput('user_min', 'Scheduled departure minute', 0:59,
                selected=30)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)
