library(shiny)
library(shinydashboard)
library(tidyverse)

cards <- read_csv("../Data/cards.csv")

ui <- dashboardPage(
  
  dashboardHeader(title = "Hearthstone"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Minion Explorer", tabName = "MinionExplorer", icon = icon("paw")),
      menuItem("Deck Explorer", tabName = "Deck Explorer", icon = icon("bars"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "MinionExplorer",
               box(plotOutput("minionPlot", height = 500))
      ),
      tabItem(tabName = "Deck Explorer",
              h2("Content"))
    )
  )
)

server <- function(input, output) {
  
  output$minionPlot <- renderPlot(
    cards %>% 
      filter(type == "MINION") %>%
      ggplot(aes(x = health, y = attack, color = cost)) + 
      geom_jitter(alpha = 0.5) + 
      geom_smooth() +
      theme_minimal() +
      scale_color_viridis_c()
  )
}

shinyApp(ui = ui, server = server)

