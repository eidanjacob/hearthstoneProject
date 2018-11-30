library(shiny)
library(shinydashboard)
library(tidyverse)

cards <- read_csv("../Data/cards.csv") %>% 
  filter(!is.na(cost))
head(cards)

hsClasses = c("Druid", "Hunter", "Mage", "Paladin", "Priest", "Rogue", "Shaman", "Warlock", "Warrior")
hsMechanics = names(cards)[15:66]

ui <- dashboardPage(
  
  dashboardHeader(title = "Hearthstone"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Minion Explorer", tabName = "MinionExplorer", icon = icon("paw")),
      menuItem("Spell Explorer", tabName = "SpellExplorer", icon = icon("book")),
      menuItem("Deck Explorer", tabName = "Deck Explorer", icon = icon("bars"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "MinionExplorer",
              # Output plot
              box(
                h2("Plot of Something"),
                plotOutput("minionPlot")),
              # Controls
              box(
                h2("Controls"),
                # 2-value slider for card cost
                sliderInput(inputId = "minionCostRange",
                            label = "Filter: Minion Costs",
                            min = 0, max = max(cards$cost),
                            value = c(0,10),
                            step = 1),
                checkboxGroupInput(inputId = "minionClass",
                                   label = "Filter: Minion Class",
                                   choices = hsClasses, selected = hsClasses),
                selectInput(inputId = "minionMechanics",
                            label = "Filter: Minion Mechanics",
                            choices = hsMechanics,
                            multiple = TRUE)
                
              )
      ),
      tabItem(tabName = "SpellExplorer",
              h2("Content")),
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

