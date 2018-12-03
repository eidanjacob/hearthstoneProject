library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyBS)
library(foreign)

cards <- read_csv("../Data/cards.csv") %>% 
  filter(!is.na(cost)) %>%
  select(-starts_with("collection")) %>%
  mutate_at(vars(name), function(x){gsub('[^ -~]', '', x)}) %>%
  mutate(Parity = case_when(cost %% 2 == 0 ~ "Even",
                            cost %% 2 == 1 ~ "Odd"))

minions <- cards %>% filter(type == "MINION")

hsClasses = c("Druid", "Hunter", "Mage", "Paladin", "Priest", "Rogue", "Shaman", "Warlock", "Warrior")
hsMechanics = names(cards)[15:ncol(cards)]

ui <- dashboardPage(
  
  dashboardHeader(title = "Hearthstone"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Minion Explorer", tabName = "MinionExplorer", icon = icon("paw")),
      menuItem("Deck Explorer", tabName = "Deck Explorer", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "MinionExplorer",
              fluidRow(
                # Output plot
                box(
                  h2("Minion Explorer"),
                  plotOutput("minionPlot",
                             hover = hoverOpts(id = "plot_hover")),
                  h2("Controls"),
                  # 2-value slider for card cost
                  sliderInput(inputId = "minionCostRange",
                              label = "Costs",
                              min = 0, max = max(cards$cost),
                              value = c(0,10),
                              step = 1),
                  # Selection of Mechanics
                  selectInput(inputId = "minionMechanics",
                              label = "Mechanics",
                              choices = hsMechanics,
                              multiple = TRUE),
                  # Axes
                  selectInput(inputId = "minionX",
                              label = "X Axis",
                              choices = c("Attack", "Cost", "Health"),
                              selected = "Health"),
                  selectInput(inputId = "minionY",
                              label = "Y Axis",
                              choices = c("Attack", "Cost", "Health", "Count"),
                              selected = "Attack"),
                  # Coloring
                  selectInput(inputId = "minionColor",
                              label = "Coloring",
                              choices = c("Attack", "Class", "Cost", "Health", "None"),
                              selected = "Cost"),
                  # Faceting
                  selectInput(inputId = "minionFacet",
                              label = "Faceting",
                              choices = c("Class", "Parity", "None"),
                              selected = "None")
                ),
                box(
                  h2("Card Viewer"),
                  selectInput(inputId = "Viewer",
                              label = "Select a card to View",
                              choices = sort(minions$name)),
                  imageOutput("minionImage")
                )
              )
      ),
      tabItem(tabName = "Deck Explorer",
              h2("Content"))
    )
  )
)

server <- function(input, output, session) {
  
  minionsToPlot <- reactive({
    
    m <- minions %>%
      filter(cost >= input$minionCostRange[1]) %>%
      filter(cost <= input$minionCostRange[2])
    
    if(!is.null(input$minionMechanics)){
      
      for(mechanic in input$minionMechanics){
        m <- m[unlist(m[,mechanic]),]
      }
      
    }
    
    m <- as.data.frame(m)
    updateSelectInput(session,
                      "Viewer",
                      choices = sort(m$name))
    m
  })
  
  minionPlotter <- reactive({
    p <- minionsToPlot() %>% ggplot()
    
    # X axis
    if(input$minionX == "Attack"){
      p <- p + aes(x = attack)
    } else {
      if(input$minionX == "Health"){
        p <- p + aes(x = health)
      } else {
        p <- p + aes(x = cost)
      }
    }
    
    # Coloring
    if(input$minionColor == "Attack"){
      p <- p + aes(color = attack)
    } else {
      if(input$minionColor == "Health"){
        p <- p + aes(color = health)
      } else {
        if(input$minionColor == "Cost"){
          p <- p + aes(color = cost)
        } else {
          if(input$minionColor == "Class"){
            p <- p + aes(color = cardClass)
          }
        }
      }
    }
    
    # Faceting
    if(input$minionFacet == "Class"){
      p <- p + facet_wrap( ~ cardClass, ncol = 3)
    } else {
      if(input$minionFacet == "Parity"){
        p <- p + facet_wrap(~ Parity, ncol = 2)
      } 
    }
    
    # Y axis
    if(input$minionY == "Count"){
      # Make histogram(s)
      p <- p + geom_histogram(binwidth = 1)
    } else {
      if(input$minionY == "Attack"){
        p <- p + aes(y = attack)
      } else {
        if(input$minionY == "Health"){
          p <- p + aes(y = health)
        } else {
          p <- p + aes(y = cost)
        }
      }
      # Make scatterplot(s)
      p <- p + geom_count()
    }
    p
  })
  
  output$minionPlot <- renderPlot(minionPlotter())
  
  output$minionImage <- renderImage({
    dbfId <- minions$dbfId[minions$name == input$Viewer]
    list(src = paste0("../hearthstone-card-images/rel/", dbfId, ".png"))
  },
  deleteFile = FALSE)
  
}

shinyApp(ui = ui, server = server)

