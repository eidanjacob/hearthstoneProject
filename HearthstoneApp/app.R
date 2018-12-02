library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyBS)
library(foreign)

cards <- read_csv("../Data/cards.csv") %>% 
  filter(!is.na(cost)) %>%
  select(-starts_with("collection")) %>%
  mutate_at(vars(name), function(x){gsub('[^ -~]', '', x)})

minions <- cards %>% filter(type == "MINION")

hsClasses = c("Druid", "Hunter", "Mage", "Paladin", "Priest", "Rogue", "Shaman", "Warlock", "Warrior")
hsMechanics = names(cards)[15:ncol(cards)]

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
              fluidRow(
                # Output plot
                box(
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
                              choices = c("Class", "Mechanic", "Parity", "None"),
                              selected = "None")
                ),
                box(
                  selectInput(inputId = "Viewer",
                              label = "View a card",
                              choices = sort(minions$name)),
                  imageOutput("minionImage")
                )
              )
      ),
      tabItem(tabName = "SpellExplorer",
              h2("Content")),
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

