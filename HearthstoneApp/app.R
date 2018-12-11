library(shiny)
library(shinydashboard)
library(flexdashboard)
library(tidyverse)
library(shinyBS)
library(foreign)

cards <- read_csv("../Data/cards.csv",
                  locale = locale(encoding = "latin1")) %>% 
  filter(!is.na(cost)) %>%
  select(-starts_with("collection")) %>%
  mutate_at(vars(name), function(x){gsub('[^ -~]', '', x)}) %>%
  mutate(Parity = case_when(cost %% 2 == 0 ~ "Even",
                            cost %% 2 == 1 ~ "Odd"))

minions <- cards %>% filter(type == "MINION")

pwn <- read_csv("../Data/5000.csv",
               locale = locale(encoding = "latin1"))
cardCols <- 10:ncol(pwn)
decksNum <- pwn[,cardCols]
sums <- rowSums(decksNum)
pwn <- pwn[sums == 30,]
names(pwn) <- gsub(" ", "", names(pwn))

adjTable <- read_csv("../Data/adjTable.csv")
ranking <- order(diag(as.matrix(adjTable)), decreasing = TRUE)
rankedCardNames <- names(adjTable)[ranking]
noSpaceCardNames <- gsub(" ", "", cards$name)

hsMechanics = names(cards)[15:ncol(cards)]

ui <- dashboardPage(
  
  dashboardHeader(title = "Hearthstone"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Minion Explorer", tabName = "MinionExplorer", icon = icon("paw")),
      menuItem("Card Relationships", tabName = "CardRelationships", icon = icon("book"))
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
      tabItem(tabName = "CardRelationships",
              fluidRow(
                box(
                  h2("Controls"),
                  h5("Percentage of cards included"),
                  gaugeOutput("cardGauge"),
                  h5("Percentage of Decks with at least one top Card"),
                  gaugeOutput("deckGauge"),
                  sliderInput("nCards", "Top N Cards",
                               min = 10, max = nrow(cards), 100)
                ),
                box(
                  h2("Associations"),
                  plotOutput("cardAssoc"),
                  h5("Top 10 Card Co-occurrences"),
                  htmlOutput("top10pairs")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  ### Minion Explorer
  
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
      p <- p + aes(color = attack) + scale_color_viridis_c()
    } else {
      if(input$minionColor == "Health"){
        p <- p + aes(color = health) + scale_color_viridis_c()
      } else {
        if(input$minionColor == "Cost"){
          p <- p + aes(color = cost) + scale_color_viridis_c()
        } else {
          if(input$minionColor == "Class"){
            p <- p + aes(color = cardClass) + scale_color_brewer(type = "qual", palette = "Set3")
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
    p + theme_minimal()
  })
  
  output$minionPlot <- renderPlot(minionPlotter())
  
  output$minionImage <- renderImage({
    dbfId <- minions$dbfId[minions$name == input$Viewer]
    list(src = paste0("../hearthstone-card-images/rel/", dbfId, ".png"))
  },
  deleteFile = FALSE)
  
  ### Card Relationships
  
  topNCards <- reactive({
    req(input$nCards)
    adjTable[ranking[1:input$nCards], ranking[1:input$nCards]]
  })
  
  topNDecks <- reactive({
    req(input$nCards)
    includedCards <- rankedCardNames[1:input$nCards]
    includedDecks <- apply(pwn, 1, function(deck){
      any(deck[includedCards]>0)
    })
    pwn[includedDecks,]
  })
  
  output$cardGauge <- renderGauge({
    p <- round(nrow(topNCards()) / nrow(cards) * 100, 0)
    gauge(p, 0, 100, 
          sectors = gaugeSectors(success = c(20, 100), 
                                 warning = c(10,19),
                                 danger = c(0,9)),
          symbol = "%")
  })
  
  output$deckGauge <- renderGauge({
    p <- round(nrow(topNDecks()) / nrow(pwn) * 100, 0)
    gauge(p, 0, 100,
          sectors = gaugeSectors(success = c(50,100),
                                  warning = c(25,49),
                                  danger = c(0,24)),
          symbol = "%")
  })
  
  output$cardAssoc <- renderPlot({
    heatmap(as.matrix(t(topNCards())))
  })
  
  output$top10pairs <- renderUI({
    adjTable <- topNCards()
    adjTable[row(adjTable) >= col(adjTable)] <- 0
    top10index <- order(as.matrix(adjTable), decreasing = TRUE)[1:10]
    pairsOutput <- ""
    entries <- sapply(1:10, function(i){
      nameIndices <- arrayInd(top10index[i], dim(adjTable))
      n1 <- colnames(adjTable)[nameIndices][1]
      n2 <- colnames(adjTable)[nameIndices][2]
      paste0(i, ". ", n1, " and ", n2, ", ", as.matrix(adjTable)[top10index[i]])
    })
    HTML(paste0(entries, sep = "\n", collapse = "<br/>"))
  })
  
}

shinyApp(ui = ui, server = server)

