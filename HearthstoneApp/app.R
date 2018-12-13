library(shiny)
library(shinydashboard)
library(flexdashboard)
library(tidyverse)
library(tidytext)
library(shinyBS)
library(foreign)
library(tm)
library(topicmodels)

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
cardCols <- 2:ncol(pwn)
pwn <- pwn[,cardCols]

adjTable <- read_csv("../Data/adjTable.csv")
rownames(adjTable) <- names(adjTable)
ranking <- order(diag(as.matrix(adjTable)), decreasing = TRUE)
rankedCardNames <- names(adjTable)[ranking]
noSpaceCardNames <- gsub(" ", "", cards$name)

hsMechanics = names(cards)[13:ncol(cards)]

ui <- dashboardPage(
  
  dashboardHeader(title = "Hearthstone"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Minion Explorer", tabName = "MinionExplorer", icon = icon("paw")),
      menuItem("Card Relationships", tabName = "CardRelationships", icon = icon("book")),
      menuItem("Deck Archetypes", tabName = "DeckArchetypes", icon = icon("object-group"))
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
                               min = 10, max = nrow(cards), 50)
                ),
                box(
                  h5("Top 10 Card Co-occurrences"),
                  htmlOutput("top10pairs")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  h2("Associations"),
                  plotOutput("cardAssoc", height = 1100, width = 1100)
                )
              )
      ),
      tabItem(tabName = "DeckArchetypes",
              fluidRow(
                box(
                  h2("Controls"),
                  numericInput("numArch", "Number of Archetypes", value = 10, 
                               min = 5, max = nrow(pwn)),
                  selectInput("cardToInclude", "Include only decks with these cards",
                              choices = rankedCardNames,
                              multiple = TRUE,
                              selected = rankedCardNames[1]),
                  textOutput("decksForLDADiagnostic")
                ),
                box(
                  h2("Number of decks by Archetype"),
                  h5("Please allow a moment to load results"),
                  div(style = "overflow-y: scroll", tableOutput("topicsCount"))
                )
              ),
              fluidRow(
                box(
                  h2("Deck Topic Modeling: Core Cards"),
                  h5("Please allow a moment to load results"),
                  div(style = "overflow-x: scroll", tableOutput("deckLDA"))
                )
              ))
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
    adjTableMatrix <- adjTable[ranking[1:input$nCards], ranking[1:input$nCards]] %>% as.matrix()
    row.names(adjTableMatrix) <- rankedCardNames[1:input$nCards]
    adjTableMatrix
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
    heatmap(topNCards())
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
  
  decksForLDA <- reactive({
    if(is.null(input$cardToInclude)){
      pwn
    } else {
      include <- apply(pwn, 1, function(x) { all(x[input$cardToInclude])})
      pwn[include,]
    }
  })
  
  output$decksForLDADiagnostic <- renderText({
    paste("Using data from", nrow(decksForLDA()), "decks.")
  })
  
  decksLDA <- reactive({
    documents <- apply(decksForLDA(), 1, function(x){
      includedCards = removePunctuation(names(pwn)[which(x>0)])
      a <- paste(includedCards, collapse = "--")
    })
    
    a <- DocumentTermMatrix(Corpus(VectorSource(documents)))
    LDA(a, k = input$numArch)
  })
  
  output$deckLDA <- renderTable({
    top10 <- as.data.frame(terms(decksLDA(), 10))
    names(top10) <- paste("Archetype", 1:length(names(top10)))
    
    top10
  })
  
  output$topicsCount <- renderTable({
    topicsN <- decksLDA() %>%
      topics %>%
      table() %>%
      tidy()
    names(topicsN) <- c("Archetype", "Count")
    topicsN
  })
  
}

shinyApp(ui = ui, server = server)

