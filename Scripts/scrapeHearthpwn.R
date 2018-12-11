# Single script to scrape decks with error catching and audio signals

library(rvest)
library(XML)
library(RCurl)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(beepr)
library(httr)

# Read cards.csv, get names without spaces
cards <- read_csv("./Data/cards.csv",
                  locale = locale(encoding = "latin1"))
noSpaceCardNames <- gsub("ñ", "n", gsub(" ", "", cards$name))

# Set up data frame of decks
decks <- data.frame ( matrix (data = 0, ncol = length(noSpaceCardNames)), stringsAsFactors = FALSE)
names(decks) <- noSpaceCardNames
decks <- decks[-1,]

# Parameters for scraping
minimumDeckNum <- 5000
listUrl <- "https://www.hearthpwn.com/decks"
pageNum <- 1
waitTime <- 3

# Scraping loop
while(nrow(decks) < minimumDeckNum){
  Sys.sleep(waitTime)
  deckLinks <- read_html(listUrl) %>%
    html_nodes(".col-name") %>%
    html_nodes(".tip") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  for(deckLink in deckLinks){
    tryCatch(expr = {
      deckUrl <- paste0("https://www.hearthpwn.com", deckLink)
      Sys.sleep(waitTime)
      info <- read_html(deckUrl) %>%
        html_nodes(".listing-cards-tabular") %>% 
        html_nodes("td.col-name") %>% 
        html_text()
      
      # Clean up
      info <- gsub("[[:cntrl:]]", "", info)
      info <- gsub(" ", "", info)
      info <- gsub("&nbsp", "", info)
      cardNames <- substr(info, start = 1, stop = nchar(info)-2)
      cardN <- as.integer( substr(info, start = nchar(info), stop = nchar(info)))
      
      if(sum(cardN)==30 & max(cardN) <= 2){
        decks[nrow(decks)+1 , cardNames] <- cardN
      }
    },
    finally = {
      next
    })
    
  }
  
  pageNum <- pageNum + 1
  listUrl <- paste0("https://www.hearthpwn.com/decks?page=",pageNum)
  print(nrow(decks))
  
}
# Replace NA with 0
decks[is.na(decks)] <- 0
# Complete!
beep(3)