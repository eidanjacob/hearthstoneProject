# Requires packages

library(rvest)
library(XML)
library(RCurl)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)

# Data on Individual Cards

cards <- read_csv("../Data/cards.csv")
noSpaceCardNames <- gsub(" ", "", cards$name)
nPages <- 1

decks <- data.frame ( matrix (data = 0, ncol = 9 + length(noSpaceCardNames)), stringsAsFactors = FALSE)
names(decks) <- c("name", "author", "url", "type", "class", "rate", "view", "comment", "date", noSpaceCardNames)
decks[1, 1:9] <- "zero"
source("./hearthpwn_scrape_deck.R")
url <- "https://www.hearthpwn.com/decks?filter-unreleased-cards=f&"

for(i in 1:nPages){
  page <- read_html(url)
  
  # Deck Names
  info <- page %>%
    html_nodes(".col-name") %>%
    html_text()
  deckNamesAuthors <- info[-1]
  deckNamesAuthors <- gsub("[[:cntrl:]]", "", deckNamesAuthors) %>%
    str_split(" by ")
  deckNames <- map(deckNamesAuthors, 1) %>%
    trimws()
  deckAuthors <- map(deckNamesAuthors, 2) %>%
    trimws()
  
  # Deck Links
  deckLinks <- page %>%
    html_nodes(".col-name") %>%
    html_nodes(".tip") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  # Deck Types
  deckTypes <- page %>%
    html_nodes(".col-deck-type") %>%
    html_text()
  deckTypes <- deckTypes[-1]
  
  # Class
  deckClass <- page %>%
    html_nodes(".col-class") %>%
    html_text()
  deckClass <- deckClass[-1]
  
  # Ratings
  deckRatings <- page %>%
    html_nodes(".col-ratings") %>%
    html_text()
  deckRatings <- deckRatings[-1]
  deckRatings <- gsub("[[:cntrl:]]", "", deckRatings)
  deckRatings <- gsub("+", "", deckRatings)
  
  # Views & Comments
  deckViews <- page %>%
    html_nodes(".col-views") %>%
    html_text()
  deckViews <- deckViews[-1]
  deckComments <- page %>%
    html_nodes(".col-comments") %>%
    html_text()
  deckComments <- deckComments[-1]
  
  # Last Update
  deckLastUpdate <- page %>%
    html_nodes(".standard-datetime") %>%
    html_text()
  
  decks <- full_join(decks, data.frame(
    name = deckNames, 
    author = deckAuthors,
    url = deckLinks,
    type = deckTypes,
    class = deckClass,
    rate = deckRatings,
    view = deckViews,
    comment = deckComments,
    date = deckLastUpdate,
    stringsAsFactors = FALSE
  ))
  
}

for(relUrl in decks$url[-1]){
  urlstring <- paste0("https://www.hearthpwn.com" , relUrl)
  deckList <- scrapeDeckPwn(urlstring)
  for(card in names(deckList)){
    decks[decks$url == relUrl, card] <- deckList[1,card]
  }
}

decks <- decks %>% 
  mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
