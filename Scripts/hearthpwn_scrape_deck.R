scrapeDeckPwn <- function(url){
  
  # Scrape deck data
  
  page = read_html(url)
  info = page %>%
    html_nodes(".listing-cards-tabular") %>% 
    html_nodes("td.col-name") %>% 
    html_text()
  
  # Clean up
  
  info <- gsub("[[:cntrl:]]", "", info)
  info <- gsub(" ", "", info)
  info <- gsub("&nbsp", "", info)
  cardNames <- substr(info, start = 1, stop = nchar(info)-2)
  cardN <- as.integer( substr(info, start = nchar(info), stop = nchar(info)))
  
  deck <- data.frame(rbind(cardN))
  names(deck) = cardNames
  
  return(deck)
}

### Debugging

url = "https://www.hearthpwn.com/decks/1193667-patch-12-4-pilfer-rogue"
# url = "https://www.hearthpwn.com/decks/1192961-bazooka-paladin"
a <- scrapeDeckPwn(url)