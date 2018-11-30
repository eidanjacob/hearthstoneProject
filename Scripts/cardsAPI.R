# Downloads data on Hearthstone cards from online API, converts to wide table and saves in Data folder.

library(jsonlite)
library(dplyr)

cards <- fromJSON("https://api.hearthstonejson.com/v1/27641/enUS/cards.collectible.json",
                 simplifyDataFrame = TRUE,
                 flatten = TRUE) %>%
  select(-classes, -entourage, -overload, -starts_with("playReq"), -artist, -collectible, -flavor, -id,
         -howToEarn, -howToEarnGolden, -collectionText, -hideStats, -questReward, -elite, -targetingArrowText,
         -multiClassGroup, -faction)

unique_mechanics <- 
  union(unique(unlist(cards$mechanics)), unique(unlist(cards$referencedTags)))

cards_wide <- cards %>% select(-mechanics, -referencedTags)
cards_wide[unique_mechanics] <- FALSE

for(i in 1:nrow(cards_wide)){
  m <- c(unlist(cards$mechanics[i]), unlist(cards$referencedTags[i]))
  if(is.null(m[[1]])){
    next
  }
  for(x in m){
    cards_wide[i,x] <- TRUE
  }
}
write.csv(cards_wide %>% as.data.frame(), "Data/cards.csv", row.names = FALSE)