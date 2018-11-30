# Downloads data on Hearthstone cards from online API, converts to wide table and saves in Data folder.

library(jsonlite)
library(dplyr)

cards <- fromJSON("https://api.hearthstonejson.com/v1/27641/enUS/cards.collectible.json",
                 simplifyDataFrame = TRUE,
                 flatten = TRUE) %>%
  select(-classes, -entourage, -overload)

unique_mechanics <- 
  union(tolower(unique(unlist(cards$mechanics))), tolower(unique(unlist(cards$referencedTags))))

cards_wide <- cards %>% select(-mechanics, -referencedTags)

mechanics <- lapply(unique_mechanics, function(x){
  mechanic_indicator <- sapply(cards$mechanics, function(y){
    x %in% y
  })
})
mechanics_table <- do.call("cbind", mechanics) %>% as.data.frame()
names(mechanics_table) = unique_mechanics

cards <- cbind(cards_wide, mechanics_table)

write.csv(cards, "Data/cards.csv", row.names = FALSE)