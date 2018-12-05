# Creates an "adjacency table" for hearthstone cards, given deck data
# # Output is square matrix with dimension = number of unique cards
# # cell (i,j) is number of decks containing both cards i and j 
# # (when i=j, cell is number of decks with just that card)

library(gtools)
library(tidyverse)

deckDataPath <- "./Data/hearthpwn.csv"
decksCSV <- read_csv(deckDataPath, locale = locale(encoding = "latin1"))
cardCols <- 10:ncol(decksCSV)
decksNum <- decksCSV[,cardCols]
sums <- rowSums(decksNum)
decksNum <- decksNum[sums == 30,]
decksBool <- decksNum > 0

m <- matrix(0, 
            nrow = length(cardCols),
            ncol = length(cardCols))

for(i in 1:nrow(decksBool)){
  indices <- which(decksBool[i,])
  a <- permutations(length(indices), 2, indices)
  m[a[,1], a[,2]] <- m[a[,1], a[,2]] + 1
}

write.csv(m, "./Data/adjTable.csv", row.names = FALSE)