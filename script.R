gamesRaw <- read.csv('CSVs/gamedata.csv', header = TRUE, na.strings=c("NA","NULL"))
games <- gamesRaw[,-2:-3]
games <- games[is.na(games$Metascore)==FALSE,]
games <- games[is.na(games$UserScore)==FALSE,]