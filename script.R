install.packages('Rcpp') # To make installation of 'dyplr' work
# install.packages('plyr')
install.packages('dplyr')
install.packages('ggplot2')

library(Rcpp)
# library(plyr)
library(dplyr)
library(ggplot2)

gamesRaw <- read.csv('CSVs/gamedata.csv', header = TRUE, na.strings=c("NA","NULL"))
games <- gamesRaw[,-2:-3] # Remove columns containing Metacritic page URL and boxart URL

games <- games[is.na(games$Metascore)==FALSE,] # Exclude where Metascore is NULL
games <- games[is.na(games$UserScore)==FALSE,] # Exclude where user score is NULL
games <- mutate(games, UserScoreX10 = UserScore*10) # Add column where user score multiplied by 10, since user score of 7.5 is like Metascore 75.

games$MetascoreCategory <- ifelse(games$Metascore < 50, "Bad",
                           ifelse(games$Metascore < 75, "Mixed",
                                                        "Good"))

games$UserScoreCategory <- ifelse(games$UserScoreX10 < 50, "Bad",
                           ifelse(games$UserScoreX10 < 75, "Mixed",
                                                           "Good"))

games$ReleaseDate <- as.Date(games$ReleaseDate, '%Y-%m-%d') # Convert `ReleaseDate` to dates
games$ReleaseYear <- format(games$ReleaseDate,'%Y') # Add ReleaseYear column
games$ReleaseMonth <- as.numeric(format(games$ReleaseDate,'%m')) # Add month column
games$ReleaseQuarter <- ifelse(games$ReleaseMonth <= 3, 1,
                        ifelse(games$ReleaseMonth <= 6, 2,
                        ifelse(games$ReleaseMonth <= 9, 3,
                                                        4))) # Add quarter column
games <- mutate(games, ReleaseYearQuarter = paste(ReleaseYear,ReleaseQuarter,sep='Q')) # Combine year and quarter columns into new column


ggplot(
  count(games,System),
  aes(
    x = reorder(System, n), 
    y = n
  )
) + labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_y_continuous(breaks=seq(0,4000,500))

summary(games) # Min, Max, Median and other summary stats for each variable/column

# Histogram, Metascore distribution
ggplot(games, aes(x=Metascore)) + 
  geom_histogram(binwidth=1) +
  geom_vline(
    aes(xintercept=mean(Metascore)),
    color="red"
  )

# Histogram, User score distribution
ggplot(games, aes(x=UserScore)) + 
  geom_histogram(binwidth=0.1) +
  geom_vline(
    aes(xintercept=mean(UserScore)),
    color="red"
  )





# Boxplot, Metascore by system
ggplot(
  games, 
  aes(
    x=reorder(System, -Metascore, FUN=median), 
    y=Metascore
  )
) + geom_boxplot() + coord_flip()
