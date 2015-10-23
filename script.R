install.packages('Rcpp') # Makes installation of 'dyplr' work
install.packages('dplyr')
install.packages('ggplot2')

library(Rcpp)
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
games$ReleaseYear <- as.numeric(format(games$ReleaseDate,'%Y')) # Add ReleaseYear column
games$ReleaseMonth <- as.numeric(format(games$ReleaseDate,'%m')) # Add month column
games$ReleaseQuarter <- ifelse(games$ReleaseMonth <= 3, 1,
                        ifelse(games$ReleaseMonth <= 6, 2,
                        ifelse(games$ReleaseMonth <= 9, 3,
                                                        4))) # Add quarter column
games <- mutate(games, ReleaseYearQuarter = paste(ReleaseYear,ReleaseQuarter,sep='Q')) # Combine year and quarter columns into new column
games$YearCategory <- ifelse(games$ReleaseYear>=2010, '2010 to present',
                       ifelse(games$ReleaseYear>=2005, '2005-2009',
                       ifelse(games$ReleaseYear>=2000, '2000-2004',
                                                       'Before 2000'))) # YearCategory column

summaryStats <- summary(games) # Min, Max, Median and other summary stats for each variable/column

## SINGLE-VARIABLE EXPLORATORY DATA ANALYSES
# System count
ggplot(
  count(games,System),
  aes(
    x = reorder(System, n), 
    y = n
  )
) + ggtitle("Systems") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_y_continuous(breaks=seq(0,4000,500))

# Publisher count
pubCount <- count(games,Publisher)
ggplot(
  pubCount[pubCount$n>=50,],
  aes(
    x = reorder(Publisher, n), 
    y = n
  )
) + ggtitle("Publishers") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

# Developer count
devCount <- count(games,Developer)
devCount <- devCount[devCount$Developer!='',]
ggplot(
  devCount[devCount$n>=50,],
  aes(
    x = reorder(Developer, n), 
    y = n
  )
) + ggtitle("Developers") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

# Genre count
genreCount <- count(games,Genre)
ggplot(
  genreCount[genreCount$n>=50],
  aes(
    x = reorder(Genre, n), 
    y = n
  )
) + ggtitle("Genres") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

# ESRB count
esrbCount <- count(games,ESRB)
ggplot(
  esrbCount[esrbCount$n>=50],
  aes(
    x = reorder(ESRB, n), 
    y = n
  )
) + ggtitle("ESRB ratings") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 


# Metascore categories count
ggplot(
  count(games,MetascoreCategory),
  aes(
    x = factor(
      MetascoreCategory,
      levels = c('Bad','Mixed','Good')
    ), 
    y = n
  )
) + ggtitle("Metascore categories") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

# User score categories count
ggplot(
  count(games,UserScoreCategory),
  aes(
    x = factor(
      UserScoreCategory,
      levels = c('Bad','Mixed','Good')
    ), 
    y = n
  )
) + ggtitle("User score categories") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

# Release years
ggplot(
  count(games,ReleaseYear),
  aes(
    x = factor(
      ReleaseYear, 
      levels=rev(
        levels(factor(ReleaseYear))
      )
    ),
    y = n
  )
) + ggtitle("Release years") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

# Release months
ggplot(
  count(games,ReleaseMonth),
  aes(
    x = factor(
      ReleaseMonth, 
      levels=rev(
        levels(factor(ReleaseMonth))
      )
    ),
    y = n
  )
) + ggtitle("Release months") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

# Year categories
ggplot(
  count(games,YearCategory),
  aes(
    x = factor(
      YearCategory,
      levels = c('Before 2000','2000-2004','2005-2009','2010 to present')
    ), 
    y = n
  )
) + ggtitle("Year categories") +
  labs(x=NULL, y="Count") + 
  geom_bar(stat="identity") + 
  coord_flip() 

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


## TOP GAMES BY CRITICS' AND USERS' SCORES
topMetascores <- function(df){
  df <- df[order(df$Metascore, decreasing = TRUE),]
  return(df)
}

topUserScores <- function(df){
  df <- df[order(df$UserScore, decreasing = TRUE),]
  return(df)  
}

topAllMetascore <- games[order(-games$Metascore),]
userCounts <- c(0,10,50,100,500,1000)

for(i in userCounts){
  varName <- paste('topAllUserScore',i,'Users', sep='')
  assign(
    varName,
    topUserScores(games[which(games$Users >= i),])
  )
}

for(i in unique(games$System)){
  varName <- paste('top',i,'UserScore', sep='')
  assign(
    varName,
    topUserScores(
      games[which(games$System == i & games$Users >= 10),])
  )
  
  varName <- paste('top',i,'Metascore', sep='')
  assign(
    varName,
    topMetascores(games[which(games$System == i),])
  )
}


## MULTIVARIATE EXPLORATORY DATA ANALYSES
metascores <- games$Metascore
userScores <- games$UserScore
scores <- list(metascores,userScores)
categoryCols <- c(3:7,23:27) # Variables for boxplots

# Boxplots, ordered by median scores
for(i in categoryCols){
  j <- 1
  while(j <= length(scores)){
    if(i>=4 & i<=6){
      if(i==4){
        countTbl <- count(games,Publisher)
      } else if(i==5){
        countTbl <- count(games,Developer)
        countTbl <- countTbl[is.na(countTbl$Developer)==FALSE,]
        countTbl <- countTbl[countTbl$Developer!='',]
      } else if(i==6){
        countTbl <- count(games,Genre)
      }
      
      jData <- merge(
        games,
        countTbl[countTbl$n>=50,],
        by=names(games)[i]
      )
      
      category <- assign(
        names(jData[1]),
        jData[[1]]
      )
      
      xLabel <- names(jData[1])
      
      if(j==1){
        jData <- jData$Metascore
      } else {
        jData <- jData$UserScore
      }
    } else {
      jData <- scores[[j]]
      category <- assign(
        names(games[i]),
        games[[i]]
      )
      xLabel <- names(games[i])
    }
      
    yLabel <- ifelse(j==1,'Metascore','User score')
    
    xData <- reorder(
      category,
      jData,
      median
    )
    
    print(
      ggplot() + 
        geom_boxplot(
          aes(
            x=xData,
            y=jData
          )
        ) + 
        xlab(xLabel) + 
        ylab(yLabel) + 
        coord_flip()
    )
    
    j <- j+1
  }
}