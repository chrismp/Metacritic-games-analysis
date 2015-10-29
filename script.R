install.packages('Rcpp') # Makes installation of 'dyplr' work
install.packages('dplyr')
install.packages('reshape2')
install.packages('ggplot2')

library(Rcpp)
library(dplyr)
library(reshape2)
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

summaryStats <- summary(games) # Min, Max, Median and other summary stats for each variable/column\


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
give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}
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
        stat_summary(fun.data = give.n, geom="text", fun.y = median) +
        xlab(xLabel) + 
        ylab(yLabel) + 
        coord_flip()
    )
    
    # kruskal.test(Metascore~System,data=games)
    print(
      paste(
        names(games[i]),
        'by',
        yLabel,
        sep=' '
      )
    )
    print(summary(aov(jData~xData)))
    #print('==')
    #print(TukeyHSD(aov(jData~xData)))
    print('=======')
    j <- j+1
  }
}

#ggplot(diamonds, aes(cut, price)) + 
#  geom_boxplot() + 
#  stat_summary(fun.data = give.n, geom = "text") + 
#  coord_flip()


# LINEAR CORRELATIONS
lmStats <- lm(UserScore~Metascore,data=games)
lmStatsSummary <- summary(lmStats)
print(lmStatsSummary)
for(i in c(3,7,23:27)){
  print(names(games[i]))
  print(summary( lm(games$UserScore ~ games[[i]]) ))
  print(summary( lm(games$UserScore ~ games$Metascore + games[[i]]) ))
}

ggplot(
  games,
  aes(
    Metascore,
    UserScore,
    alpha = Users
  )
) +
  theme(legend.position='none') + 
  geom_point(position = 'jitter') + 
  geom_smooth(method=lm) +
  labs(
    title = paste(
      "Adj R2 = ",signif(lmStatsSummary$adj.r.squared, 2),
      "Intercept =",signif(lmStats$coef[[1]],2 ),
      " Slope =",signif(lmStats$coef[[2]], 2),
      " P =",signif(lmStatsSummary$coef[2,4], 2)
    )
  )

# Correlation between Release Year and User Score. Weak
lmStats <- lm(UserScore~ReleaseYear,data=games)
lmStatsSummary <- summary(lmStats)
ggplot(
  games,
  aes(
    ReleaseYear,
    UserScore,
    alpha=Users
  )
) +
  theme(legend.position='none') + 
  geom_point(position = 'jitter') + 
  geom_smooth(method=lm) +
  labs(
    title = paste(
      "Adj R2 = ",signif(lmStatsSummary$adj.r.squared, 2),
      "Intercept =",signif(lmStats$coef[[1]],2 ),
      " Slope =",signif(lmStats$coef[[2]], 2),
      " P =",signif(lmStatsSummary$coef[2,4], 2)
    )
  )

multiLM <- lm(UserScore ~ Metascore+ReleaseYear, data=games)
summary(multiLM)


# CHARTS
# Make boxplot comparing scores by users and critics for each System
games.chart1Data <- melt(
  games, 
  id.vars=c('Id','System'), 
  measure.vars=c('Metascore','UserScoreX10')
)

for(i in unique(games.chart1Data$System)){
  medianMetascore <- median(
    games.chart1Data[
      games.chart1Data$variable=='Metascore' &
        games.chart1Data$System==i,
      ]$value
  )
  
  medianUserScore <- median(
    games.chart1Data[
      games.chart1Data$variable=='UserScoreX10' &
        games.chart1Data$System==i,
      ]$value
  )
  
  MetascoreMinusUserScore <- medianMetascore-medianUserScore

  games.chart1Data$SystemMedianScoreDifference <-ifelse(
    games.chart1Data$System==i,
    MetascoreMinusUserScore,
    games.chart1Data$SystemMedianScoreDifference
  )
}

ggplot(games.chart1Data) + 
  geom_boxplot(
    aes(
      x=reorder(System,SystemMedianScoreDifference),
      y=value,
      fill=variable
    )
  ) +
  coord_flip()

# Make boxplot comparing scores by users and critics for each YearCategory
games.chart2Data <- melt(
  games, 
  id.vars=c('Id','YearCategory'), 
  measure.vars=c('Metascore','UserScoreX10')
)

ggplot(games.chart2Data) + 
  geom_boxplot(
    aes(
      x=factor(
        YearCategory,
        levels = c('Before 2000','2000-2004','2005-2009','2010 to present')
      ),
      y=value,
      fill=variable
    )
  ) +
  coord_flip()

# Make boxplot comparing scores by users and critic for top publishers
topPubs <- pubCount[pubCount$n>=50,]
games.chart3Data <- melt(
  merge(
    games,
    topPubs,
    by=names(games)[4]
  ), 
  id.vars=c('Id','Publisher'), 
  measure.vars=c('Metascore','UserScoreX10')
)

games.chart3Data$PublisherMedianScoreDifference <- NA
for(i in unique(games.chart4Data$Publisher)){
  medianMetascore <- median(
    games.chart3Data[
      games.chart3Data$variable=='Metascore' &
      games.chart3Data$Publisher==i,
    ]$value
  )
  
  medianUserScore <- median(
    games.chart3Data[
      games.chart3Data$variable=='UserScoreX10' &
      games.chart3Data$Publisher==i,
    ]$value
  )
  
  MetascoreMinusUserScore <- medianMetascore-medianUserScore
  
  games.chart3Data$PublisherMedianScoreDifference <-ifelse(
    games.chart3Data$Publisher==i,
    MetascoreMinusUserScore,
    games.chart3Data$PublisherMedianScoreDifference
  )
}

ggplot(games.chart3Data) + 
  geom_boxplot(
    aes(
      x=reorder(Publisher,PublisherMedianScoreDifference),
      y=value,
      fill=variable
    )
  ) +
  coord_flip()

# Make boxplot comparing scores by users and critic for top devs
topDevs <- devCount[devCount$n>=50,]
games.chart4Data <- melt(
  merge(
    games,
    topDevs,
    by=names(games)[5]
  ), 
  id.vars=c('Id','Developer'), 
  measure.vars=c('Metascore','UserScoreX10')
)

games.chart4Data$DeveloperMedianScoreDifference <- NA
for(i in unique(games.chart4Data$Developer)){
  medianMetascore <- median(
    games.chart4Data[
      games.chart4Data$variable=='Metascore' &
        games.chart4Data$Developer==i,
      ]$value
  )
  
  medianUserScore <- median(
    games.chart4Data[
      games.chart4Data$variable=='UserScoreX10' &
        games.chart4Data$Developer==i,
      ]$value
  )
  
  MetascoreMinusUserScore <- medianMetascore-medianUserScore
  
  games.chart4Data$DeveloperMedianScoreDifference <-ifelse(
    games.chart4Data$Developer==i,
    MetascoreMinusUserScore,
    games.chart4Data$DeveloperMedianScoreDifference
  )
}

ggplot(games.chart4Data) + 
  geom_boxplot(
    aes(
      x=reorder(Developer,DeveloperMedianScoreDifference),
      y=value,
      fill=variable
    )
  ) +
  coord_flip()