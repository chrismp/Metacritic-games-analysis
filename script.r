# install.packages('Rcpp') # Makes installation of 'dyplr' work
# install.packages('dplyr')
# install.packages("reshape2")
# install.packages("plotly")

library(Rcpp)
library(dplyr)
library(reshape2)
library(plotly)

## RAW DATA CSV FILE NAMES
rawData.Directory <- "Raw-data"
rawData.Files <- list.files(
	path = rawData.Directory,
	pattern = ".csv",
	full.names = TRUE
)

## MAKE DATAFRAME VARIABLES FROM CSVs
for (i in 1:length(rawData.Files)) {
	temp.rawDataFileName <- rawData.Files[i]
	temp.dfName <- gsub(
		pattern = paste0(rawData.Directory,'/'),
		replacement = '',
		x = temp.rawDataFileName
	)
	temp.dfName <- gsub(
		pattern = ".csv",
		replacement = '',
		x = temp.dfName
	)
	
	assign(
		x = paste0("raw.",temp.dfName),
		value = read.csv(temp.rawDataFileName)
	)
}


## MAKE DATAFRAME FOR GAME DATA
df.Games <- raw.Games

# Remove games without Metascore or User score
df.Games <- filter(
	.data = df.Games,
	is.na(raw.Games$Metascore)==FALSE,
	is.na(raw.Games$UserScore)==FALSE
)

# Remove games with user scores that seem overrun by insincere votes
misc.SpamGames <- c(
	"/game/pc/navy-seals-weapons-of-mass-destruction",
	"/game/pc/gods-and-generals"
	
)
for (i in misc.SpamGames) {
	df.Games <- df.Games[!df.Games$GameURL==i, ]
}
df.Games <- filter(
	.data = df.Games,
	!grepl(
		pattern = "shrek",
		ignore.case = TRUE,
		x = Title
	)
)

# Add columns relating to critic/user scores
df.Games <- mutate(
	.data = df.Games,
	UserScoreX10 = UserScore*10
)
df.Games <- mutate(
	.data = df.Games, 
	CriticUserDiff = Metascore - UserScoreX10
)
df.Games$MetascoreCategory <- ifelse(df.Games$Metascore < 50, "Bad",
																	ifelse(df.Games$Metascore < 75, "Mixed",
																				 "Good"))

df.Games$UserScoreCategory <- ifelse(df.Games$UserScoreX10 < 50, "Bad",
																	ifelse(df.Games$UserScoreX10 < 75, "Mixed",
																				 "Good"))
misc.ReleaseDateFormat <- "%b %d, %Y"
misc.ReleaseYearFormat <- "%Y"
df.Games$ReleaseDateFormatted <- as.Date(
	x = df.Games$ReleaseDate,
	misc.ReleaseDateFormat
)
df.Games$ReleaseYear <- as.numeric(
	x = format(
		x = df.Games$ReleaseDateFormatted,
		misc.ReleaseYearFormat
	)
)


# EXPLORATORY DATA ANALYSIS
eda.Summary <- summary(df.Games)
eda.metascoreCategoryCount <- filter( count(df.Games,MetascoreCategory), is.na(MetascoreCategory)==FALSE)
eda.topMetascoreGames <- df.Games %>% arrange(desc(Metascore))
eda.bottomMetascoreGames <- df.Games %>% arrange(Metascore)
eda.topUserGames <- df.Games %>% arrange(desc(UserScore))
eda.bottomUserGames <- df.Games %>% arrange(UserScore)
eda.ReleaseYears <- summarise(
  group_by(df.Games, ReleaseYear),
  n = n(),
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScoreX10 = mean(UserScoreX10),
  MedianMetascore = median(Metascore),
  MedianUserScoreX10 = median(UserScoreX10),
  MeanCriticUserDiff = mean(CriticUserDiff),
  MedianCriticUserDiff = median(CriticUserDiff)
)
eda.MetascoreByYear <- summarise(
  group_by(df.Games, ReleaseYear, MetascoreCategory),
  n = n()
)
eda.UserScoreByYear <- summarise(
  group_by(df.Games, ReleaseYear, UserScoreCategory),
  n = n()
)
eda.Systems <- summarise(
  group_by(df.Games, SystemLabel),
  n = n(),
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScoreX10 = mean(UserScoreX10),
  MedianMetascore = median(Metascore),
  MedianUserScoreX10 = median(UserScoreX10),
  MeanCriticUserDiff = mean(CriticUserDiff),
  MedianCriticUserDiff = median(CriticUserDiff)
)
eda.Devs <- summarise(
  group_by(df.Games, Developer),
  n = n(),
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScoreX10 = mean(UserScoreX10),
  MedianMetascore = median(Metascore),
  MedianUserScoreX10 = median(UserScoreX10),
  MeanCriticUserDiff = mean(CriticUserDiff),
  MedianCriticUserDiff = median(CriticUserDiff)
)


# ANALYSIS WITH GENRES
df.Games_Genres <- merge(
  x = df.Games,
  y = raw.Genres,
  by = "GameURL"
)
eda.Genres <- summarise(
  group_by(df.Games_Genres, Genre),
  n = n(),
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScoreX10 = mean(UserScoreX10),
  MedianMetascore = median(Metascore),
  MedianUserScoreX10 = median(UserScoreX10),
  MeanCriticUserDiff = mean(CriticUserDiff),
  MedianCriticUserDiff = median(CriticUserDiff)
)


# ANALYSIS WITH PUBLISHERS
df.Games_Publishers <- merge(
  x = df.Games,
  y = raw.Publishers,
  by = "GameURL"
)
eda.Publishers <- summarise(
  group_by(df.Games_Publishers, Publisher),
  n = n(),
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScoreX10 = mean(UserScoreX10),
  MedianMetascore = median(Metascore),
  MedianUserScoreX10 = median(UserScoreX10),
  MeanCriticUserDiff = mean(CriticUserDiff),
  MedianCriticUserDiff = median(CriticUserDiff)
)


# ANALYSIS WITH CRITICS
func.EDACritics <- function(df){
  eda.criticsSummary <- summarise(
    .data = group_by(df, Critic),
    Count = n(),
    Pearson_CriticScore_Metascore = cor(x = Score, y = Metascore, use = "p", method = "pearson"),
    Pearson_CriticScore_UserScore = cor(x = Score, y = UserScore, use = "p", method = "pearson"),
    AverageCriticScore = mean(Score, na.rm = TRUE), # Average score given by this critic
    AverageMetascore = mean(Metascore, na.rm = TRUE), # Average Metascore for albums rated by this critic
    AverageCriticScoreMetascoreDifference = mean(Score) - mean(Metascore),
    AverageUserScoreX10 = mean(UserScore*10, na.rm = TRUE),
    AverageCriticScoreUserScoreDifference = mean(Score, na.rm = TRUE) - mean(UserScore*10, na.rm = TRUE),
    MedianCriticScore = median(Score, na.rm = TRUE),
    MedianMetascore = median(Metascore, na.rm = TRUE),
    MedianCriticScoreMetascoreDifference = median(Score, na.rm = TRUE) - median(Metascore, na.rm = TRUE),
    MedianUserScoreX10 = median(UserScore*10, na.rm = TRUE),
    MedianCriticScoreUserScoreDifference = median(Score, na.rm = TRUE) - median(UserScore*10, na.rm = TRUE)
  )
}
df.Critics <- raw.Critics
df.Critics$ReviewDateFormatted <- as.Date(
  x = df.Critics$Date,
  misc.ReleaseDateFormat
)
df.Games_Critics <- merge(
  x = df.Games,
  y = df.Critics,
  by = "GameURL"
)
eda.Critics <- func.EDACritics(df.Games_Critics)
eda.Critics_1990s <- func.EDACritics(
  filter(
    .data = df.Games_Critics,
    ReleaseYear < 2000
  )
)
eda.Critics_2000_2004 <- func.EDACritics(
  filter(
    .data = df.Games_Critics,
    ReleaseYear >= 2000,
    ReleaseYear <= 2004
  )
)
eda.Critics_2005_2009 <- func.EDACritics(
  filter(
    .data = df.Games_Critics,
    ReleaseYear >= 2005,
    ReleaseYear <= 2009
  )
)
eda.Critics_2010s <- func.EDACritics(
  filter(
    .data = df.Games_Critics,
    ReleaseYear >= 2010
  )
)


## TURN DATAFRAMES INTO SOMETHING PLOTLY/D3 CAN USE
chartData.MetascoreByYear <- dcast(
  data = eda.MetascoreByYear,
  formula = ReleaseYear ~ MetascoreCategory
)
chartData.UserScoreByYear <- dcast(
  data = eda.UserScoreByYear,
  formula = ReleaseYear ~ UserScoreCategory
)




## Rock Paper Shotgun has no Metascores.
# dummy <- filter(
#   .data = df.Games_Critics,
#   Critic=="Rock, Paper, Shotgun"
# )
dummy <- NULL

# analysis needed
# - Correlation between userscores and Metascores
# - "Overrated" games
# - "Underrated" games




# library(plotly)
# library(htmlwidgets)
# set.seed(100)
# d <- diamonds[sample(nrow(diamonds), 1000), ]
# p <- plot_ly(d, x = carat, y = price, text = paste("Clarity: ", clarity),
#         mode = "markers", color = carat, size = carat)
# saveWidget(
#   widget = as.widget(
#     x = p
#   ),
#   file = "plot.html"
# )