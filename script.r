# install.packages('Rcpp') # Makes installation of 'dyplr' work
# install.packages('dplyr')
# install.packages("reshape2")
# install.packages("plotly")

library(Rcpp)
library(dplyr)
library(reshape2)
library(plotly)
library(ggplot2)

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
df.Games$UserScoreCountCategory <- ifelse(df.Games$UserScores < 10, "Less than 10",
                                          ifelse(df.Games$UserScores < 25, "10-24",
                                                 ifelse(df.Games$UserScores < 50, "25-49",
                                                        ifelse(df.Games$UserScores < 100, "50-99",
                                                               ifelse(df.Games$UserScores < 500, "100-499",
                                                                      "500+")))))


# EXPLORATORY DATA ANALYSIS
eda.Summary <- summary(df.Games)
eda.PercentilesUserScores <- quantile(x = df.Games$UserScores, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99))
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
  MedianCriticUserDiff = median(CriticUserDiff),
  PearsonR_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  ),
  R2_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  )^2
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
  PearsonR_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  ),
  R2_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  )^2,
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
  PearsonR_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  ),
  R2_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  )^2,
  CountCriticScores = sum(CriticScores),
  CountUserScores = sum(UserScores),
  MeanMetascore = mean(Metascore),
  MeanUserScoreX10 = mean(UserScoreX10),
  MedianMetascore = median(Metascore),
  MedianUserScoreX10 = median(UserScoreX10),
  MeanCriticUserDiff = mean(CriticUserDiff),
  MedianCriticUserDiff = median(CriticUserDiff)
)
eda.UserScoreCount <- summarise(
  group_by(df.Games, UserScoreCountCategory),
  n = n(),
  PearsonR_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  ),
  R2_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  )^2,
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
  PearsonR_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  ),
  R2_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  )^2,
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
  PearsonR_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  ),
  R2_Metascore_UserScore = round(  x = cor(x = Metascore, y = UserScore, use = "p", method = "pearson"), digits = 2  )^2,
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
misc.R2_MetascoreUserScore <- cor(
	x = df.Games$Metascore,
	y = df.Games$UserScore,
	use = 'p',
	method = "pearson"
)^2
func.EDACritics <- function(df){
  eda.criticsSummary <- summarise(
    .data = group_by(df, Critic),
    Count = n(),
    PearsonR_CriticScore_Metascore = cor(x = Score, y = Metascore, use = "p", method = "pearson"),
    R2_CriticScore_Metascore = cor(x = Score, y = Metascore, use = "p", method = "pearson")^2,
    PearsonR_CriticScore_UserScore = cor(x = Score, y = UserScore, use = "p", method = "pearson"),
    R2_CriticScore_UserScore = cor(x = Score, y = UserScore, use = "p", method = "pearson")^2,
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


## SCATTERPLOT FOR CERTAIN CRITICS, SEE WHAT CORRELATIONS LOOK LIKE
lm_eqn <- function(m){ # This function makes string of regression line equation, also shows r2 value
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}
misc.Critic <- "Nintendo Life"
df.CriticFilter <- filter(
  .data = df.Games_Critics,
  Critic == misc.Critic
)
chart.CriticUser <- ggplot(
  data = df.CriticFilter,
  mapping = aes(
    x = Score,
    y = UserScore
  )
)
chart.CriticUser + 
  geom_point() +
  ggtitle(misc.Critic) +
  labs(
    x = "Critic score",
    y = "User score"
  ) +
  geom_smooth(
    method = "lm",
    colour = "red"
  ) +
  geom_text(
    x = 25,
    y = 9,
    parse = TRUE,
    label = lm_eqn(
      lm(
        formula = Score ~ UserScore,
        data = filter(
          .data = df.CriticFilter,
          Critic == misc.Critic
        )
      )
    )
  )


dummy <- NULL



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