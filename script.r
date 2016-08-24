# install.packages('Rcpp') # Makes installation of 'dyplr' work
# install.packages('dplyr')

library(Rcpp)
library(dplyr)

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


# ONE-DIMENSIONAL EXPLORATORY DATA ANALYSIS
eda1.Summary <- summary(df.Games)
eda1.topMetascoreGames <- df.Games %>% arrange(desc(Metascore))
eda1.bottomMetascoreGames <- df.Games %>% arrange(Metascore)
eda1.topUserGames <- df.Games %>% arrange(desc(UserScore))
eda1.bottomUserGames <- df.Games %>% arrange(UserScore)

# analysis needed
# - Scores by release year
# - scores by genre
# - scores by dev
# - scores by publisher
# - scores by game system, maybe exclude PC
# - Correlation between userscores and Metascores
# - How each critic's Metascore lines up with user scores
# - "Overrated" games
# - "Underrated" games