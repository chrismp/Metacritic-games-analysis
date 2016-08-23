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
