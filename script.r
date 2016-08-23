rawData.Directory <- "Raw-data"

rawData.Files <- list.files(
	path = rawData.Directory,
	pattern = ".csv",
	full.names = TRUE
)

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
		x = paste0("df.",temp.dfName),
		value = read.csv(temp.rawDataFileName)
	)
}

