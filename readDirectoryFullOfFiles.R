readDirectoryFullOfFiles <- function(directory) {
	filesToProcess <- dir(path = directory, pattern = "*.csv", no.. = TRUE)
	dataList <- lapply(filesToProcess, function(x) {
		read.csv(file = paste(directory,x,sep = "/"),header = TRUE)})
	Reduce(function(x,y) {rbind(x,y)}, dataList)
}