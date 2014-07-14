corr <- function(directory, threshold=0) {
    #Finds the correlation of nitrate and sulfate data for all monitor data
    #   files where the number of complete cases is greater than the threshold
    #
    #Args:
    #   directory: the path to the directory containing monitor data from WD
    #               ie. 'specdata' or '../../atmoshpere_monitoring/specdata'
    #
    #   threshold: the minimum number of complete cases that must exist to 
    #
    #Returns:
    #   a numeric vector containing correlation data for all cases that meet
    #   the threshold, or a numeric vector of a single zero if there are no
    #   cases that meet the required number of complete observations.
    files <- list.files(directory)
    cr <- numeric()
    
    for(file in files) {
        fileData <- read.csv(paste(getwd(), '/', directory, '/', file, sep=''))
        fileData <- fileData[(!is.na(fileData$nitrate)
                              & !is.na(fileData$sulfate)),]
        if(length(fileData[,1]) > threshold) {
            cr <- append(cr, cor(fileData[,'nitrate'], fileData[,'sulfate'],
                           use="complete.obs")
                         )
        }
    }
    return(cr)
}