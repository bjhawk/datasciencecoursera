pollutantmean <- function(directory, pollutant, id=1:332) {
    #Finds the mean of a named pollutant across specified list of monitors
    #
    #Args:
    #   directory: the path to the directory containing monitor data from WD
    #               ie. 'specdata' or '../../atmoshpere_monitoring/specdata'
    #
    #   pollutant: name of pollutant, should match a pollutant column name in
    #               the given data, ie either 'nitrate' or 'sulfate'
    #
    #   id: a numeric vector of monitor ids to pull data for
    #
    #Returns:
    #   the mean of the given pollutant readings from the given set of monitors
    
    
    #error handling: id of input files oustide the range of 1:322
    if(max(id) > 332 | min(id) < 1 | sum(is.na(id) > 0)) {
        stop("Monitor id out of bounds!")
    }
    
    #monitor data filenames are 3 digits, padded with zeros
    id <- formatC(id, width=3, flag=0)
    dataSum <- 0
    dataCount <- 0
    for(f in id) {
        filePath <- paste(getwd(), '/', directory, '/', f, '.csv', sep="")
        data <- read.csv(filePath)
        dataSum <- dataSum + sum(data[!is.na(data[[pollutant]]),pollutant])
        dataCount <- dataCount + length(data[!is.na(data[[pollutant]]),pollutant])
    }
    
    #error handling: prevent division by zero
    if(dataCount < 1) {
        stop("Cannot divide by zero, no meaningful data for mean!")
    }
    
    #return mean, rounded to 3 significant digits
    round(dataSum/dataCount, digits=3)
}