complete <- function(directory, id=1:332) {
    #Parses through data files, returning the file name and number of complete
    #   (ie both nitrate and sulfate ratings, no NA's) entries in that file.
    #
    #Args:
    #   directory: the path to the directory containing monitor data from WD
    #               ie. 'specdata' or '../../atmoshpere_monitoring/specdata'
    #
    #   id: a numeric vector of monitor ids to pull data for
    #
    #Returns:
    #   a data frame containing two colums, the id of the data file ('id') and the
    #    number of complete cases ('nobs')
    
    #error handling: id of input files oustide the range of 1:322
    if(max(id) > 332 | min(id) < 1 | sum(is.na(id) > 0)) {
        stop("Monitor id out of bounds!")
    }
    
    nobs = numeric()
    for(i in id) {
        #monitor data filenames are 3 digits, padded with zeros
        monitorID <- formatC(i, width=3, flag=0)
        filePath <- paste(getwd(), '/', directory, '/', monitorID, '.csv', sep="")
        data <- read.csv(filePath)
        #filter out complete readings
        data <- data[(!is.na(data$nitrate) & !is.na(data$sulfate)),]
        nobs <- append(nobs,length(data[,1]))
    }
    data.frame(id,nobs)
}
