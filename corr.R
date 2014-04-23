corr <- function(directory, threshold = 0){


    corVector <- vector()
    
    for (i in 1:332){
    
        fileName <- paste("./", directory, "/", sprintf("%03s", i), ".csv", sep="")
        File <- read.csv(fileName)
        badsulfate <- is.na(File$sulfate) | is.na(File$nitrate)
        sulfate <- File$sulfate[!badsulfate]
        nitrate <- File$nitrate[!badsulfate]
        if(length(nitrate)>threshold & length(sulfate)>threshold)
        {
            r <- cor(sulfate, nitrate)
            corVector <- c(corVector, r)
        }
    }
    corVector
}


install.packages("swirl")
library(swirl)
swirl()
M

cr <- corr("specdata", 400)
head(cr)



corr.t <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    # 1. As before, set the correct directory.
    directory = "specdata"
    # 2. An integer relationship shall be established in order to provide a comparative
    # basis for the number of total correlations to the threshold value, this will be
    # expressed finally in a vector (corcomplete)
    # 3. A for loop shall be created in order to read the file and determine the number
    # of complete cases.
    corcomplete <- vector()
    for (i in 1:332)
    {
        fileName <- paste("./", directory, "/", sprintf("%03d", i), ".csv", sep="")
        File <- read.csv(fileName)
        completecases <- length(File$sulfate[complete.cases(File$sulfate & File$nitrate)])
        # 4. Now we need to create two vectors for sulfate and nitrate,
        # so that when there are complete cases we can see the correlation
        # The vectors mu
        sulfate <- File$sulfate[complete.cases(File$nitrate & File$sulfate)]
        nitrate <- File$nitrate[complete.cases(File$sulfate & File$nitrate)]
        if (completecases > threshold) {
            cor(sulfate, nitrate)
            corcomplete <- c(corcomplete, cor(sulfate, nitrate))
        }
    }
    corcomplete
}













corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    # 1. As before, set the correct directory.
    # 2. An integer relationship shall be established in order to provide a comparative
    # basis for the number of total correlations to the threshold value, this will be
    # expressed finally in a vector (corcomplete)
    # 3. A for loop shall be created in order to read the file and determine the number
    # of complete cases.
    corcomplete <- vector()
    for (i in 1:332)
    {
        DataSet <- paste("./", directory, "/", sprintf("%03d", i), ".csv", sep="")
        Data <- read.csv(DataSet)
        completecases <- length(Data$sulfate[complete.cases(Data$sulfate & Data$nitrate)])
        # 4. Now we need to create two vectors for sulfate and nitrate,
        # so that when there are complete cases we can see the correlation
        # The vectors therefore need to exclude incomplete cases and chill
        sulfate <- Data$sulfate[complete.cases(Data$nitrate & Data$sulfate)]
        nitrate <- Data$nitrate[complete.cases(Data$sulfate & Data$nitrate)]
        if (completecases > threshold) {
            cor(sulfate, nitrate)
            corcomplete <- c(corcomplete, cor(sulfate, nitrate))
        }
    }
    corcomplete
}















