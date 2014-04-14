corr <- function(directory, threshold = 0) {

    nitrate <- vector()
    sulfate <- vector()
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