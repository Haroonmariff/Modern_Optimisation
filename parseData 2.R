parseData <- function(data, firstcolumn, noRuns){
    col <- firstcolumn
    allstats <- (ncol(data)-1)/noRuns   #how many stats were collected. Omit the first column (Generations)
    cols <- seq(col,noRuns*allstats, by=allstats)
    subdata <- data[,cols]
    noGens <- nrow(data)
    pdata <- matrix(nrow = noGens, ncol = 3)
    for (i in 1:noGens){
      pdata[i,1] = i
      pdata[i,2] = mean(subdata[i,])
      pdata[i,3] = 1.96*sd((subdata[i,]))/sqrt(noRuns)   #compute the length of error bar. 
      print(paste(pdata[i,3]))
    }
    return (pdata)
}


