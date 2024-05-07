getData <- function(){
#  data("eurodist", package = "datasets")
  data <- read.csv("C:/Users/Haroon Muhammad Arif/Desktop/BCU/mo/Pakistan.csv")
  D <- as.matrix(data) #eurodist
#  print(head(D))
  return(D)
}

#D:\Modern Optimization\project\routeopt\summary_to_publish.csv
#D:/Modern Optimization/project/routeopt/indian-cities-dataset.csv

tourLength <- function(tour, distMatrix) {
     tour <- c(tour, tour[1])             #e.g. convert A,B,C to A,B,C,A. Thus the tour finishes where it started.
     route <- embed(tour, 2)[,2:1]        #converts the tour into a matrix of trips. i.e. 
#     print(paste("dimesion -> ",dim(distMatrix), "heheh route -> ",route))
     if (any(route < 1) || any(route > nrow(distMatrix))) {
       stop("Invalid indices in route. Check your input data.")
     }
     distMatrix <- matrix(c(route))
     numeric_mask <- sapply(distMatrix, is.numeric)
     
     # Subset the distance matrix, keeping only numeric elements
     clean_distMatrix <- distMatrix[numeric_mask]
#     print(paste(clean_distMatrix))
#     print("hehehehe")
#     print(paste(route))
     tourlength <- sum(clean_distMatrix[route]) #tour length must be minimised
#     tourlength <- sum(route)
     return(tourlength)                   #however, GA package only maximises. So 1/tourlength can be maximised. 
}

tspFitness <- function(tour, ...){       #... allows passing some unspecified arguments to the function, which can be passed on further. 
     return (1/tourLength(tour, ...))    #Since the tour length must be minimsed, 1/tourlength can be maximised. 
                                         #We convert it into a maximisation problem because the GA package can only maximise. 
}

#To call this function, you must pass it on a GA produced solution. 
#For example:
#results <- runGA(problem = "tsp")
#solution <- getBestSolution()
# plotTSPSolution(solution)
plotTSPSolution<-function(solution){
#  data("eurodist", package = "datasets")
  data <- read.csv("C:/Users/Haroon Muhammad Arif/Desktop/BCU/mo/Pakistan.csv")
  mds <- cmdscale(data)
  x <- mds[, 1]
  y <- -mds[, 2]
  plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
  abline(h = pretty(range(x), 10), v = pretty(range(y), 10),
            col = "light gray")
  tour <- solution[1, ]
  tour <- c(tour, tour[1])
  n <- length(tour)
  arrows(x[tour[-n]], y[tour[-n]], x[tour[-1]], y[tour[-1]],
            length = 0.15, angle = 25, col = "steelblue", lwd = 2)
  text(x, y, labels(Distance), cex=0.8)
}

