set.seed(123)  
# Install necessary packages if not already installed
if (!requireNamespace("GA", quietly = TRUE)) install.packages("GA")
if (!requireNamespace("data.table", quietly = TRUE)) install.packages("data.table")

# Load the packages
library(GA)
library(data.table)

# Load data from your specified file path (adjust path as per actual use)
data_path <- "C:/Users/Haroon Muhammad Arif/Desktop/BCU/mo/Pakistan.csv"
data <- fread(data_path)

# Assuming data has columns 'Origin', 'Destination', and 'Distance'
# Create a symmetric distance matrix
cities <- unique(c(data$Origin, data$Destination))
distance_matrix <- matrix(Inf, nrow = length(cities), ncol = length(cities))
rownames(distance_matrix) <- cities
colnames(distance_matrix) <- cities

for (i in 1:nrow(data)) {
  origin <- data$Origin[i]
  destination <- data$Destination[i]
  dist <- data$Distance[i]
  distance_matrix[origin, destination] <- dist
  distance_matrix[destination, origin] <- dist  # Assume distance is symmetric
}

# Diagonal should be zero
diag(distance_matrix) <- 0



# Define the TSP Fitness Function
tspFitness <- function(tour, distMatrix) {
  # Ensure that the tour is valid and calculate the distance
  tour_length <- sum(distMatrix[cbind(tour, c(tour[-1], tour[1]))])
  return(-tour_length)  # Since GA maximizes, we need to minimize the route distance
}

# Setup and run the genetic algorithm
result <- ga(type = "permutation", 
             fitness = function(tour) tspFitness(tour, distance_matrix), 
             lower = 1, upper = length(cities), 
             popSize = 50, maxiter = 500, run = 100)

# Print the results
cat("Shortest Route Distance: ", -min(result@fitnessValue), "km\n")
cat("Optimal Tour: ", cities[result@solution], "\n")





install.packages("ggplot2")
install.packages("ggmap")
install.packages("dplyr")
library(ggplot2)
library(ggmap)
library(dplyr)

# Create a data frame with cities and their coordinates
city_coords <- data.frame(
  city = c("Islamabad", "Sialkot", "Peshawar", "Muzaffarabad", "Quetta", "Karachi", 
           "Gawadar", "Hyderabad", "Bahawalpur", "Multan", "Faisalabad", "Sargodha", 
           "Gujrawala", "Lahore"),
  latitude = c(33.6844, 32.4972, 34.0150, 34.3700, 30.1798, 24.8607, 
               25.1216, 25.3960, 29.3959, 30.1575, 31.4187, 32.0836, 
               32.1617, 31.5497),
  longitude = c(73.0479, 74.5361, 71.5825, 73.4711, 66.9750, 67.0011,
                62.3254, 68.3737, 71.6833, 71.5259, 73.0791, 72.6747,
                74.2179, 74.3436)
)

# Arrange the coordinates according to the TSP solution route order you obtained
cities_ordered <- c("Islamabad", "Sialkot", "Peshawar", "Muzaffarabad", "Quetta", "Karachi", 
                    "Gawadar", "Hyderabad", "Bahawalpur", "Multan", "Faisalabad", "Sargodha", 
                    "Gujrawala", "Lahore")

route_data <- city_coords[match(cities_ordered, city_coords$city),]






library(ggplot2)

# Create the plot
ggplot(data = route_data, aes(x = longitude, y = latitude)) +
  geom_point(aes(color = city), size = 3) +  # Plot points for each city
  geom_text(aes(label = city), hjust = 1.2, vjust = 1.2) +  # Add city names
  geom_path(aes(group = 1), color = "red", size = 1) +  # Draw paths between cities
  ggtitle("TSP Route Through Major Cities in Pakistan") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide legend for cleanliness





