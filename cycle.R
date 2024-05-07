# Load necessary library
library(dplyr)

# Read the data
data <- read.csv("C:/Users/Haroon Muhammad Arif/Desktop/BCU/mo/Pakistan.csv")

# Define Cycle Crossover Function
cycle_crossover <- function(parent1, parent2) {
  offspring <- integer(length(parent1))
  flag <- rep(FALSE, length(parent1))
  start <- 1
  while (TRUE) {
    offspring[start] <- parent1[start]
    next_elem <- parent2[start]
    start <- match(next_elem, parent1)
    if (flag[start]) break
    flag[start] <- TRUE
  }
  offspring[offspring == 0] <- parent2[offspring == 0]
  return(offspring)
}




# Generate Initial Population
generate_population <- function(cities, size) {
  replicate(size, sample(cities), simplify = FALSE)
}

# Calculate Distance
calculate_distance <- function(route, distance_matrix) {
  sum(sapply(1:length(route), function(i) distance_matrix[route[i], route[(i %% length(route)) + 1]]))
}

# Main GA Function
run_genetic_algorithm <- function(data, pop_size, generations) {
  # Adjust column names
  cities <- unique(c(data$Origin, data$Destination))
  
  # Create distance matrix
  distance_matrix <- matrix(Inf, nrow = length(cities), ncol = length(cities), dimnames = list(cities, cities))
  
  # Populate distance matrix
  for (i in 1:nrow(data)) {
    distance_matrix[data$Origin[i], data$Destination[i]] <- data$Distance[i]
    distance_matrix[data$Destination[i], data$Origin[i]] <- data$Distance[i]
  }
  
  population <- generate_population(cities, pop_size)
  
  for (gen in 1:generations) {
    # Select parents and perform crossover
    parents <- sample(population, 2)
    offspring <- cycle_crossover(parents[[1]], parents[[2]])
    population <- c(population, list(offspring))
    
    # Evaluate and select the best individuals
    fitness <- sapply(population, calculate_distance, distance_matrix = distance_matrix)
    population <- population[order(fitness)][1:pop_size]
  }
  
  list(route = population[[which.min(fitness)]], distance = min(fitness))
}

# Run the GA
result <- run_genetic_algorithm(data, 10, 100)
print(result)






# Define the optimized route
optimized_route <- result$route

# Create a data frame with the route cities
route_coords <- data.frame(City = optimized_route)

# Reorder the cities in the route to start from Bahawalpur and end at Muzaffarabad
route_coords$City <- factor(route_coords$City, levels = rev(optimized_route))

# Get the y-coordinate positions for city labels
city_positions <- data.frame(City = optimized_route, Y = seq_along(optimized_route))

# Plot the optimized route with city labels
ggplot() +
  geom_line(data = route_coords, aes(x = 1, y = as.numeric(City), group = 1), color = "blue", size = 1) +
  geom_point(data = city_positions, aes(x = 1, y = Y), color = "red", size = 3) +
  geom_text(data = city_positions, aes(x = 1, y = Y, label = City), vjust = 1.5, hjust = 0) +
  labs(title = "Optimized Route Visualization",
       x = "", y = "City") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),  # Hide x-axis labels
        axis.ticks.x = element_blank()) # Hide x-axis ticks















