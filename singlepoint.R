# Corrected single-point crossover function with a repair mechanism
single_point_crossover_corrected <- function(parent1, parent2) {
  # Select a random crossover point
  crossover_point <- sample(1:(length(parent1) - 1), 1)
  
  # Perform initial crossover
  temp_child1 <- c(parent1[1:crossover_point], parent2[(crossover_point + 1):length(parent2)])
  temp_child2 <- c(parent2[1:crossover_point], parent1[(crossover_point + 1):length(parent1)])
  
  # Repair mechanism to remove duplicates and add missing cities
  repair_child <- function(child, full_set) {
    missing <- setdiff(full_set, child)
    duplicate <- child[duplicated(child)]
    for (dup in duplicate) {
      idx <- which(child == dup)[1]  # Only replace the first occurrence of the duplicate
      child[idx] <- missing[1]       # Replace with the first missing city
      missing <- missing[-1]         # Remove the used city from missing
    }
    return(child)
  }
  
  # Apply the repair mechanism
  child1 <- repair_child(temp_child1, parent1)
  child2 <- repair_child(temp_child2, parent2)
  
  return(list(child1, child2))
}

# Example usage of the corrected crossover function
parent1 <- sample(1:14)
parent2 <- sample(1:14)

# Perform corrected single-point crossover
children_corrected <- single_point_crossover_corrected(parent1, parent2)

# Print the children
cat("Parent 1:", parent1, "\n")
cat("Parent 2:", parent2, "\n")
cat("Child 1:", children_corrected[[1]], "\n")
cat("Child 2:", children_corrected[[2]], "\n")






# Define the vector of city names
cities <- c("Lahore", "Islamabad", "Karachi", "Faisalabad", "Peshawar", 
            "Quetta", "Muzaffarabad", "Multan", "Hyderabad", "Gujranwala", 
            "Bahawalpur", "Sargodha", "Sialkot", "Gwadar")

# Indices from your output (indices have been adjusted to be zero-based for R)
parent1_indices <- c(7, 2, 11, 8, 12, 3, 13, 1, 4, 5, 14, 10, 9, 6) - 1
parent2_indices <- c(8, 6, 11, 3, 1, 12, 9, 10, 5, 4, 13, 14, 7, 2) - 1
child1_indices <- c(9, 6, 11, 8, 12, 3, 10, 1, 4, 5, 13, 14, 7, 2) - 1
child2_indices <- c(8, 2, 11, 3, 1, 12, 7, 13, 5, 4, 14, 10, 9, 6) - 1

# Convert indices to city names
parent1_route <- cities[parent1_indices + 1]
parent2_route <- cities[parent2_indices + 1]
child1_route <- cities[child1_indices + 1]
child2_route <- cities[child2_indices + 1]

# Display the routes
cat("Parent 1 Route:", parent1_route, "\n")
cat("Parent 2 Route:", parent2_route, "\n")
cat("Child 1 Route:", child1_route, "\n")
cat("Child 2 Route:", child2_route, "\n")

###

library(ggplot2)

# Define the cities
cities <- c("Lahore", "Islamabad", "Karachi", "Faisalabad", "Peshawar", 
            "Quetta", "Muzaffarabad", "Multan", "Hyderabad", "Gujranwala", 
            "Bahawalpur", "Sargodha", "Sialkot", "Gwadar")

# Convert indices to city names for the routes
parent1_route <- cities[c(11, 6, 4, 2, 13, 9, 7, 8, 5, 3, 14, 1, 10, 12)]
parent2_route <- cities[c(1, 12, 9, 7, 3, 6, 13, 5, 4, 14, 10, 11, 2, 8)]
child1_route <- cities[c(1, 6, 4, 10, 13, 9, 7, 12, 5, 3, 14, 11, 2, 8)]
child2_route <- cities[c(11, 8, 9, 7, 3, 6, 13, 5, 4, 14, 2, 1, 10, 12)]

