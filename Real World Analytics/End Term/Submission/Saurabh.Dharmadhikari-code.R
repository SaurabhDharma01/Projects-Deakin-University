# Define the objective function
obj <- c(10, 8)

# Define the constraint matrix
mat <- matrix(c(40, 20, 40, 100, 20, 20), nrow = 3, byrow = TRUE)

# Define the right-hand side of the constraints
rhs <- c(8 * 60 * 20, 8 * 60 * 50, 8 * 60 * 14)

# Define the constraint direction
dir <- c("<=", "<=", "<=")

# Solve the LP problem
library(lpSolve)
lp <- lp("max", obj, mat, dir, rhs)

# Print the optimal solution
cat("The optimal number of shirts to produce per day is", lp$solution[1], "and the optimal number of pants to produce per day is", lp$solution[2], ".\n")

cat("The maximum profit that can be achieved is $", round(lp$objval, 2), "per day.")






# Graphical
library(ggplot2)

# Define the objective function
obj <- c(10, 8)

# Define the constraint matrix
mat <- matrix(c(40, 20, 40, 100, 20, 20), nrow = 3, byrow = TRUE)

# Define the right-hand side of the constraints
rhs <- c(8 * 60 * 20, 8 * 60 * 50, 8 * 60 * 14)

# Define the constraint direction
dir <- c("<=", "<=", "<=")

# Create a data frame for plotting
df <- data.frame(x = c(0, 200), y = c(0, 200))

# Plot constraints
ggplot(df, aes(x, y)) +
  geom_abline(slope = -mat[1, 1]/mat[1, 2], intercept = rhs[1]/mat[1, 2], linetype = "dashed", color = "red") +
  geom_abline(slope = -mat[2, 1]/mat[2, 2], intercept = rhs[2]/mat[2, 2], linetype = "dashed", color = "blue") +
  geom_abline(slope = -mat[3, 1]/mat[3, 2], intercept = rhs[3]/mat[3, 2], linetype = "dashed", color = "green") +
  
  # Shade the feasible region
  geom_polygon(aes(fill = "Feasible Region"), data = data.frame(x = c(0, 150, 180), y = c(0, 180, 0)), alpha = 0.3) +
  
  # Highlight the optimal solution
  geom_point(aes(x = 150, y = 180), color = "black", size = 3) +
  
  # Annotate the lines
  annotate("text", x = 50, y = 150, label = "40x + 20y <= 8*60*20", color = "red") +
  annotate("text", x = 50, y = 100, label = "40x + 100y <= 8*60*50", color = "blue") +
  annotate("text", x = 150, y = 30, label = "20x + 20y <= 8*60*14", color = "green") +
  
  # Set labels and title
  labs(x = "Number of Shirts", y = "Number of Pants", title = "Linear Programming Feasible Region") +
  
  # Customize plot theme
  theme_minimal()









# Define a function to perform sensitivity analysis
sensitivity_analysis <- function(profit_per_shirt_range) {
  results <- data.frame()
  
  for (profit_shirt in profit_per_shirt_range) {
    # Vary the profit per shirt coefficient
    obj <- c(profit_shirt, 8)
    
    # Solve the LP problem with the updated objective function
    lp <- lp("max", obj, mat, dir, rhs)
    
    # Store the results
    results <- rbind(results, data.frame(Profit_Per_Shirt = profit_shirt, Optimal_Profit = lp$objval))
  }
  
  return(results)
}

# Define a range for profit per shirt (adjust as needed)
profit_per_shirt_range <- seq(5, 15, by = 1)

# Perform sensitivity analysis
sensitivity_results <- sensitivity_analysis(profit_per_shirt_range)

# Print the results
print(sensitivity_results)






## Question 2.
# Load the lpSolveAPI package
library(lpSolveAPI)

# Sales prices, production costs, and purchase prices per ton
sales_prices <- c(60, 55, 60)
production_costs <- c(5, 4, 5)
purchase_prices <- c(40, 45, 30)

# Maximum demand for each product
max_demand <- c(4200, 3200, 3500)

# Minimum proportion of Cotton and Wool in each product
min_cotton_proportion <- c(0.5, 0.6, 0.5)
min_wool_proportion <- c(0.4, 0.4, 0.3)

# Decision variables (materials for each product)
materials <- c("Cotton", "Wool", "Nylon")

# Create the LP matrix
lp_matrix <- matrix(0, nrow = 10, ncol = 9)

# Objective function coefficients (Maximize profit)
lp_matrix[1, ] <- c(15, 10, 25, 11, 6, 21, 15, 10, 25)

# Minimum proportion of Cotton in each product
lp_matrix[2:4, c(1, 4, 7)] <- matrix(c(min_cotton_proportion, rep(0, 3)), ncol = 9, byrow = TRUE)

# Minimum proportion of Wool in each product
lp_matrix[5:7, c(2, 5, 8)] <- matrix(c(0, min_wool_proportion, rep(0, 3)), ncol = 9, byrow = TRUE)

# Maximum demand for each product
lp_matrix[8:10, c(1, 2, 3)] <- -diag(1, 3)

# Right-hand side vector
rhs_vector <- c(0, 0, 0, 0, 0, 0, 0, max_demand)

# Add column and row names
colnames(lp_matrix) <- c("bc", "bw", "bn", "ac", "aw", "an", "lc", "lw", "ln")
rownames(lp_matrix) <- c("Maximize", "Min. Cotton in Bloom", "Min. Cotton in Amber", "Min. Cotton in Leaf",
                         "Min. Wool in Bloom", "Min. Wool in Amber", "Min. Wool in Leaf",
                         "Max Demand of Bloom", "Max Demand of Amber", "Max Demand of Leaf")

# Print the LP matrix
print(lp_matrix)

# Print the right-hand side vector
print(rhs_vector)

# Create a linear programming model
lp_model <- make.lp(nrow = nrow(lp_matrix), ncol = ncol(lp_matrix))

# Set the objective function coefficients
set.objfn(lp_model, lp_matrix[1, ])

# Set the constraint coefficients
for (i in 2:nrow(lp_matrix)) {  # Start from the second row to skip the objective function
  set.row(lp_model, i, lp_matrix[i, ])
}

# Set the direction of the constraints ("<=" instead of ">=")
set.constr.type(lp_model, rep("<=", nrow(lp_matrix)))

# Set the right-hand side vector
set.rhs(lp_model, rhs_vector)

# Solve the linear programming problem
lp_status <- solve(lp_model)

# Extract the results
optimal_profit <- -get.objective(lp_model)
optimal_values <- get.variables(lp_model)

# Print the results
cat("Optimal Profit:", optimal_profit, "\n")
cat("Optimal Values of Decision Variables:\n", optimal_values)









#######################################################################
##################------------Q3---------------########################
#######################################################################



# Define the possible bids
bids <- c(10, 20, 30, 35, 40)

# Create the payoff matrix as a 5x5 matrix
payoff_matrix <- matrix(0, nrow = 5, ncol = 5)

# Fill in the payoffs based on the rules of the game
for (i in 1:5) {
  for (j in 1:5) {
    if (bids[i] == bids[j]) {
      payoff_matrix[i, j] <- ifelse(i == 4, 0, -5)  # Handle tie-breaking for Giant
    } else if (bids[i] > bids[j]) {
      payoff_matrix[i, j] <- ifelse(i == 5, 5, -5)  # Adjust for Giant's preference
    } else {
      payoff_matrix[i, j] <- ifelse(i == 1, -35, 40) - (bids[i] + bids[j])  # General payoffs
    }
  }
}

# Print the payoff matrix
print(payoff_matrix)

# Visualize the payoff matrix (optional)
library(ggplot2)

# Create a heatmap with labels
heatmap(payoff_matrix, Rowv = NA, Colv = NA, 
        main = "Payoff Matrix for Giant and Sky", 
        xlab = "Sky's Bid", ylab = "Giant's Bid",
        labRow = bids, labCol = bids)







# Install and load the lpSolve package
# install.packages("lpSolve")
library(lpSolve)

# Define the objective function coefficients
objective_coef <- c(-5, -5, -5, -5, -5)

# Define the constraint matrix (coefficients for the decision variables)
constraint_matrix <- matrix(c(1, 1, 1, 1, 1), nrow = 1, byrow = TRUE)

# Define the right-hand side of the constraints
rhs <- c(1)

# Set the type of the LP problem (maximization)
lp_type <- "max"

# Solve the LP problem
lp_solution <- lp(lp_type, objective_coef, constraint_matrix, "=", rhs, all.int = TRUE)

# Print the results
print(lp_solution)

# Extract and print the optimal probabilities
optimal_probabilities <- lp_solution$solution
print(optimal_probabilities)

