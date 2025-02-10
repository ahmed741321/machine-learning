# Defining the variables x and y without using a matrix
x <- c(179, 173, 181, 170, 158, 174, 172, 166, 194, 185, 
       162, 187, 198, 177, 178, 165, 154, 183, 166, 171, 
       175, 182, 167, 169, 172, 186, 172, 176, 168, 187)

y <- c(85, 65, 71, 65, 51, 66, 62, 60, 90, 75, 
       55, 78, 109, 61, 70, 58, 50, 93, 51, 65, 
       70, 60, 59, 62, 70, 71, 54, 68, 67, 80)

# Sorting x and y
y <- sort(y)
x <- sort(x)
points <- data.frame(x = x, y = y)

# Plotting the points with custom axis formatting
plot(points, main = "Scatter Plot of Given Points", 
     xlab = "X Values", ylab = "Y Values", pch = 19, col = "blue", 
     xaxt = 'n')  # Hiding default x-axis

# Adding only the x values to the x-axis
axis(1, at = x, labels = x)

# Adding a grid
grid()

# Calculating the means
mean_x <- mean(x)
mean_y <- mean(y)

# Calculating the variance of x and y
var_x <- var(x)
var_y <- var(y)

# Calculating the covariance between x and y
cov_xy <- cov(x, y)

# Calculating the number of elements in x and y
n_X <- length(x)  # Number of elements in x
n_Y <- length(y)  # Number of elements in y

# Printing the results
cat("Mean of X:", mean_x, "\n")
cat("Mean of Y:", mean_y, "\n")
cat("Variance of X:", var_x, "\n")
cat("Variance of Y:", var_y, "\n")
cat("Covariance of X and Y:", cov_xy, "\n")
cat("n(X):", n_X, "\n")
cat("n(Y):", n_Y, "\n")

# Calculating the expressions for updating y and x
cov_xy_var_x_expr <- cov_xy / var_x
cov_xy_var_y_expr <- cov_xy / var_y

# Creating update expressions for y and x
y_update_expr <- sprintf("%f + %f * (x - %f)", mean_y, cov_xy / var_x, mean_x)
x_update_expr <- sprintf("%f + %f * (y - %f)", mean_x, cov_xy / var_y, mean_y)

# Printing the update expressions
cat("Updated Y expression:", y_update_expr, "\n")
cat("Updated X expression:", x_update_expr, "\n")
cat("Cov_xy / Var_x expression:", cov_xy_var_x_expr, "\n")
cat("Cov_xy / Var_y expression:", cov_xy_var_y_expr, "\n")

# Calculating the line for updated y expression
y_update_expr_line <- mean_y + cov_xy_var_x_expr * (x - mean_x)
points <- data.frame(x, y_update_expr_line)

# Adding the updated y expression line (red)
lines(points$x, y_update_expr_line, col = "red", lwd = 2)

# Calculating the line for updated x expression
x_update_expr_line <- mean_x + cov_xy_var_y_expr * (y - mean_y)
points <- data.frame(y, x_update_expr_line)

# Adding the updated x expression line (green)
lines(x_update_expr_line, points$y, col = "green", lwd = 2)

# Plotting the mean point (black)
points(mean_x, mean_y, col = "black", pch = 19, cex = 1.5)
