#' Set problem
#'
#' @param data 
#' @param N
#' @param short_sell
#' @param tcr
#' @return The final data and minimum value
#' @export
set.problem <- function(data, N, short_sell = FALSE, tcr = 0){
  X <- Variable(nrow(data), integer = TRUE) # Set X_{i,j}
  P <- data$price # Set P_{i,j}
  V <- data$volume # Set V_{i,j}
  # Set Objective: Minimum P * X
  if(tcr == 0){
    objective <- Minimize(sum(P * X))
  }else{
    objective <- Minimize(sum(P * X) + tcr * sum(P * abs(X)))
  }
  # Set Constraints
  constraints <- set.constraints(data, N, X, V, short_sell = short_sell)
  # Solve this problem
  problem <- Problem(objective, constraints)
  # Return the final data and minimum value
  return(problem)
}
