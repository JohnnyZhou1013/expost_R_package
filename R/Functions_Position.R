#' Set problem
#'
#' @param data 1
#' @param N 1
#' @param short_sell 1
#' @param tcr 1
#' @return The final data and minimum value
#' @export
set.problem <- function(data, N, short_sell = FALSE, tcr = 0){
  X <- Variable(nrow(data), integer = TRUE) # Set X_{i,j}
  P <- data$price # Set P_{i,j}
  V <- data$volume # Set V_{i,j}
  # Set Objective: Minimum P * X
  if(tcr==0){
    objective <- Minimize(sum(P * X))
  }else{
    objective <- Minimize(sum(P * X) + sum(P * abs(X)) * tcr)
  }
  # Set Constraints
  constraints <- set.constraints(data, N, X, V, short_sell = short_sell)
  # Solve this problem
  problem <- Problem(objective, constraints)
  # Return the final data and minimum value
  return(problem)
}

#' Set all constraints
#'
#' Description
#' @param data 1
#' @param N 1
#' @param X 1
#' @param V 1
#' @param short_sell 1
#' @return All constraints
#' @export
set.constraints <- function(data, N, X, V, short_sell = short_sell){
  # Set Constraints
  constraints <- list()
  
  price_n <- data %>%
    # Type of time stamp
    group_by(time) %>%
    # For every time stamp, number of transaction prices
    summarize(price_n = n()) %>% 
    ungroup()
  
  times <- price_n$time
  prices <- price_n$price_n
  
  # Set constraint 1
  if(short_sell == FALSE){
    constraints <- set.constraint1(constraints, times, prices, N, X)
  }else{
    constraints <- set.constraint4(constraints, times, prices, N, X)
  }
  # Set constraint 2
  constraints <- set.constraint2(constraints, X)
  # Set constraint 3
  constraints <- set.constraint3(constraints, X, V)
  # Return the constraints
  return(constraints)
}

#' Position limits and Short-Selling constraints
#'
#' Description
#' @param constraints 1
#' @param times 1
#' @param prices 1
#' @param N 1
#' @param X 1
#' @return constraint1
#' @export
set.constraint1 <- function(constraints, times, prices, N, X){
  # Constraint 1: N Limitation & Selling Limitation
  for(i in 1:length(times)){
    if(i == 1){
      # When i=1, new position < N
      sp <- 1
      ep <- prices[1]
      constraints <- c(constraints,
                       # Position Limits
                       abs(sum(X[sp:ep])) <= N, 
                       # Selling Limitation
                       -sum(X[sp:ep]) <= 0)
    }else{
      # When i>1, new position < N - all of the previous positions
      sp <- sum(prices[1:i-1]) + 1 # Start Point
      ep <- sp + prices[i] - 1 # End Point
      sp_ <- sp - 1 # the Point before the start point
      constraints <- c(constraints,
                       # Capital Limitation
                       abs(sum(X[sp:ep])) <= N - sum(X[1:sp_]),
                       # Selling Limitation
                       -sum(X[sp:ep]) <= sum(X[1:sp_]))
    }
  }
  return(constraints)
}

#' Zero-Position Assumption
#'
#' Description
#' @param constraints 1
#' @param X 1
#' @return constraint2
#' @export
set.constraint2 <- function(constraints, X){
  # Constraint 2: Final Position = 0
  constraints <- c(constraints, sum(X) == 0)
  
  return(constraints)
}

#' Market Liquidity Assumption
#'
#' Description
#' @param constraints 1
#' @param X 1
#' @param V 1
#' @return constraint3
#' @export
set.constraint3 <- function(constraints, X, V){
  # Constraint 3: The absolute value of X should be less than V
  ## |X_{i,j}| <= |V_{i,j}|
  constraints <- c(constraints, abs(X) <= V)
  return(constraints)
}

#' Modified constraint for constraint1.
#'
#' Description
#' @param constraints 1
#' @param times 1
#' @param prices 1
#' @param N 1
#' @param X 1
#' @return constraint4
#' @export
set.constraint4 <- function(constraints, times, prices, N, X){
  # Constraint 4: Capital Limitation under Short Selling
  for(i in 1:length(times)){
    if(i == 1){
      # When i = 1
      sp <- 1
      ep <- prices[1]
      constraints <- c(constraints,
                       # Position Limits
                       abs(sum(X[sp:ep])) <= N)
    }else{
      # When i > 1, sum(i) > - sum(1:i-1)
      sp <- sum(prices[1:i-1]) + 1 # Start Point
      ep <- sp + prices[i] - 1 # End Point
      sp_ <- sp - 1
      constraints <- c(constraints,
                       # Position Limits
                       abs(sum(X[1:ep])) <= N)
    }
  }
  return(constraints)
}
