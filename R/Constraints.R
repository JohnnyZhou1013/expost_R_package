#' Position limits and Short-Selling constraints
#'
#' Description
#' @param constraints
#' @param times
#' @param prices
#' @param N
#' @param X
#' @return constraint1
#' @export
set.constraint1 <- function(constraints, times, prices, N, X){
  # Position limits and Short-Selling constraints
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
                       # Position Limits
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
#' @param constraints
#' @param X
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
#' @param constraints
#' @param X
#' @param V
#' @return constraint3
#' @export
set.constraint3 <- function(constraints, X, V){
  # Constraint 3: The absolute value of X should be less than V
  ## |X_{i,j}| <= V_{i,j}
  constraints <- c(constraints, abs(X) <= V)
  return(constraints)
}

#' Modified constraint of constraint1.
#'
#' Description
#' @param constraints
#' @param times
#' @param prices
#' @param N
#' @param X
#' @return constraint4
#' @export
set.constraint4 <- function(constraints, times, prices, N, X){
  # Constraint 4: Position Limitation under Short Selling
  for(i in 1:length(times)){
    if(i == 1){
      # When i = 1
      sp <- 1
      ep <- prices[1]
      constraints <- c(constraints, abs(sum(X[sp:ep])) <= N)
    }else{
      # When i > 1, sum(i) > - sum(1:i-1)
      sp <- sum(prices[1:i-1]) + 1 # Start Point
      ep <- sp + prices[i] - 1 # End Point
      sp_ <- sp - 1
      constraints <- c(constraints,
                       # Position Limitation
                       abs(sum(X[sp:ep])) <= N - abs(sum(X[1:sp_])))
    }
  }
  return(constraints)
}

#' Set all constraints
#'
#' Description
#' @param data
#' @param N
#' @param X
#' @param V
#' @param short_sell
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
    constraints <- set.constraint4(constraints, times, prices, N, X, ssp = ssp)
  }
  # Set constraint 2
  constraints <- set.constraint2(constraints, X)
  # Set constraint 3
  constraints <- set.constraint3(constraints, X, V)
  # Return the constraints
  return(constraints)
}
