# expost_R_package

## Introduction

This R package is part of our group project in *MFIT 5008 Decision
Analytics for FinTech* course.

In our project, we propose a novel approach to derive optimal trading strategies within a day based on completed trade data. 

In this package, we identify the functional relationship between the optimal
strategy’s profitability and the maximum position.

## Problem Design

In order to obtain the optimal strategy for a stock within a single day retrospectively, we need to determine the trading direction and trading volume for each price at every moment. 

Here are the key variables involved:

1.	Maximum Position ($N_i$): $N_i$ represents the maximum number of shares to hold, with $N_1=N$ being a value we set.

2.	Transaction Cost Ratio ($tcr$): Commissions can be expressed as a percentage, such as 0.05.

3.	Price Sequence ($P_{i,j}$): The $j_{th}$ actual price at time $i$.
 
4.	Volume Sequence ($V_{i,j}$): The actual trade volume corresponding to the price $P_{i,j}$.

5.	Trade Volume ($X_{i,j}$): The trade volume in our strategies at price $P_{i,j}$. Positive values of $X_{i,j}$ indicate purchases of $X_{i,j}$ shares, while negative values indicate sales of $X_{i,j}$ shares.

To use this package, you can install it from GitHub using the following code:

```{r}
library(devtools)
install_github("JohnnyZhou1013/expost_R_package")
library(expost)
```

The main function in this package is `set.problem()`. It takes in a data frame of completed trade data and returns the final data and minimum value. The data frame should have the following columns: `time`, `price`, `volume`.

Then the problem can be solved using `solve.problem()` and using the `solve()` function from the CVXR package. 

