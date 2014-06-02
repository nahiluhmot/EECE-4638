# Project 3: Tom Hulihan and Todd Lunter

# Use cplex instead of minos
option solver cplex;
# Set the solver's maxmium runtime to 10 minutes (600 seconds)
option cplex_options 'time=600';

# The total number of objects that may be placed into the knapsack
param n;

# The total weight that the knapsack can hold
param costBound;

# The values of each possible item
param value {0..n-1} >= 0;

# The costs of each possible item
param cost {0..n-1} >=0;

param totalCost default 0;
param totalValue default 0;

# For an item with an ID, i, isIncluded[i] is 1 if the object is in the
# knapsack, zero otherwise.
var isIncluded {0..n-1} binary;

# Maximize the total value stored in the knapsack
maximize sumOfValues: sum{i in 0..n-1} value[i] * isIncluded[i];

# Ensure that the knapsack can hold each object
subject to costBoundIsNotExceeded: sum{i in 0..n-1} cost[i] * isIncluded[i] <= costBound;

data;
include data/knapsack512.dat
solve;

# Display each item in the knapsack
print "Items in the knapsack:";
printf "ID\tCost\tValue\n";
for {i in 0..n-1} {
    # If an item was included, print it out and add its cost and value to the
    # totals
    if isIncluded[i] = 1 then {
        printf "%0s\t%d\t%d\n", i, cost[i], value[i];
        let totalCost := totalCost + cost[i];
        let totalValue := totalValue + value[i];
    }
}

# Display the costBound, totalCost, and totalValue of the knapsack
printf "\n";
display costBound;
display totalCost;
display totalValue;
