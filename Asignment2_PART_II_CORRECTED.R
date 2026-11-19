# Assignment 2 - part 2 
# Kyle, Nassi, Susane 

# randomized experiment 
# two groups: experimental + control 
# 50 participants per group, randomly assigned 

# Y : test score 
# exposure X: training on learning strategies before exam 

# Expect 
# control group: mean score of 150, sd = 15 
# experimental group: 160, sd = 15 

# goal: t.test to know the power of the difference of mean_diff = 10

#Exercise 1 
# Step 1: Understand the data 
# normal data, mean, sd 
# Step 2: Data simulation
set.seed(1111)
n <- 1000
group_size <- 50
control <- rnorm(group_size, mean = 150, sd = 15)
experimental <- rnorm(group_size, mean = 160, sd = 15)

# Step 3: p-value from the t.test: 
t.test(control, experimental)$p.value
? t.test()
# H0: no difference in means
# H1: difference in means
# p-value = 0.001017028 is significant, we can reject the null hypothesis
# the groups' means are significantly different 

# ------------ CORRECTION #1 : Create a function around the for loop with arguments n and group_size
simulate_power <- function(n = 1000, group_size = 50){
vector <- numeric(n)
t.statistic <- numeric(n)

for (i in 1:n){
  # generate data 
  # ------------ CORRECTION #2: group_size instead of n (otherwise we'd create sample sizes with 10000 obs.)
  control <- rnorm(group_size, mean = 150, sd = 15)
  experimental <- rnorm(group_size, mean = 160, sd = 15)
  # t-test 1000 times (n = 1000)
  vector[i] <- t.test(control, experimental)$p.value
  t.statistic[i] <- t.test(control, experimental)$statistic
  }
head(vector)
head(t.statistic)

# Question 2 

# Power is the probability of rejecting the null hypothesis when it is false 
# power of test = (1 minus Type II error probability)

# So our idea is to calculate: 
########## the probability of getting a significant p-value

# ------------ CORRECTION 3: power = proportion of times estimated p-value < .05
# estimate the number the times the estimated p-value was lower than .05
signif_values <- vector < 0.05 
# create an average of the vector, this means the proportion of significant p-values
power <- mean(signif_values)

list(power = power)
}

simulate_power(n = 1000, group_size = 50)
# power = .904 
power.t.test(n = 50, delta = 10, sd = 15)
# power = 0.9099633 

# Conclusion: 
# The power simulated with the Monte Carlo algorithm with 1000 repetitions (.904) 
# was very close to the power obtained with the function power.t.test (.9099). 
# Both methods suggest that with 50 participants per group, the chance of
# detecting a true effect is around 91%.


