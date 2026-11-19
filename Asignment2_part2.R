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

vector <- numeric(n)
t.statistic <- numeric(n)

for (i in 1:n){
  # generate data 
  control <- rnorm(n, mean = 150, sd = 15)
  experimental <- rnorm(n, mean = 160, sd = 15)
  # t-test 1000 times (n = 1000)
  vector[i] <- t.test(control, experimental)$p.value
  t.statistic[i] <- t.test(control, experimental)$statistic
  }
head(vector)
head(t.statistic)

# Question 2 
# probability 


# Power is the probability of not rejecting the null hypothesis when it is false 
# power of test = (1 minus Type II error probability)


# So our idea is to calculate: 
########## the probability of getting a significant p-value



# we need to figure out how to calculate this probability from this equation
# using an equation similar do this, but with the p-value probability

# in.ci.t[i] <- abs(mean(x)) < qt(.975, df = 50 - 1) * sd(x) / sqrt(50) 



power.t.test(n = 50, delta = 10, sd = 15)
# power = 0.9099633 




