# Notes from week 1 (Introduction)

library(extraDistr)

# Discrete random variables for Bernoulli trials 
# (event can have only two possible outcomes, such as "success" or "failure")
rbern(n=10, prob=0.5)
dbern(0, prob=0.5)
pbern(1, prob=0.5)
qbern(1, prob=0.5)

# Discrete random variables for Binomial trails:
# (series of n Bernoulli trails)
rbinom(n=10, size=1, prob=0.5) # making data
dbinom(0:10, size=10, prob=0.5) # single event probabilities for an event ocurring
pbinom(0:10, size=10, prob=0.5) # cumilative probabilities for an event ocurring
props <-pbinom(0:10, size=10, prob=0.5) # saving the cumulated probabilities
qbinom(props, size=10, prob=0.5) # what's the quantile q such that p=p?

# Continious random variables (more specifically normal distribution for our purpose)
rnorm(5, mean=0, sd=1) # generating 5 datapoints from the standard normal distribution
pnorm(2) # probability of observing 2 or less
pnorm(2, lower.tail = FALSE) # probability of observing 2 or larger
qnorm(0.9772499) # what's the quantile so that area under curve left to it is less than 0.9772499
dnorm(2) # density at a certain point

# Bivariate distribution (joint distribution of two variables)
Sigma <- matrix(c(5^2, 5*10*0.6, 5*10*0.6, 10^2), nrow = 2, ncol = 2) # defining a variance-covaricance matrix
u <- MASS::mvrnorm(n=100, mu=c(0,0), Sigma=Sigma ) # generating data
head(u, n=3) # plotting first n rows





# Exercises from week 1 (Introduction)

# Question: Consider participating in a lottery ten times. Each time the probability of winning 
# a prize is 0.10. What is the probability of winning exactly 5 times?
dbinom(0:10, size=10, prob=0.1)[5]

# Question: Consider lending 10 books from a library. The probability of getting a damaged book 
# is 0.15. Compute the cumulative probability of having 2 or fewer damaged books rounded 
# to three digits.
round(pbinom(2, size=10, prob=0.15),3)

# Question: Given a standard normal distribution, what is the probability of getting a value 
# lower than -3?
pnorm(-3)

# Question: Given a standard normal distribution, what is the probability of getting a value 
# higher than -3?
pnorm(-3, lower.tail = FALSE)

# Question: Compute the probability of obtaining a value less than or equal to 5 in a normal distribution 
# with mean 3 and standard deviation 1. 
pnorm(5, mean=3)

# Question: Compute the probability of obtaining a value less than or equal to 5 in a normal distribution 
# with mean 3 and standard deviation 3.
pnorm(5, mean=3, sd=3)

# Question: Compute the probability of obtaining a value more than 5 in a normal distribution 
# with mean 3 and standard deviation 1.
pnorm(5, mean=3, lower.tail = FALSE)

# Question: Compute the probability of obtaining a value more than or equal to 5 
# in a normal distribution with mean 4 and standard deviation 3.
pnorm(5, mean=4, sd=3, lower.tail = FALSE)

# Question: Compute the probability of obtaining a value between 2 and 5 
# in a normal distribution with mean 3 and standard deviation 1.
pnorm(5, mean=3) - pnorm(2, mean=3)

# Question: Compute the probability of obtaining a value between 1 and 1.5 
# in a normal distribution with mean 3 and standard deviation 1.
pnorm(1.5, mean=3) - pnorm(1, mean=3)

# Question: Compute the probability of obtaining a value between 1 and 1.5 
# in a standard normal distribution.
pnorm(1.5) - pnorm(1)

# Question: Compute the probability of obtaining 4 or fewer successes in 15 trials 
# following a binomial distribution with probability of success being 0.5.
pbinom(4, size=15, prob=0.5)

# Question: Compute the probability of obtaining 3, 4, or 5 successes in 15 trials 
# following a binomial distribution with probability of success being 0.5.
pbinom(5, size=15, prob=0.5) - pbinom(2, size=15, prob=0.5)

# Question: Compute the probability of obtaining 2, 3, 4, or 5 successes in 25 trials 
# following a binomial distribution with probability of success being 0.25.
pbinom(5, size=25, prob=0.25) - pbinom(1, size=25, prob=0.25)

# Question: Consider participating in a lottery ten times. Each time the probability 
# of winning a prize is 0.05. Compute the cumulative probability of winning 
# 2 times or less rounded to three digits.
round(pbinom(2, size=10, prob=0.05),3)

# Question: Compute the value k such that the probability of obtaining k 
# or fewer successes in 15 binomial samples with probability of success being 0.5 is 0.75.
qbinom(0.75, size=15, prob=0.5)

# Question: Compute the value k such that the probability of obtaining k or fewer successes 
# in 55 binomial samples with probability of success being 0.75 is 0.25.
qbinom(0.25, size=55, prob=0.75)

# Question: What is the quantile q in a standard normal distribution, such that the probability 
# of observing that value q or a value less than q is 0.25?
qnorm(0.25) 

# Question: Consider a normal distribution with mean 1 and standard deviation 1. 
# Compute the boundary such that the area (the probability) to the left of it is 0.10.
qnorm(0.1, mean=1) 

# Question: What is the probability of obtaining 6 successes in 18 trials 
# following the binomial distribution with probability of success being 0.65?
dbinom(6, size=18, prob=0.65)

# Question: What is the probability of obtaining 17 successes in 18 trials 
# following the binomial distribution with probability of success being 0.75?
dbinom(17, size=18, prob=0.75)

# Question: What is the probability of obtaining 6 successes in 60 trials 
# following the binomial distribution with probability of success being 0.1?
dbinom(6, size=60, prob=0.1)

# Question: Given a normal distribution with mean 650 and standard deviation 125, 
# there exist two quantiles, the lower quantile q1 and the upper quantile q2, 
# that are equidistant from the mean, such that the area under the curve 
# between q1 and q2 is 80%. Find q1 and q2.
qnorm(0.1, mean=650, sd=125)
qnorm(0.9, mean=650, sd=125)