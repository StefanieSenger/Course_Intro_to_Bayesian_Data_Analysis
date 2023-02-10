# Notes from week 2 (Bayesian Data Analysis)

# computing marginal likelihood
sum(dbinom(7, size=10, prob=c(0.1,0.5,0.9)))/3
# like a weighted sum of likelihoods
# 7 out of 10 and prob can only take three possible values





# Question: "We ask a subject 10 yes/no questions, and the subject returns 0 correct answers. 
# We assume a binomial likelihood function for this data. We also assume a Beta(1, 1) 
# prior on the parameter θ, which represents the probability of success. 
# Write down the posterior distribution of the θ parameter.
prior <- dbeta(x = 0:1, shape1 = 1, shape2 = 1) # Define the prior distribution as Beta(1, 1)
likelihood <- dbinom(0, size = 10, prob = 0:1) # Define the likelihood function as a binomial distribution
post <- prior * likelihood # Calculate the posterior distribution
post <- post / sum(post) # Normalize the posterior distribution

# Question: Suppose that we want to set a prior on the theta parameter of the 
# binomial distribution and choose the beta distribution with parameters a = 19 and b =3. 
# Compute (up to three decimal places) the 95% credible interval for the prior (use R).
qbeta(c(0.025, 0.975), shape1 = 19, shape2 = 2)

# Question: Assuming a binomial likelihood, you are given the following data: 
# 18 successes out of 100 independent trials. Moreover, suppose the prior on 
# the θ parameter in the binomial distribution is a beta distribution with 
# a = 15 and b = 15. Find the kernel of the posterior distribution of θ.
prior <- dbeta(x = 0:1, shape1 = 15, shape2 = 15)
likelihood <- dbinom(18, size = 100, prob = 0:1)
post <- prior * likelihood
post
