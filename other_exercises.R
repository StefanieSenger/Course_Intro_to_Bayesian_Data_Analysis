# More exercises on statistics and probability in R

# Question: A set of elementary school student heights are normally distributed with a mean of 105 centimeters and a standard deviation of 7 centimeters.
# What proportion of student heights are between 94.5 centimeters and 115.5 centimeters?
pnorm(115.5, mean=105, sd=7) - pnorm(94.5, mean=105, sd=7)

# Question: The lifespans of seals in a particular zoo are normally distributed. The average seal lives 13.8 years; the standard deviation is 3.2 years.
# Use the empirical rule (68−95−99.7%) to estimate the probability of a seal living between 7.4 and 17 years.
(95/2)+(68/2) 
# or:
pnorm(17, mean=13.8, sd=3.2) - pnorm(7.4, mean=13.8, sd=3.2) # which returns us sligly different results, since the first one is only an approximation ...

# Question: A set of average city temperatures in September are normally distributed with a mean of 21.02∘C degrees and a standard deviation of 2∘C.
# What proportion of temperatures are between 17.02∘C and 25∘C degrees?
pnorm(25, mean=21.02, sd=2) - pnorm(17.02, mean=21.02, sd=2) 

# Question: The distribution of SAT scores of all college-bound seniors taking the SAT in 2014 was approximately normal 
# with mean μ=1497 and standard deviation σ=322. A certain summer program only admits students whose SAT scores are in the 
# top 15 percent of those who take the test in a given year. What is the minimum SAT score in 2014that meets the program’s requirements?
qnorm(0.85, mean=1497, sd=322) 

# Question: Contestants on a gameshow spin a wheel with 24 equally-sized segments. Most of those segments show different prize amounts, 
# but 2 of them are labeled "bankrupt":Suppose that a contestant is going to spin the wheel twice in a row, and the outcome of one spin doesn't affect 
# the outcome of future spins. What is the probability that NEITHER of the spins land on "bankrupt"?
22/24*22/24

# Question: Jillian surveyed students at her school and found out the following probabilities:
# P(freshman)=0.30
# P(plays instrument)=0.20
# P(freshman and instrument)=0.10
# Find the probability that a student plays an instrument given they are a freshman.
0.1/0.3

# Question: Maya is playing a trivia game with multiple choice questions. Each question has 2 correct answers among 5 answer choices.
# Maya has no idea what the answers to a certain question are, so she needs to choose two different answers at random.
# What is the probability that Maya's first guess is CORRECT and her second guess is INCORRECT?
p_a <- 2/5
p_b <- 3/4
p_a*p_b

# Question: A civics teacher asked her students to indicate whether they believed each of two headlines. 
# One headline was false and the other was true, but the students did not know this. The probability that a student selected at random 
# believed the true headline was 90% and the probability that the student believed the false headline was 82%. 
# She found that 75% of the students believed both headlines.
# Find the probability that a randomly selected person from this sample believed the true headline OR believed the false headline.
0.9+0.82-0.75

# Question: What is the maximum likelihood estimate of the mean of a normal distribution 
# if we have only one data point sampled from the distribution, and this data point has the value 3?
3

# Question: Compute the probability of obtaining a value between 1 and 5 in a normal distribution with mean 4 and standard deviation 3.
pnorm(5, mean=4, sd=3) - pnorm(1, mean=4, sd=3) 

# Question: Compute the probability of obtaining a value between 1 and 1.5 in a normal distribution with mean 1 and standard deviation 0.5.
pnorm(1.5, mean=1, sd=0.5) - pnorm(1, mean=1, sd=0.5) 

# Question: Consider participating in a lottery ten times. Each time, the probability of winning a prize is 0.10. 
# What is the probability of winning exactly 3 times?
dbinom(0:10, size=10, prob=0.1)[3]

# Question: Suppose we want to define a prior on the number of words a person types per day 
# (here, we assume that everyone types at least some words a day, not 0). 
# Which prior DOES NOT strongly violate common sense assumptions? Only one answer is correct.
# Uniform(0, 6000)
# Normal(0, 12000)
# Normal(3000, 3000)
# Normal+(5000, 20000) 
answer <- 'Normal+(5000, 20000)'
answer

