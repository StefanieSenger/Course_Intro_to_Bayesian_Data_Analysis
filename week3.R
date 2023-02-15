# Notes from week 3 (Computational Bayesian Data Analysis)

# drawing inferences from posterior distribitions analytically (in this example within 95% credible interval)
qgamma(c(0.025, 0.975), shape=20, rate=7) # shape and rate are the a and b parameter from the gamma distribution

# alternative: we can also sample from the posterior distribution and thus derive the same conclusions:
lambda_posteror <- rgamma(4000, shape=20, rate=7) # saving 4000 sampled values from a gamma distribution with a=20 and b=7 in lambda_posteror
lambda_posteror
quantile(lambda_posteror, probs = c(0.025, 0.975)) # finding the values at the quantiles 0.025, 0.975





# Example 1: simple linear model with normal distribution as a prior

# loading data on reaction times (rt), only one feature
library(bcogsci)
data("df_spacebar")
head(df_spacebar, n=2) # displaying head of df

# for comparision: frequentist linear model
m <-lm(rt~1, df_spacebar)
coef(m) # mean of intercept in form of a maximum likelihood estimate
sigma(m) # mean residual error in form of a maximum likelihood estimate
mean(df_spacebar$rt) # returns exaclty the same numer as coef(m)
sd(df_spacebar$rt) # returns exaclty the same numer as sigma(m)


# in bayesian statistics however, the mean intercept and the mean residual error are not unknown values, these have prior distributions

# starting with unrealistic priors from a uniform distribution
library(brms)
fit_press <- brm(rt~1, 
                 data=df_spacebar, 
                 family=gaussian(), # likelihood we're assuming for the data shall be the gaussian likelihood (normal distribution)
                 prior=c(
                   prior(uniform(0,60000), class=Intercept, lb=0, ub=60000), # assuming mu (mean) comes from a prior uniform distribution between 0 and 60000
                   prior(uniform(0,2000), class=sigma, lb=0, ub=2000) # assuming sigma (sd) comes from a prior uniform distribution between 0 and 2000
                   ),
                 chains= 4, # number of independent runs for sampling, default is 4
                 iter=2000, # rumber of iterations that the sampler makes to sample from the posterior distribution, default is 2000
                 warmup=1000 # number of iterations from the start of sampling that are eventually discarded, default is 1000 (half of iter)
                 )

# plotting posterior distributions of mu and sigma (and the chains)
plot(fit_press)

# extracting values for mu and sigma from tables of trials
library(bayesplot)
library(magrittr) # for interpreting the "%>%" operation
as_draws_df(fit_press) %>% head(3) # summary from 3 interations, 1st chain
as_draws_df(fit_press)$b_Intercept %>% mean() # mean of the Intercepts from all iterations
as_draws_df(fit_press)$b_Intercept %>%  quantile(c(0.025, .975)) # confidence interval for 95% confidence
as_draws_df(fit_press)$sigma %>% mean() # mean of the Intercepts from all iterations
as_draws_df(fit_press)$sigma %>%  quantile(c(0.025, .975)) # confidence interval for 95% confidence

# generating prior predictive distribution
# once we have defined a bayesian model (containing the traditional model AND the prior beliefs), we can already generate a prior predictive distribution from it:
mu <- runif(1, min=0, max=60000) # sample from prior distribution from mu
sigma <- runif(1,0,2000) # sample from prior distribution from sigma
y_pred_1 <- rnorm(n=5, mu, sigma) # plugging those samples into a PDF/PMF to generate a data set y_pred
y_pred_1 # each sample is a potential data set

# sensitivity analysis:
# the predictions from the priors are not generally reasonable
# but predictions from priors can be used to evaluate the quality of the prior
# we should take priors from previous research or (if not available) from intuition
# informative priors are usually specific compared to ininformative priors
# trying different priors and comparing the results is called a sensitivity analysis

# the posterior is a compromise between the prior and the likelihood from the current data
# the more informative a prior is, the more it will determine the posterior
# the sparser the new data ism the more the prior will determinate the posterior
# if we have lots of new data, then the likelihood will dominate the posterior

# posterior distributions
# the posterior predictive distribution is a collection of data sets generated from the model
pp_check(fit_press, ndraws=100, type="dens_overlay") # plots posterior distributions given the observed data





# Example 2: simple linear model with log-normal distribution as a prior

# log-normal-distribution
# if y is log-normally distributed, it means that log(y) is normally distributed
# generating data from a lognormal:
mu <- 6
sigma <- 0.5
N <- 500000
sl <- rlnorm(N, mu, sigma)
sl

# attention: if we change our model into lnorm format, we also need to transform our priors into logarithmic scale (by scaling down the params)

# generating prior predictive distributions
library(tidyverse) # for function "mutate"
N_samples <- 1000
N_obs <- nrow(df_spacebar)
mu_samples <- runif(N_samples, 0, 11) # scaled down from 60000 to logarithmic scale
sigma_samples <- runif(N_samples, 0, 1) # scaled down from 2000 to logarithmic scale
prior_pred_ln <- normal_predictive_distribution(
  mu_samples = mu_samples,
  sigma_samples = sigma_samples,
  N_obs = N_obs
) %>% mutate(rt_pred = exp(rt_pred)) # those priors are unreasobale

# prior predictive distribution
fit_prior_press_ln <- brm(rt~1,
                          data = df_spacebar,
                          family = lognormal(),
                          prior = c(
                            prior(normal(6,1.5), class = Intercept),
                            prior(normal(0,1), class = sigma)
                          ),
                          sample_prior = "only", # do not consider data, only sample from prior
                          control = list(adapt_delta = .9)
                          )

# fitting model to data
fit_press_ln <- brm(rt~1,
                          data = df_spacebar,
                          family = lognormal(),
                          prior = c(
                            prior(normal(6,1.5), class = Intercept),
                            prior(normal(0,1), class = sigma)
                            )
                          )
fit_press_ln # prints out some summary statistics

# getting mean mu and sd
as_draws_df(fit_press)$b_Intercept %>% mean() # mean of the Intercepts from all iterations
as_draws_df(fit_press)$sigma %>% mean()

# back-transforming to milliseconds
estimate_ms <- exp(as_draws_df(fit_press_ln)$b_Intercept) # exponent of all the samples for intercept
c(mean = mean(estimate_ms), quantile(estimate_ms, probs = c(.025, .975)))




# Exercise from week 3 (Computational Bayesian Data Analysis)

# Question: Fit the model fit_press with just a few iterations, say 50 iterations 
# (set warmup to the default of 25, and use four chains). Does the model converge?
fit_press <- brm(rt~1, 
                 data=df_spacebar, 
                 family=gaussian(), # likelihood we're assuming for the data shall be the gaussian likelihood (normal distribution)
                 prior=c(
                   prior(uniform(0,60000), class=Intercept, lb=0, ub=60000), # assuming mu (mean) comes from a prior uniform distribution between 0 and 60000
                   prior(uniform(0,2000), class=sigma, lb=0, ub=2000) # assuming sigma (sd) comes from a prior uniform distribution between 0 and 2000
                 ),
                 chains= 4, # number of independent runs for sampling, default is 4
                 iter=50, # rumber of iterations that the sampler makes to sample from the posterior distribution, default is 2000
                 warmup=25 # number of iterations from the start of sampling that are eventually discarded, default is 1000 (half of iter)
                )
plot(fit_press)
as_draws_df(fit_press) %>% head(3) # summary from 3 interations, 1st chain
as_draws_df(fit_press)$b_Intercept %>% mean() # mean of the Intercepts from all iterations
as_draws_df(fit_press)$b_Intercept %>%  quantile(c(0.025, .975)) # confidence interval for 95% confidence
as_draws_df(fit_press)$sigma %>% mean() # mean of the Intercepts from all iterations
as_draws_df(fit_press)$sigma %>%  quantile(c(0.025, .975)) 
# the values within the confidence interval are so large that I will deduce, that the model did non converge
