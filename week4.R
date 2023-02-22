# Notes from week 3 (Bayesian regression and hierarchical models)




# Example 1: experiment that involves measuring peoples pupil sizes to figure when 
# they are stressed

# our model: p_size = Normal(a + c_load * b, sigma), where c_load stands for the centered load column of the df
# so, mean is substituted by a linear function

# first from previous research we define a prior for the intercept a
data('df_pupil_pilot')
df_pupil_pilot$p_size %>% summary()

# based on this let's define the following prior for a:
qnorm(c(.025, .975), mean=1000, sd=500) # extracting qualtiles

# for sigma, we use in uninformative prior:
extraDistr::qtnorm(c(.025, .975), mean=0, sd=1000, a=0) # extracting qualtiles

# for b, which will measure the effect of c_load on the pupil size:
qnorm(c(.025, .975), mean=0, sd=100) # extracting qualtiles
# with this prior we would be equally open to the possibilities that stress would
# decrease or increase pupil sizes

# centering the predictor
library(dplyr) # for the mutate function
data('df_pupil')
(df_pupil <- df_pupil %>% mutate(c_load = load - mean(load))) # mutate creates a new collumn "c_load"

# modeling pupil size as a function of an intercept and a slope:
library(brms)
fit_pupil <- brm(p_size ~ 1 + c_load,
                 data = df_pupil,
                 family = gaussian (),
                 prior = c(
                   prior(normal(1000, 500), class=Intercept),
                   prior(normal(0, 1000), class=sigma),
                   prior(normal(0, 100), class=b, coef=c_load)
                 ))

plot(fit_pupil)
# intercept is telling us the average pupil size
# b is our slope and tells us, when we inclease the stress, how much does the pupil size change
# sigma is the variation between separate samples

# provides a table with confidence intervals for all the params:
library(rstanarm) # rstanarm not installable here
short_summary(fit_pupil)





# Example 2: effect of trial_id on spacebar pressing times (later experiments have higher ids 
# and we want to discover the relationship)

# centering data round mean of trail_id (here only called trial)
library(bcogsci)
data("df_spacebar")
df_spacebar <- df_spacebar %>% mutate(c_trial = trial - mean(trial))
df_spacebar

# now we assume that the response times will be log-normally distributed:
# model: reaction_times = LogNormal(a + c_trial * b, sigma)

# the priors need to be defined on the log scale:
# a ~ Normal(6, 1.5)
# sigma ~ Normal+(0, 1) # sigma always needs to be truncated to the positive values, this Normal+
# b ~ Normal(0, 1) # quite uninformative, but since it's on the log scale, allows for lot of variability

# modifying a the rt column, so that's all 1s (because we want to priors to depend on only the model, not the data this time)
df_spacebar_ref <- df_spacebar %>% mutate(rt = rep(1, n()))
df_spacebar_ref # dummy dataset

# fitting model
library(brms)
fit_prior_press_trail <- brm(rt ~ 1 + c_trial,
                 data = df_spacebar_ref,
                 family = lognormal (),
                 prior = c(
                   prior(normal(6, 1.5), class=Intercept),
                   prior(normal(0, 1), class=sigma),
                   prior(normal(0, 1), class=b, coef=c_trial)
                 ),
                 sample_prior='only', # ignores any data from df_spacebar_ref
                 control=list(adapt_delta = 0.9)
                 )

# so the goal here is to produce prior predictive data in order to check if our priors are reasonable

# looking at statistics from the prior predictive distributions:

median_diff <- function(x){median(x-lag(x), na.rm=TRUE)}
library(ggplot2) # for method "coord_cartesian"
pp_check(fit_prior_press_trail,
         type="stat",
         stat="median_diff",
         prefix="ppd", # show only prior predictive distributions
         binwidth=500) +
coord_cartesian(ylim=c(0,50)) + theme_bw() # binwidth and cut top of plot



# let's redefine our b to be in a more reasonable range:
# b ~ Normal(0, 0.01)

# refitting model for predictive prior
fit_prior_press_trail <- brm(rt ~ 1 + c_trial,
                             data = df_spacebar_ref,
                             family = lognormal (),
                             prior = c(
                               prior(normal(6, 1.5), class=Intercept),
                               prior(normal(0, 1), class=sigma),
                               prior(normal(0, .01), class=b, coef=c_trial)
                             ),
                             sample_prior='only', # ignores any data from df_spacebar_ref
                             control=list(adapt_delta = 0.9)
)

# fitting model on new data
fit_press_trail <- brm(rt ~ 1 + c_trial,
                             data = df_spacebar,
                             family = lognormal(),
                             prior = c(
                               prior(normal(6, 1.5), class=Intercept),
                               prior(normal(0, 1), class=sigma),
                               prior(normal(0, .01), class=b, coef=c_trial)
                             )
                       )

# plotting results
plot(fit_press_trail) # it's still on the log scale though, so for interpretability we need to transform back

# extracting samples for the params from the model
alpha_samples <- as_draws_df(fit_press_trail)$b_Intercept
alpha_samples

beta_samples <- as_draws_df(fit_press_trail)$b_c_trial
beta_samples

# casting back to ms scale
beta_ms <- exp(alpha_samples) - exp(alpha_samples - beta_samples)
beta_ms # a represents the mean and b the change in reaction time
# so we extract from the mean of the reaction times the difference in reaction times between the mean and one trial

beta_msmean <- round(mean(beta_ms),5)
beta_msmean

beta_mslow <- round(quantile(beta_ms, prob = 0.025), 5)
beta_mslow

beta_mshigh <- round(quantile(beta_ms, prob = 0.975), 5)
beta_mshigh

c(beta_msmean, beta_mslow, beta_mshigh)

# show slowdown after 100 trials from the middle of the experiment
effect_100 <- exp(alpha_samples + 100 * beta_samples) - exp(alpha_samples)
effect_100

c(mean = mean(effect_100), quantile(effect_100, c(.025, .975)))






# Example 3: logistic regression

data('df_recall')
head(df_recall) # df has 0-1-responses (bernoulli likelihood correcponds to it,
# more generally here: the binomial distribution)

# our dependent variable will be the set size

# examining which set sizes we have
df_recall$set_size %>% unique() %>% sort()

# making a new column with the set_size correlated to the mean of the set_sizes
df_recall <- df_recall %>% mutate(c_set_size = set_size - mean(set_size))
df_recall

# our model will be: correct ~ Bernoulli_n(theta_n)
# we still use a linear model, but the prediction will be on the log-odd scale
# note: there is no error term in the formula

# visualising graphically
x <- seq(0.001, 0.999, by=0.001) # making a line
y <- log(x/(1-x)) # log-odds-formula
logistic_dat <- data.frame(theta=x, eta=y) # theta now is the log-odds-transformation
logistic_dat

p1 <- qplot(logistic_dat$theta, logistic_dat$eta, geom='line') +
  xlab(expression(theta)) +
  ylab(expression(eta)) +
  ggtitle("The logit link function") +
  annotate('text',
           x=0.3, y=4,
           label = expression(paste(eta, '=', g(theta))), parse=TRUE,
           size=8)
p1
# we can calculate the inverse of the logit function and we'll get probabilities out
# calculating the inverse is possible, since there is no error term in the formula

# those functions for implementing logit and inverse logit functions:
# qlogis(p): logit functions
# plogis(x): inverse logit functions

# modeling the effect of set_size on the log-odds of theta:
fit_recall <- brm(correct ~ 1 + c_set_size,
                       data = df_recall,
                       family = bernoulli(link=logit),
                       prior = c(
                         prior(normal(0, 1.5), class=Intercept),
                         prior(normal(0, .1), class=b, coef=c_set_size)
                       )
                    )

# summarizing results (in log-odds-space)
posterior_summary(fit_recall, variable=c("b_Intercept", "b_c_set_size"))

# plotting results (in log-odds-space)
plot(fit_recall)

# transorming back to interpretable scales
alpha_samples <- as_draws_df(fit_recall)$b_Intercept
beta_samples <- as_draws_df(fit_recall)$b_c_set_size

beta_mean <- round(mean(beta_samples), 5)
beta_mean

beta_low <- round(quantile(beta_samples, prob = 0.025), 5)
beta_low

beta_high <- round(quantile(beta_samples, prob = 0.975), 5)
beta_high

av_accuracy <- plogis(alpha_samples) # we get back average accuracy of the posterior distribution
c(mean=mean(av_accuracy), quantile(av_accuracy, c(.025, .975)))

