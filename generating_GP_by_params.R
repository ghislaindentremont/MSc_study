library(MASS)
library(tidyverse)
library(plyr)


# parameter-wise GPs ----
n = 16  # number of participants

# trajectory time points
x = 0:100/100  # I make it 0 to 1 so that amplitudes and volatilities are on the right scale
n_x = length(x)

# mean function
mu = rep(0, n_x)

# covariance matrix with hyperparameters
# c(intercept, effect1, effect2, interaction_effect)
# 0.5 contrast matrix 
amplitudes = c(0.11, 1.35, 0.68, 0.20)
volatilities = c(1.22, 1.31, 0.91, 0.10)

Sigmas = list()

for (param in 1:length(amplitudes)) {
  Sigmas[[param]] = matrix(0, n_x, n_x)
  for (i in 1:n_x){
    for (j in 1:n_x){
      Sigmas[[param]][i, j] = amplitudes[param]^2*exp(-volatilities[param]^2*(1/2)*(x[i] - x[j])^2)  # + ifelse(i == j, sigma_sq, 0)
    }
  }
}


# Visualizing Kernel ----
curve(amplitudes[1]^2*exp(-volatilities[1]^2*(1/2)*(x)^2), 0, (max(x)-min(x))*3, xlab="Lag", ylab="Covariance", main = "Squared Exponential", ylim = c(0, 3))
for (param in 2:length(amplitudes)) {
  curve(amplitudes[param]^2*exp(-volatilities[param]^2*(1/2)*(x)^2), 0, (max(x)-min(x))*3, add = T, xlab="Lag", ylab="Covariance", main = "Squared Exponential")
}


# Sample Functions ----
fs = list()
fs_subj = list()

subj_amplitude_sd = c(0.13, 0.23, 0.30, 0.25)
subj_volatility_sd = c(2.91, 2.34, 2.51, 2.48)

for (param in 1:length(amplitudes)) {
  fs[[param]] = mvrnorm(1, mu, Sigmas[[param]])
  
  for (subj in 1:n) {
    subj_amplitude = rnorm(1, 0, subj_amplitude_sd[param])
    subj_volatility = rnorm(1, 0, subj_volatility_sd[param])
    
    subj_Sigma = matrix(0, n_x, n_x)
    for (i in 1:n_x){
      for (j in 1:n_x){
        subj_Sigma[i, j] = subj_amplitude^2*exp(-subj_volatility^2*(1/2)*(x[i] - x[j])^2)  
      }
    }
    
    f_subj_val = fs[[param]] + mvrnorm(1, mu, subj_Sigma)
    f_subj = data.frame(value = f_subj_val, parameter = param, id = subj, time = x)
    
    idx = ((param-1)*n+1) + subj
    fs_subj[[idx]] = f_subj
  }
}

df = do.call(rbind, fs_subj)
df$parameter = factor(
  ifelse(
    df$parameter == 1
    , "intercept"
    , ifelse(
      df$parameter == 2
      , "effect1"
      , ifelse(
        df$parameter == 3
        , "effect2"
        , "interaction_effect"
      )
    )
  )
)


# Visualizing and Saving Group f(x)s ----
df_pop = do.call(cbind, fs)
df_pop = data.frame(df_pop)
names(df_pop) = c("intercept", "effect1", "effect2", "interaction_effect")
df_pop %>%
  mutate(
    time = x
    , condition1 = intercept + effect1/2 + effect2/2 + interaction_effect/4
    , condition2 = intercept - effect1/2 + effect2/2 - interaction_effect/4
    , condition3 = intercept + effect1/2 - effect2/2 - interaction_effect/4
    , condition4 = intercept - effect1/2 - effect2/2 + interaction_effect/4
  ) -> df_pop

# same generative function (f(x))
saveRDS(df_pop, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/fake_data3_group.rds")

df_pop %>%
  gather(parameter, value, intercept:interaction_effect) %>%
  ggplot()+
    geom_line(aes(x=time, y=value))+
    facet_grid(parameter~.)

df_pop %>%
  gather(condition, value, condition1:condition4) %>%
  ggplot()+
    geom_line(aes(x=time, y=value))+
    facet_grid(condition~.)


# Visualizing Noiseless Parameter f(x)s ----
df %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=factor(id)), alpha = 0.3)+
  facet_grid(parameter~.)+
  theme(legend.position="none")


# Visualizing Noiseless Condition f(x)s ----
df %>%
  spread(parameter, value) %>%
  mutate(
    condition1 = intercept + effect1/2 + effect2/2 + interaction_effect/4
    , condition2 = intercept - effect1/2 + effect2/2 - interaction_effect/4
    , condition3 = intercept + effect1/2 - effect2/2 - interaction_effect/4
    , condition4 = intercept - effect1/2 - effect2/2 + interaction_effect/4
    ) %>%
  gather(condition, value, condition1:condition4) -> df_cond

df_cond %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=factor(id)), alpha = 0.3)+
  facet_grid(condition~.)+
  theme(legend.position="none")


# Sampling from f(x)s with noise ----
n_trials_by_condition = 200
# I sample the subj_noise from a distribution that will capture the range of subj_noise 
# computed: roughly -2.22 to -1.5
# this actually works out to look a bit different than the gp_regression stan model which estimates 
# the subject noise sd, but sets the mean to zero in log space 
# and then goes on to estimate the actual subject by subject noise
noise_mean = -1.8
noise_sd = 0.15
hist(rnorm(10000, noise_mean, noise_sd))

df_noise2 = ddply(
  .data = df_cond
  , .variables = c("condition","id")
  , .fun = function(df_piece) {
    noise =  exp(rnorm(1, noise_mean, noise_sd))
    f_noise = t(mvrnorm(n_trials_by_condition, df_piece$value, noise^2*diag(nrow(df_piece))))
    data.frame(f_noise, time=x)
  }
)

df_noise = df_noise2 %>%
  gather(trial, value, -c(condition, id, time), factor_key = T)

df_noise$condition = factor(df_noise$condition)
df_noise$id = factor(df_noise$id)

df_noise %>%
  filter(as.numeric(id) <= 5) %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=trial), alpha = 0.1)+
  facet_grid(condition~id)+
  theme(legend.position="none")

df_id_means = aggregate(value ~ time + condition + id, data=df_noise, FUN=mean)

df_id_means %>%
  filter(as.numeric(id) <= 5) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(condition~id)

# change column names etc.
names(df_noise)[5] = c("position")
df_noise$coordinate = factor("z")

# Save File ----
saveRDS(df_noise, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/fake_data3.rds")
