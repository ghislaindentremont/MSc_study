library(MASS)
library(tidyverse)
library(plyr)


# Functions ----
get_Sigmas = function(amplitudes, volatilities) {
  Sigmas = list()
  
  for (param in 1:length(amplitudes)) {
    Sigmas[[param]] = matrix(0, n_x, n_x)
    for (i in 1:n_x){
      for (j in 1:n_x){
        Sigmas[[param]][i, j] = amplitudes[param]^2*exp(-volatilities[param]^2*(1/2)*(x[i] - x[j])^2)  
      }
    }
  } 
  
  curve(amplitudes[1]^2*exp(-volatilities[1]^2*(1/2)*(x)^2), 0, (max(x)-min(x))*3, xlab="Lag", ylab="Covariance", main = "Squared Exponential", ylim = c(0, 3))
  for (param in 2:length(amplitudes)) {
    curve(amplitudes[param]^2*exp(-volatilities[param]^2*(1/2)*(x)^2), 0, (max(x)-min(x))*3, add = T, xlab="Lag", ylab="Covariance", main = "Squared Exponential")
  }
  
  return(Sigmas)
}

get_subj_Sigmas = function(subj_amplitude_sd, subj_volatility_sd, Sigmas){
  fs = list()
  fs_subj = list()
  
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
  
  return(list("fs" = fs, "fs_subj" = fs_subj))
}


# Experimental parameters ----
n = 20  # number of participants

# trajectory time points
x = 0:100/100  # I make it 0 to 1 so that amplitudes and volatilities are on the right scale
n_x = length(x)

# mean function
mu = rep(0, n_x)


# Group GPs ----
# covariance matrix with hyperparameters
# c(condition1, condition2)
# 1s contrast matrix 
amplitudes = c(0.80, 1.35)
volatilities = c(1.22, 1.31)

Sigmas = get_Sigmas(amplitudes, volatilities)


# Group GPs for Noise ----
namplitudes = c(0.50, 0.60)
nvolatilities = c(0.45, 0.51)

nSigmas = get_Sigmas(namplitudes, nvolatilities)



# Sample Mean Functions ----
subj_amplitude_sd = c(0.13, 0.23)
subj_volatility_sd = c(2.91, 2.34)

temp = get_subj_Sigmas(subj_amplitude_sd, subj_volatility_sd, Sigmas)
fs = temp$fs
fs_subj = temp$fs_subj

df = do.call(rbind, fs_subj)
df$parameter = factor(
  ifelse(
    df$parameter == 1
    , "condition1"
    , "condition2"
  )
)

names(df)[2] = c("condition")

df %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=factor(id)), alpha = 0.3)+
  facet_grid(condition~.)+
  theme(legend.position="none")

# save generative function (f(x))
saveRDS(df, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal/fake_data_proposal_subj.rds")

# population
df_pop = do.call(cbind, fs)
df_pop = data.frame(df_pop)
names(df_pop) = c("condition1", "condition2")  
df_pop %>%
  mutate(
    time = x
  ) -> df_pop

df_pop %>%
  gather(condition, value, condition1:condition2) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(condition~.)

# save generative function (f(x))
saveRDS(df_pop, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal/fake_data_proposal_pop.rds")



# Sample Noise Functions ----
subj_namplitude_sd = c(0.13, 0.23)
subj_nvolatility_sd = c(2.91, 2.34)

temp = get_subj_Sigmas(subj_namplitude_sd, subj_nvolatility_sd, nSigmas)
fs_noise = temp$fs
fs_noise_subj = temp$fs_subj

df_noise = do.call(rbind, fs_noise_subj)
df_noise$parameter = factor(
  ifelse(
    df_noise$parameter == 1
    , "condition1"
    , "condition2"
  )
)

names(df_noise)[2] = c("condition")

df_noise %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=factor(id)), alpha = 0.3)+
  facet_grid(condition~.)+
  theme(legend.position="none")

# save generative function (f(x))
saveRDS(df_noise, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal/fake_data_proposal_subj_noise.rds")

# population
df_noise_pop = do.call(cbind, fs_noise)
df_noise_pop = data.frame(df_noise_pop)
names(df_noise_pop) = c("condition1", "condition2") 
df_noise_pop %>%
  mutate(
    time = x
  ) -> df_noise_pop

df_noise_pop %>%
  gather(condition, value, condition1:condition2) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(condition~.)

# same generative function (f(x))
saveRDS(df_noise_pop, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal/fake_data_proposal_pop_noise.rds")



# Sampling from f(x)s with noise ----
n_trials_by_condition = 100

mean(df$id == df_noise$id)
mean(df$time == df_noise$time)
mean(df$condition == df_noise$condition)

df$noise = df_noise$value

df_final2 = ddply(
  .data = df
  , .variables = c("condition","id")
  , .fun = function(df_piece) {
    f_noise = t(mvrnorm(n_trials_by_condition, df_piece$value, exp(df_piece$noise)^2*diag(nrow(df_piece))))
    data.frame(f_noise, time=df_piece$time)
  }
)

df_final = df_final2 %>%
  gather(trial, value, -c(condition, id, time), factor_key = T)

df_final$condition = factor(df_final$condition)
df_final$id = factor(df_final$id)

df_final %>%
  filter(as.numeric(id) <= 5) %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=trial), alpha = 0.1)+
  facet_grid(condition~id)+
  theme(legend.position="none")

df_id_means = aggregate(value ~ time + condition + id, data=df_final, FUN=mean)

df_id_means %>%
  filter(as.numeric(id) <= 5) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(condition~id)

# change column names etc.
names(df_final)[5] = c("position")
df_final$coordinate = factor("z")

# Save File ----
saveRDS(df_final, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal/fake_data_proposal.rds")
