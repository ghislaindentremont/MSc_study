# Load Pakages ----

library(MASS)
library(tidyverse)
library(plyr)



# Functions ----

# this function takes in amplitude and volatility values (one for each condition) and creates covariance matrices for each condition
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

# this function takes the variability (sd) among participant amplitude and volatility
# it then samples participant-specific amplitude and volatility values
# based on these, participant-specific covariance matrices are generated
# samples are then taken from a multivariate normal with mean zero and variance defined by these covariances (one for each participant)
# these samples make up a participant-specific deviation from the population mean function which itself is sampled from a multivariate normal
# the sum of the population mean function and the participant-specific deviation function ultimately make up the participant-specific function 
# this process is repeated for each condition and each participant
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

# number of participants
n = 29  

# trajectory time points; they are bounded from 0 to 1 so that amplitudes and volatilities are on the right scale
x = 0:14/14

# the number of time points
n_x = length(x)

# mean function: all zeros
mu = rep(0, n_x)



# Group GPs ----

# the prior on the population amplitude is examined
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "population amplitude")

# the population amplitudes for each condition are specified
amplitudes = c(0.80, 1.35)

# the prior on the population volatility is examined
curve(dcauchy(x, 0, 10), 0, 50, ylab = "density", xlab = "population volatility")

# the population volatilities for each condition are specified
volatilities = c(1.12, 2.51)

# the covariance matrix for the population mean function is computed
Sigmas = get_Sigmas(amplitudes, volatilities)



# Group GPs for Noise ----

# the population amplitudes for the trial-wise variability (noise) functions of each condition are specified
namplitudes = c(1.0, 1.0)

# the population volatilities for the trial-wise variability (noise) functions of each condition are specified
nvolatilities = c(0.85, 1.81)

# the covariance matrix for the population noise function is computed
nSigmas = get_Sigmas(namplitudes, nvolatilities)



# Sample Mean Functions ----

# the prior on the amplitude standard deviation (among participants) is examined
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject amplitude sd")

# the participant amplitude standard deviations are specified
subj_amplitude_sd = c(0.53, 0.43)

# the prior on the volatility standard deviation (among participants) is examined
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject volatility sd")

# the participant volatility standard deviations are specified
subj_volatility_sd = c(0.71, 1.14)

# the participant-level deviation functions are computed
temp = get_subj_Sigmas(subj_amplitude_sd, subj_volatility_sd, Sigmas)

# identify the population mean function
fs = temp$fs

# indentify the participant mean functions  
fs_subj = temp$fs_subj

# join all the participant mean functions together
df = do.call(rbind, fs_subj)

# add labels for the two arbitrary conditions
df$parameter = factor(
  ifelse(
    df$parameter == 1
    , "condition1"
    , "condition2"
  )
)

# change the column name for the condition labels
names(df)[2] = c("condition")

# plot the participant mean functions
df %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=factor(id)), alpha = 0.3)+
  facet_grid(condition~.)+
  theme(legend.position="none")

# save the generative participant mean functions
saveRDS(df, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal_subj.rds")

# join the population mean functions together
df_pop = do.call(cbind, fs)

# create data frame object 
df_pop = data.frame(df_pop)

# change column names - one per condition
names(df_pop) = c("condition1", "condition2") 

# add column for arbitrary time labels
df_pop %>%
  mutate(
    time = x
  ) -> df_pop

# plot the population mean functions for each condition
df_pop %>%
  gather(condition, value, condition1:condition2) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(condition~.)

# save the generative population mean functions
saveRDS(df_pop, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal_pop.rds")



# Sample Noise Functions ----

# the prior for the participant noise amplitude standard deviation
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject noise amplitude sd")

# specify participant noise amplitude sds
subj_namplitude_sd = c(0.43, 0.53)

# the prior for the participant noise volatility standard deviation
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject noise volatility sd")

# specify participant noise volatility sds
subj_nvolatility_sd = c(1.2, 0.8)

# the participant-level noise deviation functions are computed
temp = get_subj_Sigmas(subj_namplitude_sd, subj_nvolatility_sd, nSigmas)

# identify population noise functions
fs_noise = temp$fs

# identify participant noise functions
fs_noise_subj = temp$fs_subj

# join participant noise functions together
df_noise = do.call(rbind, fs_noise_subj)

# add condition labels
df_noise$parameter = factor(
  ifelse(
    df_noise$parameter == 1
    , "condition1"
    , "condition2"
  )
)

# change condition column name
names(df_noise)[2] = c("condition")

# plot participant noise functions
df_noise %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=factor(id)), alpha = 0.3)+
  facet_grid(condition~.)+
  theme(legend.position="none")

# save generative participant noise functions
saveRDS(df_noise, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal_subj_noise.rds")

# join population noise functions
df_noise_pop = do.call(cbind, fs_noise)

# create data frame object
df_noise_pop = data.frame(df_noise_pop)

# change column names
names(df_noise_pop) = c("condition1", "condition2") 

# add tiem column
df_noise_pop %>%
  mutate(
    time = x
  ) -> df_noise_pop

# plot population noise functions
df_noise_pop %>%
  gather(condition, value, condition1:condition2) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(condition~.)

# save generative population noise functions
saveRDS(df_noise_pop, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal_pop_noise.rds")



# Sampling from f(x)s with noise ----

# specify number of trials by condition
n_trials_by_condition = 20

# verify that the participant noise function and participant mean function data frames are alligned 
mean(df$id == df_noise$id)
mean(df$time == df_noise$time)
mean(df$condition == df_noise$condition)

# add participant noise as new column to participant mean data frame 
df$noise = df_noise$value

# sample from participant mean functions with noise (trial-by-trial variability)
df_final2 = ddply(
  .data = df
  , .variables = c("condition","id")
  , .fun = function(df_piece) {
    f_noise = t(mvrnorm(n_trials_by_condition, df_piece$value, exp(df_piece$noise)^2*diag(nrow(df_piece))))
    data.frame(f_noise, time=df_piece$time)
  }
)

# reformat data frame
df_final = df_final2 %>%
  gather(trial, value, -c(condition, id, time), factor_key = T)

# make condition labels a factor column
df_final$condition = factor(df_final$condition)

# make id labels a factor column
df_final$id = factor(df_final$id)

# plot fake participant trajectories 
df_final %>%
  filter(as.numeric(id) <= 5) %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=trial), alpha = 0.1)+
  facet_grid(condition~id)+
  theme(legend.position="none")

# get mean participant trajectories by averaging over trials
df_id_means = aggregate(value ~ time + condition + id, data=df_final, FUN=mean)

# plot fake participant mean trajectories 
df_id_means %>%
  filter(as.numeric(id) <= 5) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(condition~id)

# change column names 
names(df_final)[5] = c("position")

# set coordinate (z; there is only one coordinate) to factor column
df_final$coordinate = factor("z")



# Save Fake Data File ----

saveRDS(df_final, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal.rds")
