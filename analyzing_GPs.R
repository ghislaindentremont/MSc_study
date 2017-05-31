# Load Pakages & Data ----
library(tidyverse)
library(rstan)
library(ezStan)
library(MASS)

# load a function to do some pre-computation on x
source('~/Documents/Experiments/Trajectory/GP Demos/Mike Demos/gp_example/prep_x.R')

# load a function to do some pre-computation on x
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/data_trim.Rdata")


# Look at Data ----
summary(df_long_trim)

head(df_long_trim)

# it looks like there is a slight sub-grouping within each target condition. Probably explained by the cues which 
# are ignored here
# also, you can see impact of rounding time. There are sometimes two data points for a given time point
df_long_trim %>%
  dplyr::filter(as.numeric(id) < 10, coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color=trial), alpha = 0.2)+
  facet_grid(id~target_final)+
  theme(legend.position="none")
df_long_trim %>%
  dplyr::filter(as.numeric(id) >= 10, coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color=trial), alpha = 0.2)+
  facet_grid(id~target_final)+
  theme(legend.position="none")

# average over trials
df_long_trim %>%
  group_by(id, coordinate, time, target_final) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_id_means

df_id_means %>%
  filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  facet_grid(.~target_final)

# by condition
df_id_means %>%
  group_by(coordinate, time, target_final) %>%
  dplyr::summarize(
    position_grand_avg = mean(position_avg)
  ) -> df_condition_means

df_condition_means %>%
  filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = target_final))


# Binning Waveforms ----
# first select only the z coordinates
df_long_trim %>%
  dplyr::filter(coordinate == "z") -> df_long_trimz

# OF_NOTE: I bined the time axis and averaged over position values within a bin, within a trial
# Should I be averaging these values?
# I believe that it would be reasonable to have multiple position values for a given time point, in a given trial as
# the Bayesian analysis could then consider those values as replicates and account for corresponding uncertainty
# I believe that it would also be equivalent to trial replicates... In theory it would even be possible to account for 
# variabilty associated for a given trial! Anyway, here we just average over each bin
round0 = function(x,z){
  round(x/z,0)*z
}
# will the trajectories be good enough with this low-resolution?
# I average over bin to make analysis compute time reasonable
bin_width = 5
df_long_trimz$time_lores = round0(df_long_trimz$time, bin_width)

df_long_trimz %>% 
  group_by(id, coordinate, trial, time_lores, target_final) %>%
  dplyr::summarise(position_bin = mean(position)) -> df_long_trimz
df_long_trimz %>%
  dplyr::filter(as.numeric(id) < 10) %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin, color=trial), alpha = 0.2)+
  facet_grid(id~target_final)+
  theme(legend.position="none")
df_long_trimz %>%
  dplyr::filter(as.numeric(id) >= 10) %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin, color=trial), alpha = 0.2)+
  facet_grid(id~target_final)+
  theme(legend.position="none")

df_long_trimz %>%
  group_by(id, coordinate, time_lores, target_final) %>%
  dplyr::summarise(position_bin_avg = mean(position_bin)) -> df_id_meansz
df_id_meansz %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin_avg, color = id), alpha = 0.5)+
  facet_grid(.~target_final)

df_id_meansz %>%
  group_by(coordinate, time_lores, target_final) %>%
  dplyr::summarise(position_bin_grand_avg = mean(position_bin_avg)) -> df_condition_meansz
df_condition_meansz %>%
  ggplot()+
    geom_line(aes(x=time_lores, y=position_bin_grand_avg, color = target_final))


# Establish Prior ----
# volatility
# subj_volatility_sd
curve(dweibull(x, 2, 10), 0, 20, ylab = "density", xlab = "subject volatility sd")
# subj_volatility
curve(dcauchy(x, 0, mean(rweibull(1000, 2, 10))), 0, 20, ylab = "density", xlab = "subject volatility")
# volatility
curve(dcauchy(x, 0, 10), 0, 20, ylab = "density", xlab = "population volatility")

# amplitude
# subj_amplitude_sd
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject amplitude sd")
# subj_amplitude
curve(dnorm(x, 0, mean(rweibull(1000, 2, 1))), 0, 5, ylab = "density", xlab = "subject amplitude")
# amplitude
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "population amplitude")

# trajectory time points
x = 0:20
n_x = length(x)

# mean function
mu = rep(0, n_x)

# covariance matrix with hyperparameters
amplitudes = 2
volatilities = 0.1

Sigmas = matrix(0, n_x, n_x)
for (i in 1:n_x){
  for (j in 1:n_x){
    Sigmas[i, j] = amplitudes^2*exp(-volatilities^2*(1/2)*(x[i] - x[j])^2)
  }
}

# # these approximately correspond to the peak of the prior
n = 20
fs = mvrnorm(n, mu, Sigmas)
fs_df = data.frame(t(fs))
fs_df %>%
  gather(sample, position, 1:n, factor_key = T) -> fs_df
fs_df$time = rep(1:count(fs_df$sample)$freq[1], n)

fs_df %>%
  ggplot()+
    geom_line(aes(x=time, y=position, color=sample), alpha=0.5)+
    theme(legend.position = "none")



# Rstan ----
x_list = prep_x(df_long_trimz$time_lores)  

print(x_list$n_x)

# get the predictor matrix W
W = ezStan::get_contrast_matrix(
  data = df_long_trimz
  , formula = ~ target_final
)

# we use same data as before, but a different model
stan_data = list(
  n_dx = x_list$n_dx
  , dx_unique = x_list$dx_unique
  , n_x = x_list$n_x
  , dx_index = x_list$dx_index
  , n_y = nrow(df_long_trimz)
  , y = df_long_trimz$position_bin
  , x_index = x_list$x_index
  , n_w = ncol(W)
  , w = W
  , n_subj = length(unique(df_long_trimz$id))
  , subj = as.numeric(factor(df_long_trimz$id))
)

# package for googleComputeEngine
save(stan_data, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/stan_data.Rdata")

# see cluster_analysis


# Examine Results ----
# load stan fit object that was computed in the cloud
# I saved it as post_rt because I forgot to change the name from what was written down in Mike's version from which I adapted the code
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/cluster_test_post.rdata")

# how long did it take (in hours)?
# 100 iter, took roughly 2 hours
sort(rowSums(get_elapsed_time(post)/60/60))

# the estimates of these parameters are quite bad
stan_res = ezStan::stan_summary(
  from_stan = post
  , par = c('volatility','amplitude', 'subj_noise_sd', 'subj_noise' )
)

ezStan::stan_summary(
  from_stan = post
  , par = c('f')
)

# visualize GPs
f = rstan::extract(
  post
  , pars = 'f'
)[[1]]

# get GP of intercept
intercept = data.frame(f[,1,])
intercept$sample = 1:nrow(intercept)
f_I = intercept %>%
  gather(key="time", value="value", -sample) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 1
  )
f_I_sum = f_I %>%
  dplyr::group_by(
    time
  ) %>%
  dplyr::summarise(
    med = median(value)
    , lo95 = quantile(value,.025)
    , hi95 = quantile(value,.975)
    , lo50 = quantile(value,.25)
    , hi50 = quantile(value,.75)
  )
f_I_sum %>%
  ggplot()+
  geom_line(aes(x=time, y=med))+
  geom_line(aes(x=time, y=hi95), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95), linetype = "dashed")

# get GP of effect
effect = data.frame(f[,2,])
effect$sample = 1:nrow(effect)
f_e = effect %>%
  gather(key="time", value="value", -sample) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 2
  )
f_e_sum = f_e %>%
  dplyr::group_by(
    time
  ) %>%
  dplyr::summarise(
    med = median(value)
    , lo95 = quantile(value,.025)
    , hi95 = quantile(value,.975)
    , lo50 = quantile(value,.25)
    , hi50 = quantile(value,.75)
  )
f_e_sum %>%
  ggplot()+
  geom_line(aes(x=time, y=med))+
  geom_line(aes(x=time, y=hi95), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95), linetype = "dashed")

# get GP of conditions
f_sum = f_I
f_sum$value2 = f_e$value
condition1 = f_sum$value + f_sum$value2/2
condition2 = f_sum$value - f_sum$value2/2
f_sum = cbind(f_sum, data.frame(condition1 = condition1, condition2 = condition2))

to_plot = f_sum %>%
  dplyr::group_by(
    time
  ) %>%
  dplyr::summarise(
    med_1 = median(condition1)
    , lo95_1 = quantile(condition1,.025)
    , hi95_1 = quantile(condition1,.975)
    , lo50_1 = quantile(condition1,.25)
    , hi50_1 = quantile(condition1,.75)

    , med_2 = median(condition2)
    , lo95_2 = quantile(condition2,.025)
    , hi95_2 = quantile(condition2,.975)
    , lo50_2 = quantile(condition2,.25)
    , hi50_2 = quantile(condition2,.75)
  )

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), color = "red")+
  geom_line(aes(x=time, y=hi95_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "red")+
  geom_line(aes(x=time, y=lo95_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "red")+
  geom_line(data=subset(df_condition_meansz, target_final == "1"), aes(x=time_lores*length(x_list$n_x)/bin_width, y=position_bin_grand_avg), alpha = 0.5)+
  ylab('value')+
  xlab('time')

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), color = "turquoise")+
  geom_line(aes(x=time, y=hi95_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=lo95_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "turquoise")+
  geom_line(data=subset(df_condition_meansz, target_final == "0"), aes(x=time_lores*length(x_list$n_x)/bin_width, y=position_bin_grand_avg), alpha = 0.5)+
  ylab('value')+
  xlab('time')
