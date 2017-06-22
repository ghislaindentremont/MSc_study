# Load Pakages & Data ----
library(tidyverse)
library(rstan)
library(ezStan)
library(MASS)
library(reshape2)

rstan_options(auto_write = TRUE) 

# load a function to do some pre-computation on x
source('~/Documents/Experiments/Trajectory/GP Demos/Mike Demos/gp_example/prep_x.R')

# load a function to do some pre-computation on x
# load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/data_trim.Rdata")
df_long_trim = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/fake_data3.rds")

# Look at Data ----
summary(df_long_trim)

head(df_long_trim)

# set trial start to zero
df_long_trim %>%
  group_by(id, trial, condition, coordinate) %>%
  dplyr::mutate(position = position - position[time == 0]) -> df_long_trim

# it looks like there is a slight sub-grouping within each target condition. Probably explained by the cues which 
# are ignored here
# also, you can see impact of rounding time. There are sometimes two data points for a given time point
df_long_trim %>%
  dplyr::filter(as.numeric(id) < 10, coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color=trial), alpha = 0.2)+
  facet_grid(id~condition)+
  theme(legend.position="none")
df_long_trim %>%
  dplyr::filter(as.numeric(id) >= 10, coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color=trial), alpha = 0.2)+
  facet_grid(id~condition)+
  theme(legend.position="none")

# average over trials
df_long_trim %>%
  group_by(id, coordinate, time, condition) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_id_means

df_id_means %>%
  filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  facet_grid(.~condition)

# by condition
df_id_means %>%
  group_by(coordinate, time, condition) %>%
  dplyr::summarize(
    position_grand_avg = mean(position_avg)
  ) -> df_condition_means

df_condition_means %>%
  filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = condition))


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
# bin_width = 10
bin_width = 0.1
df_long_trimz$time_lores = round0(df_long_trimz$time, bin_width)

df_long_trimz %>% 
  group_by(id, coordinate, trial, time_lores, condition) %>%  # this is important because it organizes by id
  dplyr::summarise(position_bin = mean(position)) -> df_long_trimz
df_long_trimz %>%
  dplyr::filter(as.numeric(id) < 10) %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin, color=trial), alpha = 0.2)+
  facet_grid(id~condition)+
  theme(legend.position="none")
df_long_trimz %>%
  dplyr::filter(as.numeric(id) >= 10) %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin, color=trial), alpha = 0.2)+
  facet_grid(id~condition)+
  theme(legend.position="none")

df_long_trimz %>%
  group_by(id, coordinate, time_lores, condition) %>%
  dplyr::summarise(position_bin_avg = mean(position_bin)) -> df_id_meansz
df_id_meansz %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin_avg, color = id), alpha = 0.5)+
  facet_grid(.~condition)

df_id_meansz %>%
  group_by(coordinate, time_lores, condition) %>%
  dplyr::summarise(position_bin_grand_avg = mean(position_bin_avg)) -> df_condition_meansz
df_condition_meansz %>%
  ggplot()+
    geom_line(aes(x=time_lores, y=position_bin_grand_avg, color = condition))


# Establish Prior ----
# volatility
# subj_volatility_sd
curve(dweibull(x, 2, 10), 0, 10, ylab = "density", xlab = "subject volatility sd")
# subj_volatility
curve(dcauchy(x, 0, mean(rweibull(1000, 2, 10))), 0, 20, ylab = "density", xlab = "subject volatility")
# volatility
curve(dcauchy(x, 0, 20), 0, 50, ylab = "density", xlab = "population volatility")

# amplitude
# subj_amplitude_sd
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject amplitude sd")
# subj_amplitude
curve(dnorm(x, 0, mean(rweibull(1000, 2, 1))), 0, 5, ylab = "density", xlab = "subject amplitude")
# amplitude
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "population amplitude")

# trajectory time points
x = seq(0, 1, 0.1)
n_x = length(x)

# mean function
mu = rep(0, n_x)

# covariance matrix with hyperparameters
amplitudes = 2
volatilities = 10  # not any bigger than this

Sigmas = matrix(0, n_x, n_x)
for (i in 1:n_x){
  for (j in 1:n_x){
    Sigmas[i, j] = amplitudes^2*exp(-volatilities^2*(1/2)*(x[i] - x[j])^2)
  }
}

# # these approximately correspond to the peak of the prior
n = 5
fs = mvrnorm(n, mu, Sigmas)
fs_df = data.frame(t(fs))
fs_df %>%
  gather(sample, position, 1:n, factor_key = T) -> fs_df
fs_df$time = rep(1:table(fs_df$sample)[[1]], n)

fs_df %>%
  ggplot()+
    geom_line(aes(x=time, y=position, color=sample))+  #, alpha=0.5)+
    theme(legend.position = "none")



# Rstan ----
# get the sorted unique value for x
x = sort(unique(df_long_trimz$time_lores))

# for each value in df_long_trimz$x, get its index x
x_index = match(df_long_trimz$time_lores,x)

# do something similar for the subjects 
s = sort(unique(df_long_trimz$id))
s_index = match(df_long_trimz$id, s)

# # compute the model matrix
# z = model.matrix(
#   data = df_long_trimz
#   , object = ~ condition
# )

# need to create new columns for fake data before making contrast matrix
df_long_trimz$A = ifelse(
  (df_long_trimz$condition == "condition1"|df_long_trimz$condition == "condition3")
  , "a1"
  , "a2"
)
df_long_trimz$B = ifelse(
  (df_long_trimz$condition == "condition1"|df_long_trimz$condition == "condition2")
  , "b1"
  , "b2"
)

z = get_contrast_matrix(
  df_long_trimz
  , ~ A * B
)

# compute the unique entries in the model matrix
temp = as.data.frame(z)
temp = tidyr::unite_(data = temp, col = 'combined', from = names(temp))
temp_unique = unique(temp)
z_unique = z[row.names(z)%in%row.names(temp_unique),]

# for each row in z, get its index z_unique
z_unique_index = match(temp$combined,temp_unique$combined)

# combine the two index objects to get the index into the flattened z_by_f vector
z_by_f_index = z_unique_index + (x_index-1)*nrow(z_unique)

z_by_f_by_s = array(split(z_by_f_index, s_index))

subj_obs = NULL
for (si in 1:length(z_by_f_by_s)) {
  subj_obs = c(subj_obs, length(z_by_f_by_s[[si]]))
}

z_by_f_by_s_pad = array(0,dim=c(16,10000))
for (si in 1:length(subj_obs)) {
  z_by_f_by_s_pad[si,] = c(z_by_f_by_s[[si]], rep(0, 10000 - subj_obs[si]))
}

y = scale(df_long_trimz$position_bin)[,1]  # scaled to mean=0,sd=1
y_by_s = array(split(y, s_index))

y_by_s_pad = array(0,dim=c(16,10000))
for (si in 1:length(subj_obs)) {
  y_by_s_pad[si,] = c(y_by_s[[si]], rep(0, 10000 - subj_obs[si]))
}

# create the data list for stan
data_for_stan = list(
  n_y = nrow(df_long_trimz)
  , y = y_by_s_pad
  , n_x = length(x)
  , x = (x-min(x))/(max(x)-min(x)) #scaled to min=0,max=1
  , x_index = x_index
  , n_subj = length(s)
  , n_z = ncol(z)
  , rows_z_unique = nrow(z_unique)
  , z_unique = z_unique
  , z_by_f_by_s_pad = z_by_f_by_s_pad
  , subj_obs = subj_obs
)

# # package for googleComputeEngine
# save(data_for_stan, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/fake_stan_data3.Rdata")

# # # see cluster_analysis
# mod = rstan::stan_model("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/jenn_study/gp_regression.stan")
# post_test = sampling(
#   mod
#   , data = data_for_stan
#   , iter = 100
#   , init = 0
#   , chains = 2
#   , cores = 2
#   , verbose = T
#   , control = list(max_treedepth = 15)
#   , refresh = 1
#   , include = F
#   , pars = c(
#     'f_normal01'
#     , 'subj_f_normal01'
#     , 'volatility_helper'
#     , 'subj_volatility_helper'
#   )
# )
# post = post_test
# save(post, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/post_test.rdata")


# Examine Results ----
# load stan fit object that was computed in the cloud
# I saved it as post_rt because I forgot to change the name from what was written down in Mike's version from which I adapted the code
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/fake3_post_3000.rdata")

# how long did it take (in hours)?
# fake2_post_11 did not even take 1 hour at 1000 iterations, which is almost 10 times faster
# than its counterpart fake1_post_11, which used 01 contast and did not start every trial at position 0
# fake3_post took 4-5 hours. In additionin this case was 2 conditions, and therefore 2 parameters to estimate
# also, adapt_delta was moved from 0.8 (default) to 0.95
# fake3_post_3000 took a little over one whole day There were 3000 iterations per chain
# adapt_delta was raised to 0.99 and succesfully decreased the number of transitions. It does not seem like the time increase
# was worth the increased efficiency
sort(rowSums(get_elapsed_time(post)/60/60))

# fake3: amplitudes were captured, but not all volatilities were captured by 95% CIs
ezStan::stan_summary(
  from_stan = post
  , par = c('volatility','amplitude', 'subj_noise_sd', 'subj_noise' )
)

# fake3: in this case the volatilities get captured, not all the amplitudes
ezStan::stan_summary(
  from_stan = post
  , par = c('subj_volatility_sd','subj_amplitude_sd')
)

# the intercept samples well with 
ezStan::stan_summary(
  from_stan = post
  , par = c('f')
)

# look at subject-by-subject estimates
ezStan::stan_summary(
  from_stan = post
  , par = c('subj_f')
)

# visualize subject GPs
subj_f = rstan::extract(
  post
  , pars = 'subj_f'
)[[1]]

# intercept
subj_intercept = NULL
for (si in 1:dim(subj_f)[2]) {
  temp = data.frame(subj_f[,si,,1])
  temp$id = si
  temp$sample = 1:nrow(temp)
  subj_intercept = rbind(subj_intercept, temp)
}

subj_f_I = subj_intercept %>%
  gather(key="time", value="value", -c(sample, id)) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 1
  )
subj_f_I_sum = subj_f_I %>%
  dplyr::group_by(
    time
    , id
  ) %>%
  dplyr::summarise(
    med = median(value)
    , lo95 = quantile(value,.025)
    , hi95 = quantile(value,.975)
    , lo50 = quantile(value,.25)
    , hi50 = quantile(value,.75)
  )
subj_f_I_sum %>%
  ggplot()+
  geom_line(aes(x=time, y=med, color=factor(id)))+
  geom_line(aes(x=time, y=hi95, color=factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95, color=factor(id)), linetype = "dashed")+
  ggtitle("intercepts")

# effect 1
subj_effect1 = NULL
for (si in 1:dim(subj_f)[2]) {
  temp = data.frame(subj_f[,si,,2])
  temp$id = si
  temp$sample = 1:nrow(temp)
  subj_effect1 = rbind(subj_effect1, temp)
}

subj_f_e1 = subj_effect1 %>%
  gather(key="time", value="value", -c(sample, id)) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 1
  )
subj_f_e1_sum = subj_f_e1 %>%
  dplyr::group_by(
    time
    , id
  ) %>%
  dplyr::summarise(
    med = median(value)
    , lo95 = quantile(value,.025)
    , hi95 = quantile(value,.975)
    , lo50 = quantile(value,.25)
    , hi50 = quantile(value,.75)
  )
subj_f_e1_sum %>%
  ggplot()+
  geom_line(aes(x=time, y=med, color=factor(id)))+
  geom_line(aes(x=time, y=hi95, color=factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95, color=factor(id)), linetype = "dashed")+
  ggtitle("effects 1")

# effect 2
subj_effect2 = NULL
for (si in 1:dim(subj_f)[2]) {
  temp = data.frame(subj_f[,si,,3])
  temp$id = si
  temp$sample = 1:nrow(temp)
  subj_effect2 = rbind(subj_effect2, temp)
}

subj_f_e2 = subj_effect2 %>%
  gather(key="time", value="value", -c(sample, id)) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 1
  )
subj_f_e2_sum = subj_f_e2 %>%
  dplyr::group_by(
    time
    , id
  ) %>%
  dplyr::summarise(
    med = median(value)
    , lo95 = quantile(value,.025)
    , hi95 = quantile(value,.975)
    , lo50 = quantile(value,.25)
    , hi50 = quantile(value,.75)
  )
subj_f_e2_sum %>%
  ggplot()+
  geom_line(aes(x=time, y=med, color=factor(id)))+
  geom_line(aes(x=time, y=hi95, color=factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95, color=factor(id)), linetype = "dashed")+
  ggtitle("effects 2")

# effect 3
subj_effect3 = NULL
for (si in 1:dim(subj_f)[2]) {
  temp = data.frame(subj_f[,si,,4])
  temp$id = si
  temp$sample = 1:nrow(temp)
  subj_effect3 = rbind(subj_effect3, temp)
}

subj_f_e3 = subj_effect3 %>%
  gather(key="time", value="value", -c(sample, id)) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 1
  )
subj_f_e3_sum = subj_f_e3 %>%
  dplyr::group_by(
    time
    , id
  ) %>%
  dplyr::summarise(
    med = median(value)
    , lo95 = quantile(value,.025)
    , hi95 = quantile(value,.975)
    , lo50 = quantile(value,.25)
    , hi50 = quantile(value,.75)
  )
subj_f_e3_sum %>%
  ggplot()+
  geom_line(aes(x=time, y=med, color=factor(id)))+
  geom_line(aes(x=time, y=hi95, color=factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95, color=factor(id)), linetype = "dashed")+
  ggtitle("effects 3")

# get GP of conditions
subj_f_sum = subj_f_I
subj_f_sum$value2 = subj_f_e1$value
subj_f_sum$value3 = subj_f_e2$value
subj_f_sum$value4 = subj_f_e3$value

subj_condition1 = subj_f_sum$value + subj_f_sum$value2/2 + subj_f_sum$value3/2 + subj_f_sum$value4/4
subj_condition2 = subj_f_sum$value - subj_f_sum$value2/2 + subj_f_sum$value3/2 - subj_f_sum$value4/4
subj_condition3 = subj_f_sum$value + subj_f_sum$value2/2 - subj_f_sum$value3/2 - subj_f_sum$value4/4
subj_condition4 = subj_f_sum$value - subj_f_sum$value2/2 - subj_f_sum$value3/2 + subj_f_sum$value4/4

subj_f_sum = cbind(subj_f_sum, data.frame(
  condition1 = subj_condition1
  , condition2 = subj_condition2
  , condition3 = subj_condition3
  , condition4 = subj_condition4
  ))

subj_to_plot = subj_f_sum %>%
  dplyr::group_by(
    time
    , id
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
    
    , med_3 = median(condition3)
    , lo95_3 = quantile(condition3,.025)
    , hi95_3 = quantile(condition3,.975)
    , lo50_3 = quantile(condition3,.25)
    , hi50_3 = quantile(condition3,.75)
    
    , med_4 = median(condition4)
    , lo95_4 = quantile(condition4,.025)
    , hi95_4 = quantile(condition4,.975)
    , lo50_4 = quantile(condition4,.25)
    , hi50_4 = quantile(condition4,.75)
  )

subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)))+
  geom_line(aes(x=time, y=hi95_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(df_id_meansz, condition == "condition1"), aes(x=time_lores/bin_width+1, y=position_bin_avg, group = id), alpha = 1)+
  ylab('value')+
  xlab('time')

subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)))+
  geom_line(aes(x=time, y=hi95_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(df_id_meansz, condition == "condition2"), aes(x=time_lores/bin_width+1, y=position_bin_avg, group = id), alpha = 1)+
  ylab('value')+
  xlab('time')

subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_3*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)))+
  geom_line(aes(x=time, y=hi95_3*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_3*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(df_id_meansz, condition == "condition3"), aes(x=time_lores/bin_width+1, y=position_bin_avg, group = id), alpha = 1)+
  ylab('value')+
  xlab('time')

subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_4*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)))+
  geom_line(aes(x=time, y=hi95_4*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_4*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin), color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(df_id_meansz, condition == "condition4"), aes(x=time_lores/bin_width+1, y=position_bin_avg, group = id), alpha = 1)+
  ylab('value')+
  xlab('time')

 
# visualize GPs
f = rstan::extract(
  post
  , pars = 'f'
)[[1]]

# get GP of intercept
intercept = data.frame(f[,,1])
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
  ggtitle("intercept")+
  geom_line(aes(x=time, y=med))+
  geom_line(aes(x=time, y=hi95), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95), linetype = "dashed")

# get GP of effect 1
effect1 = data.frame(f[,,2])
effect1$sample = 1:nrow(effect1)
f_e1 = effect1 %>%
  gather(key="time", value="value", -sample) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 2
  )
f_e1_sum = f_e1 %>%
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
f_e1_sum %>%
  ggplot()+
  ggtitle("effect")+
  geom_line(aes(x=time, y=med))+
  geom_line(aes(x=time, y=hi95), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95), linetype = "dashed")

# get GP of effect 2
effect2 = data.frame(f[,,3])
effect2$sample = 1:nrow(effect2)
f_e2 = effect2 %>%
  gather(key="time", value="value", -sample) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 2
  )
f_e2_sum = f_e2 %>%
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
f_e2_sum %>%
  ggplot()+
  ggtitle("effect")+
  geom_line(aes(x=time, y=med))+
  geom_line(aes(x=time, y=hi95), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95), linetype = "dashed")

# get GP of effect 3
effect3 = data.frame(f[,,4])
effect3$sample = 1:nrow(effect3)
f_e3 = effect3 %>%
  gather(key="time", value="value", -sample) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , parameter = 2
  )
f_e3_sum = f_e3 %>%
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
f_e3_sum %>%
  ggplot()+
  ggtitle("effect")+
  geom_line(aes(x=time, y=med))+
  geom_line(aes(x=time, y=hi95), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95), linetype = "dashed")

# get GP of conditions
f_sum = f_I
f_sum$value2 = f_e1$value
f_sum$value3 = f_e2$value
f_sum$value4 = f_e3$value

condition1 = f_sum$value + f_sum$value2/2 + f_sum$value3/2 + f_sum$value4/4
condition2 = f_sum$value - f_sum$value2/2 + f_sum$value3/2 - f_sum$value4/4
condition3 = f_sum$value + f_sum$value2/2 - f_sum$value3/2 - f_sum$value4/4
condition4 = f_sum$value - f_sum$value2/2 - f_sum$value3/2 + f_sum$value4/4

f_sum = cbind(f_sum, data.frame(
  condition1 = condition1
  , condition2 = condition2
  , condition3 = condition3
  , condition4 = condition4
))

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
    
    , med_3 = median(condition3)
    , lo95_3 = quantile(condition3,.025)
    , hi95_3 = quantile(condition3,.975)
    , lo50_3 = quantile(condition3,.25)
    , hi50_3 = quantile(condition3,.75)
    
    , med_4 = median(condition4)
    , lo95_4 = quantile(condition4,.025)
    , hi95_4 = quantile(condition4,.975)
    , lo50_4 = quantile(condition4,.25)
    , hi50_4 = quantile(condition4,.75)
  )



# load real population means 
df_pop = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/fake_data3_group.rds")
# set to zero
df_pop %>%
  dplyr::mutate(
    condition1 = condition1 - condition1[time == 0]
    , condition2 = condition2 - condition2[time == 0]
    , condition3 = condition3 - condition3[time == 0]
    , condition4 = condition4 - condition4[time == 0]
  ) -> df_pop

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), color = "turquoise")+
  geom_line(aes(x=time, y=hi95_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=lo95_1*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "turquoise")+
  geom_line(data=subset(df_condition_meansz, condition == "condition1"), aes(x=time_lores/bin_width+1, y=position_bin_grand_avg), alpha = 0.5)+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = condition1))+
  ylab('value')+
  xlab('time')

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), color = "red")+
  geom_line(aes(x=time, y=hi95_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "red")+
  geom_line(aes(x=time, y=lo95_2*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "red")+
  geom_line(data=subset(df_condition_meansz, condition == "condition2"), aes(x=time_lores/bin_width+1, y=position_bin_grand_avg), alpha = 0.5)+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = condition2))+
  ylab('value')+
  xlab('time')

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_3*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), color = "turquoise")+
  geom_line(aes(x=time, y=hi95_3*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=lo95_3*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "turquoise")+
  geom_line(data=subset(df_condition_meansz, condition == "condition3"), aes(x=time_lores/bin_width+1, y=position_bin_grand_avg), alpha = 0.5)+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = condition3))+
  ylab('value')+
  xlab('time')

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_4*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), color = "turquoise")+
  geom_line(aes(x=time, y=hi95_4*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=lo95_4*sd(df_long_trimz$position_bin)+mean(df_long_trimz$position_bin)), linetype = "dashed", color = "turquoise")+
  geom_line(data=subset(df_condition_meansz, condition == "condition4"), aes(x=time_lores/bin_width+1, y=position_bin_grand_avg), alpha = 0.5)+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = condition4))+
  ylab('value')+
  xlab('time')
