####################################################
####            Simulation                      ####
####################################################



# Load Pakages ----

library(MASS)
library(tidyverse)
library(plyr)



# Functions ----

# this function takes in amplitude and volatility values (one for each parameter) and creates covariance matrices for each parameter
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
# this process is repeated for each parameter and each participant
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
n = 10  

# trajectory time points; they are bounded from 0 to 1 so that amplitudes and volatilities are on the right scale
x = 0:5/5

# the number of time points
n_x = length(x)

# mean function: all zeros
mu = rep(0, n_x)



# Group GPs ----

# the prior on the population amplitude is examined
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "population amplitude")

# intercept, effect
amplitudes = c(1, 2)

# the prior on the population volatility is examined
curve(dcauchy(x, 0, 10), 0, 50, ylab = "density", xlab = "population volatility")

# the population volatilities for each parameter are specified
volatilities = c(1, 2)

# the covariance matrix for the population mean function is computed
Sigmas = get_Sigmas(amplitudes, volatilities)



# Group GPs for Noise ----

# the population amplitudes for the trial-wise variability (noise) functions of each parameter are specified
namplitudes = c(1, 1)

# the population volatilities for the trial-wise variability (noise) functions of each parameter are specified
nvolatilities = c(1, 1)

# the covariance matrix for the population noise function is computed
nSigmas = get_Sigmas(namplitudes, nvolatilities)



# Sample Mean Functions ----

# the prior on the amplitude standard deviation (among participants) is examined
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject amplitude sd")

# the participant amplitude standard deviations are specified
subj_amplitude_sd = c(1, 1)

# the prior on the volatility standard deviation (among participants) is examined
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject volatility sd")

# the participant volatility standard deviations are specified
subj_volatility_sd = c(1, 1)

# the participant-level deviation functions are computed
temp = get_subj_Sigmas(subj_amplitude_sd, subj_volatility_sd, Sigmas)

# identify the population mean function
fs = temp$fs

# indentify the participant mean functions  
fs_subj = temp$fs_subj

# join all the participant mean functions together
df = do.call(rbind, fs_subj)

# add labels for the two parameters
df$parameter = factor(
  ifelse(
    df$parameter == 1
    , "intercept"
    , "effect"
  )
)

# plot the participant mean functions
df %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=factor(id)), alpha = 0.3)+
  facet_grid(parameter~.)+
  theme(legend.position="none")

# join the population mean functions together
df_pop = do.call(cbind, fs)

# create data frame object 
df_pop = data.frame(df_pop)

# change column names - one per parameter
names(df_pop) = c("intercept", "effect") 

# add column for arbitrary time labels
df_pop %>%
  mutate(
    time = x
  ) -> df_pop

# plot the population mean functions for each parameter
df_pop %>%
  gather(parameter, value, intercept:effect) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(parameter~.)



# Sample Noise Functions ----

# the prior for the participant noise amplitude standard deviation
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject noise amplitude sd")

# specify participant noise amplitude sds
subj_namplitude_sd = c(1, 1)

# the prior for the participant noise volatility standard deviation
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject noise volatility sd")

# specify participant noise volatility sds
subj_nvolatility_sd = c(1, 1)

# the participant-level noise deviation functions are computed
temp = get_subj_Sigmas(subj_namplitude_sd, subj_nvolatility_sd, nSigmas)

# identify population noise functions
fs_noise = temp$fs

# identify participant noise functions
fs_noise_subj = temp$fs_subj

# join participant noise functions together
df_noise = do.call(rbind, fs_noise_subj)

# add parameter labels
df_noise$parameter = factor(
  ifelse(
    df_noise$parameter == 1
    , "intercept"
    , "effect"
  )
)

# change parameter column name
names(df_noise)[2] = c("parameter")

# plot participant noise functions
df_noise %>%
  ggplot()+
  geom_line(aes(x=time, y=value, color=factor(id)), alpha = 0.3)+
  facet_grid(parameter~.)+
  theme(legend.position="none")

# join population noise functions
df_noise_pop = do.call(cbind, fs_noise)

# create data frame object
df_noise_pop = data.frame(df_noise_pop)

# change column names
names(df_noise_pop) = c("intercept", "effect") 

# add tiem column
df_noise_pop %>%
  mutate(
    time = x
  ) -> df_noise_pop

# plot population noise functions
df_noise_pop %>%
  gather(parameter, value, intercept:effect) %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(parameter~.)



# Sampling from f(x)s with noise ----

# need to create conditions
df %>%
  spread(parameter, value) %>%
  dplyr::mutate(
    condition1 = intercept + effect/2
    , condition2 = intercept - effect/2
  ) %>%
  dplyr::select(-c(effect, intercept)) %>%
  gather(condition, value, condition1:condition2) -> df_c

df_noise %>%
  spread(parameter, value) %>%
  dplyr::mutate(
    condition1 = intercept + effect/2
    , condition2 = intercept - effect/2
  ) %>%
  dplyr::select(-c(effect, intercept)) %>%
  gather(condition, value, condition1:condition2) -> df_noise_c

# specify number of trials by condition
n_trials_by_condition = 10

# verify that the participant noise function and participant mean function data frames are alligned 
mean(df_c$id == df_noise_c$id)
mean(df_c$time == df_noise_c$time)
mean(df_c$condition == df_noise_c$condition)

# add participant noise as new column to participant mean data frame 
df_c$noise = df_noise_c$value

# sample from participant mean functions with noise (trial-by-trial variability)
df_final2 = ddply(
  .data = df_c
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
  ggplot()+
  geom_line(aes(x=time, y=value, color=trial), alpha = 0.5)+
  facet_grid(condition~id)+
  theme(legend.position="none")

# get mean participant trajectories by averaging over trials
df_id_means = aggregate(value ~ time + condition + id, data=df_final, FUN=mean)

# plot fake participant mean trajectories 
df_id_means %>%
  ggplot()+
  geom_line(aes(x=time, y=value))+
  facet_grid(condition~id)

# change column names 
names(df_final)[5] = c("position")







####################################################
####            Analysis                        ####
####################################################



# Load Pakages & Data ----

library(rstan)
library(reshape2)
library(coda)

# load in the data
df_long_sim = df_final


# Look at Data ----

#  summarize dataset
summary(df_long_sim)

# look at a few rows of dataset
head(df_long_sim)

# function
df_long_sim %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color=trial), alpha = 0.5)+
  facet_grid(id~condition)+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white") 
    , axis.ticks = element_blank()
    , axis.text = element_blank()
  )


# average over trials
df_long_sim %>%
  group_by(id, time, condition) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_long_sim_avg


# plot participant mean trajectories 
df_long_sim_avg %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  facet_grid(.~condition)+
  ylab('average position')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

# average over participants
df_long_sim_avg %>%
  group_by(time, condition) %>%
  dplyr::summarize(
    position_grand_avg = mean(position_avg)
  ) -> df_long_sim_grand_avg

# plot population mean trajectories
df_long_sim_grand_avg %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = condition))+
  ylab('grand average position')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )





########################################################
####        Gaussian Process Regression             ####
########################################################


# Binning Waveforms ----

# specify bin width
bin_width = 1/5

# define a new column if no binning is required (fake data)
df_long_sim$time_lores = df_long_sim$time

# define a new column if no binning is required (fake data)
df_long_sim$position_bin = df_long_sim$position



# Rstan ----

get_data_for_stan = function(zero_one_contrast = T) {
  
  #get the sorted unique value for x
  x = sort(unique(df_long_sim$time_lores))
  
  # for each value in df_long_sim$time_lores, get its index x
  x_index = match(df_long_sim$time_lores,x)
  
  # do something similar for the subjects
  s = sort(unique(df_long_sim$id))
  s_index = match(df_long_sim$id, s)
  
  if (zero_one_contrast) {
    # compute the model matrix
    z = data.frame(
      condition1 = ifelse(df_long_sim$condition == "condition1", 1, 0)
      , condition2 =ifelse(df_long_sim$condition == "condition2", 1, 0)
    )
  } else {
    # compute the model matrix
    z = data.frame(
      intercept = 1
      , effect =ifelse(df_long_sim$condition == "condition1", 1/2, -1/2)
    )
  }
  
  # compute the unique entries in the model matrix
  temp = as.data.frame(z)
  temp = tidyr::unite_(data = temp, col = 'combined', from = names(temp))
  temp_unique = unique(temp)
  z_unique = z[row.names(z)%in%row.names(temp_unique),]
  
  # for each row in z, get its index z_unique
  z_unique_index = match(temp$combined,temp_unique$combined)
  
  # combine the two index objects to get the index into the flattened z_by_f vector
  z_by_f_index = z_unique_index + (x_index-1)*nrow(z_unique)
  
  # get one index object for each participant
  z_by_f_by_s = array(split(z_by_f_index, s_index))
  
  # specify number of observations for a given participant
  subj_obs = NULL
  for (si in 1:length(z_by_f_by_s)) {
    subj_obs = c(subj_obs, length(z_by_f_by_s[[si]]))
  }
  
  # create a padded version of the z_by_f_by_s object so that the entry for each participant has the same number of observations
  z_by_f_by_s_pad = array(0,dim=c(10,10000))
  for (si in 1:length(subj_obs)) {
    z_by_f_by_s_pad[si,] = c(z_by_f_by_s[[si]], rep(0, 10000 - subj_obs[si]))
  }
  
  # if fake data
  y = df_long_sim$position_bin
  
  # get data specific to each participant
  y_by_s = array(split(y, s_index))
  
  # pad that data
  y_by_s_pad = array(0,dim=c(10,10000))
  for (si in 1:length(subj_obs)) {
    y_by_s_pad[si,] = c(y_by_s[[si]], rep(0, 10000 - subj_obs[si]))
  }
  
  # create the data list for stan
  data_for_stan = list(
    n_y = nrow(df_long_sim)
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
  
  return(data_for_stan)
}
  

# run models
data_for_stan_01 = get_data_for_stan()
data_for_stan_1_05 = get_data_for_stan(zero_one_contrast = F)



# Run Stan Model ----
options(mc.cores=parallel::detectCores())

mod = rstan::stan_model("gp_regression.stan")

# set the model running on each core
get_samples = function(data_for_stan) {
  post = sampling(
    mod
    , data = data_for_stan
    , iter = 200
    , init = 0
    , chains = 4
    , cores = 4
    , verbose = T
    , refresh = 1
    , control = list(
      max_treedepth = 15
      , adapt_delta = 0.99  
    )  
    , include = F
    , pars = c(
      'f_normal01'
      , 'subj_f_normal01'
      , 'volatility_helper'
      , 'subj_volatility_helper'
      , 'noise_f_normal01'
      , 'noise_subj_f_normal01'
      , 'noise_volatility_helper'
      , 'noise_subj_volatility_helper'
    )
  )
  
  return(post)
}

# for 01 contrasts
post_01 = get_samples(data_for_stan_01)

# for intercept/effect contrasts 
post_1_05 = get_samples(data_for_stan_1_05)




# Examine Results ----


####                      Overview                             

# how long did it take (in hours)?
sort(rowSums(get_elapsed_time(post_1_05)/60/60))
sort(rowSums(get_elapsed_time(post_01)/60/60))


# function from 'ezStan' package
stan_summary = function(
  from_stan
  , par
  , probs = c(.5,.025,.975)
  , digits = 2
  , X = NULL
  , W = NULL
  , B = NULL
  , is_cor = F
  , return_array = F
){
  
  s = summary(object=from_stan,pars=par,probs=probs,use_cache=F)$summary
  s = array(
    s[,4:ncol(s)]
    , dim = c(dim(s)[1],ncol(s)-3)
    , dimnames = list(
      dimnames(s)[[1]]
      , dimnames(s)[[2]][4:ncol(s)]
    )
  )
  if(!is_cor){
    if(!is.null(X)){
      dimnames(s)[[1]] = dimnames(X)[[2]]
    }
    if(!is.null(W)){
      dimnames(s)[[1]] = names_from_WB(W,B)
    }
  }else{
    temp = dimnames(s)[[1]]
    temp = gsub(']','',temp)
    temp = unlist(strsplit(temp,'[',fixed=T))
    temp = temp[(1:length(temp))%%2==0]
    temp = unlist(strsplit(temp,',',fixed=T))
    v1 = temp[(1:length(temp))%%2==1]
    v2 = temp[(1:length(temp))%%2==0]
    keep = v2>v1
    v1 = v1[keep]
    v2 = v2[keep]
    if(!is.null(X)){
      v1 = dimnames(X)[[2]][as.numeric(v1)]
      v2 = dimnames(X)[[2]][as.numeric(v2)]
    }
    if(!is.null(W)){
      temp = names_from_WB(W,B)
      v1 = temp[as.numeric(v1)]
      v2 = temp[as.numeric(v2)]
    }
    s = array(
      s[keep,]
      , dim = c(sum(keep),ncol(s))
      , dimnames = list(
        paste(v1,v2,sep='~')
        , dimnames(s)[[2]]
      )
    )
  }
  if(!return_array){
    print(s,digits=digits)
  }else{
    return(s)
  }
}


# population hyperparameter estimates
stan_summary(
  from_stan = post_1_05
  , par = c('volatility','amplitude', 'noise_volatility', 'noise_amplitude')
)
stan_summary(
  from_stan = post_01
  , par = c('volatility','amplitude', 'noise_volatility', 'noise_amplitude')
)

# participant-to-participant variability estimates
stan_summary(
  from_stan = post_1_05
  , par = c('subj_volatility_sd','subj_amplitude_sd', 'noise_subj_volatility_sd','noise_subj_amplitude_sd')
)
stan_summary(
  from_stan = post_01
  , par = c('subj_volatility_sd','subj_amplitude_sd', 'noise_subj_volatility_sd','noise_subj_amplitude_sd')
)


# population mean function
stan_summary(
  from_stan = post_1_05
  , par = c('f')
)
stan_summary(
  from_stan = post_01
  , par = c('f')
)


# population noise function
stan_summary(
  from_stan = post_1_05
  , par = c('noise_f')
)
stan_summary(
  from_stan = post_01
  , par = c('noise_f')
)


# participant mean functions
stan_summary(
  from_stan = post_1_05
  , par = c('subj_f')
)
stan_summary(
  from_stan = post_01
  , par = c('subj_f')
)


# participant noise functions
stan_summary(
  from_stan = post_1_05
  , par = c('noise_subj_f')
)
stan_summary(
  from_stan = post_01
  , par = c('noise_subj_f')
)



####                 Violins                                


# Functions ----

# highest density interval functions (HDIs)
get_95_HDI = function(y) {
  HDI = HPDinterval( as.mcmc( as.vector(y) ), prob = .95 )
  # Den = density( as.vector(y) )
  min = HDI[1]
  # mod = Den$x[which(Den$y == max(Den$y))] # mode as indicator of central tendency
  med = median(y)
  max = HDI[2]
  return( c( ymin = min, y = med, ymax = max) )
}

get_50_HDI = function(y) {
  HDI = HPDinterval( as.mcmc( as.vector(y) ), prob = .50 )
  # Den = density( as.vector(y) )
  min = HDI[1]
  # mod = Den$x[which(Den$y == max(Den$y))]  # mode as indicator of central tendency
  med = median(y)
  max = HDI[2]
  return( c( ymin = min, y = med, ymax = max) )
}



#-----------------------------------------------------------------#
####                  Population Level                         ####
#-----------------------------------------------------------------#


# Mean ----

# extract samples
extract_samples = function(post) {
  f = rstan::extract(
    post
    , pars = 'f'
  )[[1]]
  
  # get GP of intercept
  condition1 = data.frame(f[,,1])
  condition1$sample = 1:nrow(condition1)
  f_1 = condition1 %>%
    gather(key="time", value="value", -sample) %>%
    dplyr::mutate(
      time= as.numeric(gsub('X','',time))
      , condition = 1
    )
  
  # get GP of condition 2
  condition2 = data.frame(f[,,2])
  condition2$sample = 1:nrow(condition2)
  f_2 = condition2 %>%
    gather(key="time", value="value", -sample) %>%
    dplyr::mutate(
      time= as.numeric(gsub('X','',time))
      , condition = 2
    )
  
  # get GP of conditions
  f_sum = rbind(f_1, f_2)
  f_sum %>%
    spread(condition, value) -> f_sum
  
  names(f_sum)[3:4] = c("condition1", "condition2")
  
  return(f_sum)
}

f_sum = extract_samples(post_1_05)

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
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = intercept), linetype = "longdash")+
  ylab('scaled position')+
  xlab('normalized time')+
  ggtitle('intercept')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

 to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2), color = "purple")+
  geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "purple", alpha=0.5)+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = effect), linetype = "longdash")+
  ylab('scaled position')+
  xlab('normalized time')+
  ggtitle('effect')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )


# Get Effect ----
f_01_sum = extract_samples(post_01)

f_01_sum %>%
  dplyr::group_by(sample, time) %>%
  dplyr::summarise(
    effect = condition1-condition2
  ) -> f_sum_effect

to_plot_effect = f_sum_effect %>%
  dplyr::group_by(
    time
  ) %>%
  dplyr::summarise(
    med_1 = median(effect)
    , lo95_1 = quantile(effect,.025)
    , hi95_1 = quantile(effect,.975)
    , lo50_1 = quantile(effect,.25)
    , hi50_1 = quantile(effect,.75)
  )

# plot effect
to_plot_effect %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "purple")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "purple", alpha=0.5)+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = effect), linetype = "longdash")+
  ylab('scaled position effect (vision - no-vision)')+
  xlab('normalized time')+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , panel.background = element_rect(fill = "white", color = "black")
  )

# compare effects 
to_plot_effect %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data = to_plot, aes(x=time, y=med_2), color = "red")+
  geom_ribbon(data = to_plot, aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "red", alpha=0.5)+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = effect), linetype = "longdash")+
  ylab('scaled position effect (vision - no-vision)')+
  xlab('normalized time')+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , panel.background = element_rect(fill = "white", color = "black")
  )



# Noise ----

extract_noise_samples = function(post) {
  noise_f = rstan::extract(
    post
    , pars = 'noise_f'
  )[[1]]
  
  # get GP of intercept
  noise_condition1 = data.frame(noise_f[,,1])
  noise_condition1$sample = 1:nrow(noise_condition1)
  noise_f_1 = noise_condition1 %>%
    gather(key="time", value="value", -sample) %>%
    dplyr::mutate(
      time= as.numeric(gsub('X','',time))
      , condition = 1
    )
  
  # get GP of condition 2
  noise_condition2 = data.frame(noise_f[,,2])
  noise_condition2$sample = 1:nrow(noise_condition2)
  noise_f_2 = noise_condition2 %>%
    gather(key="time", value="value", -sample) %>%
    dplyr::mutate(
      time= as.numeric(gsub('X','',time))
      , condition = 2
    )
  
  # get GP of conditions
  noise_f_sum = rbind(noise_f_1, noise_f_2)
  noise_f_sum %>%
    spread(condition, value) -> noise_f_sum
  
  names(noise_f_sum)[3:4] = c("condition1", "condition2")
  
  return(noise_f_sum)
}

noise_f_sum = extract_noise_samples(post_1_05)

noise_to_plot = noise_f_sum %>%
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

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data=df_noise_pop, aes(x=time/bin_width+1, y=intercept), linetype = "longdash")+
  ylab('log standard deviation')+
  xlab('time')+
  ggtitle('intercept')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2), color = "purple")+
  geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "purple", alpha=0.5)+
  geom_line(data=df_noise_pop, aes(x=time/bin_width+1, y=effect), linetype = "longdash")+
  ylab('log standard deviation')+
  xlab('time')+
  ggtitle('effect')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )


# Get Effect ----
noise_f_01_sum = extract_noise_samples(post_01)

# summarize to get difference
noise_f_01_sum %>%
  dplyr::group_by(sample, time) %>%
  dplyr::summarise(
    effect = condition1-condition2
  ) -> noise_f_sum_effect

noise_to_plot_effect = noise_f_sum_effect %>%
  dplyr::group_by(
    time
  ) %>%
  dplyr::summarise(
    med_1 = median(effect)
    , lo95_1 = quantile(effect,.025)
    , hi95_1 = quantile(effect,.975)
    , lo50_1 = quantile(effect,.25)
    , hi50_1 = quantile(effect,.75)
  )

noise_to_plot_effect %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "purple")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "purple", alpha=0.5)+
  geom_line(data=df_noise_pop, aes(x=time/bin_width+1, y=effect), linetype = "longdash")+
  ylab('log standard deviation effect (vision - no-vision)')+
  xlab('normalized time')+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , panel.background = element_rect(fill = "white", color = "black")
  )


# plot both together
noise_to_plot_effect %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data=noise_to_plot, aes(x=time, y=med_2), color = "red")+
  geom_ribbon(data=noise_to_plot, aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "red", alpha=0.5)+
  geom_line(data=df_noise_pop, aes(x=time/bin_width+1, y=effect), linetype = "longdash")+
  ylab('log standard deviation effect (vision - no-vision)')+
  xlab('normalized time')+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , panel.background = element_rect(fill = "white", color = "black")
  )

