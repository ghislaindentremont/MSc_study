# Load Pakages & Data ----

library(tidyverse)
library(rstan)
# library(ezStan)
library(MASS)
library(reshape2)
library(coda)

# load in the data
df_long_sim = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/comparing_contrasts/fake_data.rds")


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

# get the sorted unique value for x
x = sort(unique(df_long_sim$time_lores))

# for each value in df_long_sim$time_lores, get its index x
x_index = match(df_long_sim$time_lores,x)

# do something similar for the subjects 
s = sort(unique(df_long_sim$id))
s_index = match(df_long_sim$id, s)

# compute the model matrix
z = data.frame(
  condition1 = ifelse(df_long_sim$condition == "condition1", 1, 0)
  , condition2 =ifelse(df_long_sim$condition == "condition2", 1, 0)
)

# # compute the model matrix
# z = data.frame(
#   intercept = 1
#   , effect =ifelse(df_long_sim$condition == "condition1", 1/2, -1/2)
# )

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

# package for cluster
save(data_for_stan, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/comparing_contrasts/data_for_stan_01.RData")





# Examine Results ----

# load stan fit object that was computed in the cloud
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/comparing_contrasts/post_1_05.rdata")
post_1_05 = post  # copy post over to post_1_05
rm(post)

# overwrite post and save to different name again
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/comparing_contrasts/post_01.rdata")
post_01 = post
rm(post)



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

# load real population means
df_pop = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/comparing_contrasts/pop.rds")

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

# load real population means
df_pop_noise = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/comparing_contrasts/pop_noise.rds")

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data=df_pop_noise, aes(x=time/bin_width+1, y=intercept), linetype = "longdash")+
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
  geom_line(data=df_pop_noise, aes(x=time/bin_width+1, y=effect), linetype = "longdash")+
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
  geom_line(data=df_pop_noise, aes(x=time/bin_width+1, y=effect), linetype = "longdash")+
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
  geom_line(data=df_pop_noise, aes(x=time/bin_width+1, y=effect), linetype = "longdash")+
  ylab('log standard deviation effect (vision - no-vision)')+
  xlab('normalized time')+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , panel.background = element_rect(fill = "white", color = "black")
  )
