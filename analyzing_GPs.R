# Load Pakages & Data ----

library(tidyverse)
library(rstan)
# library(ezStan)
library(MASS)
library(reshape2)
library(ez)
library(coda)

# load in the data
df_long_sim = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_proposal_15/fake_data_proposal.rds")

# rename coordinate 'y', not 'z'
df_long_sim$coordinate = factor("y")



# Look at Data ----

#  summarize dataset
summary(df_long_sim)

# look at a few rows of dataset
head(df_long_sim)

# # set trial start to zero (for real data)
# df_long_sim %>%
#   group_by(id, trial, condition, coordinate) %>%
#   dplyr::mutate(position = position - position[time == 0]) -> df_long_sim

# function
plot_sim = function(ids_use, coor) {
  df_long_sim %>%
    dplyr::filter(as.numeric(id) %in% ids_use, coordinate == "y") %>%
    ggplot()+
    geom_line(aes(x=time, y=position, color=trial), alpha = 0.2)+
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
}

# plot participant data, for each trial
plot_sim(1:10)
plot_sim(11:20)


# average over trials
df_long_sim %>%
  group_by(id, coordinate, time, condition) %>%
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
  group_by(coordinate, time, condition) %>%
  dplyr::summarize(
    position_grand_avg = mean(position_avg)
  ) -> df_long_sim_grand_avg

# plot population mean trajectories
df_long_sim_grand_avg %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = condition))+
  ylab('grand average position')+
  theme_gray(base_size = 20)#+
  # theme(
  #   panel.grid.major = element_line(size = 0)
  #   , panel.grid.minor = element_line(size = 0)
  #   , legend.position = "none"
  #   , panel.background = element_rect(fill = "white", color = "black")
  # )




########################################################
####                   R^2                          ####
########################################################

# create kinematic marker-like variables
# what time points should be chosen? 
# for real data they all are before slightly past the half way mark
df_long_sim %>%
  dplyr::group_by(id, condition, trial, coordinate) %>%
  dplyr::summarize(
    T1 = position[round(time,1) ==  0.4]
    , T2 = position[round(time,1) == 0.6]
    , T3 = position[round(time,1) == 0.8]
    , END = position[round(time,1) == 1]
  ) -> df_rsq_lm

# for each participant and each condition extract R^2
df_rsq_anova2 = ddply(
  .data = df_rsq_lm
  , .variables = .(id, condition, coordinate)
  , .fun = function(x) {
    m_T1 = lm(END~T1, data=x)
    m_T2 = lm(END~T2, data=x)
    m_T3 = lm(END~T3, data=x)
    
    coor = unique(x$coordinate)
    id = unique(x$id)
    cond = unique(x$condition)
    
    # plot(END~T1, data = x)
    # title(main = sprintf("%s %s %s", id, coor, cond))
    # plot(END~T2, data = x)
    # title(main = sprintf("%s %s %s", id, coor, cond))
    # plot(END~T3, data = x)
    # title(main = sprintf("%s %s %s", id, coor, cond))
    
    R_T1 = summary(m_T1)$r.squared
    R_T2 = summary(m_T2)$r.squared
    R_T3 = summary(m_T3)$r.squared
    
    return(data.frame(T1 = R_T1, T2 = R_T2, T3 = R_T3))
  }
)

df_rsq_anova2 %>%
  gather("kinematic_marker", "R_sq", T1:T3) -> df_rsq_anova

df_rsq_anova$kinematic_marker = factor(df_rsq_anova$kinematic_marker, levels = c("T1", "T2", "T3"))


# Y ----
df_yrsq_anova = subset(df_rsq_anova, coordinate == "y")

# run anova on R^2 analysis
ezANOVA(
  df_yrsq_anova
  , dv = R_sq
  , wid = id
  , within = .(condition, kinematic_marker)
)
ezPlot(
  df_yrsq_anova
  , dv = R_sq
  , wid = id
  , within = .c(condition, kinematic_marker)
  , x = kinematic_marker
  , split = condition
  , do_bar = F
  , y_lab = "y R^2"
  , x_lab = "kinematic marker"
)

# NOTE: unclear how to interpret R^2 results for these fake data





########################################################
####        Gaussian Process Regression             ####
########################################################


# Binning Waveforms ----

# create rounding function
# round0 = function(x,z){
#   round(x/z,0)*z
# }

# specify bin width
bin_width = 1/15

# # round time data given specified bin width 
# df_long_sim$time_lores = round0(df_long_sim$time, bin_width)

# define a new column if no binning is required (fake data)
df_long_sim$time_lores = df_long_sim$time

# # bin the time axis and averaged over position values within a bin, within a trial
# df_long_sim %>% 
#   group_by(id, coordinate, trial, time_lores, condition) %>%
#   dplyr::summarise(position_bin = mean(position)) -> df_long_sim

# define a new column if no binning is required (fake data)
df_long_sim$position_bin = df_long_sim$position



# Visualize Priors ----

# in this section, one is intended to play around around with prior distributions 
# and also test different GP hyperparameters to see what sorts of GP samples are possible given those hyperparameters
# ultimately the code of this section aids in specifying reasonable priors for the data in question

# subj_volatility_sd
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject volatility sd")

# subj_volatility
curve(dcauchy(x, 0, mean(rweibull(1000, 2, 1))), 0, 5, ylab = "density", xlab = "subject volatility")

# volatility
curve(dcauchy(x, 0, 10), 0, 50, ylab = "density", xlab = "population volatility")

# subj_amplitude_sd
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject amplitude sd")

# subj_amplitude
curve(dnorm(x, 0, mean(rweibull(1000, 2, 1))), 0, 5, ylab = "density", xlab = "subject amplitude")

# amplitude
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "population amplitude")

# trajectory time points
xx = seq(0, 1, 0.001) 

# number of time points
n_x = length(xx)

# mean function
mu = rep(0, n_x)

# covariance matrix hyperparameters
amplitudes = 5
volatilities = 15  

# define covariance matrix
Sigmas = matrix(0, n_x, n_x)
for (i in 1:n_x){
  for (j in 1:n_x){
    Sigmas[i, j] = amplitudes^2*exp(-volatilities^2*(1/2)*(xx[i] - xx[j])^2)
  }
}

# number of samples of GP with covariance defined above
nn = 5

# sample from GP
fs = mvrnorm(nn, mu, Sigmas)

# transform to data frame
fs_df = data.frame(t(fs))

# reorganize data frame
fs_df %>%
  gather(sample, position, 1:nn, factor_key = T) -> fs_df

# add arbitrary time column
fs_df$time = rep(1:table(fs_df$sample)[[1]], nn)

# plot samples
fs_df %>%
  ggplot()+
    geom_line(aes(x=time, y=position, color=sample), size = 2)+  
    # xlab('time')+
    # ylab('position')+
    # ggtitle('volatility=5; amplitude=1')+
    ylim(c(-12, 12))+
    theme_gray(base_size = 20)+
    theme(
      panel.grid.major = element_line(size = 0)
      , panel.grid.minor = element_line(size = 0)
      , legend.position = "none"
      , panel.background = element_rect(fill = "white")
      , axis.ticks = element_blank()
      , axis.text = element_blank()
      , axis.title = element_blank()
    )



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
z_by_f_by_s_pad = array(0,dim=c(20,10000))
for (si in 1:length(subj_obs)) {
  z_by_f_by_s_pad[si,] = c(z_by_f_by_s[[si]], rep(0, 10000 - subj_obs[si]))
}

# # if real data
# y = scale(df_long_sim$position_bin)[,1]  # scaled to mean=0,sd=1

# if fake data
y = df_long_sim$position_bin

# get data specific to each participant
y_by_s = array(split(y, s_index))

# pad that data
y_by_s_pad = array(0,dim=c(20,10000))
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

# # package for cluster
# save(data_for_stan, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal_15/fake_stan_data_proposal.Rdata")

# # see 'cluster_analysis'
# mod = rstan::stan_model("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/jenn_study/gp_regression.stan")
# post_test = sampling(
#   mod
#   , data = data_for_stan
#   , iter = 100
#   , init = 0
#   , chains = 2
#   , cores = 2
#   , verbose = T
#   , control = list(
#     max_treedepth = 15
#     , adapt_delta = 0.99 
#     )
#   , refresh = 1
#   , include = F
#   , pars = c(
#     'f_normal01'
#     , 'subj_f_normal01'
#     , 'volatility_helper'
#     , 'subj_volatility_helper'
#     , 'noise_f_normal01'
#     , 'noise_subj_f_normal01'
#     , 'noise_volatility_helper'
#     , 'noise_subj_volatility_helper'
#   )
# )



# Examine Results ----

# load stan fit object that was computed in the cloud
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_proposal_15/fake_proposal_post_500_15.rdata")



####                      Overview                             

# how long did it take (in hours)?
sort(rowSums(get_elapsed_time(post)/60/60))

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
  from_stan = post
  , par = c('volatility','amplitude', 'noise_volatility', 'noise_amplitude')
)

# participant-to-participant variability estimates
stan_summary(
  from_stan = post
  , par = c('subj_volatility_sd','subj_amplitude_sd', 'noise_subj_volatility_sd','noise_subj_amplitude_sd')
)

# population mean function
stan_summary(
  from_stan = post
  , par = c('f')
)

# population noise function
stan_summary(
  from_stan = post
  , par = c('noise_f')
)

# participant mean functions
stan_summary(
  from_stan = post
  , par = c('subj_f')
)

# participant noise functions
stan_summary(
  from_stan = post
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

# get violin plots
get_violin = function(df, y_lab, samps = 16*500/2, hline = FALSE, facet = FALSE) {
  
  df$condition = factor(df$condition)
  
  gg = ggplot(data = df)+
    geom_violin(aes(x = condition, y = value))+
    labs(x = "", y = y_lab)+
    stat_summary(aes(x = condition, y = value), fun.data = get_95_HDI, size = 0.5)+
    stat_summary(aes(x = condition, y = value), fun.data = get_50_HDI, size = 1.5)
  
  if (hline) {
    gg = gg + geom_hline(yintercept = 0, linetype = 2, size = 1)
  }
  
  if (facet) {
    gg = gg + facet_wrap(~condition, scales = "free")
  } 
  
  gg = gg + theme_gray(base_size = 20)+
    theme(
      panel.grid.major = element_line(size = 1)
      , panel.grid.minor = element_line(size = 0.5)
      , strip.background = element_blank()
      , strip.text.x = element_blank() 
      , axis.ticks.x = element_blank() 
    ) 
  
  print(gg)
  
  # print( get_95_HDI(subset(df, condition == "condition1")$value) )
  # print( get_95_HDI(subset(df, condition == "condition2")$value)  )
 
  return(gg)
}



# Parameters ----

# extract posterior samples 
post_samples = rstan::extract(post)

# population mean volatilty
volatilities = data.frame(post_samples$volatility)
names(volatilities) = c("condition1", "condition2")

volatilities$effect = volatilities$condition1 - volatilities$condition2

volatilities %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> volatilities_c

gg_volatilities = get_violin(volatilities_c, "volatility")

gg_volatilities+ 
  geom_segment(aes(x = 0.5, y = 1.12, xend = 1.5, yend = 1.12), linetype = 'dotted', size = 0.5)+ 
  geom_segment(aes(x = 1.5, y = 2.51, xend = 2.5, yend = 2.51), linetype = 'dotted', size = 0.5)

volatilities %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> volatilities_e

gg_volatilities_effect = get_violin(volatilities_e, "volatility effect", hline = T)

gg_volatilities_effect+ 
  geom_segment(aes(x = 0.5, y = 1.12 - 2.51, xend = 1.5, yend = 1.12 - 2.51), linetype = 'dotted', size = 0.5)


# population mean amplitude
amplitudes = data.frame(post_samples$amplitude)
names(amplitudes) = c("condition1", "condition2")

amplitudes$effect = amplitudes$condition1 - amplitudes$condition2

amplitudes %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> amplitudes_c

gg_amplitudes = get_violin(amplitudes_c, "amplitude")

gg_amplitudes+ 
  geom_segment(aes(x = 0.5, y = 0.80, xend = 1.5, yend = 0.80), linetype = 'dotted', size = 0.5)+ 
  geom_segment(aes(x = 1.5, y = 1.35, xend = 2.5, yend = 1.35), linetype = 'dotted', size = 0.5)

amplitudes %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> amplitudes_e

gg_amplitudes_effect = get_violin(amplitudes_e, "amplitude effect", hline = T)

gg_amplitudes_effect+ 
  geom_segment(aes(x = 0.5, y = 0.80 - 1.35, xend = 1.5, yend = 0.80 - 1.35), linetype = 'dotted', size = 0.5)


# participant mean volatilty sd
subj_volatility_sds = data.frame(post_samples$subj_volatility_sd)
names(subj_volatility_sds) = c("condition1", "condition2")

subj_volatility_sds$effect = subj_volatility_sds$condition1 - subj_volatility_sds$condition2

subj_volatility_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> subj_volatility_sds_c

gg_volatility_sds = get_violin(subj_volatility_sds_c, "participant volatility sd")

gg_volatility_sds+
  geom_segment(aes(x = 0.5, y = 0.71, xend = 1.5, yend = 0.71), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.14, xend = 2.5, yend = 1.14), linetype = 'dotted', size = 0.5)

subj_volatility_sds %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> subj_volatility_sds_e

gg_volatility_sds_effect = get_violin(subj_volatility_sds_e, "participant volatility sd effect", hline = T)

gg_volatility_sds_effect+
  geom_segment(aes(x = 0.5, y = 0.71 - 1.14, xend = 1.5, yend = 0.71 - 1.14), linetype = 'dotted', size = 0.5)


# participant mean amplitude sd
subj_amplitude_sds = data.frame(post_samples$subj_amplitude_sd)
names(subj_amplitude_sds) = c("condition1", "condition2")

subj_amplitude_sds$effect = subj_amplitude_sds$condition1 - subj_amplitude_sds$condition2

subj_amplitude_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> subj_amplitude_sds_c

gg_amplitude_sds = get_violin(subj_amplitude_sds_c, "participant amplitude sd")

gg_amplitude_sds+
  geom_segment(aes(x = 0.5, y = 0.53, xend = 1.5, yend = 0.53), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.43, xend = 2.5, yend = 0.43), linetype = 'dotted', size = 0.5)

subj_amplitude_sds %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> subj_amplitude_sds_e

gg_amplitude_sds_effect = get_violin(subj_amplitude_sds_e, "participant amplitude sd effect", hline = T)

gg_amplitude_sds_effect+
  geom_segment(aes(x = 0.5, y = 0.53 - 0.43, xend = 1.5, yend = 0.53 - 0.43), linetype = 'dotted', size = 0.5)


# population noise volatilty
noise_volatilities = data.frame(post_samples$noise_volatility)
names(noise_volatilities) = c("condition1", "condition2")

noise_volatilities$effect = noise_volatilities$condition1 - noise_volatilities$condition2

noise_volatilities %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> noise_volatilities_c

gg_noise_volatilities = get_violin(noise_volatilities_c, "noise volatility")

gg_noise_volatilities+
  geom_segment(aes(x = 0.5, y = 0.85, xend = 1.5, yend = 0.85), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.81, xend = 2.5, yend = 1.81), linetype = 'dotted', size = 0.5)
# +scale_y_log10()
# +ylim(c(0,5))

noise_volatilities %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> noise_volatilities_e

gg_noise_volatilities_effect = get_violin(noise_volatilities_e, "noise volatility effect", hline = T)

gg_noise_volatilities_effect+
  geom_segment(aes(x = 0.5, y = 0.85-1.81, xend = 1.5, yend = 0.85-1.81), linetype = 'dotted', size = 0.5)
# +scale_y_log10()
# +ylim(c(0,5))


# population noise amplitude
noise_amplitudes = data.frame(post_samples$noise_amplitude)
names(noise_amplitudes) = c("condition1", "condition2")

noise_amplitudes$effect = noise_amplitudes$condition1 - noise_amplitudes$condition2

noise_amplitudes %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> noise_amplitudes_c

gg_noise_amplitudes = get_violin(noise_amplitudes_c, "noise amplitude")

gg_noise_amplitudes+
  geom_segment(aes(x = 0.5, y = 1, xend = 1.5, yend = 1), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1, xend = 2.5, yend = 1), linetype = 'dotted', size = 0.5)
# +scale_y_log10()
# +ylim(c(0,5))

noise_amplitudes %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> noise_amplitudes_e

gg_noise_amplitudes_effect = get_violin(noise_amplitudes_e, "noise amplitude effect", hline = T)

gg_noise_amplitudes_effect+
  geom_segment(aes(x = 0.5, y = 1-1, xend = 1.5, yend = 1-1), linetype = 'dotted', size = 0.5)
# +scale_y_log10()
# +ylim(c(0,5))


# participant noise volatilty sd
noise_subj_volatility_sds = data.frame(post_samples$noise_subj_volatility_sd)
names(noise_subj_volatility_sds) = c("condition1", "condition2")

noise_subj_volatility_sds$effect = noise_subj_volatility_sds$condition1 - noise_subj_volatility_sds$condition2

noise_subj_volatility_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> noise_subj_volatility_sds_c

gg_noise_volatility_sds = get_violin(noise_subj_volatility_sds_c, "noise participant volatility sd")

gg_noise_volatility_sds+
  geom_segment(aes(x = 0.5, y = 1.2, xend = 1.5, yend = 1.2), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.8, xend = 2.5, yend = 0.8), linetype = 'dotted', size = 0.5)

noise_subj_volatility_sds %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> noise_subj_volatility_sds_e

gg_noise_volatility_sds_effect = get_violin(noise_subj_volatility_sds_e, "noise participant volatility sd effect", hline = T)

gg_noise_volatility_sds_effect+
  geom_segment(aes(x = 0.5, y = 1.2-0.8, xend = 1.5, yend = 1.2-0.8), linetype = 'dotted', size = 0.5)


# participant noise amplitude sd
noise_subj_amplitude_sds = data.frame(post_samples$noise_subj_amplitude_sd)
names(noise_subj_amplitude_sds) = c("condition1", "condition2")

noise_subj_amplitude_sds$effect = noise_subj_amplitude_sds$condition1 - noise_subj_amplitude_sds$condition2

noise_subj_amplitude_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> noise_subj_amplitude_sds_c

gg_noise_amplitude_sds = get_violin(noise_subj_amplitude_sds_c, "noise participant amplitude sd")

gg_noise_amplitude_sds+
  geom_segment(aes(x = 0.5, y = 0.43, xend = 1.5, yend = 0.43), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.53, xend = 2.5, yend = 0.53), linetype = 'dotted', size = 0.5)

noise_subj_amplitude_sds %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> noise_subj_amplitude_sds_e

gg_noise_amplitude_sds_effect = get_violin(noise_subj_amplitude_sds_e, "noise participant amplitude sd effect", hline = T)

gg_noise_amplitude_sds_effect+
  geom_segment(aes(x = 0.5, y = 0.43-0.53, xend = 1.5, yend = 0.43-0.53), linetype = 'dotted', size = 0.5)




####                 Subject Level                       

# Mean ----

# extract samples
subj_f = rstan::extract(
  post
  , pars = 'subj_f'
)[[1]]

# condition 1
subj_condition1 = NULL
for (si in 1:dim(subj_f)[2]) {
  temp = data.frame(subj_f[,si,,1])
  temp$id = si
  temp$sample = 1:nrow(temp)
  subj_condition1 = rbind(subj_condition1, temp)
}

subj_f_1 = subj_condition1 %>%
  gather(key="time", value="value", -c(sample, id)) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , condition = 1
  )

# condition2
subj_condition2 = NULL
for (si in 1:dim(subj_f)[2]) {
  temp = data.frame(subj_f[,si,,2])
  temp$id = si
  temp$sample = 1:nrow(temp)
  subj_condition2 = rbind(subj_condition2, temp)
}

subj_f_2 = subj_condition2 %>%
  gather(key="time", value="value", -c(sample, id)) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , condition = 2
  )

# get GP of conditions
subj_f_sum = rbind(subj_f_1, subj_f_2)
subj_f_sum %>%
  spread(condition, value) -> subj_f_sum

names(subj_f_sum)[4:5] = c("condition1", "condition2")

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
  )

# load real population means
df_subj = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_proposal_15/fake_data_proposal_subj.rds")
df_subj %>%
  group_by(id, condition) %>%
  # dplyr::mutate(value = value - value[time == 0]) %>%
  spread(condition, value) -> df_subj

subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1, color = factor(id)))+
  geom_line(aes(x=time, y=hi95_1, color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_1, color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(df_long_sim_avg, condition == "condition1"), aes(x=time/bin_width+1, y=position_avg, group = id), size = 0.5, color = "gray50")+
  geom_line(data=df_subj, aes(x = time/bin_width+1, y = condition1, group = id), linetype = "longdash")+
  ylab('position')+
  ylim(c(-3, 2.5))+
  xlab('time')+
  ggtitle('condition 1')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  ) ->p1
print(p1)

# figure out participant details
p1cols = ggplot_build(p1)$data[[1]]

# just pick one subject
subj_to_plot %>%
  group_by(id) %>%
  dplyr::summarise(mins = min(med_1)) %>%
  dplyr::summarise(idx = which.min(mins))

subj_to_plot %>%
  dplyr::filter(id == 11, time >=8, time <=10) -> subj_to_plot_11

p1cols %>%
  dplyr::filter(group ==11, x == 1)

subj_to_plot_11 %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = '#00BFC4', size = 2)+
  geom_line(aes(x=time, y=hi95_1), color = '#00BFC4', linetype = "dashed", size = 2)+
  geom_line(aes(x=time, y=lo95_1), color = '#00BFC4', linetype = "dashed", size = 2)+
  geom_line(data = subset(df_long_sim_avg, condition == "condition1" & id == 11 & time/bin_width+1 >=8 & time/bin_width+1 <=10), aes(x=time/bin_width+1, y=position_avg, group = id), size = 2, color = "gray50")+
  geom_line(data = subset(df_subj, id == 11 & time/bin_width+1 >= 8 & time/bin_width+1 <= 10), aes(x = time/bin_width+1, y = condition1, group = id), size = 2, linetype = "longdash")+
  ylab('')+
  xlab('')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , axis.ticks = element_line(size = 0)
    , axis.text = element_blank()
    , legend.position = "none"
    , panel.border = element_rect(size = 2, fill = NA)
    , panel.background = element_rect(fill = "white", color = "black")
  )

# condition 2
subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2, color = factor(id)))+
  geom_line(aes(x=time, y=hi95_2, color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_2, color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(df_long_sim_avg, condition == "condition2"), aes(x=time/bin_width+1, y=position_avg, group = id ),size = 0.5, color = "gray50")+
  geom_line(data=df_subj, aes(x = time/bin_width+1, y = condition2, group = id), linetype = "longdash")+
  ylab('position')+
  ylim(c(-5, 2.5))+
  xlab('time')+
  ggtitle('condition 2')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  ) -> p2
print(p2)

# figure out participant details
p2cols = ggplot_build(p2)$data[[1]]

# just pick one subject
subj_to_plot %>%
  group_by(id) %>%
  dplyr::summarise(mins = min(med_2)) %>%
  dplyr::summarise(idx = which.min(mins))

subj_to_plot %>%
  dplyr::filter(id == 6, time >=1, time <=3) -> subj_to_plot_6

p1cols %>%
  dplyr::filter(group ==6, x == 1)

subj_to_plot_6 %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2), color = '#7CAE00', size = 2)+
  geom_line(aes(x=time, y=hi95_2), color = '#7CAE00', linetype = "dashed", size = 2)+
  geom_line(aes(x=time, y=lo95_2), color = '#7CAE00', linetype = "dashed", size = 2)+
  geom_line(data = subset(df_long_sim_avg, condition == "condition2" & id == 6 & time/bin_width+1 >=1 & time/bin_width+1 <=3), aes(x=time/bin_width+1, y=position_avg, group = id), size = 2, color = "gray50")+
  geom_line(data = subset(df_subj, id == 6 & time/bin_width+1 >=1 & time/bin_width+1 <=3), aes(x = time/bin_width+1, y = condition2, group = id), linetype = "longdash", size = 2)+
  ylab('')+
  xlab('')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , axis.ticks = element_line(size = 0)
    , axis.text = element_blank()
    , legend.position = "none"
    , panel.border = element_rect(size = 2, fill = NA)
    , panel.background = element_rect(fill = "white", color = "black")
  )



# Noise ----

# extract samples
noise_subj_f = rstan::extract(
  post
  , pars = 'noise_subj_f'
)[[1]]

# condition1
noise_subj_condition1 = NULL
for (si in 1:dim(noise_subj_f)[2]) {
  temp = data.frame(noise_subj_f[,si,,1])
  temp$id = si
  temp$sample = 1:nrow(temp)
  noise_subj_condition1 = rbind(noise_subj_condition1, temp)
}

noise_subj_f_1 = noise_subj_condition1 %>%
  gather(key="time", value="value", -c(sample, id)) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , condition = 1
)

# condition2
noise_subj_condition2 = NULL
for (si in 1:dim(noise_subj_f)[2]) {
  temp = data.frame(noise_subj_f[,si,,2])
  temp$id = si
  temp$sample = 1:nrow(temp)
  noise_subj_condition2 = rbind(noise_subj_condition2, temp)
}

noise_subj_f_2 = noise_subj_condition2 %>%
  gather(key="time", value="value", -c(sample, id)) %>%
  dplyr::mutate(
    time= as.numeric(gsub('X','',time))
    , condition = 2
  )

# get GP of conditions
noise_subj_f_sum = rbind(noise_subj_f_1, noise_subj_f_2)
noise_subj_f_sum %>%
  spread(condition, value) -> noise_subj_f_sum

names(noise_subj_f_sum)[4:5] = c("condition1", "condition2")

noise_subj_to_plot = noise_subj_f_sum %>%
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
  )

# get SDs
df_long_sim$position_bin_scaled = df_long_sim$position_bin

df_long_sim %>%
  group_by(id, time_lores, condition) %>%
  dplyr::summarise(
    SD = log(sd(position_bin_scaled))
  ) -> noise_df_long_sim

# get population parameters
df_subj_noise = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_proposal_15/fake_data_proposal_subj_noise.rds")
df_subj_noise %>%
  spread(condition, value) -> df_subj_noise

noise_subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1, color = factor(id)))+
  geom_line(aes(x=time, y=hi95_1, color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_1, color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(noise_df_long_sim, condition == "condition1"), aes(x=time_lores/bin_width+1, y=SD, group = id), size = 0.5, color = "gray50")+
  geom_line(data=df_subj_noise, aes(x = time/bin_width+1, y = condition1, group = id), linetype = "longdash")+
  ylab('log standard deviation')+
  xlab('time')+
  ggtitle('condition 1')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

noise_subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2, color = factor(id)))+
  geom_line(aes(x=time, y=hi95_2, color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_2, color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(noise_df_long_sim, condition == "condition2"), aes(x=time_lores/bin_width+1, y=SD, group = id), size = 0.5, color = "gray50")+
  geom_line(data=df_subj_noise, aes(x = time/bin_width+1, y = condition2, group = id), linetype = "longdash")+
  ylab('log standard deviation')+
  xlab('time')+
  ggtitle('condition 2')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )





#-----------------------------------------------------------------#
####                  Population Level                         ####
#-----------------------------------------------------------------#


# Mean ----

# extract samples
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
df_pop = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_proposal_15/fake_data_proposal_pop.rds")

# # if real data, then set to zero
# df_pop %>%
#   dplyr::mutate(
#     condition1 = condition1 - condition1[time == 0]
#     , condition2 = condition2 - condition2[time == 0]
#   ) -> df_pop

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data=subset(df_long_sim_grand_avg, condition == "condition1"), aes(x=time/bin_width+1, y=position_grand_avg), size = 0.5, color = "grey50")+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = condition1), linetype = "longdash")+
  ylab('scaled position')+
  xlab('normalized time')+
  ggtitle('condition1')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2), color = "red")+
  geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "red", alpha=0.5)+
  geom_line(data=subset(df_long_sim_grand_avg, condition == "condition2"), aes(x=time/bin_width+1, y=position_grand_avg), size = 0.5, color = "grey50")+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = condition2), linetype = "longdash")+
  ylab('scaled position')+
  xlab('normalized time')+
  ggtitle('condition2')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

# and against one another
to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(aes(x=time, y=med_2), color = "red")+
  geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "red", alpha=0.5)+
  annotate("text", label = "condition1", x = 2, y = -0.15, color = "turquoise")+
  annotate("text", label = "condition2", x = 6, y = -0.75, color = "red")+
  ylab('scaled position')+
  xlab('normalized time')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , panel.background = element_rect(fill = "white", color = "black")
  )


# summarize to get difference
f_sum %>%
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

# get effect
df_pop$effect = df_pop$condition1 - df_pop$condition2

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




# Noise ----
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

noise_df_long_sim %>%
  group_by(time_lores, condition) %>%
  dplyr::summarise(
    avg_SD = mean(SD)
  ) -> subj_noise

# load real population means
df_pop_noise = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_proposal_15/fake_data_proposal_pop_noise.rds")

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data=subset(subj_noise, condition == "condition1"), aes(x=time_lores/bin_width+1, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x=time/bin_width+1, y=condition1), linetype = "longdash")+
  ylab('log standard deviation')+
  xlab('time')+
  ggtitle('condition1')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2), color = "red")+
  geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "red", alpha=0.5)+
  geom_line(data=subset(subj_noise, condition == "condition2"), aes(x=time_lores/bin_width+1, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x=time/bin_width+1, y=condition2), linetype = "longdash")+
  ylab('log standard deviation')+
  xlab('time')+
  ggtitle('condition2')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

# one against the other
noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1),fill = "turquoise", alpha=0.5)+
  geom_line(aes(x=time, y=med_2), color = "red")+
  geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "red", alpha=0.5)+
  ylab('log standard deviation')+
  xlab('time')+
  annotate("text", label = "condition1", x = 3, y = -1.0, color = "turquoise")+
  annotate("text", label = "condition2", x = 5, y = 0, color = "red")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

# summarize to get difference
noise_f_sum %>%
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

# get true population effect
df_pop_noise$effect = df_pop_noise$condition1 - df_pop_noise$condition2

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


