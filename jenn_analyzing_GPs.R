# Load Pakages & Data ----
library(tidyverse)
library(rstan)
library(MASS)
library(reshape2)

rstan_options(auto_write = TRUE) 

# load a function to do some pre-computation on x
load("~/Documents/Experiments/Trajectory/Jenn Study/jenn_data/data_norm.Rdata")



# Examine Data ----
summary(df_long_clean)

head(df_long_clean)



# Visualize Data ----

# look at trial by trial for given coordinate and participant
df_long_clean %>%
  dplyr::filter(coordinate == "z", as.numeric(id) == 1) %>%
  ggplot()+
  geom_line(aes(x=frame, y=position, color = trial), alpha = 0.5)+
  xlab("normalized time")+
  ylab("position (x)")+
  facet_grid(.~target)+
  theme(legend.position = "none")

# we average over trials
df_long_clean %>%
  group_by(id, coordinate, target, cue, frame) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_long_avg

df_long_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_avg, color = id))+
  xlab("normalized time")+
  ylab("position (x)")+
  facet_grid(cue~target)

df_long_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_avg, color = id))+
  xlab("normalized time")+
  ylab("position (y)")+
  facet_grid(cue~target)

df_long_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_avg, color = id))+
  xlab("normalized time")+
  ylab("position (z)")+
  facet_grid(cue~target)

# now we average over participants
df_long_avg %>%
  group_by(coordinate, target, cue, frame) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg)
  ) -> df_long_grand_avg

df_long_grand_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(.~target)

df_long_grand_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(.~target)

df_long_grand_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(.~target)

# now we do 2D in space
df_long_grand_avg %>%
  spread(coordinate, position_grand_avg) -> for_2D 

for_2D %>%
  ggplot()+
  geom_line(aes(x=y, y=z, color = cue))+
  xlab("y")+
  ylab("z")+
  facet_grid(.~target)




# Binning Waveforms ----
# first select only the z coordinates
df_long_clean %>%
  dplyr::filter(coordinate == "z") -> df_long_cleanz

# rounding function
round0 = function(x,z){
  round(x/z,0)*z
}

# I average over bin to make analysis compute time reasonable
bin_width = 15
df_long_cleanz$time_lores = round0(df_long_cleanz$frame, bin_width)

df_long_cleanz %>%
  group_by(id, coordinate, trial, time_lores, target, cue) %>%  # this is important because it organizes by id
  dplyr::summarise(position_bin = mean(position)) -> df_long_cleanz

# plot trial by trial for given participant
df_long_cleanz %>%
  dplyr::filter(as.numeric(id) == 1) %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin, color=trial), alpha = 0.2)+
  facet_grid(cue~target)+
  theme(legend.position="none")

# average over trials
df_long_cleanz %>%
  group_by(id, coordinate, time_lores, cue, target) %>%
  dplyr::summarise(position_bin_avg = mean(position_bin)) -> df_id_meansz

df_id_meansz %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin_avg, color = id), alpha = 0.5)+
  facet_grid(cue~target)+
  xlab('time')+
  ylab('average binned position')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = 'black')
  )

# average over participants
df_id_meansz %>%
  group_by(coordinate, time_lores, cue, target) %>%
  dplyr::summarise(position_bin_grand_avg = mean(position_bin_avg)) -> df_condition_meansz

df_condition_meansz %>%
  ggplot()+
    geom_line(aes(x=time_lores, y=position_bin_grand_avg, color = cue))+
    xlab('time')+
    ylab('grand average binned position')+
    facet_grid(.~target)+
    theme_gray(base_size = 20)+
    theme(
      panel.grid.major = element_line(size = 0)
      , panel.grid.minor = element_line(size = 0)
      , legend.position = "none"
      , panel.background = element_rect(fill = "white", color = 'black')
    )


# Establish Prior ----
# volatility
# subj_volatility_sd
curve(dweibull(x, 2, 1), 0, 5, ylab = "density", xlab = "subject volatility sd")
# subj_volatility
curve(dnorm(x, 0, mean(rweibull(1000, 2, 1))), 0, 5, ylab = "density", xlab = "subject volatility")
# volatility
curve(dnorm(x, 0, 5), 0, 20, ylab = "density", xlab = "population volatility")

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
amplitudes = 5
volatilities = 5  # not any bigger than this

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
    geom_line(aes(x=time, y=position, color=sample), size = 2)+  
    ylim(c(-12, 12))+
    theme_gray(base_size = 20)+
    theme(
      panel.grid.major = element_line(size = 0)
      , panel.grid.minor = element_line(size = 0)
      , legend.position = "none"
      # , panel.background = element_rect(fill = "white")
      # , axis.ticks = element_blank()
      # , axis.text = element_blank()
      # , axis.title = element_blank()
    )



# Rstan ----
# get the sorted unique value for x
x = sort(unique(df_long_cleanz$time_lores))

# for each value in df_long_cleanz$x, get its index x
x_index = match(df_long_cleanz$time_lores,x)

# do something similar for the subjects 
s = sort(unique(df_long_cleanz$id))
s_index = match(df_long_cleanz$id, s)

# compute the model matrix
z = data.frame(
  condition1 = ifelse(df_long_cleanz$target == "0", 1, 0)
  , condition2 =ifelse(df_long_cleanz$target == "1", 1, 0)
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

maxi = max(data.frame(lapply(z_by_f_by_s, length)))

z_by_f_by_s_pad = array(0,dim=c(length(subj_obs), maxi ))
for (si in 1:length(subj_obs)) {
  z_by_f_by_s_pad[si,] = c(z_by_f_by_s[[si]], rep(0, maxi - subj_obs[si]))
}

y = scale(df_long_cleanz$position_bin)[,1]  # scaled to mean=0,sd=1
y_by_s = array(split(y, s_index))

y_by_s_pad = array(0,dim=c(length(subj_obs),maxi))
for (si in 1:length(subj_obs)) {
  y_by_s_pad[si,] = c(y_by_s[[si]], rep(0, maxi - subj_obs[si]))
}

# create the data list for stan
data_for_stan = list(
  n_y = nrow(df_long_cleanz)
  , y = y_by_s_pad
  , n_x = length(x)
  , x = (x-min(x))/(max(x)-min(x)) #scaled to min=0,max=1
  , x_index = x_index
  , n_subj = length(s)
  , maxi = maxi
  , n_z = ncol(z)
  , rows_z_unique = nrow(z_unique)
  , z_unique = z_unique
  , z_by_f_by_s_pad = z_by_f_by_s_pad
  , subj_obs = subj_obs
)

# package for cluster
save(data_for_stan, file = "~/Documents/Experiments/Trajectory/Jenn Study/jenn_data/data_for_stan.Rdata")

# see cluster_analysis
mod = rstan::stan_model("~/Documents/Experiments/Trajectory/Jenn Study/jenn_study/jenn_gp_regression.stan")
post_test = sampling(
  mod
  , data = data_for_stan
  , iter = 50
  , init = 0
  , chains = 4
  , cores = 4
  , verbose = T
  , control = list(
    max_treedepth = 12
    , adapt_delta = 0.95
    )
  , refresh = 1
  , include = F
  , pars = c(
    'f_normal01'
    , 'subj_f_normal01'
    , 'noise_f_normal01'
    , 'noise_subj_f_normal01'
  )
)



# Examine Results ----
# load stan fit object that was computed in the cloud
# I saved it as post_rt because I forgot to change the name from what was written down in Mike's version from which I adapted the code
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal_15/fake_proposal_post_500_15.rdata")

# look at pairs
pairs(post, pars = c("volatility", "noise_volatility", "amplitude", "noise_amplitude"))



###################################################################
####                      Overview                             ####
###################################################################

# how long did it take (in hours)?
sort(rowSums(get_elapsed_time(post)/60/60))

ezStan::stan_summary(
  from_stan = post
  , par = c('volatility','amplitude', 'noise_volatility', 'noise_amplitude')
)

ezStan::stan_summary(
  from_stan = post
  , par = c('subj_volatility_sd','subj_amplitude_sd', 'noise_subj_volatility_sd','noise_subj_amplitude_sd')
)

# condition samples 
ezStan::stan_summary(
  from_stan = post
  , par = c('f')
)

ezStan::stan_summary(
  from_stan = post
  , par = c('noise_f')
)

# look at subject-by-subject estimates
ezStan::stan_summary(
  from_stan = post
  , par = c('subj_f')
)

ezStan::stan_summary(
  from_stan = post
  , par = c('noise_subj_f')
)



###################################################################
####                 Violins                                   ####
###################################################################

# Functions ----
library(rstan)
library(coda)
# Highest Density Interval Functions
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
  
  print( get_95_HDI(subset(df, condition == "condition1")$value) )
  print( get_95_HDI(subset(df, condition == "condition2")$value)  )
 
  return(gg)
}


post_samples = rstan::extract(post)

# displacement volatilty
volatilities = data.frame(post_samples$volatility)
names(volatilities) = c("condition1", "condition2")
volatilities %>%
  gather(condition, value, condition1:condition2) -> volatilities

gg_volatilities = get_violin(volatilities, "volatility")

gg_volatilities+ 
  geom_segment(aes(x = 0.5, y = 1.12, xend = 1.5, yend = 1.12), linetype = 'dotted', size = 0.5)+ 
  geom_segment(aes(x = 1.5, y = 2.51, xend = 2.5, yend = 2.51), linetype = 'dotted', size = 0.5)

# displacement amplitude
amplitudes = data.frame(post_samples$amplitude)
names(amplitudes) = c("condition1", "condition2")
amplitudes %>%
  gather(condition, value, condition1:condition2) -> amplitudes

gg_amplitudes = get_violin(amplitudes, "amplitude")

gg_amplitudes+
  geom_segment(aes(x = 0.5, y = 0.80, xend = 1.5, yend = 0.80), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.35, xend = 2.5, yend = 1.35), linetype = 'dotted', size = 0.5)


# participant displacement volatilty sd
subj_volatility_sds = data.frame(post_samples$subj_volatility_sd)
names(subj_volatility_sds) = c("condition1", "condition2")
subj_volatility_sds %>%
  gather(condition, value, condition1:condition2) -> subj_volatility_sds

gg_volatility_sds = get_violin(subj_volatility_sds, "participant volatility sd")

gg_volatility_sds+
  geom_segment(aes(x = 0.5, y = 0.71, xend = 1.5, yend = 0.71), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.14, xend = 2.5, yend = 1.14), linetype = 'dotted', size = 0.5)


# participant displacement amplitude sd
subj_amplitude_sds = data.frame(post_samples$subj_amplitude_sd)
names(subj_amplitude_sds) = c("condition1", "condition2")
subj_amplitude_sds %>%
  gather(condition, value, condition1:condition2) -> subj_amplitude_sds

gg_amplitude_sds = get_violin(subj_amplitude_sds, "participant amplitude sd")

gg_amplitude_sds+
  geom_segment(aes(x = 0.5, y = 0.53, xend = 1.5, yend = 0.53), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.43, xend = 2.5, yend = 0.43), linetype = 'dotted', size = 0.5)

# noise volatilty
noise_volatilities = data.frame(post_samples$noise_volatility)
names(noise_volatilities) = c("condition1", "condition2")
noise_volatilities %>%
  gather(condition, value, condition1:condition2) -> noise_volatilities

gg_noise_volatilities = get_violin(noise_volatilities, "noise volatility")

gg_noise_volatilities+
  geom_segment(aes(x = 0.5, y = 0.85, xend = 1.5, yend = 0.85), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.81, xend = 2.5, yend = 1.81), linetype = 'dotted', size = 0.5)
# +scale_y_log10()
# +ylim(c(0,5))


# noise amplitude
noise_amplitudes = data.frame(post_samples$noise_amplitude)
names(noise_amplitudes) = c("condition1", "condition2")
noise_amplitudes %>%
  gather(condition, value, condition1:condition2) -> noise_amplitudes

gg_noise_amplitudes = get_violin(noise_amplitudes, "noise amplitude")

# NOTE: the scaling will affect this I believe!
gg_noise_amplitudes+
  geom_segment(aes(x = 0.5, y = 1.0, xend = 1.5, yend = 1.0), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.0, xend = 2.5, yend = 1.0), linetype = 'dotted', size = 0.5)

# participant noise volatilty sd
noise_subj_volatility_sds = data.frame(post_samples$noise_subj_volatility_sd)
names(noise_subj_volatility_sds) = c("condition1", "condition2")
noise_subj_volatility_sds %>%
  gather(condition, value, condition1:condition2) -> noise_subj_volatility_sds

gg_noise_volatility_sds = get_violin(noise_subj_volatility_sds, "noise participant volatility sd")

gg_noise_volatility_sds+
  geom_segment(aes(x = 0.5, y = 1.2, xend = 1.5, yend = 1.2), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.8, xend = 2.5, yend = 0.8), linetype = 'dotted', size = 0.5)

# participant noise amplitude sd
noise_subj_amplitude_sds = data.frame(post_samples$noise_subj_amplitude_sd)
names(noise_subj_amplitude_sds) = c("condition1", "condition2")
noise_subj_amplitude_sds %>%
  gather(condition, value, condition1:condition2) -> noise_subj_amplitude_sds

gg_noise_amplitude_sds = get_violin(noise_subj_amplitude_sds, "noise participant amplitude sd")

gg_noise_amplitude_sds+
  geom_segment(aes(x = 0.5, y = 0.43, xend = 1.5, yend = 0.43), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.53, xend = 2.5, yend = 0.53), linetype = 'dotted', size = 0.5)



###################################################################
####                 Subject Level                             ####
###################################################################

# Position ----
subj_f = rstan::extract(
  post
  , pars = 'subj_f'
)[[1]]

# intercept
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
df_subj = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal_15/fake_data_proposal_subj.rds")
df_subj %>%
  group_by(id, condition) %>%
  # dplyr::mutate(value = value - value[time == 0]) %>%
  spread(condition, value) -> df_subj

subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1, color = factor(id)))+
  geom_line(aes(x=time, y=hi95_1, color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_1, color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(df_id_meansz, condition == "condition1"), aes(x=time_lores/bin_width+1, y=position_bin_avg, group = id), size = 0.5, color = "gray50")+
  geom_line(data=df_subj, aes(x = time/bin_width+1, y = condition1, group = id), linetype = "longdash")+
  ylab('position')+
  xlab('time')+ 
  ggtitle('condition 1')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2, color = factor(id)))+
  geom_line(aes(x=time, y=hi95_2, color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_2, color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(df_id_meansz, condition == "condition2"), aes(x=time_lores/bin_width+1, y=position_bin_avg, group = id ),size = 0.5, color = "gray50")+
  geom_line(data=df_subj, aes(x = time/bin_width+1, y = condition2, group = id), linetype = "longdash")+
  ylab('position')+
  xlab('time')+ 
  ggtitle('condition 2')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )


# Noise ----
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
df_long_cleanz$position_bin_scaled = df_long_cleanz$position_bin

df_long_cleanz %>%
  group_by(id, time_lores, target) %>%
  dplyr::summarise(
    SD = log(sd(position_bin_scaled))
  ) -> noise_df_long_cleanz

# get population parameters
df_subj_noise = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal_15/fake_data_proposal_subj_noise.rds")
df_subj_noise %>%
  spread(condition, value) -> df_subj_noise

# I believe the binning has resulted in a loss in noise!
# also, it appears that scaling results in a loss in noise - which makes sense
noise_subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1, color = factor(id)))+
  geom_line(aes(x=time, y=hi95_1, color = factor(id)), linetype = "dashed")+
  geom_line(aes(x=time, y=lo95_1, color = factor(id)), linetype = "dashed")+
  geom_line(data = subset(noise_df_long_cleanz, condition == "condition1"), aes(x=time_lores/bin_width+1, y=SD, group = id), size = 0.5, color = "gray50")+
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
  geom_line(data = subset(noise_df_long_cleanz, condition == "condition2"), aes(x=time_lores/bin_width+1, y=SD, group = id), size = 0.5, color = "gray50")+
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



###################################################################
####                 Population Level                          ####
###################################################################

# Position ----
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
df_pop = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal_15/fake_data_proposal_pop.rds")
# # set to zero
# df_pop %>%
#   dplyr::mutate(
#     condition1 = condition1 - condition1[time == 0]
#     , condition2 = condition2 - condition2[time == 0]
#   ) -> df_pop

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_line(aes(x=time, y=hi95_1), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=lo95_1), linetype = "dashed", color = "turquoise")+
  geom_line(data=subset(df_condition_meansz, condition == "condition1"), aes(x=time_lores/bin_width+1, y=position_bin_grand_avg), size = 0.5, color = "grey50")+
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = condition1), linetype = "longdash")+
  ylab('position')+
  xlab('time')+
  ggtitle('condition 1')+
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
  geom_line(aes(x=time, y=hi95_2), linetype = "dashed", color = "red")+
  geom_line(aes(x=time, y=lo95_2), linetype = "dashed", color = "red")+
  geom_line(data=subset(df_condition_meansz, condition == "condition2"), aes(x=time_lores/bin_width+1, y=position_bin_grand_avg), size = 0.5, color = "grey50") +
  geom_line(data=df_pop, aes(x = time/bin_width+1, y = condition2), linetype = "longdash")+
  ylab('position')+
  xlab('time')+
  ggtitle('condition 2')+
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
  geom_line(aes(x=time, y=hi95_1), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=lo95_1), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=med_2), color = "red")+
  geom_line(aes(x=time, y=hi95_2), linetype = "dashed", color = "red")+
  geom_line(aes(x=time, y=lo95_2), linetype = "dashed", color = "red")+
  annotate("text", label = "condition 1", x = 3, y = -0.15, color = "turquoise")+
  annotate("text", label = "condition 2", x = 9, y = -1.25, color = "red")+
  ylab('position')+
  xlab('time')+
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

noise_df_long_cleanz %>%
  group_by(time_lores, condition) %>%
  dplyr::summarise(
    avg_SD = mean(SD)
  ) -> subj_noise

# load real population means
df_pop_noise = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Jenn Study/previous_analyses/fake_proposal_15/fake_data_proposal_pop_noise.rds")

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_line(aes(x=time, y=hi95_1), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=lo95_1), linetype = "dashed", color = "turquoise")+
  geom_line(data=subset(subj_noise, condition == "condition1"), aes(x=time_lores/bin_width+1, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x = time/bin_width+1, y = condition1), linetype = "longdash")+
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

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2), color = "red")+
  geom_line(aes(x=time, y=hi95_2), linetype = "dashed", color = "red")+
  geom_line(aes(x=time, y=lo95_2), linetype = "dashed", color = "red")+
  geom_line(data=subset(subj_noise, condition == "condition2"), aes(x=time_lores/bin_width+1, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x = time/bin_width+1, y = condition2), linetype = "longdash")+
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

# one against the other
noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_line(aes(x=time, y=hi95_1), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=lo95_1), linetype = "dashed", color = "turquoise")+
  geom_line(aes(x=time, y=med_2), color = "red")+
  geom_line(aes(x=time, y=hi95_2), linetype = "dashed", color = "red")+
  geom_line(aes(x=time, y=lo95_2), linetype = "dashed", color = "red")+
  ylab('log standard deviation')+
  xlab('time')+
  annotate("text", label = "condition 1", x = 3, y = -1.0, color = "turquoise")+
  annotate("text", label = "condition 2", x = 5, y = 0.0, color = "red")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

