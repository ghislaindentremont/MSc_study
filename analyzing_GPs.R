# Load Pakages & Data ----

library(tidyverse)
library(rstan)
# library(ezStan)
library(MASS)
library(reshape2)
library(ez)
library(coda)
library(plyr)

# load in the data
df_long_sim = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal.rds")

# rename coordinate 'y', not 'z'
df_long_sim$coordinate = factor("y")



# Look at Data ----

#  summarize dataset
summary(df_long_sim)

# look at a few rows of dataset
head(df_long_sim)

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
plot_sim(21:29)


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
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )




########################################################
####                   R^2                          ####
########################################################

num_points = length(unique(df_long_sim$time))

# for each participant and each condition extract R^2
df_rsq_anova = ddply(
  .data = df_long_sim
  , .variables = .(id, condition, coordinate)
  , .fun = function(x) {
    
    Rs = NULL
    for (point in 1:num_points) {
      
      X = x$position[x$time == x$time[point]]
      Y = x$position[x$time == x$time[num_points]]
      
      m = lm(Y~X)
      
      R_m = summary(m)$r.squared
      
      Rs = c(Rs, R_m)
    }

    return(data.frame(R_sq = Rs, time = unique(x$time)))
  }
)

df_rsq_anova$time = factor(round(df_rsq_anova$time, 2))


# Y ----
df_yrsq_anova = subset(df_rsq_anova, coordinate == "y" & time != 1)

df_yrsq_anova$condition = revalue(df_yrsq_anova$condition, c("condition1"="Condition1", "condition2" = "Condition2"))

# change factor order
df_yrsq_anova$condition = factor(df_yrsq_anova$condition, c("Condition1", "Condition2"))

# run anova on R^2 analysis
ezANOVA(
  df_yrsq_anova
  , dv = R_sq
  , wid = id
  , within = .(condition, time)
)
ezPlot(
  df_yrsq_anova
  , dv = R_sq
  , wid = id
  , within = .c(condition, time)
  , x = time
  , split = condition
  , do_bar = T
  , y_lab = "R Squared"
  , x_lab = "Proportion of Movement"
  , split_lab = "Condition"
)
# NOTE: unclear how to interpret R^2 results for these fake data







##########################################################
####          Spatial Variability Profiles            ####
##########################################################

df_long_sim %>%
  dplyr::group_by(id, condition, coordinate, time) %>%
  dplyr::summarise(spat_var = sd(position)) -> df_long_spat_var

# function to plot spatial variability
plot_spat_var = function(ids_use, dv, y_label) {

  df_long_spat_var %>%
    dplyr::filter(as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=time, y=spat_var, group=id, color=id), alpha = 0.5, size = .5)+
    xlab("normalized time")+
    ylab(y_label)+
    facet_grid(.~condition) %>% print()
}

# Y
plot_spat_var(1:10, "y_inter", "y spatial variability (sd)")
plot_spat_var(11:20, "y_inter", "y spatial variability (sd)")

# grand averages
df_long_spat_var %>%
  dplyr::group_by(condition, coordinate, time) %>%
  dplyr::summarise(spat_var_avg = mean(spat_var)) -> df_long_spat_var_avg


# plot grand averages
df_long_spat_var_avg %>%
  ggplot()+
  geom_line(aes(x=time, y=spat_var_avg, group=condition, color=condition))+
  xlab("time")+
  ylab("position") %>% print()


# ANOVA ----
df_spat_var_anova = df_long_spat_var

df_spat_var_anova$time = factor(round(df_spat_var_anova$time,2))


# Y ----
df_yspat_var_anova = subset(df_spat_var_anova, coordinate == "y")

df_yspat_var_anova$condition = revalue(df_yspat_var_anova$condition, c("condition1"="Condition1", "condition2" = "Condition2"))

# change factor order
df_yspat_var_anova$condition = factor(df_yspat_var_anova$condition, c("Condition1", "Condition2"))

# get population noise
df_pop_noise = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal_pop_noise.rds")

# we cannot oversample because in this case we do not have that many samples 
num_samples = 15

ezANOVA(
  df_yspat_var_anova
  , dv = spat_var
  , wid = id
  , within = .(condition, time)
)
spatplot = ezPlot(
  df_yspat_var_anova
  , dv = spat_var
  , wid = id
  , within = .c(condition, time)
  , x = time
  , split = condition
  , do_bar = T
  , y_lab = "Spatial Variability (SD)"
  , x_lab = "Proportion of Movement"
  , split_lab = "Condition"
)
spatplot+
  geom_line(data=df_pop_noise, aes(x = time*(num_samples-1)+1, y = exp(condition1)), linetype = "longdash")+
  geom_line(data=df_pop_noise, aes(x = time*(num_samples-1)+1, y = exp(condition2)), linetype = "longdash")
  




########################################################
####               Spline Method                    ####
########################################################

run_custom_spline = T

round0 = function(x,z){
  round(x/z,0)*z
}

bin_width = 1/(num_samples-1)

if (run_custom_spline) {
  # NOTE: df_spline should be nearly identical to df_long_sim
  # NOTE: in the case of time normalization with so few samples there is no point in this
  df_spline = ddply(
    .data = df_long_sim
    , .variables = .(id, condition, trial, coordinate)
    , .fun = function(x){
      
      temp = smooth.spline(x$time, x$position, spar = 0.4)  # 4th order (i.e. cubic)
      
      the_bin_width = max(temp$x)/(num_samples-1)
      
      # between 0 and 1
      time_over = seq(0, max(temp$x), length.out = num_samples)
      
      time_round = unique(round0(time_over, the_bin_width))
      
      # actually get values
      pred = predict(temp, time_round)
      
      return(data.frame(time = pred$x, value = pred$y, norm_idx = 1:length(pred$x)))
    }
  )
} else{
  df_spline = df_long_sim
  df_spline$norm_idx = 1:num_samples
  names(df_spline)[5] = c("value")
}


# average over trials
df_spline %>%
  dplyr::group_by(id, condition, coordinate, norm_idx) %>%
  dplyr::summarize(
    value = mean(value)
  ) -> df_spline_avg

# average over participants
df_spline_avg %>%
  dplyr::group_by(condition, coordinate, norm_idx) %>%
  dplyr::summarize(
    value = mean(value)
  ) -> df_spline_grand_avg


df_spline_grand_avg$Condition = df_spline_grand_avg$condition
df_spline_grand_avg$Condition = revalue(df_spline_grand_avg$Condition, c("condition1"="Condition1", "condition2" = "Condition2"))
df_spline_grand_avg$Condition = factor(df_spline_grand_avg$Condition, c("Condition1", "Condition2"))

# get true population functions
df_pop = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal_pop.rds")

# function
df_spline_grand_avg %>%
  ggplot()+
  geom_line(aes(x=(norm_idx-1)/(num_samples-1), y=value, group=Condition, color=Condition))+
  labs(x = "Proportion of Movement", y = "Y Position", color = "Condition")+
  geom_line(data=df_pop, aes(x = time, y = condition1), linetype = "longdash")+
  geom_line(data=df_pop, aes(x = time, y = condition2), linetype = "longdash")
  

# take difference 
df_spline_avg %>%
  dplyr::group_by(id, coordinate, norm_idx) %>%
  dplyr::summarize(
    effect = -diff(value)
  ) -> df_spline_avg_effect

# get sample size 
n = length(unique(df_long_sim$id))

# average over differences
df_spline_avg_effect %>%
  dplyr::group_by(coordinate, norm_idx) %>%
  dplyr::summarize(
    M = mean(effect)
    , SD = sd(effect)
    , SE = SD/sqrt(n)
    , pval = (1-pt(abs(M/SE), n-1))*2  # two-sided
    , CI_hi = M + SE*qt(1-0.025, n-1)
    , CI_lo = M - SE*qt(1-0.025, n-1)
  ) -> df_spline_grand_avg_effect

# where is it significant?
df_spline_grand_avg_effect %>% 
  dplyr::mutate(
    sigcheck = CI_lo > 0 | CI_hi < 0
    , sig = pval < 0.05
  ) -> df_spline_grand_avg_effect

# check p-value calculation
mean(df_spline_grand_avg_effect$sig == df_spline_grand_avg_effect$sigcheck)

# function
df_spline_grand_avg_effect %>%
  ggplot()+
  geom_line(aes(x=norm_idx, y=M), color = "purple")+
  geom_ribbon(aes(x=norm_idx, ymin=CI_lo, ymax=CI_hi), fill = "purple", alpha=0.5)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  # geom_line(data=df_pop, aes(x = time/bin_width+1, y = effect), linetype = "longdash")+
  xlab("time")+
  ylab("y position effect (condition1 - condition2)")+
  theme_gray(base_size = 20)+
    theme(
      panel.grid.major = element_line(size = 0)
      , panel.grid.minor = element_line(size = 0)
      , panel.background = element_rect(fill = "white", color = "black")
    )


# For MATLAB ----
df_long_sim %>%
  dplyr::select(id, condition, trial, coordinate, time, position) %>%
  spread(coordinate, position) -> df_for_matlab2

df_for_matlab2 %>%
  dplyr::group_by() %>%
  dplyr::mutate(
    id = as.numeric(id)
    , condition = as.numeric(condition)
  ) -> df_for_matlab

# convert trial to numeric
df_for_matlab$trial = as.numeric(df_for_matlab$trial)

# convert to matrix
mat_long_trim = as.matrix(df_for_matlab[,c("id", "condition", "trial", "y")])

# # save matrix as .mat file
# write.csv(mat_long_trim, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/FDA/fake15_mat_long_trim.csv")

# where is it significant? 
df_spline_grand_avg_effect$sig

# From MATLAB ----
display_fanovan = function(filename, frames) {
  # p-values
  py = read.csv(sprintf('%s/py.csv', filename), header = F)
  names(py) = c("p_value")

  # y
  y_nv = read.csv(sprintf('%s/y_nv.csv', filename), header = F)
  names(y_nv) = c("y")
  y_nv$condition = "no_vision"
  y_v = read.csv(sprintf('%s/y_v.csv', filename), header = F)
  names(y_v) = c("y")
  y_v$condition = "vision"

  # MSEs
  y_MSE = read.csv(sprintf('%s/msy.csv', filename), header = F)
  names(y_MSE) = c("mse")


  # create data frames
  nv = y_nv
  v = y_v

  fanovan =  rbind(nv, v)

  fanovan$time = 0:(frames-1)/(frames-1)

  # when is it significant?
  py$sig = py$p_value < 0.05

  # add to fanovan
  fanovan$py_sig = py$sig

  # how significant is it?
  fanovan$py_val = py$p_value

  # MSEs
  fanovan$y_SE = sqrt(y_MSE$mse/n)

  # t-value
  tval = qt(1-0.025, n-1)

  # change names and order of levels
  fanovan$condition = revalue(fanovan$condition, c("no_vision"="Condition1", "vision" = "Condition2"))
  fanovan$condition = factor(fanovan$condition, c("Condition1", "Condition2"))

  for_plot1 = subset(fanovan, condition == "Condition1")
  for_plot2 = subset(fanovan, condition == "Condition2")
  # y vs. time
  fanovan %>%
    ggplot()+
    geom_path(aes(x=time, y=y, group=condition, color=condition))+
    # geom_area(data = subset(fanovan, condition == "V"), aes(x=time, y=as.numeric(py_sig)*max(y+y_SE*tval)), alpha = 0.2)+
    # geom_area(data = subset(fanovan, condition == "V"), aes(x=time, y=as.numeric(py_sig)*min(y-y_SE*tval)), alpha = 0.2)+
    annotate(
      "rect"
      , xmin=for_plot1$time[1]
      , xmax=for_plot1$time[8]+1/(frames-1)/2
      , ymin=min(for_plot2$y-for_plot2$y_SE*tval)
      , ymax=max(for_plot1$y+for_plot1$y_SE*tval)
      , alpha=0.2
      )+ 
    geom_ribbon(aes(x=time, ymin=y-y_SE*tval, ymax=y+y_SE*tval, group=condition, fill=condition), alpha = 0.2)+
    geom_line(data=df_pop, aes(x = time, y = condition1), linetype = "longdash")+
    geom_line(data=df_pop, aes(x = time, y = condition2), linetype = "longdash")+
    labs(x = "Proportion of Movement", y = "Y Position", color = "Condition", fill = "Condition") -> gg
  print(gg)

  return(fanovan)
}

# run fanovan
# fanovan_norm_time = display_fanovan('/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_results/fake_fanovan_norm_time_200', frames = 200)
# fanovan_norm_time = display_fanovan('/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_results/fake_fanovan_norm_time_16', frames = num_samples)
fanovan_norm_time = display_fanovan('/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_results/fake_fanovan_norm_time_15', frames = num_samples)

# compare my functional anova analysis with fanovan
df_spline_grand_avg_effecty = df_spline_grand_avg_effect[df_spline_grand_avg_effect$coordinate == "y",]

# the analyses produce identical results
plot(df_spline_grand_avg_effecty$sig - fanovan_norm_time[1:num_samples,]$py_sig, ylab = "y significance match")

# NOTE: the p-values are somewhat different!
# NOTE: this is likely because the normalization has a smoothing effect that only makes 
# a difference for the fake_thesis data and not the fake_proposal data because the former is noisier!
plot(df_spline_grand_avg_effecty$pval, ylab = "y p-value", ty = "l", col = "red")
lines(fanovan_norm_time[1:num_samples,]$py_val, col = "blue")






########################################################
####        Gaussian Process Regression             ####
########################################################


# Binning Waveforms ----

# create rounding function
# round0 = function(x,z){
#   round(x/z,0)*z
# }

# define a new column if no binning is required (fake data)
df_long_sim$time_lores = df_long_sim$time

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
z_by_f_by_s_pad = array(0,dim=c(n,10000))
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
y_by_s_pad = array(0,dim=c(n,10000))
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
# save(data_for_stan, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_stan_data_thesis.Rdata")

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
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_thesis_post_1000_15_mtd15.rdata")



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
f_print = stan_summary(
  from_stan = post
  , par = c('f')
)

# population noise function
stan_summary(
  from_stan = post
  , par = c('noise_f')
)

# participant mean functions
subj_f_print = stan_summary(
  from_stan = post
  , par = c('subj_f')
)
subj_f_print = data.frame(subj_f_print)
hist(subj_f_print$Rhat)
range(subj_f_print$Rhat)
hist(subj_f_print$n_eff)
range(subj_f_print$n_eff)

# participant noise functions
noise_subj_f_print = stan_summary(
  from_stan = post
  , par = c('noise_subj_f')
)
noise_subj_f_print = data.frame(noise_subj_f_print)
hist(noise_subj_f_print$Rhat)
range(noise_subj_f_print$Rhat)
hist(noise_subj_f_print$n_eff)
range(noise_subj_f_print$n_eff)





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
get_violin = function(df, y_lab, iseffect = F, hline = FALSE, facet = FALSE) {
  
  df$condition = factor(df$condition)
  
  if (iseffect) {
    df$condition = revalue(df$condition, c("effect" = "C1 - C2"))
    x_lab = "Condition Effect"
  }
  else  {
    df$condition = revalue(df$condition, c("condition1" = "C1", "condition2" = "C2"))
    df$condition = factor(df$condition, c("C1", "C2"))
    x_lab = "Condition"
  }
  
  gg = ggplot(data = df)+
    geom_violin(aes(x = condition, y = value))+
    labs(x = x_lab, y = y_lab)+
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

gg_volatilities = get_violin(volatilities_c, "Volatility")

gg_volatilities+
  geom_segment(aes(x = 0.5, y = 1.12, xend = 1.5, yend = 1.12), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 2.51, xend = 2.5, yend = 2.51), linetype = 'dotted', size = 0.5)

volatilities %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> volatilities_e

gg_volatilities_effect = get_violin(volatilities_e, "Volatility", hline = T, iseffect = T)

gg_volatilities_effect+
  geom_segment(aes(x = 0.5, y = 1.12 - 2.51, xend = 1.5, yend = 1.12 - 2.51), linetype = 'dotted', size = 0.5)


# population mean amplitude
amplitudes = data.frame(post_samples$amplitude)
names(amplitudes) = c("condition1", "condition2")

amplitudes$effect = amplitudes$condition1 - amplitudes$condition2

amplitudes %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> amplitudes_c

gg_amplitudes = get_violin(amplitudes_c, "Amplitude")

gg_amplitudes+
  geom_segment(aes(x = 0.5, y = 0.80, xend = 1.5, yend = 0.80), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.35, xend = 2.5, yend = 1.35), linetype = 'dotted', size = 0.5)

amplitudes %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> amplitudes_e

gg_amplitudes_effect = get_violin(amplitudes_e, "Amplitude", hline = T, iseffect = T)

gg_amplitudes_effect+
  geom_segment(aes(x = 0.5, y = 0.80 - 1.35, xend = 1.5, yend = 0.80 - 1.35), linetype = 'dotted', size = 0.5)


# participant mean volatilty sd
subj_volatility_sds = data.frame(post_samples$subj_volatility_sd)
names(subj_volatility_sds) = c("condition1", "condition2")

subj_volatility_sds$effect = subj_volatility_sds$condition1 - subj_volatility_sds$condition2

subj_volatility_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> subj_volatility_sds_c

gg_volatility_sds = get_violin(subj_volatility_sds_c, "Volatility Variability (SD)")

gg_volatility_sds+
  geom_segment(aes(x = 0.5, y = 0.71, xend = 1.5, yend = 0.71), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.14, xend = 2.5, yend = 1.14), linetype = 'dotted', size = 0.5)

subj_volatility_sds %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> subj_volatility_sds_e

gg_volatility_sds_effect = get_violin(subj_volatility_sds_e, "Volatility Variability (SD)", hline = T, iseffect = T)

gg_volatility_sds_effect+
  geom_segment(aes(x = 0.5, y = 0.71 - 1.14, xend = 1.5, yend = 0.71 - 1.14), linetype = 'dotted', size = 0.5)


# participant mean amplitude sd
subj_amplitude_sds = data.frame(post_samples$subj_amplitude_sd)
names(subj_amplitude_sds) = c("condition1", "condition2")

subj_amplitude_sds$effect = subj_amplitude_sds$condition1 - subj_amplitude_sds$condition2

subj_amplitude_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> subj_amplitude_sds_c

gg_amplitude_sds = get_violin(subj_amplitude_sds_c, "Amplitude Variability (SD)")

gg_amplitude_sds+
  geom_segment(aes(x = 0.5, y = 0.53, xend = 1.5, yend = 0.53), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.43, xend = 2.5, yend = 0.43), linetype = 'dotted', size = 0.5)

subj_amplitude_sds %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> subj_amplitude_sds_e

gg_amplitude_sds_effect = get_violin(subj_amplitude_sds_e, "Amplitude Variability (SD)", hline = T, iseffect = T)

gg_amplitude_sds_effect+
  geom_segment(aes(x = 0.5, y = 0.53 - 0.43, xend = 1.5, yend = 0.53 - 0.43), linetype = 'dotted', size = 0.5)


# population noise volatilty
noise_volatilities = data.frame(post_samples$noise_volatility)
names(noise_volatilities) = c("condition1", "condition2")

noise_volatilities$effect = noise_volatilities$condition1 - noise_volatilities$condition2

noise_volatilities %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> noise_volatilities_c

gg_noise_volatilities = get_violin(noise_volatilities_c, "Noise Volatility")

gg_noise_volatilities+
  geom_segment(aes(x = 0.5, y = 0.85, xend = 1.5, yend = 0.85), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1.81, xend = 2.5, yend = 1.81), linetype = 'dotted', size = 0.5)
# +scale_y_log10()
# +ylim(c(0,5))

noise_volatilities %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> noise_volatilities_e

gg_noise_volatilities_effect = get_violin(noise_volatilities_e, "Noise Volatility", hline = T, iseffect = T)

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

gg_noise_amplitudes = get_violin(noise_amplitudes_c, "Noise Amplitude")

gg_noise_amplitudes+
  geom_segment(aes(x = 0.5, y = 1, xend = 1.5, yend = 1), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 1, xend = 2.5, yend = 1), linetype = 'dotted', size = 0.5)
# +scale_y_log10()
# +ylim(c(0,5))

noise_amplitudes %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> noise_amplitudes_e

gg_noise_amplitudes_effect = get_violin(noise_amplitudes_e, "Noise Amplitude", hline = T, iseffect = T)

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

gg_noise_volatility_sds = get_violin(noise_subj_volatility_sds_c, "Noise Volatility Variability (SD)")

gg_noise_volatility_sds+
  geom_segment(aes(x = 0.5, y = 1.2, xend = 1.5, yend = 1.2), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.8, xend = 2.5, yend = 0.8), linetype = 'dotted', size = 0.5)

noise_subj_volatility_sds %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> noise_subj_volatility_sds_e

gg_noise_volatility_sds_effect = get_violin(noise_subj_volatility_sds_e, "Noise Volatility Variability (SD)", iseffect = T, hline = T)

gg_noise_volatility_sds_effect+
  geom_segment(aes(x = 0.5, y = 1.2-0.8, xend = 1.5, yend = 1.2-0.8), linetype = 'dotted', size = 0.5)


# participant noise amplitude sd
noise_subj_amplitude_sds = data.frame(post_samples$noise_subj_amplitude_sd)
names(noise_subj_amplitude_sds) = c("condition1", "condition2")

noise_subj_amplitude_sds$effect = noise_subj_amplitude_sds$condition1 - noise_subj_amplitude_sds$condition2

noise_subj_amplitude_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, condition1:condition2) -> noise_subj_amplitude_sds_c

gg_noise_amplitude_sds = get_violin(noise_subj_amplitude_sds_c, "Noise Amplitude Variability (SD)")

gg_noise_amplitude_sds+
  geom_segment(aes(x = 0.5, y = 0.43, xend = 1.5, yend = 0.43), linetype = 'dotted', size = 0.5)+
  geom_segment(aes(x = 1.5, y = 0.53, xend = 2.5, yend = 0.53), linetype = 'dotted', size = 0.5)

noise_subj_amplitude_sds %>%
  dplyr::select(-c(condition1, condition2)) %>%
  gather(condition, value, effect) -> noise_subj_amplitude_sds_e

gg_noise_amplitude_sds_effect = get_violin(noise_subj_amplitude_sds_e, "Noise Amplitude Variability (SD)", iseffect = T, hline = T)

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
    time= (as.numeric(gsub('X','',time))-1)*bin_width
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
    time= (as.numeric(gsub('X','',time))-1)*bin_width
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

# get population parameters
df_subj= readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal_subj.rds")
df_subj %>%
  spread(condition, value) -> df_subj

by_subj = function(id_lo, id_hi) {
  subj_to_plot %>%
    dplyr::filter(as.numeric(id) <= id_hi, as.numeric(id) >= id_lo) %>%
    ggplot()+
    geom_line(aes(x=time, y=med_1, group = factor(id)), color = "turquoise")+
    geom_line(aes(x=time, y=hi95_1, group = factor(id)), linetype = "dashed", color = "turquoise")+
    geom_line(aes(x=time, y=lo95_1, group = factor(id)), linetype = "dashed", color = "turquoise")+
    geom_line(data = subset(df_long_sim_avg, condition == "condition1" & as.numeric(id) <= id_hi & as.numeric(id) >= id_lo), aes(x=time, y=position_avg, group = id), size = 0.5, color = "gray50")+
    geom_line(data=subset(df_subj, id <= id_hi & id >= id_lo), aes(x = time, y = condition1, group = id), linetype = "longdash")+
    geom_line(aes(x=time, y=med_2, group = factor(id)), color = "red")+
    geom_line(aes(x=time, y=hi95_2, group = factor(id)), linetype = "dashed", color = "red")+
    geom_line(aes(x=time, y=lo95_2, group = factor(id)), linetype = "dashed", color = "red")+
    geom_line(data = subset(df_long_sim_avg, condition == "condition2" & as.numeric(id) <= id_hi & as.numeric(id) >= id_lo), aes(x=time, y=position_avg, group = id), size = 0.5, color = "gray50")+
    geom_line(data=subset(df_subj, id <= id_hi & id >= id_lo), aes(x = time, y = condition2, group = id), linetype = "longdash")+
    ylab('Scaled Position')+
    xlab('Proportion of Movement')+ 
    facet_wrap(~id, ncol = 2)+
    theme_gray(base_size = 20)+
    theme(
      panel.grid.major = element_line(size = 0)
      , panel.grid.minor = element_line(size = 0)
      , strip.text = element_text(size=15)
      , strip.background = element_blank()
      , axis.text.x = element_text(size = 12)
      , legend.position = "none"
      , panel.background = element_rect(fill = "white", color = "black")
    ) ->p1
  return(p1)
}

# plot
by_subj(1, 10)
by_subj(11, 20)
by_subj(21, 29)

# condition 1
subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1, color = factor(id)))+
  # geom_line(aes(x=time, y=hi95_1, color = factor(id)), linetype = "dashed")+
  # geom_line(aes(x=time, y=lo95_1, color = factor(id)), linetype = "dashed")+
  # geom_line(data = subset(df_long_sim_avg, condition == "condition1"), aes(x=time, y=position_avg, group = id), size = 0.5, color = "gray50")+
  # geom_line(data=df_subj, aes(x = time, y = condition1, group = id), linetype = "longdash")+
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+
  ggtitle('Condition 1')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  ) -> p1
print(p1)

# condition 2 
subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2, color = factor(id)))+
  # geom_line(aes(x=time, y=hi95_2, color = factor(id)), linetype = "dashed")+
  # geom_line(aes(x=time, y=lo95_2, color = factor(id)), linetype = "dashed")+
  # geom_line(data = subset(df_long_sim_avg, condition == "condition2"), aes(x=time, y=position_avg, group = id), size = 0.5, color = "gray50")+
  # geom_line(data=df_subj, aes(x = time, y = condition2, group = id), linetype = "longdash")+
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+
  ggtitle('Condition 2')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  ) -> p2
print(p2)




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
    time= (as.numeric(gsub('X','',time))-1)*bin_width
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
    time= (as.numeric(gsub('X','',time))-1)*bin_width
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
  group_by(id, time, condition) %>%
  dplyr::summarise(
    SD = log(sd(position))
  ) -> noise_df_long_sim

# get population parameters
df_subj_noise = readRDS("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/previous_analyses/fake_thesis_15/fake_data_proposal_subj_noise.rds")
df_subj_noise %>%
  spread(condition, value) -> df_subj_noise

noise_by_subj = function(id_lo, id_hi) {
  noise_subj_to_plot %>%
    dplyr::filter(as.numeric(id) <= id_hi, as.numeric(id) >= id_lo) %>%
    ggplot()+
    geom_line(aes(x=time, y=med_1, group = factor(id)), color = "turquoise")+
    geom_line(aes(x=time, y=hi95_1, group = factor(id)), linetype = "dashed", color = "turquoise")+
    geom_line(aes(x=time, y=lo95_1, group = factor(id)), linetype = "dashed", color = "turquoise")+
    geom_line(data = subset(noise_df_long_sim, condition == "condition1" & as.numeric(id) <= id_hi & as.numeric(id) >= id_lo), aes(x=time, y=SD, group = id), size = 0.5, color = "gray50")+
    geom_line(data=subset(df_subj_noise, id <= id_hi & id >= id_lo), aes(x = time, y = condition1, group = id), linetype = "longdash")+
    geom_line(aes(x=time, y=med_2, group = factor(id)), color = "red")+
    geom_line(aes(x=time, y=hi95_2, group = factor(id)), linetype = "dashed", color = "red")+
    geom_line(aes(x=time, y=lo95_2, group = factor(id)), linetype = "dashed", color = "red")+
    geom_line(data = subset(noise_df_long_sim, condition == "condition2" & as.numeric(id) <= id_hi & as.numeric(id) >= id_lo), aes(x=time, y=SD, group = id), size = 0.5, color = "gray50")+
    geom_line(data=subset(df_subj_noise, id <= id_hi & id >= id_lo), aes(x = time, y = condition2, group = id), linetype = "longdash")+
    ylab('Log Standard Deviation')+
    xlab('Proportion of Movement')+
    facet_wrap(~id, ncol = 2)+
    theme_gray(base_size = 20)+
    theme(
      panel.grid.major = element_line(size = 0)
      , panel.grid.minor = element_line(size = 0)
      , strip.text = element_text(size=15)
      , strip.background = element_blank()
      , axis.text.x = element_text(size = 12)
      , legend.position = "none"
      , panel.background = element_rect(fill = "white", color = "black")
    ) ->p1
  return(p1)
}

# plot
noise_by_subj(1, 10)
noise_by_subj(11, 20)
noise_by_subj(21, 29)

noise_subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1, color = factor(id)))+
  # geom_line(aes(x=time, y=hi95_1, color = factor(id)), linetype = "dashed")+
  # geom_line(aes(x=time, y=lo95_1, color = factor(id)), linetype = "dashed")+
  # geom_line(data = subset(noise_df_long_sim, condition == "condition1"), aes(x=time, y=SD, group = id), size = 0.5, color = "gray50")+
  # geom_line(data=df_subj_noise, aes(x = time, y = condition1, group = id), linetype = "longdash")+
  ylab('Log Standard Deviation')+
  xlab('Proportion of Movement')+
  ggtitle('Condition 1')+
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
  # geom_line(aes(x=time, y=hi95_2, color = factor(id)), linetype = "dashed")+
  # geom_line(aes(x=time, y=lo95_2, color = factor(id)), linetype = "dashed")+
  # geom_line(data = subset(noise_df_long_sim, condition == "condition2"), aes(x=time, y=SD, group = id), size = 0.5, color = "gray50")+
  # geom_line(data=df_subj_noise, aes(x = time, y = condition2, group = id), linetype = "longdash")+
  ylab('Log Standard Deviation')+
  xlab('Proportion of Movement')+
  ggtitle('Condition 2')+
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
    time= (as.numeric(gsub('X','',time))-1)*bin_width
    , condition = 1
  )

# get GP of condition 2
condition2 = data.frame(f[,,2])
condition2$sample = 1:nrow(condition2)
f_2 = condition2 %>%
  gather(key="time", value="value", -sample) %>%
  dplyr::mutate(
    time= (as.numeric(gsub('X','',time))-1)*bin_width
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

to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data = subset(df_long_sim_grand_avg, condition == "condition1"), aes(x=time, y=position_grand_avg), size = 0.5, color = "gray50")+
  geom_line(data=df_pop, aes(x = time, y = condition1), linetype = "longdash")+  
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+ 
  ggtitle('Condition 1')+
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
  geom_line(data = subset(df_long_sim_grand_avg, condition == "condition2"), aes(x=time, y=position_grand_avg), size = 0.5, color = "gray50")+
  geom_line(data=df_pop, aes(x = time, y = condition2), linetype = "longdash")+    
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+ 
  ggtitle('Condition 2')+
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
  annotate("text", label = "Condition 1", x = 3/15, y = -0, color = "turquoise")+
  annotate("text", label = "Condition 2", x = 8/15, y = -2, color = "red")+
  geom_line(data = subset(df_long_sim_grand_avg, condition == "condition1"), aes(x=time, y=position_grand_avg), size = 0.5, color = "gray50")+
  geom_line(data=df_pop, aes(x = time, y = condition1), linetype = "longdash")+   
  geom_line(data = subset(df_long_sim_grand_avg, condition == "condition2"), aes(x=time, y=position_grand_avg), size = 0.5, color = "gray50")+
  geom_line(data=df_pop, aes(x = time, y = condition2), linetype = "longdash")+   
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+ 
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

# get empirical difference as well
df_long_sim_grand_avg %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    effect = position_grand_avg[condition == "condition1"] - position_grand_avg[condition == "condition2"]
  ) -> df_long_sim_condition_effect

to_plot_effect %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "purple")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "purple", alpha=0.5)+
  ylab('Scaled Position (C1 - C2)')+
  xlab('Proportion of Movement')+ 
  geom_line(data = df_long_sim_condition_effect, aes(x=time, y=effect), size = 0.5, color = "gray50")+
  geom_line(data=df_pop, aes(x = time, y = effect), linetype = "longdash")+   
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
    time= (as.numeric(gsub('X','',time))-1)*bin_width
    , condition = 1
  )

# get GP of condition 2
noise_condition2 = data.frame(noise_f[,,2])
noise_condition2$sample = 1:nrow(noise_condition2)
noise_f_2 = noise_condition2 %>%
  gather(key="time", value="value", -sample) %>%
  dplyr::mutate(
    time= (as.numeric(gsub('X','',time))-1)*bin_width
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
  group_by(time, condition) %>%
  dplyr::summarise(
    avg_SD = mean(SD)
  ) -> subj_noise

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data = subset(subj_noise, condition == "condition1"), aes(x=time, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x = time, y = condition1), linetype = "longdash")+    
  ylab('Log Standard Deviation')+
  xlab('Proportion of Time')+
  ggtitle('Condition 1')+
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
  geom_line(data = subset(subj_noise, condition == "condition2"), aes(x=time, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x = time, y = condition2), linetype = "longdash")+   ylab('Log Standard Deviation')+
  xlab('Proportion of Time')+
  ggtitle('Condition 2')+
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
  geom_line(data = subset(subj_noise, condition == "condition1"), aes(x=time, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x = time, y = condition1), linetype = "longdash")+    
  geom_line(data = subset(subj_noise, condition == "condition2"), aes(x=time, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x = time, y = condition2), linetype = "longdash")+  
  ylab('Log Standard Deviation')+
  xlab('Proportion of Time')+
  annotate("text", label = "Condition 1", x = 12/15, y = -0.4, color = "turquoise")+
  annotate("text", label = "Condition 2", x = 4/15, y = 0.6, color = "red")+
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

# get empirical effect
subj_noise %>%
  dplyr::group_by(time) %>%
  dplyr::summarise(
    effect = avg_SD[condition == "condition1"] - avg_SD[condition == "condition2"]
  ) -> subj_noise_effect

noise_to_plot_effect %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "purple")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "purple", alpha=0.5)+
  geom_line(data = subj_noise_effect, aes(x=time, y=effect), size = 0.5, color = "gray50")+
  geom_line(data=df_pop_noise, aes(x = time, y = effect), linetype = "longdash")+
  ylab('Log Standard Deviation (C1 - C2)')+
  xlab('Proportion of Time')+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , panel.background = element_rect(fill = "white", color = "black")
  )




