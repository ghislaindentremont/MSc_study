# Upload stan_data ----
load("data_for_stan_effect_15.RData")



# Load Packages ----
library(tidyverse)
library(rstan)
options(mc.cores=parallel::detectCores())



# Run Stan Model ----
mod = rstan::stan_model("gp_regression.stan")

# set the model running on each core
post = sampling(
 mod
  , data = data_for_stan
  , iter = 300
  , init = 0
  , chains = 16
  , cores = 16
  , verbose = T
  , refresh = 1
  , control = list(
    max_treedepth = 17
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

#save result for later
save(
  post
  , file = 'post_300_15_mtd17_effect.rdata'
)

