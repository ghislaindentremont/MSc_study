# Upload stan_data ----
load("stan_data.Rdata")


# Load Packages ----
library(tidyverse)
library(rstan)
options(mc.cores=parallel::detectCores())


# Run Stan Model ----
mod = rstan::stan_model("gp_regression.stan")

# set the model running on each core
post = sampling(
  mod
  , data = stan_data
  , iter = 100
  , init = 0
  , include = F
  , chains = 16
  , cores = 16
  , verbose = T
  , refresh = 1
  # , control = list(max_treedepth = 15)
  , pars = c(
  'f_normal01'
  , 'subj_f_normal01'
  , 'volatility_helper'
  , 'subj_volatility_helper'
  )
)

#save result for later
save(
  post
  , file = 'cluster_test_post.rdata'
)

