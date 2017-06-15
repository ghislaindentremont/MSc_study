# Upload stan_data ----
load("fake_stan_data3.Rdata")
# load("stan_data.Rdata")
# load("stan_data_15.Rdata")
# load("stan_data_21.rdata")


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
  , iter = 1000
  , init = 0
  , chains = 16
  , cores = 16
  , verbose = T
  , refresh = 1
  , control = list(
    max_treedepth = 15
    , adapt_delta = 0.95  # consider reaising to .99
  )  
  , include = F
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
  , file = 'fake3_post.rdata'
)

