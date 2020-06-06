setwd("~/Documents/Experiments/Trajectory/Jenn Study")

library(R.matlab)
library(tidyverse)

# Read Data ----
mats = map_df(
  .x = list.files(
    pattern = "trialmatrix.mat"
    , recursive = T
    , path = "/Volumes/LACIE SHARE/Jenn Study/Matrices"
    , full.names = T
  )
  , .f = function(file) {
    temp = strsplit(file, "/")[[1]]
    id = substr(temp[6],2,4)
    block = substr(temp[6],10, 10)
    MAT = readMat(file)
    df_piece = data.frame(MAT[[1]])
    names(df_piece) = c("cue", "target", "soa")
    df_piece$id = id
    df_piece$block = block
    
    return(df_piece)
  }
)

mats$cue = factor(mats$cue)
mats$target = factor(mats$target)
# mats$soa = factor(mats$soa)
mats$id = factor(mats$id)
mats$block = factor(mats$block)

mats %>%
  group_by(id, block) %>%
  mutate(
    trial = (as.numeric(block)-1) * 112 + 1:112
  ) -> mats

save(mats, file = "data_matrices.rdata")
