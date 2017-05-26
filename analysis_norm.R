library(R.matlab)
library(tidyverse)

setwd("~/Documents/Experiments/Trajectory/Jenn Study")

# Read Data ----
df = map_df(
  .x = list.files(
    pattern = "normalized"
    , recursive = T
    , path = "./Subset/data"
    , full.names = T
  )
  , .f = function(file) {
    
    temp = strsplit(file, "/")[[1]]
    id = substr(temp[4],11,13)
    trial = substr(temp[5],16, nchar(temp[5]) - 4)  # minus four because '.mat'
    
    MAT = readMat(file)
    df_piece = data.frame(MAT[[1]])
    names(df_piece) = c("x1", "y1", "z1", "x2", "y2", "z2")
    df_piece$id = id
    df_piece$trial = trial
    df_piece$time = 1:nrow(df_piece)
    
    return(df_piece)
  }
)

# only select first marker for now
df %>%
  dplyr::select(id, trial, time, x1, y1, z1) -> df

names(df)[4:6] = c("x", "y", "z")
df$id = factor(df$id)
df$trial = factor(df$trial)

# Combine Data ----
load("data_matrices.rdata")
mats$trial = ifelse(
  nchar(as.character(mats$trial)) == 3
  , as.character(mats$trial)
  , ifelse(
    nchar(as.character(mats$trial)) == 2
    , paste(0, as.character(mats$trial), sep = "")
    , paste(0,0, as.character(mats$trial), sep = "")
  )
)

# df_orig = df
df = merge(df, mats)  # should merge based on trial and id


# Raw Data ----
df %>%
  gather(coordinate, position, x:z, factor_key = T) -> df_long

# X
df_long %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
    geom_line(aes(x=time, y=position, color = trial), alpha = 0.5)+
    xlab("time")+
    ylab("position (x)")+
    facet_grid(id ~ target)+
    theme(legend.position = "none")

# Y
df_long %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.5)+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(id ~ target)+
  theme(legend.position = "none")

# Z
df_long %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.5)+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(id ~ target)+
  theme(legend.position = "none")

