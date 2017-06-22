library(R.matlab)
library(tidyverse)
library(plyr)

setwd("~/Documents/Experiments/Trajectory/Jenn Study/Workspaces")

# Read Data ----
df = map_df(
  .x = list.files(
    pattern = "normalized"
    , recursive = T
    , path = "/Volumes/LACIE SHARE/Jenn Study/Jenn CPU/Data"
    , full.names = T
  )
  , .f = function(file) {
    
    temp = strsplit(file, "/")[[1]]
    id = substr(temp[7],11,13)
    trial = substr(temp[8],16, nchar(temp[8]) - 4)  # minus four because '.mat'
    
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

# put frames in order
ptm = proc.time()
df = ddply(
  .data = df
  , .variables = c("id", "trial")
  , .fun = function(df_piece) {
    dplyr::arrange(df_piece, time)
  }
)
proc.time() - ptm
# user  system elapsed 
# 5.046   0.775   6.080 


# Raw Data ----
df %>%
  gather(coordinate, position, x:z, factor_key = T) -> df_long
rm(df)

df_norm = df_long

# X
df_norm %>%
  dplyr::filter(coordinate == "x", as.numeric(id) < 8) %>%
  ggplot()+
    geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
    xlab("time")+
    ylab("position (x)")+
    facet_grid(id ~ target)+
    theme(legend.position = "none")
df_norm %>%
  dplyr::filter(coordinate == "x", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(id ~ target)+
  theme(legend.position = "none")

# Y
df_norm %>%
  dplyr::filter(coordinate == "y", as.numeric(id) < 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(id ~ target)+
  theme(legend.position = "none")
df_norm %>%
  dplyr::filter(coordinate == "y", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(id ~ target)+
  theme(legend.position = "none")

# Z
df_norm %>%
  dplyr::filter(coordinate == "z", as.numeric(id) < 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (z)")+
  ylim(c(-200, 200))+
  facet_grid(id ~ target)+
  theme(legend.position = "none")
df_norm %>%
  dplyr::filter(coordinate == "z", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (z)")+
  ylim(c(-200, 200))+
  facet_grid(id ~ target)+
  theme(legend.position = "none")

# NOTE: there are some substantial outliers

# # the target mappings for participants 12, 16, and 17 are no good
# df_norm %>% dplyr::filter(id != "012", id != "016", id != "017") -> df_norm


# Averages ----
df_norm %>%
  group_by(id, coordinate, cue, target, time) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_norm_avg

df_norm_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(cue~target)

df_norm_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(cue~target)

df_norm_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(cue~target)

# NOTE: clearly some outliers are still affecting the participant wise waveforms

# take averages over participants
df_norm_avg %>%
  group_by(coordinate, cue, target, time) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg)
  ) -> df_norm_grand_avg

df_norm_grand_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(.~target)

df_norm_grand_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(.~target)

df_norm_grand_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(.~target)


# Save Data ----
save(df_norm, file = "data_norm_matched.RData")




#### Comparison with my Methods ####
setwd("~/Documents/Experiments/Trajectory/Jenn Study/Workspaces")
# load("data_norm_matched.RData")
load("data_trim_matched.RData")

# set trial start to zero
df_long_trim %>%
  group_by(id, trial, coordinate) %>%
  dplyr::mutate(position = position - position[time == 0]) -> df_long_trim


# By ID and Trial ----
df_long_trim %>%
  dplyr::filter(coordinate == "x", as.numeric(id) < 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long_trim %>%
  dplyr::filter(coordinate == "x", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(id~target)+
  theme(legend.position = "none")

df_long_trim %>%
  dplyr::filter(coordinate == "y", as.numeric(id) < 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long_trim %>%
  dplyr::filter(coordinate == "y", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(id~target)+
  theme(legend.position = "none")

df_long_trim %>%
  dplyr::filter(coordinate == "z", as.numeric(id) < 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long_trim %>%
  dplyr::filter(coordinate == "z", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(id~target)+
  theme(legend.position = "none")


# Averages ----
df_long_trim %>%
  group_by(id, coordinate, cue, target, time) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_long_trim_avg

df_long_trim_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(cue~target)

df_long_trim_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(cue~target)

df_long_trim_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_avg, color = id), alpha = 0.5)+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(cue~target)

# take averages over participants
df_long_trim_avg %>%
  group_by(coordinate, cue, target, time) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg)
  ) -> df_long_trim_grand_avg

df_long_trim_grand_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(.~target)

df_long_trim_grand_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(.~target)

df_long_trim_grand_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(.~target)


# Compare Trial and ID Counts ----
# ids
unique(df_long_trim$id)
length(unique(df_long_trim$id))
# -- 
unique(df_norm$id)
length(unique(df_norm$id))

# trials
df_long_trim %>%
  dplyr::filter(coordinate == "z") %>%  # arbitrary
  group_by(id) %>%
  dplyr::summarise(num_trials = length(unique(trial))) %>%
  dplyr::summarise(avg_trials_per_id = mean(num_trials))
# --
df_norm %>%
  dplyr::filter(coordinate == "z") %>%  # arbitrary
  group_by(id) %>%
  dplyr::summarise(num_trials = length(unique(trial))) %>%
  dplyr::summarise(avg_trials_per_id = mean(num_trials))

# take averages over participants WITHOUT outliers
df_norm_avg %>%
  dplyr::filter(id != "017", id != "018") %>%
  group_by(coordinate, cue, target, time) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg)
  ) -> ndf_norm_grand_avg

ndf_norm_grand_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(.~target)

ndf_norm_grand_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(.~target)

ndf_norm_grand_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(.~target)
