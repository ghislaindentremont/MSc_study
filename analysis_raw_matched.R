# let's analyze some real trajectories
setwd("~/Documents/Experiments/Trajectory/Jenn Study")

library(tidyverse)
library(forecast)
library(plyr)


# Read Data ----
df = map_df(
  .x = list.files(
    pattern = ".csv"
    , recursive = T
    , path = "/Volumes/LACIE SHARE/Jenn Study/Jenn CPU/Data" #/#013
    , full.names = T
    )
  , .f = function(file) {
    temp = strsplit(file, "/")[[1]]
    id = substr(temp[7],11,13)
    trial = substr(temp[8],10, 12)
    df_piece = read_csv(
      file = file
      , col_names = T
    )
    df_piece$id = id
    df_piece$trial = trial
    
    return(df_piece)
  }
)

# only select first marker for now
df %>%
  dplyr::select(id, trial, Frame, `1-1`, `1-2`, `1-3`) -> df

names(df)[3:6] = c("frame", "x", "y", "z")
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

# below line takes roughly 1 minute
# df_orig
df = merge(df, mats)   # should merge based on trial and id

# the merge causes the order of frames to get messed up so we sort
ptm = proc.time()
df = ddply(
  .data = df
  , .variables = c("id", "trial")
  , .fun = function(df_piece) {
    dplyr::arrange(df_piece, frame)
  }
)
proc.time() - ptm
# user  system elapsed 
# 30.676  16.651  51.715 

# Examine Data ----
# there are huge outlying values!
summary(df)

# look for trials where there are huge values
df %>%
  dplyr::group_by(id, trial) %>%
  dplyr::summarize(
    meanx = mean(x)
    , meany = mean(y)
    , meanz = mean(z)
  ) %>%
  dplyr::filter(abs(meanx) > 1e25, abs(meany) > 1e25, abs(meanz) > 1e25) %>%
  dplyr::summarize(num_trials = length(trial))


# Figure Mapping ----
df %>%
  dplyr::filter(x >-10000, y >-10000, z>-10000, z< -2000) %>%
  gather(coordinate, position, x:z, factor_key = T) -> df_long
# get rid of df after creating long format
rm(df)

df_long %>%
  dplyr::filter(
    id %in% c("005", "006", "010",  "011",  "013", "014", "015")
    , coordinate == "z"
  ) %>%
  group_by(id, target, trial) %>%
  dplyr::summarize(
    id_trial_max = max(position)
    , id_trial_min = min(position)
  ) %>%
  group_by(id, target) %>%
  dplyr::summarize(
    id_max = median(id_trial_max)
    , id_min = median(id_trial_min)
  ) %>%
  group_by(target) %>%
  dplyr::summarize(
    Ma = min(id_max)  # minimum max
    , Mi = max(id_min)  # maximum min
  )

# participants 2, 3, 4, 7, 8, 9, 12, 16, 17, *18*, 19 don't have the correct target mappings
# *18 may have questionable data
# but NOT 5, 6, 10, 11, 13, 14, 15
df_long %>%
  group_by(id, target, trial) %>%
  dplyr::mutate(
    target_fixed = ifelse(
      min(position) < -2470   # the group_by actually works for min(position)
      , 0
      , 1
    )
  ) -> df_long
df_long$target_fixed = factor(df_long$target_fixed)
# 3, 4, 7, *8*, 9, *12*, *16*, 17, *18*, 19   are now fixed
# but NOT 2
# *8 and 12 may lose a few trials
# *18 looks wonky

df_long %>%
  dplyr::filter(id != "002", id != "018") %>%  # get rid of 2 and 18
  dplyr::mutate(
    target_final = ifelse(
      id %in% c("005", "006", "010", "011", "013", "014", "015")
      , as.character(target)
      , as.character(target_fixed)
    )
  ) -> df_long
df_long$target_final = factor(df_long$target_final)

# how well do diferent target labels match up? 
df_long %>%
  group_by(id, coordinate) %>%
  dplyr::summarise(
    prop_final_target = mean(target_final == target)
    , prop_final_fixed = mean(target_final == target_fixed)
    , prop_fixed_target = mean(target_fixed == target)
  ) %>%
  group_by(id) %>%
  dplyr::summarize(
    prop_final_target = unique(prop_final_target)
    , prop_final_fixed = unique(prop_final_fixed)
    , prop_fixed_target = unique(prop_fixed_target)
  )
# P17 is the only one for which original mappings don't match up with inferred (fixed) ones!


# Raw Data ----
# establish cut-offs for 'wacky' trials
df_long %>%
  group_by(id, coordinate) %>%
  dplyr::mutate(
    hi_95 = quantile(position, 0.975)   # grouping works here also
    , lo_95 = quantile(position, 0.025)
  ) -> df_long

# X
df_long %>%
  dplyr::filter(coordinate == "x", position > hi_95 | position < lo_95) %>%
  ggplot()+
    geom_point(aes(x=frame, y=position), alpha = 0.2, size = .5)+
    xlab("time")+
    ylab("position (x)")+
    geom_hline(yintercept = 250, color = "red")+  # high cut-off, applicable across participants
    geom_hline(yintercept = 75, color = "red")+  # low cut-off, applicable across participants
    facet_grid(.~target)

# Y
df_long %>%
  dplyr::filter(coordinate == "y", position > hi_95 | position < lo_95) %>%
  ggplot()+
  geom_point(aes(x=frame, y=position), alpha = 0.2, size = .5)+
  xlab("time")+
  ylab("position (y)")+
  geom_hline(yintercept = -20, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~target)

# Z
df_long %>%
  dplyr::filter(coordinate == "z", position > hi_95 | position < lo_95) %>%
  ggplot()+
  geom_point(aes(x=frame, y=position), alpha = 0.2, size = .5)+
  xlab("time")+
  ylab("position (z)")+
  geom_hline(data = data.frame(Z=-2350, target=factor("0")), aes(yintercept = Z), color = "red")+
  geom_hline(data = data.frame(Z=-2600, target=factor("0")), aes(yintercept = Z), color = "red")+
  geom_hline(data = data.frame(Z=-2250, target=factor("1")), aes(yintercept = Z), color = "red")+
  geom_hline(data = data.frame(Z=-2500, target=factor("1")), aes(yintercept = Z), color = "red")+
  facet_grid(.~target)


# Exclusion ----
# identify trials that exceed thresholds
df_long %>%
  dplyr::select(id, coordinate, trial, frame, position, target) %>%
  spread(coordinate, position) %>%
  dplyr::filter(
    y < -20 | x > 250 | x < 75
    | (target == "0" & ((z> -2350) | (z < -2600)))
    | (target == "1" & ((z > -2250) | (z < -2500)))
  ) %>%
  group_by(id, trial) %>%
  dplyr::summarise(dummy = mean(x)) -> df_exclude

# look at which trials are excluded, for each participant
aggregate(trial ~ id, data = df_exclude, FUN = length)

# # need to get rid of 17 on basis of original trial mappings 
# df_long %>% dplyr::filter(id != "017") -> df_long

ptm = proc.time()
df_long_clean = ddply(
  .data = df_long
  , .variables = c("id")
  , .fun = function(df_piece) {
    id = unique(df_piece$id)
    to_return = df_piece[!(df_piece$trial %in% df_exclude$trial[df_exclude$id == id]),]
    return(to_return)
  }
)
proc.time() - ptm
# user  system elapsed 
# 51.214  23.491  84.949 
# get rid of df_long after clean is created
rm(df_long)

# what trials do we have? 
df_long_clean %>%
  group_by(id) %>%
  dplyr::summarize(
    trial_count = length(unique(trial))
  )

# confirm that data are cleaned up
# X
df_long_clean %>%
  dplyr::filter(coordinate == "x", position > hi_95 | position < lo_95) %>%
  ggplot()+
  geom_point(aes(x=frame, y=position), alpha = 0.2, size = .5)+
  xlab("time")+
  ylab("position (x)")+
  ggtitle("clean")+
  geom_hline(yintercept = 250, color = "red")+  # high cut-off, applicable across participants
  geom_hline(yintercept = 75, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~target)

# Y
df_long_clean %>%
  dplyr::filter(coordinate == "y", position > hi_95 | position < lo_95) %>%
  ggplot()+
  geom_point(aes(x=frame, y=position), alpha = 0.2, size = .5)+
  xlab("time")+
  ylab("position (y)")+
  ggtitle("clean")+
  geom_hline(yintercept = -20, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~target)

# Z
df_long_clean %>%
  dplyr::filter(coordinate == "z", position > hi_95 | position < lo_95) %>%
  ggplot()+
  geom_point(aes(x=frame, y=position), alpha = 0.2, size = .5)+
  xlab("time")+
  ylab("position (z)")+
  ggtitle("clean")+
  geom_hline(data = data.frame(Z=-2350, target=factor("0")), aes(yintercept = Z), color = "red")+
  geom_hline(data = data.frame(Z=-2600, target=factor("0")), aes(yintercept = Z), color = "red")+
  geom_hline(data = data.frame(Z=-2250, target=factor("1")), aes(yintercept = Z), color = "red")+
  geom_hline(data = data.frame(Z=-2500, target=factor("1")), aes(yintercept = Z), color = "red")+
  facet_grid(.~target)


# Derivatives ----
# unfortunately this grouping does not work, so the first frame is not always 0
# it should not matter later that the first frame is not always zero
df_long_clean$velocity = c(0, diff(df_long_clean$position))

df_long_clean %>% dplyr::filter(frame != 1) -> df_long_clean  # getting rid of first frame
  

# Trial start ----
df_long_clean %>%
  dplyr::select(id, trial, frame, coordinate, velocity) %>%
  spread(coordinate, velocity) %>%
  dplyr::mutate(
    abs_velo = sqrt(x^2 + y^2 + z^2)
    , non_stationary = abs_velo > 1
    ) %>%
  gather(coordinate, velocity, x:z, factor_key = T) %>%
  dplyr::arrange(id, coordinate, trial) -> df_long_velo  # the arrange function is necessary for lining up both data frames


# make sure they are lined up
mean(df_long_velo$velocity == df_long_clean$velocity)
mean(df_long_velo$id == df_long_clean$id)
mean(df_long_velo$trial == df_long_clean$trial)
mean(df_long_velo$frame == df_long_clean$frame)

df_long_velo %>%
  ungroup() %>%
  dplyr::select(abs_velo, non_stationary) %>%
  bind_cols(df_long_clean) %>%
  dplyr::select(id:velocity, abs_velo:non_stationary) -> df_long_clean
# get rid of velocity sub-df
rm(df_long_velo)

ma_length = 5
# we ignore seperations among ids, coordinates, and trials for efficiency
# the following processing should make it so that this does not effect trial starts and ends
df_long_clean$potential_start = as.numeric(ma(df_long_clean$non_stationary, ma_length))  

# I actually think this is faster than just using mutate
df_long_clean$trial_start_bool = df_long_clean$potential_start == 1
df_long_clean$trial_start_frame = df_long_clean$trial_start_bool * df_long_clean$frame 

# select some columns to make things faster
df_long_clean %>%
  dplyr::select(
    -c(soa, hi_95, lo_95, block, target_final, target_fixed, velocity, non_stationary, potential_start, trial_start_bool)
    ) -> df_long_clean

# give some space 
buffer = 20
df_long_clean %>%
  group_by(id, coordinate, trial) %>%
  dplyr::mutate(
    trial_start = sort(unique(trial_start_frame))[2] - (ma_length - 1)/2 - buffer   # we pick the second lowest because there are many zeroes
    # , trial_end = max(trial_start_frame, na.rm = T) + 5  # this does not work because it captures reversal
  ) -> df_long_clean

# check how many trials are left now
df_long_clean %>%
  group_by(id) %>%
  dplyr::summarize(
    trial_count = length(unique(trial))
  )

df_long_clean %>%
  dplyr::filter(
    frame >= trial_start
    # , frame <= trial_end
    ) -> df_long_trim

df_long_trim %>%
  group_by(id, coordinate, trial) %>%
  dplyr::mutate(
    time = frame - min(frame)  # grouping works for min()
  ) -> df_long_trim

# trial end threshold
trial_end = 100
# look at velocities first
df_long_trim %>%
  dplyr::filter(coordinate == "z", as.numeric(id) < 8) %>%  # the coordinate is arbitrary. 
  # The absolute velocity is the same across all three coordinates
  ggplot()+
  geom_line(aes(x=time, y=abs_velo, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("absolute velocity")+
  ylim(c(0, 10))+
  geom_vline(xintercept = trial_end)+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long_trim %>%
  dplyr::filter(coordinate == "z", as.numeric(id) >= 8) %>%  # the coordinate is arbitrary. 
  # The absolute velocity is the same across all three coordinates
  ggplot()+
  geom_line(aes(x=time, y=abs_velo, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("absolute velocity")+
  ylim(c(0, 10))+
  geom_vline(xintercept = trial_end)+
  facet_grid(id~target)+
  theme(legend.position = "none")

# averages
df_long_trim %>%
  dplyr::filter(coordinate == "z") %>%
  group_by(id, target, time) %>%
  dplyr::summarise(
    abs_velo_avg = mean(abs_velo)
  ) -> df_velo_avg

df_velo_avg %>%
  ggplot()+
  geom_line(aes(x=time, y=abs_velo_avg, color = id), alpha = 0.5)+
  xlab("time")+
  ylab("absolute velocity")+
  ylim(c(0,10))+
  geom_vline(xintercept = trial_end)+
  facet_grid(.~target)

# again averages
df_velo_avg %>%
  group_by(target, time) %>%
  dplyr::summarise(
    abs_velo_grand_avg = mean(abs_velo_avg)
  ) -> df_velo_grand_avg

df_velo_grand_avg %>%
  ggplot()+
  geom_line(aes(x=time, y=abs_velo_grand_avg))+
  xlab("time")+
  ylab("absolute velocity")+
  geom_vline(xintercept = trial_end, color = "red")+
  facet_grid(.~target)


# do the trajectories line up?
df_long_trim %>%
  dplyr::filter(coordinate == "x", as.numeric(id) < 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (x)")+
  geom_vline(xintercept = trial_end)+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long_trim %>%
  dplyr::filter(coordinate == "x", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (x)")+
  geom_vline(xintercept = trial_end)+
  facet_grid(id~target)+
  theme(legend.position = "none")

df_long_trim %>%
  dplyr::filter(coordinate == "y", as.numeric(id) < 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (y)")+
  geom_vline(xintercept = trial_end)+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long_trim %>%
  dplyr::filter(coordinate == "y", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (y)")+
  geom_vline(xintercept = trial_end)+
  facet_grid(id~target)+
  theme(legend.position = "none")

df_long_trim %>%
  dplyr::filter(coordinate == "z", as.numeric(id) < 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (z)")+
  geom_vline(xintercept = trial_end)+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long_trim %>%
  dplyr::filter(coordinate == "z", as.numeric(id) >= 8) %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial), alpha = 0.2)+
  xlab("time")+
  ylab("position (z)")+
  geom_vline(xintercept = trial_end)+
  facet_grid(id~target)+
  theme(legend.position = "none")

# NOTE: a few number of trials obviously don't have the right target mapping! 
# Since we are concerned with the proper cue mappings it may be best just to remove those trials


# Trial End ----
df_long_trim %>%
  dplyr::filter(
    time <= trial_end
  ) -> df_long_trim


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


# Save Data ----
save(df_long_trim, file = "data_trim_matched.RData")
  

# # Normalize ----
# round0 = function(x,z){
#   round(x/z,0)*z
# }
# 
# df_long_trim %>%
#   group_by(id, trial, coordinate) %>%
#   dplyr::mutate(
#     norm_time = round0((time-min(time))/(max(time)-min(time)), 0.01)
#     ) -> df_long_norm
# 
# df_long_norm %>%
#   dplyr::filter(coordinate == "x", id == "013") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position, color = trial), alpha = 0.2)+
#   xlab("normalized time")+
#   ylab("position (x)")+
#   facet_grid(.~target_final)+
#   theme(legend.position = "none")
# 
# df_long_norm %>%
#   dplyr::filter(coordinate == "y", id == "013") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position, color = trial), alpha = 0.2)+
#   xlab("normalized time")+
#   ylab("position (y)")+
#   facet_grid(.~target_final)+
#   theme(legend.position = "none")
# 
# df_long_norm %>%
#   dplyr::filter(coordinate == "z", id == "013") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position, color = trial), alpha = 0.2)+
#   xlab("normalized time")+
#   ylab("position (z)")+
#   facet_grid(.~target_final)+
#   theme(legend.position = "none")
# 
# # how do the id averages look?.. We average over trials
# df_long_norm %>%
#   group_by(id, coordinate, target_final, norm_time) %>%
#   dplyr::summarise(
#     position_avg = mean(position)
#   ) -> df_long_norm_avg 
# 
# df_long_norm_avg %>%
#   dplyr::filter(coordinate == "x") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position_avg, color = id), alpha = 0.5)+
#   xlab("normalized time")+
#   ylab("position (x)")+
#   facet_grid(.~target_final)
# 
# df_long_norm_avg %>%
#   dplyr::filter(coordinate == "y") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position_avg, color = id), alpha = 0.5)+
#   xlab("normalized time")+
#   ylab("position (y)")+
#   facet_grid(.~target_final)
# 
# df_long_norm_avg %>%
#   dplyr::filter(coordinate == "z") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position_avg, color = id), alpha = 0.5)+
#   xlab("normalized time")+
#   ylab("position (z)")+
#   facet_grid(.~target_final)
# 
# # now we average over participants
# df_long_norm_avg %>%
#   group_by(coordinate, target_final, norm_time) %>%
#   dplyr::summarise(
#     position_grand_avg = mean(position_avg)
#   ) -> df_long_norm_grand_avg 
# 
# df_long_norm_grand_avg %>%
#   dplyr::filter(coordinate == "x") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position_grand_avg))+
#   xlab("time")+
#   ylab("position (x)")+
#   facet_grid(.~target_final)
# 
# df_long_norm_grand_avg %>%
#   dplyr::filter(coordinate == "y") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position_grand_avg))+
#   xlab("time")+
#   ylab("position (y)")+
#   facet_grid(.~target_final)
# 
# df_long_norm_grand_avg %>%
#   dplyr::filter(coordinate == "z") %>%
#   ggplot()+
#   geom_line(aes(x=norm_time, y=position_grand_avg))+
#   xlab("time")+
#   ylab("position (z)")+
#   facet_grid(.~target_final)
# 
# 
# # # Save normalized data ----
# # save(df_long_norm, file = "data_norm.Rdata")
