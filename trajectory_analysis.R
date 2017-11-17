library(tidyverse)
library(plyr)
library(forecast)
library(signal)

setwd("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_data/touch_screen")



##############################
#       Touch Screen
##############################

dat = map_df(
  .x = list.files(
    pattern = ".csv"
  )
  , .f = function(file) {
    read_csv(
      file = file
      , col_names = FALSE
    )
  }
)

# matlab mappings:
# [pilot id age sex hand year month day hour minute seconds blocking block trial iti blocking rt response_time too_soon too_late xfix yfix xtarget ytarget]

names(dat) = c('pilot', 'id', 'age', 'sex', 'hand', 'year', 'month', 'day', 'hour', 'minute', 'seconds', 'blocking', 'block', 'trial', 'iti', 'rt', 'response_time', 'too_soon', 'too_late', 'xfix', 'yfix', 'xtarget', 'ytarget')


# Get Level Names ----
dat$pilot = factor(dat$pilot, levels = c(1, 2), labels = c("pilot", "experimental"))
dat$id = factor(dat$id)
dat$sex = factor(dat$sex, levels = c(1, 2), labels = c("male", "female"))
dat$hand = factor(dat$hand, levels = c(1, 2), labels = c("left", "right"))
dat$blocking = factor(dat$blocking, levels = c(1, 2), labels = c("vision", "no_vision"))

# check that coding is either 0 or 1
mean(dat$too_late == 0 | dat$too_late == 1)
dat$too_late = ifelse(dat$too_late == 0, FALSE, TRUE)

mean(dat$too_soon == 0 | dat$too_soon == 1)
dat$too_soon = ifelse(dat$too_soon == 0, FALSE, TRUE)

# make practice block boolean - will later remove practice blocks
dat$practice = factor(ifelse(dat$block == 1 | dat$block == 3, "practice", "experimental"))

# make vision vs. no-vision conditions 
dat$condition = factor(ifelse((dat$blocking == "vision" & (dat$block == 1 | dat$block == 2)) | (dat$blocking == "no_vision" & (dat$block == 3 | dat$block == 4)), "vision", "no_vision"))


# Summarize Data ----
summary(dat)

# reaction time
dat$good_rt = dat$rt * 1000

ggplot()+
  geom_histogram(data = subset(dat, condition == "no_vision"), aes(good_rt, ..density..), bins=20, alpha = 0.2, fill = "red")+
  geom_density(data = subset(dat, condition == "no_vision"), aes(good_rt, ..density..), color = "red")+
  geom_histogram(data = subset(dat, condition == "vision"), aes(good_rt, ..density..), bins=20, alpha = 0.2, fill = "blue")+
  geom_density(data = subset(dat, condition == "vision"), aes(good_rt, ..density..), color = "blue")+
  facet_wrap(practice~id)+
  xlab("reaction time (ms)")

dat %>%
  dplyr::group_by(pilot, id, practice, condition) %>%
  dplyr::summarize(mean_id_rt = mean(good_rt, na.rm = T)) %>%
  dplyr::group_by(practice, condition) %>%
  dplyr::summarize(mean_rt = mean(mean_id_rt, na.rm = T))
  

# response time
dat$good_response_time = dat$response_time * 1000

ggplot()+
  geom_histogram(data = subset(dat, condition == "no_vision"), aes(good_response_time, ..density..), bins=20, alpha = 0.2, fill = "red")+
  geom_density(data = subset(dat, condition == "no_vision"), aes(good_response_time, ..density..), color = "red")+
  geom_histogram(data = subset(dat, condition == "vision"), aes(good_response_time, ..density..), bins=20, alpha = 0.2, fill = "blue")+
  geom_density(data = subset(dat, condition == "vision"), aes(good_response_time, ..density..), color = "blue")+
  facet_wrap(practice~id)+
  xlab("response time (ms)")

dat %>%
  dplyr::group_by(pilot, id, practice, condition) %>%
  dplyr::summarize(mean_id_response_time = mean(good_response_time, na.rm = T)) %>%
  dplyr::group_by(practice, condition) %>%
  dplyr::summarize(mean_response_time = mean(mean_id_response_time, na.rm = T))

# movement time
dat$good_movement_time = dat$good_response_time - dat$good_rt

ggplot()+
  geom_histogram(data = subset(dat, condition == "no_vision"), aes(good_movement_time, ..density..), bins=20, alpha = 0.2, fill = "red")+
  geom_density(data = subset(dat, condition == "no_vision"), aes(good_movement_time, ..density..), color = "red")+
  geom_histogram(data = subset(dat, condition == "vision"), aes(good_movement_time, ..density..), bins=20, alpha = 0.2, fill = "blue")+
  geom_density(data = subset(dat, condition == "vision"), aes(good_movement_time, ..density..), color = "blue")+
  facet_wrap(practice~id)+
  xlab("movement time (ms)")

dat %>%
  dplyr::group_by(pilot, id, practice, condition) %>%
  dplyr::summarize(mean_id_movement_time = mean(good_movement_time, na.rm = T)) %>%
  dplyr::group_by(practice, condition) %>%
  dplyr::summarize(mean_movement_time = mean(mean_id_movement_time, na.rm = T))


# define touch screen parameters in terms of participant coordinates
mm_per_pixel =  0.26458333333333

# what is the size of the target?
# target is a dot
target_diameter = 10
target_size = mm_per_pixel * target_diameter

xdim = 1080 * mm_per_pixel
ydim = 1920 * mm_per_pixel

xfixcoor = xdim/2
yfixcoor = ydim*1/16

xtargetcoor = xdim/2
ytargetcoor = ydim*2/3

# now redefine touch screen fixation touches to be in interpretable frame of reference (0,0 in bottom left corner)
dat$good_xfix = xdim - dat$yfix * mm_per_pixel
dat$good_yfix = ydim - dat$xfix * mm_per_pixel

# now redefine touch screen target responses to be in interpretable frame of reference
dat$good_xtarget = xdim - dat$ytarget * mm_per_pixel
dat$good_ytarget = ydim - dat$xtarget * mm_per_pixel

# just look at experimental data
dat %>%
  dplyr::filter(practice == "experimental") %>%
    ggplot()+
    geom_point(aes(x=good_xfix, y=good_yfix, group = id), na.rm = T, size = 0.25)+
    geom_point(aes(x=xfixcoor, y=yfixcoor, group = id), na.rm = T, size = 0.5, color = "red")+
    geom_point(aes(x=good_xtarget, y=good_ytarget, group = id), na.rm = T, size = 0.25)+
    geom_point(aes(x=xtargetcoor, y=ytargetcoor, group = id), na.rm = T, size = 0.5, color = "red")+
    xlim(c(0, xdim))+
    ylim(c(0, ydim))+
    xlab("x (mm)")+
    ylab("y (mm)")+
    facet_grid(condition~id)

# look at error 
dat$fix_error = sqrt((dat$good_xfix - xfixcoor)^2 + (dat$good_yfix - yfixcoor)^2)
dat$target_error = sqrt((dat$good_xtarget - xtargetcoor)^2 + (dat$good_ytarget - ytargetcoor)^2)  

dat %>%
  group_by(condition) %>%
  dplyr::summarize(mean_target_error = mean(target_error, na.rm = T))



# Bad Trial Information ----
dat$ez_trial = ifelse(
  dat$block == 1
  , dat$trial
  , ifelse(
    dat$block == 2
    , dat$trial + 10
    , ifelse(
      dat$block == 3
      , dat$trial + 30
      , dat$trial + 40
    )
  )
)

dat$touch_screen_error = ifelse(
  (dat$id == 1 & (dat$ez_trial %in% c(17, 22, 24, 33, 34, 36, 39,51)))
  | (dat$id == 2 & (dat$ez_trial %in% c(18, 21, 23, 28)))
  | (dat$id == 3 & (dat$ez_trial %in% c(25, 31)))
  | (dat$id == 4 & (dat$ez_trial %in% c(10, 11, 19, 23, 36, 41, 48)))
  | (dat$id == 5 & (dat$ez_trial %in% c(11, 12, 14, 17, 22, 26)))
  | (dat$id == 6 & (dat$ez_trial %in% c(5, 16, 32, 39, 46, 52, 54)))
  | (dat$id == 7 & (dat$ez_trial %in% c())) 
  , TRUE
  , FALSE
  )

# look at 'too soons'!
dat %>%
  dplyr::group_by(pilot, id, practice, condition) %>% 
  dplyr::summarize(mean_id_too_soon = mean(too_soon)) %>%
  dplyr::group_by(practice, condition) %>% 
  dplyr::summarize(mean_too_soon = mean(mean_id_too_soon))
# what proportion occur along with touch screen error?
dat %>%
  dplyr::group_by(pilot, id, practice, condition, touch_screen_error) %>%
  dplyr::summarize(mean_id_too_soon = mean(too_soon)) %>%
  dplyr::group_by(practice, condition, touch_screen_error) %>%
  dplyr::summarize(mean_too_soon = mean(mean_id_too_soon))

# look at 'too lates'!
dat %>%
  dplyr::group_by(pilot, id, practice, condition) %>% 
  dplyr::summarize(mean_id_too_late = mean(too_late)) %>%
  dplyr::group_by(practice, condition) %>%
  dplyr::summarize(mean_too_late = mean(mean_id_too_late))
# what proportion occur along with touch screen error?
dat %>%
  dplyr::group_by(pilot, id, practice, condition, touch_screen_error) %>%
  dplyr::summarize(mean_id_too_late = mean(too_late)) %>%
  dplyr::group_by(practice, condition, touch_screen_error) %>%
  dplyr::summarize(mean_too_late = mean(mean_id_too_late))


# Identify trials for which no optotrak information was collected or other optotrak or goggle error occured 
dat$optotrak_error = ifelse(
  (dat$id == 1 & (dat$ez_trial %in% c(12)))
  | (dat$id == 2 & (dat$ez_trial %in% c()))
  | (dat$id == 3 & (dat$ez_trial %in% c(1, 2, 3)))
  | (dat$id == 4 & (dat$ez_trial %in% c(14)))
  | (dat$id == 5 & (dat$ez_trial %in% c()))
  | (dat$id == 6 & (dat$ez_trial %in% c()))
  | (dat$id == 7 & (dat$ez_trial %in% c()))
  , TRUE
  , FALSE
)



# Eliminate Bad Trials ----
dat %>%
  dplyr::filter(
    practice == "experimental"
    , too_soon == FALSE
    , too_late == FALSE
    , touch_screen_error == FALSE
    , optotrak_error == FALSE
  ) -> dat_clean

# count trials 
dat_clean %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(trial)) 
  




##############################
#       Optotrak
##############################

# Read Data ----
df_orig = map_df(
  .x = list.files(
    pattern = ".csv"
    , recursive = T
    , path = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_data/optotrak" 
    , full.names = T
  )
  , .f = function(file) {
    temp = strsplit(file, "/")[[1]]
    id = substr(temp[10],2,3)
    pilot = substr(temp[10],1,1)
    trial = as.numeric(substr(temp[11],10, 12))
    
    if (trial <= 10) {
      block = 1
    } else if (trial <= 30) {
      block = 2
      trial = trial - 10
    } else if (trial <= 40) {
      block = 3
      trial = trial - 30 
    } else {
      block = 4
      trial = trial - 40
    }
    
    df_piece = read_csv(
      file = file
      , col_names = T
    )
    df_piece$id = id
    df_piece$trial = trial
    df_piece$pilot = pilot
    df_piece$block = block
    
    return(df_piece)
  }
)

# only select first marker for now
df_orig %>%
  dplyr::select(pilot, id, block, trial, Frame, `1-1`, `1-2`, `1-3`, `2-1`, `2-2`, `2-3`) -> df_orig

names(df_orig)[5:11] = c("frame", "x", "y", "z", "x2", "y2", "z2")
df_orig$id = factor(as.numeric(df_orig$id))
df_orig$trial = as.numeric(df_orig$trial)
df_orig$pilot = factor(df_orig$pilot, levels = c("p", "e"), labels = c("pilot", "experimental"))



# Merge ---- 
df = merge(df_orig, dat_clean)   # should merge based on block, trial, id ,and participant type

# the merge causes the order of frames to get messed up so we sort
ptm = proc.time()
df = ddply(
  .data = df
  , .variables = c("pilot", "id", "block", "trial")
  , .fun = function(df_piece) {
    dplyr::arrange(df_piece, frame)
  }
)
proc.time() - ptm



# Lost Markers ----
# there are huge outlying values!
summary(df)

# how many of these trials contain these outlying values?
df %>%
  dplyr::filter(abs(x) > 1e25 | abs(y) > 1e25 | abs(z) > 1e25) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial))

# replace them with other marker
# NOTE: in proposal, would only use other marker if 30 consecutive ms (6 samples) were lost. 
# Otherwise, would just get rid of data point
df$x = ifelse(abs(df$x) > 1e25, df$x2, df$x)
df$y = ifelse(abs(df$y) > 1e25, df$y2, df$y)
df$z = ifelse(abs(df$z) > 1e25, df$z2, df$z)

# now, how many of these trials contain these outlying values?
df %>%
  dplyr::filter(abs(x) > 1e25 | abs(y) > 1e25 | abs(z) > 1e25) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial))

# replace remaining extreme trials with median value (basically, extremely simple interpolation)
df$x = ifelse(abs(df$x) > 1e25, median(df$x), df$x)
df$y = ifelse(abs(df$y) > 1e25, median(df$y), df$y)
df$z = ifelse(abs(df$z) > 1e25, median(df$z), df$z)
# do the same for the secondary markers
df$x2 = ifelse(abs(df$x2) > 1e25, median(df$x2), df$x2)
df$y2 = ifelse(abs(df$y2) > 1e25, median(df$y2), df$y2)
df$z2 = ifelse(abs(df$z2) > 1e25, median(df$z2), df$z2)

# verify that the values have been replaced
df %>%
  dplyr::filter(abs(x) > 1e25 | abs(y) > 1e25 | abs(z) > 1e25) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial))


# Outliers ----

# create time variable (ms) from frame variable
df$time = df$frame/200*1000

# X
xhi = 400
xlo = 175

df %>%
  ggplot()+
  geom_point(aes(x=time, y=x), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("x position (mm)")+
  geom_hline(yintercept = xhi, color = "red")+  # high cut-off, applicable across participants
  geom_hline(yintercept = xlo, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~condition)

# Y
df %>%
  ggplot()+
  geom_point(aes(x=time, y=y), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("y position (mm)")+
  facet_grid(.~condition)

# Z
zhi = -3150
zlo = -3300

df %>%
  ggplot()+
  geom_point(aes(x=time, y=z), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("z position (mm)")+
  ylim(c(-3300,-3000))+
  geom_hline(yintercept = zhi, color = "red")+  # high cut-off, applicable across participants
  geom_hline(yintercept = zlo, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~condition)

# how many are there?
df %>%
  dplyr::filter(x > xhi | x < xlo | z > zhi | z < zlo) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial))

# replace odd points with other marker 
df$x = ifelse(df$x > xhi | df$x < xlo, df$x2, df$x)
df$z = ifelse(df$z >  zhi | df$z < zlo , df$z2, df$z)

# X
df %>%
  ggplot()+
  geom_point(aes(x=time, y=x), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("x position (mm)")+
  geom_hline(yintercept = xhi, color = "red")+  # high cut-off, applicable across participants
  geom_hline(yintercept = xlo, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~condition)

# Y
df %>%
  ggplot()+
  geom_point(aes(x=time, y=y), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("y position (mm)")+
  facet_grid(.~condition)

# Z
df %>%
  ggplot()+
  geom_point(aes(x=time, y=z), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("z position (mm)")+
  ylim(c(-3300,-3000))+
  geom_hline(yintercept = zhi, color = "red")+  # high cut-off, applicable across participants
  geom_hline(yintercept = zlo, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~condition)

# now, how many are there?
df %>%
  dplyr::filter(x > xhi | x < xlo | z > zhi | z < zlo) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial))

# there are too many so we do most basic interpolation on data that exceed threshold
df$x = ifelse(df$x > xhi | df$x < xlo, median(df$x), df$x)
df$z = ifelse(df$z >  zhi | df$z < zlo , median(df$z), df$z)
  
# verify
# X
df %>%
  ggplot()+
  geom_point(aes(x=time, y=x), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("x position (mm)")+
  geom_hline(yintercept = xhi, color = "red")+  # high cut-off, applicable across participants
  geom_hline(yintercept = xlo, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~condition)

# Y
df %>%
  ggplot()+
  geom_point(aes(x=time, y=y), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("y position (mm)")+
  facet_grid(.~condition)

# Z
df %>%
  ggplot()+
  geom_point(aes(x=time, y=z), alpha = 0.2, size = .5)+
  xlab("time (ms)")+
  ylab("z position (mm)")+
  ylim(c(-3300,-3000))+
  geom_hline(yintercept = zhi, color = "red")+  # high cut-off, applicable across participants
  geom_hline(yintercept = zlo, color = "red")+  # low cut-off, applicable across participants
  facet_grid(.~condition)

# finally, how many are there?
df %>%
  dplyr::filter(x > xhi | x < xlo | z > zhi | z < zlo) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial))




# Long Format ----
df %>%
  dplyr::select(-x2, -y2, -z2) %>%
  gather(coordinate, position, x:z, factor_key = T) -> df_long



# Filtering ----
# want low pass butterworth filter (zero-shift) with 8 Hz cutoff

# filter order of 2 (first argument) according to documentation
bf = butter(2, 8/(200/2), type = "low")

# make copy for renaming purpose (lazy)
df_long$unfil_position = df_long$position

df_long %>%
  dplyr::group_by(pilot, id, block, coordinate, trial) %>%
  dplyr::mutate(position = signal::filtfilt(bf, unfil_position)) -> df_long

# look at result
edge_line = 150

df_long %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("x position (mm)")+
  facet_grid(id~condition)+
  geom_vline(xintercept = edge_line, color = "red")+
  theme(legend.position = "none")

df_long %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("y position (mm)")+
  facet_grid(id~condition)+
  geom_vline(xintercept = edge_line, color = "red")+
  theme(legend.position = "none")

df_long %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("z position (mm)")+
  facet_grid(id~condition)+
  geom_vline(xintercept = edge_line, color = "red")+
  theme(legend.position = "none")

# get rid of edge effect at beginning
df_long %>% dplyr::filter(time > edge_line) -> df_long


# Derivatives ----
df_long$velocity = c(0, diff(df_long$position))

df_long %>% dplyr::filter(time != edge_line + 1000/200) -> df_long_clean  # getting rid of first frame



# Trial start ----
# ensure order of data
df_long_clean %>% dplyr::arrange(pilot, id, condition, trial, coordinate, time) -> df_long_clean

df_long_clean %>%
  dplyr::select(pilot, id, condition, trial, coordinate, time, velocity) %>%
  spread(coordinate, velocity) %>%
  dplyr::mutate(
    abs_velo = sqrt(x^2 + y^2 + z^2)
    , non_stationary = abs_velo > 1
  ) %>%
  gather(coordinate, velocity, x:z, factor_key = T) %>%
  dplyr::arrange(pilot, id, condition, trial, coordinate, time) -> df_long_velo  # the arrange function is necessary for lining up both data frames


# make sure they are lined up
mean(df_long_velo$velocity == df_long_clean$velocity)
mean(df_long_velo$id == df_long_clean$id)
mean(df_long_velo$trial == df_long_clean$trial)
mean(df_long_velo$time == df_long_clean$time)

df_long_velo %>%
  ungroup() %>%
  dplyr::select(abs_velo, non_stationary) %>%
  bind_cols(df_long_clean) %>%
  dplyr::select(pilot:velocity, abs_velo:non_stationary) -> df_long_clean
# get rid of velocity sub-df
rm(df_long_velo)

ma_length = 5
# we ignore seperations among ids, coordinates, and trials for efficiency
# the following processing should make it so that this does not effect trial starts and ends
df_long_clean$potential_start = as.numeric(ma(df_long_clean$non_stationary, ma_length))  

# I actually think this is faster than just using mutate
df_long_clean$trial_start_bool = df_long_clean$potential_start == 1
df_long_clean$trial_start_frame = df_long_clean$trial_start_bool * df_long_clean$frame 

# identify first negative change in 'potential start'
df_long_clean$trial_end_diff = diff(c(0,df_long_clean$trial_start_bool))
df_long_clean$trial_end_bool = df_long_clean$trial_end_diff == -1
df_long_clean$trial_end_frame = df_long_clean$trial_end_bool * df_long_clean$frame - 1

# select some columns to make things faster
df_long_clean %>%
  dplyr::select(
    -c(block, velocity, non_stationary, potential_start, trial_start_bool, trial_end_diff, trial_end_bool)
  ) -> df_long_clean

# give some space 
buffer = 20
df_long_clean %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    trial_start = sort(unique(trial_start_frame))[2] - (ma_length - 1)/2 - buffer   # we pick the second lowest because there are many zeroes
    , trial_end = sort(unique(trial_end_frame))[2] + buffer  # we pick the second lowest because there are many zeroes
  ) -> df_long_clean

# check how many trials are left now
df_long_clean %>%
  group_by(pilot, id, condition) %>%
  dplyr::summarize(
    trial_count = length(unique(trial))
  )

df_long_clean %>%
  dplyr::filter(
    frame >= trial_start
    , frame <= trial_end
  ) -> df_long_trim

df_long_trim %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    zero_time = time - min(time)  # grouping works for min()
  ) -> df_long_trim

# look at velocities first
df_long_trim %>%
  dplyr::filter(coordinate == "z") %>%  # the coordinate is arbitrary. 
  # The absolute velocity is the same across all three coordinates
  ggplot()+
  geom_line(aes(x=zero_time, y=abs_velo, color = trial, group = trial), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("absolute velocity (mm^2)")+
  facet_grid(id~condition)+
  theme(legend.position = "none")

# averages
df_long_trim %>%
  dplyr::filter(coordinate == "z") %>%
  group_by(pilot, id, condition, zero_time) %>%
  dplyr::summarise(
    abs_velo_avg = mean(abs_velo)
  ) -> df_velo_avg

df_velo_avg %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=abs_velo_avg, color = id, group = id), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("absolute velocity (mm^2)")+
  facet_grid(.~condition)

# again averages
df_velo_avg %>%
  group_by(condition, zero_time) %>%
  dplyr::summarise(
    abs_velo_grand_avg = mean(abs_velo_avg)
  ) -> df_velo_grand_avg

df_velo_grand_avg %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=abs_velo_grand_avg))+
  xlab("time (ms)")+
  ylab("absolute velocity (mm^2)")+
  facet_grid(.~condition)


# do the trajectories line up?
df_long_trim %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("x position (mm)")+
  facet_grid(id~condition)+
  theme(legend.position = "none")

df_long_trim %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("y position (mm)")+
  facet_grid(id~condition)+
  theme(legend.position = "none")

df_long_trim %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("z position (mm)")+
  facet_grid(id~condition)+
  theme(legend.position = "none")



# Averages ----
df_long_trim %>%
  group_by(pilot, id, condition, coordinate, zero_time) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_long_trim_avg

df_long_trim_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position_avg, color = id, group = id), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("x position (mm)")+
  facet_grid(.~condition)

df_long_trim_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position_avg, color = id, group = id), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("y position (mm)")+
  facet_grid(.~condition)

df_long_trim_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position_avg, color = id, group = id), alpha = 0.5)+
  xlab("time (ms)")+
  ylab("z position (mm)")+
  facet_grid(.~condition)

# take averages over participants
df_long_trim_avg %>%
  group_by(condition, coordinate, zero_time) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg)
  ) -> df_long_trim_grand_avg

df_long_trim_grand_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position_grand_avg))+
  xlab("time (ms)")+
  ylab("x position (mm)")+
  facet_grid(.~condition)

df_long_trim_grand_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position_grand_avg))+
  xlab("time (ms)")+
  ylab("y position (mm)")+
  facet_grid(.~condition)

df_long_trim_grand_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=zero_time, y=position_grand_avg))+
  xlab("time (ms)")+
  ylab("z position (mm)")+
  facet_grid(.~condition)



# Normalize ----
round0 = function(x,z){
  round(x/z,0)*z
}

df_long_trim %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    norm_time = round0((zero_time-min(zero_time))/(max(zero_time)-min(zero_time)), 0.05)
    ) -> df_long_norm

df_long_norm %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("normalized time")+
  ylab("x position (mm)")+
  facet_grid(id~condition)+
  theme(legend.position = "none")

df_long_norm %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("normalized time")+
  ylab("y position (mm)")+
  facet_grid(id~condition)+
  theme(legend.position = "none")

df_long_norm %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position, color = trial, group = trial), alpha = 0.5)+
  xlab("normalized time")+
  ylab("z position (mm)")+
  facet_grid(id~condition)+
  theme(legend.position = "none")

# how do the id averages look?.. We average over trials
df_long_norm %>%
  group_by(pilot, id, condition, coordinate, norm_time) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_long_norm_avg

df_long_norm_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_avg, color = id, group = id), alpha = 0.5)+
  xlab("normalized time")+
  ylab("x position (mm)")+
  facet_grid(.~condition)

df_long_norm_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_avg, color = id, group = id), alpha = 0.5)+
  xlab("normalized time")+
  ylab("y position (mm)")+
  facet_grid(.~condition)

df_long_norm_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_avg, color = id, group = id), alpha = 0.5)+
  xlab("normalized time")+
  ylab("z position (mm)")+
  facet_grid(.~condition)

# now we average over participants
df_long_norm_avg %>%
  group_by(condition, coordinate, norm_time) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg)
  ) -> df_long_norm_grand_avg

df_long_norm_grand_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_grand_avg))+
  xlab("normalized time")+
  ylab("x position (mm)")+
  facet_grid(.~condition)
df_long_norm_grand_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_grand_avg, group = condition, color = condition))+
  xlab("normalized time")+
  ylab("x position (mm)")

df_long_norm_grand_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_grand_avg))+
  xlab("normalized time")+
  ylab("y position (mm)")+
  facet_grid(.~condition)
df_long_norm_grand_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_grand_avg, group = condition, color = condition))+
  xlab("normalized time")+
  ylab("y position (mm)")

df_long_norm_grand_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_grand_avg))+
  xlab("normalized time")+
  ylab("z position (mm)")+
  facet_grid(.~condition)
df_long_norm_grand_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=norm_time, y=position_grand_avg, group = condition, color = condition))+
  xlab("normalized time")+
  ylab("z position (mm)")

