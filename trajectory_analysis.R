library(tidyverse)
library(plyr)
library(forecast)
library(signal)
library(zoo)
library(imputeTS)
library(ez)
library(MASS)
library(reshape2)
library(rstan)
library(fda.usc)
library(R.matlab)
library(coda)
# library(ezStan)

setwd("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_data/touch_screen")



########################################################
####            Touch Screen                        ####
########################################################

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



# Mislabelled ids ----
# same day
aggregate(day ~ id, data=dat[dat$id == "12",], FUN=unique)
# same hour
aggregate(hour ~ id, data=dat[dat$id == "12",], FUN=unique)
# different minute, ealier one should be 22
minute_df = aggregate(minute ~ id, data=dat[dat$id == "12",], FUN=unique)
e22_min = min(minute_df$minute)

dat[dat$id=="12" & dat$minute == e22_min,]$id = 22



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



########################################################
####             Summarize Data                     ####
########################################################


# Extract Demographic Info ----
summary(dat)

# participant count
length(unique(dat$id))

# median age and age range 
ages = aggregate(age ~ id, data = dat, FUN = unique)
range(ages$age)
median(ages$age)

# sex count 
sexes = aggregate(sex ~ id, data = dat, FUN = unique)
sum(sexes$sex == "male")


# Create Functions ----
plot_outcomes = function(ids_use, dv, xlabel) {
  
  dat$temp = dplyr::pull(dat, dv)
  
  gg = ggplot()+
    geom_histogram(data = subset(dat, condition == "no_vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), bins=20, alpha = 0.2, fill = "red")+
    geom_density(data = subset(dat, condition == "no_vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), color = "red")+
    geom_histogram(data = subset(dat, condition == "vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), bins=20, alpha = 0.2, fill = "blue")+
    geom_density(data = subset(dat, condition == "vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), color = "blue")+
    facet_grid(id~practice)+
    xlab(sprintf("%s (ms)", xlabel))
  print(gg)
}

sum_outcomes = function(dv, practice = T) {
  if (practice) {
    dat$temp = dplyr::pull(dat, dv)
    dat %>%
      dplyr::group_by(pilot, id, practice, condition) %>%
      dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
      dplyr::group_by(practice, condition) %>%
      dplyr::summarize(mean_tot = mean(mean_id, na.rm = T))%>%
      print()
    dat %>%
      dplyr::group_by(pilot, id, practice, condition) %>%
      dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
      dplyr::group_by(practice, condition) %>%
      dplyr::summarize(sd_tot = sd(mean_id, na.rm = T))%>%
      print()
    # sd of difference
    dat %>%
      dplyr::group_by(pilot, id, practice, condition) %>%
      dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
      dplyr::group_by(pilot, id, practice) %>%
      dplyr::summarize(diff_id = diff(mean_id), na.rm = T) %>%
      dplyr::group_by(practice) %>%
      dplyr::summarize(sd_effect = sd(diff_id, na.rm = T))%>%
      print()
  } else {
    dat$temp = dplyr::pull(dat, dv)
    dat %>%
      dplyr::group_by(blocking, pilot, id, condition) %>%
      dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
      dplyr::group_by(blocking, condition) %>%
      dplyr::summarize(mean_tot = mean(mean_id, na.rm = T))%>%
      print()
    dat %>%
      dplyr::group_by(blocking, pilot, id, condition) %>%
      dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
      dplyr::group_by(blocking, condition) %>%
      dplyr::summarize(sd_tot = sd(mean_id, na.rm = T))%>%
      print()
    # sd of difference
    dat %>%
      dplyr::group_by(blocking, pilot, id, condition) %>%
      dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
      dplyr::group_by(blocking, pilot, id) %>%
      dplyr::summarize(diff_id = diff(mean_id), na.rm = T) %>%
      dplyr::group_by(blocking) %>%
      dplyr::summarize(sd_effect = sd(diff_id, na.rm = T))%>%
      print()
  }
}

# Reaction Time ----
dat$good_rt = dat$rt * 1000

plot_outcomes(1:10, "good_rt", "reaction time")
plot_outcomes(11:20, "good_rt", "reaction time")
plot_outcomes(21:33, "good_rt", "reaction time")
sum_outcomes("good_rt")
sum_outcomes("good_rt", F)


# Response Time ----
dat$good_response_time = dat$response_time * 1000

plot_outcomes(1:10, "good_response_time", "response time")
plot_outcomes(11:20, "good_response_time", "response time")
plot_outcomes(21:33, "good_response_time", "response time", T)
sum_outcomes("good_response_time")
sum_outcomes("good_response_time", F)


# Movement Time ----
dat$good_movement_time = dat$good_response_time - dat$good_rt

plot_outcomes(1:10, "good_movement_time", "movement time")
plot_outcomes(11:20, "good_movement_time", "movement time")
plot_outcomes(21:33, "good_movement_time", "movement time", T)
sum_outcomes("good_movement_time")
sum_outcomes("good_movement_time", F)


# Compute Touch Errors ----

# define touch screen parameters in terms of participant coordinates
mm_per_pixel =  0.26458333333333

# what is the size of the target?
# target is a dot
target_diameter = 10
target_size = mm_per_pixel * target_diameter
target_area = (target_size/2)^2 * pi

# what is the size of the fixation cross?
# how long is an arm from center?
fixation_arm = 20
fixation_size = mm_per_pixel * fixation_arm
fixation_area = fixation_size^2

xdim = 1080 * mm_per_pixel
ydim = 1920 * mm_per_pixel

xfixcoor = xdim/2
yfixcoor = ydim*1/16

xtargetcoor = xdim/2
ytargetcoor = ydim*2/3

# how far away is the target from the fixation cross?
fix_target_dist = ytargetcoor - yfixcoor

# now redefine touch screen fixation touches to be in interpretable frame of reference (0,0 in bottom left corner)
dat$good_xfix = xdim - dat$yfix * mm_per_pixel
dat$good_yfix = ydim - dat$xfix * mm_per_pixel

# now redefine touch screen target responses to be in interpretable frame of reference
dat$good_xtarget = xdim - dat$ytarget * mm_per_pixel
dat$good_ytarget = ydim - dat$xtarget * mm_per_pixel

# create function
plot_error = function(ids_use) {
  dat %>%
    dplyr::filter(practice == "experimental", as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_point(aes(x=good_xfix, y=good_yfix, group = id), na.rm = T, size = 0.25)+
    geom_point(aes(x=xfixcoor, y=yfixcoor, group = id), na.rm = T, size = 0.5, color = "red")+
    geom_point(aes(x=good_xtarget, y=good_ytarget, group = id), na.rm = T, size = 0.25)+
    geom_point(aes(x=xtargetcoor, y=ytargetcoor, group = id), na.rm = T, size = 0.5, color = "red")+
    xlim(c(0, xdim))+
    ylim(c(0, ydim))+
    xlab("x (mm)")+
    ylab("y (mm)")+
    facet_grid(condition~id) %>% print()
}

# use function
plot_error(1:10)
plot_error(11:20)
plot_error(21:33)


# Absolute (2D) Error ----
# at fixation
dat$fix_error = sqrt((dat$good_xfix - xfixcoor)^2 + (dat$good_yfix - yfixcoor)^2)
sum_outcomes("fix_error")

# at target
dat$target_error = sqrt((dat$good_xtarget - xtargetcoor)^2 + (dat$good_ytarget - ytargetcoor)^2)  
sum_outcomes("target_error")


# Amplitude Errors ----
dat$CE_amp = dat$good_ytarget - ytargetcoor
# both Constant (first) and Variable Errors (second) are Shown
sum_outcomes("CE_amp")


# Directional Errors ----
dat$CE_dir = dat$good_xtarget - xtargetcoor
# both Constant (first) and Variable Errors (second) are Shown
sum_outcomes("CE_dir")



########################################################
####              Bad Trial Information             ####
########################################################

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
  | (dat$id == 8 & (dat$ez_trial %in% c(18))) 
  | (dat$id == 9 & (dat$ez_trial %in% c(7, 22))) 
  | (dat$id == 10 & (dat$ez_trial %in% c(15, 54, 55))) 
  | (dat$id == 11 & (dat$ez_trial %in% c())) 
  | (dat$id == 12 & (dat$ez_trial %in% c(31))) 
  | (dat$id == 13 & (dat$ez_trial %in% c(11, 18, 52, 60))) 
  | (dat$id == 14 & (dat$ez_trial %in% c()))
  | (dat$id == 15 & (dat$ez_trial %in% c(40, 45)))
  | (dat$id == 16 & (dat$ez_trial %in% c(57)))
  | (dat$id == 17 & (dat$ez_trial %in% c()))
  | (dat$id == 18 & (dat$ez_trial %in% c()))
  | (dat$id == 19 & (dat$ez_trial %in% c()))
  | (dat$id == 20 & (dat$ez_trial %in% c(22, 24, 32)))
  | (dat$id == 21 & (dat$ez_trial %in% c(23, 27, 45, 47)))
  | (dat$id == 22 & (dat$ez_trial %in% c(16, 40, 45)))
  | (dat$id == 23 & (dat$ez_trial %in% c()))
  | (dat$id == 24 & (dat$ez_trial %in% c(11, 15, 38, 48)))
  | (dat$id == 25 & (dat$ez_trial %in% c(7, 18, 30, 40, 46)))
  | (dat$id == 26 & (dat$ez_trial %in% c()))
  | (dat$id == 27 & (dat$ez_trial %in% c(12, 15, 16, 31, 41, 43, 50, 51, 52, 53, 55, 56)))
  | (dat$id == 28 & (dat$ez_trial %in% c(1, 3, 20, 24)))
  | (dat$id == 29 & (dat$ez_trial %in% c(21)))
  | (dat$id == 30 & (dat$ez_trial %in% c(17, 20, 22:27, 30, 50)))
  | (dat$id == 31 & (dat$ez_trial %in% c(19, 58)))
  | (dat$id == 32 & (dat$ez_trial %in% c(12, 29, 39, 42, 44, 51, 53)))
  | (dat$id == 33 & (dat$ez_trial %in% c()))
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
  | (dat$id == 8 & (dat$ez_trial %in% c()))
  | (dat$id == 9 & (dat$ez_trial %in% c()))
  | (dat$id == 10 & (dat$ez_trial %in% c(54, 55)))
  | (dat$id == 11 & (dat$ez_trial %in% c()))
  | (dat$id == 12 & (dat$ez_trial %in% c()))
  | (dat$id == 13 & (dat$ez_trial %in% c()))
  | (dat$id == 14 & (dat$ez_trial %in% c()))
  | (dat$id == 15 & (dat$ez_trial %in% c()))
  | (dat$id == 16 & (dat$ez_trial %in% c()))
  | (dat$id == 17 & (dat$ez_trial %in% c()))
  | (dat$id == 18 & (dat$ez_trial %in% c()))
  | (dat$id == 19 & (dat$ez_trial %in% c()))
  | (dat$id == 20 & (dat$ez_trial %in% c()))
  | (dat$id == 21 & (dat$ez_trial %in% c(31)))
  | (dat$id == 22 & (dat$ez_trial %in% c(5, 6, 26, 58)))
  | (dat$id == 23 & (dat$ez_trial %in% c(30)))
  | (dat$id == 24 & (dat$ez_trial %in% c()))
  | (dat$id == 25 & (dat$ez_trial %in% c(8, 9)))
  | (dat$id == 26 & (dat$ez_trial %in% c()))
  | (dat$id == 27 & (dat$ez_trial %in% c(1:10)))
  | (dat$id == 28 & (dat$ez_trial %in% c()))
  | (dat$id == 29 & (dat$ez_trial %in% c()))
  | (dat$id == 30 & (dat$ez_trial %in% c()))
  | (dat$id == 31 & (dat$ez_trial %in% c()))
  | (dat$id == 32 & (dat$ez_trial %in% c(4)))
  | (dat$id == 33 & (dat$ez_trial %in% c(33)))
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
  dplyr::summarize(count = length(trial)) %>%
  print(n=66)
  
# NOTE: should again here look at touch screen related data, however
# in theory nearlly all the removed trials would not have been included in the original analysis (NaNs)




########################################################
####                   Optotrak                     ####
########################################################

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
# rm(df_orig)

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




##########################################################
####              Lost Markers                        ####
##########################################################

# there are huge outlying values!
summary(df)

# create function to plot points across trials and participants
plot_points = function(ids_use, dv, ylimits, outlier_cutoffs) {
  df$temp = dplyr::pull(df, dv)
  
  df %>%
    dplyr::filter(as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_point(aes(x=frame, y=temp, group=trial, color=trial), alpha = 0.2, size = .2)+
    xlab("frame")+
    ylab(sprintf("%s position (mm)", dv))+
    ylim(ylimits)+
    geom_hline(yintercept=outlier_cutoffs[1], size = 0.25, color = "red")+
    geom_hline(yintercept=outlier_cutoffs[2], size = 0.25, color = "red")+
    facet_grid(condition~id) %>% print()
}


# Primary Marker ----
# how many of these trials contain at least one of these outlying values?
df %>%
  dplyr::filter(abs(x) > 1e25 | abs(y) > 1e25 | abs(z) > 1e25) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial)) %>%
  print(n=33)

# X
xbounds = c(100, 400)
xcutoffs = c(190, 325)
plot_points(1:10, "x", xbounds, xcutoffs)
plot_points(11:21, "x", xbounds, xcutoffs)
plot_points(21:33, "x", xbounds, xcutoffs)
# Y
ybounds = c(-200, 200)
ycutoffs =  c(-200, 200)
plot_points(1:10, "y", ybounds, ycutoffs)
plot_points(11:21, "y", ybounds, ycutoffs)
plot_points(21:33, "y", ybounds, ycutoffs)
# Z
zbounds = c(-3250, -3150)
zcutoffs = c(-3250, -3175)
plot_points(1:10, "z", zbounds, zcutoffs)
plot_points(11:21, "z", zbounds, zcutoffs)
plot_points(21:33, "z", zbounds, zcutoffs)


# Secondary Marker ----
# how many of these trials contain at least one of these outlying values?
df %>%
  dplyr::filter(abs(x2) > 1e25 | abs(y2) > 1e25 | abs(z2) > 1e25) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x2)) %>%
  dplyr::summarize(num_trials = length(ez_trial)) %>%
  print(n=33)

# use same bounds as primary marker coordinates 
# X2
x2cutoffs = c(200, 340)
plot_points(1:10, "x2", xbounds, x2cutoffs)
plot_points(11:21, "x2", xbounds, x2cutoffs)
plot_points(21:33, "x2", xbounds, x2cutoffs)
# Y2
y2cutoffs =  c(-200, 200)
plot_points(1:10, "y2", ybounds, y2cutoffs)
plot_points(11:21, "y2", ybounds, y2cutoffs)
plot_points(21:33, "y2", ybounds, y2cutoffs)
# Z2
z2cutoffs = c(-3250, -3175)
plot_points(1:10, "z2", zbounds, z2cutoffs)
plot_points(11:21, "z2", zbounds, z2cutoffs)
plot_points(21:33, "z2", zbounds, z2cutoffs)


# Compare Markers ----
compare_lines = function(ids_use, dvs, ylimits) {
  df$temp1 = dplyr::pull(df, dvs[1])
  df$temp2 = dplyr::pull(df, dvs[2])
  
  df %>%
    dplyr::filter(as.numeric(id) %in% ids_use, abs(temp1) < 1e25, abs(temp2) < 1e25) %>%
    ggplot()+
    geom_line(aes(x=frame, y=temp1, group=trial), alpha = 0.2, size = .2, color="red")+
    geom_line(aes(x=frame, y=temp2, group=trial), alpha = 0.2, size = .2, color="blue")+
    xlab("frame")+
    ylab(sprintf("%s vs. %s position (mm)", dvs[1], dvs[2]))+
    ylim(ylimits)+
    facet_grid(condition~id) %>% print()
}

# in how many trials have both markers been lost?
df %>%
  dplyr::filter(abs(x2) > 1e25 | abs(y2) > 1e25 | abs(z2) > 1e25 | abs(x) > 1e25 | abs(y) > 1e25 | abs(z) > 1e25) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x2)) %>%
  dplyr::summarize(num_trials = length(ez_trial)) %>%
  print(n=33)

# which participants have many trials for which both markers have problematic start points?
bad_ids = c(3,5,9,14,19,21,24,25,26,28)
# which participants have have problematic primary marker but reasonable second marker?
replace_ids = c(1,8,22)

# X
compare_lines(1:10, c("x", "x2"), c(100, 400))
compare_lines(11:20, c("x", "x2"), c(100, 400))
compare_lines(21:33, c("x", "x2"), c(100, 400))
compare_lines(bad_ids , c("x", "x2"), c(100, 400))
compare_lines(replace_ids , c("x", "x2"), c(100, 400))
# Y
compare_lines(1:10, c("y", "y2"), c(-200, 200))
compare_lines(11:20, c("y", "y2"), c(-200, 200))
compare_lines(21:33, c("y", "y2"), c(-200, 200))
compare_lines(bad_ids, c("y", "y2"), c(-200, 200))
compare_lines(replace_ids, c("y", "y2"), c(-200, 200))
# Z
compare_lines(1:10, c("z", "z2"), c(-3250, -3150))
compare_lines(11:20, c("z", "z2"), c(-3250, -3150))
compare_lines(21:33, c("z", "z2"), c(-3250, -3150))
compare_lines(bad_ids, c("z", "z2"), c(-3250, -3150))
compare_lines(replace_ids, c("z", "z2"), c(-3250, -3150))


# Remove Lost/Outlying Marker Samples ---- 
# first generate NAs for lost/outlying markers starting with primary marker
boolean_vector = (df$x < xcutoffs[1] | df$x > xcutoffs[2]) | (df$y < ycutoffs[1] | df$y > ycutoffs[2]) | (df$z < zcutoffs[1] | df$z > zcutoffs[2])
df$x_na = ifelse(boolean_vector, NA, df$x)
df$y_na = ifelse(boolean_vector, NA, df$y)
df$z_na = ifelse(boolean_vector, NA, df$z)
# and for secondary marker
boolean_vector2 = (df$x2 < x2cutoffs[1] | df$x2 > x2cutoffs[2]) | (df$y2 < y2cutoffs[1] | df$y2 > y2cutoffs[2]) | (df$z2 < z2cutoffs[1] | df$z2 > z2cutoffs[2])
df$x2_na = ifelse(boolean_vector2, NA, df$x2)
df$y2_na = ifelse(boolean_vector2, NA, df$y2)
df$z2_na = ifelse(boolean_vector2, NA, df$z2)



# Indentify Bad Trials ----
# how many bad markers in close proximity will be tolerate?
max_lost = 7
max_lost_ms = max_lost * 1000/200

# turn NAs (FOR ANY COORDINATE) into T
df$isna = is.na(df$x_na) | is.na(df$y_na) | is.na(df$z_na)
df$isna2 = is.na(df$x2_na) | is.na(df$y2_na) | is.na(df$z2_na)

# get moving average of T/Fs
df$isna_ma = as.numeric(ma(df$isna, max_lost))  
df$isna_ma2 = as.numeric(ma(df$isna2, max_lost))  

# determine when the moving average was 1 (all Ts)
df$bad_chunk = df$isna_ma == 1
df$bad_chunk = ifelse(is.na(df$bad_chunk), 0, df$bad_chunk)
df$bad_chunk2 = df$isna_ma2 == 1
df$bad_chunk2 = ifelse(is.na(df$bad_chunk2), 0, df$bad_chunk2)

# identify the trials in which there are bad chunks
df %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::mutate(
    bad_trial = max(bad_chunk)
    , bad_trial2 = max(bad_chunk2)
  ) -> df


# Replace and Remove Trials ----
# how many trials of primary marker must be replaced?
df %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::summarize(bad = unique(bad_trial)) %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarise(bad = sum(bad)) %>% print(n=66)

# how many trials of secondary marker are bad?
df %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::summarize(bad = unique(bad_trial2)) %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarise(bad = sum(bad)) %>% print(n=66)

# how many trials are bad for both? 
df %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::summarize(bad = unique(bad_trial2 & bad_trial)) %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarise(bad = sum(bad)) %>% print(n=66)

# we get rid of trials that are bad with respect to both markers
df %>% 
  dplyr::filter(!bad_trial, !bad_trial2) -> df_clean
# rm(df)

# now we replace primary marker with secondary marker for bad trials
df_clean$x_clean = ifelse(df_clean$bad_trial, df_clean$x2_na, df_clean$x_na)
df_clean$y_clean = ifelse(df_clean$bad_trial, df_clean$y2_na, df_clean$y_na)
df_clean$z_clean = ifelse(df_clean$bad_trial, df_clean$z2_na, df_clean$z_na)

# finally we replace all trials from certain participants with other marker
# if value does not correspond to one of the pre-selected participant, then the value from the previous procedure is kept
df_clean$x_clean = ifelse(df_clean$id %in% replace_ids, df_clean$x2_na, df_clean$x_clean)
df_clean$y_clean = ifelse(df_clean$id %in% replace_ids, df_clean$y2_na, df_clean$y_clean)
df_clean$z_clean = ifelse(df_clean$id %in% replace_ids, df_clean$z2_na, df_clean$z_clean)



# Remove Participants ----
# how many trials are there in each condition?
df_clean %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(unique(trial))) %>%
  print(n=66) -> trial_count

# identify participants with fewer than 10 trials for any given condition
trial_count %>% dplyr::filter(count >= 10) -> id_keep
id_keep %>% dplyr::filter(condition == "vision") %>% dplyr::pull("id") -> id_keep_v
id_keep %>% dplyr::filter(condition == "no_vision") %>% dplyr::pull("id") -> id_keep_nv
keep = dplyr::intersect(id_keep_v, id_keep_nv) %>% print()

# only keep participants with sufficient trials
df_clean %>% dplyr::filter(id %in% keep) -> df_cleaner
rm(df_clean)

# # get rid of bad ids
# df_cleaner %>% dplyr::filter(!(id %in% bad_ids)) -> df_cleanest
df_cleaner -> df_cleanest
rm(df_cleaner)

# how many trials are left?
df_cleanest %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(unique(trial))) %>%
  print(n=66) 
# how many participants are left?
n2=length(unique(df_cleanest$id))
n2



# Interpolate ----
inter_ma_len = 21
inter_ma_len_ms = inter_ma_len * 1000/200

# interpolate each trial
ptm = proc.time()
df_cleanest %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::mutate(
    # x_inter = na.approx(x_clean, na.rm=F)
    # , y_inter = na.approx(y_clean, na.rm=F)
    # , z_inter = na.approx(z_clean, na.rm=F)
    x_inter = na.locf(x_clean)
    , y_inter = na.locf(y_clean)
    , z_inter = na.locf(z_clean)
    ) -> df_cleanest
proc.time() - ptm

# how many trials still have NAs?
df_cleanest %>%
  dplyr::filter(is.na(x_inter) | is.na(x_inter) | is.na(x_inter)) %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(unique(trial))) %>% print(n2=n2*2)

# create function to plot new data frame
plot_lines = function(ids_use, dv, ylimits, y_label) {
  df_cleanest$temp = dplyr::pull(df_cleanest, dv)
  
  df_cleanest %>%
    dplyr::filter(as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=frame, y=temp, group=trial, color=trial), alpha = 0.4, size = .2)+
    xlab("frame")+
    ylab(y_label)+
    ylim(ylimits)+
    facet_grid(condition~id) %>% print()
}

# identify bad Ps. These tend to have ill-defined start points
still_bad_ids = c(3, 9, 19, 26)

# visualize interpolated data 
# X
plot_lines(1:10, "x_inter", xbounds, "x position (mm)")
plot_lines(11:21, "x_inter", xbounds, "x position (mm)")
plot_lines(21:33, "x_inter", xbounds, "x position (mm)")
plot_lines(still_bad_ids, "x_inter", xbounds, "bad x position (mm)")
# Y
plot_lines(1:10, "y_inter", ybounds, "y position (mm)")
plot_lines(11:21, "y_inter", ybounds, "y position (mm)")
plot_lines(21:33, "y_inter", ybounds, "y position (mm)")
plot_lines(still_bad_ids, "y_inter", ybounds, "bad y position (mm)")
# Z
plot_lines(1:10, "z_inter", zbounds, "z position (mm)")
plot_lines(11:21, "z_inter", zbounds, "z position (mm)")
plot_lines(21:33, "z_inter", zbounds, "z position (mm)")
plot_lines(still_bad_ids, "z_inter", zbounds, "bad z position (mm)")

# remove 'still' bad participants
df_cleanest %>% dplyr::filter(!(id %in% still_bad_ids)) -> df_for_long
rm(df_cleanest)
# how many trials are left?
df_for_long %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(unique(trial))) %>%
  print(n=66) 
# how many participants are left?
n=length(unique(df_for_long$id))
n

# Demographics ----
# median age and age range 
ages = aggregate(age ~ id, data = df_for_long, FUN = unique)
range(ages$age)
median(ages$age)

# sex count 
sexes = aggregate(sex ~ id, data = df_for_long, FUN = unique)
sum(sexes$sex == "male")





##########################################################
####             Signal Processing                    ####
##########################################################

# Long Format ----
# create time variable (ms) from frame variable
sampling_freq = 200
df_for_long$time = df_for_long$frame/sampling_freq*1000

df_for_long %>%
  dplyr::select(-x2, -y2, -z2) %>%
  gather(coordinate, position, x_inter:z_inter, factor_key = T) -> df_long
rm(df_for_long)

centering_window = 100
centering_window_ms = centering_window * 1000/200

# make trial start ~zero by subtracting median of frist few samples
df_long %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(centered_position = position - median(position[1:centering_window])) -> df_long

# create function to plot points across trials and participants
plot_long = function(ids_use, dv, y_label) {
  
  df_long %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=time/1000, y=centered_position, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (s)")+
    ylab(y_label)+
    facet_grid(condition~id) %>% print()
}

# X
plot_long(1:10, "x_inter", "x position (mm)")
plot_long(11:20, "x_inter", "x position (mm)")
plot_long(21:33, "x_inter", "x position (mm)")
# Y
plot_long(1:10, "y_inter", "y position (mm)")
plot_long(11:20, "y_inter", "y position (mm)")
plot_long(21:33, "y_inter", "y position (mm)")
# Z
plot_long(1:10, "z_inter", "z position (mm)")
plot_long(11:20, "z_inter", "z position (mm)")
plot_long(21:33, "z_inter", "z position (mm)")




# Filtering ----
# want low pass butterworth filter (zero-shift) with 8 Hz cutoff

# filter order is first argument according to documentation
filter_order = 6
hi_cutoff = 8
nyquist_freq = sampling_freq/2 
bf = butter(filter_order, hi_cutoff/nyquist_freq, type = "low")

# make copy for renaming purpose (lazy)
df_long$unfil_position = df_long$centered_position
# filter data
df_long %>%
  dplyr::group_by(pilot, id, block, coordinate, trial) %>%
  dplyr::mutate(centered_position = signal::filtfilt(bf, unfil_position)) -> df_long

# X
plot_long(1:10, "x_inter", "x position (mm)")
plot_long(11:20, "x_inter", "x position (mm)")
plot_long(21:33, "x_inter", "x position (mm)")
plot_long(bad_ids, "x_inter", "bad x position (mm)")
# Y
plot_long(1:10, "y_inter", "y position (mm)")
plot_long(11:20, "y_inter", "y position (mm)")
plot_long(21:33, "y_inter", "y position (mm)")
# examine some participants that remain problematic
plot_long(bad_ids, "y_inter", "bad y position (mm)")
# Z
plot_long(1:10, "z_inter", "z position (mm)")
plot_long(11:20, "z_inter", "z position (mm)")
plot_long(21:33, "z_inter", "z position (mm)")


# take off early edge effect
edge_start = 50
edge_end = 4500

# get rid of edge effect at beginning
df_long %>% dplyr::filter(time > edge_start, time < edge_end) -> df_long_noedge
rm(df_long)



# Trial Bounds ----
# how far should the fixation* and target be apart?
(ytargetcoor - yfixcoor)
# what are the cutoffs for trial start and end?
# start_line in mm
start_line = 30
end_line = 270

plot_bounds = function(ids_use, dv, y_label, start_line, end_line) {
  
  df_long_noedge %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=time/1000, y=centered_position, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (s)")+
    ylab(y_label)+
    geom_hline(yintercept = start_line, color = "red")+
    geom_hline(yintercept = end_line, color = "red")+
    facet_grid(condition~id) %>% print()
}

# Y
plot_bounds(1:10, "y_inter", "y position (mm)", start_line, end_line)
plot_bounds(11:20, "y_inter", "y position (mm)", start_line, end_line)
plot_bounds(21:33, "y_inter", "y position (mm)", start_line, end_line)
plot_bounds(5, "y_inter", "bad y position (mm)", start_line, end_line)


# Derivatives ----
# position/(frame * seconds/frame) = position/second
df_long_noedge$velocity = c(0, diff(df_long_noedge$centered_position))/(1/sampling_freq)
# (position/second)/(frame * seconds/frame) = position/second^2
df_long_noedge$acceleration = c(0, diff(df_long_noedge$velocity))/(1/sampling_freq)

df_long_noedge %>% dplyr::filter(time != edge_start + 1000/sampling_freq) -> df_long_clean  # getting rid of first frame
rm(df_long_noedge)

# create function to plot velocity and acceleration
plot_devs = function(ids_use, dv, y_label, ylimits, dev = "velocity") {
  df_long_clean$temp = dplyr::pull(df_long_clean, dev)
  
  df_long_clean %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=time, y=temp, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (ms)")+
    ylab(sprintf("%s %s", y_label, dev))+
    ylim(ylimits)+
    facet_grid(id~condition) %>% print()
}


# Velocity
# X
xbounds_velo = c(-1000, 1000)
plot_devs(1:10, "x_inter", "x", xbounds_velo)
plot_devs(11:20, "x_inter", "x", xbounds_velo)
plot_devs(21:33, "x_inter", "x", xbounds_velo)
# Y
ybounds_velo = c(-3000, 3000)
plot_devs(1:10, "y_inter", "y", ybounds_velo)
plot_devs(11:20, "y_inter", "y", ybounds_velo)
plot_devs(21:33, "y_inter", "y", ybounds_velo)
# Z
zbounds_velo = c(-500, 500)
plot_devs(1:10, "z_inter", "z", zbounds_velo)
plot_devs(11:20, "z_inter", "z", zbounds_velo)
plot_devs(21:33, "z_inter", "z", zbounds_velo)

# Acceleration
# X
xbounds_accel = c(-20000, 20000)
plot_devs(1:10, "x_inter", "x", xbounds_accel, "acceleration")
plot_devs(11:20, "x_inter", "x", xbounds_accel, "acceleration")
plot_devs(21:33, "x_inter", "x", xbounds_accel, "acceleration")
# Y
ybounds_accel = c(-30000, 30000)
plot_devs(1:10, "y_inter", "y", ybounds_accel, "acceleration")
plot_devs(11:20, "y_inter", "y", ybounds_accel, "acceleration")
plot_devs(21:33, "y_inter", "y", ybounds_accel, "acceleration")
# Z
ybounds_accel = c(-10000, 10000)
plot_devs(1:10, "z_inter", "z", ybounds_accel, "acceleration")
plot_devs(11:20, "z_inter", "z", ybounds_accel, "acceleration")
plot_devs(21:33, "z_inter", "z", ybounds_accel, "acceleration")



# Trial start ----
# ensure order of data
df_long_clean %>% dplyr::arrange(pilot, id, condition, trial, coordinate, time) -> df_long_clean

# determine when thresholds are exceeded in primary movement axis
df_long_clean %>%
  dplyr::select(pilot, id, condition, trial, coordinate, time, centered_position) %>%
  spread(coordinate, centered_position) %>%
  dplyr::mutate(
    moved = y_inter > start_line
    , stopped = y_inter > end_line
  ) %>%
  gather(coordinate, centered_position, x_inter:z_inter, factor_key = T) %>%
  dplyr::arrange(pilot, id, condition, trial, coordinate, time) -> df_long_moved  # the arrange function is necessary for lining up both data frames

# make sure they are lined up
mean(df_long_moved$centered_position == df_long_clean$centered_position)
mean(df_long_moved$id == df_long_clean$id)
mean(df_long_moved$trial == df_long_clean$trial)
mean(df_long_moved$time == df_long_clean$time)

# add to data frame
df_long_moved %>%
  ungroup() %>%
  dplyr::select(moved, stopped) %>%
  bind_cols(df_long_clean) -> df_long_clean
# get rid of velocity sub-df
rm(df_long_moved)

# determine when thresholds are exceeded in primary movement axis
velo_start = 10
velo_end = 100
df_long_clean %>%
  dplyr::select(pilot, id, condition, trial, coordinate, time, velocity) %>%
  spread(coordinate, velocity) %>%
  dplyr::mutate(
    non_stationary = y_inter > velo_start
    , stationary = y_inter < velo_end
    , greater_than_zero = y_inter > 0
  ) %>%
  gather(coordinate, velocity, x_inter:z_inter, factor_key = T) %>%
  dplyr::arrange(pilot, id, condition, trial, coordinate, time) -> df_long_velo # the arrange function is necessary for lining up both data frames

# make sure they are lined up
mean(df_long_velo$velocity == df_long_clean$velocity)
mean(df_long_velo$id == df_long_clean$id)
mean(df_long_velo$trial == df_long_clean$trial)
mean(df_long_velo$time == df_long_clean$time)

df_long_velo %>% 
  ungroup() %>%
  dplyr::select(non_stationary, stationary, greater_than_zero) %>%
  bind_cols(df_long_clean) -> df_long_clean
# get rid of velocity sub-df
rm(df_long_velo)



# 200 Hz = 200 samples/second ... 'ma_length_ms'/1000 seconds * 200 samples/second = 'ma_length' samples
# start
ma_length_start = 5
ma_length_start_ms = ma_length_start * 1000/200
# end
ma_length_end = 5
ma_length_end_ms = ma_length_end * 1000/200
# we ignore seperations among ids, coordinates, and trials for efficiency
# the following processing should make it so that this does not effect trial starts and ends
df_long_clean$potential_start = as.numeric(ma(df_long_clean$non_stationary & df_long_clean$moved, ma_length_start))  
df_long_clean$potential_end = as.numeric(ma(df_long_clean$stationary & df_long_clean$stopped, ma_length_end)) 

# if 1 then zero cross from negative to positive. If -1 then zero cross from positive to negative 
df_long_clean$zero_cross = diff(c(0, df_long_clean$greater_than_zero)) 

df_long_clean$real_trial_start_bool = df_long_clean$zero_cross == 1
df_long_clean$real_trial_end_bool = df_long_clean$zero_cross == -1

df_long_clean$real_trial_start_frame = df_long_clean$real_trial_start_bool * df_long_clean$frame
df_long_clean$real_trial_end_frame = df_long_clean$real_trial_end_bool * df_long_clean$frame 

# I actually think this is faster than just using mutate
df_long_clean$trial_start_bool = df_long_clean$potential_start == 1
df_long_clean$trial_end_bool = df_long_clean$potential_end == 1

df_long_clean$trial_start_frame = df_long_clean$trial_start_bool * df_long_clean$frame 
# df_long_clean$trial_end_frame = df_long_clean$trial_end_bool * df_long_clean$frame 

# identify first positive change in 'potential end'
df_long_clean$trial_end_diff = diff(c(0,df_long_clean$trial_end_bool))
df_long_clean$trial_end_bool_2 = df_long_clean$trial_end_diff == 1
df_long_clean$trial_end_frame = df_long_clean$trial_end_bool_2 * df_long_clean$frame 

# give some space 
buffer_start = 0
buffer_start_ms = buffer_start * 1000/200

buffer_end = 0
buffer_end_ms = buffer_end * 1000/200

df_long_clean %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    trial_start = sort(unique(trial_start_frame))[2] - (ma_length_start-1)/2 - buffer_start   # we pick the second lowest because there are many zeroes
    , trial_end = sort(unique(trial_end_frame))[2] + (ma_length_end-1)/2  + buffer_end  # we pick the second lowest because there are many -1s
  ) -> df_long_clean

# create difference vectors to find closest real ends and starts 
df_long_clean$closest_real_start = df_long_clean$trial_start - df_long_clean$real_trial_start_frame
df_long_clean$closest_real_end = df_long_clean$real_trial_end_frame - df_long_clean$trial_end 

# roll back to velocity zero crossings 
df_long_clean %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    real_trial_start = trial_start - min(closest_real_start[which(closest_real_start > 0)])
    , real_trial_end = trial_end + min(closest_real_end[which(closest_real_end > 0)])
  ) -> df_long_clean

# identify trials that do not contain trial start or trial end
# count number of trials to be discarded based on trial start OR trial ends
df_long_clean %>%
  dplyr::filter(coordinate == "y_inter") %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::summarise(
    bad_start = is.na(unique(real_trial_start))
    , bad_end = is.na(unique(real_trial_end))
    , bad_trial = is.na(unique(real_trial_start)) | is.na(unique(real_trial_end))
  ) %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarise(
    bad_starts = sum(bad_start)
    , bad_ends = sum(bad_end)
    , bad_trials = sum(bad_trial)
  ) %>% print(n=n*2) %>%
  dplyr::group_by(pilot,id) %>%
  dplyr::summarise(
    bad_starts_tot = sum(bad_starts)
    , bad_ends_tot = sum(bad_ends)
    , bad_trials_tot = sum(bad_trials)
  ) %>% print(n=n) %>%
  dplyr::summarise(
    bad_starts_pop = sum(bad_starts_tot)
    , bad_ends_pop = sum(bad_ends_tot)
    , bad_trials_pop = sum(bad_trials_tot)
  ) %>% print(n=n)
  

# identify trials that don't have proper start and end cutoffs
df_long_clean %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    bad_start = is.na(unique(real_trial_start))
    , bad_end = is.na(unique(real_trial_end))
  ) -> df_long_clean

# get rid of bad trials
df_long_clean %>%
  dplyr::filter(
    bad_start == F
    , bad_end == F
  ) -> df_long_cleaner
rm(df_long_clean)

# count trials per participant and per condition
df_long_cleaner %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(unique(trial))) %>%
  print(n=n*2) %>%  # notice that participant 30 has 
  dplyr::filter(count < 10) # make sure all conditions have at least 10 trials

# trim data set
df_long_cleaner %>%
  dplyr::filter(
    frame >= real_trial_start
    , frame <= real_trial_end
  ) -> df_long_trim
rm(df_long_cleaner)

df_long_trim %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    zero_time = round(time - min(time))  # grouping works for min()
  ) -> df_long_trim




##########################################################
####             Trimmed Position                     ####
##########################################################

# create function to plot trimmed data 
plot_trim = function(ids_use, dv, dev, y_label) {
  
  df_long_trim$tempy = dplyr::pull(df_long_trim, dev)
  
  df_long_trim %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=zero_time, y=tempy, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (ms)")+
    ylab(y_label)+
    facet_grid(id~condition) %>% print()
}


####  NOTE: NORMALIZTION SORT OF TAKES CARE OF OUTLYING TRIALS ####


# X
plot_trim(1:10, "x_inter", "centered_position", "x position (mm)")
plot_trim(11:20, "x_inter", "centered_position", "x position (mm)")
plot_trim(21:33, "x_inter", "centered_position", "x position (mm)")
# Y
plot_trim(1:10, "y_inter", "centered_position", "y position (mm)")
plot_trim(11:20, "y_inter", "centered_position", "y position (mm)")
plot_trim(21:33, "y_inter", "centered_position", "y position (mm)")
plot_trim(5, "y_inter", "centered_position", "y position (mm)")
# Z
plot_trim(1:10, "z_inter", "centered_position", "z position (mm)")
plot_trim(11:20, "z_inter", "centered_position", "z position (mm)")
plot_trim(21:33, "z_inter", "centered_position", "z position (mm)")



# Participant Averages ----
df_long_trim %>%
  group_by(pilot, id, condition, coordinate, zero_time) %>%
  dplyr::summarise(
    position_avg = mean(centered_position) 
    , velocity_avg = mean(velocity) 
    , acceleration_avg = mean(acceleration) 
  ) -> df_long_trim_avg

plot_trim_avg = function(ids_use, dv, dev, y_label) {
  
  df_long_trim_avg$tempy = dplyr::pull(df_long_trim_avg, dev)
  
  df_long_trim_avg %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=zero_time, y=tempy, group=id, color=id), alpha = 0.5, size = .5)+
    xlab("time (ms)")+
    ylab(y_label)+
    facet_grid(condition~.) %>% print()
}

# X
plot_trim_avg(1:10, "x_inter", "position_avg", "x position (mm)")
plot_trim_avg(11:20, "x_inter", "position_avg", "x position (mm)")
plot_trim_avg(21:33, "x_inter", "position_avg", "x position (mm)")
# Y
plot_trim_avg(1:10, "y_inter", "position_avg", "y position (mm)")
plot_trim_avg(11:20, "y_inter", "position_avg", "y position (mm)")
plot_trim_avg(21:33, "y_inter", "position_avg", "y position (mm)")
# Z
plot_trim_avg(1:10, "z_inter", "position_avg", "z position (mm)")
plot_trim_avg(11:20, "z_inter", "position_avg", "z position (mm)")
plot_trim_avg(21:33, "z_inter", "position_avg", "z position (mm)")



# Group Averages ----
df_long_trim_avg %>%
  group_by(condition, coordinate, zero_time) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg) 
    , velocity_grand_avg = mean(velocity_avg)
    , acceleration_grand_avg = mean(acceleration_avg)
  ) -> df_long_trim_grand_avg

# function
plot_trim_grand_avg = function(dv, dev, y_label) {
  
  df_long_trim_grand_avg$tempy = dplyr::pull(df_long_trim_grand_avg, dev)
  
  df_long_trim_grand_avg %>%
    dplyr::filter(coordinate == dv) %>%
    ggplot()+
    geom_line(aes(x=zero_time, y=tempy))+
    xlab("time (ms)")+
    ylab(y_label)+
    facet_grid(.~condition) %>% print()
}

# X
plot_trim_grand_avg("x_inter", "position_grand_avg", "x position (mm)")
# Y
plot_trim_grand_avg("y_inter", "position_grand_avg", "y position (mm)")
# Z
plot_trim_grand_avg("z_inter", "position_grand_avg", "z position (mm)")



##########################################################
####             Trimmed Velocity                     ####
##########################################################

# Trial-wise ----
# X
plot_trim(1:10, "x_inter", "velocity", "x velocity (mm/s)")
plot_trim(11:20, "x_inter", "velocity", "x velocity (mm/s)")
plot_trim(21:33, "x_inter", "velocity", "x velocity (mm/s)")
# Y
plot_trim(1:10, "y_inter", "velocity", "y velocity (mm/s)")
plot_trim(11:20, "y_inter", "velocity", "y velocity (mm/s)")
plot_trim(21:33, "y_inter", "velocity", "y velocity (mm/s)")
# individual participants with delays
plot_trim(1, "y_inter", "velocity", "y velocity (mm/s)")
plot_trim(30, "y_inter", "velocity", "y velocity (mm/s)")
# Z
plot_trim(1:10, "z_inter", "velocity", "z velocity (mm/s)")
plot_trim(11:20, "z_inter", "velocity", "z velocity (mm/s)")
plot_trim(21:33, "z_inter", "velocity", "z velocity (mm/s)")


# Participant ----
# X
plot_trim_avg(1:10, "x_inter", "velocity_avg", "x velocity (mm/s)")
plot_trim_avg(11:20, "x_inter", "velocity_avg", "x velocity (mm/s)")
plot_trim_avg(21:33, "x_inter", "velocity_avg", "x velocity (mm/s)")
# Y
plot_trim_avg(1:10, "y_inter", "velocity_avg", "y velocity (mm/s)")
plot_trim_avg(11:20, "y_inter", "velocity_avg", "y velocity (mm/s)")
plot_trim_avg(21:33, "y_inter", "velocity_avg", "y velocity (mm/s)")
# Z
plot_trim_avg(1:10, "z_inter", "velocity_avg", "z velocity (mm/s)")
plot_trim_avg(11:20, "z_inter", "velocity_avg", "z velocity (mm/s)")
plot_trim_avg(21:33, "z_inter", "velocity_avg", "z velocity (mm/s)")


# Group Averages ----
# X
plot_trim_grand_avg("x_inter", "velocity_grand_avg", "x velocity (mm/s)")
# Y
plot_trim_grand_avg("y_inter", "velocity_grand_avg", "y velocity (mm/s)")
# Z
plot_trim_grand_avg("z_inter", "velocity_grand_avg", "z velocity (mm/s)")



##########################################################
####             Trimmed Acceleration                 ####
##########################################################

# Trial-wise ----
# X
plot_trim(1:10, "x_inter", "acceleration", "x acceleration (mm/s^2)")
plot_trim(11:20, "x_inter", "acceleration", "x acceleration (mm/s^2)")
plot_trim(21:33, "x_inter", "acceleration", "x acceleration (mm/s^2)")
# Y
plot_trim(1:10, "y_inter", "acceleration", "y acceleration (mm/s^2)")
plot_trim(11:20, "y_inter", "acceleration", "y acceleration (mm/s^2)")
plot_trim(21:33, "y_inter", "acceleration", "y acceleration (mm/s^2)")
# Z
plot_trim(1:10, "z_inter", "acceleration", "z acceleration (mm/s^2)")
plot_trim(11:20, "z_inter", "acceleration", "z acceleration (mm/s^2)")
plot_trim(21:33, "z_inter", "acceleration", "z acceleration (mm/s^2)")


# Participant ----
# X
plot_trim_avg(1:10, "x_inter", "acceleration_avg", "x acceleration (mm/s^2)")
plot_trim_avg(11:20, "x_inter", "acceleration_avg", "x acceleration (mm/s^2)")
plot_trim_avg(21:33, "x_inter", "acceleration_avg", "x acceleration (mm/s^2)")
# Y
plot_trim_avg(1:10, "y_inter", "acceleration_avg", "y acceleration (mm/s^2)")
plot_trim_avg(11:20, "y_inter", "acceleration_avg", "y acceleration (mm/s^2)")
plot_trim_avg(21:33, "y_inter", "acceleration_avg", "y acceleration (mm/s^2)")
# Z
plot_trim_avg(1:10, "z_inter", "acceleration_avg", "z acceleration (mm/s^2)")
plot_trim_avg(11:20, "z_inter", "acceleration_avg", "z acceleration (mm/s^2)")
plot_trim_avg(21:33, "z_inter", "acceleration_avg", "z acceleration (mm/s^2)")


# Group Averages ----
# X
plot_trim_grand_avg("x_inter", "acceleration_grand_avg", "x acceleration (mm/s^2)")
# Y
plot_trim_grand_avg("y_inter", "acceleration_grand_avg", "y acceleration (mm/s^2)")
# Z
plot_trim_grand_avg("z_inter", "acceleration_grand_avg", "z acceleration (mm/s^2)")




##########################################################
####            Normalized Waveforms                  ####
##########################################################


# Normalize ----
round0 = function(x,z){
  round(x/z,0)*z
}

# what is the resolution?
norm_size = 0.05

# normalize time
df_long_trim %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    norm_time = round0((zero_time-min(zero_time))/(max(zero_time)-min(zero_time)), norm_size)
    ) -> df_long_norm2

df_long_norm2 %>%
  dplyr::group_by(blocking, pilot, id, condition, trial, coordinate, norm_time) %>%
  dplyr::summarize(
    norm_position = mean(centered_position) 
    , norm_velocity = mean(velocity)
    , norm_acceleration = mean(acceleration)
    
    , good_movement_time = unique(good_movement_time)
    , good_response_time = unique(good_response_time)
    , good_rt = unique(good_rt)
    , fix_error = unique(fix_error)
    , target_error = unique(target_error)
    , CE_amp = unique(CE_amp)
    , CE_dir = unique(CE_dir)
    , good_xfix = unique(good_xfix)
    , good_yfix = unique(good_yfix)
    , good_xtarget = unique(good_xtarget)
    , good_ytarget = unique(good_ytarget)
  ) -> df_long_norm


# Normalized Trials ----
# get peak acceleration and decceleration values for each participant 
df_long_norm %>%
  dplyr::group_by(blocking, pilot, id, condition, coordinate, trial) %>%
  dplyr::summarize(
    time_peak_velocity = norm_time[which(norm_velocity == max(norm_velocity))]
    , time_peak_acceleration = norm_time[which(norm_acceleration == max(norm_acceleration))]
    , time_peak_deceleration = norm_time[which(norm_acceleration == min(norm_acceleration))]
    , peak_velocity = max(norm_velocity)
    , peak_acceleration = max(norm_acceleration)
    , peak_deceleration = min(norm_acceleration)
  ) -> kms

plot_norm = function(ids_use, dv, dev, y_label) {
  df_long_norm$tempy = dplyr::pull(df_long_norm, dev)
  rang = range(df_long_norm$tempy[df_long_norm$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat = kms[kms$coordinate == dv & (as.numeric(kms$id) %in% ids_use),]
  
  df_long_norm %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=tempy, group=trial, color=trial), alpha = 0.5, size = .2)+
    geom_point(data = kin_dat, aes(x=time_peak_velocity, y=top + data_range/10, group=trial, color=trial), alpha = 0.5, size = 0.2)+
    geom_point(data = kin_dat, aes(x=time_peak_acceleration, y=top, group=trial, color=trial), alpha = 0.5, size = 0.2)+
    geom_point(data = kin_dat, aes(x=time_peak_deceleration, y=bottom, group=trial, color=trial), alpha = 0.5, size = 0.2)+
    xlab("normalized time")+
    ylab(y_label)+
    facet_grid(id~condition) %>% print()
}

# X
plot_norm(1:10, "x_inter", "norm_position", "x position (mm)")
plot_norm(11:20, "x_inter", "norm_position", "x position (mm)")
plot_norm(21:33, "x_inter", "norm_position", "x position (mm)")
# Y
plot_norm(1:10, "y_inter", "norm_position", "y position (mm)")
plot_norm(11:20, "y_inter", "norm_position", "y position (mm)")
plot_norm(21:33, "y_inter", "norm_position", "y position (mm)")
# P33 appears to be the outlying one from the data
# when comparing to the other trajectories, it appears that 33 overshoots the target more often
# this would explain why the average presented below appears to peak earlier than it should
plot_norm(33, "y_inter", "norm_position", "y position (mm)")
# Z
plot_norm(1:10, "z_inter", "norm_position", "z position (mm)")
plot_norm(11:20, "z_inter", "norm_position", "z position (mm)")
plot_norm(21:33, "z_inter", "norm_position", "z position (mm)")



# Normalized Averages ----
df_long_norm %>%
  group_by(blocking, pilot, id, condition, coordinate, norm_time) %>%
  dplyr::summarise(
    position_avg = mean(norm_position)
    , velocity_avg = mean(norm_velocity)
    , acceleration_avg = mean(norm_acceleration)
  ) -> df_long_norm_avg

# average over peak times 
kms %>%
  dplyr::group_by(blocking, pilot, id, condition, coordinate) %>%
  dplyr::summarise(
    time_peak_velocity = mean(time_peak_velocity)
    , time_peak_acceleration = mean(time_peak_acceleration)
    , time_peak_deceleration = mean(time_peak_deceleration)
    , peak_velocity = mean(peak_velocity)
    , peak_acceleration = mean(peak_acceleration)
    , peak_deceleration = mean(peak_deceleration)
  ) -> kms_ids


# create function
plot_norm_avg = function(ids_use, dv, dev, y_label) {
  df_long_norm_avg$tempy = dplyr::pull(df_long_norm_avg, dev)
  rang = range(df_long_norm_avg$tempy[df_long_norm_avg$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat = kms_ids[kms_ids$coordinate == dv & (as.numeric(kms_ids$id) %in% ids_use),]
  
  df_long_norm_avg %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=tempy, group=id, color=id), size = .3)+
    geom_point(data = kin_dat, aes(x=time_peak_velocity, y=top + data_range/50, group=id, color=id), size = 1)+
    geom_point(data = kin_dat, aes(x=time_peak_acceleration, y=top, group=id, color=id), size = 1)+
    geom_point(data = kin_dat, aes(x=time_peak_deceleration, y=bottom, group=id, color=id), size = 1)+
    xlab("normalized time")+
    ylab(y_label)+
    facet_grid(.~condition) %>% print()
}

# X
plot_norm_avg(1:10, "x_inter", "position_avg", "x position (mm)")
plot_norm_avg(11:20, "x_inter", "position_avg", "x position (mm)")
plot_norm_avg(21:33, "x_inter", "position_avg", "x position (mm)")
# Y
plot_norm_avg(1:10, "y_inter", "position_avg", "y position (mm)")
plot_norm_avg(11:20, "y_inter", "position_avg", "y position (mm)")
plot_norm_avg(21:33, "y_inter", "position_avg", "y position (mm)")
# P33 appears to be the outlying one from the data
plot_norm_avg(33, "y_inter", "position_avg", "y position (mm)")
# Z
plot_norm_avg(1:10, "z_inter", "position_avg", "z position (mm)")
plot_norm_avg(11:20, "z_inter", "position_avg", "z position (mm)")
plot_norm_avg(21:33, "z_inter", "position_avg", "z position (mm)")


# Normalize Grand Averages ----
df_long_norm_avg %>%
  group_by(blocking, condition, coordinate, norm_time) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg) 
    , velocity_grand_avg = mean(velocity_avg)
    , acceleration_grand_avg = mean(acceleration_avg)
  ) -> df_long_norm_grand_avg

# get average kinematic markers for each group
kms_ids %>%
  dplyr::group_by(blocking, condition, coordinate) %>%
  dplyr::summarise(
    time_peak_velocity= mean(time_peak_velocity) 
    , time_peak_acceleration = mean(time_peak_acceleration)
    , time_peak_deceleration =  mean(time_peak_deceleration)
    , peak_velocity= mean(peak_velocity) 
    , peak_acceleration = mean(peak_acceleration)
    , peak_deceleration =  mean(peak_deceleration)
  ) -> kms_avg
kms_avg
  

# function
plot_norm_grand_avg = function(dv, dev, y_label) {
  df_long_norm_grand_avg$tempy = dplyr::pull(df_long_norm_grand_avg, dev)
  rang = range(df_long_norm_grand_avg$tempy[df_long_norm_grand_avg$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat = kms_avg[kms_avg$coordinate == dv,]
  
  df_long_norm_grand_avg %>%
    dplyr::filter(coordinate == dv) %>%
    ggplot(aes(x=norm_time, y=tempy, group=condition, color=condition))+
    geom_line()+
    geom_point(data = kin_dat, aes(x=time_peak_velocity, y=top+data_range/50, group=condition, color=condition), size = 1)+
    geom_point(data = kin_dat, aes(x=time_peak_acceleration, y=top, group=condition, color=condition), size = 1)+
    geom_point(data = kin_dat, aes(x=time_peak_deceleration, y=bottom, group=condition, color=condition), size = 1)+
    facet_grid(.~blocking)+
    xlab("normalized time")+
    ylab(y_label) %>% print()
}

# X
plot_norm_grand_avg("x_inter", "position_grand_avg", "x position (mm)")
# Y
plot_norm_grand_avg("y_inter", "position_grand_avg", "y position (mm)")
# Z
plot_norm_grand_avg("z_inter", "position_grand_avg", "z position (mm)")




##########################################################
####            Normalized Velocity                   ####
##########################################################

# Normalized Trials ----
# X
plot_norm(1:10, "x_inter", "norm_velocity", "x velocity (mm/s)")
plot_norm(11:20, "x_inter", "norm_velocity", "x velocity (mm/s)")
plot_norm(21:33, "x_inter", "norm_velocity", "x velocity (mm/s)")
# Y
plot_norm(1:10, "y_inter", "norm_velocity", "y velocity (mm/s)")
plot_norm(11:20, "y_inter", "norm_velocity", "y velocity (mm/s)")
plot_norm(21:33, "y_inter", "norm_velocity", "y velocity (mm/s)")
# Z
plot_norm(1:10, "z_inter", "norm_velocity", "z velocity (mm/s)")
plot_norm(11:20, "z_inter", "norm_velocity", "z velocity (mm/s)")
plot_norm(21:33, "z_inter", "norm_velocity", "z velocity (mm/s)")


# Normalized Averages ----
# X
plot_norm_avg(1:10, "x_inter", "velocity_avg", "x velocity (mm/s)")
plot_norm_avg(11:20, "x_inter", "velocity_avg", "x velocity (mm/s)")
plot_norm_avg(21:33, "x_inter", "velocity_avg", "x velocity (mm/s)")
# Y
plot_norm_avg(1:10, "y_inter", "velocity_avg", "y velocity (mm/s)")
plot_norm_avg(11:20, "y_inter", "velocity_avg", "y velocity (mm/s)")
plot_norm_avg(21:33, "y_inter", "velocity_avg", "y velocity (mm/s)")
# Z
plot_norm_avg(1:10, "z_inter", "velocity_avg", "z velocity (mm/s)")
plot_norm_avg(11:20, "z_inter", "velocity_avg", "z velocity (mm/s)")
plot_norm_avg(21:33, "z_inter", "velocity_avg", "z velocity (mm/s)")


# Normalize Grand Averages ----
# X
plot_norm_grand_avg("x_inter", "velocity_grand_avg", "x velocity (mm/s)")
# Y
plot_norm_grand_avg("y_inter", "velocity_grand_avg", "y velocity (mm/s)")
# Z
plot_norm_grand_avg("z_inter", "velocity_grand_avg", "z velocity (mm/s)")




##########################################################
####            Normalized Acceleration               ####
##########################################################

# Normalized Trials ----
# X
plot_norm(1:10, "x_inter", "norm_acceleration", "x acceleration (mm/s^2)")
plot_norm(11:20, "x_inter", "norm_acceleration", "x acceleration (mm/s^2)")
plot_norm(21:33, "x_inter", "norm_acceleration", "x acceleration (mm/s^2)")
# Y
plot_norm(1:10, "y_inter", "norm_acceleration", "y acceleration (mm/s^2)")
plot_norm(11:20, "y_inter", "norm_acceleration", "y acceleration (mm/s^2)")
plot_norm(21:33, "y_inter", "norm_acceleration", "y acceleration (mm/s^2)")
# Z
plot_norm(1:10, "z_inter", "norm_acceleration", "z acceleration (mm/s^2)")
plot_norm(11:20, "z_inter", "norm_acceleration", "z acceleration (mm/s^2)")
plot_norm(21:33, "z_inter", "norm_acceleration", "z acceleration (mm/s^2)")



# Normalized Averages ----
# X
plot_norm_avg(1:10, "x_inter", "acceleration_avg", "x acceleration (mm/s^2)")
plot_norm_avg(11:20, "x_inter", "acceleration_avg", "x acceleration (mm/s^2)")
plot_norm_avg(21:33, "x_inter", "acceleration_avg", "x acceleration (mm/s^2)")
# Y
plot_norm_avg(1:10, "y_inter", "acceleration_avg", "y acceleration (mm/s^2)")
plot_norm_avg(11:20, "y_inter", "acceleration_avg", "y acceleration (mm/s^2)")
plot_norm_avg(21:33, "y_inter", "acceleration_avg", "y acceleration (mm/s^2)")
# Z
plot_norm_avg(1:10, "z_inter", "acceleration_avg", "z acceleration (mm/s^2)")
plot_norm_avg(11:20, "z_inter", "acceleration_avg", "z acceleration (mm/s^2)")
plot_norm_avg(21:33, "z_inter", "acceleration_avg", "z acceleration (mm/s^2)")


# Normalize Grand Averages ----
# X
plot_norm_grand_avg("x_inter", "acceleration_grand_avg", "x acceleration (mm/s^2)")
# Y
plot_norm_grand_avg("y_inter", "acceleration_grand_avg", "y acceleration (mm/s^2)")
# Z
plot_norm_grand_avg("z_inter", "acceleration_grand_avg", "z acceleration (mm/s^2)")




##########################################################
####          Spatial Variability Profiles            ####
##########################################################

df_long_norm %>%
  dplyr::group_by(blocking, pilot, id, condition, coordinate, norm_time) %>%
  dplyr::summarise(spat_var = sd(norm_position)) -> df_long_spat_var

# function to plot spatial variability
plot_spat_var = function(ids_use, dv, y_label) {
  
  rang = range(df_long_spat_var$spat_var[df_long_spat_var$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat = kms_ids[kms_ids$coordinate == dv & (as.numeric(kms_ids$id) %in% ids_use),]
  
  df_long_spat_var %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=spat_var, group=id, color=id), alpha = 0.5, size = .5)+
    geom_point(data = kin_dat, aes(x=time_peak_velocity, y=top + data_range/50, group=id, color=id), size = 1)+
    geom_point(data = kin_dat, aes(x=time_peak_acceleration, y=top, group=id, color=id), size = 1)+
    geom_point(data = kin_dat, aes(x=time_peak_deceleration, y=bottom, group=id, color=id), size = 1)+
    xlab("normalized time")+
    ylab(y_label)+
    facet_grid(.~condition) %>% print()
}

# X
plot_spat_var(1:10, "x_inter", "x spatial variability (sd)")
plot_spat_var(11:20, "x_inter", "x spatial variability (sd)")
plot_spat_var(21:33, "x_inter", "x spatial variability (sd)")
# Y
plot_spat_var(1:10, "y_inter", "y spatial variability (sd)")
plot_spat_var(11:20, "y_inter", "y spatial variability (sd)")
plot_spat_var(21:33, "y_inter", "y spatial variability (sd)")
# Z
plot_spat_var(1:10, "z_inter", "z spatial variability (sd)")
plot_spat_var(11:20, "z_inter", "z spatial variability (sd)")
plot_spat_var(21:33, "z_inter", "z spatial variability (sd)")


# Average of Spatial Variabilty Profiles ----
df_long_spat_var %>%
  dplyr::group_by(blocking, condition, coordinate, norm_time) %>%
  dplyr::summarise(spat_var_avg = mean(spat_var)) -> df_long_spat_var_avg  

# NOTE: outlying trials/participants are affecting results
# difference between mean and median averages


# a function to plot group spatial variability profiles
plot_spat_var_avg = function(dv, y_label) {
    
  rang = range(df_long_spat_var_avg$spat_var_avg[df_long_spat_var_avg$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat = kms_avg[kms_avg$coordinate == dv,]
  
  df_long_spat_var_avg %>%
    dplyr::filter(coordinate == dv) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=spat_var_avg, group=condition, color=condition))+
    geom_point(data = kin_dat, aes(x=time_peak_velocity, y=top + data_range/50, group=condition, color=condition), size = 1)+
    geom_point(data = kin_dat, aes(x=time_peak_acceleration, y=top, group=condition, color=condition), size = 1)+
    geom_point(data = kin_dat, aes(x=time_peak_deceleration, y=bottom, group=condition, color=condition), size = 1)+
    xlab("normalized time")+
    facet_grid(.~blocking)+
    ylab(y_label) %>% print()
}

# X
plot_spat_var_avg("x_inter", "x spatial variability (sd)")
# Y
plot_spat_var_avg("y_inter", "y spatial variability (sd)")
# Z
plot_spat_var_avg("z_inter", "z spatial variability (sd)")




########################################################
####            Outcome Variables                   ####
########################################################

df_long_norm %>%
  dplyr::group_by(blocking, pilot, id, condition, trial) %>%
  dplyr::summarise(
    good_movement_time = unique(good_movement_time)
    , good_response_time = unique(good_response_time)
    , good_rt = unique(good_rt)
    , fix_error = unique(fix_error)
    , target_error = unique(target_error)
    , CE_amp = unique(CE_amp)
    , CE_dir = unique(CE_dir)
    , good_xfix = unique(good_xfix)
    , good_yfix = unique(good_yfix)
    , good_xtarget = unique(good_xtarget)
    , good_ytarget = unique(good_ytarget)
  ) -> df_outcomes

# summarize cleaned outcome variable data
summary(df_outcomes)
# see how well counter balanced block is
df_outcomes %>%
  dplyr::group_by(blocking) %>%
  dplyr::summarise(
    count = length(unique(id))
  )   

# how many trials per condition per participant
df_outcomes %>%
  dplyr::group_by(id, condition) %>%
  dplyr::summarise(
    count = length(unique(trial))
  ) %>% print(n=n*2) -> reduced
# compare to non-reduced data frame
df_long_norm %>%
  dplyr::group_by(id, condition) %>%
  dplyr::summarise(
    count = length(unique(trial))
  ) -> full
mean(reduced$count == full$count)



# create function
plot_clean_outcomes = function(ids_use, dv, xlabel) {
  
  df_outcomes$temp = dplyr::pull(df_outcomes, dv)
  
  gg = ggplot()+
    geom_histogram(data = subset(df_outcomes, condition == "no_vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), bins=20, alpha = 0.2, fill = "red")+
    geom_density(data = subset(df_outcomes, condition == "no_vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), color = "red")+
    geom_histogram(data = subset(df_outcomes, condition == "vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), bins=20, alpha = 0.2, fill = "blue")+
    geom_density(data = subset(df_outcomes, condition == "vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), color = "blue")+
    facet_grid(id~.)+
    xlab(sprintf("%s (ms)", xlabel))
  print(gg)
}

sum_clean_outcomes = function(dv) {
  df_outcomes$temp = dplyr::pull(df_outcomes, dv)
  df_outcomes %>%
    dplyr::group_by(blocking, pilot, id, condition) %>%
    dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
    dplyr::group_by(blocking, condition) %>%
    dplyr::summarize(mean_tot = mean(mean_id, na.rm = T))%>%
    print()
  df_outcomes %>%
    dplyr::group_by(blocking, pilot, id, condition) %>%
    dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
    dplyr::group_by(blocking, condition) %>%
    dplyr::summarize(sd_tot = sd(mean_id, na.rm = T))%>%
    print()
  # sd of difference
  df_outcomes %>%
    dplyr::group_by(blocking, pilot, id, condition) %>%
    dplyr::summarize(mean_id = mean(temp, na.rm = T)) %>%
    dplyr::group_by(blocking, pilot, id) %>%
    dplyr::summarize(diff_id = diff(mean_id), na.rm = T) %>%
    dplyr::group_by(blocking) %>%
    dplyr::summarize(sd_effect = sd(diff_id, na.rm = T))%>%
    print()
}


# Reaction Time ----
plot_clean_outcomes(1:10, "good_rt", "reaction time")
plot_clean_outcomes(11:20, "good_rt", "reaction time")
plot_clean_outcomes(21:33, "good_rt", "reaction time")
sum_clean_outcomes("good_rt")


# Response Time ----
plot_clean_outcomes(1:10, "good_response_time", "response time")
plot_clean_outcomes(11:20, "good_response_time", "response time")
plot_clean_outcomes(21:33, "good_response_time", "response time")
sum_clean_outcomes("good_response_time")


# Movement Time ----
plot_clean_outcomes(1:10, "good_movement_time", "movement time")
plot_clean_outcomes(11:20, "good_movement_time", "movement time")
plot_clean_outcomes(21:33, "good_movement_time", "movement time")
sum_clean_outcomes("good_movement_time")


# Movement Error ----
plot_clean_error = function(ids_use) {
  
  df_outcomes %>%
    dplyr::filter(as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_point(aes(x=good_xfix, y=good_yfix, group = id), na.rm = T, size = 0.25)+
    geom_point(aes(x=xfixcoor, y=yfixcoor, group = id), na.rm = T, size = 0.5, color = "red")+
    geom_point(aes(x=good_xtarget, y=good_ytarget, group = id), na.rm = T, size = 0.25)+
    geom_point(aes(x=xtargetcoor, y=ytargetcoor, group = id), na.rm = T, size = 0.5, color = "red")+
    xlim(c(0, xdim))+
    ylim(c(0, ydim))+
    xlab("x (mm)")+
    ylab("y (mm)")+
    facet_grid(condition~id) %>% print()
}

# use function
plot_clean_error(1:10)
plot_clean_error(11:20)
plot_clean_error(21:33)


# Absolute (2D) Error ----
# at fixation
sum_clean_outcomes("fix_error")

# at target
sum_clean_outcomes("target_error")


# Amplitude Errors ----
# both Constant (first) and Variable Errors (second) are Shown
sum_clean_outcomes("CE_amp")


# Directional Errors ----
# both Constant (first) and Variable Errors (second) are Shown
sum_clean_outcomes("CE_dir")



#### GENERAL NOTE: more variability of outcome DVs in 'vision first' blocking condition ####



########################################################
####           rANOVAs Outcomes                     ####
########################################################

df_outcomes %>%
  dplyr::group_by(blocking, pilot, id, condition) %>%
  dplyr::summarise(
    good_movement_time = mean(good_movement_time)
    , good_response_time = mean(good_response_time)
    , good_rt = mean(good_rt)
    , fix_error = mean(fix_error)
    , target_error = mean(target_error)
    , VE_amp = sd(CE_amp)
    , CE_amp = mean(CE_amp)
    , VE_dir = sd(CE_dir)
    , CE_dir = mean(CE_dir)
  ) -> df_outcomes_avg

df_outcomes_avg$condition = revalue(df_outcomes_avg$condition, c("no_vision"="NV", "vision" = "V"))
df_outcomes_avg$blocking = revalue(df_outcomes_avg$blocking, c("no_vision"="NV", "vision" = "V"))

# create function to fun ANOVAs and summarize data
do_anovas = function(dv, YL, XL, SL) {
  df_outcomes_avg$temp = dplyr::pull(df_outcomes_avg, dv)
  ez_print = ezANOVA(
    df_outcomes_avg
    , dv = temp
    , wid = id
    , within = condition
    # , between = blocking
    # , return_aov = T
  )
  print(ez_print)
  ezPlot(
    df_outcomes_avg
    , dv = temp
    , wid = id
    , within = condition
    # , between = blocking
    , x = condition
    # , split = blocking
    , do_bar = T
    , y_lab = YL
    , x_lab = XL
    # , split_lab = SL
  )
}


# RT ----
do_anovas("good_rt", "RT (ms)", "Vision Condition", "Blocking")


# MT ----
do_anovas("good_movement_time", "MT (ms)", "Vision Condition", "Blocking")


# Response Time ----
do_anovas("good_response_time", "Response Time (ms)", "Vision Condition", "Blocking")


# Absolute (2D) Error ----
do_anovas("target_error", "Target Error (mm)", "Vision Condition", "Blocking")


# Constant Error (Amplitude) ----
do_anovas("CE_amp",  "CE (amplitude; mm)", "Vision Condition", "Blocking")


# Constant Error (Direction) ----
do_anovas("CE_dir",  "CE (direction; mm)", "Vision Condition", "Blocking")


# Variable Error (Amplitude) ----
do_anovas("VE_amp",  "VE (amplitude; mm)", "Vision Condition", "Blocking")


# Variable Error (Direction) ----
do_anovas("VE_dir",  "VE (direction; mm)", "Vision Condition", "Blocking")




########################################################
####                    TAPV                        ####
########################################################

df_ykinematics = subset(kms_ids, coordinate == "y_inter")
df_ykinematics$time_after_peak_velocity = 1 - df_ykinematics$time_peak_velocity

df_ykinematics$condition = revalue(df_ykinematics$condition, c("no_vision"="NV", "vision" = "V"))
df_ykinematics$blocking = revalue(df_ykinematics$blocking, c("no_vision"="NV", "vision" = "V"))

ezANOVA(
  df_ykinematics
  , dv = time_after_peak_velocity
  , wid = id
  , within = condition
  # , between = blocking
)
ezPlot(
  df_ykinematics
  , dv = time_after_peak_velocity
  , wid = id
  , within = condition
  # , between = blocking
  , x = condition
  # , split = blocking
  , do_bar = T
  , y_lab = "TAPV (ms)"
  , x_lab = "Vision Condition"
  # , split_lab = "Blocking"
)



########################################################
####            Spatial Variability                 ####
########################################################

df_spat_var = merge(df_long_spat_var, kms_ids)

mround <- function(x,base){ 
  base*round(x/base) 
} 

df_spat_var %>%
  dplyr::mutate(
   time_peak_velocity_round = mround(time_peak_velocity, 0.05)
   , time_peak_acceleration_round = mround(time_peak_acceleration, 0.05)
   , time_peak_deceleration_round = mround(time_peak_deceleration, 0.05)
  ) -> df_spat_var

df_spat_var %>%
  dplyr::group_by(pilot, id, condition, coordinate) %>%
  dplyr::summarize(
   PV = spat_var[norm_time == time_peak_velocity_round]
   , PA = spat_var[norm_time == time_peak_acceleration_round]
   , PD = spat_var[norm_time == time_peak_deceleration_round]
   , END = spat_var[norm_time == 1]
  ) %>%
  gather("kinematic_marker", "spatial_variability", PV:END) -> df_spat_var_anova

# change factor order for plot
df_spat_var_anova$kinematic_marker = factor(df_spat_var_anova$kinematic_marker, levels = c("PA", "PV", "PD", "END"))

# Y ----
df_yspat_var_anova = subset(df_spat_var_anova, coordinate == "y_inter")

df_yspat_var_anova$condition = revalue(df_yspat_var_anova$condition, c("vision" = "V", "no_vision"="NV"))

# change factor order
df_yspat_var_anova$condition = factor(df_yspat_var_anova$condition, levels = c("V", "NV"))

ezANOVA(
  df_yspat_var_anova
  , dv = spatial_variability
  , wid = id
  , within = .(condition, kinematic_marker)
)
ezPlot(
  df_yspat_var_anova
  , dv = spatial_variability
  , wid = id
  , within = .c(condition, kinematic_marker)
  , x = kinematic_marker
  , split = condition
  , do_bar = T
  , y_lab = "Spatial Variability (SD)"
  , x_lab = "Kinematic Marker"
  , split_lab = "Vision Condition"
)



########################################################
####               Ellipsoid                        ####
########################################################

df_spat_var %>%
  dplyr::select(pilot, id, condition, coordinate, norm_time, spat_var) %>%
  spread(coordinate, spat_var) -> df_ellipsoid

df_ellipsoid$volume = 4/3 * pi * df_ellipsoid$x_inter * df_ellipsoid$y_inter * df_ellipsoid$z_inter

# function to plot ellipsoid volumes
plot_ellipsoids = function(ids_use, y_label) {
  df_ellipsoid %>%
    dplyr::filter(as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=volume, group=id, color=id), alpha = 0.5, size = .5)+
    xlab("normalized time")+
    ylab(y_label)+
    facet_grid(.~condition) %>% print()
}

# use function
plot_ellipsoids(1:10, "volume (mm^3)")
plot_ellipsoids(11:21, "volume (mm^3)")
plot_ellipsoids(21:30, "volume (mm^3)")

# change factor names
df_ellipsoid$condition = revalue(df_ellipsoid$condition, c("vision" = "V", "no_vision"="NV"))

# change factor order
df_ellipsoid$condition = factor(df_ellipsoid$condition, levels = c("V", "NV"))

# convert to factor
df_ellipsoid$norm_time = factor(df_ellipsoid$norm_time)

# run ANOVAs
ezANOVA(
  df_ellipsoid
  , dv = volume
  , wid = id
  , within = .(condition, norm_time)
)
ezPlot(
  df_ellipsoid
  , dv = volume
  , wid = id
  , within = .c(condition, norm_time)
  , x =  norm_time
  , split = condition
  , do_bar = T
  , y_lab = "Volume (mm cubed)"
  , x_lab = "Proportion of Movement"
  , split_lab = "Vision Condition"
)



########################################################
####                   R^2                          ####
########################################################

# merge kinematic markers with 
df_rsq = merge(df_long_norm, kms)

# create rounded time variables
df_rsq %>%
  dplyr::mutate(
    time_peak_velocity_round = mround(time_peak_velocity, 0.05)
    , time_peak_acceleration_round = mround(time_peak_acceleration, 0.05)
    , time_peak_deceleration_round = mround(time_peak_deceleration, 0.05)
  ) -> df_rsq

# NOTE: rounding is not necessary - they are already rounded
mean(df_rsq$time_peak_velocity == df_rsq$time_peak_velocity_round)

df_rsq %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::summarize(
    PV = norm_position[norm_time == time_peak_velocity_round]
    , PA = norm_position[norm_time == time_peak_acceleration_round]
    , PD = norm_position[norm_time == time_peak_deceleration_round]
    , END = norm_position[norm_time == 1]
  ) -> df_rsq_lm

# for each participant and each condition extract R^2
df_rsq_anova2 = ddply(
  .data = df_rsq_lm
  , .variables = .(pilot, id, condition, coordinate)
  , .fun = function(x) {
    m_PA = lm(END~PA, data=x)
    m_PV = lm(END~PV, data=x)
    m_PD = lm(END~PD, data=x)
    
    coor = unique(x$coordinate)
    id = unique(x$id)
    cond = unique(x$condition)
    
    # plot(END~PA, data = x)
    # title(main = sprintf("%s %s %s", id, coor, cond))
    # plot(END~PV, data = x)
    # title(main = sprintf("%s %s %s", id, coor, cond))
    # plot(END~PD, data = x)
    # title(main = sprintf("%s %s %s", id, coor, cond))
    
    R_PA = summary(m_PA)$r.squared
    R_PV = summary(m_PV)$r.squared
    R_PD = summary(m_PD)$r.squared
    
    return(data.frame(PA = R_PA, PV = R_PV, PD = R_PD))
  }
)

df_rsq_anova2 %>%
  gather("kinematic_marker", "R_sq", PA:PD) -> df_rsq_anova

df_rsq_anova$kinematic_marker = factor(df_rsq_anova$kinematic_marker, levels = c("PA", "PV", "PD"))


# Y ----
df_yrsq_anova = subset(df_rsq_anova, coordinate == "y_inter")

df_yrsq_anova$condition = revalue(df_yrsq_anova$condition, c("no_vision"="NV", "vision" = "V"))

# change factor order
df_yrsq_anova$condition = factor(df_yrsq_anova$condition, c("V", "NV"))

# run anova on R^2 analysis
ezANOVA(
  df_yrsq_anova
  , dv = R_sq
  , wid = id
  , within = .(condition, kinematic_marker)
)
ezPlot(
  df_yrsq_anova
  , dv = R_sq
  , wid = id
  , within = .c(condition, kinematic_marker)
  , x = kinematic_marker
  , split = condition
  , do_bar = T
  , y_lab = "R Squared"
  , x_lab = "Kinematic Marker"
  , split_lab = "Vision Condition"
)



########################################################
####                Discontinuities                 ####
########################################################

# identify trials with negative velocities 
df_long_norm$neg_velo = (df_long_norm$norm_velocity < 0) & (df_long_norm$norm_time != 1) # make sure end of movement is not captured

# identify first trial that goes from positive to negative 
df_long_norm %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    neg_velo_diff = diff(c(1,neg_velo))
    , neg_velo_bool = neg_velo_diff == 1 # difference of 1 should mean positive to negative  
  ) -> df_long_norm

# get time associated with correction start 
df_long_norm$neg_velo_time = df_long_norm$neg_velo_bool * df_long_norm$norm_time

# label trials
df_long_norm %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    neg_velo_trial = max(neg_velo)
  ) -> df_long_norm

# how many are there?
df_long_norm %>%
  dplyr::filter(coordinate == "y_inter") %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::summarize(
    neg_velo_trial = unique(neg_velo_trial)
  ) %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarise(
    count = sum(neg_velo_trial)
  )
  
# identify when acceleration is negative 
df_long_norm$neg_accel = df_long_norm$norm_acceleration < 0

df_long_norm %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    neg_accel_diff = diff(c(0,neg_accel)) # positive difference should mean positive to negative
    # negative difference should mean negative to positive 
    , neg_accel_bool = neg_accel_diff == -1
  ) -> df_long_norm

# get time associated with correction start 
df_long_norm$neg_accel_time = df_long_norm$neg_accel_bool * df_long_norm$norm_time

df_long_norm %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    neg_accel_trial = min(neg_accel_diff) == -1
  ) -> df_long_norm
  
# how many are there?
df_long_norm %>%
  dplyr::filter(coordinate == "y_inter") %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::summarize(
    neg_accel_trial = unique(neg_accel_trial)
  ) %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarise(
    count = sum(neg_accel_trial)
  )

#### NOTE: how to do IECE when there is no reliable way to determine error at a given stage in the movement? I could extrapolate from screen error where target should be for given trial. ####

# Identify/Plot Discontinuity Time Points ----
df_long_norm %>%
  dplyr::filter(coordinate == "y_inter") %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::summarize(
    neg_accel_trial = unique(neg_accel_trial)
    , accel_time_point = unique(neg_accel_time)[2]  # we pick earliest discontinuity
    , neg_velo_trial = unique(neg_velo_trial)
    , velo_time_point = unique(neg_velo_time)[2]
    , discontinuity_trial = unique(neg_accel_trial) | unique(neg_velo_trial)
  ) %>% print(n=100) -> df_disccontinuity

# create plotting function
plot_discontinuities = function(ids_use, dev, y_label) {
  df_long_norm$tempy = dplyr::pull(df_long_norm, dev)
  rang = range(df_long_norm$tempy)
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  dis_dat = df_disccontinuity[(as.numeric(df_disccontinuity$id) %in% ids_use),]
  
  df_long_norm %>%
    dplyr::filter(coordinate == "y_inter", as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=tempy, group=trial, color=trial), alpha = 0.5, size = .2)+
    geom_point(data = dis_dat, aes(x=velo_time_point, y=top, group=trial, color=trial), alpha = 0.5, size = 0.5)+
    geom_point(data = dis_dat, aes(x=accel_time_point, y=bottom, group=trial, color=trial), alpha = 0.5, size = 0.5)+
    xlab("normalized time")+
    ylab(y_label)+
    facet_grid(id~condition) %>% print()
}

# plot velocities
plot_discontinuities(1:10, "norm_velocity", "y velocity (mm/s)")
plot_discontinuities(11:20, "norm_velocity", "y velocity (mm/s)")
plot_discontinuities(21:33, "norm_velocity", "y velocity (mm/s)")
# plot individual
plot_discontinuities(2, "norm_velocity", "y velocity (mm/s)")

# plot accelerations
plot_discontinuities(1:10, "norm_acceleration", "y acceleration (mm/s^2)")
plot_discontinuities(11:20, "norm_acceleration", "y acceleration (mm/s^2)")
plot_discontinuities(21:33, "norm_acceleration", "y acceleration (mm/s^2)")
# plot individual
plot_discontinuities(2, "norm_acceleration", "y acceleration (mm/s^2)")



# RM-ANOVA on Discontinuities ---- 

# identify trials with at least one discontinuity 
df_long_norm %>%
  dplyr::filter(coordinate == "y_inter") %>%
  dplyr::group_by(pilot, id, blocking, condition, trial) %>%
  dplyr::summarize(
    discontinuity_trial = unique(neg_accel_trial) | unique(neg_velo_trial)
  ) %>%
  dplyr::group_by(pilot, id, blocking, condition) %>%
  dplyr::summarise(
    count = sum(discontinuity_trial)
  ) -> df_disccontinuity_anova

# # are there too few discontinuities if block is added as factor?
# df_long_norm %>%
#   dplyr::filter(coordinate == "y_inter") %>%
#   dplyr::group_by(pilot, id, blocking, condition, trial) %>%
#   dplyr::summarize(
#     discontinuity_trial = unique(neg_accel_trial) | unique(neg_velo_trial)
#   ) %>%
#   dplyr::group_by(pilot, id, blocking, condition) %>%
#   dplyr::summarise(
#     count = sum(discontinuity_trial)
#   ) -> df_disccontinuity_blocks
# 
# df_disccontinuity_blocks %>%
#   dplyr::group_by(pilot, blocking, condition) %>%
#   dplyr::summarize(
#     discontinuity_trial = mean(count)
#   )
# # NOTE: I do not see why I would not add blocking as factor... 
 
df_disccontinuity_anova$condition = revalue(df_disccontinuity_anova$condition, c("no_vision"="NV", "vision" = "V"))
df_disccontinuity_anova$blocking = revalue(df_disccontinuity_anova$blocking, c("no_vision"="NV", "vision" = "V"))

# run anova
ezANOVA(
  df_disccontinuity_anova
  , dv = count
  , wid = id
  , within = .(condition)
  # , between = .(blocking)
)
ezPlot(
  df_disccontinuity_anova
  , dv = count
  , wid = id
  , within = .c(condition)
  # , between = .c(blocking)
  # , split = blocking
  , x = condition
  , do_bar = T
  , y_lab = "Discontinuities (count)"
  , x_lab = "Vision Condition"
  # , split_lab = "Blocking"
)



########################################################
####               Spline Method                    ####
########################################################

num_samples = 200

# # NOTE: this normalizes to space 
# # NOTE: not always cut up into 200 samples, max was 211
# df_spline = ddply(
#   .data = df_long_trim
#   , .variables = .(pilot, id, condition, trial)
#   , .fun = function(x){
# 
#     dfy = x[x$coordinate == "y_inter",]
#     dfx = x[x$coordinate == "x_inter",]
#     dfz = x[x$coordinate == "z_inter",]
# 
#     tempx = smooth.spline(dfx$zero_time, dfx$centered_position)  # 4th order (i.e. cubic)
#     tempy = smooth.spline(dfy$zero_time, dfy$centered_position)  # 4th order (i.e. cubic)
#     tempz = smooth.spline(dfz$zero_time, dfz$centered_position)  # 4th order (i.e. cubic)
# 
#     # normalize to Y
#     pred_over = predict(tempy, seq(min(tempy$x), max(tempy$x), length.out = 3500))
# 
#     the_bin_width = max(pred_over$y)/(num_samples-1)
# 
#     pred_round = round0(pred_over$y, the_bin_width)
# 
#     idx = 1:length(pred_round)
# 
#     idx_uni = idx[!duplicated(pred_round)]
# 
#     # actually get values
#     predx = predict(tempx, pred_over$x[c(idx_uni)])
#     predy = predict(tempy, pred_over$x[c(idx_uni)])
#     predz = predict(tempz, pred_over$x[c(idx_uni)])
# 
#     predx_df = data.frame(time = predx$x, value = predx$y, coordinate = "x", norm_idx = 1:length(predx$y))
#     predy_df = data.frame(time = predy$x, value = predy$y, coordinate = "y", norm_idx = 1:length(predy$y))
#     predz_df = data.frame(time = predz$x, value = predz$y, coordinate = "z", norm_idx = 1:length(predz$y))
# 
#     return(rbind(predx_df, predy_df, predz_df))
#   }
# )


# NOTE: this normalizes to time
df_spline = ddply(
  .data = df_long_trim
  , .variables = .(pilot, id, condition, trial, coordinate)
  , .fun = function(x){
    
    temp = smooth.spline(x$zero_time, x$centered_position)  # 4th order (i.e. cubic)
    
    the_bin_width = max(temp$x)/(num_samples-1)
    
    # ms by ms resolution
    time_over = 1:max(temp$x)
    
    time_round = unique(round0(time_over, the_bin_width))
    
    # actually get values
    pred = predict(temp, time_round)
    
    return(data.frame(time = pred$x, value = pred$y, norm_idx = 1:length(pred$x)))
  }
)

# average over trials
df_spline %>%
  dplyr::group_by(pilot, id, condition, coordinate, norm_idx) %>%
  dplyr::summarize(
    value = mean(value)
  ) -> df_spline_avg

# average over participants
df_spline_avg %>%
  dplyr::group_by(condition, coordinate, norm_idx) %>%
  dplyr::summarize(
    value = mean(value)
  ) -> df_spline_grand_avg

# function
plot_spline = function(coor, ylabel) {
  df_spline_grand_avg %>%
    dplyr::filter(coordinate == coor) %>%
    ggplot()+
    geom_line(aes(x=norm_idx, y=value, group=condition, color=condition))+
    xlab("normalized time index")+
    ylab(ylabel)
}

# plot
plot_spline("x_inter", "x position (mm)")
plot_spline("y_inter", "y position (mm)")
plot_spline("z_inter", "z position (mm)")


# take difference 
df_spline_avg %>%
  dplyr::group_by(pilot, id, coordinate, norm_idx) %>%
  dplyr::summarize(
    effect = diff(value)
  ) -> df_spline_avg_effect

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
plot_spline_effects = function(coor, ylabel){
  
df_spline_grand_avg_effect %>%
  dplyr::filter(coordinate == coor) %>%
  ggplot()+
  geom_line(aes(x=norm_idx, y=M), color = "purple")+
  geom_ribbon(aes(x=norm_idx, ymin=CI_lo, ymax=CI_hi), fill = "purple", alpha=0.5)+
  geom_area(aes(x=norm_idx, y=as.numeric(sig)*max(CI_hi)), alpha = 0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab("normalized time index")+
  ylab(ylabel)
}

# plot
plot_spline_effects("x_inter", "x position effect (vision - no_vision; mm)")
plot_spline_effects("y_inter", "y position effect (vision - no_vision; mm)")
plot_spline_effects("z_inter", "z position effect (vision - no_vision; mm)")



# # Plot 2D Waveforms ----
# # trial-wise
# df_spline %>%
#   dplyr::select(-c(pilot, norm_idx, time)) %>%
#   spread(coordinate, value) -> df_2D
# 
# plot_2D = function(ids_use) {
#   df_2D %>%
#     dplyr::filter(as.numeric(id) %in% ids_use) %>%
#     ggplot(aes(x=x_inter, y=y_inter, group=trial, color=trial))+
#     geom_path()+
#     facet_grid(condition~id)+
#     xlab("x position")+
#     ylab("normalized y position")
# }
# 
# plot_2D(1:10)
# plot_2D(11:20)
# plot_2D(21:33)
# 
# # id averages
# df_spline_avg %>%
#   spread(coordinate, value) -> df_2D_avg
# 
# # overhead view
# df_2D_avg %>%
#   ggplot(aes(x=x_inter, y=y_inter, group=id, color=id))+
#   geom_path()+
#   facet_grid(.~condition)+
#   xlab("x position")+
#   ylab("normalized y position")
# 
# # side view
# df_2D_avg %>%
#   ggplot(aes(x=y_inter, y=z_inter, group=id, color=id))+
#   geom_path()+
#   facet_grid(.~condition)+
#   xlab("normalized y position")+
#   ylab("z position")
# 
# # group averages
# df_spline_avg %>%
#   dplyr::group_by(condition, coordinate, norm_x) %>%
#   dplyr::summarize(
#     value = mean(value)
#   ) -> df_spline_avg_group
# 
# df_spline_avg_group %>%
#   spread(coordinate, value) -> df_2D_avg_group
# 
# # overhead view
# df_2D_avg_group %>%
#   ggplot(aes(x=x_inter, y=y_inter, group=condition, color=condition))+
#   geom_path()+
#   xlab("x position")+
#   ylab("normalized y position")
# 
# # side view
# df_2D_avg_group %>%
#   ggplot(aes(x=y_inter, y=z_inter, group=condition, color=condition))+
#   geom_path()+
#   xlab("normalized y position")+
#   ylab("z position")



# For MATLAB ----
df_long_trim %>% 
  dplyr::select(pilot, id, blocking, condition, trial, coordinate, zero_time, centered_position) %>%
  spread(coordinate, centered_position) -> df_for_matlab2

df_for_matlab2 %>%
  dplyr::group_by() %>%
  dplyr::mutate(
    id = as.numeric(id)
    , blocking = as.numeric(blocking)  # 1: vision
    , condition = as.numeric(condition)  # 1: no-vision
  ) -> df_for_matlab

# convert to matrix
mat_long_trim = as.matrix(df_for_matlab[,c("id", "condition", "trial", "y_inter", "x_inter", "z_inter")])

# # save matrix as .mat file 
# write.csv(mat_long_trim, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/FDA/mat_long_trim.csv")


# From MATLAB ----
display_fanovan = function(filename, norm_y = T) {
  # p-values
  px = read.csv(sprintf('%s/px.csv', filename), header = F)
  names(px) = c("p_value")
  py = read.csv(sprintf('%s/py.csv', filename), header = F)
  names(py) = c("p_value")
  pz = read.csv(sprintf('%s/pz.csv', filename), header = F)
  names(pz) = c("p_value")
  
  # x
  x_nv = read.csv(sprintf('%s/x_nv.csv', filename), header = F)
  names(x_nv) = c("x")
  x_nv$condition = "no_vision"
  x_v = read.csv(sprintf('%s/x_v.csv', filename), header = F)
  names(x_v) = c("x")
  x_v$condition = "vision"
  
  # y
  y_nv = read.csv(sprintf('%s/y_nv.csv', filename), header = F)
  names(y_nv) = c("y")
  y_nv$condition = "no_vision"
  y_v = read.csv(sprintf('%s/y_v.csv', filename), header = F)
  names(y_v) = c("y")
  y_v$condition = "vision"
  
  # z
  z_nv = read.csv(sprintf('%s/z_nv.csv', filename), header = F)
  names(z_nv) = c("z")
  z_nv$condition = "no_vision"
  z_v = read.csv(sprintf('%s/z_v.csv', filename), header = F)
  names(z_v) = c("z")
  z_v$condition = "vision"
  
  # MSEs
  x_MSE = read.csv(sprintf('%s/msx.csv', filename), header = F)
  names(x_MSE) = c("mse")
  y_MSE = read.csv(sprintf('%s/msy.csv', filename), header = F)
  names(y_MSE) = c("mse")
  z_MSE = read.csv(sprintf('%s/msz.csv', filename), header = F)
  names(z_MSE) = c("mse")
  
  
  # create data frames 
  nv = cbind(z = z_nv$z, y = y_nv$y, x_nv)
  v = cbind(z = z_v$z, y = y_v$y, x_v)
  
  fanovan =  rbind(nv, v)
  
  fanovan$time = 0:199/199
  
  # when is it significant?
  px$sig = px$p_value < 0.05
  py$sig = py$p_value < 0.05
  pz$sig = pz$p_value < 0.05
  
  # add to fanovan
  fanovan$px_sig = px$sig
  fanovan$py_sig = py$sig
  fanovan$pz_sig = pz$sig
  
  # how significant is it?
  fanovan$px_val = px$p_value
  fanovan$py_val = py$p_value
  fanovan$pz_val = pz$p_value
  
  # MSEs
  fanovan$x_SE = sqrt(x_MSE$mse/n)
  fanovan$y_SE = sqrt(y_MSE$mse/n)
  fanovan$z_SE = sqrt(z_MSE$mse/n)
  
  # t-value 
  tval = qt(1-0.025, n-1) 
  
  # change names and order of levels
  fanovan$condition = revalue(fanovan$condition, c("no_vision"="NV", "vision" = "V"))
  fanovan$condition = factor(fanovan$condition, c("V", "NV"))
  
  if (norm_y){
    # overhead view 
    fanovan %>%
      ggplot()+
      geom_path(aes(x=y, y=x, group=condition, color=condition))+
      geom_area(data = subset(fanovan, condition == "vision"), aes(x=y, y=as.numeric(px_sig)*max(x+x_SE*tval)), alpha = 0.2)+
      geom_ribbon(aes(x=y, ymin=x-x_SE*tval, ymax=x+x_SE*tval, group=condition, fill=condition), alpha = 0.2)+
      xlab("normalized y position")+
      ylab("x position") +
      coord_flip() -> gg
    print(gg)
    
    # side view
    fanovan %>%
      ggplot()+
      geom_path(aes(x=y, y=z, group=condition, color=condition))+
      geom_area(data = subset(fanovan, condition == "vision"), aes(x=y, y=as.numeric(pz_sig)*max(z+z_SE*tval)), alpha = 0.2)+
      geom_ribbon(aes(x=y, ymin=z-z_SE*tval, ymax=z+z_SE*tval, group=condition, fill=condition), alpha = 0.2)+
      xlab("normalized y position")+
      ylab("z position") -> gg
    print(gg)
    
  } else {
    
    # x vs. time
    fanovan %>%
      ggplot()+
      geom_path(aes(x=time, y=x, group=condition, color=condition))+
      geom_area(data = subset(fanovan, condition == "V"), aes(x=time, y=as.numeric(px_sig)*max(x+x_SE*tval)), alpha = 0.2)+
      geom_ribbon(aes(x=time, ymin=x-x_SE*tval, ymax=x+x_SE*tval, group=condition, fill=condition), alpha = 0.2)+
      labs(x = "Proportion of Movement", y = "X Position (mm)", color = "Vision Condition", fill = "Vision Condition") -> gg
    print(gg)
    
    # y vs. time
    fanovan %>%
      ggplot()+
      geom_path(aes(x=time, y=y, group=condition, color=condition))+
      geom_area(data = subset(fanovan, condition == "V"), aes(x=time, y=as.numeric(py_sig)*max(y+y_SE*tval)), alpha = 0.2)+
      geom_ribbon(aes(x=time, ymin=y-y_SE*tval, ymax=y+y_SE*tval, group=condition, fill=condition), alpha = 0.2)+
      labs(x = "Proportion of Movement", y = "Y Position (mm)", color = "Vision Condition", fill = "Vision Condition") -> gg
    print(gg)
    
    # z vs. time
    fanovan %>%
      ggplot()+
      geom_path(aes(x=time, y=z, group=condition, color=condition))+
      geom_area(data = subset(fanovan, condition == "NV"), aes(x=time, y=as.numeric(pz_sig)*max(z+z_SE*tval)), alpha = 0.2)+
      geom_ribbon(aes(x=time, ymin=z-z_SE*tval, ymax=z+z_SE*tval, group=condition, fill=condition), alpha = 0.2)+
      labs(x = "Proportion of Movement", y = "Z Position (mm)", color = "Vision Condition", fill = "Vision Condition") -> gg
    print(gg)
    
  }
  
  return(fanovan)
}

# NOTE: figure out interpretation of LSDs

# fanovan_norm_y = display_fanovan('/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_results/fanovan_norm_y')
fanovan_norm_time = display_fanovan('/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_results/fanovan_norm_time', norm_y = F)

# compare my functional anova analysis with fanovan
df_spline_grand_avg_effectx = df_spline_grand_avg_effect[df_spline_grand_avg_effect$coordinate == "x_inter",]
df_spline_grand_avg_effecty = df_spline_grand_avg_effect[df_spline_grand_avg_effect$coordinate == "y_inter",]
df_spline_grand_avg_effectz = df_spline_grand_avg_effect[df_spline_grand_avg_effect$coordinate == "z_inter",]

# the analyses produce virtually identical results
plot(df_spline_grand_avg_effectx$sig - fanovan_norm_time[1:200,]$px_sig, ylab = "x significance match")
plot(df_spline_grand_avg_effecty$sig - fanovan_norm_time[1:200,]$py_sig, ylab = "y significance match")
plot(df_spline_grand_avg_effectz$sig - fanovan_norm_time[1:200,]$pz_sig, ylab = "z significance match")

# the analyses produce virtually identical results
plot(df_spline_grand_avg_effectx$pval, ylab = "x p-value", ty = "l", col = "red")
lines(fanovan_norm_time[1:200,]$px_val, col = "blue")

plot(df_spline_grand_avg_effecty$pval, ylab = "y p-value", ty = "l", col = "red")
lines(fanovan_norm_time[1:200,]$py_val, col = "blue")

plot(df_spline_grand_avg_effectz$pval, ylab = "z p-value", ty = "l", col = "red")
lines(fanovan_norm_time[1:200,]$pz_val, col = "blue")


# One-way ezANOVA ----
oneway_test = function(coor, idx) {
  df_spline_avg %>%
    dplyr::filter(coordinate == coor, norm_idx == idx) -> df_spline_avg_test
  
  ezANOVA(
    data = df_spline_avg_test
    , dv = value
    , wid = id
    , within = .(condition)
  ) %>% print()
  ezPlot(
    data = df_spline_avg_test
    , dv = value
    , wid = id
    , within = .(condition)
    , x = condition
    , do_bar = T
    , y_lab = "Position (mm)"
    , x_lab = "Vision Condition"
  )
}

# X coordinate threshold
oneway_test("x_inter", 7)
oneway_test("x_inter", 8)
oneway_test("x_inter", 9)

# Y coordinate threshold
oneway_test("y_inter", 12)
oneway_test("y_inter", 13)
oneway_test("y_inter", 14)
oneway_test("y_inter", 15)

# Z coordinate threshold
oneway_test("z_inter", 79)
oneway_test("z_inter", 80)

# NOTE: The FLSD from ezPlot allign with the pairwise t-tests and the fanovan error bars


# One-way Manual rANOVA ----
df_spline_avg %>%
  dplyr::filter(coordinate == "z_inter", norm_idx == 79) -> df_spline_avg_test

aov_obj = aov(value ~ condition + Error(id/condition), data = df_spline_avg_test)
aov_sum = summary(aov_obj)
aov_sum_w = aov_sum$`Error: id:condition`
aov_unlist = unlist(aov_sum_w)
MSE = aov_unlist["Mean Sq2"][[1]]
DF = aov_unlist["Df2"][[1]]
NN = DF + 1
SE = sqrt(MSE/NN)
TT = qt(1-0.025, DF)
FLSD = SE * TT  * sqrt(2) 

df_spline_avg_test %>%
  dplyr::group_by(condition) %>%
  dplyr::summarise(mvalue = mean(value)) -> df_spline_avg_test_graph

df_spline_avg_test_graph$FLSD = FLSD

df_spline_avg_test_graph %>%
  ggplot()+
  geom_point(aes(x=condition, y=mvalue))+
  geom_errorbar(aes(x=condition, ymin = mvalue - FLSD/2, ymax = mvalue + FLSD/2), width = 0.25)+
  labs(x = "Vision Condition", y = "Position (mm)") 


# USE FANOVAN ----
fanovan_test = fanovan_norm_time[c(79, 279),]

# NOTE: FLSD matches if not devided by two and not multiplied by sqrt(2)!
zFLSD = fanovan_test$z_SE * TT

fanovan_test$zFLSD = zFLSD

fanovan_test$condition = factor(fanovan_test$condition, c("NV", "V"))

fanovan_test %>%
  ggplot()+
  geom_point(aes(x=condition, y=z))+
  geom_errorbar(aes(x=condition, ymin = z - zFLSD, ymax = z + zFLSD), width = 0.25)+
  labs(x = "Vision Condition", y = "Position (mm)") 





########################################################
####        Gaussian Process Regression             ####
########################################################

# Binning Waveforms ----

# select only a single coordinate for a given analysis
df_long_trim %>%
  dplyr::filter(coordinate == "y_inter") -> df_long_trimy

# specify bin width
bin_width = 1/14  # simulation is 1/15 (so 16 time points)

# normalize time
df_long_trimy %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    time_lores = round0((zero_time-min(zero_time))/(max(zero_time)-min(zero_time)), bin_width)
  ) -> df_long_trimy

df_long_trimy %>%
  dplyr::group_by(blocking, pilot, id, condition, trial, coordinate, time_lores) %>%
  dplyr::summarize(
    position_bin = mean(centered_position) 
    , velocity_bin = mean(velocity)
    , acceleration_bin = mean(acceleration)
  ) -> df_long_biny

# for later plotting
df_long_biny$position_bin_scale = scale(df_long_biny$position_bin)[,1]

# create function to plot binned data
plot_bin = function(ids_use, y_label) {
  df_long_biny %>%
    dplyr::filter(as.numeric(id) %in% ids_use) %>%
      ggplot()+
      geom_line(aes(x=time_lores, y=position_bin, color=trial, group=trial), alpha = 0.2)+
      facet_grid(id~condition)+
      ylab(y_label)+
      xlab("normalized time") %>% print()
}

plot_bin(1:10, "y binned position (mm)")
plot_bin(11:21, "y binned position (mm)")
plot_bin(21:31, "y binned position (mm)")

# averave over trials
df_long_biny %>%
  group_by(blocking, pilot, id, condition, coordinate, time_lores) %>%
  dplyr::summarise(
    position_bin_avg = mean(position_bin)
    , position_bin_scale_avg = mean(position_bin_scale)
    ) -> df_id_meansy

# plot participant mean trajectories
df_id_meansy %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin_avg, color = id), alpha = 0.5)+
  facet_grid(.~condition)+
  xlab('normalized time')+
  ylab('y binned position (mm)')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = 'black')
  )

# average over participants
df_id_meansy %>%
  group_by(condition, coordinate, time_lores) %>%
  dplyr::summarise(
    position_bin_grand_avg = mean(position_bin_avg)
    , position_bin_scale_grand_avg = mean(position_bin_scale_avg)
    ) -> df_condition_meansy

# plot population mean trajectories
df_condition_meansy %>%
  ggplot()+
  geom_line(aes(x=time_lores, y=position_bin_grand_avg, color = condition))+
  xlab('normalized time')+
  ylab('y binned position (mm)')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = 'black')
  )


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
x = sort(unique(df_long_biny$time_lores))

# for each value in df_long_biny$time_lores, get its index x
x_index = match(df_long_biny$time_lores,x)

# do something similar for the subjects 
s = sort(unique(df_long_biny$id))
s_index = match(df_long_biny$id, s)

# compute the model matrix
z = data.frame(
  condition1 = ifelse(df_long_biny$condition == "vision", 1, 0)
  , condition2 =ifelse(df_long_biny$condition == "no_vision", 1, 0)
)

# # compute the model matrix
# z = data.frame(
#   condition1 = 1  # intercept
#   , condition2 = ifelse(df_long_biny$condition == "vision", 1/2, -1/2)
# )

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

# if real data
y = scale(df_long_biny$position_bin)[,1]  # scaled to mean=0,sd=1

# get data specific to each participant
y_by_s = array(split(y, s_index))

# pad that data
y_by_s_pad = array(0,dim=c(n,10000))
for (si in 1:length(subj_obs)) {
  y_by_s_pad[si,] = c(y_by_s[[si]], rep(0, 10000 - subj_obs[si]))
}

# create the data list for stan
data_for_stan = list(
  n_y = nrow(df_long_biny)
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
# save(data_for_stan, file = "/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_data/data_for_stan_11.RData")

# # see 'cluster_analysis'
# mod = rstan::stan_model("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_study/gp_regression.stan")
# post_test = sampling(
#   mod
#   , data = data_for_stan
#   , iter = 50
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
load("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory\ Studies/MSc_results/post_1000_15_mtd17.rdata")



#### Overview

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
f_print = data.frame(f_print)
hist(f_print$Rhat)
range(f_print$Rhat)
hist(f_print$n_eff)
range(f_print$n_eff)

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
    df$condition = revalue(df$condition, c("effect" = "V - NV"))
    x_lab = "Vision Condition Effect"
  }
  else  {
    df$condition = revalue(df$condition, c("no_vision" = "NV", "vision" = "V"))
    df$condition = factor(df$condition, c("NV", "V"))
    x_lab = "Vision Condition"
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

  gg = gg + 
    theme_gray(base_size = 20)+
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
names(volatilities) = c("vision", "no_vision")

volatilities$effect = volatilities$vision - volatilities$no_vision

volatilities %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, vision:no_vision) -> volatilities_c

gg_volatilities = get_violin(volatilities_c, "Volatility")

volatilities %>%
  dplyr::select(-c(vision, no_vision)) %>%
  gather(condition, value, effect) -> volatilities_e

gg_volatilities_effect = get_violin(volatilities_e, "Volatility", iseffect = T, hline = T)


# population mean amplitude
amplitudes = data.frame(post_samples$amplitude)
names(amplitudes) = c("vision", "no_vision")

amplitudes$effect = amplitudes$vision - amplitudes$no_vision

amplitudes %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, vision:no_vision) -> amplitudes_c

gg_amplitudes = get_violin(amplitudes_c, "Amplitude")

amplitudes %>%
  dplyr::select(-c(vision, no_vision)) %>%
  gather(condition, value, effect) -> amplitudes_e

gg_amplitudes_effect = get_violin(amplitudes_e, "Amplitude", iseffect = T, hline = T)


# participant mean amplitude sd
subj_amplitude_sds = data.frame(post_samples$subj_amplitude_sd)
names(subj_amplitude_sds) = c("vision", "no_vision")

subj_amplitude_sds$effect = subj_amplitude_sds$vision - subj_amplitude_sds$no_vision

subj_amplitude_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, vision:no_vision) -> subj_amplitude_sds_c

gg_amplitude_sds = get_violin(subj_amplitude_sds_c, "Amplitude Variability (SD)")

subj_amplitude_sds %>%
  dplyr::select(-c(vision, no_vision)) %>%
  gather(condition, value, effect) -> subj_amplitude_sds_e

gg_amplitude_sds_effect = get_violin(subj_amplitude_sds_e, "Amplitude Variability (SD)", iseffect = T, hline = T)


# participant mean volatilty sd
subj_volatility_sds = data.frame(post_samples$subj_volatility_sd)
names(subj_volatility_sds) = c("vision", "no_vision")

subj_volatility_sds$effect = subj_volatility_sds$vision - subj_volatility_sds$no_vision

subj_volatility_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, vision:no_vision) -> subj_volatility_sds_c

gg_volatility_sds = get_violin(subj_volatility_sds_c, "Volatility Variability (SD)")

subj_volatility_sds %>%
  dplyr::select(-c(vision:no_vision)) %>%
  gather(condition, value, effect) -> subj_volatility_sds_e

gg_volatility_sds_effect = get_violin(subj_volatility_sds_e, "Volatility Variability (SD)", iseffect = T, hline = T)



# population noise volatilty
noise_volatilities = data.frame(post_samples$noise_volatility)
names(noise_volatilities) = c("vision", "no_vision")

noise_volatilities$effect = noise_volatilities$vision - noise_volatilities$no_vision

noise_volatilities %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, vision:no_vision) -> noise_volatilities_c

gg_noise_volatilities = get_violin(noise_volatilities_c, "Noise Volatility")

noise_volatilities %>%
  dplyr::select(-c(vision:no_vision)) %>%
  gather(condition, value, effect) -> noise_volatilities_e

gg_noise_volatilities_effect = get_violin(noise_volatilities_e, "Noise Volatility", iseffect = T, hline = T)



# population noise amplitude
noise_amplitudes = data.frame(post_samples$noise_amplitude)
names(noise_amplitudes) = c("vision", "no_vision")

noise_amplitudes$effect = noise_amplitudes$vision - noise_amplitudes$no_vision

noise_amplitudes %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, vision:no_vision) -> noise_amplitudes_c

gg_noise_amplitudes = get_violin(noise_amplitudes_c, "Noise Amplitude")

noise_amplitudes %>%
  dplyr::select(-c(vision:no_vision)) %>%
  gather(condition, value, effect) -> noise_amplitudes_e

gg_noise_amplitudes_effect = get_violin(noise_amplitudes_e, "Noise Amplitude", iseffect = T, hline = T)



# participant noise volatilty sd
noise_subj_volatility_sds = data.frame(post_samples$noise_subj_volatility_sd)
names(noise_subj_volatility_sds) = c("vision", "no_vision")

noise_subj_volatility_sds$effect = noise_subj_volatility_sds$vision - noise_subj_volatility_sds$no_vision

noise_subj_volatility_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, vision:no_vision) -> noise_subj_volatility_sds_c

gg_noise_volatility_sds = get_violin(noise_subj_volatility_sds_c, "Noise Volatility Variability (SD)")

noise_subj_volatility_sds %>%
  dplyr::select(-c(vision:no_vision)) %>%
  gather(condition, value, effect) -> noise_subj_volatility_sds_e

gg_noise_volatility_sds_effect = get_violin(noise_subj_volatility_sds_e, "Noise Volatility Variability (SD)", iseffect = T, hline = T)



# participant noise amplitude sd
noise_subj_amplitude_sds = data.frame(post_samples$noise_subj_amplitude_sd)
names(noise_subj_amplitude_sds) = c("vision", "no_vision")

noise_subj_amplitude_sds$effect = noise_subj_amplitude_sds$vision - noise_subj_amplitude_sds$no_vision

noise_subj_amplitude_sds %>%
  dplyr::select(-c(effect)) %>%
  gather(condition, value, vision:no_vision) -> noise_subj_amplitude_sds_c

gg_noise_amplitude_sds = get_violin(noise_subj_amplitude_sds_c, "Noise Amplitude Variability (SD)")

noise_subj_amplitude_sds %>%
  dplyr::select(-c(vision:no_vision)) %>%
  gather(condition, value, effect) -> noise_subj_amplitude_sds_e

gg_noise_amplitude_sds_effect = get_violin(noise_subj_amplitude_sds_e, "Noise Amplitude Variability (SD)", iseffect = T, hline = T)





####   Subject Level

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

subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1, color = factor(id)))+
  # geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = factor(id), alpha=0.2)+
  # geom_line(aes(x=time, y=hi95_1, color = factor(id)), linetype = "dashed")+
  # geom_line(aes(x=time, y=lo95_1, color = factor(id)), linetype = "dashed")+
  # geom_line(data = subset(df_id_meansy, condition == "vision"), aes(x=time_lores, y=position_bin_scale_avg, group = id), size = 0.5, color = "gray50")+
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+ 
  ggtitle('Vision')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  ) ->p1
print(p1)


# # figure out participant details
# p1cols = ggplot_build(p1)$data[[1]]
# 
# # get range
# lo=0.5
# hi=0.7
# 
# # just pick one subject
# subj_to_plot %>%
#   group_by(id) %>%
#   dplyr::filter(time > lo, time < hi) %>%
#   dplyr::summarise(mins = min(med_1)) %>%
#   dplyr::summarise(idx = which.min(mins)) -> p1_id2
# p1_id = p1_id2[[1]]
# 
# subj_to_plot %>%
#   dplyr::filter(id == p1_id, time >=lo, time <=hi) -> subj_to_plot_id
# 
# p1cols %>%
#   dplyr::filter(group ==p1_id, x == 1) -> p1_id_col2
# p1_id_col = p1_id_col2$colour
# 
# # get id for df_id_meansy
# # NOTE: stan id and df id will not necessarily match (some participant numbers removed)
# id_list = as.numeric(sort(unique(df_id_meansy$id)))
# df_id = id_list[p1_id]
# 
# # plot
# subj_to_plot_id %>%
#   ggplot()+
#   geom_line(aes(x=time, y=med_1), color = p1_id_col, size = 2)+
#   geom_line(aes(x=time, y=hi95_1), color = p1_id_col, linetype = "dashed", size = 2)+
#   geom_line(aes(x=time, y=lo95_1), color = p1_id_col, linetype = "dashed", size = 2)+
#   geom_line(data = subset(df_id_meansy, condition == "vision" & id == df_id & time_lores >=lo & time_lores <=hi), aes(x=time_lores, y=position_bin_scale_avg, group = id), size = 2, color = "gray50")+
#   ylab('')+
#   xlab('')+
#   theme_gray(base_size = 20)+
#   theme(
#     panel.grid.major = element_line(size = 0)
#     , panel.grid.minor = element_line(size = 0)
#     , axis.ticks = element_line(size = 0)
#     , axis.text = element_blank()
#     , legend.position = "none"
#     , panel.border = element_rect(size = 2, fill = NA)
#     , panel.background = element_rect(fill = "white", color = "black")
#   )



# condition 2
subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_2, color = factor(id)))+
  # geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = factor(id), alpha=0.2)+
  # geom_line(aes(x=time, y=hi95_2, color = factor(id)), linetype = "dashed")+
  # geom_line(aes(x=time, y=lo95_2, color = factor(id)), linetype = "dashed")+
  # geom_line(data = subset(df_id_meansy, condition == "no_vision"), aes(x=time_lores, y=position_bin_scale_avg, group = id ),size = 0.5, color = "gray50")+
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+ 
  ggtitle('No Vision')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  ) -> p2
print(p2)


# # figure out participant details
# p2cols = ggplot_build(p2)$data[[1]]
# 
# # get range
# lo2=0.3
# hi2=0.5
# 
# # just pick one subject
# subj_to_plot %>%
#   group_by(id) %>%
#   dplyr::filter(time > lo2, time < hi2) %>%
#   dplyr::summarise(maxs = max(med_2)) %>%
#   dplyr::summarise(idx = which.max(maxs)) -> p2_id2
# p2_id = p2_id2[[1]]
# 
# subj_to_plot %>%
#   dplyr::filter(id == p2_id, time >=lo2, time <=hi2) -> subj_to_plot_id2
# 
# p2cols %>%
#   dplyr::filter(group ==p2_id, x == 1) -> p2_id_col2
# p2_id_col = p2_id_col2$colour
# 
# # get id for df_id_meansy
# # NOTE: stan id and df id will not necessarily match (some participant numbers removed)
# df_id2 = id_list[p2_id]
# 
# subj_to_plot_id2 %>%
#   ggplot()+
#   geom_line(aes(x=time, y=med_2), color = p2_id_col, size = 2)+
#   geom_line(aes(x=time, y=hi95_2), color = p2_id_col, linetype = "dashed", size = 2)+
#   geom_line(aes(x=time, y=lo95_2), color = p2_id_col, linetype = "dashed", size = 2)+
#   geom_line(data = subset(df_id_meansy, condition == "no_vision" & id == df_id2 & time_lores >=lo2 & time_lores <=hi2), aes(x=time_lores, y=position_bin_scale_avg, group = id), size = 2, color = "gray50")+
#   ylab('')+
#   xlab('')+
#   theme_gray(base_size = 20)+
#   theme(
#     panel.grid.major = element_line(size = 0)
#     , panel.grid.minor = element_line(size = 0)
#     , axis.ticks = element_line(size = 0)
#     , axis.text = element_blank()
#     , legend.position = "none"
#     , panel.border = element_rect(size = 2, fill = NA)
#     , panel.background = element_rect(fill = "white", color = "black")
#   )

# need to order participant IDs from 1 to 29 for df_id_meansy
df_id_meansy$orig_id = df_id_meansy$id
df_id_meansy$id = factor(as.numeric(droplevels(df_id_meansy$id)))

by_subj = function(id_lo, id_hi) {
  subj_to_plot %>%
    dplyr::filter(id <= id_hi, id >= id_lo) %>%
    ggplot()+
    geom_line(aes(x=time, y=med_1, group = factor(id)), color = "turquoise")+
    geom_line(aes(x=time, y=hi95_1, group = factor(id)), linetype = "dashed", color = "turquoise")+
    geom_line(aes(x=time, y=lo95_1, group = factor(id)), linetype = "dashed", color = "turquoise")+
    geom_line(data = subset(df_id_meansy, condition == "vision" & as.numeric(id) <= id_hi & as.numeric(id) >= id_lo), aes(x=time_lores, y=position_bin_scale_avg, group = id ),size = 0.5, color = "gray50")+
    geom_line(aes(x=time, y=med_2, group = factor(id)), color = "red")+
    geom_line(aes(x=time, y=hi95_2, group = factor(id)), linetype = "dashed", color = "red")+
    geom_line(aes(x=time, y=lo95_2, group = factor(id)), linetype = "dashed", color = "red")+
    geom_line(data = subset(df_id_meansy, condition == "no_vision" & as.numeric(id) <= id_hi & as.numeric(id) >= id_lo), aes(x=time_lores, y=position_bin_scale_avg, group = id ),size = 0.5, color = "gray50")+
    ylab('Scaled Position')+
    xlab('Proportion of Movement')+ 
    facet_wrap(~factor(id), ncol = 2)+
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
df_long_biny %>%
  group_by(id, time_lores, condition) %>%
  dplyr::summarise(
    SD = log(sd(position_bin_scale))
  ) -> noise_df_long_biny

noise_subj_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1, color = factor(id)))+
  # geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = factor(id), alpha=0.2)+
  # geom_line(aes(x=time, y=hi95_1, color = factor(id)), linetype = "dashed")+
  # geom_line(aes(x=time, y=lo95_1, color = factor(id)), linetype = "dashed")+
  # geom_line(data = subset(noise_df_long_biny, condition == "vision"), aes(x=time_lores, y=SD, group = id), size = 0.5, color = "gray50")+
  ylab('Log Standard Deviation')+
  xlab('Proportion of Movement')+
  ggtitle('Vision')+
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
  # geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = factor(id), alpha=0.2)+
  # geom_line(aes(x=time, y=hi95_2, color = factor(id)), linetype = "dashed")+
  # geom_line(aes(x=time, y=lo95_2, color = factor(id)), linetype = "dashed")+
  # geom_line(data = subset(noise_df_long_biny, condition == "no_vision"), aes(x=time_lores, y=SD, group = id), size = 0.5, color = "gray50")+
  ylab('Log Standard Deviation')+
  xlab('Proportion of Movement')+
  ggtitle('No Vision')+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , legend.position = "none"
    , panel.background = element_rect(fill = "white", color = "black")
  )

# need to order participant IDs from 1 to 29 for df_id_meansy
noise_df_long_biny$orig_id = noise_df_long_biny$id
noise_df_long_biny$id = factor(as.numeric(droplevels(noise_df_long_biny$id)))

noise_by_subj = function(id_lo, id_hi) {
  noise_subj_to_plot %>%
    dplyr::filter(id <= id_hi, id >= id_lo) %>%
    ggplot()+
    geom_line(aes(x=time, y=med_1, group = factor(id)), color = "turquoise")+
    geom_line(aes(x=time, y=hi95_1, group = factor(id)), linetype = "dashed", color = "turquoise")+
    geom_line(aes(x=time, y=lo95_1, group = factor(id)), linetype = "dashed", color = "turquoise")+
    geom_line(data = subset(noise_df_long_biny, condition == "vision" & as.numeric(id) <= id_hi & as.numeric(id) >= id_lo), aes(x=time_lores, y=SD, group = id ),size = 0.5, color = "gray50")+
    geom_line(aes(x=time, y=med_2, group = factor(id)), color = "red")+
    geom_line(aes(x=time, y=hi95_2, group = factor(id)), linetype = "dashed", color = "red")+
    geom_line(aes(x=time, y=lo95_2, group = factor(id)), linetype = "dashed", color = "red")+
    geom_line(data = subset(noise_df_long_biny, condition == "no_vision" & as.numeric(id) <= id_hi & as.numeric(id) >= id_lo), aes(x=time_lores, y=SD, group = id ),size = 0.5, color = "gray50")+
    ylab('Log Standard Deviation')+
    xlab('Proportion of Movement')+ 
    facet_wrap(~factor(id), ncol = 2)+
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

# get average kinematic markers for each group
kms_ids %>%
  dplyr::filter(coordinate == "y_inter") %>%
  dplyr::group_by(condition, coordinate) %>%
  dplyr::summarise(
    time_peak_velocity= mean(time_peak_velocity) 
    , time_peak_acceleration = mean(time_peak_acceleration)
    , time_peak_deceleration =  mean(time_peak_deceleration)
    , peak_velocity= mean(peak_velocity) 
    , peak_acceleration = mean(peak_acceleration)
    , peak_deceleration =  mean(peak_deceleration)
  ) -> kms_to_plot
kms_to_plot


to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data=subset(df_condition_meansy, condition == "vision"), aes(x=time_lores, y=position_bin_scale_grand_avg), size = 0.5, color = "grey50")+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_velocity, y=min(to_plot$med_1)), size = 1)+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_acceleration, y=min(to_plot$med_1)), size = 1)+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_deceleration, y=min(to_plot$med_1)), size = 1)+
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+ 
  ggtitle('Vision')+
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
  geom_line(data=subset(df_condition_meansy, condition == "no_vision"), aes(x=time_lores, y=position_bin_scale_grand_avg), size = 0.5, color = "grey50")+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_velocity, y=min(to_plot$med_1)), size = 1)+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_acceleration, y=min(to_plot$med_1)), size = 1)+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_deceleration, y=min(to_plot$med_1)), size = 1)+
  ylab('Scaled Position')+
  xlab('Proportion of Movement')+ 
  ggtitle('No Vision')+
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
  geom_line(aes(x=time, y=med_1), color = "red")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "red", alpha=0.5)+
  geom_line(aes(x=time, y=med_2), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "turquoise", alpha=0.5)+
  annotate("text", label = "Vision", x = 3/15, y = -0.15, color = "red")+
  annotate("text", label = "No Vision", x = 8/15, y = -1, color = "turquoise")+
  geom_line(data=subset(df_condition_meansy, condition == "vision"), aes(x=time_lores, y=position_bin_scale_grand_avg), size = 0.5, color = "grey50")+
  geom_line(data=subset(df_condition_meansy, condition == "no_vision"), aes(x=time_lores, y=position_bin_scale_grand_avg), size = 0.5, color = "grey50")+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_velocity, y=min(to_plot$med_1)), size = 1, color = "turquoise")+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_acceleration, y=min(to_plot$med_1)), size = 1, color = "turquoise")+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_deceleration, y=min(to_plot$med_1)), size = 1, color = "turquoise")+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_velocity, y=min(to_plot$med_1)), size = 1, color = "red")+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_acceleration, y=min(to_plot$med_1)), size = 1, color = "red")+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_deceleration, y=min(to_plot$med_1)), size = 1, color = "red")+
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

# average over peak times 
kms %>%
  dplyr::group_by(blocking, pilot, id, coordinate) %>%
  dplyr::summarise(
    time_peak_velocity = mean(time_peak_velocity)
    , time_peak_acceleration = mean(time_peak_acceleration)
    , time_peak_deceleration = mean(time_peak_deceleration)
    , peak_velocity = mean(peak_velocity)
    , peak_acceleration = mean(peak_acceleration)
    , peak_deceleration = mean(peak_deceleration)
  ) -> kms_ids_to_plot

kms_ids_to_plot %>%
  dplyr::filter(coordinate == "y_inter") %>%
  dplyr::group_by(coordinate) %>%
  dplyr::summarise(
    time_peak_velocity= mean(time_peak_velocity) 
    , time_peak_acceleration = mean(time_peak_acceleration)
    , time_peak_deceleration =  mean(time_peak_deceleration)
    , peak_velocity= mean(peak_velocity) 
    , peak_acceleration = mean(peak_acceleration)
    , peak_deceleration =  mean(peak_deceleration)
  ) -> kms_to_plot_effect
kms_to_plot_effect

# get empirical difference as well
df_condition_meansy %>%
  dplyr::group_by(time_lores) %>%
  dplyr::summarise(
    effect = position_bin_scale_grand_avg[condition == "vision"] - position_bin_scale_grand_avg[condition == "no_vision"]
      ) -> df_condition_effecty

to_plot_effect %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "purple")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "purple", alpha=0.5)+
  ylab('Scaled Position (V - NV)')+
  xlab('Proportion of Movement')+ 
  geom_line(data=subset(df_condition_effecty), aes(x=time_lores, y=effect), size = 0.5, color = "grey50")+
  geom_point(data = kms_to_plot_effect, aes(x=time_peak_velocity, y=max(to_plot_effect$hi95_1)), size = 1)+
  geom_point(data = kms_to_plot_effect, aes(x=time_peak_acceleration, y=max(to_plot_effect$hi95_1)), size = 1)+
  geom_point(data = kms_to_plot_effect, aes(x=time_peak_deceleration, y=max(to_plot_effect$hi95_1)), size = 1)+
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

noise_df_long_biny %>%
  group_by(time_lores, condition) %>%
  dplyr::summarise(
    avg_SD = mean(SD)
  ) -> subj_noise

noise_to_plot %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "turquoise", alpha=0.5)+
  geom_line(data=subset(subj_noise, condition == "vision"), aes(x=time_lores, y=avg_SD), size = 0.5, color = "gray50")+
  ylab('Log Standard Deviation')+
  xlab('Proportion of Time')+
  ggtitle('Vision')+
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
  geom_line(data=subset(subj_noise, condition == "no_vision"), aes(x=time_lores, y=avg_SD), size = 0.5, color = "gray50")+
  ylab('Log Standard Deviation')+
  xlab('Proportion of Time')+
  ggtitle('No Vision')+
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
  geom_line(aes(x=time, y=med_1), color = "red")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1),fill = "red", alpha=0.5)+
  geom_line(aes(x=time, y=med_2), color = "turquoise")+
  geom_ribbon(aes(x=time, ymin=lo95_2, ymax=hi95_2), fill = "turquoise", alpha=0.5)+
  geom_line(data=subset(subj_noise, condition == "vision"), aes(x=time_lores, y=avg_SD), size = 0.5, color = "gray50")+
  geom_line(data=subset(subj_noise, condition == "no_vision"), aes(x=time_lores, y=avg_SD), size = 0.5, color = "gray50")+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_velocity, y=min(noise_to_plot$med_1)), size = 1, color = "red")+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_acceleration, y=min(noise_to_plot$med_1)), size = 1, color = "red")+
  # geom_point(data = subset(kms_to_plot, condition == "vision"), aes(x=time_peak_deceleration, y=min(noise_to_plot$med_1)), size = 1, color = "red")+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_velocity, y=min(noise_to_plot$med_1)), size = 1, color = "turquoise")+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_acceleration, y=min(noise_to_plot$med_1)), size = 1, color = "turquoise")+
  # geom_point(data = subset(kms_to_plot, condition == "no_vision"), aes(x=time_peak_deceleration, y=min(noise_to_plot$med_1)), size = 1, color = "turquoise")+
  ylab('Log Standard Deviation')+
  xlab('Proportion of Time')+
  annotate("text", label = "Vision", x = 2/15, y = -1.0, color = "red")+
  annotate("text", label = "No Vision", x = 4/15, y = -2.5, color = "turquoise")+
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

# get empirical effect
subj_noise %>%
  dplyr::group_by(time_lores) %>%
  dplyr::summarise(
    effect = avg_SD[condition == "vision"] - avg_SD[condition == "no_vision"]
  ) -> subj_noise_effect

noise_to_plot_effect %>%
  ggplot()+
  geom_line(aes(x=time, y=med_1), color = "purple")+
  geom_ribbon(aes(x=time, ymin=lo95_1, ymax=hi95_1), fill = "purple", alpha=0.5)+
  geom_line(data=subj_noise_effect, aes(x=time_lores, y=effect), size = 0.5, color = "gray50")+
  geom_point(data = kms_to_plot_effect, aes(x=time_peak_velocity, y=min(noise_to_plot_effect$lo95_1)), size = 1)+
  geom_point(data = kms_to_plot_effect, aes(x=time_peak_acceleration, y=min(noise_to_plot_effect$lo95_1)), size = 1)+
  geom_point(data = kms_to_plot_effect, aes(x=time_peak_deceleration, y=min(noise_to_plot_effect$lo95_1)), size = 1)+
  ylab('Log Standard Deviation (V - NV)')+
  xlab('Proportion of Time')+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_gray(base_size = 20)+
  theme(
    panel.grid.major = element_line(size = 0)
    , panel.grid.minor = element_line(size = 0)
    , panel.background = element_rect(fill = "white", color = "black")
  )


