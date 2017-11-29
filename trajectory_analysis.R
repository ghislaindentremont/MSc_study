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

summary(dat)

plot_outcomes = function(ids_use, dv, xlabel, summa = F) {
  
  dat$temp = dplyr::pull(dat, dv)
  
  gg = ggplot()+
    geom_histogram(data = subset(dat, condition == "no_vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), bins=20, alpha = 0.2, fill = "red")+
    geom_density(data = subset(dat, condition == "no_vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), color = "red")+
    geom_histogram(data = subset(dat, condition == "vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), bins=20, alpha = 0.2, fill = "blue")+
    geom_density(data = subset(dat, condition == "vision" & as.numeric(id) %in% ids_use), aes(temp, ..density..), color = "blue")+
    facet_grid(practice~id)+
    xlab(sprintf("%s (ms)", xlabel))
  print(gg)
  
  if (summa) {
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
      print
  }
}


# Reaction Time ----
dat$good_rt = dat$rt * 1000

plot_outcomes(1:10, "good_rt", "reaction time")
plot_outcomes(11:20, "good_rt", "reaction time")
plot_outcomes(21:30, "good_rt", "reaction time", T)


# Response Time ----
dat$good_response_time = dat$response_time * 1000

plot_outcomes(1:10, "good_response_time", "response time")
plot_outcomes(11:20, "good_response_time", "response time")
plot_outcomes(21:30, "good_response_time", "response time", T)


# Movement Time ----
dat$good_movement_time = dat$good_response_time - dat$good_rt

plot_outcomes(1:10, "good_movement_time", "movement time")
plot_outcomes(11:20, "good_movement_time", "movement time")
plot_outcomes(21:30, "good_movement_time", "movement time", T)


# Movement Error ----

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
plot_error(21:30)

# look at error 
dat$fix_error = sqrt((dat$good_xfix - xfixcoor)^2 + (dat$good_yfix - yfixcoor)^2)
dat$target_error = sqrt((dat$good_xtarget - xtargetcoor)^2 + (dat$good_ytarget - ytargetcoor)^2)  

dat %>%
  group_by(pilot, id, practice, condition) %>%
  dplyr::summarize(mean_id_target_error = mean(target_error, na.rm = T)) %>%
  group_by(practice, condition) %>%
  dplyr::summarize(mean_target_error = mean(mean_id_target_error, na.rm = T)) 
dat %>%
  group_by(pilot, id, practice, condition) %>%
  dplyr::summarize(mean_id_target_error = mean(target_error, na.rm = T)) %>%
  group_by(practice, condition) %>%
  dplyr::summarize(sd_target_error = sd(mean_id_target_error, na.rm = T)) 




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
  print(n=60)
  
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
plot_points = function(ids_use, dv, ylimits, plot_lines = F) {
  df$temp = dplyr::pull(df, dv)
  
  df %>%
    dplyr::filter(as.numeric(id) %in% ids_use) %>%
    ggplot() -> gg
  
  if (plot_lines) {
    gg = gg + geom_line(aes(x=frame, y=temp, group=trial, color=trial), alpha = 0.4, size = .2)
  } else {
    gg = gg + geom_point(aes(x=frame, y=temp, group=trial, color=trial), alpha = 0.2, size = .2)
  }
  
  gg = gg +
    xlab("frame")+
    ylab(sprintf("%s position (mm)", dv))+
    ylim(ylimits)+
    facet_grid(condition~id)
  
  print(gg)
}


# Primary Marker ----
# how many of these trials contain at least one of these outlying values?
df %>%
  dplyr::filter(abs(x) > 1e25 | abs(y) > 1e25 | abs(z) > 1e25) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial)) %>%
  print(n=30)

# X
plot_points(1:10, "x", c(100, 400))
plot_points(11:21, "x", c(100, 400))
plot_points(21:30, "x", c(100, 400))
# Y
plot_points(1:10, "y", c(-200, 200))
plot_points(11:21, "y", c(-200, 200))
plot_points(21:30, "y", c(-200, 200))
plot_points(c(3,9,19,26), "y", c(-200, 200))
# Z
plot_points(1:10, "z", c(-3250, -3150))
plot_points(11:21, "z", c(-3250, -3150))
plot_points(21:30, "z", cc(-3250, -3150))


# Secondary Marker ----
# how many of these trials contain at least one of these outlying values?
df %>%
  dplyr::filter(abs(x2) > 1e25 | abs(y2) > 1e25 | abs(z2) > 1e25) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x2)) %>%
  dplyr::summarize(num_trials = length(ez_trial)) %>%
  print(n=30)

# X
plot_points(1:10, "x2", c(100, 400))
plot_points(11:21, "x2", c(100, 400))
plot_points(21:30, "x2", c(100, 400))
# Y
plot_points(1:10, "y2", c(-200, 200))
plot_points(11:21, "y2", c(-200, 200))
plot_points(21:30, "y2", c(-200, 200))
plot_points(c(19,26), "y2", c(-200, 200))
# Z
plot_points(1:10, "z2", c(-3250, -3150))
plot_points(11:21, "z2", c(-3250, -3150))
plot_points(21:30, "z2", cc(-3250, -3150))


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

# X
compare_lines(1:10, c("x", "x2"), c(100, 400))
compare_lines(11:21, c("x", "x2"), c(100, 400))
compare_lines(21:30, c("x", "x2"), c(100, 400))
# Y
compare_lines(1:10, c("y", "y2"), c(-200, 200))
compare_lines(11:21, c("y", "y2"), c(-200, 200))
compare_lines(21:30, c("y", "y2"), c(-200, 200))
compare_lines(c(3, 9, 19,26), c("y", "y2"), c(-200, 200))
# Z
compare_lines(1:10, c("z", "z2"), c(-3250, -3150))
compare_lines(11:21, c("z", "z2"), c(-3250, -3150))
compare_lines(21:30, c("z", "z2"), c(-3250, -3150))



# Running Average Interpolation ---- 
# needs to be odd for runmed()
ma_inter_length = 11
ma_inter_length_ms = ma_inter_length * 1000/200

ptm = proc.time()
df %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::mutate(
    x_inter = as.numeric(runmed(x, ma_inter_length))
    , y_inter = as.numeric(runmed(y, ma_inter_length))
    , z_inter = as.numeric(runmed(z, ma_inter_length))
    , x2_inter = as.numeric(runmed(x2, ma_inter_length))
    , y2_inter = as.numeric(runmed(y2, ma_inter_length))
    , z2_inter = as.numeric(runmed(z2, ma_inter_length))

  ) -> df
proc.time() - ptm

# interpolated positions
# X
compare_lines(1:10, c("x_inter", "x2_inter"), c(100, 400))
compare_lines(11:21, c("x_inter", "x2_inter"), c(100, 400))
compare_lines(21:30, c("x_inter", "x2_inter"), c(100, 400))
# Y
compare_lines(1:10, c("y_inter", "y2_inter"), c(-200, 200))
compare_lines(11:21, c("y_inter", "y2_inter"), c(-200, 200))
compare_lines(21:30, c("y_inter", "y2_inter"), c(-200, 200))
# compare_lines(c(3, 9, 19,26), c("yinter", "y2_inter"), c(-200, 200))
# Z
compare_lines(1:10, c("z_inter", "z2_inter"), c(-3250, -3150))
compare_lines(11:21, c("z_inter", "z2_inter"), c(-3250, -3150))
compare_lines(21:30, c("z_inter", "z2_inter"), c(-3250, -3150))




# Replace Values ----
# replace extreme trials with interpolated value
df$x = ifelse(abs(df$x) > 1e25, df$x_inter, df$x)
df$y = ifelse(abs(df$y) > 1e25, df$y_inter, df$y)
df$z = ifelse(abs(df$z) > 1e25, df$z_inter, df$z)
# do the same for the secondary markers
df$x2 = ifelse(abs(df$x2) > 1e25, df$x2_inter, df$x2)
df$y2 = ifelse(abs(df$y2) > 1e25, df$y2_inter, df$y2)
df$z2 = ifelse(abs(df$z2) > 1e25, df$z2_inter, df$z2)

# identify trials that are bad
df %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::mutate(
    x_drop = ifelse(max(abs(x)) > 1e25, TRUE, FALSE)
    , y_drop = ifelse(max(abs(y)) > 1e25, TRUE, FALSE)
    , z_drop = ifelse(max(abs(z)) > 1e25, TRUE, FALSE)
    , x2_drop = ifelse(max(abs(x2)) > 1e25, TRUE, FALSE)
    , y2_drop = ifelse(max(abs(y2)) > 1e25, TRUE, FALSE)
    , z2_drop = ifelse(max(abs(z2)) > 1e25, TRUE, FALSE)
  ) -> df

# how many bad trials for the primary marker?
df %>%
  dplyr::filter(x_drop | y_drop | z_drop) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial)) %>%
  print(n=30)
# how many bad trials for the secondary marker?
df %>%
  dplyr::filter(x2_drop | y2_drop | z2_drop) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial)) %>%
  print(n=30)
# how many trials will be dropped?
df %>%
  dplyr::filter(x_drop | y_drop | z_drop, x2_drop | y2_drop | z2_drop) %>%
  dplyr::group_by(pilot, id, ez_trial) %>%
  dplyr::summarize(dummy = mean(x)) %>%
  dplyr::summarize(num_trials = length(ez_trial)) %>%
  print(n=30)

# replace bad trial with secondary marker
df %>%
  dplyr::group_by(pilot, id, condition, trial) %>%
  dplyr::mutate(
    x = ifelse(x_drop, ifelse(x2_drop, NA, x2), x)
    , y = ifelse(y_drop, ifelse(y2_drop, NA, y2), y)
    , z = ifelse(z_drop, ifelse(z2_drop, NA, z2), z)
  ) -> df

# get rid of bad trials
df %>%
  dplyr::filter(!is.na(x), !is.na(y), !is.na(z)) -> df
  
# how many trials are left in each condition?
df %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(unique(trial))) %>%
  print(n=60) -> trial_count

# get rid of participants with fewer than 10 trials for any given condition
trial_count %>% dplyr::filter(count > 10) -> id_keep
id_keep %>% dplyr::filter(condition == "vision") %>% dplyr::pull("id") -> id_keep_v
id_keep %>% dplyr::filter(condition == "no_vision") %>% dplyr::pull("id") -> id_keep_nv
keep = dplyr::intersect(id_keep_v, id_keep_nv)

df %>% dplyr::filter(id %in% keep) -> df

# how many trials are left?
df %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(unique(trial))) %>%
  print(n=60) 
# how many participants are left?
n=length(unique(df$id))


# X
plot_points(1:10, "x", c(100, 400), T)
plot_points(11:21, "x", c(100, 400), T)
plot_points(21:30, "x", c(100, 400), T)
# Y
plot_points(1:10, "y", c(-200, 200), T)
plot_points(11:21, "y", c(-200, 200), T)
plot_points(21:30, "y", c(-200, 200), T)
# Z
plot_points(1:10, "z", c(-3250, -3150), T)
plot_points(11:21, "z", c(-3250, -3150), T)
plot_points(21:30, "z", c(-3250, -3150), T)



# Outliers ----
# DO THIS?





##########################################################
####             Signal Processing                    ####
##########################################################

# Long Format ----
# create time variable (ms) from frame variable
sampling_freq = 200
df$time = df$frame/sampling_freq*1000

df %>%
  dplyr::select(-x2, -y2, -z2) %>%
  gather(coordinate, position, x:z, factor_key = T) -> df_long
# rm(df)

# make trial start ~zero by subtracting median of frist few samples
df_long %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(position = position - median(position[1:100])) -> df_long

# create function to plot points across trials and participants
plot_long = function(ids_use, dv, edge_line = 0) {
  
  df_long %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=time, y=position, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (ms)")+
    ylab(sprintf("%s position (mm)", dv))+
    geom_vline(xintercept = edge_line, color = "red")+
    facet_grid(condition~id) %>% print()
}

# X
plot_long(1:10, "x")
plot_long(11:20, "x")
plot_long(21:30, "x")
# Y
plot_long(1:10, "y")
plot_long(11:20, "y")
plot_long(21:30, "y")
# Z
plot_long(1:10, "z")
plot_long(11:20, "z")
plot_long(21:30, "z")




# Filtering ----
# want low pass butterworth filter (zero-shift) with 8 Hz cutoff

# filter order is first argument according to documentation
filter_order = 6
hi_cutoff = 8
nyquist_freq = sampling_freq/2 
bf = butter(filter_order, hi_cutoff/nyquist_freq, type = "low")

# make copy for renaming purpose (lazy)
df_long$unfil_position = df_long$position

df_long %>%
  dplyr::group_by(pilot, id, block, coordinate, trial) %>%
  dplyr::mutate(position = signal::filtfilt(bf, unfil_position)) -> df_long

# look at result
edge_line = 50

# X
plot_long(1:10, "x", edge_line)
plot_long(11:20, "x", edge_line)
plot_long(21:30, "x", edge_line)
# Y
plot_long(1:10, "y", edge_line)
plot_long(11:20, "y", edge_line)
plot_long(21:30, "y", edge_line)
# Z
# THE Z DATA ARE CRAP!
plot_long(1:10, "z", edge_line)
plot_long(11:20, "z", edge_line)
plot_long(21:30, "z", edge_line)


# get rid of edge effect at beginning
df_long %>% dplyr::filter(time > edge_line) -> df_long



# Trial Bounds ----
# how far should the fixation* and target be apart?
(ytargetcoor - yfixcoor)
# what are the cutoffs for trial start and end?
start_line = 50
end_line = 270

plot_bounds = function(ids_use, dv, start_line, end_line) {
  
  df_long %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=time, y=position, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (ms)")+
    ylab(sprintf("%s position (mm)", dv))+
    geom_hline(yintercept = start_line, color = "red")+
    geom_hline(yintercept = end_line, color = "red")+
    facet_grid(condition~id) %>% print()
}

# Y
plot_bounds(1:10, "y", start_line, end_line)
plot_bounds(11:20, "y", start_line, end_line)
plot_bounds(21:30, "y", start_line, end_line)



# Derivatives ----
# position/(frame * seconds/frame)
df_long$velocity = c(0, diff(df_long$position))/(1/sampling_freq)
df_long$acceleration = c(0, diff(df_long$velocity))/(1/sampling_freq)

df_long %>% dplyr::filter(time != edge_line + 1000/sampling_freq) -> df_long_clean  # getting rid of first frame
# rm(df_long)

# create function to plot velocity and acceleration
plot_devs = function(ids_use, dv, dev = "velocity") {
  df_long_clean$temp = dplyr::pull(df_long_clean, dev)
  
  df_long_clean %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=time, y=temp, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (ms)")+
    ylab(sprintf("%s %s", dv, dev))+
    facet_grid(condition~id) %>% print()
}

# DEVS ARE TOO NOISY
# # Velocity
# # X
# plot_devs(1:10, "x")
# plot_devs(11:20, "x")
# plot_devs(21:30, "x")
# # Y
# plot_devs(1:10, "y")
# plot_devs(11:20, "y")
# plot_devs(21:30, "y")
# # Z
# plot_devs(1:10, "z")
# plot_devs(11:20, "z")
# plot_devs(21:30, "z")
# 
# # Acceleration
# # X
# plot_devs(1:10, "x", "acceleration")
# plot_devs(11:20, "x", "acceleration")
# plot_devs(21:30, "x", "acceleration")
# # Y
# plot_devs(1:10, "y", "acceleration")
# plot_devs(11:20, "y", "acceleration")
# plot_devs(21:30, "y", "acceleration")
# # Z
# plot_devs(1:10, "z", "acceleration")
# plot_devs(11:20, "z", "acceleration")
# plot_devs(21:30, "z", "acceleration")



# Trial start ----
# ensure order of data
df_long_clean %>% dplyr::arrange(pilot, id, condition, trial, coordinate, time) -> df_long_clean

# determine when thresholds are exceeded in primary movement axis
df_long_clean %>%
  dplyr::select(pilot, id, condition, trial, coordinate, time, position) %>%
  spread(coordinate, position) %>%
  dplyr::mutate(
    non_stationary = y > start_line
    , stationary = y > end_line
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
  dplyr::select(non_stationary, stationary) %>%
  bind_cols(df_long_clean) %>%
  dplyr::select(pilot:velocity, non_stationary:stationary) -> df_long_clean
# get rid of velocity sub-df
rm(df_long_velo)



# 200 Hz = 200 samples/second ... 'ma_length_ms'/1000 seconds * 200 samples/second = 'ma_length' samples
ma_length = 4
ma_length_ms = ma_length * 1000/200
# we ignore seperations among ids, coordinates, and trials for efficiency
# the following processing should make it so that this does not effect trial starts and ends
df_long_clean$potential_start = as.numeric(ma(df_long_clean$non_stationary, ma_length))  
df_long_clean$potential_end = as.numeric(ma(df_long_clean$stationary, ma_length)) 

# I actually think this is faster than just using mutate
df_long_clean$trial_start_bool = df_long_clean$potential_start == 1
df_long_clean$trial_end_bool = df_long_clean$potential_end == 1

df_long_clean$trial_start_frame = df_long_clean$trial_start_bool * df_long_clean$frame 
df_long_clean$trial_end_frame = df_long_clean$trial_end_bool * df_long_clean$frame 

# identify first positive change in 'potential emd'
df_long_clean$trial_end_diff = diff(c(0,df_long_clean$trial_end_bool))
df_long_clean$trial_end_bool_2 = df_long_clean$trial_end_diff == 1
df_long_clean$trial_end_frame = df_long_clean$trial_end_bool_2 * df_long_clean$frame - 1

# # select some columns to make things faster
# df_long_clean %>%
#   dplyr::select(
#     -c(block, velocity, non_stationary, potential_start, trial_start_bool, trial_end_diff, trial_end_bool)
#   ) -> df_long_clean

# give some space 
buffer_start = 20
buffer_start_ms = buffer_start * 1000/200

buffer_end = 20
buffer_end_ms = buffer_end * 1000/200

df_long_clean %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    trial_start = sort(unique(trial_start_frame))[2] - ma_length/2 - buffer_start   # we pick the second lowest because there are many zeroes
    , trial_end = sort(unique(trial_end_frame))[2] + buffer_end  # we pick the second lowest because there are many -1s
  ) -> df_long_clean

df_long_clean %>%
  dplyr::filter(
    frame >= trial_start
    , frame <= trial_end
  ) -> df_long_trim

df_long_trim %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    zero_time = round(time - min(time))  # grouping works for min()
  ) -> df_long_trim




##########################################################
####             Visualize Waveforms                  ####
##########################################################

# create function to plot trimmed data 
plot_trim = function(ids_use, dv) {
  
  df_long_trim %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=zero_time, y=position, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (ms)")+
    ylab(sprintf("%s position (mm)", dv))+
    facet_grid(condition~id) %>% print()
}

# X
plot_trim(1:10, "x")
plot_trim(11:20, "x")
plot_trim(21:30, "x")
# Y
plot_trim(1:10, "y")
plot_trim(11:20, "y")
plot_trim(21:30, "y")
# Z
plot_trim(1:10, "z")
plot_trim(11:20, "z")
plot_trim(21:30, "z")



# Participant Averages ----
df_long_trim %>%
  group_by(pilot, id, condition, coordinate, zero_time) %>%
  dplyr::summarise(
    position_avg = median(position) # MEDIAN
  ) -> df_long_trim_avg

plot_trim_avg = function(ids_use, dv) {
  
  df_long_trim_avg %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=zero_time, y=position_avg, group=id, color=id), alpha = 0.5, size = .5)+
    xlab("time (ms)")+
    ylab(sprintf("%s average position (mm)", dv))+
    facet_grid(.~condition) %>% print()
}

# X
plot_trim_avg(1:10, "x")
plot_trim_avg(11:20, "x")
plot_trim_avg(21:30, "x")
# Y
plot_trim_avg(1:10, "y")
plot_trim_avg(11:20, "y")
plot_trim_avg(21:30, "y")
# Z
plot_trim_avg(1:10, "z")
plot_trim_avg(11:20, "z")
plot_trim_avg(21:30, "z")



# Group Averages ----
df_long_trim_avg %>%
  group_by(condition, coordinate, zero_time) %>%
  dplyr::summarise(
    position_grand_avg = median(position_avg) # MEDIAN
  ) -> df_long_trim_grand_avg

plot_trim_grand_avg = function(dv) {
  
  df_long_trim_grand_avg %>%
    dplyr::filter(coordinate == dv) %>%
    ggplot()+
    geom_line(aes(x=zero_time, y=position_grand_avg))+
    xlab("time (ms)")+
    ylab(sprintf("%s grand average position", dv))+
    facet_grid(.~condition) %>% print()
}

# X
plot_trim_grand_avg("x")
# Y
plot_trim_grand_avg("y")
# Z
plot_trim_grand_avg("z")





##########################################################
####            Normalized Waveforms                  ####
##########################################################


# Normalize ----
round0 = function(x,z){
  round(x/z,0)*z
}

df_long_trim %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    norm_time = round0((zero_time-min(zero_time))/(max(zero_time)-min(zero_time)), 0.05)
    ) -> df_long_norm2

df_long_norm2 %>%
  dplyr::group_by(pilot, id, condition, trial, coordinate, norm_time) %>%
  dplyr::mutate(
    norm_position = median(position) # MEDIAN
  ) -> df_long_norm

# create function to plot norm data 
plot_norm = function(ids_use, dv) {
  
  df_long_norm %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=norm_position, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("normalized time")+
    ylab(sprintf("%s position (mm)", dv))+
    facet_grid(condition~id) %>% print()
}

# X
plot_norm(1:10, "x")
plot_norm(11:20, "x")
plot_norm(21:30, "x")
# Y
plot_norm(1:10, "y")
plot_norm(11:20, "y")
plot_norm(21:30, "y")
# Z
plot_norm(1:10, "z")
plot_norm(11:20, "z")
plot_norm(21:30, "z")



# Normalized Averages ----
df_long_norm %>%
  group_by(pilot, id, condition, coordinate, norm_time) %>%
  dplyr::summarise(
    position_avg = median(norm_position) # MEDIAN
  ) -> df_long_norm_avg

plot_norm_avg = function(ids_use, dv) {
  
  df_long_norm_avg %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=position_avg, group=id, color=id), size = .3)+
    xlab("normalized time")+
    ylab(sprintf("%s average position (mm)", dv))+
    facet_grid(.~condition) %>% print()
}

# X
plot_norm_avg(1:10, "x")
plot_norm_avg(11:20, "x")
plot_norm_avg(21:30, "x")
# Y
plot_norm_avg(1:10, "y")
plot_norm_avg(11:20, "y")
plot_norm_avg(21:30, "y")
# Z
plot_norm_avg(1:10, "z")
plot_norm_avg(11:20, "z")
plot_norm_avg(21:30, "z")


# now we average over participants
df_long_norm_avg %>%
  group_by(condition, coordinate, norm_time) %>%
  dplyr::summarise(
    position_grand_avg = median(position_avg) # MEDIAN
  ) -> df_long_norm_grand_avg

plot_norm_grand_avg = function(dv) {
  
  df_long_norm_grand_avg %>%
    dplyr::filter(coordinate == dv) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=position_grand_avg, group=condition, color=condition))+
    xlab("normalized time")+
    ylab(sprintf("%s grand average position (mm)", dv)) %>% print()
}

# X
plot_norm_grand_avg("x")
# Y
plot_norm_grand_avg("y")
# Z
plot_norm_grand_avg("z")




##########################################################
####          Spatial Variability Profiles            ####
##########################################################

df_long_norm %>%
  dplyr::group_by(pilot, id, condition, coordinate, norm_time) %>%
  dplyr::summarise(spat_var = sd(norm_position)) -> df_long_spat_var

# function to plot spatial variability
plot_spat_var = function(ids_use, dv) {
  
  df_long_spat_var %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=spat_var, group=id, color=id), alpha = 0.5, size = .5)+
    xlab("normalized time")+
    ylab(sprintf("%s spatial variability (sd)", dv))+
    facet_grid(.~condition) %>% print()
}

# X
plot_spat_var(1:10, "x")
plot_spat_var(11:20, "x")
plot_spat_var(21:30, "x")
# Y
plot_spat_var(1:10, "y")
plot_spat_var(11:20, "y")
plot_spat_var(21:30, "y")
# Z
plot_spat_var(1:10, "z")
plot_spat_var(11:20, "z")
plot_spat_var(21:30, "z")


# Average of Spatial Variabilty Profiles ----
df_long_spat_var %>%
  dplyr::group_by(condition, coordinate, norm_time) %>%
  dplyr::summarise(spat_var_avg = median(spat_var)) -> df_long_spat_var_avg  # MEDIAN

# a function to plot group spatial variability profiles
plot_spat_var_avg = function(dv) {
  
  df_long_spat_var_avg %>%
    dplyr::filter(coordinate == dv) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=spat_var_avg, group=condition, color=condition))+
    xlab("normalized time")+
    ylab(sprintf("%s average spatial variability (sd)", dv)) %>% print()
}

# X
plot_spat_var_avg("x")
# Y
plot_spat_var_avg("y")
# Z
plot_spat_var_avg("z")
