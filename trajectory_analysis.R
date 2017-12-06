library(tidyverse)
library(plyr)
library(forecast)
library(signal)
library(zoo)
library(imputeTS)

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
  }
}


# Reaction Time ----
dat$good_rt = dat$rt * 1000

plot_outcomes(1:10, "good_rt", "reaction time")
plot_outcomes(11:20, "good_rt", "reaction time")
plot_outcomes(21:33, "good_rt", "reaction time", T)


# Response Time ----
dat$good_response_time = dat$response_time * 1000

plot_outcomes(1:10, "good_response_time", "response time")
plot_outcomes(11:20, "good_response_time", "response time")
plot_outcomes(21:33, "good_response_time", "response time", T)


# Movement Time ----
dat$good_movement_time = dat$good_response_time - dat$good_rt

plot_outcomes(1:10, "good_movement_time", "movement time")
plot_outcomes(11:20, "good_movement_time", "movement time")
plot_outcomes(21:33, "good_movement_time", "movement time", T)


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
plot_error(21:33)

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
dat %>%
  dplyr::group_by(pilot, id, practice, condition) %>%
  dplyr::summarize(mean_id = mean(target_error, na.rm = T)) %>%
  dplyr::group_by(pilot, id, practice) %>%
  dplyr::summarize(diff_id = diff(mean_id), na.rm = T) %>%
  dplyr::group_by(practice) %>%
  dplyr::summarize(sd_effect = sd(diff_id, na.rm = T))%>%
  print()




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
# how many bad markers in a row will we tolerate
max_lost = 7
max_lost_ms = max_lost * 1000/200

# turn NAs (for any coordinate) into T
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

# identify the trials in which the 
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
  dplyr::summarize(count = length(unique(trial))) %>% print(n=n*2)

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
# how many trials are left?
df_for_long %>%
  dplyr::group_by(pilot, id, condition) %>%
  dplyr::summarize(count = length(unique(trial))) %>%
  print(n=66) 
# how many participants are left?
n=length(unique(df_for_long$id))
n




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
# rm(df_for_long)

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



# Trial Bounds ----
# how far should the fixation* and target be apart?
(ytargetcoor - yfixcoor)
# what are the cutoffs for trial start and end?
# start_line in mm
start_line = 10

plot_bounds = function(ids_use, dv, y_label, start_line) {
  
  df_long_noedge %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=time/1000, y=centered_position, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (s)")+
    ylab(y_label)+
    geom_hline(yintercept = start_line, color = "red")+
    facet_grid(condition~id) %>% print()
}

# Y
plot_bounds(1:10, "y_inter", "y position (mm)", start_line)
plot_bounds(11:20, "y_inter", "y position (mm)", start_line)
plot_bounds(21:33, "y_inter", "y position (mm)", start_line)
plot_bounds(30, "y_inter", "bad y position (mm)", start_line)


# Derivatives ----
# position/(frame * seconds/frame) = position/second
df_long_noedge$velocity = c(0, diff(df_long_noedge$centered_position))/(1/sampling_freq)
# (position/second)/(frame * seconds/frame) = position/second^2
df_long_noedge$acceleration = c(0, diff(df_long_noedge$velocity))/(1/sampling_freq)

df_long_noedge %>% dplyr::filter(time != edge_line + 1000/sampling_freq) -> df_long_clean  # getting rid of first frame
# rm(df_long_noedge)

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
  dplyr::select(moved) %>%
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
  dplyr::select(non_stationary, stationary) %>%
  bind_cols(df_long_clean) -> df_long_clean
# get rid of velocity sub-df
rm(df_long_velo)



# 200 Hz = 200 samples/second ... 'ma_length_ms'/1000 seconds * 200 samples/second = 'ma_length' samples
ma_length = 5
ma_length_ms = ma_length * 1000/200
# we ignore seperations among ids, coordinates, and trials for efficiency
# the following processing should make it so that this does not effect trial starts and ends
df_long_clean$potential_start = as.numeric(ma(df_long_clean$non_stationary & df_long_clean$moved, ma_length))  
df_long_clean$potential_end = as.numeric(ma(df_long_clean$stationary, ma_length)) 

# I actually think this is faster than just using mutate
df_long_clean$trial_start_bool = df_long_clean$potential_start == 1
df_long_clean$trial_end_bool = df_long_clean$potential_end == 1

df_long_clean$trial_start_frame = df_long_clean$trial_start_bool * df_long_clean$frame 
df_long_clean$trial_end_frame = df_long_clean$trial_end_bool * df_long_clean$frame 

# identify first positive change in 'potential end'
df_long_clean$trial_end_diff = diff(c(0,df_long_clean$trial_end_bool))
df_long_clean$trial_end_bool_2 = df_long_clean$trial_end_diff == 1
df_long_clean$trial_end_frame = df_long_clean$trial_end_bool_2 * df_long_clean$frame - 1

# give some space 
buffer_start = 0
buffer_start_ms = buffer_start * 1000/200

buffer_end = 0
buffer_end_ms = buffer_end * 1000/200

df_long_clean %>%
  group_by(pilot, id, condition, trial, coordinate) %>%
  dplyr::mutate(
    trial_start = sort(unique(trial_start_frame))[2] - (ma_length-1)/2 - buffer_start   # we pick the second lowest because there are many zeroes
    , trial_end = sort(unique(trial_end_frame))[2] + (ma_length-1)/2  + buffer_end  # we pick the second lowest because there are many -1s
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
plot_trim = function(ids_use, dv, y_label) {
  
  df_long_trim %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=zero_time, y=centered_position, group=trial, color=trial), alpha = 0.5, size = .2)+
    xlab("time (ms)")+
    ylab(y_label)+
    facet_grid(id~condition) %>% print()
}


####  NOTE: NORMALIZTION SORT OF TAKES CARE OF OUTLYING TRIALS ####


# X
plot_trim(1:10, "x_inter", "x position (mm)")
plot_trim(11:20, "x_inter", "x position (mm)")
plot_trim(21:33, "x_inter", "x position (mm)")
# Y
plot_trim(1:10, "y_inter", "y position (mm)")
plot_trim(11:20, "y_inter", "y position (mm)")
plot_trim(21:33, "y_inter", "y position (mm)")
# Z
plot_trim(1:10, "z_inter", "z position (mm)")
plot_trim(11:20, "z_inter", "z position (mm)")
plot_trim(21:33, "z_inter", "z position (mm)")



# Participant Averages ----
df_long_trim %>%
  group_by(pilot, id, condition, coordinate, zero_time) %>%
  dplyr::summarise(
    position_avg = median(centered_position) # MEDIAN
  ) -> df_long_trim_avg

plot_trim_avg = function(ids_use, dv) {
  
  df_long_trim_avg %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=zero_time, y=position_avg, group=id, color=id), alpha = 0.5, size = .5)+
    xlab("time (ms)")+
    ylab(sprintf("%s average position (mm)", dv))+
    facet_grid(condition~.) %>% print()
}

# X
plot_trim_avg(1:10, "x_inter")
plot_trim_avg(11:20, "x_inter")
plot_trim_avg(21:33, "x_inter")
# Y
plot_trim_avg(1:10, "y_inter")
plot_trim_avg(11:20, "y_inter")
plot_trim_avg(21:33, "y_inter")
# Z
plot_trim_avg(1:10, "z_inter")
plot_trim_avg(11:20, "z_inter")
plot_trim_avg(21:33, "z_inter")



# Group Averages ----
df_long_trim_avg %>%
  group_by(condition, coordinate, zero_time) %>%
  dplyr::summarise(
    position_grand_avg = median(position_avg) # MEDIAN
  ) -> df_long_trim_grand_avg

# function
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
plot_trim_grand_avg("x_inter")
# Y
plot_trim_grand_avg("y_inter")
# Z
plot_trim_grand_avg("z_inter")





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
    norm_position = median(centered_position) # MEDIAN
    , norm_velocity = median(velocity)
    , norm_acceleration = median(acceleration)
  ) -> df_long_norm


# Normalized Trials ----
plot_norm = function(ids_use, dv, dev, y_label) {
  df_long_norm$tempy = dplyr::pull(df_long_norm, dev)
  df_long_norm %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=tempy, group=trial, color=trial), alpha = 0.5, size = .2)+
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
# Z
plot_norm(1:10, "z_inter", "norm_position", "z position (mm)")
plot_norm(11:20, "z_inter", "norm_position", "z position (mm)")
plot_norm(21:33, "z_inter", "norm_position", "z position (mm)")



# Normalized Averages ----
df_long_norm %>%
  group_by(pilot, id, condition, coordinate, norm_time) %>%
  dplyr::summarise(
    position_avg = median(norm_position) # MEDIAN
    , velocity_avg = median(norm_velocity)
    , acceleration_avg = median(norm_acceleration)
  ) -> df_long_norm_avg

# get peak acceleration and decceleration values for each participant 
df_long_norm_avg %>%
  dplyr::group_by(pilot, id, condition, coordinate) %>%
  dplyr::summarize(
    peak_velocity = norm_time[which(velocity_avg == max(velocity_avg))]
    , peak_acceleration = norm_time[which(acceleration_avg == max(acceleration_avg))]
    , peak_deceleration = norm_time[which(acceleration_avg == min(acceleration_avg))]
  ) -> kms

# create function
plot_norm_avg = function(ids_use, dv, dev, y_label) {
  df_long_norm_avg$tempy = dplyr::pull(df_long_norm_avg, dev)
  rang = range(df_long_norm_avg$tempy[df_long_norm_avg$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat = kms[kms$coordinate == dv & (as.numeric(kms$id) %in% ids_use),]
  
  df_long_norm_avg %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=tempy, group=id, color=id), size = .3)+
    geom_point(data = kin_dat, aes(x=peak_velocity, y=top + data_range/50, group=id, color=id), size = 1)+
    geom_point(data = kin_dat, aes(x=peak_acceleration, y=top, group=id, color=id), size = 1)+
    geom_point(data = kin_dat, aes(x=peak_deceleration, y=bottom, group=id, color=id), size = 1)+
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
# Z
plot_norm_avg(1:10, "z_inter", "position_avg", "z position (mm)")
plot_norm_avg(11:20, "z_inter", "position_avg", "z position (mm)")
plot_norm_avg(21:33, "z_inter", "position_avg", "z position (mm)")


# Normalize Grand Averages ----
df_long_norm_avg %>%
  group_by(condition, coordinate, norm_time) %>%
  dplyr::summarise(
    position_grand_avg = median(position_avg) # MEDIAN
    , velocity_grand_avg = median(velocity_avg)
    , acceleration_grand_avg = median(acceleration_avg)
  ) -> df_long_norm_grand_avg

# get average kinematic markers for each group
kms %>%
  dplyr::group_by(condition, coordinate) %>%
  dplyr::summarise(
    peak_velocity_avg = median(peak_velocity) # MEDIAN
    , peak_acceleration_avg = median(peak_acceleration)
    , peak_deceleration_avg =  median(peak_deceleration)
  ) -> kms_avg

# get kinematic markers of each avrage
df_long_norm_grand_avg %>%
  dplyr::group_by(condition, coordinate) %>%
  dplyr::summarize(
    grand_peak_velocity = norm_time[which(velocity_grand_avg == max(velocity_grand_avg))]
    , grand_peak_acceleration = norm_time[which(acceleration_grand_avg == max(acceleration_grand_avg))]
    , grand_peak_deceleration = norm_time[which(acceleration_grand_avg == min(acceleration_grand_avg))]
  ) -> grand_kms
  

# function
plot_norm_grand_avg = function(dv, dev, y_label) {
  df_long_norm_grand_avg$tempy = dplyr::pull(df_long_norm_grand_avg, dev)
  rang = range(df_long_norm_grand_avg$tempy[df_long_norm_grand_avg$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat_avg = kms_avg[kms_avg$coordinate == dv,]
  
  df_long_norm_grand_avg %>%
    dplyr::filter(coordinate == dv) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=tempy, group=condition, color=condition))+
    geom_point(data = kin_dat_avg, aes(x=peak_velocity_avg, y=top+data_range/50, group=condition, color=condition), size = 1)+
    geom_point(data = kin_dat_avg, aes(x=peak_acceleration_avg, y=top, group=condition, color=condition), size = 1)+
    geom_point(data = kin_dat_avg, aes(x=peak_deceleration_avg, y=bottom, group=condition, color=condition), size = 1)+
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
  dplyr::group_by(pilot, id, condition, coordinate, norm_time) %>%
  dplyr::summarise(spat_var = sd(norm_position)) -> df_long_spat_var

# function to plot spatial variability
plot_spat_var = function(ids_use, dv) {
  
  rang = range(df_long_spat_var$spat_var[df_long_spat_var$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat = kms[kms$coordinate == dv & (as.numeric(kms$id) %in% ids_use),]
  
  df_long_spat_var %>%
    dplyr::filter(coordinate == dv, as.numeric(id) %in% ids_use) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=spat_var, group=id, color=id), alpha = 0.5, size = .5)+
    geom_point(data = kin_dat, aes(x=peak_velocity, y=top + data_range/50, group=id, color=id), size = 1)+
    geom_point(data = kin_dat, aes(x=peak_acceleration, y=top, group=id, color=id), size = 1)+
    geom_point(data = kin_dat, aes(x=peak_deceleration, y=bottom, group=id, color=id), size = 1)+
    xlab("normalized time")+
    ylab(sprintf("%s spatial variability (sd)", dv))+
    facet_grid(.~condition) %>% print()
}

# X
plot_spat_var(1:10, "x_inter")
plot_spat_var(11:20, "x_inter")
plot_spat_var(21:33, "x_inter")
# Y
plot_spat_var(1:10, "y_inter")
plot_spat_var(11:20, "y_inter")
plot_spat_var(21:33, "y_inter")
# Z
plot_spat_var(1:10, "z_inter")
plot_spat_var(11:20, "z_inter")
plot_spat_var(21:33, "z_inter")


# Average of Spatial Variabilty Profiles ----
df_long_spat_var %>%
  dplyr::group_by(condition, coordinate, norm_time) %>%
  dplyr::summarise(spat_var_avg = median(spat_var)) -> df_long_spat_var_avg  # MEDIAN

# a function to plot group spatial variability profiles
plot_spat_var_avg = function(dv) {
    
  rang = range(df_long_spat_var_avg$spat_var_avg[df_long_spat_var_avg$coordinate == dv])
  data_range = rang[2] - rang[1]
  bottom = rang[1]
  top = rang[2]
  
  kin_dat_avg = kms_avg[kms_avg$coordinate == dv,]
  
  df_long_spat_var_avg %>%
    dplyr::filter(coordinate == dv) %>%
    ggplot()+
    geom_line(aes(x=norm_time, y=spat_var_avg, group=condition, color=condition))+
    geom_point(data = kin_dat_avg, aes(x=peak_velocity_avg, y=top + data_range/50, group=condition, color=condition), size = 1)+
    geom_point(data = kin_dat_avg, aes(x=peak_acceleration_avg, y=top, group=condition, color=condition), size = 1)+
    geom_point(data = kin_dat_avg, aes(x=peak_deceleration_avg, y=bottom, group=condition, color=condition), size = 1)+
    xlab("normalized time")+
    ylab(sprintf("%s average spatial variability (sd)", dv)) %>% print()
}

# X
plot_spat_var_avg("x_inter")
# Y
plot_spat_var_avg("y_inter")
# Z
plot_spat_var_avg("z_inter")
