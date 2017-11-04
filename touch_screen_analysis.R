library(tidyverse)

setwd("/Users/ghislaindentremont/Documents/Experiments/Trajectory/Trajectory Studies/MSc_data")

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

names(dat) = c('pilot', 'id', 'age', 'sex', 'hand', 'year', 'month', 'day', 'hour', 'minute', 'seconds', 'blocking', 'block', 'trial', 'iti', 'blocking', 'rt', 'response_time', 'too_soon', 'too_late', 'xfix', 'yfix', 'xtarget', 'ytarget')

# GET RID OF COLUMN DUPLICATE
dat = dat[,-16]


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

# get rid of practice
dat = dat[dat$block == 2 | dat$block == 4,]

# make vision vs. no-vision conditions 
dat$condition = factor(ifelse((dat$blocking == "vision" & dat$block == 2) | (dat$blocking == "no_vision" & dat$block == 4), "vision", "no_vision"))


# Summarize Data ----
summary(dat)

# reaction time
dat %>%
  ggplot(aes(rt*1000, ..density.., group = id))+
    geom_histogram(bins=20)+
    geom_density()+
    facet_wrap(~id)+
    xlab("reaction time (ms)")

# response time
dat %>%
  ggplot(aes(response_time*1000, ..density.., group = id))+
  geom_histogram(bins=20)+
  geom_density()+
  facet_wrap(~id)+
  xlab("response time (ms)")

# define touch screen parameters in terms of participant coordinates
xdim = 1080
ydim = 1920

xfixcoor = xdim/2
yfixcoor = ydim*1/16

xtargetcoor = xdim/2
ytargetcoor = ydim*2/3

# now redefine touch screen fixation touches to be in interpretable frame of reference (0,0 in bottom left corner)
dat$good_xfix = xdim - dat$yfix
dat$good_yfix = ydim - dat$xfix

# now redefine touch screen target responses to be in interpretable frame of reference
dat$good_xtarget = xdim - dat$ytarget
dat$good_ytarget = ydim - dat$xtarget

# target
dat %>%
  ggplot()+
  geom_point(aes(x=good_xfix, y=good_yfix, group = id), na.rm = T, size = 0.25)+
  geom_point(aes(x=xfixcoor, y=yfixcoor, group = id), na.rm = T, size = 0.5, color = "red")+
  geom_point(aes(x=good_xtarget, y=good_ytarget, group = id), na.rm = T, size = 0.25)+
  geom_point(aes(x=xtargetcoor, y=ytargetcoor, group = id), na.rm = T, size = 0.5, color = "red")+
  xlim(c(0, xdim))+
  ylim(c(0, ydim))+
  facet_wrap(condition~id)

# look at error 
dat$fix_error = sqrt((dat$good_xfix - xfixcoor)^2 + (dat$good_yfix - yfixcoor)^2)
dat$target_error = sqrt((dat$good_xtarget - xtargetcoor)^2 + (dat$good_ytarget - ytargetcoor)^2)  

dat %>%
  group_by(condition) %>%
  dplyr::summarize(mean_target_error = mean(target_error, na.rm = T))

