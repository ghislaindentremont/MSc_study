setwd("~/Documents/Experiments/Trajectory/Jenn Study/jenn_data")

library(tidyverse)
library(forecast)
library(plyr)
library(R.matlab)



# Read Data ----
df = map_df(
  .x = list.files(
    pattern = ".mat"
    , recursive = T
    , path =  "/Volumes/LACIE SHARE/Jenn Study/Jenn CPU/Data"
    , full.names = T
    )
  , .f = function(file) {
    temp = strsplit(file, "/")[[1]]
    id = substr(temp[5],11,13)
    
    temp2 = strsplit(temp[6], "[.]")[[1]]
    trial = substr(temp2[1],16, nchar(temp2[1]))
    
    MAT = readMat(file)
    df_piece = data.frame(MAT[[1]])
    df_piece$id = id
    df_piece$trial = trial
    df_piece$frame = 1:150
    
    return(df_piece)
  }
)

names(df)[1:6] = c("x", "y", "z", "x2", "y2", "z2")
df$id = factor(df$id)
df$trial = factor(df$trial)



# Combine Data ----
load("data_matrices.rdata")
mats$trial = as.character(mats$trial)

# below line takes roughly 1 minute
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
# 23.25    0.20   23.99  



# Examine Data ----
# there may be some outliers for the second marker
summary(df)

# which participants are one trial short?
table(df$id, df$block)/150

# turn into long format
df %>%
  dplyr::select(-x2, -y2, -z2) %>%
  gather(coordinate, position, x:z, factor_key = T) -> df_long

# plot trial-by-trial data
df_long %>%
  dplyr::filter(coordinate == "x", as.numeric(id)>7) %>%
  ggplot()+
  geom_line(aes(x=frame, y=position, color = trial), alpha = 0.2)+
  xlab("normalized time")+
  ylab("position (x)")+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long %>%
  dplyr::filter(coordinate == "x", as.numeric(id)<=7) %>%
  ggplot()+
  geom_line(aes(x=frame, y=position, color = trial), alpha = 0.2)+
  xlab("normalized time")+
  ylab("position (x)")+
  facet_grid(id~target)+
  theme(legend.position = "none")
# outlying participant
df_long %>%
  dplyr::filter(coordinate == "x", id == "018") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position, color = trial), alpha = 0.2)+
  xlab("normalized time")+
  ylab("position (x)")+
  facet_grid(id~target)+
  theme(legend.position = "none")

df_long %>%
  dplyr::filter(coordinate == "y", as.numeric(id)>7) %>%
  ggplot()+
  geom_line(aes(x=frame, y=position, color = trial), alpha = 0.2)+
  xlab("normalized time")+
  ylab("position (y)")+
  facet_grid(id~target)+
  theme(legend.position = "none")
df_long %>%
  dplyr::filter(coordinate == "y", as.numeric(id)<=7) %>%
  ggplot()+
  geom_line(aes(x=frame, y=position, color = trial), alpha = 0.2)+
  xlab("normalized time")+
  ylab("position (y)")+
  facet_grid(id~target)+
  theme(legend.position = "none")

df_long %>%
  dplyr::filter(coordinate == "z", as.numeric(id)>7) %>%
  ggplot()+
  geom_line(aes(x=frame, y=position, color = trial), alpha = 0.2)+
  xlab("normalized time")+
  ylab("position (z)")+
  facet_grid(id~target)+
  theme(legend.position = "none")+
  ylim(c(-125, 125))
df_long %>%
  dplyr::filter(coordinate == "z", as.numeric(id)<=7) %>%
  ggplot()+
  geom_line(aes(x=frame, y=position, color = trial), alpha = 0.2)+
  xlab("normalized time")+
  ylab("position (z)")+
  facet_grid(id~target)+
  theme(legend.position = "none")+
  ylim(c(-125, 125))

# participants 016 and 017 are not properly coded




# Clean Data ----
df_long %>%
  dplyr::filter(id != "016", id != "017", position > -200, position < 200) -> df_long_clean 




# Visualize Data ----

# we average over trials
df_long_clean %>%
  group_by(id, coordinate, target, cue, frame) %>%
  dplyr::summarise(
    position_avg = mean(position)
  ) -> df_long_avg

df_long_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_avg, color = id))+
  xlab("normalized time")+
  ylab("position (x)")+
  facet_grid(cue~target)

df_long_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_avg, color = id))+
  xlab("normalized time")+
  ylab("position (y)")+
  facet_grid(cue~target)

df_long_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_avg, color = id))+
  xlab("normalized time")+
  ylab("position (z)")+
  facet_grid(cue~target)

# now we average over participants
df_long_avg %>%
  group_by(coordinate, target, cue, frame) %>%
  dplyr::summarise(
    position_grand_avg = mean(position_avg)
  ) -> df_long_grand_avg

df_long_grand_avg %>%
  dplyr::filter(coordinate == "x") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (x)")+
  facet_grid(.~target)

df_long_grand_avg %>%
  dplyr::filter(coordinate == "y") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (y)")+
  facet_grid(.~target)

df_long_grand_avg %>%
  dplyr::filter(coordinate == "z") %>%
  ggplot()+
  geom_line(aes(x=frame, y=position_grand_avg, color = cue))+
  xlab("time")+
  ylab("position (z)")+
  facet_grid(.~target)

# now we do 2D in space
df_long_grand_avg %>%
  spread(coordinate, position_grand_avg) -> for_2D 

for_2D %>%
  ggplot()+
  geom_line(aes(x=y, y=z, color = cue))+
  xlab("y")+
  ylab("z")+
  facet_grid(.~target)



# Save normalized data ----
save(df_long_clean, file = "data_norm.Rdata")
