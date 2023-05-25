# Explore the distance at which prey target ressources

library(tidyverse)
library(sf)
library(ggplot2)
library(amt)

#-------------#
#  Open data  #
#-------------#
player_data <- read.csv2("data_raw/Player_Data_corrected.csv", sep = ";")[, -1]

gps_data <- read.csv2("data_raw/Tracking_Anonymous.csv", sep = ",")

accelero_data <- read.csv2("data_raw/GPS_tracks_from_accelerometers.csv", sep = ",")[, -1]
names(accelero_data)[1:3] <- c("Game_ID", "player_id", "player_name")


#--------------------#
#   Some functions   #
#--------------------#
# Function to mesure bearing between two point
bearing <- function(pos1, pos2){
  pos1 <- st_drop_geometry(pos1)
  pos2 <- st_drop_geometry(pos2)
  dx <- pos2[, 1] - pos1[, 1]
  dy <- pos2[, 2] - pos1[, 2]
  
  bearing <- atan2(dy, dx)*180/pi
  return(bearing)
}

# Function to measure angle difference between two bearing
angle_diff <- function(origin, end, target){
  theta1 <- bearing(pos1 = origin, pos2 = end)
  theta2 <- bearing(pos1 = origin, pos2 = target)
  theta <- abs(theta1 - theta2) %% 360 
  return(ifelse(theta > 180, 360 - theta, theta))
}


#----------------------------------------------------#
#   Compute the difference in bearing for one prey   #
#----------------------------------------------------#
# The difference in computed between the bearing toward the targeted ressource
# and the true bearing of the next step
# There is some redundancy in the code and lines that are useless, but i'm too afraid to change stuff...

react.distance <- function(data, t_a){
  
  react_dist <- data.frame(dist = numeric(),
                           angle = numeric(),
                           burst = numeric())
  
  # Load track data of one indiv
  # Change lat lon as num
  player_steps <- data |> 
    mutate_at(c('latitude', 'longitude'), as.numeric) |> 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
    st_transform(crs = 32619)
  
  player_steps <- mutate(.data = player_steps, x = st_coordinates(player_steps)[ ,1], y = st_coordinates(player_steps)[, 2]) |> 
    st_drop_geometry()
  
  # Better timestamp
  player_steps$time <- lubridate::hms(player_steps$time)
  
  # Get unique row
  player_steps <- player_steps |> 
    distinct()
  
  # Subsample
  seconds <- seconds(player_steps$time[nrow(player_steps)] - player_steps$time[1])
  subsample <- seconds(seq(0, as.numeric(seconds), 30))
  subsample <- player_steps$time[1] + subsample
  
  player_steps <- player_steps[which(player_steps$time %in% subsample),] |> 
    mutate(step_id = row_number()) |> 
    st_as_sf(coords = c("x", "y"), crs = 32619)
  
  # # Creates tracks from gps points
  # player_steps_formated <- make_track(tbl = player_steps, .x = x, .y = y, .t = time, all_cols = T) |> 
  #   arrange(t_)
  # 
  # # Compute the step metrics (turning angle, step length)
  # player_steps <- steps(player_steps_formated) |> 
  #   mutate(step_id = row_number()) |> 
  #   st_as_sf(coords = c("x1_", "y1_"), crs = 32619)
  # 
  # ## Select 20 fisrt minutes
  # start <- player_steps$t1_[1]
  # min20 <- start + lubridate::minutes(20)
  # 
  ## Extract turning angle within each ressources
  # swift steps before 20
  # player_steps <- filter(player_steps, t1_ <= min20)
  
  ## Create buffer on ressources
  ress_loc <- read.csv2("data_raw/Resources_Location_Final.csv") |> 
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> 
    st_transform(crs = 32619)
  
  ress_buff <- st_buffer(ress_loc, dist = 5)
  
  ## Check for union with 
  in_ress <- st_intersects(player_steps, ress_buff, sparse = FALSE)
  in_ress <- rowSums(in_ress)
  in_ress <- player_steps[which(in_ress == 1), ]
  
  ## Establish presence within ressources buffer
  player_steps$foraging <-player_steps$step_id %in% in_ress$step_id
  
  # Add id to each burst within resource patch
  player_steps$burst <- cumsum(player_steps$foraging)
  
  # If the prey ever get to a resource
  if(sum(player_steps$burst) > 0){
    # Move down each values of the burst column to isolate a full burst
    # From when it leave the patch to the first point when it enters
    player_steps$burst <- lag(player_steps$burst, 1)
    
    # Nest data frame by burst
    player_steps <- player_steps |>
      group_by(burst) |> 
      nest() |> 
      rowwise() |> 
      mutate(b.length = nrow(data)) |> 
      filter(b.length > 2)
    
    if(nrow(player_steps) > 0){
      #----------------------------------------------------#
      #   Loop through all segment between resource use   #
      #----------------------------------------------------#
      for (i in 1:(nrow(player_steps))) {
        # Select data for shorter name in code
        df <- player_steps$data[[i]]
        
        # Identify taget ressource
        target_ress <- st_intersection(st_geometry(ress_buff), st_geometry(df[nrow(df), ]))
        
        # Mesure distance between points and ressource patch
        dist <- st_distance(df, target_ress)
        dist <- dist[-length(dist)]
        
        # Extract geometry
        df_geometry <- st_coordinates(df) 
        
        
        # Mesure bearing difference between next position and ressource patch
        angle <- angle_diff(origin = df_geometry[c(1:nrow(df)-1),], 
                            end = df_geometry[c(2:nrow(df)), ], 
                            target = as.data.frame(st_coordinates(target_ress)))
        
        # # Mesure bearing difference between random position and ressource patch
        # df.track <- cbind(df, st_coordinates(df))
        # df.track <- rbind(df.track[1, ], df.track)
        # df.track[1, c("X", "Y")] <- st_drop_geometry(df.track[1, c("X", "Y")]) - 1 # Creates artificial first point
        # df.track <- make_track(tbl = df.track, .x = X, .y = Y)
        # df.steps <- steps(df.track)
        # df.steps <- random_steps(df.steps, n_control = 1, rand_ta = t_a)
        # df.steps <- df.steps[which(df.steps$case_ == FALSE), ]
        # 
        # angle_rd <- matrix(NA, ncol = 5, nrow = length(angle))
        # 
        # for (i in 1:nrow(angle_rd)) {
        #   # draw 5 ta
        #   draw_ta <- t_a[floor(runif(5, 1, length(t_a)))]
        #   
        #   # compute new coordinate from random ta
        #   
        #   # compute bearing difference
        # }
        
        react_dist <- rbind(react_dist, cbind(dist, angle, burst = rep(i, length(dist))))
      }
      return(react_dist)
      
    } else {
      return(react_dist)
    }
  } else {
    return(react_dist)
    }
}


# #------------------------------------------#
# #  Extract distribution of turning angles  #
# #------------------------------------------#
# 
# t_a <- accelero_data |> 
#   mutate_at(c('latitude', 'longitude'), as.numeric)
# 
# t_a$time <- lubridate::hms(t_a$time)
# 
# # Subsample data at each 30sec
# t_a <- t_a |> 
#   group_by(Game_ID, player_id) |> 
#   nest()
# 
# # subsample for each game and player
# for (i in 1:nrow(t_a)) {
#   t_a_data <- t_a$data[[i]]
#   t_a_data <- unique(t_a_data)
#   
#   # Subsample
#   seconds <- seconds(t_a_data$time[nrow(t_a_data)] - t_a_data$time[1])
#   subsample <- seconds(seq(0, as.numeric(seconds), 30))
#   subsample <- t_a_data$time[1] + subsample
#   
#   t_a$data[[i]] <- t_a_data[which(t_a_data$time %in% subsample),] 
# }
# 
# # Unnest t_a
# t_a <- unnest(t_a, cols = c(data)) |> 
#   arrange(Game_ID, player_id, time)
# 
# # transform into track object
# t_a <- make_track(tbl = t_a, .x = longitude, .y = latitude, all_cols = T)
# 
# # make a unique payer id and game
# t_a$Id_game <- paste0(t_a$player_id, t_a$Game_ID)
# 
# # for all players
# # split the dataframe by ID and game
# t_a_split <- split(t_a, f = t_a$Id_game)
# 
# # function to make steps between successive relocations
# make_steps <- function(data){data |> 
#     ungroup() |> 
#     arrange(time) |> 
#     steps()}
# 
# # apply the function to each track
# t_a_split <- lapply(X = t_a_split, FUN= make_steps)
# 
# # put everything into one dataframe
# t_a <- do.call(rbind, t_a_split)
# t_a <- t_a$ta_
# t_a <- na.omit(t_a)

#-------------------------------------------------------------------------#
#   Run the script for the games of the second day using accelerometers   #
#-------------------------------------------------------------------------#
all_react <- data.frame(dist = numeric(),
                        angle = numeric(),
                        burst = numeric(),
                        game = numeric(),
                        player = numeric())

games <- unique(accelero_data$Game_ID)

# Loop through games
for (i in min(games):max(games)) {
  
  data_game <- subset(accelero_data, Game_ID == i)
  
  prey <- player_data |> 
    filter(game_id == i, role == "prey")
  prey <- unique(prey$player_id)
    
  for (j in 1:length(prey)) {
    
    data_player <- filter(data_game, player_id == prey[j])
    if(nrow(data_player) > 0){
      react_player <- react.distance(data = data_player, t_a = NA)
      
      if(nrow(react_player) > 0){
        n.r <- nrow(react_player)
        react_player <- cbind(react_player, game = rep(i, n.r), player = rep(prey[j], n.r))
      }
    }
    all_react <- rbind(all_react, react_player)
  }
}

# Remove dist > 400 m
all_react <- filter(all_react, dist < 400)

# color <- rgb(red = 46, green = 196, blue = 142, alpha = 20, maxColorValue = 255)
# plot(all_react$angle ~ all_react$dist, pch = 19, col = color)
# hist(all_react$dist)
# hist(all_react$angle)


# Plot all angle by distance
ggplot(all_react, aes(dist, angle)) +
  geom_point(alpha = 0.05, colour = "#2ec48d") +
  theme_classic()


# Break into bin of 5 meters
all_react <-  all_react |> 
  mutate(bin = cut(dist ,breaks = seq(0, 400, 5)))

# Boxplot by distance
ggplot(all_react, aes(x = bin, y = angle)) +
  geom_boxplot()

# Subset first 20 bins
all_react_sub <- all_react |> 
  mutate(bin = cut(dist ,breaks = seq(0, 400, 10)))

all_react_sub <- all_react_sub[which(all_react_sub$bin %in% levels(all_react_sub$bin)[1:30]) , ]

ggplot(all_react_sub, aes(x = bin, y = angle, fill = bin, color = bin)) +
  geom_violin() +
  theme_classic() + 
  viridis::scale_fill_viridis(discrete = TRUE) +
  viridis::scale_color_viridis(discrete = TRUE) + 
  theme(legend.position = "none") +
  coord_flip() +
  ylab("Delta bearing") + xlab("Distance from ressource")

all_react_bin <- all_react |> 
  group_by(bin) |> 
  nest()

## Plot histograms of all bins
# n_plots <- 
#   all_react_bin %>% 
#   mutate(plot = map2(
#     data, bin, 
#     ~ ggplot(data = .x, aes(x = angle)) +
#       geom_histogram()))
# 
# print(n_plots$plot[1:5]) 



# Ridgeplot by bins
library(ggridges)
ggplot(all_react, aes(x = angle, y = bin, fill = bin)) +
  geom_density_ridges(scale = 6, alpha = 0.7, rel_min_height = 0.01) +
  theme_ridges() + 
  theme(legend.position = "none")

se <- function(x){sd(x)/sqrt(length(x))}

# Compute mean + se of all bins
all_react_bin <- all_react_bin |> 
  arrange(bin) |> 
  rowwise() |> 
  mutate(mean = mean(data$angle),
         se = se(data$angle),
         median = median(data$angle))

# Extract mean bin distance
distances.bin <- all_react_bin$bin |> 
  substr(start = 2, stop = 4)

distances.bin <- sapply(strsplit(distances.bin, ","), "[", 1) |> 
  as.numeric()

all_react_bin <- all_react_bin |> 
  add_column(dist.bin = distances.bin + 2.5)



# Plot mean angle by mean dist
plot(all_react_bin$mean ~ all_react_bin$dist.bin, ylim = c(0, 180), pch = 19)
arrows(x0 = all_react_bin$dist.bin, x1 = all_react_bin$dist.bin,
       y0 = all_react_bin$mean, y1 = (all_react_bin$mean + 1.96*all_react_bin$se)
       , length = 0.05, angle = 90)
arrows(x0 = all_react_bin$dist.bin, x1 = all_react_bin$dist.bin,
       y0 = all_react_bin$mean, y1 = (all_react_bin$mean - 1.96*all_react_bin$se)
       , length = 0.05, angle = 90)
abline(h = 90, lty = 2)
# points(all_react_bin$median ~ all_react_bin$dist.bin, ylim = c(0, 180), col = "red", pch = 19, cex = 0.8)






# # Mesure the time before arriving at the pach
# for (i in 1:nrow(player_steps)) {
#   max.time <- max(player_steps$data[[i]]$t1_)
#   
#   player_steps$data[[i]]$time_back <- player_steps$data[[i]]$t1_ - max.time
# }
# 
# 
# par(mfrow = c(2, ceiling(nrow(player_steps)/2)))
# 
# for(j in 1:nrow(player_steps)){
#   plot(cos(player_steps$data[[j]]$ta_) ~ player_steps$data[[j]]$time_back, 
#      type = "b", xlim = c(min(player_steps$data[[j]]$time_back), 0))
# }
# 
# x.coord <- c(min(player_steps_formated$x_), max(player_steps_formated$x_))
# y.coord <- c(min(player_steps_formated$y_), max(player_steps_formated$y_))
#   
# # Vizualise the data
# ggplot()+
#   geom_path(data = player_steps_formated, linewidth = 1.2, mapping = aes(x = x_, y= y_, col = t_)) +
#   viridis::scale_fill_viridis() +
#   geom_sf(data = ress_buff, mapping = aes(), fill = NA) +
#   geom_sf(data = in_ress, mapping = aes(), col = "red") +
#   coord_sf(datum=st_crs(32619), xlim = x.coord, ylim = y.coord)
