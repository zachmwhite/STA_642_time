########################
# Exploratory analysis
########################
library(magrittr)
library(tidyverse)

########################
# Reading in the data
nba.15.16 = read.csv("C:/Users/Zachary/Desktop/Fall_2017_Projects/STA_642/STA_642_time/game_log_15_16.csv", header =TRUE, stringsAsFactors = FALSE)

dim(nba.15.16)
names(nba.15.16)
# Summarize season averages
season.stat = nba.15.16 %>% select(player_name,points,assists,field_goals_made,field_goals_attempted,free_throws_made,free_throws_attempted,points,three_pointers_made,three_pointers_attempted,
                     steals,blocks,plus_minus) %>%
  group_by(player_name) %>%
  summarize(total_points = sum(points),sd_points = sd(points), points_pg = mean(points),total_assists = sum(assists),assists_pg = mean(assists),total_fgp = sum(field_goals_made) / sum(field_goals_attempted),
            total_3pt = sum(three_pointers_made) /sum(three_pointers_attempted),total_ftp = sum(free_throws_made) / sum(free_throws_attempted),
            total_steals = sum(steals), steals_pg = mean(steals), total_blocks = sum(blocks), blocks_pg = mean(blocks), total_games = n()) %>%
  arrange(desc(total_points))

season.stat$total_games

#########################
# Exploratory
james.harden = nba.15.16 %>% filter(player_name == "James Harden")
