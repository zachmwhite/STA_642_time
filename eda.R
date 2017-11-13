########################
# Exploratory analysis
########################
library(magrittr)
library(tidyverse)
library(reshape)

########################
# Reading in the data
nba.15.16 = read.csv("C:/Users/Zachary/Desktop/Fall_2017_Projects/STA_642/STA_642_time/game_log_15_16.csv", header =TRUE, stringsAsFactors = FALSE)

dim(nba.15.16)
names(nba.15.16)
# Summarize season averages
season.stat.prelim = nba.15.16 %>% select(player_name,points,assists,field_goals_made,field_goals_attempted,free_throws_made,free_throws_attempted,points,
                                          three_pointers_made,three_pointers_attempted,steals,blocks,plus_minus, rebounds_defensive,rebounds_offensive, rebounds_total) %>%
  group_by(player_name) %>%
  summarize(total_points = sum(points),sd_points = sd(points), points_pg = mean(points),total_assists = sum(assists),assists_pg = mean(assists),total_fgp = sum(field_goals_made) / sum(field_goals_attempted),
            total_fg = sum(field_goals_attempted), 
            total_3ptp = sum(three_pointers_made) /sum(three_pointers_attempted),total_3pt = sum(three_pointers_attempted),
            total_ftp = sum(free_throws_made) / sum(free_throws_attempted), total_ft = sum(free_throws_attempted),
            total_steals = sum(steals), steals_pg = mean(steals), total_blocks = sum(blocks), blocks_pg = mean(blocks),
            total_reb = sum(rebounds_total),rebounds_pg = mean(rebounds_total),
            total_def_ref = sum(rebounds_defensive), def_reb_pg = mean(rebounds_defensive),
            total_off_reb = sum(rebounds_offensive), off_reb_pg = mean(rebounds_offensive),
            efg = sum(field_goals_made + .5 * three_pointers_made) / sum(field_goals_attempted),
            total_games = n()) %>%
  arrange(desc(total_points)) %>%
  top_n(total_points,n = )

season.stat.prelim %>% arrange(desc(efg))
sum(is.na(season.stat.prelim))


# Get rid of people with ridiculously small number of points
season.stat.point = season.stat.prelim %>% filter(total_points > 400)

# THey should play in at least 70 games I think
season.stat.game = season.stat.point %>% filter(total_games > 50)

season.stat.game %>% filter(points_pg > 8) %>% arrange(sd_points)

# What criteria should we use?
season.stat %>% filter(total_points > 1000) %>% tail()

season.stat %>% filter(total_fg > 100)

# Sorting by variance
season.stat %>% arrange(sd_points)

season.stat.game %>% arrange(desc(total_reb))
season.stat.game %>% arrange(desc(blocks_pg))

#########################
# Exploratory
james.harden = nba.15.16 %>% filter(player_name %in% )
# HIstogram
ggplot(james.harden, aes(x = points)) +
  geom_histogram(bins = 20)
# Traceplot
ggplot(james.harden,aes(x = 1:length(points), y = points)) +
  geom_line()
conf.level = .95
ciline = qnorm((1 - conf.level)/2)/sqrt(dim(james.harden)[1])
bacf = acf(james.harden$points)
bacfdf = with(bacf, data.frame(x = lag, y = acf))

ggplot(data = bacfdf, aes(x = x, y = y)) +
  geom_bar(stat = "identity")

jh.pacf = pacf(james.harden$points)
pacf.df = with(jh.pacf, data.frame(x = lag, y = pacf))

#########################################

# Some things to consider?
#Making an array by player that meet certain criteria.
# Hot hand might be seen in players.
# What is my definition of hot hand? Higher variance? Consistent scoring?
# It's all about how the games and shots relate to each other.  I don't have the shots.  
# I have the points.  I'm not sure where to go with this.  How well do the simple AR() models
# predict this stuff. I would predict not great.

###########################################################
top.100.log = nba.15.16 %>% filter(player_name %in% season.stat.prelim$player_name)

top.100.log.trim = top.100.log %>% select(player_name,field_goals_made, field_goals_attempted,three_pointers_made, 
                                          three_pointers_attempted, points, steals, assists, blocks, 
                                          time_played_total) %>%
  mutate(fgp = sum(field_goals_made) / sum(field_goals_attempted),
         f3gp = sum(three_pointers_made), sum(three_pointers_attempted),
         efg = sum(field_goals_made + .5 * three_pointers_made ) / sum(field_goals_attempted)
  )

top.100.log.trim %>% group_by(player_name) %>% count() %>% arrange(desc(n))


ggplot(top.100.log.trim, aes(y = points, col = player_name)) + geom_

player.names = unique(top.100.log.trim$player_name)

plot(0,xlab = "Game", ylab = "Points",xlim = c(0,82), ylim = c(0,50))
for(player in player.names){
  points(top.100.log.trim$points[top.100.log.trim$player_name == player], type  = "l", col = rgb(0,0,0,alpha = .2))
}

names(top.100.log)

##############
top.50.log = nba.15.16 %>% filter(player_name %in% season.stat.prelim$player_name)

top.50.log.trim = top.50.log %>% select(player_name,field_goals_made, field_goals_attempted,three_pointers_made, 
                                          three_pointers_attempted, points, steals, assists, blocks, 
                                          time_played_total) %>%
  mutate(fgp = sum(field_goals_made) / sum(field_goals_attempted),
         f3gp = sum(three_pointers_made), sum(three_pointers_attempted),
         efg = sum(field_goals_made + .5 * three_pointers_made ) / sum(field_goals_attempted),
         log.efg = log(efg / (1- efg))
  )

top.50.log.trim %>% group_by(player_name) %>% count() %>% arrange(desc(n))


ggplot(top.50.log.trim, aes(y = points, x = 1:82, group = player_name)) + geom_path()

player.names = unique(top.50.log.trim$player_name)

par(mar = c(1.8,1.8,1.3,1.3))
plot(-5,xlab = "Game", ylab = "Points",xlim = c(0,82), ylim = c(0,50))
i =1
for(player in player.names){
  points(top.50.log.trim$points[top.50.log.trim$player_name == player], type  = "l", col = rgb(i /82,0,0,alpha = .2), lwd = 2)
  i = i +1

}
###################
# Top 10
season.stat.prelim = nba.15.16 %>% select(player_name,points,assists,field_goals_made,field_goals_attempted,free_throws_made,free_throws_attempted,points,
                                          three_pointers_made,three_pointers_attempted,steals,blocks,plus_minus, rebounds_defensive,rebounds_offensive, rebounds_total) %>%
  group_by(player_name) %>%
  summarize(total_points = sum(points),sd_points = sd(points), points_pg = mean(points),total_assists = sum(assists),assists_pg = mean(assists),total_fgp = sum(field_goals_made) / sum(field_goals_attempted),
            total_fg = sum(field_goals_attempted), 
            total_3ptp = sum(three_pointers_made) /sum(three_pointers_attempted),total_3pt = sum(three_pointers_attempted),
            total_ftp = sum(free_throws_made) / sum(free_throws_attempted), total_ft = sum(free_throws_attempted),
            total_steals = sum(steals), steals_pg = mean(steals), total_blocks = sum(blocks), blocks_pg = mean(blocks),
            total_reb = sum(rebounds_total),rebounds_pg = mean(rebounds_total),
            total_def_ref = sum(rebounds_defensive), def_reb_pg = mean(rebounds_defensive),
            total_off_reb = sum(rebounds_offensive), off_reb_pg = mean(rebounds_offensive),
            efg = sum(field_goals_made + .5 * three_pointers_made) / sum(field_goals_attempted),
            total_games = n()) %>%
  arrange(desc(total_points)) %>%
  top_n(total_points, n = 10)


top.20.log = nba.15.16 %>% filter(player_name %in% season.stat.prelim$player_name)

top.20.log.trim = top.20.log %>% select(player_name,field_goals_made, field_goals_attempted,three_pointers_made, 
                                        three_pointers_attempted, points, steals, assists, blocks, 
                                        time_played_total) %>%
  mutate(efg = (field_goals_made + .5 * three_pointers_made) / field_goals_attempted,
         log.efg = log(efg / (1 - efg)))

top.20.log.trim %>% group_by(player_name) %>% count() %>% arrange(desc(n))


ggplot(top.20.log.trim, aes(y = points, x = 1:82, group = player_name)) + geom_path()

player.names = unique(top.20.log.trim$player_name)

par(mar = c(1.8,1.8,1.3,1.3))
plot(-5,xlab = "Game", ylab = "Points",xlim = c(0,82), ylim = c(0,50))
i =1
for(player in player.names){
  lines(1:length(top.20.log.trim$points[top.20.log.trim$player_name == player]),top.20.log.trim$points[top.20.log.trim$player_name == player], type  = "l", col = rgb(i /20,i/20,i/20,alpha = .2), lwd = 2)
  i = i +1
  
}

write.csv(top.20.log.trim, "game_log_top_20.csv")
write.csv(all_games, "game_log_15_16.csv")

# hot hand test
test.vec = sample(c(0,1),1000,replace =  TRUE)
mean(test.vec[which(test.vec == 1) + 1], na.rm = TRUE)

season.stat.prelim
