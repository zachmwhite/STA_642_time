########################
# Exploratory analysis
########################
library(magrittr)
library(tidyverse)
library(reshape)
library(stringr)

########################
# Reading in the data
nba.15.16 = read.csv("C:/Users/Zachary/Desktop/Fall_2017_Projects/STA_642/STA_642_time/data/game_log_15_16.csv", header =TRUE, stringsAsFactors = FALSE)
nba_games = read.csv("data/nba_games_2015_2016.csv", header = TRUE, stringsAsFactors = FALSE)
nba_game_date = nba_games %>% select(id,date) %>%
  mutate(game_id = id) %>%
  select(-id)

nba.15.16 = merge(nba.15.16,nba_game_date,all.x = TRUE) %>% 
  arrange(date)

teams = unique(nba.15.16$team_id)

i = 1
for(team in teams){
  j = 1
  sorted.dates= unique(sort(nba.15.16$date[nba.15.16$team_id == team]))
  for(date in sorted.dates){
    nba.15.16$game_num[nba.15.16$date == date & nba.15.16$team_id == team] = which(sorted.dates == date)
  }
}

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
  arrange(desc(total_points))


# Actally relevant and writing data
# Top 20
season.stat.game.20 = season.stat.prelim %>%
  top_n(total_points, n = 20)

top.20.game.log = nba.15.16 %>% filter(player_name %in% season.stat.game.20$player_name)

top.20.game.trim = top.20.game.log %>% select(player_name,field_goals_made, field_goals_attempted,three_pointers_made, 
                                              three_pointers_attempted, points, steals, assists, blocks, 
                                              time_played_total,game_num) %>%
  mutate(efg = (field_goals_made + .5 * three_pointers_made) / field_goals_attempted,
         efg = ifelse(efg > 1,1,efg),
         logit.efg = ifelse(efg == 1,log(max(efg[efg<1]) / (1 - max(efg[efg<1]))),
                            ifelse(efg == 0,log(min(efg[efg>0]) / (1 - min(efg[efg>0]))),
                                   log(efg / (1 - efg))))
  )

write.csv(top.20.game.trim, "data/top_20_game_log.csv")

save(top.20.game.trim, file  = "data/top_20_game.Rdata")



top.20.points.plot = ggplot(top.20.game.trim,aes(x = game_num, y = points, color = player_name, group = player_name)) +
  geom_line(size = 1) +
  guides(color = FALSE,group = FALSE) +
  labs(
       x = "Game Number",
       y = "Points") + 
  theme_bw()

png(file="plots/top_20_points.png",width=800,height=350)
top.20.points.plot
dev.off()

top.20.efg.plot = ggplot(top.20.game.trim,aes(x = game_num, y = efg, color = player_name, group = player_name)) +
  geom_line(size = 1) +
  guides(color = FALSE, group = FALSE) +
  labs(x = "Game Number",
       y = "EFG") +
  theme_bw()
png(file = "plots/top_20_efg.png", width = 800, height = 350)
top.20.efg.plot
dev.off()

top.20.game.logit.efg.plot = ggplot(top.20.game.trim,aes(x = game_num, y = logit.efg, color = player_name, group = player_name)) +
  geom_line(size = 1) +
  guides(color = FALSE,group = FALSE) +
  labs(
       x = "Game Number",
       y = "logit(efg)") +
  theme_bw()

png(file = "plots/top_20_logit_efg.png", width = 800, height = 350)
top.20.game.logit.efg.plot
dev.off()

jh.df = top.20.game.trim %>% filter(player_name == "James Harden")

jh.points.plot = ggplot(jh.df, aes(x = game_num,y = points)) +
  geom_line(size = 1) +
  labs(main = "James Harden Points per Game",
       x  = "Game Number",
       y = "Points") + 
  theme_bw()
png(file = "plots/jh_points_plot.png", , width = 800, height = 350)
jh.points.plot
dev.off()
jh.efg.plot
jh.logit.efg.plot










############################

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
# Making an array by player that meet certain criteria.
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
                                        time_played_total,game_) %>%
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

# hot hand test
test.vec = sample(c(0,1),1000,replace =  TRUE)
mean(test.vec[which(test.vec == 1) + 1], na.rm = TRUE)

season.stat.prelim
