library(nbastatR)
library(SportsAnalytics)
library(RCurl)

test = fetch_NBAPlayerStatistics(season = "14-15")

key = "f67e30ba45f199b0c9355d1319c8a9e2"

#install_github("stattleship/stattleship-r")

library(stattleshipR)
set_token(key)
options(stringsAsFactors = FALSE)

sport = "basketball"
league = "nba"
ep = "game_logs"

q_body = list(season_id = "nba-2015-2016")

## get the game logs
games = ss_get_result(sport = sport,
                      league = league,
                      ep = ep,
                      query= q_body,
                      version = 1,
                      verbose = TRUE,
                      walk = TRUE)

## bind the data together
g1 = lapply(games, function(x) x$game_logs)
all_games = do.call('rbind',g1)
all_games = rbindlist(g1)

## clean-up data, set zero-out na 
## for active player vars
i <- which(all_games$game_played == TRUE)
all_games <- all_games[i,]
all_games$field_goals_made[which(is.na(all_games$field_goals_made))] <- 0
all_games$assists[which(is.na(all_games$assists))] <- 0
all_games$rebounds_offensive[which(is.na(all_games$rebounds_offensive))] <- 0
all_games$field_goals_attempted[which(is.na(all_games$field_goals_attempted))] <- 0
all_games$turnovers[which(is.na(all_games$turnovers))] <- 0
all_games$points[which(is.na(all_games$points))] <- 0

## calculate offensive efficiency
all_games$oe <- (all_games$field_goals_made + all_games$assists) / (all_games$field_goals_attempted - all_games$rebounds_offensive + all_games$assists + all_games$turnovers)
## remove ineligible records
all_games <- all_games[-which(all_games$oe == Inf),]

## add player details
players <- lapply(games, function(x) x$players)
all_players <- do.call('rbind', players)
the_players <- all_players[match(all_games$player_id, all_players$id),]
all_games$player_name <- the_players[match(all_games$player_id, the_players$id),]$name
all_games$position <- the_players[match(all_games$player_id, the_players$id),]$position_abbreviation

## optional group by position
pos_grouped <- group_by(all_games, position)
OE_pos <- summarise(pos_grouped, meanOE = mean(oe, na.rm=TRUE))
# WRiting it for 2015-2016
write.csv(all_games, "game_log_15_16.csv")

###################################

sport = "basketball"
league = "nba"
ep = "game_logs"

q_body = list(season_id = "nba-2016-2017")

## get the game logs
games = ss_get_result(sport = sport,
                      league = league,
                      ep = ep,
                      query= q_body,
                      version = 1,
                      verbose = TRUE,
                      walk = TRUE)

## bind the data together
g1 = lapply(games, function(x) x$game_logs)
all_games_16_17 = do.call('rbind',g1)
all_games_16_17 = rbindlist(g1)

## clean-up data, set zero-out na 
## for active player vars
i <- which(all_games_16_17$game_played == TRUE)
all_games_16_17 <- all_games_16_17[i,]
all_games_16_17$field_goals_made[which(is.na(all_games_16_17$field_goals_made))] <- 0
all_games_16_17$assists[which(is.na(all_games_16_17$assists))] <- 0
all_games_16_17$rebounds_offensive[which(is.na(all_games_16_17$rebounds_offensive))] <- 0
all_games_16_17$field_goals_attempted[which(is.na(all_games_16_17$field_goals_attempted))] <- 0
all_games_16_17$turnovers[which(is.na(all_games_16_17$turnovers))] <- 0
all_games_16_17$points[which(is.na(all_games_16_17$points))] <- 0

## calculate offensive efficiency
all_games_16_17$oe <- (all_games_16_17$field_goals_made + all_games_16_17$assists) / (all_games_16_17$field_goals_attempted - all_games_16_17$rebounds_offensive + all_games_16_17$assists + all_games_16_17$turnovers)
## remove ineligible records
all_games_16_17 <- all_games_16_17[-which(all_games_16_17$oe == Inf),]

## add player details
players <- lapply(games, function(x) x$players)
all_players <- do.call('rbind', players)
the_players <- all_players[match(all_games_16_17$player_id, all_players$id),]
all_games_16_17$player_name <- the_players[match(all_games_16_17$player_id, the_players$id),]$name
all_games_16_17$position <- the_players[match(all_games_16_17$player_id, the_players$id),]$position_abbreviation

## optional group by position
pos_grouped <- group_by(all_games_16_17, position)
OE_pos <- summarise(pos_grouped, meanOE = mean(oe, na.rm=TRUE))

write.csv(all_games_16_17, "game_log_16_17.csv")

#############################
sport = "basketball"
league = "nba"
ep = "game_logs"

q_body = list(season_id = "nba-2016-2017")

## get the game logs
games = ss_get_result(sport = sport,
                      league = league,
                      ep = ep,
                      query= q_body,
                      version = 1,
                      verbose = TRUE,
                      walk = TRUE)

## bind the data together
g1 = lapply(games, function(x) x$game_logs)
all_games_14_15 = do.call('rbind',g1)
all_games_14_15 = rbindlist(g1)

## clean-up data, set zero-out na 
## for active player vars
i <- which(all_games_14_15$game_played == TRUE)
all_games_14_15 <- all_games_14_15[i,]
all_games_14_15$field_goals_made[which(is.na(all_games_14_15$field_goals_made))] <- 0
all_games_14_15$assists[which(is.na(all_games_14_15$assists))] <- 0
all_games_14_15$rebounds_offensive[which(is.na(all_games_14_15$rebounds_offensive))] <- 0
all_games_14_15$field_goals_attempted[which(is.na(all_games_14_15$field_goals_attempted))] <- 0
all_games_14_15$turnovers[which(is.na(all_games_14_15$turnovers))] <- 0
all_games_14_15$points[which(is.na(all_games_14_15$points))] <- 0

## calculate offensive efficiency
all_games_14_15$oe <- (all_games_14_15$field_goals_made + all_games_14_15$assists) / (all_games_14_15$field_goals_attempted - all_games_14_15$rebounds_offensive + all_games_14_15$assists + all_games_14_15$turnovers)
## remove ineligible records
all_games_14_15 <- all_games_14_15[-which(all_games_14_15$oe == Inf),]

## add player details
players <- lapply(games, function(x) x$players)
all_players <- do.call('rbind', players)
the_players <- all_players[match(all_games_14_15$player_id, all_players$id),]
all_games_14_15$player_name <- the_players[match(all_games_14_15$player_id, the_players$id),]$name
all_games_14_15$position <- the_players[match(all_games_14_15$player_id, the_players$id),]$position_abbreviation

## optional group by position
pos_grouped <- group_by(all_games_14_15, position)
OE_pos <- summarise(pos_grouped, meanOE = mean(oe, na.rm=TRUE))

write.csv(all_games_14_15, "game_log_16_17.csv")

########################################################
# Getting Game DAta
########################################################

library(jsonlite)
sport = "basketball"
league = "nba"
ep = "games"

q_body = list(season_id = "nba-2015-2016")

## get the games
games = ss_get_result(sport = sport,
                      league = league,
                      ep = ep,
                      query= q_body,
                      version = 1,
                      verbose = TRUE,
                      walk = TRUE)

gs <- lapply(games, function(x) x$games)
nba_games <- rbindlist(gs)

nba_games = nba_games %>% select(-official_ids) %>%
  mutate(date = as.Date(started_at))

write.csv(nba_games,"data/nba_games_2015_2016.csv"
