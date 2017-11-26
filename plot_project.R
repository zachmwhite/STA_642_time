library(magrittr)
library(tidyverse)
library(reshape)
library(stringr)

load("data/plot_project.Rdata")

# Overall Points
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

# Isolating James Harden for demonstration

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

jh.efg.plot = ggplot(jh.df, aes(x = game_num, y = efg)) +
  geom_line(size = 1) +
  labs(main = "James Harden EFG per Game",
       x = "Game Number",
       y = "EFG") +
  theme_bw()
jh.logit.efg.plot
