library(tidyverse)
library(janitor)
library(lubridate)
library(RJSONIO)
library(jsonlite)
library(nhlscrape)
library(tidyr)
library(stringr)

date = Sys.Date()

x <- fromJSON("https://lscluster.hockeytech.com/feed/?feed=modulekit&view=statviewtype&type=topscorers&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&league_code=&season_id=71&first=0&limit=100000&sort=points&stat=all&order_direction=")
playoff_stats <- x[["SiteKit"]][["Statviewtype"]]


playoff_stats <- playoff_stats %>% 
  select(name, team_code, position, birthdate, games_played, 
         goals, assists, points, points_per_game, 
         plus_minus, power_play_goals, power_play_assists,
         power_play_points, faceoff_wins, faceoff_attempts,
         faceoff_pct, shots) %>% 
  rename(Player = name, Team = team_code, Pos = position, 
         Birthdate = birthdate, GP = games_played, G = goals, 
         A = assists, PTS = points,`PP/G` = points_per_game, 
         `+/-` = plus_minus,
         PPG = power_play_goals, PPA = power_play_assists,
         PPPTS = power_play_points, FOW = faceoff_wins, 
         FOA = faceoff_attempts, `FO%` = faceoff_pct, 
         Shots = shots)
playoff_stats$Shots <- as.numeric(playoff_stats$Shots)
playoff_stats$G <- as.numeric(playoff_stats$G)

playoff_stats <- playoff_stats %>% 
  mutate(`S%` = Shots / G)


PTBO_stats <- playoff_stats %>% 
  filter(Team == "PBO")

HAM_stats <- playoff_stats %>% 
  filter(Team == "HAM")

PTBO_stats_faceoffs <- PTBO_stats %>% 
  filter(FOA >= 1) %>% 
  select(Player, FOW, FOA, `FO%`)

HAM_stats_faceoffs <- HAM_stats %>% 
  filter(FOA >= 1) %>% 
  select(Player, FOW, FOA, `FO%`)

NB_stats <- playoff_stats %>% 
  filter(Team == "NB")

NB_stats_faceoffs <- NB_stats %>% 
  filter(FOA >= 1) %>% 
  select(Player, FOW, FOA, `FO%`)




