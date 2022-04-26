library(tidyverse)
library(janitor)
library(lubridate)
library(RJSONIO)
library(jsonlite)
library(nhlscrape)
library(tidyr)
library(stringr)

date = Sys.Date()

# link
url <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=statviewtype&type=topscorers&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&league_code=&season_id=70&first=0&limit=1000&sort=active&stat=all&order_direction="


# use jsonlite::fromJSON to handle NULL values
json_data <- jsonlite::fromJSON(url, simplifyDataFrame = T)


# create data frame
df <- json_data[["SiteKit"]][["Statviewtype"]] %>%
  select(rank, player_id:num_teams) %>% 
  select(-c(birthtown, birthprov, birthcntry,
            loose_ball_recoveries, caused_turnovers, turnovers,
            phonetic_name, last_years_club, suspension_games_remaining,
            suspension_indefinite)) %>%
  mutate(player_id = as.numeric(player_id)) %>%
  mutate(across(active:age, ~as.numeric(.))) %>% 
  mutate(across(rookie:jersey_number, ~as.numeric(.))) %>% 
  mutate(team_id = as.numeric(team_id)) %>% 
  mutate(across(games_played:faceoff_pct, ~as.numeric(.))) %>%
  mutate(across(shots_on:num_teams, ~as.numeric(.))) %>% 
  mutate(birthdate_year = stringr::str_split(birthdate_year,
                                             "\\'", simplify = TRUE, n = 2)[,2]) %>% 
  mutate(birthdate_year = as.numeric(birthdate_year)) %>% 
  mutate(birthdate_year = 2000 + birthdate_year)

# create data frame with columns required for tableau viz
df2 <- df %>% 
  select(Nname = "name",
         FName = "first_name",
         LName = "last_name",
         `Birth date` = "birthdate_year",
         Pos = "position",
         Team = "team_code",
         GP = "games_played",
         G = "goals",
         A = "assists",
         PTS = "points",
         `Pts/G` = "points_per_game",
         PPG = "power_play_goals",
         PPA = "power_play_assists",
         GWG = "game_winning_goals",
         ENG = "empty_net_goals",
         `S%` = "shooting_percentage",
         PIM = "penalty_minutes") %>% 
  mutate(`Goals_excluding_ENG` = G - ENG) %>% 
  mutate(`Pts_excluding_ENG` = PTS - ENG) 
  df2$Name <- str_c(df2$FName, ' ', df2$LName) 
  
  
  returning_players <- df %>% 
    #filter(birthdate_year == "2001") %>% 
    select(Nname = "name",
           FName = "first_name",
           LName = "last_name",
           `Birth date` = "birthdate_year",
           Pos = "position",
           Team = "team_code",
           GP = "games_played",
           G = "goals",
           A = "assists",
           PTS = "points",
           `Pts/G` = "points_per_game",
           PPG = "power_play_goals",
           PPA = "power_play_assists",
           GWG = "game_winning_goals",
           ENG = "empty_net_goals",
           `S%` = "shooting_percentage",
           PIM = "penalty_minutes")
    
    
    
    returning_team_data <- returning_players %>% 
    group_by(Team) %>%
    summarise(TP = sum(PTS), TG = sum(G))

  
  HAM <- returning_players %>% 
    filter(Team == "HAM")
    
  PBO <- returning_players %>% 
    filter(Team == "PBO")
  
  
  
  
  
  
    shoot_perc <- df2 %>% 
    filter(GP > 34) %>% 
    select(Name, Team, `S%`)
  
  NB_Shooting_Percent <- shoot_perc %>%
    filter(Team == "NB")
  
  Visitor_Shooting_Percent <- shoot_perc %>%
    filter(Team == "MISS")
  

NorthBay_Advanced <- df %>% 
  filter(team_name == "North Bay Battalion") %>% 
  select(name, team_code, position, rookie,
         jersey_number, birthdate, games_played,
         points, goals, empty_net_goals, assists, points_per_game,
         shots, shooting_percentage, penalty_minutes) %>% 
  rename(`pp/g` = points_per_game) %>% 
  rename(Name = name, Team = team_code, Position = position, Rookie = rookie,
         `Jersey` = jersey_number, `Birthdate` = birthdate,
         GP = games_played, PTS = points, G = goals, ENG = empty_net_goals, 
         A = assists, `PP/G` = `pp/g`, `Total shots` = shots,
         `Shooting %` = shooting_percentage, PIM = penalty_minutes)%>% 
  mutate(`Goals_excluding_ENG` = G - ENG) %>% 
  mutate(`Goals_Per_Game` = G / GP) %>% 
  mutate(`Assists_Per_Game` = A / GP)


NorthBay_Advanced$Rookie[NorthBay_Advanced$Rookie == 0] <- "NO"
NorthBay_Advanced$Rookie[NorthBay_Advanced$Rookie == 1] <- "YES"
NorthBay_Advanced$Goals_Per_Game <- round(NorthBay_Advanced$Goals_Per_Game ,digit=2)

#NorthBay_Advanced$Name <- gsub("(total)","", as.character(NorthBay_Advanced$Name))


write.csv(NorthBay_Advanced,
          file = "NB_Stats_GameDay.csv",
          row.names = FALSE)

# save file to csv
write_csv(df2,
          file = "Ohl_stats_2021_2022.csv")

Visitor_Advanced <- df %>% 
  filter(team_name == "Mississauga Steelheads") %>% 
  select(name, team_code, position, rookie,
         jersey_number, birthdate, games_played,
         points, goals, empty_net_goals, assists, points_per_game,
         shots, shooting_percentage, penalty_minutes) %>% 
  rename(`pp/g` = points_per_game) %>% 
  rename(Name = name, Team = team_code, Position = position, Rookie = rookie,
         `Jersey` = jersey_number, `Birthdate` = birthdate,
         GP = games_played, PTS = points, G = goals, ENG = empty_net_goals, 
         A = assists, `PP/G` = `pp/g`, `Total shots` = shots,
         `Shooting %` = shooting_percentage, PIM = penalty_minutes)%>% 
  mutate(`Goals_excluding_ENG` = G - ENG) %>% 
  mutate(`Goals_Per_Game` = G / GP) %>% 
  mutate(`Assists_Per_Game` = A / GP)
  
  
Visitor_Advanced$Rookie[Visitor_Advanced$Rookie == 0] <- "NO"
Visitor_Advanced$Rookie[Visitor_Advanced$Rookie == 1] <- "YES"

write.csv(Visitor_Advanced,
          file = "Visitor_Stats_GameDay.csv",
          row.names = FALSE)



url10 <- "https://lscluster.hockeytech.com/feed/?feed=modulekit&view=schedule&key=2976319eb44abe94&fmt=json&client_code=ohl&lang=en&season_id=70&team_id=&league_code=&fmt=json"

json_data10 <- jsonlite::fromJSON(url10, simplifyDataFrame = T)


df_schedule <- json_data10[["SiteKit"]][["Schedule"]]

NB_schedule <- df_schedule %>% 
  filter(home_team_name == "North Bay Battalion" | visiting_team_name == "North Bay Battalion") %>% 
  select(game_id, date_played, home_team_name, home_goal_count,
         visiting_team_name, visiting_goal_count)


url_transactions <- "https://lscluster.hockeytech.com/feed/index.php?feed=modulekit&key=2976319eb44abe94&site_id=2&client_code=ohl&lang=en&view=scorebar&numberofdaysahead=3&numberofdaysback=3&league_code=&fmt=json"

transactions <- jsonlite::fromJSON(url_transactions)

transactions <- transactions[["SiteKit"]][["Scorebar"]]










