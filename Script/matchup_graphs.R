library(tidyverse)
library(hoopR)
library(rvest)
output = 'PATH'
#defense matchups
match =  nba_leagueseasonmatchups(league_id = '00', season = year_to_season(most_recent_nba_season() - 1))$SeasonMatchups
match$MATCHUP_TIME_SEC=as.numeric(match$MATCHUP_TIME_SEC)
match = match %>% 
  mutate_at(6:27, as.numeric)
match = match %>% 
  mutate(papp=(PLAYER_PTS+MATCHUP_AST)/PARTIAL_POSS,
         ppp = PLAYER_PTS/PARTIAL_POSS)
#add ranks
matchup_stats = match %>% 
  select(OFF_PLAYER_ID, OFF_PLAYER_NAME, DEF_PLAYER_ID, DEF_PLAYER_NAME, GP, PARTIAL_POSS, PLAYER_PTS, TEAM_PTS, MATCHUP_AST, MATCHUP_TOV, MATCHUP_BLK, MATCHUP_FTA, MATCHUP_FG_PCT, 
         MATCHUP_FG3_PCT, MATCHUP_FGA) %>% 
  mutate(MATCHUP_TS=PLAYER_PTS/(2*(MATCHUP_FGA+.44*MATCHUP_FTA))) %>% 
  filter(PARTIAL_POSS>=10) %>% # at least 20 possesions, totally arbitrary 
  mutate_at(7:12, funs(./PARTIAL_POSS*100)) %>%
  mutate(across(7:12, ~ rank(-.x), .names = "{.col}_rank")) %>% 
  relocate(order(colnames(.))) 
#add headshots
matchup_stats = matchup_stats %>% 
  mutate(off_head=nba_playerheadshot(player_id = OFF_PLAYER_ID),
         def_head=nba_playerheadshot(player_id = DEF_PLAYER_ID))
## add average points per posession and diff (from bref)
#scrape brf possesion data
library(stringi)
url = "https://www.basketball-reference.com/leagues/NBA_2024_per_poss.html"
page_html <- url %>%
  rvest::read_html()  
df=page_html %>%
  rvest::html_nodes("table") %>%
  .[[1]] %>%
  rvest::html_table(header = TRUE)
df$Player = gsub("`", "", iconv(df$Player, from = "UTF-8", , to='ASCII//TRANSLIT'))
## add breference TS data
library(stringi)
url_ts = "https://www.basketball-reference.com/leagues/NBA_2024_advanced.html"
page_html_ts <- url_ts %>%
  rvest::read_html()  
df_ts=page_html_ts %>%
  rvest::html_nodes("table") %>%
  .[[1]] %>%
  rvest::html_table(header = TRUE)
df_ts$Player = gsub("`", "", iconv(df_ts$Player, from = "UTF-8", , to='ASCII//TRANSLIT'))
df_ts = df_ts %>% select(Player, Tm, 'TS%')
#keep only the first duplicate of teams (which is total for players on multiple teams)
poss = df %>% 
  left_join(df_ts, by = c("Player", "Tm")) %>% 
  select(Player, Tm, PTS, AST, '3P', TOV, 'FG%', '3P%', FTA,  Pos, 'TS%') %>% 
  mutate(player=Player) %>% 
  mutate_at(c(3:9, 11), as.numeric) %>% 
  group_by(Player) %>% 
  slice(1)
#rename player names
poss$Player[poss$Player=="GG Jackson II"]="GG Jackson"
poss$Player[poss$Player=="Xavier Tillman Sr."]="Xavier Tillman"
poss$Player[poss$Player=="Kevin Knox"]="Kevin Knox II"
poss$Player[poss$Player=="Marcus Morris"]="Marcus Morris Sr."
poss$Player[poss$Player=="A.J. Green"]="AJ Green"
poss$Player[poss$Player=="Reggie Bullock"]="Reggie Bullock Jr."
poss$Player[poss$Player=="Matthew Hurt"]="Matt Hurt"

#join with matchup stats
matchup_stats = matchup_stats %>% 
  left_join(poss, by = c("OFF_PLAYER_NAME"="Player")) 
matchup_final=matchup_stats %>% 
  mutate('AST Diff'=MATCHUP_AST - AST, 'PTS Diff'=PLAYER_PTS-PTS, 'TS% Diff'=MATCHUP_FG_PCT-`FG%`,
         'FTA Diff' = MATCHUP_FTA-FTA, 'TOV Diff'=MATCHUP_TOV-TOV) %>% 
  rename('Matchup AST'='MATCHUP_AST', 'Avg AST'='AST', 'Matchup PTS'='PLAYER_PTS', 'Avg PTS'='PTS',
         'Matchup TS%'='MATCHUP_FG_PCT', 'Avg TS%'='TS%', 'Matchup FTA' = 'MATCHUP_FTA', 'Avg FTA'='FTA',
         'Matchup TOV'='MATCHUP_TOV', 'Avg TOV'='TOV') %>% 
  select(-(contains('rank')), -contains('BLK'), -contains('FG3'), -TEAM_PTS) %>% 
  select(DEF_PLAYER_NAME, OFF_PLAYER_NAME, GP, off_head, def_head, Tm, Pos, player, PARTIAL_POSS,
         contains('PTS'), contains('AST'), contains('TS%'), contains('FTA'), contains('TOV'))
write.csv(matchup_final, paste0(output,'matchup_final.csv'), row.names = FALSE)

#positional breakdown for About page
positional_stats=
  poss %>% 
  select(Player, Tm, D_pos=Pos) %>% 
  right_join(matchup_stats, by =c( "Player"="DEF_PLAYER_NAME") ) %>% 
  group_by(D_pos) %>% 
  mutate(MATCHUP_FGA=MATCHUP_FGA/PARTIAL_POSS*100) %>% 
  filter(D_pos %in% c("PG", "SG", "SF", "PF", "C")) %>% 
  summarise(mean_points=mean(PLAYER_PTS), mean_fga=mean(MATCHUP_FGA)) %>% 
  arrange(-mean_points)
write.csv(positional_stats, paste0(output, 'positional_stats.csv'), row.names = FALSE)
