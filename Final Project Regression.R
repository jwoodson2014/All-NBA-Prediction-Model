library(nbastatR)
library(tidyverse)
library(pscl)
library(car)

Sys.setenv("VROOM_CONNECTION_SIZE"=131072 * 2)
df_all_seasons <- data.frame()


for (year in (1980:2021)) {
  x <- bref_players_stats(seasons=year,tables = c("per_game",'advanced'))
  
  df_all_seasons <- rbind(df_all_seasons,x)
}

df_all_seasons <- df_all_seasons %>%
  select(-c(slugPosition,slugPlayerSeason,slugPlayerBREF,
            slugTeamBREF,isHOFPlayer,slugTeamsBREF,urlPlayerThumbnail,
            urlPlayerHeadshot,urlPlayerPhoto,urlPlayerActionPhoto,urlPlayerStats,
            countTeamsPlayerSeasonPerGame,countTeamsPlayerSeason,urlPlayerBREF))

mvp <- bref_awards(awards = 'Most Valuable Player')

mvp <- mvp %>%
  mutate(unique = paste(slugSeason,namePlayer))

df_all_seasons <- df_all_seasons %>%
  mutate(unique = paste(slugSeason,namePlayer),
         isMVP = ifelse(unique%in%mvp$unique,1,0)) %>%
  select(-c(unique))


model_df <- df_all_seasons %>%
  select(-c(slugSeason,namePlayer,groupPosition,yearSeason,isSeasonCurrent,
            countGamesStarted,idPlayerNBA,ratioBPM,fgmPerGame,fgaPerGame,fg3aPerGame,
            fg2mPerGame,fg2aPerGame,trbPerGame,ratioWS,ratioBPM,minutesPerGame,pctFG,
            ftmPerGame,tovPerGame))

model <- glm(isMVP~.,family=binomial(link = 'logit'),data = model_df)

summary(model)

pR2(model)['McFadden']

vif(model)

?glm

train_df <- bref_players_stats(seasons = 2022,tables = c('per_game','advanced'))

