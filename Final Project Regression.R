library(nbastatR)
library(tidyverse)
library(pscl)
library(car)

#double connection size so R is able to scrape properly
Sys.setenv("VROOM_CONNECTION_SIZE"=131072 * 2)

#create an empty data frame
df_all_seasons <- data.frame()

#loop through each season and get per_game and advanced player stats, add each season to df_all_seasons data frame
for (year in (1980:2021)) {
  x <- bref_players_stats(seasons=year,tables = c("per_game",'advanced'))
  
  df_all_seasons <- rbind(df_all_seasons,x)
}

#drop unecessary columns
df_all_seasons <- df_all_seasons %>%
  select(-c(slugPosition,slugPlayerSeason,slugPlayerBREF,
            slugTeamBREF,isHOFPlayer,slugTeamsBREF,urlPlayerThumbnail,
            urlPlayerHeadshot,urlPlayerPhoto,urlPlayerActionPhoto,urlPlayerStats,
            countTeamsPlayerSeasonPerGame,countTeamsPlayerSeason,urlPlayerBREF))

#create a data frame that has information on all MVP's
mvp <- bref_awards(awards = 'Most Valuable Player')

#create a unique ID in the mvp data frame that combines season and player name
mvp <- mvp %>%
  mutate(unique = paste(slugSeason,namePlayer))

#create the same unique ID in df_all_seasons data frame, if unique ID is the mvp data frame then assign a 1 to isMVP column, else 0
df_all_seasons <- df_all_seasons %>%
  mutate(unique = paste(slugSeason,namePlayer),
         isMVP = ifelse(unique%in%mvp$unique,1,0)) %>%
  select(-c(unique))

#create a data frame to build model upon, drop columns that will not be used as features
model_df <- df_all_seasons %>%
  select(-c(slugSeason,namePlayer,groupPosition,yearSeason,isSeasonCurrent,
            countGamesStarted,idPlayerNBA,ratioBPM,fgmPerGame,fgaPerGame,fg3aPerGame,
            fg2mPerGame,fg2aPerGame,trbPerGame,ratioWS,ratioBPM,minutesPerGame,pctFG,
            ftmPerGame,tovPerGame))

#run logistic regression to predict isMVP
model <- glm(isMVP~.,family=binomial(link = 'logit'),data = model_df)

#summarize the model
summary(model)

#get r-squared of the model
pR2(model)['McFadden']

#check multicollinearity of the model
vif(model)

train_df <- bref_players_stats(seasons = 2022,tables = c('per_game','advanced'))

