library(nbastatR)
library(tidyverse)
library(pscl)
library(car)
library(rvest)
library(gbm)
library(caret)
library(rsample)
library(data.table)

Sys.setenv("VROOM_CONNECTION_SIZE"=131072 * 2)
df_all_seasons <- data.frame()


for (year in (1980:2021)) {
  x <- bref_players_stats(seasons=year,tables = c("per_game",'advanced'))
  
  df_all_seasons <- rbind(df_all_seasons,x)
}

url <- 'https://www.basketball-reference.com/awards/all_league.html#awards_all_league'
pg <- read_html(url)
tb <- html_table(pg,fill = T)

df <- tb[[1]]

colnames(df)[5:9] <- c('x1','x2','x3','x4','x5')

df <- df[!apply(df == "", 1, all), ] %>%
  select(-c(Lg,Tm,Voting))

all_nba <- data.frame(a=df$Season,b=c(df$x1,df$x2,df$x3,df$x4,df$x5)) %>%
  mutate(unique = paste(a,b))

df_all_seasons <- df_all_seasons %>%
  select(-c(slugPosition,slugPlayerSeason,slugPlayerBREF,
            slugTeamBREF,isHOFPlayer,slugTeamsBREF,urlPlayerThumbnail,
            urlPlayerHeadshot,urlPlayerPhoto,urlPlayerActionPhoto,urlPlayerStats,
            countTeamsPlayerSeasonPerGame,countTeamsPlayerSeason,urlPlayerBREF)) %>%
  mutate(unique = paste(slugSeason,namePlayer))

ids <- c(all_nba$unique)

df_all_seasons$isALLNBA <- 0


for (i in 1:nrow(df_all_seasons)){
  
  x <- ifelse(grepl(df_all_seasons$unique[i],ids),1,0)
  
  df_all_seasons$isALLNBA[i] <- sum(x)
  
}

model_df <- df_all_seasons %>%
  select(-c(slugSeason,namePlayer,groupPosition,yearSeason,
            isSeasonCurrent,countGames,countGamesStarted,idPlayerNBA,
            unique))


set.seed(0)
model_split <- initial_split(model_df,prop = .70)

model_train <- training(model_split)
model_test <- testing(model_split)

model_fit_1 <- gbm(isALLNBA~.,
                   data = model_train,
                   verbose = T,
                   distribution = 'bernoulli',
                   shrinkage = .05,
                   interaction.depth = 3,
                   n.minobsinnode = 5,
                   n.trees = 50,
                   cv.folds = 10)

summary(model_fit_1)

print(model_fit_1)

predictions <- predict(model_fit_1,model_test,type = 'response')

predictions

threshold <- .5

class_pred <- c()
class_pred[predictions <= threshold] <- 0
class_pred[predictions >  threshold] <- 1

class_pred

model_test <- model_test %>%
  mutate(prediction = class_pred)

TP <- sum(model_test['isALLNBA']==1 & model_test['prediction']==1)
TN <- sum(model_test['isALLNBA']==0 & model_test['prediction']==0)
FN <- sum(model_test['isALLNBA']==1 & model_test['prediction']==0)
FP <- sum(model_test['isALLNBA']==0 & model_test['prediction']==1)

Accuracy <- (TP + TN)/nrow(model_test)
sensitivity <- TP/(TP + FN)
Precision <- TP/(TP + FP)
