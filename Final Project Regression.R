#import necessary libraries

library(nbastatR)
library(tidyverse)
library(pscl)
library(car)
library(rvest)
library(gbm)
library(caret)
library(rsample)
library(data.table)

#increase connection size for optimal scraping speed

Sys.setenv("VROOM_CONNECTION_SIZE"=131072 * 2)

#create an empty data frame to store player data for all seasons

df_all_seasons <- data.frame()

#for each season between 1980-2021, scrape every player's per-game and advanced stats
#bind each season's resulting data frame to df_all_seasons

for (year in (1980:2021)) {
  x <- bref_players_stats(seasons=year,tables = c("per_game",'advanced'))
  
  df_all_seasons <- rbind(df_all_seasons,x)
}

#scrape all-nba teams for every season from basketball reference

url <- 'https://www.basketball-reference.com/awards/all_league.html#awards_all_league'
pg <- read_html(url)
tb <- html_table(pg,fill = T)

df <- tb[[1]]

colnames(df)[5:9] <- c('x1','x2','x3','x4','x5')

df <- df[!apply(df == "", 1, all), ] %>%
  select(-c(Lg,Tm,Voting))

all_nba <- data.frame(a=df$Season,b=c(df$x1,df$x2,df$x3,df$x4,df$x5)) %>%
  mutate(unique = paste(a,b))

#drop unnecessary columns and add a column "unique" that combines a player's name and season

df_all_seasons <- df_all_seasons %>%
  select(-c(slugPosition,slugPlayerSeason,slugPlayerBREF,
            slugTeamBREF,isHOFPlayer,slugTeamsBREF,urlPlayerThumbnail,
            urlPlayerHeadshot,urlPlayerPhoto,urlPlayerActionPhoto,urlPlayerStats,
            countTeamsPlayerSeasonPerGame,countTeamsPlayerSeason,urlPlayerBREF)) %>%
  mutate(unique = paste(slugSeason,namePlayer))

#save unique id's from all_nba data frame to a vector

ids <- c(all_nba$unique)

#add a column "isALLNBA" that will serve as the predictor for the analysis

df_all_seasons$isALLNBA <- 0

#for each row in df_all_seasons, search for the unique id in the vector ids, if it is present assign a 1 if not assign a 0. Sum the vector and either assign a 1 or 0 for that player in the isALLNBA column

for (i in 1:nrow(df_all_seasons)){
  
  x <- ifelse(grepl(df_all_seasons$unique[i],ids),1,0)
  
  df_all_seasons$isALLNBA[i] <- sum(x)
  
}

#create a data frame for modeling purposes that only has feature columns and the predictor column

model_df <- df_all_seasons %>%
  select(-c(slugSeason,namePlayer,groupPosition,yearSeason,
            isSeasonCurrent,countGames,countGamesStarted,idPlayerNBA,
            unique))


#split the model into training and testing

set.seed(0)
model_split <- initial_split(model_df,prop = .70)

model_train <- training(model_split)
model_test <- testing(model_split)

#run a boosted gradient machine with the following parameters

model_fit_1 <- gbm(isALLNBA~.,
                   data = model_train,
                   verbose = T,
                   distribution = 'bernoulli',
                   shrinkage = .05,
                   interaction.depth = 3,
                   n.minobsinnode = 5,
                   n.trees = 50,
                   cv.folds = 10)

#print a summary of the model results

summary(model_fit_1)

print(model_fit_1)

#run the model on the test data set and save the predicted values 

predictions <- predict(model_fit_1,model_test,type = 'response')

#set a threshold of 50%

threshold <- .5

#classify each prediction as a 1 or 0 based on the threshold

class_pred <- c()
class_pred[predictions <= threshold] <- 0
class_pred[predictions >  threshold] <- 1

#save the predictions to the model_test data frame

model_test <- model_test %>%
  mutate(prediction = class_pred)

#calculate the confusion matrix

TP <- sum(model_test['isALLNBA']==1 & model_test['prediction']==1)
TN <- sum(model_test['isALLNBA']==0 & model_test['prediction']==0)
FN <- sum(model_test['isALLNBA']==1 & model_test['prediction']==0)
FP <- sum(model_test['isALLNBA']==0 & model_test['prediction']==1)

#print summary statistics

Accuracy <- (TP + TN)/nrow(model_test)
sensitivity <- TP/(TP + FN)
Precision <- TP/(TP + FP)
