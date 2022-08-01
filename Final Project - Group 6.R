#import necessary libraries
# devtools::install_github("abresler/nbastatR")
# install.packages('tidyverse')
# install.packages('pscl')
# install.packages('car')
# install.packages('rvest')
# install.packages('gbm')
# install.packages('caret')
# install.packages('rsample')
# install.packages('data.table')
# install.packages('fastDummies')
# install.packages('gt')


library(nbastatR)
library(tidyverse)
library(pscl)
library(car)
library(rvest)
library(gbm)
library(caret)
library(rsample)
library(data.table)
library(fastDummies)
library(gt)
webshot::install_phantomjs()

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
  select(-c(slugSeason,namePlayer,yearSeason,
            isSeasonCurrent,countGames,countGamesStarted,idPlayerNBA,
            unique)) %>%
  mutate(dummy_cols(groupPosition)) %>%
  select(-c(groupPosition,".data"))


#split the model into training and testing
set.seed(0)
model_split <- initial_split(model_df,prop = .70)

model_train <- training(model_split)
model_test <- testing(model_split)

df_parameters <- data.frame()
threshold <- .50

for (treesize in (5:25)) {
  
  treesize <- treesize * 10
  
  model_fit_1<- gbm(isALLNBA~.,
                    data = model_train,
                    verbose = T,
                    distribution = 'bernoulli',
                    shrinkage = .05,
                    interaction.depth = 3,
                    n.minobsinnode = 5,
                    n.trees = treesize,
                    cv.folds = 10)  
  
  model_test$predictions <- predict(model_fit_1,model_test,type = 'response')
  
  #classify each prediction as a 1 or 0 based on the threshold
  class_pred <- c()
  class_pred[model_test$predictions <= threshold] <- 0
  class_pred[model_test$predictions >  threshold] <- 1
  
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
  
  df_parameters[nrow(df_parameters)+1,"Tree Size"] <- treesize
  df_parameters[nrow(df_parameters),"Accuracy"] <- Accuracy
  df_parameters[nrow(df_parameters),"Precision"] <- Precision
  df_parameters[nrow(df_parameters),"Sensitivity"] <- sensitivity
}

#scrape every player's per-game and advanced stats for the most recent season
Season2022 <- bref_players_stats(seasons=2022,tables = c("per_game",'advanced')) %>% 
  select(-c(slugPosition,slugPlayerSeason,slugPlayerBREF,
            slugTeamBREF,isHOFPlayer,slugTeamsBREF,urlPlayerThumbnail,
            urlPlayerHeadshot,urlPlayerPhoto,urlPlayerActionPhoto,urlPlayerStats,
            countTeamsPlayerSeasonPerGame,countTeamsPlayerSeason,urlPlayerBREF,slugSeason,yearSeason,
            isSeasonCurrent,countGames,countGamesStarted,idPlayerNBA)) %>%
  mutate(dummy_cols(groupPosition)) %>%
  select(-c(groupPosition,".data"))

#create model based on optimal model found during cross-validation process
model_fit_1<- gbm(isALLNBA~.,
                  data = model_train,
                  verbose = T,
                  distribution = 'bernoulli',
                  shrinkage = .05,
                  interaction.depth = 3,
                  n.minobsinnode = 5,
                  n.trees = 120,
                  cv.folds = 10)

#save predictions to Season2022
Season2022$predictions <- predict(model_fit_1,Season2022,type = 'response')

#create AverageRelativeInfluence graphic
df_all_seasons %>%
  filter(isALLNBA==1) %>%
  mutate(Years = cut(yearSeason,
                     c(1980,1985,1990,1995,2000,2005,2010,2015,2020,2022),
                     right = F,
                     labels = c('1980-1984','1985-1989','1990-1994','1995-1999',
                                '2000-2004','2005-2009','2010-2014','2015-2019','2020-'))) %>%
  group_by(Years) %>%
  summarise(VORP = mean(ratioVORP),
            WS = mean(ratioWS),
            PER = mean(ratioPER),
            DWS = mean(ratioDWS),
            PTS = mean(ptsPerGame)) %>%
  mutate_if(is.numeric,~round(.,2)) %>%
  gt() %>%
  data_color(columns = c(VORP,WS,PER,DWS,PTS), 
    colors = scales::col_numeric(
      palette = 'Blues',
      domain = NULL)) %>%
  tab_header(title = 'Average Relative Variable Influence by Season',
             subtitle = 'Top 5 variables in terms of relative influence as determined by the gradient boosted machine group in 5 year intervals for the last 40 seasons') %>%
  cols_width(everything() ~ px(150)) %>%
  gtsave(.,'AverageRelativeInfluence.png',expand = 10)

#create GBMResults graphic
df_parameters %>%
  mutate(Accuracy = round(Accuracy,3),
         Precision = round(Precision,3),
         Sensitivity = round(Sensitivity,3)) %>%
  gt() %>%
  tab_header(title = 'Gradient Boosted Machine Results',
             subtitle = 'Accuracy, Precision, and Sensitivity for each tree size') %>%
  cols_width(everything() ~ px(150)) %>%
  data_color(columns = c(Accuracy,Precision,Sensitivity), 
             colors = scales::col_numeric(
               palette = 'Blues',
               domain = NULL)) %>%
  gtsave(.,'GBMResults.png')

#find the top 3 centers predicted to make All-NBA in the most recent season
centers <- Season2022 %>%
  arrange(.,desc(predictions)) %>%
  filter(.data_C == 1) %>%
  slice(1:3)

#find the top 5 forwards predicted to make All-NBA in the most recent season
forwards <- Season2022 %>%
  arrange(.,desc(predictions)) %>%
  filter(.data_F == 1) %>%
  slice(1:6)

#find the top 5 guards predicted to make All-NBA in the most recent season
guards <- Season2022 %>%
  arrange(.,desc(predictions)) %>%
  filter(.data_G == 1) %>%
  slice(1:6)

#create 2022Results graphic
All_NBA_2022 <- rbind(centers,forwards,guards) %>%
  select(c(namePlayer,minutesPerGame,ptsPerGame,trbPerGame,astPerGame,pctUSG, ratioWS,ratioDWS,ratioVORP,predictions)) %>%
  arrange(.,desc(predictions)) %>%
  rename(Player = namePlayer,
         MPG = minutesPerGame,
         PPG = ptsPerGame,
         RPG = trbPerGame,
         APG = astPerGame,
         USG = pctUSG,
         WS = ratioWS,
         DWS = ratioDWS,
         VORP= ratioVORP,
         Prediction = predictions) %>%
  gt() %>%
  tab_header(title = 'Prediciting 2022 All-NBA Players',
             subtitle = 'Players that our model predicts will be named to 1 of the 3 All-NBA teams in 2022') %>%
  cols_width(everything() ~ px(85)) %>%
  cols_width(Player ~ px(200)) %>%
  tab_style(
    style = cell_borders(
      sides = 'bottom',
      color = "black",
      weight = px(5),
      style = "solid"
    ),
    locations = cells_body(
      rows = 9)) %>%
  gtsave(.,'2022Results.png',expand = 10)

#create VRI graphic
VRI <- as.data.frame(summary(model_fit_1)) %>%
  slice(1:14) %>% 
  ggplot(aes(x = reorder(var,rel.inf),y =rel.inf))+
  geom_bar(stat = 'identity',fill = '#0099f9')+
  xlab('Statistic')+
  ylab('Relative Variable Influence')+
  labs(title = 'Relative Variable Influence',
       subtitle = 'Relative variable influence for variables with a RVI greater than 1')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5))+
  coord_flip()

ggsave('RVI.png',VRI,height = 5,width = 10)

#create VORP graphic
df_all_seasons %>%
  ggplot(aes(x=isALLNBA,y=ratioVORP,group = isALLNBA))+
  geom_boxplot()+
  xlab('All-NBA (Y/N)')+
  ylab('VORP')