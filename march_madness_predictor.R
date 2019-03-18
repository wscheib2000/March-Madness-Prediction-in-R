#Imports
library(rvest)

#Constants
pred_year <- 2019
pred_matchups_names <- c('year', 'team', 'opponent', 'winner', 'pred_point_diff')
temp_pred_matchups_names <- c('team', 'opponent')
data_names <- c('year', 'team', 'opponent', 'point_diff',
                     'team1_adj_eff_margin', 'team1_adj_off_eff', 'team1_adj_def_eff', 'team1_pos_per_40', 'team1_luck', 'team1_sos', 
                     'team1_opp_adj_off_eff', 'team1_opp_adj_def_eff', 
                     'team2_adj_eff_margin', 'team2_adj_off_eff', 'team2_adj_def_eff', 'team2_pos_per_40', 'team2_luck', 'team2_sos', 
                     'team2_opp_adj_off_eff', 'team2_opp_adj_def_eff')
pred_data_names <- c('year', 'team', 'opponent', 'winner', 'pred_point_diff',
                'team1_adj_eff_margin', 'team1_adj_off_eff', 'team1_adj_def_eff', 'team1_pos_per_40', 'team1_luck', 'team1_sos', 
                'team1_opp_adj_off_eff', 'team1_opp_adj_def_eff', 
                'team2_adj_eff_margin', 'team2_adj_off_eff', 'team2_adj_def_eff', 'team2_pos_per_40', 'team2_luck', 'team2_sos', 
                'team2_opp_adj_off_eff', 'team2_opp_adj_def_eff')
pred_chunks <- list(c(1, 2, 11, 12), c(3:10, 13:36), c(37:52), c(53:60), c(61:64), c(65:66), 67)

#Sets working directory
setwd('C:/Users/student/Documents/March-Madness-Prediction-in-R')

#Reads in data from CSV
team_data <- read.csv('team_data.csv', stringsAsFactors = FALSE)
matchups <- read.csv('matchups.csv', stringsAsFactors = FALSE)



###Creates data.frame with past results and stats###

#Merges data by team and year columns, drops all data for teams that did not make the tournament, and fixes names
data <- merge(x = matchups, y = team_data, by.x = c('year', 'opponent'), by.y = c('year', 'team'), all.x = TRUE)
data <- merge(x = data, y = team_data, by = c('year', 'team'), all.x = TRUE)
names(data) <- data_names

#Creates ratio stats
data$ratio_adj_eff_margin <- (data$team1_adj_eff_margin / data$team2_adj_eff_margin)
data$ratio_team1_adj_off_eff <- (data$team1_adj_off_eff / data$team2_opp_adj_off_eff)
data$ratio_team2_adj_off_eff <- (data$team2_adj_off_eff / data$team1_opp_adj_off_eff)
data$ratio_team1_adj_def_eff <- (data$team1_adj_def_eff / data$team2_opp_adj_def_eff)
data$ratio_team2_adj_def_eff <- (data$team2_adj_def_eff / data$team1_opp_adj_def_eff)
data$ratio_pos_per_40 <- data$team1_pos_per_40 / data$team2_pos_per_40



###Scrapes matchups to predict###

#Builds link and gets html code
link <- paste0('https://www.sports-reference.com/cbb/postseason/', pred_year, '-ncaa.html')
download.file(link, destfile = 'scraped_page.html', quiet=TRUE)
webpage <- read_html('scraped_page.html')

#Reads first round pred_matchups and their scores, converts them to text, and converts the resultant character vector to a data.frame
temp_data_html <- html_nodes(webpage, 'em , #east a , #west a , #midwest a , #south a')
temp_data <- html_text(temp_data_html)
temp_data <- data.frame(temp_data, stringsAsFactors = FALSE)

#Rearranges values from 1 column to 2 columns and adds names
temp_data <- data.frame(temp_data[temp_data$temp_data != "",], stringsAsFactors = FALSE)
for(i in c(0:(nrow(temp_data) - 1))){
  temp_data[(i %/% 2) + 1, (i %% 2) + 1] <- temp_data[i + 1, 1]
}
names(temp_data) <- temp_pred_matchups_names

#Adds year column, calculates point_diff colum, removes score columns, rearranges columns, and removes excess rows
temp_data <- temp_data[complete.cases(temp_data),]
temp_data$year <- pred_year
temp_data <- temp_data[, c(3, 1, 2)]

#Standardizes team names and records sports-reference names
for(i in c(1:nrow(temp_data))){
  for(j in c(2,3)){
    temp_data[i, j] <- sub('-', ' ', temp_data[i, j])
    temp_data[i, j] <- sub('State', 'St.', temp_data[i, j])
    temp_data[i, j] <- sub('NC St.', 'North Carolina St.', temp_data[i, j])
    temp_data[i, j] <- sub('UNC$', 'North Carolina', temp_data[i, j])
    temp_data[i, j] <- sub('Alabama Birmingham$', 'UAB', temp_data[i, j])
    temp_data[i, j] <- sub(' \\(NY\\)', '', temp_data[i, j])
    temp_data[i, j] <- sub('ETSU', 'East Tennessee St.', temp_data[i, j])
    temp_data[i, j] <- sub('^Little Rock', 'Arkansas Little Rock', temp_data[i, j])
    temp_data[i, j] <- sub('Louisiana', 'Louisiana Lafayette', temp_data[i, j])
    temp_data[i, j] <- sub('\\(', '', temp_data[i, j])
    temp_data[i, j] <- sub('\\)', '', temp_data[i, j])
    temp_data[i, j] <- sub('Loyola IL', 'Loyola Chicago', temp_data[i, j])
    temp_data[i, j] <- sub('Ole Miss', 'Mississippi', temp_data[i, j])
    temp_data[i, j] <- sub('Pitt', 'Pittsburgh', temp_data[i, j])
    temp_data[i, j] <- sub('Prairie View', 'Prairie View A&M', temp_data[i, j])
    temp_data[i, j] <- sub('Southeastern Louisiana Lafayette', 'Southeastern Louisiana', temp_data[i, j])
    temp_data[i, j] <- sub('St. Joseph', 'Saint Joseph', temp_data[i, j])
    temp_data[i, j] <- sub('St. Peter', 'Saint Peter', temp_data[i, j])
    temp_data[i, j] <- sub('Texas Arlington', 'UT Arlington', temp_data[i, j])
    temp_data[i, j] <- sub('Texas A&M Corpus Christi', 'Texas A&M Corpus Chris', temp_data[i, j])
    temp_data[i, j] <- sub('UConn', 'Connecticut', temp_data[i, j])
    temp_data[i, j] <- sub('UCSB', 'UC Santa Barbara', temp_data[i, j])
    temp_data[i, j] <- sub('UIC', 'Illinois Chicago', temp_data[i, j])
    temp_data[i, j] <- sub('UMass', 'Massachusetts', temp_data[i, j])
  }
}

#Renames data
pred_matchups <- temp_data



###Creates model###

#Identifies training set
train_set <- data

#Creates models
fit1 <- lm(point_diff ~ team1_adj_eff_margin + team2_adj_eff_margin + team1_adj_def_eff + team2_sos + ratio_adj_eff_margin, 
           data = train_set)
fit2 <- lm(point_diff ~ team1_adj_off_eff + team1_adj_def_eff + team1_sos + team1_opp_adj_off_eff + team1_opp_adj_def_eff + 
             team2_adj_eff_margin + team2_sos + ratio_adj_eff_margin, data = train_set)

#Creates two data.frames to track both matchups predictions and two vectors to track winners, pre-allocating data storage for efficiency
a <- numeric(67)
b <- character(67)
c <- character(67)
d <- character(67)
e <- numeric(67)
for(i in 1:67){
  a[i] <- 2019
  b[i] <- toString(i)
  c[i] <- toString(i)
  d[i] <- toString(i)
  e[i] <- i
}
pred_matchups_fit_1 <- data.frame(a, b, c, d, e, stringsAsFactors = FALSE)
names(pred_matchups_fit_1) <- pred_matchups_names
pred_matchups_fit_1[1:nrow(pred_matchups), 1:3] <- pred_matchups
pred_matchups_fit_2 <- data.frame(a, b, c, d, e, stringsAsFactors = FALSE)
names(pred_matchups_fit_2) <- pred_matchups_names
pred_matchups_fit_2[1:nrow(pred_matchups), 1:3] <- pred_matchups
pred_matchups_fit_1_end <- nrow(pred_matchups) + 1
pred_matchups_fit_2_end <- nrow(pred_matchups) + 1
winners_fit_1 <- vector(length = 67)
winners_fit_2 <- vector(length = 67)
winners_fit_1_end <- 1
winners_fit_2_end <- 1



###Does predictions###
for(pred_chunk in pred_chunks) {
  #Selects appropriate rows of data.frame
  predictions_fit_1 <- pred_matchups_fit_1[pred_chunk,]
  predictions_fit_2 <- pred_matchups_fit_2[pred_chunk,]
  
  #Merges pred_data by team and year columns, drops all pred_data for teams that did not make the tournament, and fixes names
  pred_data_fit_1 <- merge(x = predictions_fit_1, y = team_data, by.x = c('year', 'opponent'), by.y = c('year', 'team'), all.x = TRUE)
  pred_data_fit_1 <- merge(x = pred_data_fit_1, y = team_data, by = c('year', 'team'), all.x = TRUE)
  names(pred_data_fit_1) <- pred_data_names
  pred_data_fit_2 <- merge(x = predictions_fit_2, y = team_data, by.x = c('year', 'opponent'), by.y = c('year', 'team'), all.x = TRUE)
  pred_data_fit_2 <- merge(x = pred_data_fit_2, y = team_data, by = c('year', 'team'), all.x = TRUE)
  names(pred_data_fit_2) <- pred_data_names
  
  #Creates ratio stats
  pred_data_fit_1$ratio_adj_eff_margin <- (pred_data_fit_1$team1_adj_eff_margin / pred_data_fit_1$team2_adj_eff_margin)
  pred_data_fit_1$ratio_team1_adj_off_eff <- (pred_data_fit_1$team1_adj_off_eff / pred_data_fit_1$team2_opp_adj_off_eff)
  pred_data_fit_1$ratio_team2_adj_off_eff <- (pred_data_fit_1$team2_adj_off_eff / pred_data_fit_1$team1_opp_adj_off_eff)
  pred_data_fit_1$ratio_team1_adj_def_eff <- (pred_data_fit_1$team1_adj_def_eff / pred_data_fit_1$team2_opp_adj_def_eff)
  pred_data_fit_1$ratio_team2_adj_def_eff <- (pred_data_fit_1$team2_adj_def_eff / pred_data_fit_1$team1_opp_adj_def_eff)
  pred_data_fit_1$ratio_pos_per_40 <- pred_data_fit_1$team1_pos_per_40 / pred_data_fit_1$team2_pos_per_40
  pred_data_fit_2$ratio_adj_eff_margin <- (pred_data_fit_2$team1_adj_eff_margin / pred_data_fit_2$team2_adj_eff_margin)
  pred_data_fit_2$ratio_team1_adj_off_eff <- (pred_data_fit_2$team1_adj_off_eff / pred_data_fit_2$team2_opp_adj_off_eff)
  pred_data_fit_2$ratio_team2_adj_off_eff <- (pred_data_fit_2$team2_adj_off_eff / pred_data_fit_2$team1_opp_adj_off_eff)
  pred_data_fit_2$ratio_team1_adj_def_eff <- (pred_data_fit_2$team1_adj_def_eff / pred_data_fit_2$team2_opp_adj_def_eff)
  pred_data_fit_2$ratio_team2_adj_def_eff <- (pred_data_fit_2$team2_adj_def_eff / pred_data_fit_2$team1_opp_adj_def_eff)
  pred_data_fit_2$ratio_pos_per_40 <- pred_data_fit_2$team1_pos_per_40 / pred_data_fit_2$team2_pos_per_40
  
  #Does predictions
  pred_data_fit_1$pred_point_diff <- predict(fit1, pred_data_fit_1)
  pred_data_fit_2$pred_point_diff <- predict(fit1, pred_data_fit_2)
  
  #Checks which team is predicted to win and adds them to the winner wolumn and winners vector
  pred_data_fit_1$winner <- toString(1)
  for(i in 1:nrow(pred_data_fit_1)) {
    if(pred_data_fit_1$pred_point_diff[i] > 0) {
      pred_data_fit_1$winner[i] <- pred_data_fit_1$team[i]
    } else if(pred_data_fit_1$pred_point_diff[i] < 0){
      pred_data_fit_1$winner[i] <- pred_data_fit_1$opponent[i]
    }
  }
  pred_data_fit_2$winner <- toString(1)
  for(i in 1:nrow(pred_data_fit_2)) {
    if(pred_data_fit_2$pred_point_diff[i] > 0) {
      pred_data_fit_2$winner[i] <- pred_data_fit_2$team[i]
    } else if(pred_data_fit_2$pred_point_diff[i] < 0){
      pred_data_fit_2$winner[i] <- pred_data_fit_2$opponent[i]
    }
  }
  
  winners_fit_1[winners_fit_1_end:(winners_fit_1_end + nrow(pred_data_fit_1) - 1)] <- pred_data_fit_1$winner
  winners_fit_1_end <- winners_fit_1_end + nrow(pred_data_fit_1)
  winners_fit_2[winners_fit_2_end:(winners_fit_2_end + nrow(pred_data_fit_2) - 1)] <- pred_data_fit_2$winner
  winners_fit_2_end <- winners_fit_2_end + nrow(pred_data_fit_2)
  
  #Adds winner to matchups data.frame and adds column to represent bracket organization
  pred_data_fit_1$bracket <- NA
  pred_data_fit_2$bracket <- NA
  for(win in pred_data_fit_1$winner) {
    pred_matchups_fit_1[pred_matchups_fit_1$team == win | pred_matchups_fit_1$opponent == win, 'winner'] <- win
    pred_data_fit_1[pred_data_fit_1$team == win | pred_data_fit_1$opponent == win, 'bracket'] <- which(pred_matchups$team == win | pred_matchups$opponent == win)
  }
  for(i in 1:nrow(pred_data_fit_1)) {
    pred_matchups_fit_1[pred_matchups_fit_1$team == pred_data_fit_1$team[i] & pred_matchups_fit_1$opponent == pred_data_fit_1$opponent[i],
                        'pred_point_diff'] <- pred_data_fit_1[pred_data_fit_1$winner == pred_data_fit_1$winner[i], 'pred_point_diff']
  }
  for(win in pred_data_fit_2$winner) {
    pred_matchups_fit_2[pred_matchups_fit_2$team == win | pred_matchups_fit_2$opponent == win, 'winner'] <- win
    pred_data_fit_2[pred_data_fit_1$team == win | pred_data_fit_1$opponent == win, 'bracket'] <- which(pred_matchups$team == win | pred_matchups$opponent == win)
  }
  for(i in 1:nrow(pred_data_fit_2)) {
    pred_matchups_fit_2[pred_matchups_fit_2$team == pred_data_fit_2$team[i] & pred_matchups_fit_2$opponent == pred_data_fit_2$opponent[i],
                        'pred_point_diff'] <- pred_data_fit_2[pred_data_fit_2$winner == pred_data_fit_2$winner[i], 'pred_point_diff']
  }
  
  #Does different things for the play-in games versus the rest to move the winners to their next game
  if(winners_fit_1_end == 5) {
    next_games <- c(3, 7, 13, 17)
    next_games_index_fit_1 <- 1
    next_games_index_fit_2 <- 1
    for(i in 1:36) {
      if(i %in% pred_data_fit_1$bracket) {
        pred_matchups_fit_1[next_games[next_games_index_fit_1], 'opponent'] <- pred_data_fit_1[pred_data_fit_1$bracket == i, 'winner']
        next_games_index_fit_1 <- next_games_index_fit_1 + 1
      }
      if(i %in% pred_data_fit_2$bracket) {
        pred_matchups_fit_2[next_games[next_games_index_fit_2], 'opponent'] <- pred_data_fit_2[pred_data_fit_2$bracket == i, 'winner']
        next_games_index_fit_2 <- next_games_index_fit_2 + 1
      }
    }
  } else if(winners_fit_1_end < 68) {
    new_pred_matchups <- data.frame(NA)
    j_fit_1 <- 0
    j_fit_2 <- 0
    for(i in 1:36) {
      if(i %in% pred_data_fit_1$bracket) {
        new_pred_matchups[(j_fit_1 %/% 2) + 1, (j_fit_1 %% 2) + 1] <- pred_data_fit_1[pred_data_fit_1$bracket == i, 'winner']
        j_fit_1 <- j_fit_1 + 1
      }
      if(i %in% pred_data_fit_2$bracket) {
        new_pred_matchups[(j_fit_2 %/% 2) + 1, (j_fit_2 %% 2) + 1] <- pred_data_fit_2[pred_data_fit_2$bracket == i, 'winner']
        j_fit_2 <- j_fit_2 + 1
      }
    }
    pred_matchups_fit_1[pred_matchups_fit_1_end:(pred_matchups_fit_1_end + nrow(new_pred_matchups) - 1), c("team", "opponent")] <- new_pred_matchups
    pred_matchups_fit_1_end <- pred_matchups_fit_1_end + nrow(new_pred_matchups)
    pred_matchups_fit_2[pred_matchups_fit_2_end:(pred_matchups_fit_2_end + nrow(new_pred_matchups) - 1), c("team", "opponent")] <- new_pred_matchups
    pred_matchups_fit_2_end <- pred_matchups_fit_2_end + nrow(new_pred_matchups)
  }
}



#Prints results in a pretty way
print(paste0("According to fit1, the champion will be: ", winners_fit_1[67]))
print(paste0("According to fit2, the champion will be: ", winners_fit_2[67]))

#Writes results to csv
write.csv(pred_matchups_fit_1, 'results_fit_1.csv', row.names = FALSE)
write.csv(pred_matchups_fit_2, 'results_fit_2.csv', row.names = FALSE)
