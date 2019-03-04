#Sets working directory
setwd('C:/Users/student/Documents/RProjects/March Madness')

#Reads in data from CSV
team_data <- read.csv('team_data.csv', stringsAsFactors = FALSE)
first_round_matchups <- read.csv('first_round_matchups.csv', stringsAsFactors = FALSE)
extension_2018 <- read.csv('extension_2018.csv', stringsAsFactors = FALSE)


#Constants
data_names <- c('year', 'team', 'opponent', 'point_diff', 
                'team1_adj_eff_margin', 'team1_adj_off_eff', 'team1_adj_def_eff', 'team1_pos_per_40', 'team1_luck', 'team1_sos', 
                'team1_opp_adj_off_eff', 'team1_opp_adj_def_eff', 
                'team2_adj_eff_margin', 'team2_adj_off_eff', 'team2_adj_def_eff', 'team2_pos_per_40', 'team2_luck', 'team2_sos', 
                'team2_opp_adj_off_eff', 'team2_opp_adj_def_eff')
results_names <- c('year', 'team', 'opponent', 'predicted_point_diff') #, 'point_diff')


#Merges data by team and year columns, drops all data for teams that did not make the tournament, and fixes names
first_round_matchups <- rbind(first_round_matchups, extension_2018)
data <- merge(x = first_round_matchups, y = team_data, by.x = c('year', 'opponent'), by.y = c('year', 'team'), all.x = TRUE)
data <- merge(x = data, y = team_data, by = c('year', 'team'), all.x = TRUE)
names(data) <- data_names

#Creates ratio stats
data$ratio_adj_eff_margin <- (data$team1_adj_eff_margin / data$team2_adj_eff_margin)
data$ratio_team1_adj_off_eff <- (data$team1_adj_off_eff / data$team2_opp_adj_off_eff)
data$ratio_team2_adj_off_eff <- (data$team2_adj_off_eff / data$team1_opp_adj_off_eff)
data$ratio_team1_adj_def_eff <- (data$team1_adj_def_eff / data$team2_opp_adj_def_eff)
data$ratio_team2_adj_def_eff <- (data$team2_adj_def_eff / data$team1_opp_adj_def_eff)
data$ratio_pos_per_40 <- data$team1_pos_per_40 / data$team2_pos_per_40


#Sets aside training and test sets
train_set <- data[data$year < 2018,]
test_set <- data[data$year == 2018,]

#Creates model and displays stats about it
fit <- lm(point_diff ~ ratio_team1_adj_off_eff + ratio_team2_adj_off_eff + ratio_team1_adj_def_eff + 
            ratio_team2_adj_def_eff, data = train_set)
summary(fit)

#Creates results data.frame, including a column which checks whether the prediction was correct or not
results <- data.frame(test_set$year, test_set$team, test_set$opponent, predict(fit, test_set)) #test_set$point_diff, predict(fit, test_set))
names(results) <- results_names
results$correct <- (results$point_diff * results$predicted_point_diff) > 0

#Calculates the percent of games predicted correctly and displays it
pct_correct <- 100 * sum(results$correct) / nrow(results)
print(paste0('The model was correct ', pct_correct, '% of the time.'))
