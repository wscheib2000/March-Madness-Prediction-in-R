#Imports
library(ggplot2)
library(olsrr)

#Sets working directory
setwd('C:/Users/student/Documents/March-Madness-Prediction-in-R')

#Reads in data from CSV
team_data <- read.csv('team_data.csv', stringsAsFactors = FALSE)
matchups <- read.csv('matchups.csv', stringsAsFactors = FALSE)

#Constants
data_names <- c('year', 'team', 'opponent', 'point_diff', 
                'team1_adj_eff_margin', 'team1_adj_off_eff', 'team1_adj_def_eff', 'team1_pos_per_40', 'team1_luck', 'team1_sos', 
                'team1_opp_adj_off_eff', 'team1_opp_adj_def_eff', 
                'team2_adj_eff_margin', 'team2_adj_off_eff', 'team2_adj_def_eff', 'team2_pos_per_40', 'team2_luck', 'team2_sos', 
                'team2_opp_adj_off_eff', 'team2_opp_adj_def_eff')
results_names <- c('year', 'team', 'opponent', 'predicted_point_diff_fit1', 'predicted_point_diff_fit2', 'point_diff')
samp_means_names <- c('sample', 'samp_mean_fit1', 'samp_mean_fit2')



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


#Sets number of samples
n <- 2500
#Pre-allocates data storage for efficiency
a <- numeric(32 * n)
b <- character(32 * n)
c <- character(32 * n)
x <- numeric(32 * n)
y <- numeric(32 * n)
z <- numeric(32 * n)
for(i in 1:(32 * n)){
  a[i] <- i
  b[i] <- toString(i)
  c[i] <- toString(i)
  x[i] <- i
  y[i] <- i
  z[i] <- i
}
results <- data.frame(a, b, c, x, y, z, stringsAsFactors = FALSE)
names(results) <- results_names
x <- numeric(n)
y <- numeric(n)
z <- numeric(n)
for(i in 1:n){
  x[i] <- i
  y[i] <- i
  z[i] <- i
}
samp_means <- data.frame(x, y, z, stringsAsFactors = FALSE)
names(samp_means) <- samp_means_names

#Sets endpoints for each data.frame
results_end <- 1
samp_means_end <- 1

#Samples n random training and test sets and records mean differences and results
for(i in 1:n) {
  #Sets aside training and test sets
  test_rows <- sample(1:nrow(data), 32, replace = FALSE)
  train_set <- data[-test_rows,]
  test_set <- data[test_rows,]
  
  #Creates models
  fit1 <- lm(point_diff ~ team1_adj_eff_margin + team2_adj_eff_margin + team1_adj_def_eff + team2_sos + ratio_adj_eff_margin, 
              data = train_set)
  fit2 <- lm(point_diff ~ team1_adj_off_eff + team1_adj_def_eff + team1_sos + team1_opp_adj_off_eff + team1_opp_adj_def_eff + 
              team2_adj_eff_margin + team2_sos + ratio_adj_eff_margin, data = train_set)
  
  #Creates new_results data.frame
  new_results <- data.frame(test_set$year, test_set$team, test_set$opponent, predict(fit1, test_set), predict(fit2, test_set),
                            test_set$point_diff, stringsAsFactors = FALSE)
  names(new_results) <- results_names
  
  #Calculates sample mean difference between predicted and observed point differential and creates new_samp_mean data.frame
  samp_mean_diff_fit1 <- mean(abs(new_results$point_diff - new_results$predicted_point_diff_fit1))
  samp_mean_diff_fit2 <- mean(abs(new_results$point_diff - new_results$predicted_point_diff_fit2))
  new_samp_mean <- data.frame(i, samp_mean_diff_fit1, samp_mean_diff_fit2)
  names(new_samp_mean) <- samp_means_names
  
  #Puts results data and sample mean in the appropriate data.frames and changes ends accordingly
  results[(results_end):(results_end + 31),] <- new_results
  results_end <- results_end + nrow(new_results)
  samp_means[samp_means_end,] <- new_samp_mean
  samp_means_end <- samp_means_end + nrow(new_samp_mean)
}
#Calculates correct columnc
results$correct_fit1 <- (results$point_diff * results$predicted_point_diff_fit1) > 0
results$correct_fit2 <- (results$point_diff * results$predicted_point_diff_fit2) > 0

#Calculates the percent of games predicted correctly, the average difference, and the standard deviation of the difference
#and displays them
pct_correct_fit1 <- 100 * mean(results$correct_fit1)
avg_difference_fit1 <- mean(samp_means$samp_mean_fit1)
med_difference_fit1 <- median(samp_means$samp_mean_fit1)
sd_difference_fit1 <- sd(samp_means$samp_mean_fit1)
print(paste0('Model fit1 was correct ', pct_correct_fit1, '% of the time and predicted, on average, ', avg_difference_fit1,
             ' points different from the actual result with standard deviation ', sd_difference_fit1, '.'))
pct_correct_fit2 <- 100 * mean(results$correct_fit2)
avg_difference_fit2 <- mean(samp_means$samp_mean_fit2)
med_difference_fit2 <- median(samp_means$samp_mean_fit2)
sd_difference_fit2 <- sd(samp_means$samp_mean_fit2)
print(paste0('Model fit2 was correct ', pct_correct_fit2, '% of the time and predicted, on average, ', avg_difference_fit2,
             ' points different from the actual result with standard deviation ', sd_difference_fit2, '.'))

#Plots the sample mean differences
text_x_coord <- ceiling(max(samp_means$samp_mean_fit1)) - 2
ggplot(data=samp_means, aes(samp_means$samp_mean_fit1)) + geom_histogram(binwidth = .1) + 
  geom_vline(xintercept=avg_difference, color="red", lwd=2) + 
  geom_vline(xintercept=med_difference, color="blue", lwd=2) + 
  geom_text(x=text_x_coord, y=90, label=paste0("mean: ", round(avg_difference, digits=4)), size=5, color="red") +
  geom_text(x=text_x_coord, y=80, label=paste0("median: ", round(med_difference, digits=4)), size=5, color="blue") +
  ggtitle("Fit 1")

#Plots the sample mean differences
text_x_coord <- ceiling(max(samp_means$samp_mean_fit2)) - 2
ggplot(data=samp_means, aes(samp_means$samp_mean_fit2)) + geom_histogram(binwidth = .1) + 
  geom_vline(xintercept=avg_difference, color="red", lwd=2) + 
  geom_vline(xintercept=med_difference, color="blue", lwd=2) + 
  geom_text(x=text_x_coord, y=90, label=paste0("mean: ", round(avg_difference, digits=4)), size=5, color="red") +
  geom_text(x=text_x_coord, y=80, label=paste0("median: ", round(med_difference, digits=4)), size=5, color="blue") + 
  ggtitle("Fit 2")
