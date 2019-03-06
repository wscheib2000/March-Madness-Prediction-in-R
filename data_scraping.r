library(rvest)

#Sets working directory
setwd('C:/Users/student/Documents/March-Madness-Prediction-in-R')

#Constants
years <- c(2002:2018)
team_data_names <- c('year', 'team', 'adj_eff_margin', 'adj_off_eff', 'adj_def_eff', 'pos_per_40', 'luck', 'sos', 'opp_adj_off_eff', 'opp_adj_def_eff')
matchups_names <- c('team', 'score1', 'opponent', 'score2')
scorer_names <- c('team_ppg', 'leader_ppg')



###TEAM DATA###

#Initializes data.frame into which data will be scraped
team_data <- NULL

#Scrapes data into data.frame
for(y in years){
  #Builds link and gets html code
  link <- paste0('http://kenpom.com/index.php?y=', y)
  download.file(link, destfile = 'scraped_page.html', quiet=TRUE)
  webpage <- read_html('scraped_page.html')
  
  #Reads table, converts it to text, and converts the resultant character vector to a data.frame
  temp_data_html <- html_nodes(webpage, 'td')
  temp_data <- html_text(temp_data_html)
  temp_data <- data.frame(temp_data, stringsAsFactors = FALSE)
  
  #Rearranges values from 1 column to 21 columns
  for(i in c(0:(nrow(temp_data) - 1))){
    temp_data[(i %/% 21) + 1, (i %% 21) + 1] <- temp_data[i + 1, 1]
  }
  
  #Adds year column, removes unnecessary columns, rearranges columns, and removes excess rows and teams that played 25 or fewer games
  temp_data$year <- y
  for(r in 1:nrow(temp_data)){
    x <- unlist(strsplit(temp_data[r, 4], '-'))
    temp_data$games[r] <- as.numeric(x[1]) + as.numeric(x[2])
  }
  temp_data <- temp_data[complete.cases(temp_data), c(22, 2, 5, (3:9) * 2)] #& temp_data$games > 26 & temp_data[, 2] != 'Centenary' & temp_data[, 2] != 'Utah Valley St.', c(22, 2, 5, (3:9) * 2)]
  
  #Cleans up data, getting rid of rankings in team names and plus signs on numbers in certain columns
  for(i in c(1:nrow(temp_data))){
    temp_data[i, 2] <- sub(' [0-9,.]+', '', temp_data[i, 2])
    for(j in c(1:ncol(temp_data))){
      temp_data[i, j] <- sub('[+]', '', temp_data[i, j])
    }
    temp_data[i, 2] <- sub('Troy St.', 'Troy', temp_data[i, 2])
    temp_data[i, 2] <- sub('^Little Rock', 'Arkansas Little Rock', temp_data[i, 2])
  }
  
  
  #Adds year's data to the overall data.frame
  team_data <- rbind(team_data, temp_data)
}

#Adds table headers
names(team_data) <- team_data_names



###FIRST ROUND MATCHUPS###

#Initializes data.frame into which data will be scraped
first_round_matchups <- NULL

#Scrapes data into data.frame
for (y in years){
  #Builds link and gets html code
  link <- paste0('https://www.sports-reference.com/cbb/postseason/', y, '-ncaa.html')
  download.file(link, destfile = 'scraped_page.html', quiet=TRUE)
  webpage <- read_html('scraped_page.html')
  
  #Reads first round matchups and their scores, converts them to text, and converts the resultant character vector to a data.frame
  temp_data_html <- html_nodes(webpage, '.round:nth-child(1) div div a')
  temp_data <- html_text(temp_data_html)
  temp_data <- data.frame(temp_data, stringsAsFactors = FALSE)
  
  #Rearranges values from 1 column to 4 columns and adds names
  for(i in c(0:(nrow(temp_data) - 1))){
    temp_data[(i %/% 4) + 1, (i %% 4) + 1] <- temp_data[i + 1, 1]
  }
  names(temp_data) <- matchups_names
  
  #Adds year column, calculates point_diff colum, removes score columns, rearranges columns, and removes excess rows
  temp_data$year <- y
  temp_data <- transform(temp_data, score1 = as.numeric(score1))
  temp_data <- transform(temp_data, score2 = as.numeric(score2))
  temp_data$point_diff <- temp_data$score1 - temp_data$score2
  temp_data <- temp_data[complete.cases(temp_data),c(5, 1, 3, 6)]
  temp_data <- temp_data[-c(33, 34),]
  
  #Standardizes team names
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
  
  #Adds year's data to the overall data.frame
  first_round_matchups <- rbind(first_round_matchups, temp_data)
}




###2018 EXTENSION###

#Assigns link and gets html code
link <- 'https://www.sports-reference.com/cbb/postseason/2018-ncaa.html'
download.file(link, destfile = 'scraped_page.html', quiet=TRUE)
webpage <- read_html('scraped_page.html')

#Reads remaining matchups for 2018 and their scores, converts them to text, and converts the resultant character vector to a data.frame
extension_2018_html <- html_nodes(webpage, '.round+ .round .winner+ div a , .round+ .round .winner a')
extension_2018 <- html_text(extension_2018_html)
extension_2018 <- data.frame(extension_2018, stringsAsFactors = FALSE)

#Rearranges values from 1 column to 4 columns and adds names
for(i in c(0:(nrow(extension_2018) - 1))){
  extension_2018[(i %/% 4) + 1, (i %% 4) + 1] <- extension_2018[i + 1, 1]
}
names(extension_2018) <- matchups_names

#Adds year column, calculates point_diff colum, removes score columns, rearranges columns, and removes excess rows
extension_2018$year <- y
extension_2018 <- transform(extension_2018, score1 = as.numeric(score1))
extension_2018 <- transform(extension_2018, score2 = as.numeric(score2))
extension_2018$point_diff <- extension_2018$score1 - extension_2018$score2
extension_2018 <- extension_2018[complete.cases(extension_2018),c(5, 1, 3, 6)]

#Standardizes team names
for(i in c(1:nrow(extension_2018))){
  for(j in c(2,3)){
    extension_2018[i, j] <- sub('-', ' ', extension_2018[i, j])
    extension_2018[i, j] <- sub('State', 'St\\.', extension_2018[i, j])
    extension_2018[i, j] <- sub('NC St\\.', 'North Carolina St\\.', extension_2018[i, j])
    extension_2018[i, j] <- sub('UNC$', 'North Carolina', extension_2018[i, j])
    extension_2018[i, j] <- sub('Alabama Birmingham$', 'UAB', extension_2018[i, j])
    extension_2018[i, j] <- sub(' \\(NY\\)', '', extension_2018[i, j])
    extension_2018[i, j] <- sub('ETSU', 'East Tennessee St.', extension_2018[i, j])
    extension_2018[i, j] <- sub('^Little Rock', 'Arkansas Little Rock', extension_2018[i, j])
    extension_2018[i, j] <- sub('Louisiana', 'Louisiana Lafayette', extension_2018[i, j])
    extension_2018[i, j] <- sub('\\(', '', extension_2018[i, j])
    extension_2018[i, j] <- sub('\\)', '', extension_2018[i, j])
    extension_2018[i, j] <- sub('Loyola IL', 'Loyola Chicago', extension_2018[i, j])
    extension_2018[i, j] <- sub('Ole Miss', 'Mississippi', extension_2018[i, j])
    extension_2018[i, j] <- sub('Pitt', 'Pittsburgh', extension_2018[i, j])
    extension_2018[i, j] <- sub('Southeastern Louisiana Lafayette', 'Southeastern Louisiana', extension_2018[i, j])
    extension_2018[i, j] <- sub('St. Joseph', 'Saint Joseph', extension_2018[i, j])
    extension_2018[i, j] <- sub('St. Peter', 'Saint Peter', extension_2018[i, j])
    extension_2018[i, j] <- sub('Texas Arlington', 'UT Arlington', extension_2018[i, j])
    extension_2018[i, j] <- sub('Texas A&M Corpus Christi', 'Texas A&M Corpus Chris', extension_2018[i, j])
    extension_2018[i, j] <- sub('UConn', 'Connecticut', extension_2018[i, j])
    extension_2018[i, j] <- sub('UCSB', 'UC Santa Barbara', extension_2018[i, j])
    extension_2018[i, j] <- sub('UIC', 'Illinois Chicago', extension_2018[i, j])
    extension_2018[i, j] <- sub('UMass', 'Massachusetts', extension_2018[i, j])
  }
}




###LEADING SCORER###

#Initializes data.frame into which data will be scraped
# leading_scorers <- NULL

# #Scrapes data into data.frame
# for(y in years){
#   for(t in unique(team_data[team_data$year == y, 'team'])){
#     #Fixes team name so it matches what should be in the url
#     t_useable <- sub('USC', 'Southern California', t)
#     t_useable <- sub('^Penn$', 'Pennsylvania', t_useable)
#     t_useable <- sub('Bowling Green', 'Bowling Green State', t_useable)
#     t_useable <- sub('BYU', 'Brigham Young', t_useable)
#     t_useable <- sub('UNC ', 'North Carolina ', t_useable)
#     t_useable <- sub('LSU', 'Louisiana State', t_useable)
#     t_useable <- sub('St. John\'s', 'St. John\'s NY', t_useable)
#     t_useable <- sub('Saint Mary\'s', 'Saint Mary\'s CA', t_useable)
#     t_useable <- sub('UC ', 'California ', t_useable)
#     t_useable <- sub('UT ', 'Texas ', t_useable)
#     t_useable <- sub('UNLV', 'Nevada Las Vegas', t_useable)
#     t_useable <- sub('UAB', 'Alabama Birmingham', t_useable)
#     t_useable <- sub('UCF', 'Central Florida', t_useable)
#     t_useable <- sub('Detroit', 'Detroit Mercy', t_useable)
#     t_useable <- sub('SMU', 'Southern Methodist', t_useable)
#     t_useable <- sub('TCU', 'Texas Christian', t_useable)
#     t_useable <- sub('UTEP', 'Texas El Paso', t_useable)
#     t_useable <- sub('UMBC', 'Maryland Baltimore County', t_useable)
#     t_useable <- sub('Southwest ', '', t_useable)
#     t_useable <- sub('VCU', 'Virginia Commonwealth', t_useable)
#     t_useable <- sub('FIU', 'Florida International', t_useable)
#     t_useable <- sub('VMI', 'Virginia Military Institute', t_useable)
#     t_useable <- sub('UTSA', 'Texas San Antonio', t_useable)
#     t_useable <- sub('St\\. Francis PA', 'Saint Francis PA', t_useable)
#     t_useable <- sub('UMKC', 'Missouri Kansas City', t_useable)
#     t_useable <- sub('Loyola Chicago', 'Loyola IL', t_useable)
#     t_useable <- sub('Corpus Chris', 'Corpus Christi', t_useable)
#     t_useable <- sub('Utah Valley St\\.', 'Utah Valley', t_useable)
#     t_useable <- sub('Prairie View A&M', 'Prairie View', t_useable)
#     t_useable <- sub('Albany', 'Albany NY', t_useable)
#     t_useable <- sub('LIU Brooklyn', 'Long Island University', t_useable)
#     t_useable <- sub('Southern Miss', 'Southern Mississippi', t_useable)
#     t_useable <- sub('Central Connecticut', 'Central Connecticut State', t_useable)
#     t_useable <- sub('The ', '', t_useable)
#     t_useable <- sub('\'', '', t_useable)
#     t_useable <- sub('\\.', '', t_useable)
#     t_useable <- sub('\\& *', '', t_useable)
#     t_useable <- sub('Cal St', 'Cal State', t_useable)
#     t_useable <- sub('Grambling St', 'Grambling', t_useable)
#     t_useable <- sub('Middle Tennessee St', 'Middle Tennessee', t_useable)
#     t_useable <- gsub(' ', '-', t_useable)
#     t_useable <- sub('St$', 'State', t_useable)
#     t_useable <- tolower(t_useable)
    
#     #Builds link and gets html code
#     link <- paste0('https://www.sports-reference.com/cbb/schools/', t_useable, '/', y, '.html')
#     download.file(link, destfile = 'scraped_page.html', quiet=TRUE)
#     webpage <- read_html('scraped_page.html')
    
#     #Reads first round matchups and their scores, converts them to text, and converts the resultant character vector to a data.frame
#     temp_data_html <- html_nodes(webpage, '#team_stats tr:nth-child(1) .right:nth-child(25) , #per_game tr:nth-child(1) .right:nth-child(26)')
#     temp_data <- html_text(temp_data_html)
#     temp_data <- data.frame(temp_data, stringsAsFactors = FALSE)
    
#     #Rearranges values from 1 column to 4 columns and adds names
#     for(i in c(0:(nrow(temp_data) - 1))){
#       temp_data[(i %/% 2) + 1, (i %% 2) + 1] <- temp_data[i + 1, 1]
#     }
#     names(temp_data) <- scorer_names
    
#     #Adds year column, calculates point_diff colum, removes score columns, rearranges columns, and removes excess rows
#     temp_data$year <- y
#     temp_data$team <- t
#     temp_data <- transform(temp_data, team_ppg = as.numeric(team_ppg))
#     temp_data <- transform(temp_data, leader_ppg = as.numeric(leader_ppg))
#     temp_data$top_scorer_prop <- temp_data$leader_ppg / temp_data$team_ppg
#     temp_data <- temp_data[complete.cases(temp_data),c(3:5)]
    
#     #Adds year's data to the overall data.frame
#     leading_scorers <- rbind(leading_scorers, temp_data)
#   }
# }



###WRITE TO CSV###
write.csv(team_data, 'team_data.csv', row.names = FALSE)
write.csv(first_round_matchups, 'first_round_matchups.csv', row.names = FALSE)
write.csv(extension_2018, 'extension_2018.csv', row.names = FALSE)
