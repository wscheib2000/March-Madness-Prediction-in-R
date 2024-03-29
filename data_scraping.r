library(rvest)
library(mgsub)
library(tidyr)
library(rstudioapi)

#Sets working directory
setwd(dirname(getActiveDocumentContext()$path))

#Constants
years <- c(2002:2018)
team_data_names <- c('year', 'team', 'adj_eff_margin', 'adj_off_eff', 'adj_def_eff', 'pos_per_40', 'luck', 'sos', 'opp_adj_off_eff', 'opp_adj_def_eff')
matchups_names <- c('year', 'team', 'opponent', 'point_diff')
temp_matchups_names <- c('team', 'score1', 'opponent', 'score2')
scorer_names <- c('team_ppg', 'leader_ppg')

#Data cleaning functions
key <- read.csv("key.txt", stringsAsFactors = FALSE, header = FALSE)
standardize_teams <- function(team){
  if(!(temp_data[i, j] %in% sports_reference_names)){
    sports_reference_names[sports_reference_names_end] <- tolower(temp_data[i, j])
    sports_reference_names_end <- sports_reference_names_end + 1
  }
  return(mgsub(team, key[1,], key[2,]))
}


###TEAM DATA###

#Initializes data.frame into which data will be scraped and pre-allocates data storage for efficiency
team_data <- data.frame(rep(1,10000), rep("1",10000), rep(1,10000), rep(1,10000), rep(1,10000), rep(1,10000), 
                        rep(1,10000), rep(1,10000), rep(1,10000), rep(1,10000), stringsAsFactors = FALSE)
names(team_data) <- team_data_names

#Sets endpoint for team_data
team_data_end <- 1

#Scrapes data into team_data
for(y in c(years, 2019)){
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
  team_data[team_data_end:(team_data_end + nrow(temp_data) - 1),] <- temp_data
  team_data_end <- team_data_end + nrow(temp_data)
}
team_data <- team_data[1:(team_data_end - 1),]





###MATCHUPS###

#Initializes data.frame into which data will be scraped and pre-allocates data storage for efficiency
a <- numeric(1500)
b <- character(1500)
c <- character(1500)
d <- numeric(1500)
for(i in 1:(1500)){
  a[i] <- i
  b[i] <- toString(i)
  c[i] <- toString(i)
  d[i] <- i
}
matchups <- data.frame(a, b, c, d, stringsAsFactors = FALSE)
names(matchups) <- matchups_names

#Initializes vector in which sports-refence team names will be stored
sports_reference_names <- as.character(c(1:1000))

#Sets endpoints for the data.frame and vector
matchups_end <- 1
sports_reference_names_end <- 1

#Scrapes data into data.frame
for (y in years){
  #Builds link and gets html code
  link <- paste0('https://www.sports-reference.com/cbb/postseason/', y, '-ncaa.html')
  download.file(link, destfile = 'scraped_page.html', quiet=TRUE)
  webpage <- read_html('scraped_page.html')
  
  #Reads first round matchups and their scores, converts them to text, and converts the resultant character vector to a data.frame
  temp_data_html <- html_nodes(webpage, '.round div+ div div a , .winner+ div a , .winner a , #east p a , #midwest p a , #south p a ,
                               #west p a , #southeast p a , #southwest p a , #minneapolis p a , #atlanta p a , #oakland p a ,
                               #washington p a , #syracuse p a , #albuquerque p a , #austin p a , #chicago p a , #stlouis p a ,
                               #eastrutherford p a , #phoenix p a , .winner+ div a , .round:nth-child(1) div div a ,
                               .round:nth-child(2) div div a , .round:nth-child(3) div div a , .round:nth-child(4) div div a ,
                               strong a')
  temp_data <- html_text(temp_data_html) %>% data.frame(stringsAsFactors = FALSE)

  #Rearranges values from 1 column to 4 columns and adds names (trying to problem solve a better way to do this)
  temp_data2<-data.frame(matrix(ncol = 4, nrow = 1))
  sapply(seq(0, nrow(temp_data) - 1), (function(i) temp_data2[(i %/% 4) + 1, (i %% 4) + 1] <- temp_data[i + 1, 1]))
  
  temp_data$names <- rep(temp_matchups_names, length.out = nrow(temp_data))
  spread(temp_data, names, 2)
  
  for(i in c(0:(nrow(temp_data) - 1))){
    temp_data[(i %/% 4) + 1, (i %% 4) + 1] <- temp_data[i + 1, 1]
  }
  names(temp_data) <- temp_matchups_names
  
  #Adds year column, calculates point_diff colum, removes score columns, rearranges columns, and removes excess rows
  temp_data <- temp_data[complete.cases(temp_data),]
  temp_data$year <- y
  temp_data <- transform(temp_data, score1 = as.numeric(score1))
  temp_data <- transform(temp_data, score2 = as.numeric(score2))
  temp_data$point_diff <- temp_data$score1 - temp_data$score2
  temp_data <- temp_data[, c(5, 1, 3, 6)]
  
  #Standardizes team names and records sports-reference names
  temp_data[,2] <- sapply(temp_data[,2], standardize_teams)
  temp_data[,3] <- sapply(temp_data[,3], standardize_teams)
  
  #Adds year's data to the overall data.frame
  matchups[matchups_end:(matchups_end + nrow(temp_data) - 1),] <- temp_data
  matchups_end <- matchups_end + nrow(temp_data)
}
matchups <- matchups[1:(matchups_end - 1),]





# ###COACHING CONSISTENCY###
# #Initializes data.frame into which data will be scraped
# coaching_consistency <- NULL

# #Scrapes data into data.frame
# for(y in years){
#   for(t in sports_reference_names){
#     #Builds link and gets html code
#     t <- gsub(' ', '-', t)
#     t <- gsub('\\.', '', t)
#     t <- gsub('\\(', '', t)
#     t <- gsub('\\)', '', t)
#     t <- gsub("\\'", '', t)
#     t <- gsub('-st$', '-state', t)
#     t <- gsub('cal-st', 'cal-state', t)
#     t <- gsub('penn', 'pennsylvania', t)
#     t <- gsub('^nc', 'north-carolina', t)
#     t <- gsub('unc', 'north-carolina', t)
#     t <- gsub('uconn', 'connecticut', t)
#     t <- gsub('usc', 'southern-california', t)
#     t <- gsub('pitt', 'pittsburgh', t)
#     link <- paste0('https://www.sports-reference.com/cbb/schools/', t, '/', y, '.html')
#     download.file(link, destfile = 'scraped_page.html', quiet=TRUE)
#     webpage <- read_html('scraped_page.html')
    
#     #Initializes coaches data.frame
#     coaches <- NULL
    
#     #Reads coach names, converts them to text, adds them to the coaches data.frame
#     temp_data_html <- html_nodes(webpage, 'p~ p+ p a')
#     temp_data <- html_text(temp_data_html)
#     temp_data <- data.frame(temp_data, stringsAsFactors = FALSE)
#   }
# }




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
write.csv(matchups, 'matchups.csv', row.names = FALSE)
