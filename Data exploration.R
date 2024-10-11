
install.packages("readr")
library(readr)
library(tidyverse)
library(dplyr)
# clean dataset from lizawood's github
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

# read in raw data
raw_df <- url %>% 
  fread() %>% 

  janitor::clean_names() 

# clean up some of the factors and playtime data
clean_df <- raw_df %>% 
  mutate(price = as.numeric(price),
         score_rank = word(score_rank_userscore_metascore, 1),
         average_playtime = word(playtime_median, 1),
         median_playtime = word(playtime_median, 2),
         median_playtime = str_remove(median_playtime, "\\("),
         median_playtime = str_remove(median_playtime, "\\)"),
         average_playtime = 60 * as.numeric(str_sub(average_playtime, 1, 2)) +
           as.numeric(str_sub(average_playtime, 4, 5)),
         median_playtime = 60 * as.numeric(str_sub(median_playtime, 1, 2)) +
           as.numeric(str_sub(median_playtime, 4, 5)),
         metascore = as.double(str_sub(score_rank_userscore_metascore, start = -4, end = -3))) %>% 
  select(-score_rank_userscore_metascore, -score_rank, -playtime_median) %>% 
  rename(publisher = publisher_s, developer = developer_s)


#Overall Data Set

#26688 rows, 10 variables

summary(raw_df)

# Game Number
#A generic number label for the game, not all numbers are filled in range

plot(density(raw_df$number), main="Game Number Density")
summary(raw_df$number)
sum(is.na(raw_df$number))

#not sure this is useful


#Game Titles

#Character variable with name of the games

length(raw_df$game)
length(unique(raw_df$game))
sum(is.na(raw_df$game))

# 3 NAs, 75 repeated game names 


#Release Dates

#currently a character variable not a date variable.

summary(raw_df$release_date)

dates<-as.Date(raw_df$release_date, tryFormats = c( "%B %Y", "%B %d, %Y" ))

summary(dates)
# 42 release dates which are alternatively labelled, No NAs 
#Date range from 2004 to 2018, predominantly 2015 onwards

plot(dates)

#Price 

# Price is currently a character vairable, 728 missing vals


summary(raw_df$price)

raw_df$price[which(raw_df$price=="Free")]<-0

prices1<-as.numeric(raw_df$price)

nonfree_prices1<-  prices1[prices1!=0]

prices2<-nonfree_prices1[nonfree_prices1<100]

plot(density(prices2, na.rm=T))

summary(nonfree_prices1)


#right skewed, range up to 595. Median = 5.99, fairly normally distributed. 


#Owners

#chracter variable 

raw_df$owners

length(unique(raw_df$owners))

plot(as.factor(raw_df$owners))

#26688 games, 10 columns
#Some NAs are because the game is free, others are "N/A", ranges from 0.49 to 200 ($?)

raw_df[raw_df$price=="N/A",]

#Metascore

summary(raw_df$score_rank_userscore_metascore)
unique(raw_df$score_rank_userscore_metascore)
#Looks like two seperate values, score and userscore, some of which are NA

#Median_playtime

summary(raw_df$playtime_median)
unique(raw_df$playtime_median)
#Looks like two numbers, brackets is something else that is smaller
#Average is first, median is second
#Clean dataset looks like it deals with these fine, measured in minutes
#Both are count variables, measured in minutes, natural numbers only
#No medians missing, 9 averages missing
#Publisher:

unique(raw_df$publisher_s)
count(raw_df$publisher_s)
#Substitute with number of games published I think
raw_df$published_count = 0

tabP = table(raw_df$publisher_s,useNA = "always")
tabD = table(raw_df$developer_s,useNA = "always")
raw_df$publishercount = tab[raw_df$publisher_s]
raw_df$developercount = tab[raw_df$developer_s]
#Developer:
#Same idea I think



