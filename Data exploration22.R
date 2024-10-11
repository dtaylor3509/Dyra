install.packages("readr")
library(readr)
library(tidyverse)
library(dplyr)
# clean dataset from lizawood's github
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

# read in raw data
raw_df <- url %>% 
  read_csv() %>% 
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


