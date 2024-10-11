library(dplyr)
library(data.table)
library(stringr)
# clean dataset from lizawood's github
url <- "https://raw.githubusercontent.com/lizawood/apps-and-games/master/PC_Games/PCgames_2004_2018_raw.csv"

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


