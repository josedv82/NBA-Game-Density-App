
################################################################################

#NBA Game Density APP
#Jose Fernandez
#March-June 2020 (Updated March 2022)

#Data Loading File. 
#Cleans and tidy raw datasets and converts to feather objects for lighter weight

################################################################################
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

#loading required libraries####

library(tidyverse)
library(readxl)
library(RcppRoll)
library(nbastatR)
library(ballr)
library(feather)
library(maps)

#################################################################################

#loading data from spreadsheets (escraped from NBAStuffer and Basketball Reference####

#NBA Schedule
data <- read_excel("NBA_Schedule.xlsx", sheet = "schedule")

#Game Scores
score <- read_excel("nba_scores.xlsx", sheet = "scores")

#Tidying and cleaning the above data. Prepping data for later use#####

dat <- data %>%
  select(Season = SEASON, Date = DATE, Time = 3, `Away Rest` = `ROAD REST DAYS`, `Road Team` = `ROAD TEAM`, `Home Team` = `HOME TEAM`, `Home Rest` = `HOME REST DAYS`, Arena = ARENA) %>%
  #mutate(Time = hms::as_hms(Time + 18000)) %>%
  mutate_if(~'POSIXt' %in% class(.x), as.Date) %>% filter(Season != "2016-17")

sco <- score %>% 
  select(-Time, -BX, -OT, -Notes) %>%
  mutate_if(~'POSIXt' %in% class(.x), as.Date) %>% filter(Date > "2017-10-01")


#Code performing a series fo team by team cleaning and tidying options (needs refactoring and looping)####


a.a <- dat %>%
  filter(`Road Team` == "New Orleans Pelicans" | `Home Team` == "New Orleans Pelicans") %>% 
  mutate(Team = "New Orleans Pelicans") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "New Orleans", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

a.b <- sco %>%
  filter(Team == "New Orleans Pelicans" | Opponent == "New Orleans Pelicans") %>%
  mutate(Team2 = ifelse(Team == "New Orleans Pelicans", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "New Orleans Pelicans", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "New Orleans Pelicans", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "New Orleans Pelicans", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

a <- full_join(a.a, a.b, by = c("Team", "Date", "Opponent"))


b.a <- dat %>%
  filter(`Road Team` == "Los Angeles Lakers" | `Home Team` == "Los Angeles Lakers") %>% 
  mutate(Team = "Los Angeles Lakers") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Los Angeles", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`) 

b.b <- sco %>%
  filter(Team == "Los Angeles Lakers" | Opponent == "Los Angeles Lakers") %>%
  mutate(Team2 = ifelse(Team == "Los Angeles Lakers", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Los Angeles Lakers", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Los Angeles Lakers", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Los Angeles Lakers", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

b <- full_join(b.a, b.b, by = c("Team", "Date", "Opponent"))


c.a <- dat %>%
  filter(`Road Team` == "Chicago Bulls" | `Home Team` == "Chicago Bulls") %>% 
  mutate(Team = "Chicago Bulls") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Chicago", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`) 

c.b <- sco %>%
  filter(Team == "Chicago Bulls" | Opponent == "Chicago Bulls") %>%
  mutate(Team2 = ifelse(Team == "Chicago Bulls", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Chicago Bulls", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Chicago Bulls", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Chicago Bulls", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws) 

c <- full_join(c.a, c.b, by = c("Team", "Date", "Opponent"))


d.a <- dat %>%
  filter(`Road Team` == "Cleveland Cavaliers" | `Home Team` == "Cleveland Cavaliers") %>% 
  mutate(Team = "Cleveland Cavaliers") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Cleveland", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`) 


d.b <- sco %>%
  filter(Team == "Cleveland Cavaliers" | Opponent == "Cleveland Cavaliers") %>%
  mutate(Team2 = ifelse(Team == "Cleveland Cavaliers", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Cleveland Cavaliers", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Cleveland Cavaliers", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Cleveland Cavaliers", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

d <- full_join(d.a, d.b, by = c("Team", "Date", "Opponent"))


e.a <- dat %>%
  filter(`Road Team` == "Detroit Pistons" | `Home Team` == "Detroit Pistons") %>% 
  mutate(Team = "Detroit Pistons") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Detroit", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`) 


e.b <- sco %>%
  filter(Team == "Detroit Pistons" | Opponent == "Detroit Pistons") %>%
  mutate(Team2 = ifelse(Team == "Detroit Pistons", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Detroit Pistons", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Detroit Pistons", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Detroit Pistons", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

e <- full_join(e.a, e.b, by = c("Team", "Date", "Opponent"))


f.a <- dat %>%
  filter(`Road Team` == "Boston Celtics" | `Home Team` == "Boston Celtics") %>% 
  mutate(Team = "Boston Celtics") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Boston", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`) 

f.b <- sco %>%
  filter(Team == "Boston Celtics" | Opponent == "Boston Celtics") %>%
  mutate(Team2 = ifelse(Team == "Boston Celtics", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Boston Celtics", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Boston Celtics", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Boston Celtics", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

f <- full_join(f.a, f.b, by = c("Team", "Date", "Opponent"))


g.a <- dat %>%
  filter(`Road Team` == "Minnesota Timberwolves" | `Home Team` == "Minnesota Timberwolves") %>% 
  mutate(Team = "Minnesota Timberwolves") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Minnesota", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`) 


g.b <- sco %>%
  filter(Team == "Minnesota Timberwolves" | Opponent == "Minnesota Timberwolves") %>%
  mutate(Team2 = ifelse(Team == "Minnesota Timberwolves", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Minnesota Timberwolves", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Minnesota Timberwolves", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Minnesota Timberwolves", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

g <- full_join(g.a, g.b, by = c("Team", "Date", "Opponent"))



h.a <- dat %>%
  filter(`Road Team` == "Memphis Grizzlies" | `Home Team` == "Memphis Grizzlies") %>% 
  mutate(Team = "Memphis Grizzlies") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Memphis", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

h.b <- sco %>%
  filter(Team == "Memphis Grizzlies" | Opponent == "Memphis Grizzlies") %>%
  mutate(Team2 = ifelse(Team == "Memphis Grizzlies", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Memphis Grizzlies", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Memphis Grizzlies", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Memphis Grizzlies", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

h <- full_join(h.a, h.b, by = c("Team", "Date", "Opponent"))



i.a <- dat %>%
  filter(`Road Team` == "Washington Wizards" | `Home Team` == "Washington Wizards") %>% 
  mutate(Team = "Washington Wizards") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Washington D.C.", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


i.b <- sco %>%
  filter(Team == "Washington Wizards" | Opponent == "Washington Wizards") %>%
  mutate(Team2 = ifelse(Team == "Washington Wizards", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Washington Wizards", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Washington Wizards", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Washington Wizards", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

i <- full_join(i.a, i.b, by = c("Team", "Date", "Opponent"))



j.a <- dat %>%
  filter(`Road Team` == "New York Knicks" | `Home Team` == "New York Knicks") %>% 
  mutate(Team = "New York Knicks") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "New York", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

j.b <- sco %>%
  filter(Team == "New York Knicks" | Opponent == "New York Knicks") %>%
  mutate(Team2 = ifelse(Team == "New York Knicks", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "New York Knicks", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "New York Knicks", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "New York Knicks", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

j <- full_join(j.a, j.b, by = c("Team", "Date", "Opponent"))



k.a <- dat %>%
  filter(`Road Team` == "Oklahoma City Thunder" | `Home Team` == "Oklahoma City Thunder") %>% 
  mutate(Team = "Oklahoma City Thunder") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Oklahoma", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


k.b <- sco %>%
  filter(Team == "Oklahoma City Thunder" | Opponent == "Oklahoma City Thunder") %>%
  mutate(Team2 = ifelse(Team == "Oklahoma City Thunder", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Oklahoma City Thunder", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Oklahoma City Thunder", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Oklahoma City Thunder", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

k <- full_join(k.a, k.b, by = c("Team", "Date", "Opponent"))


l.a <- dat %>%
  filter(`Road Team` == "Denver Nuggets" | `Home Team` == "Denver Nuggets") %>% 
  mutate(Team = "Denver Nuggets") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Denver", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


l.b <- sco %>%
  filter(Team == "Denver Nuggets" | Opponent == "Denver Nuggets") %>%
  mutate(Team2 = ifelse(Team == "Denver Nuggets", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Denver Nuggets", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Denver Nuggets", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Denver Nuggets", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

l <- full_join(l.a, l.b, by = c("Team", "Date", "Opponent"))


m.a <- dat %>%
  filter(`Road Team` == "Sacramento Kings" | `Home Team` == "Sacramento Kings") %>% 
  mutate(Team = "Sacramento Kings") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Sacramento", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


m.b <- sco %>%
  filter(Team == "Sacramento Kings" | Opponent == "Sacramento Kings") %>%
  mutate(Team2 = ifelse(Team == "Sacramento Kings", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Sacramento Kings", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Sacramento Kings", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Sacramento Kings", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

m <- full_join(m.a, m.b, by = c("Team", "Date", "Opponent"))



n.a <- dat %>%
  filter(`Road Team` == "Atlanta Hawks" | `Home Team` == "Atlanta Hawks") %>% 
  mutate(Team = "Atlanta Hawks") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Atlanta", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

n.b <- sco %>%
  filter(Team == "Atlanta Hawks" | Opponent == "Atlanta Hawks") %>%
  mutate(Team2 = ifelse(Team == "Atlanta Hawks", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Atlanta Hawks", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Atlanta Hawks", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Atlanta Hawks", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

n <- full_join(n.a, n.b, by = c("Team", "Date", "Opponent"))



o.a <- dat %>%
  filter(`Road Team` == "Milwaukee Bucks" | `Home Team` == "Milwaukee Bucks") %>% 
  mutate(Team = "Milwaukee Bucks") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Milwaukee", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


o.b <- sco %>%
  filter(Team == "Milwaukee Bucks" | Opponent == "Milwaukee Bucks") %>%
  mutate(Team2 = ifelse(Team == "Milwaukee Bucks", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Milwaukee Bucks", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Milwaukee Bucks", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Milwaukee Bucks", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

o <- full_join(o.a, o.b, by = c("Team", "Date", "Opponent"))



p.a <- dat %>%
  filter(`Road Team` == "Los Angeles Clippers" | `Home Team` == "Los Angeles Clippers") %>% 
  mutate(Team = "Los Angeles Clippers") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Los Angeles", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


p.b <- sco %>%
  filter(Team == "Los Angeles Clippers" | Opponent == "Los Angeles Clippers") %>%
  mutate(Team2 = ifelse(Team == "Los Angeles Clippers", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Los Angeles Clippers", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Los Angeles Clippers", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Los Angeles Clippers", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

p <- full_join(p.a, p.b, by = c("Team", "Date", "Opponent"))



q.a <- dat %>%
  filter(`Road Team` == "Toronto Raptors" | `Home Team` == "Toronto Raptors") %>% 
  mutate(Team = "Toronto Raptors") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Toronto", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


q.b <- sco %>%
  filter(Team == "Toronto Raptors" | Opponent == "Toronto Raptors") %>%
  mutate(Team2 = ifelse(Team == "Toronto Raptors", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Toronto Raptors", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Toronto Raptors", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Toronto Raptors", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

q <- full_join(q.a, q.b, by = c("Team", "Date", "Opponent"))


r.a <- dat %>%
  filter(`Road Team` == "Dallas Mavericks" | `Home Team` == "Dallas Mavericks") %>% 
  mutate(Team = "Dallas Mavericks") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Dallas", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

r.b <- sco %>%
  filter(Team == "Dallas Mavericks" | Opponent == "Dallas Mavericks") %>%
  mutate(Team2 = ifelse(Team == "Dallas Mavericks", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Dallas Mavericks", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Dallas Mavericks", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Dallas Mavericks", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

r <- full_join(r.a, r.b, by = c("Team", "Date", "Opponent"))


s.a <- dat %>%
  filter(`Road Team` == "Phoenix Suns" | `Home Team` == "Phoenix Suns") %>% 
  mutate(Team = "Phoenix Suns") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Phoenix", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


s.b <- sco %>%
  filter(Team == "Phoenix Suns" | Opponent == "Phoenix Suns") %>%
  mutate(Team2 = ifelse(Team == "Phoenix Suns", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Phoenix Suns", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Phoenix Suns", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Phoenix Suns", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

s <- full_join(s.a, s.b, by = c("Team", "Date", "Opponent"))



t.a <- dat %>%
  filter(`Road Team` == "Portland Trail Blazers" | `Home Team` == "Portland Trail Blazers") %>% 
  mutate(Team = "Portland Trail Blazers") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Portland", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


t.b <- sco %>%
  filter(Team == "Portland Trail Blazers" | Opponent == "Portland Trail Blazers") %>%
  mutate(Team2 = ifelse(Team == "Portland Trail Blazers", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Portland Trail Blazers", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Portland Trail Blazers", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Portland Trail Blazers", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

t <- full_join(t.a, t.b, by = c("Team", "Date", "Opponent"))


u.a <- dat %>%
  filter(`Road Team` == "Utah Jazz" | `Home Team` == "Utah Jazz") %>% 
  mutate(Team = "Utah Jazz") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Utah", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

u.b <- sco %>%
  filter(Team == "Utah Jazz" | Opponent == "Utah Jazz") %>%
  mutate(Team2 = ifelse(Team == "Utah Jazz", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Utah Jazz", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Utah Jazz", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Utah Jazz", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

u <- full_join(u.a, u.b, by = c("Team", "Date", "Opponent"))



v.a <- dat %>%
  filter(`Road Team` == "Miami Heat" | `Home Team` == "Miami Heat") %>% 
  mutate(Team = "Miami Heat") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Miami", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


v.b <- sco %>%
  filter(Team == "Miami Heat" | Opponent == "Miami Heat") %>%
  mutate(Team2 = ifelse(Team == "Miami Heat", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Miami Heat", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Miami Heat", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Miami Heat", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

v <- full_join(v.a, v.b, by = c("Team", "Date", "Opponent"))


w.a <- dat %>%
  filter(`Road Team` == "Philadelphia 76ers" | `Home Team` == "Philadelphia 76ers") %>% 
  mutate(Team = "Philadelphia 76ers") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Philadelphia", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


w.b <- sco %>%
  filter(Team == "Philadelphia 76ers" | Opponent == "Philadelphia 76ers") %>%
  mutate(Team2 = ifelse(Team == "Philadelphia 76ers", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Philadelphia 76ers", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Philadelphia 76ers", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Philadelphia 76ers", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

w <- full_join(w.a, w.b, by = c("Team", "Date", "Opponent"))


x.a <- dat %>%
  filter(`Road Team` == "Orlando Magic" | `Home Team` == "Orlando Magic") %>% 
  mutate(Team = "Orlando Magic") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Orlando", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


x.b <- sco %>%
  filter(Team == "Orlando Magic" | Opponent == "Orlando Magic") %>%
  mutate(Team2 = ifelse(Team == "Orlando Magic", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Orlando Magic", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Orlando Magic", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Orlando Magic", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

x <- full_join(x.a, x.b, by = c("Team", "Date", "Opponent"))


y.a <- dat %>%
  filter(`Road Team` == "Indiana Pacers" | `Home Team` == "Indiana Pacers") %>% 
  mutate(Team = "Indiana Pacers") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Indiana", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

y.b <- sco %>%
  filter(Team == "Indiana Pacers" | Opponent == "Indiana Pacers") %>%
  mutate(Team2 = ifelse(Team == "Indiana Pacers", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Indiana Pacers", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Indiana Pacers", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Indiana Pacers", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

y <- full_join(y.a, y.b, by = c("Team", "Date", "Opponent"))



z.a <- dat %>%
  filter(`Road Team` == "Golden State Warriors" | `Home Team` == "Golden State Warriors") %>% 
  mutate(Team = "Golden State Warriors") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "San Francisco", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


z.b <- sco %>%
  filter(Team == "Golden State Warriors" | Opponent == "Golden State Warriors") %>%
  mutate(Team2 = ifelse(Team == "Golden State Warriors", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Golden State Warriors", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Golden State Warriors", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Golden State Warriors", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

z <- full_join(z.a, z.b, by = c("Team", "Date", "Opponent"))



a.1.a.a <- dat %>%
  filter(`Road Team` == "Brooklyn Nets" | `Home Team` == "Brooklyn Nets") %>% 
  mutate(Team = "Brooklyn Nets") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "New York", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

a.1.b.b <- sco %>%
  filter(Team == "Brooklyn Nets" | Opponent == "Brooklyn Nets") %>%
  mutate(Team2 = ifelse(Team == "Brooklyn Nets", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Brooklyn Nets", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Brooklyn Nets", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Brooklyn Nets", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

a.1 <- full_join(a.1.a.a, a.1.b.b, by = c("Team", "Date", "Opponent"))


a.2.a.a <- dat %>%
  filter(`Road Team` == "Charlotte Hornets" | `Home Team` == "Charlotte Hornets") %>% 
  mutate(Team = "Charlotte Hornets") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Charlotte", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

a.2.b.b <- sco %>%
  filter(Team == "Charlotte Hornets" | Opponent == "Charlotte Hornets") %>%
  mutate(Team2 = ifelse(Team == "Charlotte Hornets", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Charlotte Hornets", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Charlotte Hornets", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Charlotte Hornets", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

a.2 <- full_join(a.2.a.a, a.2.b.b, by = c("Team", "Date", "Opponent"))


a.3.a.a <- dat %>%
  filter(`Road Team` == "Houston Rockets" | `Home Team` == "Houston Rockets") %>% 
  mutate(Team = "Houston Rockets") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "Houston", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)


a.3.b.b <- sco %>%
  filter(Team == "Houston Rockets" | Opponent == "Houston Rockets") %>%
  mutate(Team2 = ifelse(Team == "Houston Rockets", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "Houston Rockets", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "Houston Rockets", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "Houston Rockets", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

a.3 <- full_join(a.3.a.a, a.3.b.b, by = c("Team", "Date", "Opponent"))


a.4.a.a <- dat %>%
  filter(`Road Team` == "San Antonio Spurs" | `Home Team` == "San Antonio Spurs") %>% 
  mutate(Team = "San Antonio Spurs") %>%
  mutate(Location = ifelse(`Road Team` == Team, "Away", "Home")) %>%
  mutate(Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`)) %>%
  mutate(Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`)) %>%
  mutate(`Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  select(Season, Team, Month, Date, Time,  Opponent, Location, Arena, Rest, `Opp Rest`) %>%
  
  mutate(City = gsub( " .*$", "", Opponent)) %>%
  mutate(City = ifelse(City == "Los", "Los Angeles", 
                       ifelse(City == "Golden", "San Francisco", 
                              ifelse(City == "Brooklyn", "New York", 
                                     ifelse(City == "San", "San Antonio", 
                                            ifelse(City == "Washington", "Washington D.C.", City)))))) %>%
  mutate(City = ifelse(Opponent == "New York Knicks", "New York", City)) %>%
  mutate(City = ifelse(Opponent == "New Orleans Pelicans", "New Orleans", City)) %>%
  mutate(City = ifelse(Location == "Home", "San Antonio", City)) %>%
  select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

a.4.b.b <- sco %>%
  filter(Team == "San Antonio Spurs" | Opponent == "San Antonio Spurs") %>%
  mutate(Team2 = ifelse(Team == "San Antonio Spurs", paste(Team, Team_pts), Team)) %>%
  mutate(Team2 = ifelse(Opponent == "San Antonio Spurs", paste(Opponent, Opp_pts), Team2)) %>%
  mutate(Opp2 = ifelse(Opponent != "San Antonio Spurs", paste(Opponent, Opp_pts), Opponent)) %>%
  mutate(Opp2 = ifelse(Opponent == "San Antonio Spurs", paste(Team, Team_pts), Opp2)) %>%
  select(Date, Team2, Opp2, Attendance) %>%
  mutate(Team = gsub("[[:digit:]]","",Team2)) %>%
  mutate(Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2))) %>%
  mutate(Opponent = gsub("[[:digit:]]","",Opp2)) %>%
  mutate(Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))) %>%
  select(-Team2, -Opp2) %>%
  mutate(Team = ifelse(Team == "Philadelphia ers ",  "Philadelphia 76ers", Team)) %>%
  mutate(Opponent = ifelse(Opponent == "Philadelphia ers ",  "Philadelphia 76ers", Opponent)) %>%
  mutate_if(is.character, trimws)

a.4 <- full_join(a.4.a.a, a.4.b.b, by = c("Team", "Date", "Opponent"))






#Code to create master dataet joining all the above####
sche <- full_join(a,b, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(c, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(d, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(e, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(f, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(g, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(h, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(i, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(j, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(k, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(l, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(m, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(n, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(o, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(p, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(q, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(r, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(s, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(t, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(u, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(v, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(w, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(x, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(y, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(z, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(a.1, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(a.2, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(a.3, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  full_join(a.4, by = c("Season", "Team", "Month", "Date", "Time", "Opponent", "Location", "City", "Arena", "Rest", "Opp Rest", "Attendance", "Team_pts", "Opp_pts")) %>%
  
  #corrects a minor bug with sixers scores
  mutate(Opp_pts = ifelse(Opp_pts > 70000, Opp_pts - 76000, 
                          ifelse(Opp_pts %in% 7000:8000, Opp_pts - 7600, Opp_pts))) %>% 
  mutate(Team_pts = ifelse(Team_pts > 70000, Team_pts - 76000,
                           ifelse(Team_pts %in% 7000:8000, Team_pts - 7600, Team_pts))) %>%
  mutate(Rest = ifelse(Rest == "3", "3+", Rest)) %>%
  arrange(Date)

write_feather(sche, "sche.feather")


#################################################################################

#Code to get city coordinates (longitude and latitude) and overall tidying of the table####

#coordinates for Toronto. It is outside of the US so it needs manual binding
toronto <- c("Toronto", 43.65, -79.38)

#coordinates for all US cities where games are playes
acities <- us.cities %>% 
  filter(name == "Houston TX" | 
           name == "Oklahoma City OK" |
           name == "New York NY" |
           name == "Charlotte NC" | 
           name == "Miami FL" |
           name == "Phoenix AZ" | 
           name == "Salt Lake City UT" |
           name == "Los Angeles CA" | 
           name == "Dallas TX" |
           name == "Milwaukee WI" | 
           name == "Philadelphia PA" | 
           name == "Minneapolis MN" |
           name == "San Francisco CA" | 
           name == "Portland OR" |
           name == "Denver CO" |
           name == "Sacramento CA" | 
           name == "Boston MA" |
           name == "Detroit MI" | 
           name == "Memphis TN" |
           name == "Cleveland OH" | 
           name == "Chicago IL" |
           name == "Atlanta GA" |
           name == "WASHINGTON DC" |
           name == "Indianapolis IN" |
           name == "San Antonio TX" |
           name == "New Orleans LA" |
           name == "Orlando FL") %>%
  
  select(City = name, Latitude = lat, Longitude = long) %>%
  
  mutate(City = ifelse( City == "New Orleans LA", "New Orleans", 
                ifelse( City == "Houston TX", "Houston",
                ifelse( City == "Oklahoma City OK", "Oklahoma",
                ifelse( City == "New York NY", "New York",
                ifelse( City == "Charlotte NC", "Charlotte",
                ifelse( City == "Miami FL", "Miami",
                ifelse( City == "Phoenix AZ", "Phoenix",
                ifelse( City == "Salt Lake City UT", "Utah",
                ifelse( City == "Los Angeles CA", "Los Angeles",
                ifelse( City == "Dallas TX", "Dallas",
                ifelse( City == "Milwaukee WI", "Milwaukee",
                ifelse( City == "Philadelphia PA", "Philadelphia",
                ifelse( City == "Minneapolis MN", "Minnesota",
                ifelse( City == "San Francisco CA", "San Francisco",
                ifelse( City == "Portland OR", "Portland",
                ifelse( City == "Denver CO", "Denver",
                ifelse( City == "Sacramento CA", "Sacramento",
                ifelse( City == "Boston MA", "Boston",
                ifelse( City == "Detroit MI", "Detroit",
                ifelse( City == "Memphis TN", "Memphis",
                ifelse( City == "Cleveland OH", "Cleveland",
                ifelse( City == "Chicago IL", "Chicago",
                ifelse( City == "Orlando FL", "Orlando",
                ifelse( City == "Atlanta GA", "Atlanta",
                ifelse( City == "WASHINGTON DC", "Washington D.C.",
                ifelse( City == "Indianapolis IN", "Indiana",
                ifelse( City == "San Antonio TX", "San Antonio", "Toronto")))))))))))))))))))))))))))) %>%
  
  #binding toronto with all US cities
  rbind(toronto) %>%
  
  ungroup()

write_feather(acities, "acities.feather")


#################################################################################

#Code to import team logos from internet and assing to teams and opponents. Consider future refactoring for efficiency#####

Logos <- sche %>% 
  select(Date, Team, Opponent) %>%
  
  mutate(Team_Logo = 
          ifelse(Team == "Atlanta Hawks", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/atlanta-hawks-logo.png' width=200px></img>", 
          ifelse(Team == "Boston Celtics", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/boston-celtics-logo.png' width=200px></img>", 
          ifelse(Team == "Brooklyn Nets", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/brooklyn-nets-logo.png' width=200px></img>", 
          ifelse(Team == "Charlotte Hornets", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/charlotte-hornets-logo.png' width=200px></img>", 
          ifelse(Team == "Chicago Bulls", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/chicago-bulls-logo.png' width=200px></img>", 
          ifelse(Team == "Cleveland Cavaliers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/cleveland-cavaliers-logo.png' width=200px></img>", 
          ifelse(Team == "Dallas Mavericks", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/dallas-mavericks-logo.png' width=200px></img>", 
          ifelse(Team == "Denver Nuggets", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/denver-nuggets-logo.png' width=200px></img>", 
          ifelse(Team == "Detroit Pistons", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/detroit-pistons-logo.png' width=200px></img>", 
          ifelse(Team == "Golden State Warriors", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/golden-state-warriors-logo.png' width=200px></img>", 
          ifelse(Team == "Houston Rockets", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/houston-rockets-logo.png' width=200px></img>", 
          ifelse(Team == "Indiana Pacers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/indiana-pacers-logo.png' width=200px></img>", 
          ifelse(Team == "Los Angeles Clippers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-clippers-logo.png' width=200px></img>", 
          ifelse(Team == "Los Angeles Lakers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-lakers-logo.png' width=200px></img>", 
          ifelse(Team == "Memphis Grizzlies", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/memphis-grizzlies-logo.png' width=200px></img>", 
          ifelse(Team == "Miami Heat", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/miami-heat-logo.png' width=200px></img>", 
          ifelse(Team == "Milwaukee Bucks", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/milwaukee-bucks-logo.png' width=200px></img>", 
          ifelse(Team == "Minnesota Timberwolves", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/minnesota-timberwolves-logo.png' width=200px></img>", 
          ifelse(Team == "New Orleans Pelicans", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/new-orleans-pelicans-logo.png' width=200px></img>", 
          ifelse(Team == "New York Knicks", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/new-york-knicks-logo.png' width=200px></img>", 
          ifelse(Team == "Oklahoma City Thunder", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/oklahoma-city-thunder-logo.png' width=200px></img>", 
          ifelse(Team == "Orlando Magic", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/orlando-magic-logo.png' width=200px></img>", 
          ifelse(Team == "Philadelphia 76ers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/philadelphia-76ers-logo.png' width=200px></img>", 
          ifelse(Team == "Phoenix Suns", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/phoenix-suns-logo.png' width=200px></img>",
          ifelse(Team == "Portland Trail Blazers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/portland-trail-blazers-logo.png' width=200px></img>", 
          ifelse(Team == "Sacramento Kings", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/sacramento-kings-logo.png' width=200px></img>", 
          ifelse(Team == "San Antonio Spurs", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/san-antonio-spurs-logo.png' width=200px></img>", 
          ifelse(Team == "Toronto Raptors", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/toronto-raptors-logo.png' width=200px></img>", 
          ifelse(Team == "Washington Wizards", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/washington-wizards-logo.png' width=200px></img>", 
          ifelse(Team == "Utah Jazz", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/utah-jazz-logo.png' width=200px></img>", ""))))))))))))))))))))))))))))))) %>%
  
  mutate(Opp_Logo = 
           ifelse(Opponent == "Atlanta Hawks", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/atlanta-hawks-logo.png' width=200px></img>", 
           ifelse(Opponent == "Boston Celtics", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/boston-celtics-logo.png' width=200px></img>", 
           ifelse(Opponent == "Brooklyn Nets", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/brooklyn-nets-logo.png' width=200px></img>", 
           ifelse(Opponent == "Charlotte Hornets", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/charlotte-hornets-logo.png' width=200px></img>", 
           ifelse(Opponent == "Chicago Bulls", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/chicago-bulls-logo.png' width=200px></img>", 
           ifelse(Opponent == "Cleveland Cavaliers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/cleveland-cavaliers-logo.png' width=200px></img>", 
           ifelse(Opponent == "Dallas Mavericks", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/dallas-mavericks-logo.png' width=200px></img>", 
           ifelse(Opponent == "Denver Nuggets", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/denver-nuggets-logo.png' width=200px></img>", 
           ifelse(Opponent == "Detroit Pistons", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/detroit-pistons-logo.png' width=200px></img>", 
           ifelse(Opponent == "Golden State Warriors", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/golden-state-warriors-logo.png' width=200px></img>", 
           ifelse(Opponent == "Houston Rockets", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/houston-rockets-logo.png' width=200px></img>", 
           ifelse(Opponent == "Indiana Pacers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/indiana-pacers-logo.png' width=200px></img>", 
           ifelse(Opponent == "Los Angeles Clippers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-clippers-logo.png' width=200px></img>", 
           ifelse(Opponent == "Los Angeles Lakers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-lakers-logo.png' width=200px></img>", 
           ifelse(Opponent == "Memphis Grizzlies", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/memphis-grizzlies-logo.png' width=200px></img>", 
           ifelse(Opponent == "Miami Heat", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/miami-heat-logo.png' width=200px></img>", 
           ifelse(Opponent == "Milwaukee Bucks", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/milwaukee-bucks-logo.png' width=200px></img>", 
           ifelse(Opponent == "Minnesota Timberwolves", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/minnesota-timberwolves-logo.png' width=200px></img>", 
           ifelse(Opponent == "New Orleans Pelicans", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/new-orleans-pelicans-logo.png' width=200px></img>", 
           ifelse(Opponent == "New York Knicks", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/new-york-knicks-logo.png' width=200px></img>", 
           ifelse(Opponent == "Oklahoma City Thunder", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/oklahoma-city-thunder-logo.png' width=200px></img>", 
           ifelse(Opponent == "Orlando Magic", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/orlando-magic-logo.png' width=200px></img>", 
           ifelse(Opponent == "Philadelphia 76ers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/philadelphia-76ers-logo.png' width=200px></img>", 
           ifelse(Opponent == "Phoenix Suns", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/phoenix-suns-logo.png' width=200px></img>",
           ifelse(Opponent == "Portland Trail Blazers", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/portland-trail-blazers-logo.png' width=200px></img>", 
           ifelse(Opponent == "Sacramento Kings", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/sacramento-kings-logo.png' width=200px></img>", 
           ifelse(Opponent == "San Antonio Spurs", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/san-antonio-spurs-logo.png' width=200px></img>", 
           ifelse(Opponent == "Toronto Raptors", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/toronto-raptors-logo.png' width=200px></img>", 
           ifelse(Opponent == "Washington Wizards", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/washington-wizards-logo.png' width=200px></img>", 
           ifelse(Opponent == "Utah Jazz", "<img src='https://cdn.freebiesupply.com/images/thumbs/2x/utah-jazz-logo.png' width=200px></img>", "")))))))))))))))))))))))))))))))

write_feather(Logos, "logos.feather")


#################################################################################

#Loading shotchart datasets using NBAstatR package####

#shortcharts for all players and games for 2018 season
y2018 <- teams_shots(
  all_active_teams = T,
  season_types = "Regular Season",
  seasons = 2018, #adjust dinamically based on season filter
  measures = "FGA",
  #date_from = "20171001",
  #date_to = "20200401",
  return_message = F,
  nest_data = F)

#shortcharts for all players and games for 2019 season
y2019 <- teams_shots(
  all_active_teams = T,
  season_types = "Regular Season",
  seasons = 2019, #adjust dinamically based on season filter
  measures = "FGA",
  #date_from = "20171001",
  #date_to = "20200401",
  return_message = F,
  nest_data = F)

#shortchart for all players and games for 2020 season
y2020 <- teams_shots(
  all_active_teams = T,
  season_types = "Regular Season",
  seasons = 2020, #adjust dinamically based on season filter
  measures = "FGA",
  #date_from = "20171001",
  #date_to = "20200401",
  return_message = F,
  nest_data = F)

#shortchart for all players and games for 2021 season
y2021 <- teams_shots(
  all_active_teams = T,
  season_types = "Regular Season",
  seasons = 2021, #adjust dinamically based on season filter
  measures = "FGA",
  #date_from = "20171001",
  #date_to = "20200401",
  return_message = F,
  nest_data = F)


#shortchart for all players and games for 2022 season
y2022 <- teams_shots(
  all_active_teams = T,
  season_types = "Regular Season",
  seasons = 2022, #adjust dinamically based on season filter
  measures = "FGA",
  #date_from = "20171001",
  #date_to = "20200401",
  return_message = F,
  nest_data = F)



#joining all seasons above into one table
all <- full_join(y2018, y2019) %>% full_join(y2020) %>% full_join(y2021) %>% full_join(y2022) %>%
  mutate(Date = as.Date(as.character(dateGame), format="%Y%m%d")) %>%
  select(Season = slugSeason, Date, Player = namePlayer, Team = nameTeam, Event = typeEvent, Action = typeAction, Shot = typeShot, 
         Quarter = numberPeriod, minRemaining = minutesRemaining, secRemaining = secondsRemaining, Zone = zoneBasic, Zone2 = nameZone, Range = zoneRange, 
         Distance = distanceShot, locX = locationX, locY = locationY)

write_feather(all, "shotchart.feather")


#################################################################################

#Loading Game Logs and Stats for each game in the last 3 seasons using NBAstatR function####
statlogs <- game_logs(
  seasons = 2018:2022,
  league = "NBA",
  result_types = "player",
  season_types = "Regular Season",
  nest_data = F,
  assign_to_environment = F,
  return_message = F
) %>%
  
  
  select(Season = slugSeason, Date = dateGame, Team = nameTeam, Team_Rest = countDaysRestTeam, Player = namePlayer, Rest = countDaysRestPlayer, MINS = minutes, FG_M = fgm, FG_A = fga, `FG_%` = pctFG, FG3_M = fg3m, FG3_A = fg3a, `FG3_%` = pctFG3, FG2_M = fg2m, FG2_A = fg2a, `FG2_%` = pctFG2, FT_M = ftm, FT_A = fta, `FT_%` = pctFT, OffReb = oreb, DeffReb = dreb, TotalReb = treb, AST = ast, STL = stl, BLK = blk, TOV = tov, PF = pf, PTS = pts, `+/-` = plusminus, Photo = urlPlayerThumbnail) %>%
  
  mutate(`FG_%` = round(`FG_%`,2), `FG3_%` = round(`FG3_%`,2), `FG2_%` = round(`FG2_%`,2), `FT_%` = round(`FT_%`,2)) %>%
  
  mutate(RestIndex = ifelse(Rest == 0, 5, 
                            ifelse(Rest == 1, 4,
                                   ifelse(Rest == 2, 3.5,
                                          ifelse(Rest == 3, 2,
                                                 ifelse(Rest == 4, 1.5, 1)))))) %>%
  mutate(Load = RestIndex * MINS) %>%
  
  group_by(Season) %>%
  
  mutate(Load = (round((Load-min(Load))/(max(Load)-min(Load)),2)) * 100) %>%
  
  select(-RestIndex) %>%
  
  mutate(Team = ifelse(Team == "LA Clippers", "Los Angeles Clippers", Team)) %>%
  
  ungroup()

#count total games by team
team_games <- statlogs %>% select(Season, Team, Date) %>% group_by(Season, Team) %>% distinct(Date) %>% summarise(Count = n())
#count total games by player
player_games <- statlogs %>% select(Season, Team, Player, Date) %>% group_by(Season, Team, Player) %>% distinct(Date) %>% summarise(Count1 = n())


statlogs2 <- full_join(team_games, player_games) %>% mutate(Participation = round((Count1 * 100) / Count, 0)) %>%
  full_join(statlogs)



write_feather(statlogs2, "gamelogs.feather")


#################################################################################

#loading data from other excels docs that will be used in the app ####

#dataset containing all research and media articles
articles <- read_excel("articles.xlsx", sheet = "articles") 
write_feather(articles, "article.feather")

#dataset containing links to video highlights scraped from youtube. Split for home an away games as there are two instances for each game (team and opponent)
highlights <- read_excel("highlights.xlsx", sheet = "games") %>% mutate(Date = as.Date(Date, origin = "1970-01-01")) %>% select(-Season)#video highlights
highlights2 <- highlights %>% select(Team = Opponent, Opponent = Team, Date, Link) #video highlights for away games
write_feather(highlights, "highlights.feather")
write_feather(highlights2, "highlights2.feather")

#profile images (headshots) used for shotcharts
pro_file <- nbastatR::seasons_players(seasons = 2016:2022) %>% select(Player = namePlayer, Image = urlPlayerHeadshot) #loads profile image of players
write_feather(pro_file, "pro_file.feather")

#################################################################################





