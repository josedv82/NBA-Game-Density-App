
###################################################

#NBA Game Density APP
#Jose Fernandez
#March 2020

###################################################

#loading required libraries####
library(maps)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(formattable)
library(dashboardthemes)
library(readxl)
library(RcppRoll)
library(plotly)
library(shinyBS)
library(shinyalert)
library(shinycustomloader)
library(geosphere)
library(waiter)


#####################################################

#loading data####
data <- read_excel("NBA_Schedule.xlsx", sheet = "schedule")
score <- read_excel("nba_scores.xlsx", sheet = "scores")


#####################################################

#initial cleaning data#####

dat <- data %>%
  select(Season = SEASON, Date = DATE, Time = 3, `Away Rest` = `ROAD REST DAYS`, `Road Team` = `ROAD TEAM`, `Home Team` = `HOME TEAM`, `Home Rest` = `HOME REST DAYS`, Arena = ARENA) %>%
  mutate(Time = hms::as_hms(Time + 18000)) %>%
  mutate_if(~'POSIXt' %in% class(.x), as.Date) %>% filter(Season != "2016-17")

sco <- score %>% 
  select(-Time, -BX, -OT, -Notes) %>%
  mutate_if(~'POSIXt' %in% class(.x), as.Date) %>% filter(Date > "2017-10-01")

#sort out by team 

#####################################################

#team by team cleaning####

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




###################################################

#create master table with all data####

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

###################################################

# cities table with lat and lon####

#coordinates for Toronto
toronto <- c("Toronto", 43.65, -79.38)

#coordinates for othet cities
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
  rbind(toronto) %>%
  
  ungroup()




##################################################

#Logos for table Team####

df <- data.frame(val = c("Atlanta Hawks","Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks",
                         "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers",
                         "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", 
                         "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
                         "Utah Jazz", "Washington Wizards"))

df$img = c(
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/atlanta-hawks-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[1]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/boston-celtics-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[2]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/brooklyn-nets-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[3]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/charlotte-hornets-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[4]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/chicago-bulls-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[5]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/cleveland-cavaliers-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[6]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/dallas-mavericks-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[7]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/denver-nuggets-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[8]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/detroit-pistons-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[9]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/golden-state-warriors-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[10]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/houston-rockets-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[11]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/indiana-pacers-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[12]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-clippers-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[13]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-lakers-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[14]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/memphis-grizzlies-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[15]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/miami-heat-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[16]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/milwaukee-bucks-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[17]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/minnesota-timberwolves-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[18]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/new-orleans-pelicans-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[19]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/new-york-knicks-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[20]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/oklahoma-city-thunder-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[21]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/orlando-magic-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[22]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/philadelphia-76ers-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[23]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/phoenix-suns-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[24]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/portland-trail-blazers-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[25]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/sacramento-kings-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[26]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/san-antonio-spurs-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[27]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/toronto-raptors-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[28]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/utah-jazz-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[29]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/washington-wizards-logo.png' width=30px><div class='jhr'>%s</div></img>", df$val[30]))
  
##################################################

#Logos for opening dashboard#####

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

###################################################

#links social media buttons####
url <- "https://twitter.com/intent/tweet?text=Link to the NBA game density app. @jfernandez__&url=https://josedv.shinyapps.io/NBASchedule/"
url2 <- "https://josedv.shinyapps.io/NBASchedule/"


###################################################

#User Interface

##################################################

ui <- dashboardPagePlus(
  
#header############
  
  header = dashboardHeaderPlus(
    
    
    #popup modal link in header
    tags$li(class = "dropdown", actionLink("welcome", label = 'About', icon = icon("info-circle"), style = 'color:white'),
            bsTooltip("welcome", HTML("Click to know more about this app."), placement = "bottom", trigger = "hover", options = NULL)),
    
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "cogs",
    
    title = tagList(
     shiny::span(class = "logo-lg", "NBA Game Density Simulator", style = "color:white"), 
      img(src = "hexlogo.png", width = "35px", height = "33px")),
    
    
    titleWidth = 330),
  
  
#################################################

#sidebar######  
  
  #the following code crates funtionality on the left side menu, providing filtering options for users.    
  
  sidebar = dashboardSidebar(
    
    width = 330,
    
    sidebarMenu(
      
      fluidRow(width = "100%", align = "center", #style = "padding-right:45px",
               
      tags$br(),
      
      column(width = 5),
      
      column(width = 2,  style = "padding-right:45px",
      # Create url with the 'twitter-share-button' class
      tags$a(href = url, "Tweet", class="twitter-share-button", `data-show-count` = "true"),
      includeScript("http://platform.twitter.com/widgets.js")),
      
      column(width = 1),
      
      # Create url with the 'linkedin-share-button' class
      column(width = 2,  style = "padding-right:45px",
      tags$script(src = "https://platform.linkedin.com/in.js", type = "text/javascript", "lang: en_US"),
      tags$script(type = "IN/Share",`data-url` = url2)),
      
      column(width = 2)
      
      
      ),
      
       fluidRow(width = "100%", align = "center",
               
                  pickerInput(
                    inputId = "season_filter",
                    label = p("Select Season:", style = "padding-right:160px"),
                    choices = sche$Season %>% unique() %>% sort(), 
                    selected = "2019-20", 
                    choicesOpt = list(style = rep(("color: black; background: white"),30)),
                    multiple = F)
               
               ),
      
      #team by team tab
      menuItem("Team by Team", tabName = "teambyteam", icon = icon("project-diagram"), startExpanded = T,
               
          menuSubItem(icon = NULL, tabName = "teambyteam", 
            pickerInput(
            inputId = "team_filter",
            label = "Select Team", 
            choices = df$val,
            choicesOpt = list(style = rep(("color: black; background: white"),30), content = df$img),
            multiple = F)),
          tags$hr(),
          menuSubItem(icon = NULL, tabName = "teambyteam", fluidRow(withLoader(DT::dataTableOutput("team_table_density"), type ="html", loader = "loader1"), style="padding-right: 17px;")), 
          tags$hr()
          
          ),
      
      #all teams tab
      menuItem("All Teams", tabName = "allteams", icon = icon("th"), startExpanded = F)
      
    )#sidebar menu
    
    
  ),

##################################################

#body##########
  
  #the following code provides functionality for the main body of the dashboard
  body = dashboardBody(
    
    use_waiter(), #nedded to run the waiter upon loading 
    
    #code to supress error messages when inputs are removed
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    #this piece helps alight the logos in the picker input
    tags$head(tags$style(".jhr{ display: inline;vertical-align: middle;padding-left: 10px;}")),
    
    #customised look for sliders
    chooseSliderSkin("Simple"),
    
    #custom theme for the app
    shinyDashboardThemes(theme = "grey_dark"),
    
    
    #initializes shiny alert for modal and popups
    useShinyalert(),
    
    tabItems(
    
    # Tab items for team by team tab
    tabItem(
      tabName = "teambyteam",
      tabBox(title = "", id = "tab1", height = "100%", width = "100%", 
             
      tabPanel("Game Card", icon = icon("map-marked-alt"), 
               fluidRow(column(width = 6, uiOutput("date"))),
               tags$hr(),
               
               fluidRow(
                 
                 #teamDash
                 column(width = 4, align="center",
                        htmlOutput("team_logo"),
                        uiOutput("team"), 
                        htmlOutput("location_team"), 
                        uiOutput("team_points"), 
                        tags$strong(htmlOutput("team_label")),
                        tags$br(),
                        htmlOutput("density_team"), 
                        tags$br(),
                        htmlOutput("text_index1"),
                        htmlOutput("team_index"),
                        tags$br(),
                        htmlOutput("text_movindex1"),
                        htmlOutput("team_movindex")
                        ),
                 
                 #game details
                 column(width = 4, align = "center",
                        tags$strong(htmlOutput("city")),
                        htmlOutput("arena"),
                        tags$br(),
                        tags$strong(htmlOutput("route")),
                        htmlOutput("distance"),
                        withLoader(plotOutput("map_plot", width = "100%", height = "500px"), type = "html", loader = "loader1")
                        ),
                 
                 
                 #OpponentDash
                 column(width = 4, align="center", 
                        
                        htmlOutput("opponent_logo"),
                        uiOutput("opponent"), 
                        htmlOutput("location_opponent"),
                        uiOutput("opponent_points"), 
                        tags$strong(htmlOutput("opponent_label")),
                        tags$br(),
                        htmlOutput("density_opponent"), 
                        tags$br(),
                        htmlOutput("text_index2"),
                        htmlOutput("opponent_index"),
                        tags$br(),
                        htmlOutput("text_movindex2"),
                        htmlOutput("opponent_movindex")
                 )
                 
                        ) #fluidrow
                        ),#tabpanel  
      
      tabPanel("Schedule Table", icon = icon("calendar-alt"), fluidRow(column(width = 12, withLoader(DT::dataTableOutput("team_table", width = "100%"), type = "html", loader = "loader1")))),
      tabPanel("Rolling Density", icon = icon("chart-line"), fluidRow(column(width = 12, withLoader(plotlyOutput("team_plot", height = "auto", width = "100%"), type = "html", loader = "loader1")))),
      tabPanel("Outcome", icon = icon("tasks"), fluidRow(column(width = 8, DT::dataTableOutput("w_l_table")), column(width = 4, DT::dataTableOutput("H_A_table"))))
    )#tabbox
    
    ),#tabitem
    
    # Tab items for all teams tab
    tabItem(
      tabName = "allteams",
      tabBox(title = "", id = "tab2", height = "100%", width = "100%",
             tabPanel("Density Counts", icon = icon("list-alt"), 
                      
                      fluidRow(column(width = 4,
                        pickerInput(
                        inputId = "location_filter",
                        label = "Select Game Location(s)", 
                        choices = c("Home", "Away"), 
                        selected = c("Home", "Away"), 
                        choicesOpt = list(style = rep(("color: black; background: white"),2)),
                        multiple = T)
                        
                        )),
                      
             fluidRow(column(width = 12, withLoader(DT::dataTableOutput("all_teams", width = "100%"), type = "html", loader = "loader1")))),
             tabPanel("Heatmap", icon = icon("solar-panel"), fluidRow(column(width = 12, withLoader(plotOutput("heatmap", width = "100%", height = '750px'),type = "html", loader = "loader1"))))
      )#tabbox
  )#tabitem
  
 
    ),#tabitems
  
  
################################################

#code for page loader from waiter####
  show_waiter_on_load(
    color = "white",
    div(style = "color:white;",
        tags$h2("Loading...", style = "color:grey", align = "center"),
        tags$img(src="waiter.gif", width="auto", height = "auto")
    )
  )
  

  ),#dashboardbody


##################################################

#right side bar#####


  #right side dashboard to allow users configure their loads.
    rightsidebar = rightSidebar(
      
      
      width = 300,
      
      background = "dark",
      
      
      rightSidebarTabContent(
        id = 1,
        icon = "plane-departure",
        title = "",
        active = TRUE,
        tags$h4("Travel Load Factor", style = "color:white"),
        tags$hr(),
        
        #input for game location
        tags$h4("Game Location", style = "color:#e48b38"),
        sliderTextInput("home","Home Games", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T),
        sliderTextInput("away","Away Games", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        tags$hr(),
        #input for travel
        tags$h4("Travel", style = "color:#e48b38"),
        sliderTextInput("travelyes","Game with travel", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("travelno","Game without travel", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T),
        tags$hr(),
        #direction of travel
        tags$h4("Direction", style = "color:#e48b38"),
        sliderTextInput("east","Direction Eastbound", choices = seq(from = 1, to = 5, by = 0.5), selected = 3, grid = T),
        sliderTextInput("west","Direction Westbound", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("neutral","Within same zone", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T)
        
      ),
      
      rightSidebarTabContent(
        id = 2,
        icon = "globe-americas",
        title = "",
        active = FALSE,
        tags$h4("Time Shift Factor", style = "color:white"),
        tags$hr(),
        
        #input for time zones corossed
        tags$h4("Time Zones Crossed", style = "color:#e48b38"),
        sliderTextInput("0neutral","No zone change", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T),
        sliderTextInput("1east","1 Zone East", choices = seq(from = 1, to = 5, by = 0.5), selected = 2.5, grid = T),
        sliderTextInput("2east","2 Zones East", choices = seq(from = 1, to = 5, by = 0.5), selected = 3.5, grid = T),
        sliderTextInput("3east","3 Zones East", choices = seq(from = 1, to = 5, by = 0.5), selected = 4.5, grid = T),
        sliderTextInput("1west","1 Zone West", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("2west","2 Zone West", choices = seq(from = 1, to = 5, by = 0.5), selected = 3, grid = T),
        sliderTextInput("3west","3 Zone West", choices = seq(from = 1, to = 5, by = 0.5), selected = 4, grid = T)
        
      ),
      
      rightSidebarTabContent(
        id = 3,
        icon = "clock",
        title = "",
        active = FALSE,
        tags$h4("Elapsed Days Factor", style = "color:white"),
        tags$hr(),
        
        #input for days elapsed
        tags$h4("Days since last game.", style = "color:#e48b38"),
        sliderTextInput("1day","1 Day ", choices = seq(from = 1, to = 5, by = 0.5), selected = 5, grid = T),
        sliderTextInput("2day","2 Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 4, grid = T),
        sliderTextInput("3day","3 Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 3, grid = T),
        sliderTextInput("4day","4 Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("5day","5+ Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T)
        
      ),
      
      
      rightSidebarTabContent(
        id = 4,
        icon = "suitcase-rolling",
        title = "",
        active = FALSE,
        tags$h4("Game Density Factor", style = "color:white"),
        tags$hr(),
        
        #input for density profiles
        tags$h4("Acummulated Games.", style = "color:#e48b38"),
        sliderTextInput("G3in4b2b","3 Games in 4 Days + B2B", choices = seq(from = 1, to = 5, by = 0.5), selected = 5, grid = T),
        sliderTextInput("G3in4","3 Games in 4 Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 4.5, grid = T),
        sliderTextInput("Gb2b","Back to Back", choices = seq(from = 1, to = 5, by = 0.5), selected = 4, grid = T),
        sliderTextInput("G1d","1 Rest Day", choices = seq(from = 1, to = 5, by = 0.5), selected = 3.5, grid = T),
        sliderTextInput("G2d","2 Rest Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 3, grid = T),
        sliderTextInput("G3d","3+ Rest Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T)
        
      ),#right sidebar tab content
      
      
      rightSidebarTabContent(
        id = 5,
        icon = "circle-notch",
        title = "",
        active = FALSE,
        tags$h4("Density Moving Average", style = "color:white"),
        tags$hr(),
        tags$h5("A simple moving (or rolling) average is an unweighted mean of the last n games. Use the knob below so determine the n of games to roll over.", style = "color:lightgray"),
        tags$hr(),
        
        fluidRow(width = "100%", align = "center",
        knobInput(
          inputId = "rolling",
          label = "",
          value = 7,
          min = 1,
          max = 31,
          displayPrevious = T, 
          lineCap = "round",
          fgColor = "#e48b38",
          inputColor = "#e48b38",
          bgColor = "black",
          immediate = FALSE
        )
        )#fluidRow
                   
      )#right sidebar tab content
      
      ),#right sidebar
    
###################################################

#footer###########


  #this codes add a footer to the dashboard
  footer = dashboardFooter(
    
    right_text = HTML(paste(img(src = "hexlogo.png", width = "35px", height = "33px"), 
                            tags$span("NBA Game Density APP", style = "font-family: Arial; color: grey; font-size: 16px"))),
    
    left_text = HTML(paste(icon = icon("copyright"), tags$span("2020. Jose Fernandez", style = "font-family: Arial; color: grey; font-size: 16px"),
                            tags$span(tags$a(href= "mailto:jose.fernandezdv@gmail.com", icon("envelope"))), 
                            tags$span(tags$a(href= "https://twitter.com/jfernandez__", icon("twitter"))),
                            sep = " "))
  )
  
)

###################################################


#Server Logic. 

##################################################


server <- function(input, output, session) {
  
  
#shinyalert after changing moving average value#####
  
  observeEvent(input$rolling, ignoreInit = TRUE, {
    # Show a modal when the button is pressed
    shinyalert(paste("Moving Index Set to last", input$rolling, "Games.", sep = " "), type = "success")
  })
 
  
  
#####################
  
#intro pop up modal#### 
  observeEvent(input$welcome, {
    
    
    showModal(
      
      modalDialog(
        HTML(paste(img(src = "hexlogo.png", width = "80px", height = "75px"), tags$br(), tags$span("NBA Game Density App", style = "font-family: Arial; color: white; font-size:40px"))),
        tags$hr(),
        tags$br(),
        tags$h4("Game Density Simulator", style = "padding-left:2.3em; font-family: Arial; color: white"),
        tags$img(src = "https://image.flaticon.com/icons/svg/833/833634.svg", width = "40px", height = "40px", align = "left"),
        p("The density of the schedule is different for each team and it may be influenced by different factors, such as travel, travel distance, rest days, etc.", style = "padding-left:3em; font-family: Arial; color: white"),
        tags$br(),
        tags$h4("Accumulated Games", style = "padding-left:2.3em; font-family: Arial; color: white"),
        tags$img(src = "https://image.flaticon.com/icons/svg/833/833644.svg", width = "40px", height = "40px", align = "left"),
        p("NBA teams play various games within a week. For example, 3 games in 4 days including a Back to Back (3IN4-B2B). 3 games in 4 days (3IN4). Back to Back (B2B). 1 day rest (1). Two days rest (2). 3 or more rest days (3+)", style = "padding-left:3em; font-family: Arial; color: white"),
        HTML(paste(tags$span("--------", style = "color:transparent"), tags$span("For more information on those categories visit", style = "font-family: Arial; color: white"), tags$span(tags$a(href= "https://www.nbastuffer.com/2019-2020-nba-schedule-rest-days-analysis/", "NBAstuffer.")))),
        tags$br(),
        tags$br(),
        tags$h4("Density Index", style = "padding-left:2.3em; font-family: Arial; color: white"),
        tags$img(src = "https://image.flaticon.com/icons/svg/747/747914.svg", width = "40px", height = "40px", align = "left"),
        p("Whe using this app, you can adjust the various factors involved in calculated density index such as game location, travel, time shift, direction of travel, density index, days since last game, etc. You can adjust how much weight you give to each of these factors on the right side menu.", style = "padding-left:3em; font-family: Arial; color: white"),
        tags$br(),
        tags$h4("Teams Comparison", style = "padding-left:2.3em; font-family: Arial; color: white"),
        tags$img(src = "https://image.flaticon.com/icons/svg/747/747938.svg", width = "40px", height = "40px", align = "left"),
        p("There are various options throughout the app to compare a team density index vs the rivals.", style = "padding-left:3em; font-family: Arial; color: white"),
        tags$br(),
        tags$hr(),
        HTML(paste(tags$span("League wide summaries will be added in the near feature. In the meantime, please share your feedback via", style = "font-family: Arial; color: white"), tags$span(tags$a(href= "mailto:jose.fernandezdv@gmail.com", "email.")))),
        
        
        easyClose = TRUE)
      
    )#showmodal
    
    
  })#observeEvent 
  
  
#####################
  
################################################   
#create reactive dataset. This part of the code creates the final data set with reactive filters to allow users to decide how much weight
#to give to each type of game and the rolling window for the average####


  
  #add months and rolling average for stress
  sche1 <- reactive({
    
    
     sche %>% 
      
      
      filter(Season == input$season_filter) %>%
      
      mutate(Season = as.factor(Season)) %>%
      group_by(Team) %>%
      
      mutate(Month = ifelse(Month == 1, 'January',
                            ifelse(Month == 2, 'February',
                                   ifelse(Month == 3, 'March',
                                          ifelse(Month == 4, 'April',
                                                 ifelse(Month == 5, 'May',
                                                        ifelse(Month == 10, 'October',
                                                               ifelse(Month == 11, 'November', 'December')))))))) %>%
      
      select(-Time) %>%
      
      mutate(elapsed = Date - lag(Date)) %>%
      
      mutate(Travel = ifelse(City != lag(City), "y", "n")) %>%
      
      mutate(Zone = ifelse(City == "Toronto", "Eastern",
                              ifelse( City == "New Orleans", "Central",
                              ifelse( City == "Houston", "Central",
                              ifelse( City == "Oklahoma", "Central",
                              ifelse( City == "New York", "Eastern",
                              ifelse( City == "Charlotte", "Eastern",
                              ifelse( City == "Miami", "Eastern",
                              ifelse( City == "Phoenix", "Mountain",
                              ifelse( City == "Utah", "Mountain",
                              ifelse( City == "Los Angeles", "Pacific",
                              ifelse( City == "Dallas", "Central",
                              ifelse( City == "Milwaukee", "Central",
                              ifelse( City == "Philadelphia", "Eastern",
                              ifelse( City == "Minnesota", "Central",
                              ifelse( City == "San Francisco", "Pacific",
                              ifelse( City == "Portland", "Pacific",
                              ifelse( City == "Denver", "Mountain",
                              ifelse( City == "Sacramento", "Pacific",
                              ifelse( City == "Boston", "Eastern",
                              ifelse( City == "Detroit", "Eastern",
                              ifelse( City == "Memphis", "Central",
                              ifelse( City == "Cleveland", "Eastern",
                              ifelse( City == "Chicago", "Central",
                              ifelse( City == "Orlando", "Eastern",
                              ifelse( City == "Atlanta", "Eastern",
                              ifelse( City == "Washington D.C.", "Eastern",
                              ifelse( City == "Indiana", "Eastern",
                              ifelse( City == "San Antonio", "Central", "other"))))))))))))))))))))))))))))) %>%
      
      mutate(zone.n = ifelse(Zone == "Pacific", 1,
                                ifelse(Zone == "Mountain", 2,
                                ifelse(Zone == "Central", 3,
                                ifelse(Zone == "Eastern", 4, 0))))) %>%
      
      mutate(shift = zone.n - lag(zone.n)) %>%
      
      mutate(Direction = ifelse(shift < 0, "westbound",
                                   ifelse(shift > 0, "eastbound",
                                   ifelse(shift == 0, "-", 100)))) %>%
      
      #calculate index
      
      mutate(location.i = ifelse(Location == "Home", input$home,
                                    ifelse(Location == "Away", input$away, 100)))%>%
      
      mutate(Rest.i = ifelse(Rest == "3+", input$G3d,
                             ifelse(Rest == "2", input$G2d,
                                    ifelse(Rest == "1", input$G1d,
                                           ifelse(Rest == "B2B", input$Gb2b,
                                                  ifelse(Rest == "3IN4", input$G3in4,
                                                         ifelse(Rest == "3IN4-B2B", input$G3in4b2b, 100))))))) %>%
      
      mutate(elapsed.i = ifelse(elapsed < 0, 5,
                                ifelse(elapsed == 1, input$`1day`,
                                       ifelse(elapsed == 2, input$`2day`,
                                              ifelse(elapsed == 3, input$`3day`,
                                                     ifelse(elapsed == 4, input$`4day`,
                                                            ifelse(elapsed > 4, input$`5day`, 100))))))) %>%
      
      
      mutate(travel.i = ifelse(Travel == "y", input$travelyes,
                                  ifelse(Travel == "n", input$travelno, 100))) %>%
      
      
      mutate(shift.i = ifelse(shift == 0, input$`0neutral`,
                              ifelse(shift == 1, input$`1east`,
                                     ifelse(shift == 2, input$`2east`,
                                            ifelse(shift == 3, input$`3east`, 
                                                   ifelse(shift == -1, input$`1west`,
                                                          ifelse(shift == -2, input$`2west`,
                                                                 ifelse(shift == -3, input$`3west`, 100)))))))) %>%
      
      
      mutate(direction.i = ifelse(Direction == "-", input$neutral,
                                     ifelse(Direction == "eastbound", input$east,              
                                     ifelse(Direction == "westbound", input$west, 100)))) %>%
      
      #
      
      replace_na(list(elapsed.i = 1, travel.i = 1 , shift.i = 1, direction.i = 1)) %>%
      
      mutate(index = location.i + Rest.i + elapsed.i + travel.i + shift.i + direction.i) %>%
      
      ungroup() %>%
      
      mutate(Normalized = ifelse(is.na(index), 0, round((index-min(index))/(max(index)-min(index)), 2))) %>%
      
      select(Season, Month, Date, Location, City, Arena, Team, Opponent, Team_pts, Opp_pts, Density = Rest, Normalized) %>%
      
      #
      
      group_by(Team) %>%
      
      mutate(MovIndex = round(roll_meanr(Normalized, n = input$rolling, align = "right", fill = 0, na.rm = T),2))
    
  })
  
  
  
  
  #code to add opponent rolling stress
  sche2 <- reactive({
  
    a <- sche1() %>%
      mutate(conc = paste(Team, Date, Opponent)) %>%
      select(aSeason = Season, aTeam = Team, aMonth = Month, aDate = Date, aOpponent = Opponent, aLocation = Location, aCity = City, aArena = Arena, aDensity = Density, aTeam_pts = Team_pts, aOpp_pts = Opp_pts, aNormalized = Normalized, aMovIndex = MovIndex, conc)
    
    b <- sche1() %>%
      mutate(conc = paste(Opponent, Date, Team)) %>%
      select(bSeason = Season, bTeam = Team, bMonth = Month, bDate = Date, bOpponent = Opponent, bLocation = Location, bCity = City, bArena = Arena, bDensity = Density, bTeam_pts = Team_pts, bOpp_pts = Opp_pts, bNormalized = Normalized, bMovIndex = MovIndex, conc)
    
    
    #final table with both local and opponent rolling stress
    full_join(a,b, by = "conc") %>%
      select(Season= aSeason, Month = aMonth, Date = aDate, Location = aLocation, City = aCity, Arena = aArena, Team = aTeam, Opponent = aOpponent, Team_pts = aTeam_pts, Opp_pts = aOpp_pts, Density = aDensity, `Opp Density` = bDensity, Index = aNormalized, `Opp Index` = bNormalized, movIndex = aMovIndex, `Opp movIndex` = bMovIndex) %>%
      mutate(`W/L` = ifelse(Team_pts > Opp_pts, "W", "L")) %>%
      ungroup() %>%
      select(Season, Month, Date, Location, City, Arena, Team, Opponent, `W/L`, Team_pts, Opp_pts, Density, `Opp Density`, Index, `Opp Index`, movIndex, `Opp movIndex`) %>%
      arrange(Season, Team)
  
  
  })
  
  #code with cities coordinates and travel details for mapping
  
  cities <- reactive({
    
    validate(
      need(input$team_filter, "")
    )
    
    us.cities %>% 
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
      
      rbind(toronto) %>%
      
      ungroup() %>%
      
      
      #joining master table
      
      full_join(sche2(), by = c("City")) %>%
      
      
      select(Season, Date, Location, Team, Opponent, City, Latitude, Longitude, everything()) %>%
      
      arrange(Season, Team, Date) %>%
      
      group_by(Team, Season) %>%
      
      mutate(destLat = lag(Latitude), destLon = lag(Longitude)) %>%
      
      
      #correcting for missing coordinates for first games of the season for each team
      
      mutate(destLat = ifelse(is.na(destLat) & Team == "Atlanta Hawks", acities %>% filter(City == "Atlanta") %>% select(Latitude) %>% as.numeric(), 
                       ifelse(is.na(destLat) & Team == "Boston Celtics", acities %>% filter(City == "Boston") %>% select(Latitude) %>% as.numeric(),      
                       ifelse(is.na(destLat) & Team == "New Orleans Pelicans", acities %>% filter(City == "New Orleans") %>% select(Latitude) %>% as.numeric(), 
                       ifelse(is.na(destLat) & Team == "Houston Rockets", acities %>% filter(City == "Houston") %>% select(Latitude) %>% as.numeric(), 
                       ifelse(is.na(destLat) & Team == "Oklahoma City Thunder", acities %>% filter(City == "Oklahoma") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "New York Knicks", acities %>% filter(City == "New York") %>% select(Latitude) %>% as.numeric(),  
                       ifelse(is.na(destLat) & Team == "Charlotte Hornets", acities %>% filter(City == "Charlotte") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Miami Heat", acities %>% filter(City == "Miami") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Phoenix Suns", acities %>% filter(City == "Phoenix") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Utah Jazz", acities %>% filter(City == "Utah") %>% select(Latitude) %>% as.numeric(),  
                       ifelse(is.na(destLat) & Team == "Los Angeles Lakers", acities %>% filter(City == "Los Angeles") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Los Angeles Clippers", acities %>% filter(City == "Los Angeles") %>% select(Latitude) %>% as.numeric(),       
                       ifelse(is.na(destLat) & Team == "Dallas Mavericks", acities %>% filter(City == "Dallas") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Milwaukee Bucks", acities %>% filter(City == "Milwaukee") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Philadelphia 76ers", acities %>% filter(City == "Philadelphia") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Minnesota Timberwolves", acities %>% filter(City == "Minnesota") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Golden State Warriors", acities %>% filter(City == "San Francisco") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Portland Trail Blazers", acities %>% filter(City == "Portland") %>% select(Latitude) %>% as.numeric(),       
                       ifelse(is.na(destLat) & Team == "Denver Nuggets", acities %>% filter(City == "Denver") %>% select(Latitude) %>% as.numeric(),       
                       ifelse(is.na(destLat) & Team == "Sacramento Kings", acities %>% filter(City == "Sacramento") %>% select(Latitude) %>% as.numeric(),       
                       ifelse(is.na(destLat) & Team == "Detroit Pistons", acities %>% filter(City == "Detroit") %>% select(Latitude) %>% as.numeric(),       
                       ifelse(is.na(destLat) & Team == "Memphis Grizzlies", acities %>% filter(City == "Memphis") %>% select(Latitude) %>% as.numeric(),       
                       ifelse(is.na(destLat) & Team == "Cleveland Cavaliers", acities %>% filter(City == "Cleveland") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Chicago Bulls", acities %>% filter(City == "Chicago") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Orlando Magic", acities %>% filter(City == "Orlando") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Washington Wizards", acities %>% filter(City == "Washington D.C.") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Indiana Pacers", acities %>% filter(City == "Indiana") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "San Antonio Spurs", acities %>% filter(City == "San Antonio") %>% select(Latitude) %>% as.numeric(),
                       ifelse(is.na(destLat) & Team == "Toronto Raptors", acities %>% filter(City == "Toronto") %>% select(Latitude) %>% as.numeric(), 
                       ifelse(is.na(destLat) & Team == "Brooklyn Nets", acities %>% filter(City == "New York") %>% select(Latitude) %>% as.numeric(),       
                       destLat))))))))))))))))))))))))))))))) %>%
      
      mutate(destLon = ifelse(is.na(destLon) & Team == "Atlanta Hawks", acities %>% filter(City == "Atlanta") %>% select(Longitude) %>% as.numeric(), 
                       ifelse(is.na(destLon) & Team == "Boston Celtics", acities %>% filter(City == "Boston") %>% select(Longitude) %>% as.numeric(),      
                       ifelse(is.na(destLon) & Team == "New Orleans Pelicans", acities %>% filter(City == "New Orleans") %>% select(Longitude) %>% as.numeric(), 
                       ifelse(is.na(destLon) & Team == "Houston Rockets", acities %>% filter(City == "Houston") %>% select(Longitude) %>% as.numeric(), 
                       ifelse(is.na(destLon) & Team == "Oklahoma City Thunder", acities %>% filter(City == "Oklahoma") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "New York Knicks", acities %>% filter(City == "New York") %>% select(Longitude) %>% as.numeric(),  
                       ifelse(is.na(destLon) & Team == "Charlotte Hornets", acities %>% filter(City == "Charlotte") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Miami Heat", acities %>% filter(City == "Miami") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Phoenix Suns", acities %>% filter(City == "Phoenix") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Utah Jazz", acities %>% filter(City == "Utah") %>% select(Longitude) %>% as.numeric(),  
                       ifelse(is.na(destLon) & Team == "Los Angeles Lakers", acities %>% filter(City == "Los Angeles") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Los Angeles Clippers", acities %>% filter(City == "Los Angeles") %>% select(Longitude) %>% as.numeric(),       
                       ifelse(is.na(destLon) & Team == "Dallas Mavericks", acities %>% filter(City == "Dallas") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Milwaukee Bucks", acities %>% filter(City == "Milwaukee") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Philadelphia 76ers", acities %>% filter(City == "Philadelphia") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Minnesota Timberwolves", acities %>% filter(City == "Minnesota") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Golden State Warriors", acities %>% filter(City == "San Francisco") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Portland Trail Blazers", acities %>% filter(City == "Portland") %>% select(Longitude) %>% as.numeric(),       
                       ifelse(is.na(destLon) & Team == "Denver Nuggets", acities %>% filter(City == "Denver") %>% select(Longitude) %>% as.numeric(),       
                       ifelse(is.na(destLon) & Team == "Sacramento Kings", acities %>% filter(City == "Sacramento") %>% select(Longitude) %>% as.numeric(),       
                       ifelse(is.na(destLon) & Team == "Detroit Pistons", acities %>% filter(City == "Detroit") %>% select(Longitude) %>% as.numeric(),       
                       ifelse(is.na(destLon) & Team == "Memphis Grizzlies", acities %>% filter(City == "Memphis") %>% select(Longitude) %>% as.numeric(),       
                       ifelse(is.na(destLon) & Team == "Cleveland Cavaliers", acities %>% filter(City == "Cleveland") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Chicago Bulls", acities %>% filter(City == "Chicago") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Orlando Magic", acities %>% filter(City == "Orlando") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Washington Wizards", acities %>% filter(City == "Washington D.C.") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Indiana Pacers", acities %>% filter(City == "Indiana") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "San Antonio Spurs", acities %>% filter(City == "San Antonio") %>% select(Longitude) %>% as.numeric(),
                       ifelse(is.na(destLon) & Team == "Toronto Raptors", acities %>% filter(City == "Toronto") %>% select(Longitude) %>% as.numeric(), 
                       ifelse(is.na(destLon) & Team == "Brooklyn Nets", acities %>% filter(City == "New York") %>% select(Longitude) %>% as.numeric(),       
                       destLon))))))))))))))))))))))))))))))) %>%
      
      
      mutate(Route = ifelse(is.na(lag(City)) & Team == "Atlanta Hawks", paste(acities %>% filter(City == "Atlanta") %>% select(City), City, sep = " - "), 
                     ifelse(is.na(lag(City)) & Team == "Boston Celtics", paste(acities %>% filter(City == "Boston") %>% select(City), City, sep = " - "),      
                     ifelse(is.na(lag(City)) & Team == "New Orleans Pelicans", paste(acities %>% filter(City == "New Orleans") %>% select(City), City, sep = " - "), 
                     ifelse(is.na(lag(City)) & Team == "Houston Rockets", paste(acities %>% filter(City == "Houston") %>% select(City), City, sep = " - "), 
                     ifelse(is.na(lag(City)) & Team == "Oklahoma City Thunder", paste(acities %>% filter(City == "Oklahoma") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "New York Knicks", paste(acities %>% filter(City == "New York") %>% select(City), City, sep = " - "),  
                     ifelse(is.na(lag(City)) & Team == "Charlotte Hornets", paste(acities %>% filter(City == "Charlotte") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Miami Heat", paste(acities %>% filter(City == "Miami") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Phoenix Suns", paste(acities %>% filter(City == "Phoenix") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Utah Jazz", paste(acities %>% filter(City == "Utah") %>% select(City), City, sep = " - "),  
                     ifelse(is.na(lag(City)) & Team == "Los Angeles Lakers", paste(acities %>% filter(City == "Los Angeles") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Los Angeles Clippers", paste(acities %>% filter(City == "Los Angeles") %>% select(City), City, sep = " - "),       
                     ifelse(is.na(lag(City)) & Team == "Dallas Mavericks", paste(acities %>% filter(City == "Dallas") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Milwaukee Bucks", paste(acities %>% filter(City == "Milwaukee") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Philadelphia 76ers", paste(acities %>% filter(City == "Philadelphia") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Minnesota Timberwolves", paste(acities %>% filter(City == "Minnesota") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Golden State Warriors", paste(acities %>% filter(City == "San Francisco") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Portland Trail Blazers", paste(acities %>% filter(City == "") %>% select(City), City, sep = " - "),       
                     ifelse(is.na(lag(City)) & Team == "Denver Nuggets", paste(acities %>% filter(City == "Portland") %>% select(City), City, sep = " - "),       
                     ifelse(is.na(lag(City)) & Team == "Sacramento Kings", paste(acities %>% filter(City == "Sacramento") %>% select(City), City, sep = " - "),       
                     ifelse(is.na(lag(City)) & Team == "Detroit Pistons", paste(acities %>% filter(City == "Detroit") %>% select(City), City, sep = " - "),       
                     ifelse(is.na(lag(City)) & Team == "Memphis Grizzlies", paste(acities %>% filter(City == "Memphis") %>% select(City), City, sep = " - "),       
                     ifelse(is.na(lag(City)) & Team == "Cleveland Cavaliers", paste(acities %>% filter(City == "Cleveland") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Chicago Bulls", paste(acities %>% filter(City == "Chicago") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Orlando Magic", paste(acities %>% filter(City == "Orlando") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Washington Wizards", paste(acities %>% filter(City == "Washington D.C.") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Indiana Pacers", paste(acities %>% filter(City == "Indiana") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "San Antonio Spurs", paste(acities %>% filter(City == "San Antonio") %>% select(City), City, sep = " - "),
                     ifelse(is.na(lag(City)) & Team == "Toronto Raptors", paste(acities %>% filter(City == "Toronto") %>% select(City), City, sep = " - "), 
                     ifelse(is.na(lag(City)) & Team == "Brooklyn Nets", paste(acities %>% filter(City == "Brooklyn") %>% select(City), City, sep = " - "),       
                     paste(lag(City), City, sep = " - ")))))))))))))))))))))))))))))))) %>%
      
      mutate(Latitude = as.numeric(Latitude), Longitude = as.numeric(Longitude), destLat = as.numeric(destLat), destLon = as.numeric(destLon)) %>%
      
      rowwise() %>% 
      mutate(dist = geosphere::distm(c(destLon, destLat), c(Longitude, Latitude), fun=distHaversine)) %>% 
      mutate(Distance = paste(round(dist * 0.000621,0), "miles", sep = " ")) %>%
      
      mutate(Route = ifelse(dist == 0, "No Travel", Route)) %>%
      
      select(-dist) %>%
      
      mutate(Density = ifelse(Density == "1", "1 Day Rest", 
                              ifelse(Density == "2", "2 Days Rest", 
                                     ifelse(Density == "3+", "3+ Days Rest", Density)))) %>%
      
      mutate(`Opp Density` = ifelse(`Opp Density` == "1", "1 Day Rest", 
                                    ifelse(`Opp Density` == "2", "2 Days Rest", 
                                           ifelse(`Opp Density` == "3+", "3+ Days Rest", `Opp Density`)))) %>%
      
      select(Season, Month, Date, Location, Arena, Team, Opponent, City, `W/L`, Team_pts, Opp_pts, Density, `Opp Density`, Index, `Opp Index`, movIndex, `Opp movIndex`, Latitude, Longitude, destLat, destLon, Route, Distance)
    
    
  })
  
###################################################

  
#Team by Team tab
###################################################  
  
  #mini table added to the left sidebar menu#####
  output$team_table_density <- DT::renderDataTable({
  
    num <- sche2() %>%
   
      filter(Team == input$team_filter) %>%
      
      select(Team, Density) %>%
      na.omit() %>%
      group_by(Density) %>%
      tally() %>%
      select(Type = Density, n)
    
    num[is.na(num)] <- 0
    
    
    
    num2 <- sche2() %>%
      filter(Team == input$team_filter) %>%
      select(Opponent, `Opp Density`) %>%
      na.omit() %>%
      group_by(`Opp Density`) %>%
      tally() %>%
      select(Type = `Opp Density`, Opp = n)
    
    num2[is.na(num2)] <- 0
    
    
    b <- full_join(num, num2) %>%
      
      
      mutate(order = ifelse(Type == "3IN4-B2B", 1, 
                            ifelse(Type == "3IN4", 2,
                                   ifelse(Type == "B2B", 3,
                                          ifelse(Type == "1", 4,
                                                 ifelse(Type == "2", 5,
                                                        ifelse(Type == "3+", 6, ""))))))) %>%
      arrange(order) %>%
      select(-order) %>%
      
      replace_na(list(n = 0, Opp = 0)) %>%
    
      
      formattable(list(
        
        Type = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                                  ifelse(x == "3IN4", "#ff3300", 
                                                                                    ifelse(x == "B2B", "#cc6600",
                                                                                      ifelse(x == "1", "#cc9900",
                                                                                        ifelse(x == "2", "#cccc00",
                                                                                          ifelse(x == "3+", "#ccff00", "transparent"))))))))))
        
      
    
    formattable::as.datatable(b, 
                 rownames = FALSE,
                 colnames = c("", input$team_filter, "Opposition"),
                 extensions = 'Responsive',
                 caption = HTML("Total count of density index per season for <br> the selected team vs opponents."),
                 options = list(dom = 't',
                                bSort=FALSE,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#17202a', 'color': '#fff'});",
                                  "}"))) %>%
      
      formatStyle('Type', fontWeight = 'bold', backgroundColor = "#283747") %>%
      formatStyle('n', backgroundColor =  '#bfc9ca', color = " #212f3d ") %>%
      formatStyle('Opp', backgroundColor =  '#bfc9ca', color = " #212f3d ") 
    
  })
  
  
  ###############################################
  
  #game card tab############
  
  #filter for the tab 
  output$date <- renderUI({
    
    req(input$team_filter)
    
    choices <- cities() %>% 
      filter(Team == input$team_filter) %>% 
      select(Date, Team, Opponent) %>% 
      arrange(Date) %>%
      mutate(Date = as.character(Date)) %>% unique()
    
    pickerInput(
      inputId = "date_filter",
      label = "Select Date", 
      choices = choices$Date, 
      #selected = , 
      choicesOpt = list(style = rep(("color: black; background: deepskyblue3"),100), subtext = paste(choices$Team, choices$Opponent, sep = " vs ")),
      multiple = F)
    
  })
  
  
  
  #map plot
  output$map_plot <- renderPlot({
    
    req(input$date_filter)
    req(input$team_filter)
    
    acitiesc <- cities() %>% filter(Team == input$team_filter)
    
    acitiesd <- cities() %>% 
      filter(Team == input$team_filter) %>% 
      filter(Date == input$date_filter)
    
    #create basemap
    maps::map("world", regions=c("usa"), fill=T, col="#17202a", bg="transparent", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
    points(acities$Longitude, acities$Latitude, pch=7, cex=1, col="chocolate1")
    
    
    for (i in (1:dim(acitiesc)[1])) { 
      
      inter <- geosphere::gcIntermediate(c(acitiesc$destLon[i], acitiesc$destLat[i]), c(acitiesc$Longitude[i], acitiesc$Latitude[i]), n=500)
      lines(inter, lwd=0.5, col="grey30", lty=1)
      
    }
    
    inter2 <- geosphere::gcIntermediate(c(acitiesd$destLon, acitiesd$destLat), c(acitiesd$Longitude, acitiesd$Latitude), n=200)
    lines(inter2, col="#ccff00", lwd = 5)
    
    
  })
  
  #code for game card dashboard
  
  city <- reactive({
    
    validate(
      need(input$team_filter, ""),
      need(input$date_filter, "")
    )
    
    
    cities() %>% 
      
      full_join(Logos, by = c("Team", "Opponent", "Date")) %>% 
      
      filter(Team == input$team_filter) %>% 
      
      select(Team_Logo, Opp_Logo, Date, Location, Arena, Team, Opponent, City, Outcome = `W/L`, Points = Team_pts, `Opp Points` = Opp_pts, Density, `Opp Density`, Index, `Opp Index`, movIndex, `Opp movIndex`, Route, Distance) %>%
      
      filter(Date == input$date_filter) %>%
      
      mutate(Points = ifelse(is.na(Points), "", paste(Points, "pts", sep = " ")), `Opp Points` = ifelse(is.na(`Opp Points`), "", paste(`Opp Points`, "pts", sep = " ")))
    
  })
  
  #code for individual items
  
  
  #team logo
  output$team_logo <- renderUI({
    
    HTML(city()$Team_Logo)
    
  })
  
  #team logo
  output$opponent_logo <- renderUI({
    
    HTML(city()$Opp_Logo)
    
  })
  
  #team
  output$team <- renderUI({
    
    tags$h1(city()$Team, style = "color:white")
    
  })
  
  #team
  output$opponent <- renderUI({
    
    tags$h1(city()$Opponent, style = "color:white")
    
  })
  
  #Team location
  output$location_team <- renderUI({
    
    if(city()$Location == "Home"){
      
      a <- paste("<span style=color:lightblue>", city()$Location, "</span>")
      
      
    }else{
      
      a <- paste("<span style=color:salmon>", city()$Location, "</span>")
      
    }
    
    tags$h4(HTML(a))
    
  })
  
  #Opponent location
  output$location_opponent <- renderUI({
    
    if(city()$Location == "Home"){
      
      a <- paste("<span style=color:salmon>", "Away", "</span>")
      
      
    }else{
      
      a <- paste("<span style=color:lightblue>", "Home", "</span>")
      
    }
    
    tags$h4(HTML(a))
    
  })
  
  #Team Points
  output$team_points <- renderUI({
    
    tags$h3(city()$Points)
    
  })
  
  #Team Points
  output$opponent_points <- renderUI({
    
    tags$h3(city()$`Opp Points`)
    
  })
  
  #Team density
  output$density_team <- renderUI({
    
    if(city()$Density == "3IN4-B2B"){
      
      a <- paste("<span style=color:#ff0000>", city()$Density, "</span>")
      
    }else if(city()$Density == "3IN4"){
      
      a <- paste("<span style=color:#ff3300>", city()$Density, "</span>")
      
    }else if(city()$Density == "B2B"){
    
    a <- paste("<span style=color:#cc6600>", city()$Density, "</span>")
    
    }else if(city()$Density == "1 Day Rest"){
      
      a <- paste("<span style=color:#cc9900>", city()$Density, "</span>")
    
    }else if(city()$Density == "2 Days Rest"){
      
      a <- paste("<span style=color:#cccc00>", city()$Density, "</span>")
      
    }else if(city()$Density == "3+ Days Rest"){
      
      a <- paste("<span style=color:#ccff00>", city()$Density, "</span>")
    
    }else{
      
      a <- paste("<span style=color:grey>", "Density Unknown", "</span>")
      
    }
    
    tags$h3(HTML(a))
    
    
  })
  
  #opponent density
  output$density_opponent <- renderUI({
    
    if(city()$`Opp Density` == "3IN4-B2B"){
      
      a <- paste("<span style=color:#ff0000>", city()$`Opp Density`, "</span>")
      
    }else if(city()$`Opp Density` == "3IN4"){
      
      a <- paste("<span style=color:#ff3300>", city()$`Opp Density`, "</span>")
      
    }else if(city()$`Opp Density` == "B2B"){
      
      a <- paste("<span style=color:#cc6600>", city()$`Opp Density`, "</span>")
      
    }else if(city()$`Opp Density` == "1 Day Rest"){
      
      a <- paste("<span style=color:#cc9900>", city()$`Opp Density`, "</span>")
      
    }else if(city()$`Opp Density` == "2 Days Rest"){
      
      a <- paste("<span style=color:#cccc00>", city()$`Opp Density`, "</span>")
      
    }else if(city()$`Opp Density` == "3+ Days Rest"){
      
      a <- paste("<span style=color:#ccff00>", city()$`Opp Density`, "</span>")
      
    }else{
      
      a <- paste("<span style=color:grey>", "Density Unknown", "</span>")
      
    }
    
    tags$h3(HTML(a))
    
    
  })
  
  
  #Team Index
  output$team_index <- renderUI({
    
    if(city()$Index > city()$`Opp Index` ){
      
      a <- paste(icon("sort-up"), "<span style=color:white>", city()$Index, "</span>")
      
    }else if(city()$Index < city()$`Opp Index` ){
      
      a <- paste("<span style=color:white>", city()$Index, "</span>")
      
    }else{
      
      a <- paste("<span style=color:white>", city()$Index, "</span>")
    }
    
    tags$h3(HTML(a))
    
  })
  
  #opponent Index
  output$opponent_index <- renderUI({
    
    if(city()$Index < city()$`Opp Index` ){
      
      a <- paste(icon("sort-up"), "<span style=color:white>", city()$`Opp Index`, "</span>")
      
    }else if(city()$Index > city()$`Opp Index` ){
      
      a <- paste("<span style=color:white>", city()$`Opp Index`, "</span>")
      
    }else{
      
      a <- paste("<span style=color:white>", city()$`Opp Index`, "</span>")
    }
    
    tags$h3(HTML(a))
    
  })
  
  
  #Team movIndex
  output$team_movindex <- renderUI({
    
    if(city()$movIndex > city()$`Opp movIndex` ){
      
      a <- paste(icon("sort-up"), "<span style=color:white; font-size:16px>", city()$movIndex, "</span>")
      
    }else if(city()$movIndex < city()$`Opp movIndex` ){
      
      a <- paste("<span style=color:white; font-size:16px>", city()$movIndex, "</span>")
      
   
    }else{
      
      a <- paste("<span style=color:grey; font-size:8px>", "Not enough games for moving index.", "</span>")
      
    }
    
    HTML(a)
    
  })
  
  #Opp  movIndex
  output$opponent_movindex <- renderUI({
    
    if(city()$movIndex < city()$`Opp movIndex` ){
      
      a <- paste(icon("sort-up"), "<span style=color:white; font-size:16px>", city()$`Opp movIndex`, "</span>")
      
    }else if(city()$movIndex > city()$`Opp movIndex` ){
      
      a <- paste("<span style=color:white; font-size:16px>", city()$`Opp movIndex`, "</span>")
      
      
    }else{
      
      a <- paste("<span style=color:grey; font-size:8px>", "Not enough games for moving index.", "</span>")
      
    }
    
    HTML(a)
    
  })
  
  #text for index
  output$text_index1 <- renderUI({
    
    tags$h5("Game Index:")
    
  })
  
  #text for index
  output$text_index2 <- renderUI({
    
    tags$h5("Game Index:")
    
  })
  
  #text for index
  output$text_movindex1 <- renderUI({
    
    tags$h5(paste(input$rolling, "Days Moving Index:", sep = " "))
    
  })
  
  #text for index
  output$text_movindex2 <- renderUI({
    
    tags$h5(paste(input$rolling, "Days Moving Index:", sep = " "))
    
  })
  
  #winning label logic
  output$team_label <- renderUI({
    
    if(city()$Outcome == "W"){
      
      a <- paste("<span style= background-color:seagreen; font-color:white>", " Winner ", "</span>")
      
      
    }else{
      
     a <- tags$br()
      
    }
    
    HTML(a)
    
    
  })
  
  #winning label logic
  output$opponent_label <- renderUI({
    
    if(city()$Outcome == "W"){
      
      a <- tags$br()
      
      
    }else{
      
      a <- paste("<span style= background-color:seagreen; font-color:white>", " Winner ", "</span>")
      
      
    }
    
    HTML(a)
    
    
  })
  
  #city where game is played at
  output$city <- renderUI({
    
    req(input$team_filter)
    req(input$season_filter)
    
    if(is.na(city()$City)){
      
      p(" ")
      
    }else{
    
    tags$h1(city()$City, style = "color:white")
      
    }
    
  })
  
  #arena where game is played at
  output$arena <- renderUI({
    
    req(input$team_filter)
    req(input$season_filter)
    
    if(is.na(city()$Arena)){
      
      p(" ")
      
    }else{
    
    tags$h5(city()$Arena)
      
    }
    
  })
  
  #route where game is played at
  output$route <- renderUI({
    
    req(input$team_filter)
    req(input$season_filter)
    
    
    if(city()$Route == "No Travel"){
      
      a <- paste("<span style= background-color:darkred; font-color:white>", " No Pre-Game Travel ", "</span>")
      
      
    }else{
      
      a <- paste(icon("plane-departure"), "<br>", "<span style=font-color:white>", city()$Route, "</span>", sep = " ")
      
      
    }
    
    HTML(a)
    
    
  })
  
  #distance travelled
  output$distance <- renderUI({
    
    req(input$team_filter)
    req(input$season_filter)
    
    
    
    if(city()$Route == "No Travel"){
      
      a <- paste("<span style= font-color:grey>", "-", "</span>")
      
      
    }else{
      
      a <- city()$Distance
      
      
    }
    
    HTML(a)
    
    
    
  })
  
  ###############################################
  
  
  #schedule table tab####
  output$team_table <- DT::renderDataTable({
    
    a <- sche2() %>% 
    
      
    filter(Team == input$team_filter) %>%
    
    mutate(`Density` = ifelse(is.na(`Density`), "", `Density`)) %>%
    mutate(`Opp Density` = ifelse(is.na(`Opp Density`), "", `Opp Density`)) %>%
    mutate(`Index` = ifelse(is.na(`Index`), "", `Index`)) %>%
    mutate(`movIndex` = ifelse(`movIndex` == 0, "", `movIndex`)) %>%
    mutate(`Opp movIndex` = ifelse(`Opp movIndex` == 0, "", `Opp movIndex`)) %>%
    replace_na(list(Team_pts = "-", Opp_pts = "-")) %>%
    mutate(Score = paste(Team_pts," - ", Opp_pts)) %>%
    select(-Team_pts, -Opp_pts) %>%
      
    select(Month, Date, Location, Team, Opponent, Outcome = `W/L`, Score, Density, `Opp Density`, Index, movIndex, `Opp movIndex`) %>%
    
      
    formattable(
      
      list(
      `movIndex` = color_tile("springgreen", "red"),
      `Opp movIndex` = color_tile("springgreen", "red"),
      `Index` = color_text("springgreen", "red"),
    
      
      Outcome = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(is.na(x), "transparent",
                                                                   ifelse(x == "W", "springgreen", "red")))),
      
      Location = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "Home", "lightblue", "salmon"))),
      
      Density = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                   ifelse(x == "3IN4", "#ff3300", 
                                                                          ifelse(x == "B2B", "#cc6600",
                                                                                 ifelse(x == "1", "#cc9900",
                                                                                        ifelse(x == "2", "#cccc00",
                                                                                               ifelse(x == "3+", "#ccff00", "transparent")))))))), 
      
      
    `Opp Density` = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                   ifelse(x == "3IN4", "#ff3300", 
                                                                          ifelse(x == "B2B", "#cc6600",
                                                                                 ifelse(x == "1", "#cc9900",
                                                                                        ifelse(x == "2", "#cccc00",
                                                                                               ifelse(x == "3+", "#ccff00", "transparent"))))))))
    
      
    )) 
    
    
    formattable::as.datatable(a, 
                 rownames = FALSE,
                 extensions = 'Responsive',
                 options = list(dom = 't', 
                                pageLength = 90, 
                                bSort=FALSE,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#17202a', 'color': '#fff'});",
                                  "}"))) %>%
      
    formatStyle('Team', fontWeight = 'bold') %>%
    formatStyle('Score', fontWeight = 'bold') %>%
    formatStyle('Density', fontWeight = 'bold') %>%
    formatStyle('Index', fontWeight = 'bold') %>%
    formatStyle('movIndex', fontWeight = 'bold', color = "black") %>%
    formatStyle('Opp movIndex', fontWeight = 'bold', color = "black") %>%
    formatStyle('Outcome', fontWeight = 'bold') %>%
      
    formatStyle(c(2,3,5, 7, 9, 10,12), `border-right` = "solid 2px")
    
  })
  
  ################################################
  
 #plot tab#####
  
  output$team_plot <- renderPlotly({
    
    
    #comparing rolling stress between local and opp teams
    aa <- sche2() %>% 
     
      select(`W/L`, Team, Opponent, Date, `Moving Index Density` = `movIndex`, `Opponent Moving Index` = `Opp movIndex`) %>%
      gather(Metric, Value, -Team, -Opponent, -Date, -`W/L`) %>%
      filter(Value > 0) %>%
      na.omit() %>%
      filter(Team == input$team_filter) %>%
      mutate(Teams = ifelse(Metric == "movIndex", paste(Team, "vs", Opponent, sep = " "), paste(Opponent, "vs", Team, sep = " ")))
    
    
    a1 <- ggplot(aa) +
      geom_point(aes(Metric, Value, color = Metric, text = paste(Date, "</br>", Teams)), position = position_jitter(width = 0.05), alpha = 0.5) +
      geom_boxplot(aes(Metric, Value, fill = Metric), outlier.shape = NA, size = 0.2, alpha = 0.5) +
      scale_fill_manual(name = "", values=c("orange", "deepskyblue4")) +
      scale_color_manual(name = "", values=c("orange", "deepskyblue4")) +
      ggthemes::theme_economist() +
      xlab("") + ylab("") +
      labs(title = HTML(paste("Longitudinal Comparison of Index for", tags$span(style = "color:darkorange", input$team_filter), "vs", tags$span(style = "color:steelblue", "Opposition"), "for a",  paste(tags$span(style = "color:red", input$rolling), "games moving Window"))), sep = " ") +
      theme(axis.text.y=element_blank(), axis.text.x = element_blank()) +
      theme(legend.position = "none", plot.title = element_text(face = "bold", size = 20, color = "grey20"))
      
    
    a1 <- ggplotly(a1)
    
    
    a2 <- ggplot() +
      geom_line(aa, mapping = aes(x = Date, y = Value, color = Metric), size = 0.5, linetype = 1) +
      geom_area(aa, mapping = aes(x = Date, y = Value, fill = Metric, color = Metric), alpha = .1, position = 'identity') +
      geom_point(aa, mapping = aes(x = Date, y = Value, color = Metric, text = Teams), size = 1.5) +
      geom_rug(aa %>% filter(Metric == "Moving Index Density"), mapping = aes(x = Date, color = `W/L`)) +
      scale_color_manual(name = "", values=c("red", "orange", "deepskyblue4", "darkgreen")) +
      scale_fill_manual(name = "", values=c("orange", "deepskyblue4")) +
      ggthemes::theme_economist() +
      xlab("") + ylab("") +
      theme(legend.position = "none")
      
    
    a2 <- ggplotly(a2)
    
    a3 <- subplot(a2, a1, widths = c(.8,.2))
    
    config(a3, displayModeBar = FALSE)
    
    
  })
  
  
  ################################################
  
# comparison of wins by game time tab###########
  
  output$w_l_table <- DT::renderDataTable({
  
  #code to look at winning and loosing by type of rest combination
  
  a <- sche2() %>% 
    
    filter(Team == input$team_filter) %>%
    select(Density, `Opp Density`, `W/L`) %>% na.omit()
  
  #3+ series
  
  `3+3+` <- a %>% filter(Density == "3+" & `Opp Density` == "3+") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3+ - 3+")
  
  `3+2` <- a %>% filter(Density == "3+" & `Opp Density` == "2") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3+ - 2")
  
  `3+1` <- a %>% filter(Density == "3+" & `Opp Density` == "1") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3+ - 1")
  
  `3+B2B` <- a %>% filter(Density == "3+" & `Opp Density` == "B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3+ - B2B")
  
  `3+3IN4-B2B` <- a %>% filter(Density == "3+" & `Opp Density` == "3IN4-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3+ - 3IN4-B2B")
  
  `3+3IN4` <- a %>% filter(Density == "3+" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3+ - 3IN4")
  
  `3+a` <- full_join(`3+3+`, `3+2`) %>% full_join(`3+1`) %>% full_join(`3+B2B`) %>% full_join(`3+3IN4-B2B`) %>% full_join(`3+3IN4`)
  
  #2 series
  
  `23+` <- a %>% filter(Density == 2 & `Opp Density` == "3+") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "2 - 3+")
  
  `22` <- a %>% filter(Density == "2" & `Opp Density` == "2") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "2 - 2")
  
  `21` <- a %>% filter(Density == "2" & `Opp Density` == "1") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "2 - 1")
  
  `2B2B` <- a %>% filter(Density == "2" & `Opp Density` == "B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "2 - B2B")
  
  `23IN4-B2B` <- a %>% filter(Density == "2" & `Opp Density` == "3IN4-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "2 - 3IN4-B2B")
  
  `23IN4` <- a %>% filter(Density == "2" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "2 - 3IN4")
  
  `2a` <- full_join(`23+`, `22`) %>% full_join(`21`) %>% full_join(`2B2B`) %>% full_join(`23IN4-B2B`) %>% full_join(`23IN4`)
  
  #1 series
  
  
  `13+` <- a %>% filter(Density == 1 & `Opp Density` == "3+") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "1 - 3+")
  
  `12` <- a %>% filter(Density == "1" & `Opp Density` == "2") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "1 - 2")
  
  `11` <- a %>% filter(Density == "1" & `Opp Density` == "1") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "1 - 1")
  
  `1B2B` <- a %>% filter(Density == "1" & `Opp Density` == "B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "1 - B2B")
  
  `13IN4-B2B` <- a %>% filter(Density == "1" & `Opp Density` == "3IN4-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "1 - 3IN4-B2B")
  
  `13IN4` <- a %>% filter(Density == "1" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "1 - 3IN4")
  
  `1a` <- full_join(`13+`, `12`) %>% full_join(`11`) %>% full_join(`1B2B`) %>% full_join(`13IN4-B2B`) %>% full_join(`13IN4`)
  
  #3in4 series
  
  `3IN43+` <- a %>% filter(Density == "3IN4" & `Opp Density` == "3+") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4 - 3+")
  
  `3IN42` <- a %>% filter(Density == "3IN4" & `Opp Density` == "2") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4 - 2")
  
  `3IN41` <- a %>% filter(Density == "3IN4" & `Opp Density` == "1") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4 - 1")
  
  `3IN4B2B` <- a %>% filter(Density == "3IN4" & `Opp Density` == "B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4 - B2B")
  
  `3IN43IN4-B2B` <- a %>% filter(Density == "3IN4" & `Opp Density` == "3IN4-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4 - 3IN4-B2B")
  
  `3IN43IN4` <- a %>% filter(Density == "3IN4" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4 - 3IN4")
  
  `3IN4a` <- full_join(`3IN43+`, `3IN42`) %>% full_join(`3IN41`) %>% full_join(`3IN4B2B`) %>% full_join(`3IN43IN4-B2B`) %>% full_join(`3IN43IN4`)
  
  #3in4-B2B series
  
  `3IN4-B2B3+` <- a %>% filter(Density == "3IN4-B2B" & `Opp Density` == "3+") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4-B2B - 3+")
  
  `3IN4-B2B2` <- a %>% filter(Density == "3IN4-B2B" & `Opp Density` == "2") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4-B2B - 2")
  
  `3IN4-B2B1` <- a %>% filter(Density == "3IN4-B2B" & `Opp Density` == "1") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4-B2B - 1")
  
  `3IN4-B2BB2B` <- a %>% filter(Density == "3IN4-B2B" & `Opp Density` == "B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4-B2B - B2B")
  
  `3IN4-B2B3IN4-B2B` <- a %>% filter(Density == "3IN4-B2B" & `Opp Density` == "3IN4-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4-B2B - 3IN4-B2B")
  
  `3IN4-B2B3IN4` <- a %>% filter(Density == "3IN4-B2B" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4-B2B - 3IN4")
  
  `3IN4-B2Ba` <- full_join(`3IN4-B2B3+`, `3IN4-B2B2`) %>% full_join(`3IN4-B2B1`) %>% full_join(`3IN4-B2BB2B`) %>% full_join(`3IN4-B2B3IN4-B2B`) %>% full_join(`3IN4-B2B3IN4`)
  
  #B2B series
  
  `B2B3+` <- a %>% filter(Density == "B2B" & `Opp Density` == "3+") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "B2B - 3+")
  
  `B2B2` <- a %>% filter(Density == "B2B" & `Opp Density` == "2") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "B2B - 2")
  
  `B2B1` <- a %>% filter(Density == "B2B" & `Opp Density` == "1") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "B2B - 1")
  
  `B2BB2B` <- a %>% filter(Density == "B2B" & `Opp Density` == "B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "B2B - B2B")
  
  `B2B3IN4-B2B` <- a %>% filter(Density == "B2B" & `Opp Density` == "3IN4-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "B2B - 3IN4-B2B")
  
  `B2B3IN4` <- a %>% filter(Density == "B2B" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "B2B - 3IN4")
  
  `B2Ba` <- full_join(`B2B3+`, `B2B2`) %>% full_join(`B2B1`) %>% full_join(`B2BB2B`) %>% full_join(`B2B3IN4-B2B`) %>% full_join(`B2B3IN4`)
  
  
  
  ab <- full_join(`3+a`, `2a`) %>% full_join(`1a`) %>% full_join(`3IN4a`) %>% full_join(`3IN4-B2Ba`) %>% full_join(B2Ba) %>% 
    spread(`W/L`, no_rows) 
  
  ab[is.na(ab)] <- 0
  
  aaab <- ab %>%
    separate(Type, c("Team", "-", "Opponent"), sep = " ", remove = FALSE) %>%
    select(-Type, -`-`) %>%
    
    mutate(order = ifelse(Team == "3IN4-B2B", 1, 
                          ifelse(Team == "3IN4", 2,
                                 ifelse(Team == "B2B", 3,
                                        ifelse(Team == "1", 4,
                                               ifelse(Team == "2", 5,
                                                      ifelse(Team == "3+", 6, ""))))))) %>%
    arrange(order) %>%
    select(-order) %>% 
    select(Team, Opponent, W, L) %>%
    
    formattable(list(
      
      Team = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                                ifelse(x == "3IN4", "#ff3300", 
                                                                                       ifelse(x == "B2B", "#cc6600",
                                                                                              ifelse(x == "1", "#cc9900",
                                                                                                     ifelse(x == "2", "#cccc00",
                                                                                                            ifelse(x == "3+", "#ccff00", "transparent")))))))),
      
      Opponent = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                             ifelse(x == "3IN4", "#ff3300", 
                                                                                    ifelse(x == "B2B", "#cc6600",
                                                                                           ifelse(x == "1", "#cc9900",
                                                                                                  ifelse(x == "2", "#cccc00",
                                                                                                         ifelse(x == "3+", "#ccff00", "transparent"))))))))
    ))
      
  formattable::as.datatable(aaab, 
                 rownames = FALSE,
                 extensions = 'Responsive',
                 colnames = c(input$team_filter, "Opponent", "W", "L"),
                 caption = "Game outcomes for all game density combinations for games played by the selected team and season.",
                 options = list(dom = 't',
                                bSort=FALSE,
                                pageLength = 20,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#17202a', 'color': '#fff'});",
                                  "}"))) %>%
      
      formatStyle('Team', fontWeight = 'bold', backgroundColor = "#283747") %>%
      formatStyle('Opponent', fontWeight = 'bold', backgroundColor =  '#283747') %>%
      formatStyle('L', backgroundColor =  '#bfc9ca', color = "darkred") %>%
      formatStyle('W', backgroundColor =  '#bfc9ca', color = "darkgreen")
      
    
  
  })
  
  ################################################
  
  # comparison of wins by Location#######
  
  output$H_A_table <- DT::renderDataTable({
  
  cccb <- sche2() %>% 
   
    filter(Team == input$team_filter) %>%
    select(Location, Outcome = `W/L`) %>%
    group_by(Location, Outcome) %>%
    tally() %>%
    spread(Outcome, n) %>%
    select(Location, W, L) %>%
    
      
      formattable(list(
        
        Location = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "Home", "lightblue", "salmon")))
        
      ))
    
  formattable::as.datatable(cccb, 
                 rownames = FALSE,
                 extensions = 'Responsive',
                 colnames = c(input$team_filter, "W", "L"),
                 caption = "Game outcome by location.",
                 options = list(dom = 't',
                                bSort=FALSE,
                                pageLength = 5,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#17202a', 'color': '#fff'});",
                                  "}"))) %>%
      
      formatStyle('Location', fontWeight = 'bold', backgroundColor = "#283747") %>%
      formatStyle('L', backgroundColor =  '#bfc9ca', color = "darkred") %>%
      formatStyle('W', backgroundColor =  '#bfc9ca', color = "darkgreen")
  
  
  })
  
  
  
  
  #################################################
  
  #all teams tab
  
  #################################################
  
  #density table tab#######
  
  output$all_teams <- DT::renderDataTable({
    
    req(input$location_filter)
    
    #table showing count of density type for all teams
    df1 <- sche2() %>%
     
      filter(Location %in% input$location_filter) %>%
      select(Team, Density) %>%
      na.omit() %>%
      group_by(Team, Density) %>%
      tally() %>%
      select(`Type` = Density, n) %>%
      
      spread(Type, n) %>%
      select(val = Team, `3+`, `2`, `1`, `B2B`, `3IN4`, `3IN4-B2B`) %>%
      ungroup() %>%
      
      replace_na(list(`3+` = 0, `2` = 0, `1` = 0, `B2B` = 0, `3IN4` = 0, `3IN4-B2B` = 0)) 
      
    
    all <- full_join(df1, df) %>%
      select(Team = img, -val, `3+`, `2`, `1`, `B2B`, `3IN4`, `3IN4-B2B`) %>%
      formattable()
    
   
    
    formattable::as.datatable(all, 
                 rownames = FALSE,
                 extensions = 'Responsive',
                 colnames = c("Team", "3+", "2", "1", "B2B", "3IN4", "3IN4-B2B"),
                 caption = "Density Type. Count by team for the selected season",
                 options = list(dom = 't',
                                bSort=F,
                                pageLength = 35,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#17202a', 'color': '#fff'});",
                                  "}"))) %>%
      
      formatStyle('Team', backgroundColor =  '#bfc9ca', color = "black", fontWeight = 'bold') %>%
      formatStyle('3IN4-B2B', backgroundColor =  '#ff7f7f', color = "black", fontWeight = 'bold') %>%
      formatStyle('3IN4', backgroundColor =  '#ff9999', color = "black", fontWeight = 'bold') %>%
      formatStyle('B2B', backgroundColor =  '#ffb2b2', color = "black", fontWeight = 'bold') %>%
      formatStyle('1', backgroundColor =  '#ffcccc', color = "black", fontWeight = 'bold') %>%
      formatStyle('2', backgroundColor =  '#ffe5e5', color = "black", fontWeight = 'bold') %>%
      formatStyle('3+', backgroundColor =  '#ffffff', color = "black", fontWeight = 'bold')
    
  })

  ################################################
  
  #heatmap####
  
  
  output$heatmap <- renderPlot({
  
  a <- sche2() %>%
      select(Team, movIndex) %>%
      group_by(Team) %>%
      mutate(ID = row_number()) %>%
      filter(ID > input$rolling) %>%
      ungroup() %>%
      mutate(Team = as.factor(Team)) 
    
 ggplot(a, aes(x=reorder(ID, Team), y = Team, fill=movIndex)) +
    geom_tile(colour="white",size=0.25) +
    labs(x="\n Regular Season Games \n", y="", title = paste("\n", input$rolling, "Games Rolling Density Index for all Regular Season Games in", input$season_filter, sep = " "), 
         subtitle = "\n Start of Season >", caption = "https://josedv.shinyapps.io/NBASchedule/ \n") +
    scale_y_discrete(expand=c(0,0)) +
    scale_x_discrete(expand=c(0,0)) +
    scale_fill_gradient(low = "springgreen", high = "red") +
    theme_grey(base_size=8) +
    guides(fill=guide_legend(title="Density\nRolling Index", reverse = TRUE))+
    theme(
      legend.text=element_text(face="bold", color = "grey", size = 10),
      axis.ticks=element_line(size=0.4),
      plot.background=element_rect(fill = "#273746"),
      axis.text.y = element_text(size = 12, color = "lightgray"),
      legend.background = element_blank(),
      axis.text.x = element_text(size = 10, color = "grey"),
      plot.title = element_text(size = 24, color = "white"),
      plot.caption = element_text(size = 12, color = "lightblue"),
      plot.subtitle = element_text(size = 12, color = "darkgray"),
      axis.title.x = element_text(size = 12, color = "darkgray", hjust = 1),
      legend.title = element_text(color = "gray", size = 14, face = "bold"),
      panel.border=element_blank())
  
 ####################################################
    
  })
  
  
  ##################################################
  
  #hide loader after website is rendered####
  Sys.sleep(6)
  hide_waiter()
  ################################################
  
}


##################################################

#line to create the app
shinyApp(ui, server)


###################################################

