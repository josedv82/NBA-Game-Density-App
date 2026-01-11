
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
library(httr)
library(jsonlite)

#################################################################################

#loading data from NBA stats API (schedule and scores)####

current_date <- Sys.Date()
current_year <- lubridate::year(current_date)
current_end_year <- if (lubridate::month(current_date) >= 10) current_year + 1 else current_year
season_years <- seq(current_end_year - 4, current_end_year)
season_labels <- sprintf("%d-%02d", season_years - 1, season_years %% 100)

fetch_league_games <- function(season_label) {
  response <- httr::GET(
    url = "https://stats.nba.com/stats/leaguegamefinder",
    query = list(
      LeagueID = "00",
      Season = season_label,
      SeasonType = "Regular Season"
    ),
    httr::add_headers(
      `User-Agent` = "Mozilla/5.0",
      Referer = "https://www.nba.com/",
      Origin = "https://www.nba.com"
    )
  )
  httr::stop_for_status(response)
  payload <- httr::content(response, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(payload)
  tibble::as_tibble(data$resultSets[[1]]$rowSet, .name_repair = "minimal") %>%
    rlang::set_names(data$resultSets[[1]]$headers) %>%
    mutate(Season = season_label)
}

games_raw <- purrr::map_dfr(season_labels, fetch_league_games)

games <- games_raw %>%
  mutate(
    Date = as.Date(GAME_DATE),
    Location = ifelse(stringr::str_detect(MATCHUP, " vs\\. "), "Home", "Away"),
    Team = TEAM_NAME,
    Team_pts = PTS
  ) %>%
  arrange(Season, Team, Date) %>%
  group_by(Season, Team) %>%
  mutate(
    Rest = pmax(as.integer(Date - lag(Date)) - 1, 0)
  ) %>%
  ungroup() %>%
  mutate(Rest = as.character(Rest))

dat <- games %>%
  select(GAME_ID, Season, Date, Time = GAME_DATE, Team, Location, Rest) %>%
  tidyr::pivot_wider(names_from = Location, values_from = c(Team, Rest), names_sep = " ") %>%
  rename(
    `Road Team` = `Team Away`,
    `Home Team` = `Team Home`,
    `Away Rest` = `Rest Away`,
    `Home Rest` = `Rest Home`
  ) %>%
  mutate(Arena = NA_character_) %>%
  select(Season, Date, Time, `Away Rest`, `Road Team`, `Home Team`, `Home Rest`, Arena)

sco <- games %>%
  select(GAME_ID, Date, Team, Team_pts) %>%
  left_join(
    games %>% select(GAME_ID, Opponent = Team, Opp_pts = Team_pts),
    by = "GAME_ID"
  ) %>%
  filter(Team != Opponent) %>%
  mutate(Attendance = NA_real_) %>%
  select(Date, Team, Opponent, Team_pts, Opp_pts, Attendance)


#Code performing a series fo team by team cleaning and tidying options (refactored for looping)####

team_locations <- games_raw %>%
  select(Team = TEAM_NAME, City = TEAM_CITY) %>%
  distinct() %>%
  mutate(
    City = dplyr::case_when(
      Team == "Brooklyn Nets" ~ "New York",
      Team == "Golden State Warriors" ~ "San Francisco",
      Team == "Minnesota Timberwolves" ~ "Minnesota",
      Team == "Utah Jazz" ~ "Utah",
      Team == "Indiana Pacers" ~ "Indiana",
      Team == "Washington Wizards" ~ "Washington D.C.",
      Team == "Oklahoma City Thunder" ~ "Oklahoma",
      TRUE ~ City
    )
  )

clean_team_name <- function(name) {
  name <- stringr::str_trim(name)
  name <- stringr::str_replace(name, "^Philadelphia ers$", "Philadelphia 76ers")
  name
}

sco <- sco %>%
  mutate(
    Team = clean_team_name(Team),
    Opponent = clean_team_name(Opponent)
  )

opponent_lookup <- team_locations %>%
  rename(Opponent = Team, City = City)

build_team_schedule <- function(team_name, home_city, dat, sco, opponent_lookup) {
  schedule <- dat %>%
    filter(`Road Team` == team_name | `Home Team` == team_name) %>%
    mutate(
      Team = team_name,
      Location = ifelse(`Road Team` == team_name, "Away", "Home"),
      Opponent = ifelse(Location == "Away", `Home Team`, `Road Team`),
      Rest = ifelse(Location == "Away", `Away Rest`, `Home Rest`),
      `Opp Rest` = ifelse(Location == "Away", `Home Rest`, `Away Rest`),
      Month = lubridate::month(Date)
    ) %>%
    select(Season, Team, Month, Date, Time, Opponent, Location, Arena, Rest, `Opp Rest`) %>%
    left_join(opponent_lookup, by = "Opponent") %>%
    mutate(City = ifelse(Location == "Home", home_city, City)) %>%
    select(Season, Team, Month, Date, Time, Opponent, Location, City, Arena, Rest, `Opp Rest`)

  score <- sco %>%
    filter(Team == team_name | Opponent == team_name) %>%
    mutate(
      Team2 = ifelse(Team == team_name, paste(Team, Team_pts), Team),
      Team2 = ifelse(Opponent == team_name, paste(Opponent, Opp_pts), Team2),
      Opp2 = ifelse(Opponent != team_name, paste(Opponent, Opp_pts), Opponent),
      Opp2 = ifelse(Opponent == team_name, paste(Team, Team_pts), Opp2)
    ) %>%
    select(Date, Team2, Opp2, Attendance) %>%
    mutate(
      Team = gsub("[[:digit:]]","",Team2),
      Team_pts = as.numeric(gsub("[^0-9.-]", "", Team2)),
      Opponent = gsub("[[:digit:]]","",Opp2),
      Opp_pts = as.numeric(gsub("[^0-9.-]", "", Opp2))
    ) %>%
    select(-Team2, -Opp2) %>%
    mutate(
      Team = clean_team_name(Team),
      Opponent = clean_team_name(Opponent)
    ) %>%
    mutate_if(is.character, trimws)

  full_join(schedule, score, by = c("Team", "Date", "Opponent"))
}


#Code to create master dataet joining all the above####
sche <- purrr::map2_dfr(
  team_locations$Team,
  team_locations$City,
  ~build_team_schedule(.x, .y, dat, sco, opponent_lookup)
) %>%

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

#coordinates for all US cities where games are playes
city_lookup <- tibble::tibble(
  City = c("Atlanta", "Boston", "Charlotte", "Chicago", "Cleveland", "Dallas", "Denver", "Detroit",
           "Houston", "Indiana", "Los Angeles", "Memphis", "Miami", "Milwaukee", "Minnesota", "New Orleans",
           "New York", "Oklahoma", "Orlando", "Philadelphia", "Phoenix", "Portland", "Sacramento",
           "San Antonio", "San Francisco", "Utah", "Washington D.C."),
  us_name = c("Atlanta GA", "Boston MA", "Charlotte NC", "Chicago IL", "Cleveland OH", "Dallas TX", "Denver CO",
              "Detroit MI", "Houston TX", "Indianapolis IN", "Los Angeles CA", "Memphis TN", "Miami FL",
              "Milwaukee WI", "Minneapolis MN", "New Orleans LA", "New York NY", "Oklahoma City OK", "Orlando FL",
              "Philadelphia PA", "Phoenix AZ", "Portland OR", "Sacramento CA", "San Antonio TX", "San Francisco CA",
              "Salt Lake City UT", "WASHINGTON DC")
)

acities <- us.cities %>%
  inner_join(city_lookup, by = c("name" = "us_name")) %>%
  transmute(City, Latitude = lat, Longitude = long) %>%
  bind_rows(tibble::tibble(City = "Toronto", Latitude = 43.65, Longitude = -79.38)) %>%
  distinct()

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

shots <- teams_shots(
  all_active_teams = T,
  season_types = "Regular Season",
  seasons = season_years,
  measures = "FGA",
  return_message = F,
  nest_data = F
)

all <- shots %>%
  mutate(Date = as.Date(as.character(dateGame), format = "%Y%m%d")) %>%
  select(Season = slugSeason, Date, Player = namePlayer, Team = nameTeam, Event = typeEvent, Action = typeAction,
         Shot = typeShot, Quarter = numberPeriod, minRemaining = minutesRemaining, secRemaining = secondsRemaining,
         Zone = zoneBasic, Zone2 = nameZone, Range = zoneRange, Distance = distanceShot, locX = locationX,
         locY = locationY)

write_feather(all, "shotchart.feather")


#################################################################################

#Loading Game Logs and Stats for each game in the last 3 seasons using NBAstatR function####
statlogs <- game_logs(
  seasons = season_years,
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

#dataset containing links to video highlights from the NBA content API.
extract_video_url <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (is.character(x)) {
    urls <- x[stringr::str_detect(x, "^https?://")]
    if (length(urls) > 0) {
      return(urls[[1]])
    }
    return(NA_character_)
  }
  if (is.list(x)) {
    for (item in x) {
      url <- extract_video_url(item)
      if (!is.na(url)) {
        return(url)
      }
    }
  }
  NA_character_
}

fetch_game_highlight <- function(game_id, team, opponent, date) {
  response <- httr::GET(
    url = sprintf("https://content-api-prod.nba.com/public/1/nba/v2/en-us/game/%s/videos.json", game_id),
    httr::add_headers(
      `User-Agent` = "Mozilla/5.0",
      Referer = "https://www.nba.com/",
      Origin = "https://www.nba.com"
    )
  )
  if (httr::http_error(response)) {
    return(tibble::tibble(Team = team, Opponent = opponent, Date = date, Link = NA_character_))
  }
  payload <- httr::content(response, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(payload, simplifyVector = FALSE)
  link <- extract_video_url(data)
  tibble::tibble(Team = team, Opponent = opponent, Date = date, Link = link)
}

highlights_index <- games_raw %>%
  mutate(
    Date = as.Date(GAME_DATE),
    Team = TEAM_NAME,
    Opponent = stringr::str_trim(stringr::str_replace(MATCHUP, ".* (vs\\.|@) ", ""))
  ) %>%
  distinct(GAME_ID, Team, Opponent, Date)

highlights <- purrr::pmap_dfr(
  list(highlights_index$GAME_ID, highlights_index$Team, highlights_index$Opponent, highlights_index$Date),
  fetch_game_highlight
)

highlights2 <- highlights %>% select(Team = Opponent, Opponent = Team, Date, Link) #video highlights for away games
write_feather(highlights, "highlights.feather")
write_feather(highlights2, "highlights2.feather")

#profile images (headshots) used for shotcharts
pro_file <- nbastatR::seasons_players(seasons = season_years) %>% select(Player = namePlayer, Image = urlPlayerHeadshot) #loads profile image of players
write_feather(pro_file, "pro_file.feather")

#################################################################################
