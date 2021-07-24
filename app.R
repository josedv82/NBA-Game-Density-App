
##########################################################################################

#NBA Game Density APP
#Jose Fernandez
#March-June 2020 (updated July 2021)
# MIT License

#APP File
###########################################################################################


#loading all required libraries with functions used for different aspects in the app####
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
library(shinyjs)
library(ballr)
library(grid)
library(jpeg)
library(feather)
library(sever)


###################################################

#import feathers objects. This loads the initial datasets that will be used throughout the app####
articles <- read_feather("article.feather")
highlights <- read_feather("highlights.feather")
highlights2 <- read_feather("highlights2.feather")
pro_file <- read_feather("pro_file.feather")
shotchart <- read_feather("shotchart.feather")
game_logs <- read_feather("gamelogs.feather")
sche <- read_feather("sche.feather")
acities <- read_feather("acities.feather")
Logos <- read_feather("logos.feather")
toronto <- c("Toronto", 43.65, -79.38) #for map

##################################################

#Import NBA team logos and create a table to use later in the app####

df <- data.frame(val = c("Atlanta Hawks","Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks",
                         "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers",
                         "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", 
                         "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
                         "Utah Jazz", "Washington Wizards"))

df$img2 = c(
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/atlanta-hawks-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[1]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/boston-celtics-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[2]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/brooklyn-nets-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[3]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/charlotte-hornets-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[4]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/chicago-bulls-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[5]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/cleveland-cavaliers-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[6]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/dallas-mavericks-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[7]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/denver-nuggets-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[8]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/detroit-pistons-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[9]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/golden-state-warriors-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[10]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/houston-rockets-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[11]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/indiana-pacers-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[12]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-clippers-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[13]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/los-angeles-lakers-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[14]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/memphis-grizzlies-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[15]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/miami-heat-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[16]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/milwaukee-bucks-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[17]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/minnesota-timberwolves-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[18]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/new-orleans-pelicans-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[19]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/new-york-knicks-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[20]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/oklahoma-city-thunder-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[21]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/orlando-magic-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[22]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/philadelphia-76ers-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[23]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/phoenix-suns-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[24]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/portland-trail-blazers-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[25]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/sacramento-kings-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[26]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/san-antonio-spurs-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[27]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/toronto-raptors-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[28]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/utah-jazz-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[29]),
  sprintf("<img src='https://cdn.freebiesupply.com/images/thumbs/2x/washington-wizards-logo.png' width=45px><div class='jhr'>%s</div></img>", df$val[30]))

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

#links social media buttons. URL address for the social media links in the app####
url <- "https://twitter.com/intent/tweet?text=Check out this app to explore the density of the schedule in the NBA. @NBAGameDensity&url=https://josedv.shinyapps.io/NBASchedule/"
url2 <- "https://josedv.shinyapps.io/NBASchedule/"


##################################################

#code for sever reloading screen####
disconnected <- tagList(
  h1("NBA Game Density APP"),
  p("Disconnection due to inactivity"),
  reload_button("REFRESH", class = "warning")
)


##################################################

#User Interface: This is the part that users can see and interact with. It defines what the app looks like
############################################################################################

ui <- dashboardPagePlus(
  
#Header: The code below defines aspect related to the header of the app############
  
  header = dashboardHeaderPlus(
    
    left_menu = tagList(
      
    #Code for the NBAtwitter feed
    dropdownBlock(
      id = "twitterdropdown",
      title = "NBA Feed",
      icon = "twitter-square",
      badgeStatus = NULL,
      HTML('<a class="twitter-timeline" data-height="600" href="https://twitter.com/NBAGameDensity/lists/nba-game-density-app?ref_src=twsrc%5Etfw">A Twitter List by NBAGameDensity</a> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>')
    )),
    
    #github code
    tags$li(class = "dropdown", actionLink("welcome1", label = 'Code', icon = icon("github"), style = 'color:white',
                                           onclick ="window.open('https://github.com/josedv82/NBA-Game-Density-App', '_blank')"),
            bsTooltip("welcome1", HTML("Link to app repository."), placement = "bottom", trigger = "hover", options = NULL)),
    
    #popup modal (window) link. This code opens a window with basic explanation of the app
    tags$li(class = "dropdown", actionLink("welcome", label = 'About', icon = icon("info-circle"), style = 'color:white'),
            bsTooltip("welcome", HTML("Click to know more about this app."), placement = "bottom", trigger = "hover", options = NULL)),
  
    
    #This code enables the use of a right side bar on the app, which we will use to add user inputs
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "cogs",
    
    #this code shows an icon (logo) on the top left of the screen the the left side bar is collapsed
    title = tagList(
     shiny::span(class = "logo-lg", "NBA Schedule Density", style = "color:white"), 
      img(src = "hexlogo.png", width = "35px", height = "33px")),
    
    
    titleWidth = 330),
  
  
##################################################

#sidebar######  
  
  #the following code crates funtionality on the left side bar (menu). Users will use this functionality to interact and access
  #different parts of the app
  
  sidebar = dashboardSidebar(
    
    width = 330, #width of the left side bar
    
    sidebarMenu(
      
      #fluidRow(width = "100%", align = "center", #style = "padding-right:45px",
               
      #tags$br(),
      
      #column(width = 5),
      
      #column(width = 2,  style = "padding-right:45px",
             
      # Create a twitter button for users to share the app on twitter
     # tags$a(href = url, "Tweet", class="twitter-share-button", `data-show-count` = "true"),
    #  includeScript("http://platform.twitter.com/widgets.js")),
      
      #column(width = 1),
      
      # Create 'linkedin-share-button' for users to share the app on linkedin
     # column(width = 2,  style = "padding-right:45px",
    #  tags$script(src = "https://platform.linkedin.com/in.js", type = "text/javascript", "lang: en_US"),
    #  tags$script(type = "IN/Share",`data-url` = url2)),
      
      #column(width = 2)
      
      
      #),
      
      
      #Add a season filter. Users can filter the last 3 NBAseasons (17-18, 18-19, 19-20)
       fluidRow(width = "100%", align = "center",
               
                  pickerInput(
                    inputId = "season_filter",
                    label = p("Select Season:", style = "padding-right:160px"),
                    choices = sche$Season %>% unique() %>% sort(), 
                    selected = "2020-21", 
                    choicesOpt = list(style = rep(("color: black; background: white"),30)),
                    multiple = F)
               
               ),
      
      #Team by Team tab. This tab contains individual reports for each team
      menuItem("Team by Team", tabName = "teambyteam", icon = icon("project-diagram"), startExpanded = T,
         
          #team filter: users can select which team they can to look at           
          menuSubItem(icon = NULL, tabName = "teambyteam", 
            pickerInput(
            inputId = "team_filter",
            label = "Select Team", 
            choices = df$val,
            choicesOpt = list(style = rep(("color: black; background: white"),30), content = df$img),
            multiple = F)),
          tags$br(),
          menuSubItem(icon = NULL, tabName = "teambyteam", fluidRow(withLoader(DT::dataTableOutput("daily_games"), type ="html", loader = "loader1"), style="padding-right: 17px;")), 
          tags$br()
          
          ),
      
      #All Teams Tab: This tab contains reports for all teams. Enables users to compare data across teams
      menuItem("All Teams", tabName = "allteams", icon = icon("th"), startExpanded = F),
      
      #Recommended Reading Tab. This tab displays an interactive table with related research and articles that users can explore
      menuItem("Research / Media Articles", tabName = "reading", icon = icon("book-reader"), startExpanded = F),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$hr(),
      
      
      #add airball package link
      fluidRow(width = "100%", align = "center", style = "padding-right:20px",
      column(width = 12, style = "background-color:transparent;",
      h2("airball", style = "color:white; text-align: middle"),
      column(width = 5, img(src = 'https://raw.githubusercontent.com/josedv82/airball/master/man/images/airballlogo.PNG', 
                            width = "80px")),
      column(width = 7, h6(HTML("After creating this app, <br/> I published an R package <br/> to quickly extract some of <br/> the schedule metrics shown <br/> here since the 1947 season."),
                           style = "color:slategray; text-align: left"),
                        tags$a(href = "https://github.com/josedv82/airball", "Link", style = "color:dodgerblue; text-align: left"))
      )
      )
      
      
    )#sidebar menu
    
    
  ),

##################################################

#Dashboard body. The part of the app that contains the main reports##########
  
  
  body = dashboardBody(
    
    use_waiter(), #this function is needed to add the loader page displayed while the app loads 
    
    #bring in sever dependencies
    use_sever(),
    
    #this code supresses potential error messages that may confuse the user
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    
    #this sets the width of dropdownBlock where twitter feed is embedded on the header part
    tags$head(tags$style(HTML('.navbar-custom-menu>.navbar-nav>li>.dropdown-menu {width:400px;}'))),
    
    #this code helps align the team logos in the picker input displayed on the left side bar
    tags$head(tags$style(".jhr{ display: inline;vertical-align: middle;padding-left: 10px;}")),
    
    #this code helps prevent Data Tables from altering default table font colors automatically 
    tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color:lightblue;
                    }
                    
                    thead {
                    color:white;
                    }
                    
                    tbody {
                    color:white;
                    }
                    
                    ")),
    
    #this sets the customised look for slider inputs on the right side bar
    chooseSliderSkin("Simple"),
    
    #this sets the custom theme for the app, In our case we are using a dark grey theme
    shinyDashboardThemes(theme = "grey_dark"),
    
    
    #this function is needed to initialize shiny alert for modal and popups windows
    useShinyalert(),
    
    #this function is needed for shinyJS, which allows to customise some functionalities on the background
    useShinyjs(),
    
    # here we are going to start designing what the dashboard body is going to look like
    tabItems(
    
    #the code below relates to the "Team by Team tab.  
      tabItem(
      tabName = "teambyteam",
      tabBox(title = "", id = "tab1", height = "100%", width = "100%", 
      
      #user interface for the Game Card tab       
      tabPanel("Game Card", icon = icon("map-marked-alt"), 
               fluidRow(column(width = 6, uiOutput("date")),
                        column(width = 6, align = "right",
                               actionButton("gdash", label = '', icon = icon("info-circle"), style = 'color:#e74c3c; background-color:#343E48; border-color:#343E48'),
                               bsTooltip("gdash", HTML("About This Dashboard"), placement = "left", trigger = "hover", options = NULL)
                               )),
               
               tags$hr(),
               
               fluidRow(
                 
                 #Selected team data
                 column(width = 4, align="center",
                        htmlOutput("team_logo"),
                        uiOutput("team.pct"), 
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
                 
                 #game details, buttons for stats, shotchart and youtube highlights
                 column(width = 4, align = "center",
                        tags$strong(htmlOutput("city")),
                        htmlOutput("arena"),
                        tags$br(),
                        tags$strong(htmlOutput("route")),
                        htmlOutput("distance"),
                        tags$br(),
                        actionButton("gamelogs", label = '', icon = icon("table"), style = 'color:white; background-color:seagreen'),
                        bsTooltip("gamelogs", HTML("Game Stats"), placement = "left", trigger = "hover", options = NULL),
                        tags$head(tags$style("#gamelogmodal .modal-dialog{ width:1700px}")),
                        actionButton("shotchart", label = '', icon = icon("basketball-ball"), style = 'color:white; background-color:darkcyan'),
                        bsTooltip("shotchart", HTML("Shot Chart"), placement = "bottom", trigger = "hover", options = NULL),
                        actionButton("video_yes", label = '', icon = icon("youtube"), style = 'color:white; background-color:#c4302b'),
                        bsTooltip("video_yes", HTML("Video Highlights"), placement = "right", trigger = "hover", options = NULL),
                        withLoader(plotOutput("map_plot", width = "100%"), type = "html", loader = "loader1")
                        ),
                       
                 
                 #Opponent's Data
                 column(width = 4, align="center", 
                        
                        htmlOutput("opponent_logo"),
                        uiOutput("opponent.pct"),
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
      
     
      # the code below is for the "Schedule Table" tab user interface
      tabPanel("Schedule Table", icon = icon("calendar-alt"), 
               
               
               fluidRow(column(12, align="right",
                               actionButton("pIndex", label = '', icon = icon("info-circle"), style = 'color:#e74c3c; background-color:#343E48; border-color:#343E48'),
                               bsTooltip("pIndex", HTML("About Game Index"), placement = "left", trigger = "hover", options = NULL)
               )),
               
               fluidRow(column(width = 12, withLoader(DT::dataTableOutput("team_table", width = "100%"), type = "html", loader = "loader1")))),
      
      # The code below is the the "Rolling Density" tab user interface
      tabPanel("Rolling Density", icon = icon("chart-line"), 
               
               fluidRow(column(width = 12, align = "right",
                               actionButton("denplot", label = '', icon = icon("info-circle"), style = 'color:#e74c3c; background-color:#343E48; border-color:#343E48'),
                               bsTooltip("denplot", HTML("About This Dashboard"), placement = "left", trigger = "hover", options = NULL))),
               
               fluidRow(column(width = 12, withLoader(plotlyOutput("team_plot", height = "auto", width = "100%"), type = "html", loader = "loader1")))),
      
      
      # The code below is the the "Outcome" tab user interface
      tabPanel("Outcome", icon = icon("tasks"), 
               
               fluidRow(column(width = 12, align = "right",
                               actionButton("counts", label = '', icon = icon("info-circle"), style = 'color:#e74c3c; background-color:#343E48; border-color:#343E48'),
                               bsTooltip("counts", HTML("About This Dashboard"), placement = "left", trigger = "hover", options = NULL))),
               
               
               fluidRow(style = "padding:15px",  column(width = 8, DT::dataTableOutput("w_l_table")), column(width = 4, 
               fluidRow(DT::dataTableOutput("H_A_table")), 
               tags$br(), 
               tags$br(), 
               fluidRow(DT::dataTableOutput("team_table_density"))))),
      
      
      # The code below is for the "Player Load" tab user interface
      tabPanel("Player Load", icon = icon("charging-station"), 
               
               fluidRow(column(12, align="right",
                               actionButton("pLoad", label = '', icon = icon("info-circle"), style = 'color:#e74c3c; background-color:#343E48; border-color:#343E48'),
                               bsTooltip("pLoad", HTML("About Player Load"), placement = "left", trigger = "hover", options = NULL)
               )),
               
               tags$br(),
               
               fluidRow(column(width = 12,
                               
                               boxPlus(
                                 title = "Full Roster", 
                                 closable = F, 
                                 enable_label = F,
                                 status = "info", 
                                 collapsible = TRUE,
                                 solidHeader = F,
                                 enable_sidebar = F,
                                 collapsed = TRUE,
                                 width = NULL,
                                 
                                 p(
                                   
                                   fluidRow(column(width = 12, withLoader(plotOutput("player_load", width = "100%", height = '750px'),type = "html", loader = "loader1")))
                                   
                                 )))),
               
               fluidRow(column(width = 12,
                               
                               boxPlus(
                                 title = "Individual Player Load", 
                                 closable = F, 
                                 width = NULL,
                                 enable_label = F,
                                 status = "info", 
                                 solidHeader = FALSE, 
                                 collapsible = TRUE,
                                 collapsed = FALSE,
                                 enable_sidebar = FALSE,
                                 
                                 p(
                                   
                                   
                                   column(width = 2,
                                          fluidRow(uiOutput("player_image")),
                                          fluidRow(uiOutput("playerLoad.filter")),
                                          tags$br(),
                                          tags$br(),
                                          fluidRow(uiOutput("last.n.filter")),
                                          fluidRow(DT::dataTableOutput("playerLoad.table"))
                                   ),
                                   
                                   column(width = 10,
                                          plotlyOutput("player.load.trend", width = "100%", height = '750px')
                                   )
                                 )
                               )
               )))
      
      
      
    )#tabbox
    
    ),#tabitem
    
    # The code below relates to the "ALL TEAMS" tab
    tabItem(
      
      tabName = "allteams",
      tabBox(title = "", id = "tab2", height = "100%", width = "100%",
      
             #this sets the user interface for the "Density Counts" tab       
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
             
             #this sets the user interface for the "heat map" tab
             tabPanel("Heatmap", icon = icon("solar-panel"), 
                      
                      fluidRow(column(width = 4, uiOutput("by.team")),
                               column(width = 8, align = "right",
                                      actionButton("hmap", label = '', icon = icon("info-circle"), style = 'color:#e74c3c; background-color:#343E48; border-color:#343E48'),
                                      bsTooltip("hmap", HTML("More About Heatmaps"), placement = "left", trigger = "hover", options = NULL)
                                      )),
                      
                      fluidRow(column(width = 12, withLoader(plotOutput("heatmap", width = "100%", height = '750px'),type = "html", loader = "loader1")))),
             
             #this sets the user interface for the "Flight Routes" tab
             tabPanel("Flight Routes", icon = icon("plane-departure"), 
                      
                     fluidRow(column(width = 12, align = "right",
                                      actionButton("fmap", label = '', icon = icon("info-circle"), style = 'color:#e74c3c; background-color:#343E48; border-color:#343E48'),
                                      bsTooltip("fmap", HTML("More About Flight Routes"), placement = "left", trigger = "hover", options = NULL)
                               )),
                      
                       
                    fluidRow(style = "height:700px;", 
                             column(width = 12,
                                      withLoader(
                                        plotOutput("mapsf", width = "100%"),type = "html", loader = "loader1")))),
             
             
             #This sets the user experience for the "player Loads" tab
             tabPanel("Player Loads", icon = icon("charging-station"),
                      
                      fluidRow(
                      
                      column(width = 2, uiOutput("team.filter2")),
                      column(width = 2, uiOutput("month.filter")),
                      column(width = 2, uiOutput("week.filter")),
                      column(width = 2, uiOutput("metric.filter")),
                      #column(width = 2, uiOutput("top.rows.filter"))
                      column(width = 4, align = "right",
                             actionButton("player_loads", label = '', icon = icon("info-circle"), style = 'color:#e74c3c; background-color:#343E48; border-color:#343E48'),
                             bsTooltip("player_loads", HTML("About This Table"), placement = "left", trigger = "hover", options = NULL))
                      ),
                      
                      fluidRow(
                      column(width = 12, DT::dataTableOutput("all.player.load"))
                      
                      )
                      
                      
             )
             
             
      )#tabbox
  ),#tabitem
  
  # The code below relates to the "Research / Articles tab"
  tabItem(
    tabName = "reading",
           
      fluidRow(style = "padding-bottom: 8px", column(width = 2,
                                                   
      dropdown(
        
        
        #creates an input for users to filter articles by type. i.e scientific or media articles
          pickerInput(
          inputId = "type.reads",
          label = shiny::HTML("<p><span style='color: black'>Article Type</span></p>"), 
          choices = articles$Type %>% unique(),
          selected = "Research",
          multiple = TRUE), 
          
          tags$br(),
          tags$hr(),
          tags$br(),
        
        #creates an input for users articles by year  
        sliderInput(
          inputId = "year.reads",
          label = shiny::HTML("<p><span style='color: black'>Select Year(s)</span></p>"),  
          sep = "",
          min = min(articles$Year),
          max = max(articles$Year),
          step = 1,
          value = c(min(articles$Year), max(articles$Year))
        ),
        
        tags$br(),
        tags$hr(),
        tags$br(),
        
        blockQuote(uiOutput('item'), side = "right"),
        
        inputId = "dropdown1",
        status = "info",
        icon = icon("filter"), width = "300px",
        tooltip = tooltipOptions(title = "Filtering Options")
        
      )
      
      )),#fluidrow
      
    fluidRow(column(width = 12, withLoader(DT::dataTableOutput("reads", width = "100%"), type = "html", loader = "loader1")))     
  
    
  )#tabitem
 
    ),#tabitems
  
  
##################################################

#code for page loader. this is the landing page displayed while the app loads####
  waiter_show_on_load(
    color = "white",
    div(style = "color:white;",
        tags$h2("Loading...", style = "color:grey", align = "center"),
        tags$img(src="waiter.gif", width="auto")
    )
  )
  

  ),#dashboardbody ends here


##################################################

#right side bar. This code sets user interface for the right side bar#####


  #right side bar provides several inputs for users to customise different metrics related to schedule loads and density
    rightsidebar = rightSidebar(
      
      
      width = 300, #sets the width of the rigth side bar
      
      background = "dark", #sets the background color of the bar
      
      #first tab within the right side bar
      rightSidebarTabContent(
        id = 1,
        icon = "plane-departure",
        title = "",
        active = TRUE,
        tags$h4("Travel Load Factor", style = "color:white"),
        tags$hr(),
        
        #inputs for game location load factors
        tags$h4("Game Location", style = "color:#e48b38"),
        sliderTextInput("home","Home Games", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T),
        sliderTextInput("away","Away Games", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        tags$hr(),
        
        #input for travel load factors
        tags$h4("Travel", style = "color:#e48b38"),
        sliderTextInput("travelyes","Game with travel", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("travelno","Game without travel", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T),
        tags$hr(),
        
        #inputs for direction of travel load factors
        tags$h4("Direction", style = "color:#e48b38"),
        sliderTextInput("east","Direction Eastbound", choices = seq(from = 1, to = 5, by = 0.5), selected = 3, grid = T),
        sliderTextInput("west","Direction Westbound", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("neutral","Within same zone", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T)
        
      ),
      
      #second tab within the right side bar
      rightSidebarTabContent(
        id = 2,
        icon = "globe-americas",
        title = "",
        active = FALSE,
        tags$h4("Time Shift Factor", style = "color:white"),
        tags$hr(),
        
        #input for time zones corossed load factors
        tags$h4("Time Zones Crossed", style = "color:#e48b38"),
        sliderTextInput("0neutral","No zone change", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T),
        sliderTextInput("1east","1 Zone East", choices = seq(from = 1, to = 5, by = 0.5), selected = 2.5, grid = T),
        sliderTextInput("2east","2 Zones East", choices = seq(from = 1, to = 5, by = 0.5), selected = 3.5, grid = T),
        sliderTextInput("3east","3 Zones East", choices = seq(from = 1, to = 5, by = 0.5), selected = 4.5, grid = T),
        sliderTextInput("1west","1 Zone West", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("2west","2 Zone West", choices = seq(from = 1, to = 5, by = 0.5), selected = 3, grid = T),
        sliderTextInput("3west","3 Zone West", choices = seq(from = 1, to = 5, by = 0.5), selected = 4, grid = T)
        
      ),
      
      #third tab within the right side bar
      rightSidebarTabContent(
        id = 3,
        icon = "clock",
        title = "",
        active = FALSE,
        tags$h4("Elapsed Days Factor", style = "color:white"),
        tags$hr(),
        
        #input for days elapsed between games
        tags$h4("Days since last game.", style = "color:#e48b38"),
        sliderTextInput("1day","1 Day ", choices = seq(from = 1, to = 5, by = 0.5), selected = 5, grid = T),
        sliderTextInput("2day","2 Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 4, grid = T),
        sliderTextInput("3day","3 Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 3, grid = T),
        sliderTextInput("4day","4 Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("5day","5+ Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T)
        
      ),
      
      #fourth tab within the right side bar
      rightSidebarTabContent(
        id = 4,
        icon = "suitcase-rolling",
        title = "",
        active = FALSE,
        tags$h4("Game Density Factor", style = "color:white"),
        tags$hr(),
        
        #input for game density profiles and related load factors
        tags$h4("Acummulated Games.", style = "color:#e48b38"),
        sliderTextInput("G4in5b2b","4 Games in 5 Days + B2B", choices = seq(from = 1, to = 5, by = 0.5), selected = 5, grid = T),
        sliderTextInput("G3in4b2b","3 Games in 4 Days + B2B", choices = seq(from = 1, to = 5, by = 0.5), selected = 4, grid = T),
        sliderTextInput("G3in4","3 Games in 4 Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 3.5, grid = T),
        sliderTextInput("Gb2b","Back to Back", choices = seq(from = 1, to = 5, by = 0.5), selected = 3, grid = T),
        sliderTextInput("G1d","1 Rest Day", choices = seq(from = 1, to = 5, by = 0.5), selected = 2.5, grid = T),
        sliderTextInput("G2d","2 Rest Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 2, grid = T),
        sliderTextInput("G3d","3+ Rest Days", choices = seq(from = 1, to = 5, by = 0.5), selected = 1, grid = T)
        
      ),
      
      #fith tab within the right side bar
      rightSidebarTabContent(
        id = 5,
        icon = "circle-notch",
        title = "",
        active = FALSE,
        tags$h4("Density Moving Average", style = "color:white"),
        tags$hr(),
        tags$h5("A simple moving (or rolling) average is an unweighted mean of the last n games. Use the knob below so determine the n of games to roll over.", style = "color:lightgray"),
        tags$hr(),
        
        #knob for users to select length of rolling average
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
    
##################################################

#Footer. The footer bar displayed a the bottom of the dashboard###########


  #this codes adds a footer to the dashboard
  footer = dashboardFooter(
    
    #footer details on the right side of the footer
    right_text = HTML(paste(img(src = "hexlogo.png", width = "35px", height = "33px"), 
                            tags$span("NBA Game Density APP", style = "font-family: Arial; color: grey; font-size: 16px"))),
    
    #footer details on the left side of the footer
    left_text = HTML(paste(icon = icon("copyright"), tags$span("2020. Created by Jose Fernandez", style = "font-family: Arial; color: grey; font-size: 16px"),
                            tags$span(tags$a(href= "https://twitter.com/jfernandez__", icon("twitter"))),
                            sep = " "))
  )
  
)

##################################################






#Server Logic. 

#This part of the app is not visible to the user, but it adds the required functionality for the
#user to be able to interact with the app
##################################################

server <- function(input, output, session) {
  
#this code creates an alert that is displayed when the user changes the length of the rolling average####
  
  observeEvent(input$rolling, ignoreInit = TRUE, {
    
    # Show a popup window when the rolling average button is changes
    shinyalert(paste("Moving Index Set to last", input$rolling, "Games.", sep = " "), type = "success")
  })
 
  
  
###################################################################################################
  
#Intro pop up window. This code creates a window with basic explanation about what the app does.#### 
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
        tags$h4("Player Load", style = "padding-left:2.3em; font-family: Arial; color: white"),
        tags$img(src = "https://image.flaticon.com/icons/svg/1265/1265301.svg", width = "40px", height = "40px", align = "left"),
        p("Information about individual player loads accounting for factors including rest days and minutes playes.", style = "padding-left:3em; font-family: Arial; color: white"),
        tags$br(),
        tags$hr(),
        HTML(paste(tags$span("If you have any feedback please get in touch via", style = "font-family: Arial; color: white"), tags$span(tags$a(href= "https://twitter.com/NBAGameDensity", "Twitter.")))),
        
        size = "l",
        easyClose = TRUE)
      
    )#showmodal
    
    
  })#observeEvent 
  
  
###################################################################################################
  
  
#Reactive Dataset that reponds to users interactions#### 
  
#This part of the code creates the final data set with reactive filters to allow users to decide how 
#much weight to give to each type of game and the rolling window for the average

  #let users filter by season
  sche1 <- reactive({
    
    
     sche %>% 
      
      
      filter(Season == input$season_filter) %>%
      
      mutate(Season = as.factor(Season)) %>%
      group_by(Team) %>%
      
      #converts months from numbers to actual names
      mutate(Month = ifelse(Month == 1, 'January',
                            ifelse(Month == 2, 'February',
                                   ifelse(Month == 3, 'March',
                                          ifelse(Month == 4, 'April',
                                                 ifelse(Month == 5, 'May',
                                                     ifelse(Month == 6, 'June',
                                                      ifelse(Month == 7, 'July',
                                                             ifelse(Month == 8, 'August',
                                                                    ifelse(Month == 9, 'September',
                                                                           ifelse(Month == 10, 'October',
                                                                                  ifelse(Month == 11, 'November', 'December')))))))))))) %>%
      
      select(-Time) %>%
      
      #calculates time elapsed between last game and the previous one
      mutate(elapsed = Date - lag(Date)) %>%
      
      #simple functionality to determine if there was a travel previous to a game
      mutate(Travel = ifelse(City != lag(City), "y", "n")) %>%
      mutate(Travel = ifelse(Arena == "Disney World", "n", Travel)) %>% #corrects for bubble
      mutate(Location = ifelse(Arena == "Disney World", "Home", Location)) %>% #corrects for bubble
      
      #identifies which which time zone (within the us) a game is held
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
      
      #some code to determine time changes between zones
      mutate(zone.n = ifelse(Zone == "Pacific", 1,
                                ifelse(Zone == "Mountain", 2,
                                ifelse(Zone == "Central", 3,
                                ifelse(Zone == "Eastern", 4, 0))))) %>%
      
      mutate(shift = zone.n - lag(zone.n)) %>%
      mutate(shift = ifelse(Arena == "Disney World", 0, shift)) %>% #corrects for bubble
      
      mutate(Direction = ifelse(shift < 0, "westbound",
                                   ifelse(shift > 0, "eastbound",
                                   ifelse(shift == 0, "-", 100)))) %>%
      
      #the blow code takes all the user inputs on the right side bar and calculates a game density index
      
      mutate(location.i = ifelse(Location == "Home", input$home,
                                    ifelse(Location == "Away", input$away, 100)))%>%
      
      mutate(Rest.i = ifelse(Rest == "3+", input$G3d,
                             ifelse(Rest == "2", input$G2d,
                                    ifelse(Rest == "1", input$G1d,
                                           ifelse(Rest == "B2B", input$Gb2b,
                                                  ifelse(Rest == "3IN4", input$G3in4,
                                                      ifelse(Rest == "4IN5-B2B", input$G4in5b2b,
                                                         ifelse(Rest == "3IN4-B2B", input$G3in4b2b, 100)))))))) %>%
      
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
      
      mutate(Normalized = Normalized * 100) %>%
      
      select(Season, Month, Date, Location, City, Arena, Team, Opponent, Team_pts, Opp_pts, Density = Rest, Normalized) %>%
      
      #
      mutate(bubble_game = ifelse(Arena == "Disney World", "yes", "no")) %>% #corrects for bubble
      
      group_by(Team, bubble_game) %>%
      
      
      mutate(MovIndex = round(roll_meanr(Normalized, n = input$rolling, align = "right", fill = 0, na.rm = T),1)) %>%
      ungroup() %>%
      select(-bubble_game)
    
  })
  
  
  
  
  #the code below is used to identify the stress of the oponents
  sche2 <- reactive({
  
    a <- sche1() %>%
      mutate(conc = paste(Team, Date, Opponent)) %>%
      select(aSeason = Season, aTeam = Team, aMonth = Month, aDate = Date, aOpponent = Opponent, aLocation = Location, aCity = City, aArena = Arena, aDensity = Density, aTeam_pts = Team_pts, aOpp_pts = Opp_pts, aNormalized = Normalized, aMovIndex = MovIndex, conc)
    
    b <- sche1() %>%
      mutate(conc = paste(Opponent, Date, Team)) %>%
      select(bSeason = Season, bTeam = Team, bMonth = Month, bDate = Date, bOpponent = Opponent, bLocation = Location, bCity = City, bArena = Arena, bDensity = Density, bTeam_pts = Team_pts, bOpp_pts = Opp_pts, bNormalized = Normalized, bMovIndex = MovIndex, conc)
    
    
    #creates a final dataset with the density metrics for the selected team and the opponent
    full_join(a,b, by = "conc") %>%
      select(Season= aSeason, Month = aMonth, Date = aDate, Location = aLocation, City = aCity, Arena = aArena, Team = aTeam, Opponent = aOpponent, Team_pts = aTeam_pts, Opp_pts = aOpp_pts, Density = aDensity, `Opp Density` = bDensity, Index = aNormalized, `Opp Index` = bNormalized, movIndex = aMovIndex, `Opp movIndex` = bMovIndex) %>%
      mutate(`W/L` = ifelse(Team_pts > Opp_pts, "W", "L")) %>%
      ungroup() %>%
      select(Season, Month, Date, Location, City, Arena, Team, Opponent, `W/L`, Team_pts, Opp_pts, Density, `Opp Density`, Index, `Opp Index`, movIndex, `Opp movIndex`) %>%
      arrange(Season, Team)
  
  
  })
  
  #the code below creates a dataset with density metrics as well as longitude and latitude coordinates for each game
  #based on the city where the team is played. We will use this for the maps in the app
  
  cities <- reactive({
    
    validate(
      need(input$team_filter, "")
    )
    
    #cleaning data to ensure cities in our dataset are named equally as cities in the us.cities dataset
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
      
      
      #create a master tables with density metrics and city coordinates
      
      full_join(sche2(), by = c("City")) %>%
      
      
      select(Season, Date, Location, Team, Opponent, City, Latitude, Longitude, everything()) %>%
      
      arrange(Season, Team, Date) %>%
      
      group_by(Team, Season) %>%
      
      mutate(destLat = lag(Latitude), destLon = lag(Longitude)) %>%
      mutate(destLat = ifelse(Arena == "Disney World", "28.5", destLat)) %>% #corrects for bubble
      mutate(destLon = ifelse(Arena == "Disney World", "-81.37", destLon)) %>%  #corrects for bubble
      
      
      #The following code corrects the missing coordinates for the first game of the season. 
      
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
      
      #here we calculate the distance in miles between cities
      mutate(dist = geosphere::distm(c(destLon, destLat), c(Longitude, Latitude), fun=distHaversine)) %>% 
      mutate(Distance = paste(round(dist * 0.000621,0), "miles", sep = " ")) %>%
      
      mutate(Route = ifelse(dist == 0, "No Travel", Route)) %>%
      
      select(-dist) %>%
      
      #here we change the format of the rest days, so it is more user friendly
      mutate(Density = ifelse(Density == "1", "1 Day Rest", 
                              ifelse(Density == "2", "2 Days Rest", 
                                     ifelse(Density == "3+", "3+ Days Rest", Density)))) %>%
      
      mutate(`Opp Density` = ifelse(`Opp Density` == "1", "1 Day Rest", 
                                    ifelse(`Opp Density` == "2", "2 Days Rest", 
                                           ifelse(`Opp Density` == "3+", "3+ Days Rest", `Opp Density`)))) %>%
      
      #the final dataset to feed reports
      select(Season, Month, Date, Location, Arena, Team, Opponent, City, `W/L`, Team_pts, Opp_pts, Density, `Opp Density`, Index, `Opp Index`, movIndex, `Opp movIndex`, Latitude, Longitude, destLat, destLon, Route, Distance)
    
    
  })
  
###################################################################################################

  
#TEAM BY TEAM TAB.
###################################################################################################
  
  #Left Side Bar Mini Chart. Add a mini chart on the left side bar when the first tab is open####
  output$daily_games <- DT::renderDataTable({
    
    a <- sche2() %>% 
      filter(Location == "Home") %>%
      filter(Date == input$date_filter) %>%
      filter(Team != input$team_filter & Opponent != input$team_filter) %>%
      mutate(Team = sub('^.* ([[:alnum:]]+)$', '\\1', Team)) %>%
      mutate(Opponent = sub('^.* ([[:alnum:]]+)$', '\\1', Opponent)) %>%
      select(Team, Opponent, Density, `Opp Density`) %>%
      
      mutate(Team = ifelse(Team == "Cavaliers", "Cavs", 
                           ifelse(Team == "Mavericks", "Mavs",
                                  ifelse(Team == "Timberwolves", "Wolves", Team)))) %>%
      
      mutate(Opponent = ifelse(Opponent == "Cavaliers", "Cavs", 
                           ifelse(Opponent == "Mavericks", "Mavs",
                                  ifelse(Opponent == "Timberwolves", "Wolves", Opponent)))) %>%

      mutate(Game = paste("<span style=color:white>", Team, "</span>", "<span style=color:gray>", " vs ", "</span>", "<span style=color:white>", Opponent, "</span>")) %>%
      
      select(` Rest` = Density, Game, `Rest ` = `Opp Density`) %>%
      
      
      formattable(
        
        list(
          
          ` Rest`= formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "4IN5-B2B", "#ff0000",
                                                                                 ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                                                 ifelse(x == "3IN4", "#ff3300", 
                                                                                                        ifelse(x == "B2B", "#cc6600",
                                                                                                               ifelse(x == "1", "#cc9900",
                                                                                                                      ifelse(x == "2", "#cccc00",
                                                                                                                             ifelse(x == "3+", "#ccff00", "transparent"))))))))), 
          
          
          `Rest ` = formattable::formatter("span", style = x ~ formattable::style(color =  ifelse(x == "4IN5-B2B", "#ff0000",
                                                                                  ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                                                       ifelse(x == "3IN4", "#ff3300", 
                                                                                                              ifelse(x == "B2B", "#cc6600",
                                                                                                                     ifelse(x == "1", "#cc9900",
                                                                                                                            ifelse(x == "2", "#cccc00",
                                                                                                                                   ifelse(x == "3+", "#ccff00", "transparent")))))))))
          
          
        )) 
    
    validate(need(a$Game != "", "No other games are playes on this date"))
    
    formattable::as.datatable(a,  
                              caption = paste("All games on ", input$date_filter, ":", sep = ""),
                              rownames = FALSE,
                              escape = F,
                              options = list(dom = 't', 
                                             pageLength = 90, 
                                             bSort=FALSE,
                                             columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                             initComplete = JS(
                                               "function(settings, json) {",
                                               "$(this.api().table().header()).css({'background-color': 'rgb(52,62,72)', 'color': 'ivory'});",
                                               "}"))) %>%
      
    formatStyle('Rest ', backgroundColor =  'rgb(52,62,72)') %>%
    formatStyle(' Rest', backgroundColor =  'rgb(52,62,72)') %>%
    formatStyle('Game', backgroundColor =  'rgb(52,62,72)')
    
  })
  
  
  ################################################
 
  #Game Card Tab Reactivity############
  
  #Info pop up window. Users can click on the info button to get an explanation of this report
  observeEvent(input$gdash, {
    
    
    showModal(
      
      modalDialog(
        
        tags$h3("Game Dashboard", style = "font-family: Arial; color: white"),
        tags$hr(),
        tags$br(),
        tags$h5("After selecting a season and a team of interest, the date filter will allow users to select specific games.", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$br(),
        tags$h5("This dashboard provides the following metrics for each team:", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Team Names: "), "Teams taking part in the game."))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Winning %: "), "Overall winning % for each team on the selected season."))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Location: "), "Home and Away."))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Game Score: "), "Including a green dynamic tag highlighting the game winner."))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Game Profile: "), "Type of game for each team based on rest days, i.e: 1 Rest Day, 2 Rest Days, 3 Rest Days, B2B, 3in4 or 3in4-B2B."))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Game Index: "), "A schedule density score accounting for different factors. (see schedule density tab for more info)."))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Moving Index: "), "A rolling average of the game index. Users can decide the number of rolling games."))),
        tags$br(),
        tags$h5("The game index factors described above can be adjusted by users using the inputs in the right side bar of the website which is accsible via the 'cogs' icon 
                on the top right part of the header", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$h4("Further Functionalities:", style = "font-family: Arial; color: white"),
        tags$hr(),
        tags$h5(HTML(paste(tags$span(style="color: green", "Box Scores Button: "), "Basic Games stats. Also includes information on Game Loads for each player."))),
        tags$h5(HTML(paste(tags$span(style="color: lightseagreen", "Shot Charts: "), "Shot Chart for each player in the game."))),
        tags$h5(HTML(paste(tags$span(style="color: red", "Video HighLights: "), "For most games, video highlights are available."))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Travel Route: "), "Visual display highlighting travel routes for the selected team prior to a game."))),
        tags$br(),
        
        size = "l",
        
        easyClose = TRUE)
      
      )#showmodal
    
    
  })#observeEvent 
  
  
  #Code to enable users to select a date (to look at a specific game)
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
  
  #youtube button. Displays video highlights of the selected game
  video <- reactive({
    
    req(input$date_filter)
    req(input$team_filter)
    
    full_join(highlights, highlights2) %>%
      filter(Team == input$team_filter) %>%
      filter(Date == input$date_filter) %>%
      mutate(id = sub('.*\\=', '', Link)) %>%
      mutate(Link = paste("https://www.youtube.com/embed/", id, sep = ""))
    
  })
  
 
  
  #if there is not video available for that game, then the button will be hidden
  observe({
    
    
    if(is.null(video()$Link) || length(video()$Link) == 0 || is.na(video()$Link) || video()$Link == "") {
      shinyjs::hide("video_yes")
    } else {
      shinyjs::show("video_yes")
    }
  })
  
  
  
  #the popup window where the video will be displayed
  observeEvent(input$video_yes, {
    
    showModal(
      
      modalDialog(
        
        tags$iframe(src = video()$Link, width = "100%", height = "600px"),
        
        size = "l",
        
        easyClose = TRUE)
      
    )#showmodal
    
    
  })#observeEvent 
  
  
  #This code generates a map showing flight paths for each game
  output$map_plot <- renderPlot({
    
    req(input$date_filter)
    req(input$team_filter)
    
    acitiesc <- cities() %>% filter(Team == input$team_filter)
    
    acitiesd <- cities() %>% 
      filter(Team == input$team_filter) %>% 
      filter(Date == input$date_filter)
    
    #create basemap of the US
    maps::map("world", regions=c("usa"), fill=T, col="#17202a", bg="transparent", ylim=c(21.0,50.0), xlim=c(-130.0,-65.0))
    points(acities$Longitude, acities$Latitude, pch=7, cex=1, col="chocolate1")
    
    
    for (i in (1:dim(acitiesc)[1])) { 
      
      #adds lines for all routes in that season
      inter <- geosphere::gcIntermediate(c(acitiesc$destLon[i], acitiesc$destLat[i]), c(acitiesc$Longitude[i], acitiesc$Latitude[i]), n=500)
      lines(inter, lwd=0.5, col="grey30", lty=1)
      
    }
    
    #adds route for the selected game
    inter2 <- geosphere::gcIntermediate(c(acitiesd$destLon, acitiesd$destLat), c(acitiesd$Longitude, acitiesd$Latitude), n=200)
    lines(inter2, col="#ccff00", lwd = 5)
    
    
  })
  
  #the below code filters the data set that we will be using for this dashboard
  
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
  
  #each item of the dashboard has its own individual code. Here we will create each iteam:
  
  
  #code to add the selected team logo
  output$team_logo <- renderUI({
    
    HTML(city()$Team_Logo)
    
  })
  
  #code to add the opponents logo
  output$opponent_logo <- renderUI({
    
    HTML(city()$Opp_Logo)
    
  })
  
  #code to add the name of the selected team
  output$team <- renderUI({
    
    tags$h1(city()$Team, style = "color:white")
    
  })
  
  #code to display the name of the opponent team
  output$opponent <- renderUI({
    
    tags$h1(city()$Opponent, style = "color:white")
    
  })
  
  #Code showing whether the selected team is playing at home or away
  output$location_team <- renderUI({
    
    if(city()$Location == "Home"){
      
      a <- paste("<span style=color:lightblue>", city()$Location, "</span>")
      
      
    }else{
      
      a <- paste("<span style=color:salmon>", city()$Location, "</span>")
      
    }
    
    tags$h4(HTML(a))
    
  })
  
  #code showing whether the Opponent is playing at home or away
  output$location_opponent <- renderUI({
    
    if(city()$Location == "Home"){
      
      a <- paste("<span style=color:salmon>", "Away", "</span>")
      
      
    }else{
      
      a <- paste("<span style=color:lightblue>", "Home", "</span>")
      
    }
    
    tags$h4(HTML(a))
    
  })
  
  #Final points scored for the selected team
  output$team_points <- renderUI({
    
    tags$h3(city()$Points)
    
  })
  
  #Final points scored for the opponent
  output$opponent_points <- renderUI({
    
    tags$h3(city()$`Opp Points`)
    
  })
  
  #Game density label for the selected team and game
  output$density_team <- renderUI({
    
    if(city()$Density == "4IN5-B2B"){
      
      a <- paste("<span style=color:#ff0000>", city()$Density, "</span>")
    
    }else if(city()$Density == "3IN4-B2B"){
      
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
  
  #Game density label for the opponent
  output$density_opponent <- renderUI({
    
    
    if(city()$`Opp Density` == "4IN5-B2B"){
      
      a <- paste("<span style=color:#ff0000>", city()$`Opp Density`, "</span>")
    
    }else if(city()$`Opp Density` == "3IN4-B2B"){
      
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
  
  
  #Game Index Score for the selected team and game
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
  
  #Game index score for the opponent
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
  
  
  #Rolling index score for the selected team and game
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
  
  #Rolling index score for the opponent
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
  
  #text label for index scores
  output$text_index1 <- renderUI({
    
    tags$h5("Game Index:")
    
  })
  
  #text label for index scores
  output$text_index2 <- renderUI({
    
    tags$h5("Game Index:")
    
  })
  
  #text label for rolling index
  output$text_movindex1 <- renderUI({
    
    tags$h5(paste(input$rolling, "Days Moving Index:", sep = " "))
    
  })
  
  #text label for rolling index
  output$text_movindex2 <- renderUI({
    
    tags$h5(paste(input$rolling, "Days Moving Index:", sep = " "))
    
  })
  
  #Creates a dynamic label saying "winner" below the points only for the winning team that night.
  output$team_label <- renderUI({
    
    if(city()$Outcome == "W"){
      
      a <- paste("<span style= background-color:seagreen; font-color:white>", " Winner ", "</span>")
      
      
    }else{
      
     a <- tags$br()
      
    }
    
    HTML(a)
    
    
  })
  
  #Same as above, but this time for the opponent team
  output$opponent_label <- renderUI({
    
    if(city()$Outcome == "W"){
      
      a <- tags$br()
      
      
    }else{
      
      a <- paste("<span style= background-color:seagreen; font-color:white>", " Winner ", "</span>")
      
      
    }
    
    HTML(a)
    
    
  })
  
  #Dinamically display the name of the city where the game is helps
  output$city <- renderUI({
    
    req(input$team_filter)
    req(input$season_filter)
    
    if(is.na(city()$City)){
      
      p(" ")
      
    }else{
    
    tags$h1(city()$City, style = "color:white")
      
    }
    
  })
  
  #Dinamically displayed the name of the arena where the game is played
  output$arena <- renderUI({
    
    req(input$team_filter)
    req(input$season_filter)
    
    if(is.na(city()$Arena)){
      
      p(" ")
      
    }else{
    
    tags$h5(city()$Arena)
      
    }
    
  })
  
  #Displays a label indicating the travel route (if any), prior to the game for the selected team
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
  
  #dynamic label showing distance traveled prior to the game
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
  
  
  #The code below creates a shotchart card, used to displayed locations of shots and whether they are made or missed for each 
  #player
  
  
  #This code let users select the team
  
  output$team.shotchart <- renderUI({
  
  pickerInput(
    inputId = "team_shotchart",
    label = "Select Team",
    choices = c(city()$Team, city()$Opponent),
    multiple = F
  )
    
  })
  
  #This code enables users further filter the data
  aa11 <- reactive({
    
    req(input$team_shotchart)
    req(input$season_filter)
      
    shotchart %>%
    
    filter(Season == input$season_filter) %>% 
      
    filter(Team == input$team_shotchart)
      
    
  })
  
  #this code lets users filter by specific players in the selected game
  output$player.shotchart <- renderUI({
    
    #input
    pickerInput(
      inputId = "player_shotchart",
      label = "Select Player",
      choices = as.character(aa11()$Player) %>% unique(),
      multiple = F
    )
    
    
  })
  
  #reactive datasets
 aa22 <- reactive({
   
    aa11() %>% 
     
    filter(Player == input$player_shotchart) %>% #dinamically populate players based on team selected
    
    mutate(Event = ifelse(Event == "Missed Shot", "Missed", "Made")) %>%
     
    mutate(Player = as.character(Player)) %>%
    
    full_join(pro_file, by = c("Player")) %>%
    
    na.omit() %>%
    mutate(Image = gsub(" ", "", Image))
   
 })
 
 #this code creates the plot with the basketball court and shot locations
 
 output$shotchart <- renderPlot({
   
   req(input$date_filter)
   
   # images of the basketball court and playes displayed on the chart
   courtImg.URL <- "https://thedatagame.files.wordpress.com/2016/03/nba_court.jpg"
   court <- rasterGrob(readJPEG(RCurl::getURLContent(courtImg.URL)), width = unit(1,"npc"), height = unit(1,"npc"))
   plaIMG <- rasterGrob(readJPEG(RCurl::getURLContent(unique(aa22()$Image))))
   
   # plot using NBA court background and coloured by shot zone
   ggplot(aa22() %>% filter(Zone != "Restricted Area"), aes(x=locX, y=locY)) + 
     
     annotation_custom(court, -250, 250, -50, 420) +
     annotate("rect", xmin = -248, xmax = 248, ymin = 350, ymax = 420, fill = "white") +
     annotate("rect", xmin = -250, xmax = -245, ymin = -50, ymax = 420, fill = "white", color = "white") +
     annotate("rect", xmin = 245, xmax = 250, ymin = -50, ymax = 420, fill = "white", color = "white") +
     annotate("rect", xmin = -248, xmax = 248, ymin = -50, ymax = -43, fill = "white") +
     annotate("rect", xmin = -248, xmax = -210, ymin = 225, ymax = 250, fill = "white") +
     annotate("rect", xmin = 210, xmax = 248, ymin = 225, ymax = 250, fill = "white") +
     annotate("rect", xmin = -115, xmax = -107, ymin = -49, ymax = -40, fill = "white") +
     annotate("rect", xmin = 107, xmax = 115, ymin = -49, ymax = -40, fill = "white") +
     annotate("rect", xmin = -250, xmax = 250, ymin = -50, ymax = 420, fill = "black", alpha = 0) +
     annotate("rect", xmin = -77, xmax = 79, ymin = -50, ymax = 140, fill = "black", alpha = 0) +
     
     
     stat_binhex(bins = 15, color = "transparent", alpha = 0.4) +
     
     scale_fill_gradientn(colours = c("white", "black")) +
     
     geom_point(data = aa22() %>% filter(Date == input$date_filter), aes(color=Event), pch = 19, size = 5, alpha = 0.7) + #dinamilly adjust date based on date filter
     
     #geom_rug(color = "grey", alpha = 0.4) +
     
     annotation_custom(grob = plaIMG, xmin = 160, xmax = 254, ymin = 300, ymax = 419) +
     
     annotate("text", x = -180, y = 415, label = "@NBAGameDensity", color = "grey10", fill = "steelblue4") +
     
     coord_equal() +
     
     xlim(-250, 250) +
     ylim(-50, 420) +
     
     xlab("") + ylab("") + labs(color = "Game Shots", fill = "Season Frequency") +
     
     scale_color_manual(values = c("#148f77", "#a93226"))+
     
     expand_limits(x = 0, y = 0) +
     
     theme_bw() +
     
     theme(panel.grid = element_blank(),
           axis.text = element_blank(),
           panel.border = element_rect(colour = "#212f3c", fill=NA, size=5),
           axis.line = element_blank(),
           axis.ticks = element_blank(),
           panel.background = element_rect(fill = "white"),
           panel.spacing = element_blank(),
           legend.position= "right",
           axis.title = element_blank(),
           legend.title = element_text(face = "bold", color = "black", size = 14),
           legend.text = element_text(color = "steelblue4", size = 12),
           legend.justification = "top")
   
   
 })
 
 
  
  #popup window where the shotcharts and filters are displayed. It is open when user clicks on the button
  
  observeEvent(input$shotchart, {
    
    showModal(tags$div(id="shotchartmodal", modalDialog(
      
      size = "l",
      easyClose = TRUE,
      
      fluidRow( 
        
        column(width = 4,
               
               
              uiOutput("team.shotchart"),
              uiOutput("player.shotchart")
              
               
        ),
        
        column(width = 8,
               plotOutput("shotchart", width = "100%")
                )
        
      )
      
    )))
    
  })
    
    
    #The following code sets functionality to show a table with stats for the selected game
    
    #filter for by team (either the selected team or opponent for the game)
    output$team.gamelogs <- renderUI({
      
      pickerInput(
        inputId = "team_gamelogs",
        label = "Select Team",
        choices = c(city()$Team, city()$Opponent),
        multiple = F
      )
      
    })
    
    #This codes creates the final table displayed
  output$gameLog <- DT::renderDataTable({ 
    
    brks <- quantile(game_logs$Load, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}  
    
    
   ggLog <- game_logs %>%
     
     filter(Season == input$season_filter) %>%
     filter(Date == input$date_filter) %>%
     filter(Team == input$team_gamelogs) %>%
     
      select(-Season, -Date, -Team_Rest, -Team) %>%
      arrange(Player) %>%
      mutate(FG = paste(FG_M, "/", FG_A, sep = "")) %>%
      mutate(FG3 = paste(FG3_M, "/", FG3_A, sep = "")) %>%
      mutate(FG2 = paste(FG2_M, "/", FG2_A, sep = "" )) %>%
      mutate(FT = paste(FT_M, "/", FT_A, sep = "")) %>%
     
      
     
      select(Player, Rest, MINS, Load, FG, `FG_%`, FG2, `FG2_%`, FG3, `FG3_%`, FT, `FT_%`, OffReb, DeffReb, TotalReb, AST, STL, BLK, TOV, PF, PTS, `+/-`, Photo) %>%
     
     
      mutate_all(list(~replace_na(.,""))) %>% 
     
      mutate(Photo = paste('<img src =',' "',Photo,'" ', 'height="60"></img>', sep = "")) %>% 
      mutate(`FG (%)` = paste(FG, " ", "(",`FG_%`,")", sep = "")) %>%
      mutate(`FG2 (%)` = paste(FG2, " ", "(",`FG2_%`, ")", sep = "")) %>%
      mutate(`FG3 (%)` = paste(FG3, " ", "(",`FG3_%`, ")", sep = "")) %>%
      mutate(`FT (%)` = paste(FT, " ", "(",`FT_%`, ")", sep = "")) %>%
     
     
    select(Photo, Player, `Rest (days)` = Rest, MINS, Load, `FG (%)`, `FG2 (%)`, `FG3 (%)`, `FT (%)`, OffReb, DeffReb, TotalReb, AST, STL, BLK, TOV, PF, PTS, `+/-`) %>% 
     
    mutate(Load = as.numeric(Load)) %>%    
    arrange(desc(Load)) %>%
     
    formattable::formattable(list(
      
      `Rest (days)` = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "0", "#f04d43", 
                                                                                            ifelse(x == "1", "#f68235",
                                                                                            ifelse(x == "2", "#faaf18",
                                                                                            ifelse(x == "3", "#b2ba35",
                                                                                            ifelse(x == "4", "#2ea8c7", "#82eefd")))))))
      
    ))
   
   
        as.datatable(ggLog, 
        rownames = FALSE,
        escape = FALSE,
        options = list(dom = 't',
        lengthChange = TRUE,
        pageLength = 90, 
        bSort=F,
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        initComplete = JS("function(settings, json) {", "$(this.api().table().header()).css({'background-color': '#17202a', 'color': '#fff'});", "}"))) %>%
        
        formatStyle('Player', fontWeight = 'bold', backgroundColor = 'rgb(52,62,72)', color = "ivory") %>%
        formatStyle('Photo', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('Rest (days)', backgroundColor = 'rgb(52,62,72)', fontWeight = 'bold') %>%
        #formatStyle('Load', backgroundColor = 'rgb(52,62,72)', fontWeight = 'bold', color = "ivory") %>%
        formatStyle('Load', backgroundColor = styleInterval(brks, clrs), color = "black", fontWeight = 'bold') %>%
          
        formatStyle('FG (%)', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('FG2 (%)', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('FG3 (%)', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('FT (%)', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('OffReb', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('DeffReb', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('TotalReb', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('AST', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('STL', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('BLK', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('TOV', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('PF', backgroundColor = 'rgb(52,62,72)') %>%
        formatStyle('MINS', backgroundColor = 'rgb(52,62,72)', fontWeight = 'bold') %>%
        formatStyle('PTS', backgroundColor = 'rgb(52,62,72)', color = "ivory", fontWeight = 'bold') %>%
        formatStyle('+/-', backgroundColor = 'rgb(52,62,72)', color = "ivory", fontWeight = 'bold') %>%
        
        formatStyle(c(2, 5, 9, 12, 17, 18, 19), `border-right` = "solid 1px", `border-color` = "grey") 
      
    
  })
    
    
  #popup window where the stats table and filters are displayed. It is open when user clicks on the button
    observeEvent(input$gamelogs, {
      
      showModal(tags$div(id="gamelogmodal", modalDialog(
        
        size = "l",
        easyClose = TRUE,
        
        fluidRow(
         
          column(width = 12, offset = 12,
        
        dropdown(
          
          tags$h2("Load", style = "color:black"),
          tags$hr(),
          tags$h4("Load is an arbitrary number that ranges from 0 to 100. The higher the number the higher the load.", style = "color:rgb(52,62,72)"),
          tags$br(),
          tags$h4("The load parameter is a product of the number of rest days for each individual player and total minutes played.", style = "color:rgb(52,62,72)"),
          tags$br(),
          tags$h4("For example, 0 Rest Days X 20mins will have higher load and 2 Rest Days x 20mins.", style = "color:rgb(52,62,72)"),
          
          style = "stretch", icon = icon("question"),
          status = "danger", width = "200px",
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInLeftBig,
            exit = animations$fading_exits$fadeOutRightBig
          )))),
        
        fluidRow(column(width = 4, uiOutput("team.gamelogs"))),
        fluidRow(column(width = 12, DT::dataTableOutput("gameLog")))
          
        
      )))
      
    })
    
    
    #the code below adds a dynamic tab displaying season winning percentage
    win.pct <- reactive({
      
    pct <- NBAStandingsByDate(input$date_filter)
    
    East <- pct$East %>% select(Team = eastern_conference, pct = w_lpercent)
    
    West <- pct$West %>% select(Team = western_conference, pct = w_lpercent)
    
    full_join(East, West, by = c("Team", "pct")) %>% arrange(-pct) %>%
      mutate(Team = gsub("\\*", "", Team))
    
    })
    
    
    #winning % for the selected team
    output$team.pct <- renderUI({
      
      team_pct <- win.pct() %>% filter(Team == city()$Team)
      
      a <- paste("Win %: ", team_pct$pct)
      
      HTML(a)
      
    })
    
    #winning % for the opponent
    output$opponent.pct <- renderUI({
      
      opp_pct <- win.pct() %>% filter(Team == city()$Opponent)
      
      a <- paste("Win %: ", opp_pct$pct)
      
      HTML(a)
      
    })
  
  
  ################################################
  
  #Player Load Tab Reactivity####
    
  #intro popup window with an explanation of the dashboard. It opens when user clicks on the info icon.
    observeEvent(input$pLoad, {
      
      
      showModal(
        
        modalDialog(
          
          tags$h1(icon("charging-station")),
          tags$br(),
          tags$h3("How is Load calculated?", style = "font-family: Arial; color: white"),
          tags$hr(),
          tags$br(),
          tags$h5("Load is an arbitrary number that ranges from 0 to 100. The higher the number the higher the load.", style = "font-family: Arial; color: white"),
          tags$br(),
          tags$h5("The load parameter is a product of the number of rest days for each individual player and total minutes played.", style = "font-family: Arial; color: white"),
          tags$br(),
          tags$h5("For example, 0 Rest Days X 20mins will result in a higher load than 2 Rest Days x 20mins.", style = "font-family: Arial; color: white"),
          
          
          size = "m",
          
          easyClose = TRUE)
        
      )#showmodal
      
      
    })#observeEvent 
    
    
  #This code creates a heatmap plot comparing load scores for all players in the selected team / season
  output$player_load <- renderPlot({
    
    #filters and cleans the data
    a <- game_logs %>%
      filter(Team == input$team_filter) %>%
      filter(Season == input$season_filter) %>%
      select(Date, Player, Load)
    
    #creates the actual plot displayed
    ggplot(a, aes(x=reorder(Date, Player), y = Player, fill = Load)) +
      geom_tile(colour="#273746",size=0.25) +
      labs(x="\n Regular Season Games \n", y="", title = paste("\n", input$team_filter, "Player Load for Season", input$season_filter, sep = " "), 
           subtitle = "\n Start of Season >", caption = "https://josedv.shinyapps.io/NBASchedule/ \n") +
      scale_y_discrete(expand=c(0,0)) +
      scale_x_discrete(expand=c(0,0)) +
      scale_fill_gradient(low = "white", high = "red") +
      theme_grey(base_size=8) +
      guides(fill=guide_legend(title="Player Load", reverse = TRUE)) +
      theme(
        legend.text=element_text(face="bold", color = "grey", size = 10),
        axis.ticks=element_line(size=0.4),
        plot.background=element_rect(fill = "#273746", colour = "#273746"),
        axis.text.y = element_text(size = 12, color = "lightgray"),
        legend.background = element_blank(),
        axis.text.x = element_text(size = 12, color = "white", angle = 90),
        plot.title = element_text(size = 24, color = "white"),
        plot.caption = element_text(size = 12, color = "lightblue"),
        plot.subtitle = element_text(size = 12, color = "darkgray"),
        axis.title.x = element_text(size = 12, color = "darkgray", hjust = 1),
        legend.title = element_text(color = "gray", size = 14, face = "bold"),
        legend.key = element_blank(),
        panel.background = element_rect(fill = "#212f3c"),
        panel.grid = element_blank(),
        panel.border=element_blank())  
    
  })  
    
    
  #This code displays a dynamic trendline plot showing game load values for every game and player in the selected season
    output$playerLoad.filter <- renderUI({
    
      #filter the data
     choices <- game_logs %>%
        filter(Team == input$team_filter) %>%
        filter(Season == input$season_filter) %>%
        select(Player) %>%
        unique()
    
    #input to filter by player 
    pickerInput(
      inputId = "playerLoad_filter",
      label = "", 
      choices = choices$Player,
      choicesOpt = list(style = rep(("color: black; background: white"),30), content = choices$Player),
      multiple = F)
    
})

  #code to add a profile (headshot) image of the selected player to the report
    output$player_image <- renderUI({
      
      req(input$playerLoad_filter)
      
      a <- game_logs %>%
        filter(Team == input$team_filter) %>%
        filter(Season == input$season_filter) %>%
        select(Player, Photo) %>%
        filter(Player == input$playerLoad_filter) %>%
        unique() %>%
        mutate(photo = paste("<img src='", Photo, "' width=100%></img>", sep = ""))
      
      HTML(a$photo)
      
      
    })
    
    #creates a reactive table that let users select x number of games to look at accumulated load parameters in that period
    output$last.n.filter <- renderUI({
      
      searchInput(
        inputId = "last.n_filter",
        label = "Select Last N Games", 
        placeholder = "Enter a number",
        value = 82,
        resetValue = 82,
        btnSearch = icon("search"), 
        btnReset = icon("remove"),
        width = "100%"
      )
      
      
      
    })
    
    
  #this code displays a mini table next to the main plot showing accumulated load factors over a selected number of games
  output$playerLoad.table <- DT::renderDataTable({
    
  #cleans and filters the data as per user input
   a <- game_logs %>%
      filter(Team == input$team_filter) %>%
      filter(Season == input$season_filter) %>%
      filter(Player == input$playerLoad_filter) %>%
      select(Date, Load, Rest, MINS, Team_Rest) %>%
      
      top_n(as.numeric(input$last.n_filter), Date) %>%
      
      mutate(`Days Elapsed` = max(Date) - min(Date)) %>%
      
      mutate(Rest = ifelse(Date == min(Date), 0, Rest)) %>%
      mutate(Team_Rest = ifelse(Date == min(Date), 0, Team_Rest)) %>%
      
      summarise(`Time Span` = mean(`Days Elapsed`), `Team Rest` = sum(Team_Rest), `Player Rest` = sum(Rest), `Player Load` = mean(Load), `Total Mins` = sum(MINS), `Mins per Game` = mean(MINS)) %>%
      gather(Metric, Value) %>%
      
      mutate(Value = as.integer(Value)) %>%
      
      mutate(Value = ifelse(Metric == "Time Span", paste(Value, " days"), 
                            ifelse(Metric == "Team Rest", paste(Value, " days"),
                                   ifelse(Metric == "Player Rest", paste(Value, " days"), Value)))) %>%
      
      formattable::formattable()
    
    #creates table
    as.datatable(a, 
                 rownames = FALSE,
                 escape = F,
                 options = list(dom = 't', 
                                pageLength = 90, 
                                bSort=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                headerCallback = JS(
                                  "function(thead, data, start, end, display){",
                                  "  $(thead).remove();",
                                  "}"))) %>%
      
      formatStyle('Metric', backgroundColor =  'rgb(52,62,72)') %>%
      formatStyle('Value', backgroundColor =  'rgb(52,62,72)') 
                 
    
  })
    
  
  #Dynamic trend plot
    output$player.load.trend <- renderPlotly({
      
      req(input$playerLoad_filter)
      
      #cleans and filters the data
      a <- game_logs %>%
        filter(Team == input$team_filter) %>%
        filter(Season == input$season_filter) %>%
        filter(Player == input$playerLoad_filter) %>%
        select(Date, Load, Rest, MINS, Team_Rest) %>%
        mutate(Games = n())
      
     #creates plot 
   a2 <- ggplot(data = a, aes(Date, Load)) + 
      
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=0, ymax=10), fill = "#f9ebea") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=10, ymax=20), fill = "#f2d7d5") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=20, ymax=30), fill = "#e6b0aa") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=30, ymax=40), fill = "#d98880") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=40, ymax=50), fill = "#cd6155") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=50, ymax=60), fill = "#c0392b") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=60, ymax=70), fill = "#a93226") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=70, ymax=80), fill = "#922b21") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=80, ymax=90), fill = "#7b241c") +
      geom_rect(mapping=aes(xmin=min(Date), xmax=max(Date), ymin=90, ymax=100), fill = "#641e16") +
     
      geom_hline(aes(yintercept = mean(Load) + (sd(Load) * 0.5)), color =  "#343E48", linetype = 2, size = 1) +
      geom_hline(aes(yintercept = mean(Load) - (sd(Load) * 0.5)), color =  "#343E48", linetype = 2, size = 1) +
      
      geom_line(color = "#17202a", size = 0.9) +
      geom_point(color = "#17202a", size = 3,  
                 aes(text = paste(' Date: ', Date, '<br> Player Load: ', Load, '<br> Team Rest: ', Team_Rest, '<br> Player Rest: ', Rest, '<br> Mins Game: ', MINS))) +
      scale_x_date(a$Date) +
      scale_y_continuous(breaks = seq(0,100,10)) +
      ylab("") + xlab("") +
      
      theme(
        plot.background=element_rect(fill = "#343E48", colour = "#343E48"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 16, color = "grey", vjust = 5),
        plot.title = element_text(size = 24, color = "white"),
        plot.caption = element_text(size = 8, color = "lightblue"),
        plot.subtitle = element_text(size = 12, color = "darkgray"),
        panel.background = element_rect(fill = "#343E48", colour = "#343E48"),
        panel.grid = element_blank(),
        panel.border=element_blank())
   
  
   #adds ggplotly interactivity
   ggplotly(a2, tooltip= c("text")) %>% 
     config(displayModeBar = FALSE) %>% 
     plotly::layout(titlefont = list(color="white"), title = list(text = paste0(
                                       '<br>',
                                       '<sup>',
                                       input$playerLoad_filter, " played in ", a$Games, " games in the ", input$season_filter, " season.",
                                       '</sup>'), x = 0.06))
      
      
    })
    
    
    
  ################################################
  
  #schedule table Tab Reactivity####
    
    
    #intro popup window showing overall explanation of the reports. Opens when user clicks on info button
    observeEvent(input$pIndex, {
      
      
      showModal(
        
        modalDialog(
          
          tags$h3("How is Game Density Index Calculated?", style = "font-family: Arial; color: white"),
          tags$hr(),
          tags$br(),
          tags$h5("Game index is a schedule density score that ranges between 0 - 100. The higher the number, the more 'schedule stress' for a given game.", style = "font-family: Arial; color: white"),
          tags$br(),
          img(src = "density.png", style = "text-align: center;"),
          tags$br(),
          tags$br(),
          tags$h5("Game index accounts for the following parameters:", style = "font-family: Arial; color: white"),
          tags$br(),
          tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Location: "), "Home or Away."))),
          tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Prior Travel: "), "Did the team travel prior to a game?."))),
          tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Travel Direction: "), "Eastbound, Westbound or Neutral (North/South without time zone changes)."))),
          tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Time Shift Factor: "), "Number of time zones crossed."))),
          tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Elapsed Days: "), "Days since last game."))),
          tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Game Type: "), "Game profile based on 1 Rest Day, 2 Rest Days, 3 Rest Days, B2B, 3in4 or 3in4-B2B."))),
          tags$br(),
          tags$h5("Users can adjust any of the above factors by using the inputs in the right side bar of the website which is accesiable via the 'cogs' icon 
                  on the top right part of the header", style = "font-family: Arial; color: white"),
          tags$br(),
          tags$br(),
          tags$h4("Moving Index", style = "font-family: Arial; color: white"),
          tags$hr(),
          tags$h5("Moving Index is a rolling average of the Game Index score.", style = "font-family: Arial; color: white"),
          tags$br(),
          tags$h5("It is set to seven days by default, but users are able to adjust this parameter between 1 and 30 days.", style = "font-family: Arial; color: white"), 
          tags$br(),
          
          size = "l",
          
          easyClose = TRUE)
        
      )#showmodal
      
      
    })#observeEvent 
    
    
  #main schedule density table, Displays several aspects related to games and schedule density for a selected team and its opponents  
  output$team_table <- DT::renderDataTable({
    
    #filter an clean data
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
      
      Density = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "4IN5-B2B", "#ff0000",
                                                                ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                   ifelse(x == "3IN4", "#ff3300", 
                                                                          ifelse(x == "B2B", "#cc6600",
                                                                                 ifelse(x == "1", "#cc9900",
                                                                                        ifelse(x == "2", "#cccc00",
                                                                                               ifelse(x == "3+", "#ccff00", "transparent"))))))))), 
      
      
    `Opp Density` = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "4IN5-B2B", "#ff0000",
                                                                ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                   ifelse(x == "3IN4", "#ff3300", 
                                                                          ifelse(x == "B2B", "#cc6600",
                                                                                 ifelse(x == "1", "#cc9900",
                                                                                        ifelse(x == "2", "#cccc00",
                                                                                               ifelse(x == "3+", "#ccff00", "transparent")))))))))
    
      
    )) 
    
    #creates table to be displayed
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
  
  #Rolling Density Plot Tab Reactivity#####
  
  #intro popup window showing overall explanation of the report that opens when the users clicks on the info icon
  observeEvent(input$denplot, {
    
    showModal(
      
      modalDialog(
        
        tags$h3("Moving Density Plot", style = "font-family: Arial; color: white"),
        tags$hr(),
        tags$br(),
        tags$h5("A comparison of the moving index for the selected team as well as the opponent.", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$br(),
        tags$h5("There are 3 different elements in this chart:", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Line Chart: "), "Game to game rolling index for the selected team (orange) vs the opponent teams (blue)"))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Season Boxplots: "), "A boxplot chart showing the distribution of the rolling density index for the whole season, comparing the selected team vs its opponents."))),
        tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Game Outcome Ticks: "), "The ticks are the bottom of the chart will be green if the selected team wins that game."))),
        tags$br(),
        tags$h5("The chart updates every time the user changes the length of the moving window on the right sidebar menu.", style = "font-family: Arial; color: white"),
        
        size = "m",
        
        easyClose = TRUE)
      
      )#showmodal
    
    
  })#observeEvent 
  
  
  
  
  #Creates the main plot shown in the tab
  output$team_plot <- renderPlotly({
    
    
    #dataset comparing rolling stress between local and opp teams
    aa <- sche2() %>% 
     
      select(`W/L`, Team, Opponent, Date, `Moving Index Density` = `movIndex`, `Opponent Moving Index` = `Opp movIndex`) %>%
      gather(Metric, Value, -Team, -Opponent, -Date, -`W/L`) %>%
      filter(Value > 0) %>%
      na.omit() %>%
      filter(Team == input$team_filter) %>%
      mutate(Teams = ifelse(Metric == "movIndex", paste(Team, "vs", Opponent, sep = " "), paste(Opponent, "vs", Team, sep = " ")))
    
    #blox plot code
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
    
    #trendline plot
    a2 <- ggplot() +
      geom_line(aa, mapping = aes(x = Date, y = Value, color = Metric), size = 0.5, linetype = 1) +
      geom_area(aa, mapping = aes(x = Date, y = Value, fill = Metric, color = Metric), alpha = .1, position = 'identity') +
      geom_point(aa, mapping = aes(x = Date, y = Value, color = Metric, text = Teams), size = 1.5) +
      geom_rug(aa %>% filter(Metric == "Moving Index Density"), mapping = aes(x = Date, color = `W/L`)) +
      scale_x_date(aa$Date) +
      scale_color_manual(name = "", values=c("red", "orange", "deepskyblue4", "darkgreen")) +
      scale_fill_manual(name = "", values=c("orange", "deepskyblue4")) +
      ggthemes::theme_economist() +
      xlab("") + ylab("") +
      theme(legend.position = "none")
    
    a2 <- ggplotly(a2)
    
    #put together both plots so it appears just one plot combined
    a3 <- subplot(a2, a1, widths = c(.8,.2))
    
    config(a3, displayModeBar = FALSE)
    
    
  })
  
  
  ################################################
  
  #Outcome Tab Reactivity####
  
  #intro popup window that displays general info about the report when user click the info icon
  observeEvent(input$counts, {
    
    showModal(
      
      modalDialog(
        
        tags$h3("Count Tables", style = "font-family: Arial; color: white"),
        tags$hr(),
        tags$br(),
        tags$h5("These tables provide summary counts for different situations in the season for the selected team. Where appropriate, counts are split by game outcomes.", style = "font-family: Arial; color: white"),
        tags$br(),
        
        
        size = "s",
        
        easyClose = TRUE)
      
    )#showmodal
    
    
  })#observeEvent 
  
  
  ####Table 1: table displaying number of Ws and Ls by game type
  output$w_l_table <- DT::renderDataTable({
  
  #cleaning and filtering the data to prep for table
  a <- sche2() %>% 
    
    filter(Team == input$team_filter) %>%
    select(Density, `Opp Density`, `W/L`) %>% na.omit()
  
  #team playing on 3+ rest days series
  
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
    
  `3+4IN5-B2B` <- a %>% filter(Density == "3+" & `Opp Density` == "4IN5-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3+ - 4IN5-B2B") 
  
  `3+3IN4` <- a %>% filter(Density == "3+" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3+ - 3IN4")
  
  `3+a` <- full_join(`3+3+`, `3+2`) %>% full_join(`3+1`) %>% full_join(`3+B2B`) %>% full_join(`3+3IN4-B2B`) %>% full_join(`3+3IN4`) %>% full_join(`3+4IN5-B2B`)
  
  #team playing on 2 rest days series
  
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
  
  `24IN5-B2B` <- a %>% filter(Density == "2" & `Opp Density` == "4IN5-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "2 - 4IN5-B2B")
  
  `23IN4` <- a %>% filter(Density == "2" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "2 - 3IN4")
  
  `2a` <- full_join(`23+`, `22`) %>% full_join(`21`) %>% full_join(`2B2B`) %>% full_join(`23IN4-B2B`) %>% full_join(`23IN4`) %>% full_join(`24IN5-B2B`)
  
  #team playing on 1 rest day series
  
  
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
  
  `14IN5-B2B` <- a %>% filter(Density == "1" & `Opp Density` == "4IN5-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "1 - 4IN5-B2B")
  
  `13IN4` <- a %>% filter(Density == "1" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "1 - 3IN4")
  
  `1a` <- full_join(`13+`, `12`) %>% full_join(`11`) %>% full_join(`1B2B`) %>% full_join(`13IN4-B2B`) %>% full_join(`13IN4`) %>%  full_join(`14IN5-B2B`)
  
  #team playing on 3in4 days series
  
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
  
  `3IN44IN5-B2B` <- a %>% filter(Density == "3IN4" & `Opp Density` == "4IN5-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4 - 4IN5-B2B")
  
  `3IN43IN4` <- a %>% filter(Density == "3IN4" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4 - 3IN4")
  
  
  `3IN4a` <- full_join(`3IN43+`, `3IN42`) %>% full_join(`3IN41`) %>% full_join(`3IN4B2B`) %>% full_join(`3IN43IN4-B2B`) %>% full_join(`3IN43IN4`) %>% full_join(`3IN44IN5-B2B`)
  
  #team playing on 3in4-B2B days series
  
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
  
  `3IN4-B2B4IN5-B2B` <- a %>% filter(Density == "3IN4-B2B" & `Opp Density` == "4IN5-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4-B2B - 4IN5-B2B")
  
  `3IN4-B2B3IN4` <- a %>% filter(Density == "3IN4-B2B" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "3IN4-B2B - 3IN4")
  
  `3IN4-B2Ba` <- full_join(`3IN4-B2B3+`, `3IN4-B2B2`) %>% full_join(`3IN4-B2B1`) %>% full_join(`3IN4-B2BB2B`) %>% full_join(`3IN4-B2B3IN4-B2B`) %>% full_join(`3IN4-B2B3IN4`) %>% full_join(`3IN4-B2B4IN5-B2B`)
  
  #team playing on B2B series
  
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
  
  `B2B4IN5-B2B` <- a %>% filter(Density == "B2B" & `Opp Density` == "4IN5-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "B2B - 4IN5-B2B")
  
  `B2B3IN4` <- a %>% filter(Density == "B2B" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "B2B - 3IN4")
  
  `B2Ba` <- full_join(`B2B3+`, `B2B2`) %>% full_join(`B2B1`) %>% full_join(`B2BB2B`) %>% full_join(`B2B3IN4-B2B`) %>% full_join(`B2B3IN4`) %>% full_join(`B2B4IN5-B2B`)
  
  
  #team playing on a 4in5-b2b
  `4IN5-B2B3+` <- a %>% filter(Density == "4IN5-B2B" & `Opp Density` == "3+") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "4IN5-B2B - 3+")
  
  `4IN5-B2B2` <- a %>% filter(Density == "4IN5-B2B" & `Opp Density` == "2") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "4IN5-B2B - 2")
  
  `4IN5-B2B1` <- a %>% filter(Density == "4IN5-B2B" & `Opp Density` == "1") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "4IN5-B2B - 1")
  
  `4IN5-B2BB2B` <- a %>% filter(Density == "4IN5-B2B" & `Opp Density` == "B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "4IN5-B2B - B2B")
  
  `4IN5-B2B3IN4-B2B` <- a %>% filter(Density == "4IN5-B2B" & `Opp Density` == "3IN4-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "4IN5-B2B - 3IN4-B2B")
  
  `4IN5-B2B4IN5-B2B` <- a %>% filter(Density == "4IN5-B2B" & `Opp Density` == "4IN5-B2B") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "4IN5-B2B - 4IN5-B2B")
  
  `4IN5-B2B3IN4` <- a %>% filter(Density == "4IN5-B2B" & `Opp Density` == "3IN4") %>%
    group_by(`W/L`) %>%
    summarise(no_rows = length(`W/L`)) %>% mutate(Type = "4IN5-B2B - 3IN4")
  
  `4IN5-B2Ba` <- full_join(`4IN5-B2B3+`, `4IN5-B2B2`) %>% full_join(`4IN5-B2B1`) %>% full_join(`4IN5-B2BB2B`) %>% full_join(`4IN5-B2B3IN4-B2B`) %>% full_join(`4IN5-B2B3IN4`) %>% full_join(`4IN5-B2B4IN5-B2B`)
  
  
  #joins all subtables above to create a unified table
  ab <- full_join(`3+a`, `2a`) %>% full_join(`1a`) %>% full_join(`3IN4a`) %>% full_join(`3IN4-B2Ba`) %>% full_join(B2Ba) %>% full_join(`4IN5-B2Ba`) %>%
    spread(`W/L`, no_rows) 
  
  ab[is.na(ab)] <- 0
  
  aaab <- ab %>%
    separate(Type, c("Team", "-", "Opponent"), sep = " ", remove = FALSE) %>%
    select(-Type, -`-`) %>%
    
    mutate(order = ifelse(Team == "4IN5-B2B", 1,
             ifelse(Team == "3IN4-B2B", 2, 
                          ifelse(Team == "3IN4", 3,
                                 ifelse(Team == "B2B", 4,
                                        ifelse(Team == "1", 5,
                                               ifelse(Team == "2", 6,
                                                      ifelse(Team == "3+", 7, "")))))))) %>%
    arrange(order) %>%
    select(-order) %>% 
    select(Team, Opponent, W, L) %>%
    
    formattable(list(
      
      Team = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "4IN5-B2B", "#ff0000", 
                                                                             ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                                ifelse(x == "3IN4", "#ff3300", 
                                                                                       ifelse(x == "B2B", "#cc6600",
                                                                                              ifelse(x == "1", "#cc9900",
                                                                                                     ifelse(x == "2", "#cccc00",
                                                                                                            ifelse(x == "3+", "#ccff00", "transparent"))))))))),
      
      Opponent = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "4IN5-B2B", "#ff0000", 
                                                                          ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                             ifelse(x == "3IN4", "#ff3300", 
                                                                                    ifelse(x == "B2B", "#cc6600",
                                                                                           ifelse(x == "1", "#cc9900",
                                                                                                  ifelse(x == "2", "#cccc00",
                                                                                                         ifelse(x == "3+", "#ccff00", "transparent")))))))))
    ))
   
  #final table displayed to user   
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
  

  
  ####Table 2: comparison of wins by Location
  output$H_A_table <- DT::renderDataTable({
  
  #cleans and filters the data
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
  
  #final table to be displayed  
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
  
  
  ####Table 3: Game type counts
  output$team_table_density <- DT::renderDataTable({
   
    #cleans and filters the data for the selected team 
    num <- sche2() %>%
      
      filter(Team == input$team_filter) %>%
      
      select(Team, Density) %>%
      na.omit() %>%
      group_by(Density) %>%
      tally() %>%
      select(Type = Density, n)
    
    num[is.na(num)] <- 0
    
    
    #cleans and filters the data for the opponent
    num2 <- sche2() %>%
      filter(Team == input$team_filter) %>%
      select(Opponent, `Opp Density`) %>%
      na.omit() %>%
      group_by(`Opp Density`) %>%
      tally() %>%
      select(Type = `Opp Density`, Opp = n)
    
    num2[is.na(num2)] <- 0
    
    #joins tables for selected team and opponent
    b <- full_join(num, num2) %>%
      
      
      mutate(order = ifelse(Type == "4IN5-B2B", 1, 
               ifelse(Type == "3IN4-B2B", 2, 
                            ifelse(Type == "3IN4", 3,
                                   ifelse(Type == "B2B", 4,
                                          ifelse(Type == "1", 5,
                                                 ifelse(Type == "2", 6,
                                                        ifelse(Type == "3+", 7, "")))))))) %>%
      arrange(order) %>%
      select(-order) %>%
      
      replace_na(list(n = 0, Opp = 0)) %>%
      
      
      formattable(list(
        
        Type = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "4IN5-B2B", "#ff0000",
                                                                               ifelse(x == "3IN4-B2B", "#ff0000", 
                                                                                            ifelse(x == "3IN4", "#ff3300", 
                                                                                                   ifelse(x == "B2B", "#cc6600",
                                                                                                          ifelse(x == "1", "#cc9900",
                                                                                                                 ifelse(x == "2", "#cccc00",
                                                                                                                        ifelse(x == "3+", "#ccff00", "transparent")))))))))))
    
    
    #creates final table to be displayed
    formattable::as.datatable(b, 
                              rownames = FALSE,
                              colnames = c("", input$team_filter, "Opposition"),
                              extensions = 'Responsive',
                              caption = HTML("Density counts by team & opponents.."),
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
  
#ALL TEAMS TAB.
####################################################################################################
  
  #density table tab#######
  
  #this code creates a table displaying counts of different game types for the selected season
  output$all_teams <- DT::renderDataTable({
    
    req(input$location_filter)
    
    #cleans and filters the data for the table
    df1 <- sche2() %>%
     
      filter(Location %in% input$location_filter) %>%
      select(Team, Density) %>%
      na.omit() %>%
      group_by(Team, Density) %>%
      tally() %>%
      select(`Type` = Density, n) %>%
      
      spread(Type, n) %>%
      select(val = Team, everything()) %>%
      ungroup() %>%
      
      replace_na(list(`3+` = 0, `2` = 0, `1` = 0, `B2B` = 0, `3IN4` = 0, `3IN4-B2B` = 0, `4IN5-B2B` = 0)) 
      
    
    #performs a jon with the table where we stored the team logos, so we can display the logos on the final table
    all <- full_join(df1, df) %>%
      select(Team = img, -val, everything()) %>%
      select(-img2, -val) %>%
      formattable(
        #list(
        #          `3+`= color_tile("orange", "hotpink"),
        #          `2`= color_tile("orange", "hotpink"),
        #          `1`= color_tile("orange", "hotpink"),
        #          `B2B`= color_tile("orange", "hotpink"),
        #          `3IN4`= color_tile("orange", "hotpink"),
        #          `3IN4-B2B`= color_tile("orange", "hotpink"),
        #          `4IN5-B2B`= color_tile("orange", "hotpink")
                  
        #          )
        )
    
   
    #code to create final table displayed to user
    formattable::as.datatable(all, 
                 rownames = FALSE,
                 extensions = 'Responsive',
                 #colnames = c("Team", "3+", "2", "1", "B2B", "3IN4", "3IN4-B2B", "4IN5-B2B"),
                 caption = "Density Type. Count by team for the selected season",
                 options = list(dom = 't',
                                bSort=F,
                                pageLength = 35,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#17202a', 'color': '#fff'});",
                                  "}")))
    
  })

  ###############################################
  
  #heatmap####  
  
  #intro popup window showing genral info about the report on this tab that opens when user clicks on info icon
  observeEvent(input$hmap, {
    
    showModal(
      
      modalDialog(
        
        tags$h3("Moving Density Heat Maps", style = "font-family: Arial; color: white"),
        tags$hr(),
        tags$br(),
        tags$h5("These heatmaps enable users to visualize all team's moving density scores at once.", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$h5("This makes it easier to spot patterns and identify periods of higher and lower schedule density for each team", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$h5("Lower scores are displayed as green, while colours become more red as the schedule density increases.", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$h5("The length of the moving window can be adjusted using the appropriate input on the right side bar.", style = "font-family: Arial; color: white"),
        tags$br(),
        
        size = "m",
        
        easyClose = TRUE)
      
    )#showmodal
    
    
  })#observeEvent 
    
  #input that let users select by "selected team" or opponent in the selected game
    output$by.team <- renderUI({
      
      radioGroupButtons(
        inputId = "by_team",
        choices = c("Teams" = "movIndex", "Opponents" = "Opp movIndex"),
        selected = "movIndex",
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: #273746"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: #273746"))
      )
      
    })
    
     
  #Code to create a heatmap plot showing rolling density scores for all teams
  output$heatmap <- renderPlot({
    
    req(input$by_team)
  
    #cleans and filters the data
  a <- sche2() %>%
      select(Team, movIndex, `Opp movIndex`) %>%
      gather(Metric, Value, -Team) %>%
      filter(Metric == input$by_team) %>%
      select(Team, movIndex = Value) %>%
      group_by(Team) %>%
      mutate(ID = row_number()) %>%
      filter(ID > input$rolling) %>%
      ungroup() %>%
      mutate(Team = as.factor(Team)) %>%
      mutate(movIndex = ifelse(movIndex == 0, 40, movIndex))
   
  #creates final plot 
 ggplot(a, aes(x=reorder(ID, Team), y = Team, fill = movIndex)) +
    geom_tile(colour="#273746",size=0.25) +
    labs(x="\n Regular Season Games \n", y="", title = paste("\n", input$rolling, "Games Rolling Density Index for all Regular Season Games in", input$season_filter, sep = " "), 
         subtitle = "\n Start of Season >", caption = "https://josedv.shinyapps.io/NBASchedule/ \n") +
    scale_y_discrete(expand=c(0,0)) +
    scale_x_discrete(expand=c(0,0)) +
    scale_fill_gradient(low = "springgreen", high = "red") +
    theme_grey(base_size=8) +
    guides(fill=guide_legend(title="Rolling\nDensity Index", reverse = TRUE ))+
    theme(
      legend.text=element_text(face="bold", color = "grey", size = 10),
      axis.ticks=element_line(size=0.4),
      plot.background=element_rect(fill = "#273746", colour = "#273746"),
      axis.text.y = element_text(size = 12, color = "lightgray"),
      legend.background = element_blank(),
      axis.text.x = element_text(size = 10, color = "grey"),
      plot.title = element_text(size = 24, color = "white"),
      plot.caption = element_text(size = 12, color = "lightblue"),
      plot.subtitle = element_text(size = 12, color = "darkgray"),
      axis.title.x = element_text(size = 12, color = "darkgray", hjust = 1),
      legend.title = element_text(color = "gray", size = 14, face = "bold"),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.border=element_blank())
  
    
  })
  
  
  ###############################################

  #flight paths####
  
  #plot showing mini maps with seasonal travel routes for all teams at once
  output$mapsf <- renderPlot({
   
    acitiesh <- cities() %>% filter(Route != "No Travel")
     
    ggplot() + 
      geom_polygon(data = map_data("usa"), aes(x=long, y = lat, group = group), fill = "#17202a", alpha = 0.6) + 
      geom_curve(data = cities() %>% filter(Route != "No Travel"), aes(x = destLon, y = destLat, xend = Longitude, yend = Latitude),curvature = 0.05, color = "#e8175d", size = 0.5) + 
      geom_point(data = acitiesh, aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), color = "cyan4", size = 1) +
      facet_wrap(~Team) +
      labs(title = "NBA Regular Season Flight Paths", 
           subtitle = paste("Season: ", input$season_filter, "\n"),
           caption = "Distances and flight paths are estimations and may not accurately represent actual travel management by teams.") +
      ggthemes::theme_solarized_2(light = F) +
      theme(panel.grid.major = element_line(color = "transparent"),
            panel.grid.minor = element_line(color = "transparent"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            strip.text = element_text(size = 12, color = "white"),
            strip.background = element_blank(),
            plot.title = element_text(size = 20, color = "white"),
            plot.caption = element_text(size = 12, face = "italic"),
            plot.background = element_rect(size = 0, fill = "#343e48", color = "#343e48"),
            panel.background = element_rect(fill = "transparent"))

   
  }, width = 1100, height = 700)
  
  #intro popup window to explain the report. It opens when users click on the info button
  
  observeEvent(input$fmap, {
    
    
    showModal(
      
      modalDialog(
        
        tags$h1(icon("plane-departure")),
        tags$br(),
        tags$h3("What are flight routes?", style = "font-family: Arial; color: white"),
        tags$hr(),
        tags$br(),
        tags$h5("Flight routes are estimations of what travel routes and flight paths may look like for each team.", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$h5("These are just estimations based on the location of each city and the time elapsed between games.", style = "font-family: Arial; color: white"),
        tags$br(),
        tags$h5("It is important to note that these routes may not be accurate representations of how teams actually managed their travel plans.", style = "font-family: Arial; color: white"),
        
        
        size = "m",
        
        easyClose = TRUE)
      
    )#showmodal
    
    
  })#observeEvent 
    
  
  ###############################################

  #all players load####
  
  #creates a dynamic table showing different load related metrics. Allows league wide player comparison
  
  #let users filter table by team
  output$team.filter2 <- renderUI ({
    
    pickerInput(
      inputId = "team_filter2",
      label = "Select Team(s)", 
      choices = df$val,
      choicesOpt = list(style = rep(("color: black; background: white"),30), content = df$img),
      options = list(`actions-box` = TRUE,  `selected-text-format` = "count > 2"),
      selected = c("Atlanta Hawks","Boston Celtics", "Brooklyn Nets", "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", "Dallas Mavericks",
                   "Denver Nuggets", "Detroit Pistons", "Golden State Warriors", "Houston Rockets", "Indiana Pacers", "Los Angeles Clippers", "Los Angeles Lakers",
                   "Memphis Grizzlies", "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", 
                   "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", "Toronto Raptors",
                   "Utah Jazz", "Washington Wizards"),
      multiple = T)
    
  })
  
  #let users filter the table by month
  output$month.filter <- renderUI ({
    
    choices <- game_logs %>%
      filter(Season == input$season_filter) %>%
      mutate(Month = lubridate::month(Date)) %>%
      
      mutate(Month = ifelse(Month == 1, 'January',
                            ifelse(Month == 2, 'February',
                                   ifelse(Month == 3, 'March',
                                          ifelse(Month == 4, 'April',
                                                 ifelse(Month == 5, 'May',
                                                      ifelse(Month == 6, 'June',
                                                             ifelse(Month == 7, 'July',
                                                                    ifelse(Month == 8, 'August',
                                                                           ifelse(Month == 9, 'September',
                                                        ifelse(Month == 10, 'October',
                                                               ifelse(Month == 11, 'November', 'December')))))))))))) %>% select(Month)
    
    pickerInput(
      inputId = "month_filter",
      label = "Select Month(s)", 
      choices = choices$Month %>% unique(),
      options = list(`actions-box` = TRUE,  `selected-text-format` = "count > 3"),
      selected = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
      multiple = T)
    
  })
  
  
  #let users filter the table by week
  output$week.filter <- renderUI ({
    
    choices <- game_logs %>%
      filter(Season == input$season_filter) %>%
      mutate(Month = lubridate::month(Date)) %>%
      
      mutate(Month = ifelse(Month == 1, 'January',
                            ifelse(Month == 2, 'February',
                                   ifelse(Month == 3, 'March',
                                          ifelse(Month == 4, 'April',
                                                 ifelse(Month == 5, 'May',
                                                        ifelse(Month == 6, 'June',
                                                               ifelse(Month == 7, 'July',
                                                                      ifelse(Month == 8, 'August',
                                                                             ifelse(Month == 9, 'September',
                                                               ifelse(Month == 10, 'October',
                                                                      ifelse(Month == 11, 'November', 'December')))))))))))) %>%
      mutate(Week = lubridate::week(Date)) %>%
      
      filter(Month %in% input$month_filter) %>%
      
      select(Month, Week)
    
    pickerInput(
      inputId = "week_filter",
      label = "Select Week(s) Number", 
      choices = choices$Week %>% unique(),
      options = list(`actions-box` = TRUE,  `selected-text-format` = "count > 5"),
      selected = 1:55,
      multiple = T)
    
  })
  
  #let users filter the table by top x rows (currently hidden)
  #output$top.rows.filter <- renderUI ({
    
    
  #  pickerInput(
  #    inputId = "row_filter",
  #    label = "Select N Top Rows", 
  #    choices = 1:600,
  #    options = list(`actions-box` = TRUE,  `selected-text-format` = "count > 5"),
  #    selected = 1:600,
  #    multiple = F)
    
  #})
  
  
  #let users sort the table by metric
  output$metric.filter <- renderUI ({
    
    
    pickerInput(
      inputId = "metric_filter",
      label = "Order by Metric", 
      choices = c("Total Load" = "totalLoad", "avg Load" = "avgLoad", "Total Mins" = "totalMins", "avg Mins" = "avgMins", "Games", "B2B", "Participation"),
      selected = "totalLoad",
      multiple = F)
    
  })
  
  
  #code to create the final table
 output$all.player.load <- DT::renderDataTable({
   
   #cleaning and filtering of data
   weekLoad <- game_logs %>%
     filter(Season == input$season_filter) %>%
     filter(Team %in% input$team_filter2) %>%
     mutate(Month = lubridate::month(Date), Week = lubridate::week(Date)) %>%
     
     mutate(Month = ifelse(Month == 1, 'January',
                           ifelse(Month == 2, 'February',
                                  ifelse(Month == 3, 'March',
                                         ifelse(Month == 4, 'April',
                                                ifelse(Month == 5, 'May',
                                                       ifelse(Month == 10, 'October',
                                                              ifelse(Month == 11, 'November', 'December')))))))) %>%
     filter(Month %in% input$month_filter) %>%
     filter(Week %in% input$week_filter) %>%
     group_by(Team, Player) %>%
     mutate(Date2 = lag(Date)) %>%
     mutate(dates = Date - Date2) %>% 
     mutate(B2B = ifelse(is.na(dates), 0, ifelse(dates == 1, 1, 0))) %>%
     summarise(Games = n(), totalMins = sum(MINS), avgMins = round(mean(MINS),1), avgLoad = round(mean(Load),1), totalLoad = sum(Load), B2B = sum(B2B), Participation = mean(Participation)) %>%
     select(Team, Player, totalLoad, avgLoad, totalMins, avgMins, Games, B2B, Participation) %>%
     
     ungroup()
   
   photo <- game_logs %>% distinct(Player, Photo)
   logs <- df %>% mutate(Team = val) %>% select(-val, -img)
   
   table <- full_join(weekLoad, photo, by = c("Player")) %>% 
     full_join(logs, by = c("Team")) %>%
     na.omit() %>%
     arrange(desc(totalLoad, avgLoad, totalMins, Games)) %>%
     select(-Team) %>%
     select(Photo, Player, totalLoad, avgLoad, totalMins, avgMins, Games, B2B, Participation, Team = img2) %>%
     mutate(Photo = paste('<img src =',' "', Photo,'" ', 'height="45"></img>', sep = ""))
     
     table2 <- dplyr::arrange_(table, lazyeval::interp(~desc(var), 
                                       var = as.name(input$metric_filter))) %>%
     
   
     #top_n(as.numeric(input$row_filter), input$metric_filter) %>%
   
     formattable::formattable()
   
    #creates final table to be displayed in the app
   formattable::as.datatable(table2, 
                             rownames = FALSE,
                             escape = F,
                             extensions = 'Responsive',
                             colnames = c("Photo", "Player", "Total Load", "avg Load", "Total Mins", "avg Mins", "Games", "B2B", "Participation (%)","Team"),
                             options = list(dom = 'pt',
                                            pageLength = 100,
                                            bSort=F,
                                            columnDefs = list(list(className = 'dt-center', targets = 0:8)),
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#17202a', 'color': 'ivory'});",
                                              "}"))) %>%
     
     formatStyle('Team', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('Photo', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('Player', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('totalLoad', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('avgLoad', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('totalMins', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('avgMins', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('Games', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('B2B', backgroundColor =  'rgb(52,62,72)') %>%
     formatStyle('Participation', backgroundColor =  'rgb(52,62,72)') %>%
     
     formatStyle(c(2, 4, 6, 9, 10), `border-right` = "solid 2px")
   
 })
 
 
 #help pop up modal
 
 observeEvent(input$player_loads, {
   
   
   showModal(
     
     modalDialog(
       
       tags$h3("Overall Player Load Summary", style = "font-family: Arial; color: white"),
       tags$hr(),
       tags$br(),
       tags$h5("This table provides a summary of different load related metrics. Several drill-down menus are provided for users to slice and explore the data in different ways.", style = "font-family: Arial; color: white"),
       tags$br(),
       tags$br(),
       tags$h5("The main load parameters shown on the table are:", style = "font-family: Arial; color: white"),
       tags$br(),
       tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Total Load: "), "The sum of the Load for each game the player participated in during the selected period."))),
       tags$br(),
       tags$h5(HTML(paste(tags$span(style="color: #FFA500", "avg Load: "), "The average of the Load for each game the player participated in during the selected period."))),
       tags$br(),
       tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Total Mins: "), "Total minutes played during the selected period."))),
       tags$br(),
       tags$h5(HTML(paste(tags$span(style="color: #FFA500", "avg Mins: "), "Minutes played per game in the selected period."))),
       tags$br(),
       tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Games: "), "Total number of games the player took part in during the period selected."))),
       tags$br(),
       tags$h5(HTML(paste(tags$span(style="color: #FFA500", "B2B: "), "Games of type B2B in the selected period."))),
       tags$br(),
       tags$h5(HTML(paste(tags$span(style="color: #FFA500", "Participation (%): "), "Percentage of games the player took part in during the selected season."))),
       
       size = "l",
       
       easyClose = TRUE)
     
     )#showmodal
   
   
 })#observeEvent 
  
  ###############################################
  
  
#RESEARCH / ARTICLES TAB
###################################################################################################
  
  #articles table####
 
  #cleans and filters the data to prep for final table
  art1 <- reactive({articles %>% 
    
    filter(Type %in% input$type.reads) %>%
    filter(Year >= input$year.reads[1] & Year <= input$year.reads[2]) %>%
    summarise(a = n())
    
  })
  
  #simple tag displaying number of articles selected
  output$item <- renderUI({
  
  tags$h3(paste(HTML(art1()$a, " items selected")), style = "color: slategray")
  
})
  
  
  #Code to create final table
  output$reads <- DT::renderDataTable({
    
    #cleans and filters the data
    art <- articles %>%
      mutate(Link = paste0("<a href='", Link,"' target='_blank'>", icon("link"),"</a>")) %>%
      
      filter(Type %in% input$type.reads) %>%
      filter(Year >= input$year.reads[1] & Year <= input$year.reads[2]) %>%
      
      arrange(desc(Year)) %>%
      
      formattable(list(
        
        Type = formattable::formatter("span", style = x ~ formattable::style(color = ifelse(x == "Research", "steelblue", "darkred")))
        
      ))
    
    #creates final table to be displayed
    as.datatable(art, 
                 rownames = FALSE,
                 extensions = c('Responsive', 'Buttons'),
                 class = 'cell-border stripe',
                 escape = FALSE,
                 caption = HTML("The following references are specific to Basketball / NBA. Topics include jet-lag, travel, sleep, home-court advantage, schedule density, etc. | Last update: April 2020 |"),
                 options = list(dom = 'Bt',
                                bSort=T,
                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print', I('colvis')),
                                pageLength = 500,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#17202a', 'color': '#fff'});",
                                  "}"))) %>%
      
      formatStyle('Type', backgroundColor =  '#bfc9ca', color = "#606060") %>%
      formatStyle('Year', backgroundColor =  '#bfc9ca', color = "#606060") %>%
      formatStyle('Title', backgroundColor =  '#bfc9ca', color = "#606060", fontWeight = "bold") %>%
      formatStyle('Journal', backgroundColor =  '#bfc9ca', color = "#606060") %>%
      formatStyle('Authors', backgroundColor =  '#bfc9ca', color = "#606060") %>%
      formatStyle('Link', backgroundColor =  '#bfc9ca', color = "#606060", fontWeight = "bold")
    
  })
  
  
  ###############################################
  

#Piece of code needed to hide the landing page after 3 secons####
  Sys.sleep(3)
  waiter_hide()
  
#piece for the sever loading screen
  sever(html = disconnected, bg_color = "#000")
####################################################################################################
  
  
}


###################################################################################################

#function to create render the app.
shinyApp(ui, server)


###################################################################################################

