 

# NBA Game Density App
An app to visually explore the density of the schedule for NBA teams as well as individual player loads based on rest days and minutes played.

## Logo

<img width="350" src="https://www.dropbox.com/s/5dqbiicb2uifdo2/waiter.gif?raw=1">


## Intro
The goal of this app is to enable users manipulate different aspects related to the game density (frequency of games) in the NBA for both, 
a selected team and its opponents over the last 3 seasons. Users can manipulate factors such as court advantage, travel stress, 
time zones crossed, game density index, direction of travel, etc. As all these are factors that can impact the fatigue of the players.

## Live Website
[NBA Game Density App](https://josedv.shinyapps.io/NBASchedule/)

## App Description
Professional sports leagues in America are characterized by dense schedules with teams having to play multiple games every week in different cities. Furthermore, there are many other factors such as late games, frequent travel, crossing different time zones across the country that may affect a team's performance. On top of this, opposing teams are also undergoing the same challenges. Research in the field of Sport Sciences shows the detrimental effect of these factors on games outcomes.

The goal of this app is to provide a platform for coaches to manipulate different game density factors to better understand what type of schedule stress teams are undergoing at different times during the season. This can help coaches plan training and recovery strategies with the aim to maximize player's performance.

In order to do this, the app does not only provide a summary of some of the main metrics in different forms and visualizations, but also a number of inputs (see right sidebar) to allow users to manipulate each potential factor in different ways. Hence the term "simulator".

## App Workflow

#### Left SideBar
The left side bar of the app provides users with the ability to filter by season (last 3 seasons). Besides that, there are 2 menu items 1) Team by Team and 2) All teams.

![](https://www.dropbox.com/s/vd2fsa4bzk59txj/leftside.png?raw=1)

#### Team by Team Item
A user input let users select a team of interest. Upon doing this, a mini-table with game density information is displayed below.

There are four tabs available within the team by team item:

##### Game Card
Comparison of different parameters for each game, including information about score, location, rest days, game index and moving index as well as a dynamic map tracking flight routes for the selected team prior to the game of interest.

![](https://www.dropbox.com/s/ahz38grcmii0dh8/mainview.png?raw=1)

Furthermore, a link to watch the **game highlights** is provided. Currently, more than 90% of the games have video highlights available, providing more than 200 hours of video footage. 

![](https://www.dropbox.com/s/8l3nslpq7jj87b6/frontpage.png?raw=1)

Likewise, **boxscores** for each game are available for both, the selected them and the oponent. Individual game loads are provided along with common game stats for different aspects of the game.

![](https://www.dropbox.com/s/1kki6whnxez7gdn/boxscore.png?raw=1)

Finally, users wishing to visualize **shotcharts** for every player/game can do that from the GameCard View. Upon selecting a player, the shot chart will highlight shots made/missed, the locations on the court. The court will have different degrees of shadows to highlight what are the most commong shooting locations for each player/season.

![](https://www.dropbox.com/s/kzk34afmbjeajwv/shotchart.png?raw=1)

##### Schedule Table
A dynamic table showing full season metrics. Different metrics are reported on this table, using color codes to alert users about the magnitude of the parameters. Users can compare differences in metrics between the selected team and the opponents they are facing throughout the season.

![](https://www.dropbox.com/s/k7oph5idrvxl01f/schedule.png?raw=1)

##### Rolling View Visualization
On the 3rd tab, users can interact with a dynamic visualization containing a line chart of the rolling density for the selected team (orange) and the opponents (blue). Complementary, a boxplot chart is provided to better understand the distribution of the data points throughout the season.

![](https://www.dropbox.com/s/2i1lkigsq0cmfi6/rollingview.png?raw=1)

##### Outcome Tab
The Outcome tab provides two tables. Table one (left) shows game outcomes by density type for each game. For example, wins and losses in situations when the selected team plays 3 games in 4 days (3IN4) vs a team playing on a back to back (B2B).

The second table (right) simply summarises the number of wins and losses by location (away or home). 

![](https://www.dropbox.com/s/bb9y2fma0ekdw3z/outcome.png?raw=1)

#### Player Load Tab
Player Load is an arbitrary score calculated based on how many rest days a player had prior to a game as well as the total number of minutes the player was on the court. Based on those two parameters an overall load metric is calculated. The **Player Load** tab enables users to explore longitudinal load trends for each player. 

![](https://www.dropbox.com/s/tqoytpq6gchj0sd/playerLoad.png?raw=1)

A dynamic search filter is available enabling users to select number of last games and provide cummulative load metrics for those.

#### All Teams Item
The all teams item provides a summary table counting all types of density for each team for each season. Users can filter by "away games", "home games" or both.

![](https://www.dropbox.com/s/goxco2tcp6l7bzv/counts.png?raw=1)

The Heatmap tab shows rolling index for each team for the whole season, enabling users compared this metric for all teams at any time.

![](https://www.dropbox.com/s/9anz1wz0eih6apq/heatmap.png?raw=1)

#### Travel Routes
The **travel route** tab displays approximate regular season flight paths for each team.

![](https://www.dropbox.com/s/hbvbmeq0rg26mtt/routes.png?raw=1)

The coordinates used are the main coordinates for each city. It is important to stress the fact that these paths are purely estimations based on the location of consecutive games as well as the time elapsed between them. Therefore, they may not be accurate representations of how teams actually managed their travels.

#### Player Load Table
The **Player Load** table enables league wide comparisons all load related metrics for all players in the season. A variety of filtering options are available at the top for users to slice and filter their data based on their preferences.

![](https://www.dropbox.com/s/5gobdig5zjmdwk8/playerLoadtable.png?raw=1)  


#### Research / Media Item
This part of the app contains a dynamic table showing NBA travel and density related research and media articles. While this research is independent from the author of this website, we thought it may be of interest for users wanting to learn more about how different factors related to schedule density may affect performance and injuries. 

All articles listed on the table contained a link that users can follow to reach the original text. New articles (if available) are uploaded monthly. 

![](https://www.dropbox.com/s/sn7yobimrytpqex/research.png?raw=1)

Potentially further reports will be implemented.

## Game Index Metric and Right Side Bar
Game Index is a metric that provides information about potential travel/schedule stress a team might be undergoing (note this metric is arbitrary and has not been validated). It is a composite score accounting for several factors such as rest days between games, travels prior to game, direction of travel, time zones crossed, accumulated density, game location. Rather than setting fixed weights for each parameters used to calculate overall game index, we provide options for users to manipulate each parameters based on their own preferences.

![](https://www.dropbox.com/s/mt8hpdwk1s224fj/rightbar.png?raw=1)

Users can also opt to look at a moving average of the Index metric for 'x' number of games. The last tab on the right side bar provides this option.

![](https://www.dropbox.com/s/s4v7y7i3a3jd28d/knob.png?raw=1)  


## Data Sources
The data used to create this app was downloaded from [BigDataBall](https://www.bigdataball.com/) and [Basketball-Reference](https://www.basketball-reference.com/). 

For more information on nomenclature used throughout this app when refering to types of rest days, please visit this [link](https://www.nbastuffer.com/2019-2020-nba-rest-days-stats/).

Game Stats, player images and all shot related data was downloaded using the package [NBAstatR](https://github.com/abresler/nbastatR) which allows users to pin the [stats.nba.com website](https://stats.nba.com/).

Video highlights of the games have been escraped from [Youtube](www.youtube.com)

## Final Comments
Data related to teams travel and player load is based on information publicly available from a variety of sources as stated above. It may not be an accurate representation of the way teams really managed their schedule.
