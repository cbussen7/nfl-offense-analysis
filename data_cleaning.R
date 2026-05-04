library(pacman)
p_load(tidyverse)

df <- read_csv("/Users/christopherbussen/Documents/School/UDS2024/MTH208/final/team_stats_2003_2023.csv")

# fill NA ties with 0 - could also just get rid of it
df$ties[is.na(df$ties)] <- 0


attendance <- read_csv("/Users/christopherbussen/Documents/School/UDS2024/MTH208/final/attendance.csv")
attendance <- attendance %>% 
   rename(
      tm = team
   )

attendance$team <- paste(attendance$tm, attendance$team_name)

for (i in 1:nrow(df)) {
   curr <- df[i,]
   if (curr$team == "Arizona Cardinals"){
      df[i,]$location <- "Glendale, Arizona"
   } else if (curr$team == "Atlanta Falcons"){
      df[i,]$location <- "Atlanta, Georgia"
   } else if (curr$team == "Baltimore Ravens"){
      df[i,]$location <- "Baltimore, Maryland"
   } else if (curr$team == "Buffalo Bills"){
      df[i,]$location <- "Orchard Park, New York"
   } else if (curr$team == "Carolina Panthers"){
      df[i,]$location <- "Charlotte, North Carolina"
   } else if (curr$team == "Chicago Bears"){
      df[i,]$location <- "Chicago, Illinois"
   } else if (curr$team == "Cincinnati Bengals"){
      df[i,]$location <- "Cincinnati, Ohio"
   } else if (curr$team == "Cleveland Browns"){
      df[i,]$location <- "Cleveland, Ohio"
   } else if (curr$team == "Dallas Cowboys"){
      df[i,]$location <- "Arlington, Texas"
   } else if (curr$team == "Denver Broncos"){
      df[i,]$location <- "Denver, Colorado"
   } else if (curr$team == "Detroit Lions"){
      df[i,]$location <- "Detroit, Michigan"
   } else if (curr$team == "Green Bay Packers"){
      df[i,]$location <- "Green Bay, Wisconsin"
   } else if (curr$team == "Houston Texans"){
      df[i,]$location <- "Houston, Texas"
   } else if (curr$team == "Indianapolis Colts"){
      df[i,]$location <- "Indianapolis, Indiana"
   } else if (curr$team == "Jacksonville Jaguars"){
      df[i,]$location <- "Jacksonville, Florida"
   } else if (curr$team == "Kansas City Chiefs"){
      df[i,]$location <- "Kansas City, Missouri"
   } else if (curr$team == "Las Vegas Raiders"){
      df[i,]$location <- "Las Vegas, Nevada"
   } else if (curr$team == "Los Angeles Chargers"){
      df[i,]$location <- "Inglewood, California"
   } else if (curr$team == "Los Angeles Rams"){
      df[i,]$location <- "Inglewood, California"
   } else if (curr$team == "Miami Dolphins"){
      df[i,]$location <- "Miami, Florida"
   } else if (curr$team == "Minnesota Vikings"){
      df[i,]$location <- "Minneapolis, Minnesota"
   } else if (curr$team == "New England Patriots"){
      df[i,]$location <- "Foxborough, Massachusetts"
   } else if (curr$team == "New Orleans Saints"){
      df[i,]$location <- "New Orleans, Louisiana"
   } else if (curr$team == "New York Giants"){
      df[i,]$location <- "East Rutherford, New Jersey"
   } else if (curr$team == "New York Jets"){
      df[i,]$location <- "East Rutherford, New Jersey"
   } else if (curr$team == "Philadelphia Eagles"){
      df[i,]$location <- "Philadelphia, Pennsylvania"
   } else if (curr$team == "Pittsburgh Steelers"){
      df[i,]$location <- "Pittsburgh, Pennsylvania"
   } else if (curr$team == "San Francisco 49ers"){
      df[i,]$location <- "Santa Clara, California"
   } else if (curr$team == "Seattle Seahawks"){
      df[i,]$location <- "Seattle, Washington"
   } else if (curr$team == "Tampa Bay Buccaneers"){
      df[i,]$location <- "Tampa, Florida"
   } else if (curr$team == "Tennessee Titans"){
      df[i,]$location <- "Nashville, Tennessee"
   } else if (curr$team == "Washington Commanders"){
      df[i,]$location <- "Landover, Maryland"
   } else if (curr$team == "Oakland Raiders"){
      df[i,]$location <- "Oakland, California"
   } else if (curr$team == "San Diego Chargers"){
      df[i,]$location <- "San Diego, California"
   } else if (curr$team == "St. Louis Rams"){
      df[i,]$location <- "St. Louis, Missouri"
   }
   
}

avg_attendance <- attendance %>% 
   group_by(team, year) %>% 
   summarize(avg_attendance = mean(weekly_attendance, na.rm = TRUE))

merged_w_attendance <- left_join(df, avg_attendance, by = c("team", "year"))

write.csv(merged_w_attendance, "nfl.csv", row.names = TRUE)

nfl <- read_csv("/Users/christopherbussen/Documents/School/UDS2024/MTH208/final/nfl.csv")
# then had to go in and add commanders attendance since the different team name messed it up
# also used prep to split city and state to work with Tableau map
# filled na tie values with 0

