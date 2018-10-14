data = read.csv("tennis_final.csv")
atp_rating <- read.csv("atp_rating.csv")
wta_rating <- read.csv("wta_rating.csv")
player_data <- read.csv("player_data.csv")
permatch_data <- read.csv("Final_PlayerDataPerMatch.csv")
ratings <- rbind(atp_rating,wta_rating)
ratings$player_id <- as.character(ratings$player_id)
data$Player_ID.x <- as.character(data$Player_ID.x)
data$Player_ID.y <- as.character(data$Player_ID.y)
data$Winner_ID <- as.character(data$Winner_ID)
data$Winner_ID = as.character(gsub("\\n", "", data$Winner_ID))
data$Scheduled <- as.Date(data$Scheduled)
datestart1 <- "2011-01-01"
datestart2 <- "2019-01-01"
playername <- "Federer, Roger"

playerdt <- function(playername,datestart1,datestart2){
datestart1 <- as.Date(datestart1)
datestart2 <- as.Date(datestart2)
subdata = data[(data$Scheduled > datestart1 & data$Scheduled < datestart2),]
player_matches <- data[subdata$Player_name.x == playername |subdata$Player_name.y == playername ,]
pid <- ratings[ratings$Player_Name == playername,'player_id'][1]
player_country <- ratings[ratings$player_id == pid,'Nationality_Country_Full']
current_points <- ratings[ratings$player_id == pid,'Rating_Points']
current_rank <- ratings[ratings$player_id == pid,'Rating_rank']
aces_1 <- subdata[subdata$Player_name.x == playername,'Aces.x']
aces_2 <- subdata[subdata$Player_name.y == playername,'Aces.y']
games_won = sum(player_matches$Winner_ID == pid)
total_aces = sum(c(aces_1,aces_2),na.rm = T)
bp_1 <- subdata[subdata$Player_name.x == playername,'Breakpoints_won.x']
bp_2 <- subdata[subdata$Player_name.y == playername,'Breakpoints_won.y']
total_bp = sum(c(bp_1,bp_2),na.rm = T)
df <- data.frame(playername,player_country,current_points,current_rank,dim(player_matches)[1],games_won,round((games_won/dim(player_matches)[1])*100), total_aces,total_bp)
colnames(df) <- c('Player Name','Player Country','Current Points','Current Ranking','Number of Matches','Matches Won','Winning Percentage','Total Aces','Total Breakpoints')
return(df)
  
}

getAveragedData <- function(player_data,gender,date1,date2){
  player_data$Scheduled <- as.Date(player_data$Scheduled)
  date1 <- as.Date(date1)
  date2 <- as.Date(date2)
  subdata = player_data[(player_data$Scheduled > date1 & player_data$Scheduled < date2),]
  if(gender=='ATP'){
    subdata <- subdata[subdata$Tournament_Gender=='men',]
  }
  else{
    subdata <- subdata[subdata$Tournament_Gender=='women',]
  }
  aggregated <- aggregate(cbind(Aces.x, Breakpoints_won.x, double_faults.x, first_serve_points_won.x,
                                first_serve_successful.x, Games_won.x, Max_games_won_in_a_row.x,
                                Max_points_in_a_row.x, Points_won.x, Reciever_points_won.x,
                                second_serve_points_won.x, second_serve_successful.x,
                                service_games_won.x, service_points_won.x, tiebreaks_won.x)~Player_ID.x+Player_name.x,data = subdata,FUN = function(x){ round(mean(x))})
  rm(subdata) 
  return(aggregated)
  
}

getplayerinfo <- function(Player_Data, player_name){
  
  return(Return_stats <- Player_Data[Player_Data$Player.Name == player_name,])
}

pred_gamespermatch <- function(Aces,Breakpoints_won,double_faults,first_serve_points_won,first_serve_successful,
                               Reciever_points_won,second_serve_successful,service_games_won,tiebreaks_won)
{
  pred_val <- -0.4174 + 1.3626*Aces+ 3.9073*Breakpoints_won - 0.2096*double_faults - 2.0816*first_serve_points_won+
    5.1657*first_serve_successful + 2.1799*Reciever_points_won + 0.6149*second_serve_successful+
    8.7373*service_games_won + 0.8727*tiebreaks_won
  return(round(pred_val,2))
}  

x1 <- getplayerinfo(permatch_data,'Federer, Roger')
x1

