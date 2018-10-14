length(unique(data$Player_ID.x))


playerdt <- function(playername,datestart1,datestart2){
  
  playername <- "Federer, Roger"
  datestart1 <- as.POSIXct("2016-01-08")
  datestart2 <- as.POSIXct("2018-01-08")
  subdata = data[(data$Scheduled > datestart1 & data$Scheduled < datestart2),]
  player_matches <- subdata[subdata$Player_name.x == playername |subdata$Player_name.y == playername ,]
  pid <- ratings[ratings$Player_Name == playername,'player_id'][1]
  
  aces_1_df <- subdata[subdata$Player_name.x == playername,c('Aces.x','Scheduled')]
  aces_2_df <- subdata[subdata$Player_name.y == playername,c('Aces.y','Scheduled')]
  colnames(aces_1_df) <- c("Aces","Date")
  colnames(aces_2_df) <- c("Aces","Date")
  aces = rbind(aces_1_df,aces_2_df)
  
  aces$Date <- as.Date(aces$Date, "%m/%d/%Y")
  plot(Aces ~ Date, aces)
  axis(1, aces$Date, cex.axis = .7)
  
  
  bp_1 <- subdata[subdata$Player_name.x == playername,'Breakpoints_won.x']
  bp_2 <- subdata[subdata$Player_name.y == playername,'Breakpoints_won.y']
  total_bp = sum(c(bp_1,bp_2),na.rm = T)
  
  df <- data.frame(playername,player_country,current_points,current_rank,dim(player_matches)[1],total_aces,total_bp)
  colnames(df) <- c('Player Name','Player Country','Current Points','Current Ranking','Number of Matches','Total Aces','Total Breakpoints')
  return(df)
  
}



