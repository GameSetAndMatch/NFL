Offense_Scraping <- function(Year, Week) {
  Url_NFL_Offense <-
    paste(
      "https://www.foxsports.com/nfl/team-stats?season=",
      Year,
      "&week=1",
      sprintf("%02d", Week),
      "&category=YARDAGE&opp=0&sort=0&qualified=1&sortOrder=0",
      sep = ""
    )
  
  Teams_Data_Offense <-
    WebScraping(Url_NFL_Offense, ".wisbb_fullTeam span")[1:32 * 3]
  
  #Points, Yards
  
  Offense_Data <-
    WebScraping(Url_NFL_Offense, ".wisbb_priorityColumn")[9:136]
  Points_Data <- Offense_Data[seq(1, by = 4, length.out = 32)]
  Yards_Data <- Offense_Data[seq(2, by = 4, length.out = 32)]
  
  Offense_DF <- cbind(Teams_Data_Offense, Points_Data, Yards_Data)
  Offense_DF[which(Offense_DF[, 1] == "WAS"), 1] <- "WSH"
  
  colnames(Offense_DF) <- c("Team_Abb", "Points", "Yards")
  
  return(Offense_DF)
}