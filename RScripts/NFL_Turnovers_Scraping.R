Turnovers_Scraping <- function(Year, Week) {
  Url_NFL_Turnovers <-
    paste(
      "https://www.foxsports.com/nfl/team-stats?season=",
      Year,
      "&week=1",
      sprintf("%02d", Week),
      "&category=TURNOVERS",
      sep = ""
    )
  
  Teams_Data_Turnovers <-
    WebScraping(Url_NFL_Turnovers, ".wisbb_fullTeam span")[1:32 * 3]
  
  Data_Int <- WebScraping(Url_NFL_Turnovers, "td")[c(0:31 * 6 + 3)]
  Data_Fum <- WebScraping(Url_NFL_Turnovers, "td")[c(0:31 * 6 + 4)]
  
  Turnovers_DF <- cbind(Teams_Data_Turnovers, Data_Int, Data_Fum)
  Turnovers_DF[which(Turnovers_DF[, 1] == "WAS"), 1] <- "WSH"
  
  colnames(Turnovers_DF) <- c("Team_Abb", "Int", "Fum")
  
  return(Turnovers_DF)
}