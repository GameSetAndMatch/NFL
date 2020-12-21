Downs_Scraping <- function(Year, Week) {
  Url_NFL_Downs <-
    paste(
      "https://www.foxsports.com/nfl/team-stats?season=",
      Year,
      "&week=1",
      sprintf("%02d", Week),
      "&category=DOWNS",
      sep = ""
    )
  
  Teams_Data_Downs <-
    WebScraping(Url_NFL_Downs, ".wisbb_fullTeam span")[1:32 * 3]
  
  Data_Downs <-
    WebScraping(Url_NFL_Downs, ".wisbb_priorityColumn")[10:137]
  
  Downs_Data <- Data_Downs[seq(1, by = 4, length.out = 32)]
  
  Downs_DF <- cbind(Teams_Data_Downs, Downs_Data)
  Downs_DF[which(Downs_DF[, 1] == "WAS"), 1] <- "WSH"
  
  colnames(Downs_DF) <- c("Team_Abb", "FirstDowns")
  
  return(Downs_DF)
}


