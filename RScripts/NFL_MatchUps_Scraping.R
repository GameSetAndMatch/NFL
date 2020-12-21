MatchUps_Scraping <- function(Year, Week) {
  Url_NFL_MatchUps <-
    ifelse(Year == year(Sys.Date()),
           paste("https://www.espn.com/nfl/schedule/_/week/", Week, sep = ""), #Same Year
           paste("https://www.espn.com/nfl/schedule/_/week/", Week, "/year/", Year ,"/seasontype/2", sep = ""))
  
  #Homefield
  Teams_MatchUps_Vect <- WebScraping(Url_NFL_MatchUps, ".home+ td a")
  
  #Matchups, Situation
  Teams_tot <- WebScraping(Url_NFL_MatchUps, ".team-name span")[seq(2*length(Teams_MatchUps_Vect))]
  
  Teams_MatchUps <-
    cbind(Teams_MatchUps_Vect, rep(NA, length(Teams_MatchUps_Vect)), numeric(length(Teams_MatchUps_Vect)))
  
  
  
  for (i in seq(nrow(Teams_MatchUps))) {
    teamz <-
      strsplit(gsub("[^A-Z]", " ", Teams_MatchUps[i, 1]), " ")[[1]][strsplit(gsub("[^A-Z]", " ", Teams_MatchUps[i, 1]), " ")[[1]] != ""]
    Teams_MatchUps[i, 1:2] <- teamz[1:2]
    Teams_MatchUps[i, 3] <- !is.na(teamz[3])
  }
  
  Teams_MatchUps <-
    rbind(Teams_MatchUps, matrix(rep("BYE", 3 * (
      16 - nrow(Teams_MatchUps)
    )), ncol = 3))
  
#   return(list(Teams_MatchUps,Teams_tot))
# }

# Info_VS <- function(Year, Week) {
  # Teams_MatchUps <- MatchUps_Scraping(Year, Week)[[1]]
  #Teams_tot <- MatchUps_Scraping(Year, Week)[[2]]
  MatchUp <-
    cbind(Complete_Names,
          City_Team_Sign[, "Team_Abb"],
          seq(32),
          rep("BYE", 32),
          rep("BYE", 32) ,
          numeric(32),
          rep("BYE", 32),
          rep(NA, 32))
  
  
  for (i in seq(16)) {
    Abbz <- Teams_MatchUps[i,]
    
    MatchUp[which(Abbz[1] == City_Team_Sign[, "Team_Abb"]), 4] <-
      Complete_Names[which(Abbz[2] == City_Team_Sign[, "Team_Abb"])]
    
    MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 4] <-
      Complete_Names[which(Abbz[1] == City_Team_Sign[, "Team_Abb"])]
    
    MatchUp[which(Abbz[1] == City_Team_Sign[, "Team_Abb"]), 5] <-
      Abbz[2]
    MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 5] <-
      Abbz[1]
    
    MatchUp[which(Abbz[1] == City_Team_Sign[, "Team_Abb"]), 6] <-
      which(Abbz[2] == City_Team_Sign[, "Team_Abb"])
    MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 6] <-
      which(Abbz[1] == City_Team_Sign[, "Team_Abb"])
    
    MatchUp[which(Abbz[1] == City_Team_Sign[, "Team_Abb"]), 7] <-
      ifelse(Abbz[3], "OT", "FINAL")
    MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 7] <-
      ifelse(Abbz[3], "OT", "FINAL")
    
    # Two New York and Two Los Angeles
    if(sum(grepl("NY" , Abbz[seq(2)])) ==2){
      MatchUp[which(Abbz[1] == City_Team_Sign[, "Team_Abb"]), 8] <-
        1
      MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 8] <-
        1
    }else if (sum(grepl("LA" , Abbz[seq(2)])) ==2) {
      MatchUp[which(Abbz[1] == City_Team_Sign[, "Team_Abb"]), 8] <-
        1
      MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 8] <-
        1
    }else{
      MatchUp[which(Abbz[1] == City_Team_Sign[, "Team_Abb"]), 8] <-
        (which(City_Team_Sign[which(Abbz[1] == City_Team_Sign[,3 ]),1] == Teams_tot[((2 * i -1):(2 * i) )])-1) %%2
      MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 8] <-
        (which(City_Team_Sign[which(Abbz[2] == City_Team_Sign[,3 ]),1] == Teams_tot[((2 * i -1):(2 * i) )])-1) %%2
    }
  }
  
  # Teams_MatchUps[round((which(City_Team_Sign[which(City_Team_Sign[,3] ==  Abbz[2]),1] == Teams_tot))/2)[round((which(City_Team_Sign[which(City_Team_Sign[,3] ==  Abbz[2]),1] == Teams_tot))/2) == i ],]
  
  Var_Info <- c(
    "Complete_Names",
    "Team_Abb",
    "Rang_Alpha",
    "Complete_Names_VS",
    "Team_Abb_VS" ,
    "Rang_Alpha_VS",
    "Situation",
    "HomeField")
  
  colnames(MatchUp) <- Var_Info
  
  return(MatchUp)
}
