#library("gdata")
library("rvest")
library("plyr")
library("lubridate")
library("rpart")
library("rpart.plot")
library("abind")

WebScraping <- function(URL, CSS) {
  #read_html  -- Reading the HTML code from the website
  #html_nodes -- Using CSS selectors to scrape the rankings section
  #html_text  -- Converting the ranking data to text
  html_text(html_nodes(read_html(URL), CSS))
}

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
#Downs
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


#Turnovers
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

# Matchups
#Specifying the url for desired website to be scraped

MatchUps_Scraping <- function(Year, Week) {
  Url_NFL_MatchUps <-
    ifelse(
      Year == year(Sys.Date()),
      paste("https://www.espn.com/nfl/schedule/_/week/", Week, sep = ""),
      paste(
        "https://www.espn.com/nfl/schedule/_/week/",
        Week,
        "/year/",
        Year ,
        sep = ""
      )
    )
  
  Teams_MatchUps_Vect <-
    WebScraping(Url_NFL_MatchUps, ".home+ td a")
  
  #Converting the ranking data to text
  
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
  
  return(Teams_MatchUps)
}

Info_VS <- function(Year, Week) {
  Teams_MatchUps <- MatchUps_Scraping(Year, Week)
  
  MatchUp <-
    cbind(
      Complete_Names,
      City_Team_Sign[, "Team_Abb"],
      seq(32),
      rep("BYE", 32),
      rep("BYE", 32) ,
      numeric(32),
      rep("BYE", 32),
      rep("BYE", 32)
    )
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
      "Away"
    MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 7] <-
      "Home"
    
    MatchUp[which(Abbz[1] == City_Team_Sign[, "Team_Abb"]), 8] <-
      ifelse(Abbz[3], "OT", "FINAL")
    MatchUp[which(Abbz[2] == City_Team_Sign[, "Team_Abb"]), 8] <-
      ifelse(Abbz[3], "OT", "FINAL")
    
  }
  
  colnames(MatchUp) <- c(
    "Complete_Names",
    "Team_Abb",
    "Rang_Alpha",
    "Complete_Names_VS",
    "Team_Abb_VS" ,
    "Rang_Alpha_VS",
    "Terrain",
    "Situation"
  )
  
  return(MatchUp)
}
# MatchUp[which(MatchUp[,"Team_Abb"] == "WSH"), ] <- "WAS"

Weekly_stats <- function(Year, Week) {
  Offense_DF <- Offense_Scraping(Year, Week)
  Downs_DF <- Downs_Scraping(Year, Week)
  Turnovers_DF <- Turnovers_Scraping(Year, Week)
  MatchUp <- Info_VS(Year, Week)
  BYE_TEAMS <-
    City_Team_Sign[, "Team_Abb"][which(MatchUp[, "Team_Abb_VS"] == "BYE")]
  
  if (length(BYE_TEAMS)) {
    for (i in seq(length(BYE_TEAMS))) {
      Offense_DF[(32 - i), ] <-
        c(BYE_TEAMS[i], rep(NA, ncol(Offense_DF) - 1))
      Downs_DF[(32 - i), ] <-
        c(BYE_TEAMS[i], rep(NA, ncol(Downs_DF) - 1))
      Turnovers_DF[(32 - i), ] <-
        c(BYE_TEAMS[i], rep(NA, ncol(Turnovers_DF) - 1))
    }
  }
  
  Weekly_statz <-
    join_all(
      list(
        data.frame(MatchUp),
        data.frame(Offense_DF),
        data.frame(Downs_DF),
        data.frame(Turnovers_DF)
      ),
      by = 'Team_Abb',
      type = 'full'
    )[seq(32), ]
  
  
  
  
  Complete_Names <-
    as.character(levels(Weekly_statz$Complete_Names))[Weekly_statz$Complete_Names]
  Team_Abb <-
    as.character(levels(Weekly_statz$Team_Abb))[Weekly_statz$Team_Abb]
  Rang_Alpha <-
    as.numeric(levels(Weekly_statz$Rang_Alpha))[Weekly_statz$Rang_Alpha]
  Complete_Names_VS <-
    as.character(levels(Weekly_statz$Complete_Names_VS))[Weekly_statz$Complete_Names_VS]
  Team_Abb_VS <-
    as.character(levels(Weekly_statz$Team_Abb_VS))[Weekly_statz$Team_Abb_VS]
  Rang_Alpha_VS <-
    as.numeric(levels(Weekly_statz$Rang_Alpha_VS))[Weekly_statz$Rang_Alpha_VS]
  Terrain <-
    as.character(levels(Weekly_statz$Terrain))[Weekly_statz$Terrain]
  Situation <-
    as.character(levels(Weekly_statz$Situation))[Weekly_statz$Situation]
  Points <-
    as.numeric(levels(Weekly_statz$Points))[Weekly_statz$Points]
  Yards <-
    as.numeric(levels(Weekly_statz$Yards))[Weekly_statz$Yards]
  FirstDowns <-
    as.numeric(levels(Weekly_statz$FirstDown))[Weekly_statz$FirstDown]
  Int <- as.numeric(levels(Weekly_statz$Int))[Weekly_statz$Int]
  Fum <- as.numeric(levels(Weekly_statz$Fum))[Weekly_statz$Fum]
  Points_VS   <-
    as.numeric(levels(Weekly_statz$Points))[Weekly_statz$Points][as.numeric(levels(Weekly_statz$Rang_Alpha_VS))[Weekly_statz$Rang_Alpha_VS]]
  Yards_VS    <-
    as.numeric(levels(Weekly_statz$Yards))[Weekly_statz$Yards][as.numeric(levels(Weekly_statz$Rang_Alpha_VS))[Weekly_statz$Rang_Alpha_VS]]
  FirstDown_VS <-
    as.numeric(levels(Weekly_statz$FirstDown))[Weekly_statz$FirstDown][as.numeric(levels(Weekly_statz$Rang_Alpha_VS))[Weekly_statz$Rang_Alpha_VS]]
  Int_VS      <-
    as.numeric(levels(Weekly_statz$Int))[Weekly_statz$Int][as.numeric(levels(Weekly_statz$Rang_Alpha_VS))[Weekly_statz$Rang_Alpha_VS]]
  Fum_VS      <-
    as.numeric(levels(Weekly_statz$Fum))[Weekly_statz$Fum][as.numeric(levels(Weekly_statz$Rang_Alpha_VS))[Weekly_statz$Rang_Alpha_VS]]
  
  
  for (rankz in which(MatchUp[, "Team_Abb_VS"] == "BYE")) {
    Points_VS <- append(Points_VS, NA, (rankz - 1))
    Yards_VS <- append(Yards_VS, NA, (rankz - 1))
    FirstDown_VS <- append(FirstDown_VS, NA, (rankz - 1))
    Int_VS <- append(Int_VS, NA, (rankz - 1))
    Fum_VS <- append(Fum_VS, NA, (rankz - 1))
  }
  Outcome     <-
    ifelse(as.numeric(levels(Weekly_statz$Points))[Weekly_statz$Points] > Points_VS, "WIN", "LOSS")
  
  Weekly_stat <- cbind(Complete_Names,Team_Abb,Rang_Alpha,Complete_Names_VS,
                       Team_Abb_VS,Rang_Alpha_VS,Terrain,Situation,Outcome,
                       Points,Yards,FirstDowns,Int,Fum,Points_VS,Yards_VS,
                       FirstDown_VS,Int_VS,Fum_VS
  )
  return(Weekly_stat)
}

Stat_UTD <- function(Save_RDS = T) {
  Year <- year(Sys.Date())
  Week <<- min(max(1, (week(Sys.Date(
  )) - 36)), 17)
  Years <<- 2018:Year
  Nb_Seasons <- length(Years)
  
  Stats_Years <- character(Nb_Seasons)
  for (i in seq(Nb_Seasons)) {
    Stats_Years[i] <- paste("Stats_", Years[i], sep = "")
  }
  
  Current_Year <- Stats_Years[length(Stats_Years)]
  
  setwd("Data")
  if (file.exists(paste(
    "NFL_Stats",
    as.character(Year),
    "_Week_",
    as.character(Week),
    ".RDS",
    sep = ""
  ))) {
    Stats_UTD <-
      readRDS(paste(
        "NFL_Stats",
        as.character(Year),
        "_Week_",
        as.character(Week),
        ".RDS",
        sep = ""
      ))
    setwd(path_base)
  } else{
    for (i in seq(Nb_Seasons)) {
      if (Stats_Years[i] != Current_Year) {
        assign(Stats_Years[i], do.call("abind", c(
          sapply(seq(17),
                 function(weekz)
                   Weekly_stats(Years[i], weekz), simplify = F),
          list(along = 3)
        )))
        
      } else{
        assign(Stats_Years[i], do.call("abind", c(
          sapply(seq(Week),
                 function(weekz)
                   Weekly_stats(Years[i], weekz), simplify = F),
          list(along = 3)
        )))
      }
    }
    
    assign(paste(Stats_Years[length(Stats_Years)], "_Mod", sep = ""),
           abind(eval(parse(text = Stats_Years[length(Stats_Years)])), array(NA, c(
             32, 19, (17 - Week)
           )), along = 3))
    
    Stats_UTD <-
      do.call("abind", c(list(eval(
        parse(text = Stats_Years[-length(Stats_Years)])
      ),
      eval(
        parse(text = paste(Stats_Years[length(Stats_Years)], "_Mod", sep = "")), list(along =
                                                                                        4)
      ))))
    
    dim(Stats_UTD) <- c(32, 19, 17, length(Years))
    
    if (Save_RDS) {
      saveRDS(
        Stats_UTD,
        paste(
          "NFL_Stats",
          as.character(Year),
          "_Week_",
          as.character(Week),
          ".RDS",
          sep = ""
        )
      )
      setwd(path_base)
    }
    
  }
  return(Stats_UTD)
}