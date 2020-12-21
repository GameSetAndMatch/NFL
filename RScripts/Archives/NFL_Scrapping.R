#library("gdata")
library("rvest")
library("plyr")
library("lubridate")
library("rpart")
library("rpart.plot")
library("abind")
# Every NFL Teams
Complete_Names <- c("Arizona_Cardinals", "Atlanta_Falcons", "Baltimore_Ravens", 
                    "Buffalo_Bills", "Carolina_Panthers", "Chicago_Bears", 
                    "Cincinnati_Bengals", "Cleveland_Browns", "Dallas_Cowboys", 
                    "Denver_Broncos", "Detroit_Lions", "GreenBay_Packers",
                    "Houston_Texans", "Indianapolis_Colts", "Jacksonville_Jaguars",
                    "KansasCity_Chiefs", "LosAngeles_Chargers", "LosAngeles_Rams", 
                    "Miami_Dolphins", "Minnesota_Vikings", "NewEngland_Patriots",
                    "NewOrleans_Saints", "NewYork_Giants", "NewYork_Jets",
                    "Oakland_Raiders", "Philadelphia_Eagles", "Pittsburgh_Steelers",
                    "SanFrancisco_49ers", "Seattle_Seahawks", "TampaBay_Buccaneers", 
                    "Tennessee_Titans", "Washington_Redskins")



City_Team_Sign <- cbind(c("Arizona","Atlanta","Baltimore","Buffalo",
                          "Carolina","Chicago","Cincinnati","Cleveland",
                          "Dallas","Denver","Detroit","Green Bay","Houston",
                          "Indianapolis","Jacksonville","Kansas City",
                          "Los Angeles","Los Angeles","Miami","Minnesota",
                          "New England","New Orleans","New York","New York",
                          "Oakland","Philadelphia","Pittsburgh","San Francisco",
                          "Seattle","Tampa Bay","Tennessee","Washington"),
                        
                        gsub(".*_", "", Complete_Names),
                        
                        c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL",
                                                           "DEN","DET","GB","HOU","IND","JAX","KC","LAC","LAR",
                                                           "MIA","MIN","NE","NO","NYG","NYJ","OAK","PHI","PIT",
                                                           "SF","SEA","TB","TEN","WSH"))

colnames(City_Team_Sign) <- c("City", "Team", "Team_Abb")

Year_Week_Stat <- function(Year, Week){
  
  #Specifying the url for desired website to be scraped
  
  Url_NFL_Offense <- paste("https://www.foxsports.com/nfl/team-stats?season=", Year, "&week=1", sprintf("%02d", Week),"&category=YARDAGE&opp=0&sort=0&qualified=1&sortOrder=0", sep= "")
  
  #Reading the HTML code from the website
  Webpage_NFL_Offense <- read_html(Url_NFL_Offense)
  
  #Using CSS selectors to scrape the rankings section
  Teams_Data_Html_Offense <- html_nodes(Webpage_NFL_Offense,".wisbb_fullTeam span")
  
  #Converting the ranking data to text
  Teams_Data_Offense <- html_text(Teams_Data_Html_Offense)[1:32 * 3]
  
  
  #Points, Yards
  Offense_Data_html <- html_nodes(Webpage_NFL_Offense,".wisbb_priorityColumn")
  Offense_Data <- html_text(Offense_Data_html)
  Offense_Data <- Offense_Data[9:136]
  Points_Data <- Offense_Data[seq(1, by = 4, length.out = 32) ]
  Yards_Data <- Offense_Data[seq(2, by = 4, length.out = 32) ]
  
  Offense_DF <- cbind(Teams_Data_Offense, Points_Data, Yards_Data)
  Offense_DF[which(Offense_DF[,1] == "WAS" ),1] <- "WSH"
  #Downs
  Url_NFL_Downs <- paste("https://www.foxsports.com/nfl/team-stats?season=", Year, "&week=1", sprintf("%02d", Week), "&category=DOWNS", sep = "")
  
  Webpage_NFL_Downs <- read_html(Url_NFL_Downs)
  
  Teams_Data_Html_Downs <- html_nodes(Webpage_NFL_Downs,".wisbb_fullTeam span")
  
  Teams_Data_Downs <- html_text(Teams_Data_Html_Downs)[1:32 * 3]
  
  Data_Html_Downs <- html_nodes(Webpage_NFL_Downs,".wisbb_priorityColumn")
  
  Data_Downs <-  html_text(Data_Html_Downs)
  Data_Downs <- Data_Downs[10:137]
  
  Downs_Data <- Data_Downs[seq(1, by = 4, length.out = 32) ]
  
  Downs_DF <- cbind(Teams_Data_Downs, Downs_Data)
  Downs_DF[which(Downs_DF[,1] == "WAS" ),1] <- "WSH"
  
  #Turnovers
  Url_NFL_Turnovers <- paste("https://www.foxsports.com/nfl/team-stats?season=", Year, "&week=1", sprintf("%02d", Week), "&category=TURNOVERS", sep = "")
  
  Webpage_NFL_Turnovers <- read_html(Url_NFL_Turnovers)
  
  Teams_Data_Html_Turnovers <- html_nodes(Webpage_NFL_Turnovers,".wisbb_fullTeam span")
  
  Teams_Data_Turnovers <- html_text(Teams_Data_Html_Turnovers)[1:32 *3]
  
  
  Data_Html_Turnovers <- html_nodes(Webpage_NFL_Turnovers,"td")
  
  Data_Turnovers <- html_text(Data_Html_Turnovers)
  
  Data_Int <- Data_Turnovers[c(0:31 * 6 + 3)]
  Data_Fum <- Data_Turnovers[c(0:31 * 6 + 4)]
  
  Turnovers_DF <- cbind(Teams_Data_Turnovers, Data_Int, Data_Fum)
  Turnovers_DF[which(Turnovers_DF[,1] == "WAS" ),1] <- "WSH"
  
  colnames(Offense_DF) <- c("Team_Abb", "Points", "Yards")
  colnames(Downs_DF) <- c("Team_Abb", "FirstDowns")
  colnames(Turnovers_DF) <- c("Team_Abb", "Int", "Fum")
  
  
  # Matchups
  #Specifying the url for desired website to be scraped
  Url_NFL_MatchUps <- ifelse(Year == 2019, paste("https://www.espn.com/nfl/schedule/_/week/", Week, sep = ""),
                             paste("https://www.espn.com/nfl/schedule/_/week/", Week, "/year/", Year , sep = ""))
  
  #Reading the HTML code from the website
  Webpage_NFL_MatchUps <- read_html(Url_NFL_MatchUps)
  
  #Using CSS selectors to scrape the rankings section
  Teams_Html_MatchUps <- html_nodes(Webpage_NFL_MatchUps,".home+ td a")
  
  #Converting the ranking data to text
  
  Teams_MatchUps <- cbind(html_text(Teams_Html_MatchUps), rep(NA, length(html_text(Teams_Html_MatchUps))), numeric(length(html_text(Teams_Html_MatchUps))))
  
  for (i in seq(nrow(Teams_MatchUps))){
    teamz <- strsplit(gsub("[^A-Z]"," ",Teams_MatchUps[i,1]), " ")[[1]][strsplit(gsub("[^A-Z]"," ",Teams_MatchUps[i,1]), " ")[[1]] != ""]
    Teams_MatchUps[i, 1:2] <- teamz[1:2]
    Teams_MatchUps[i, 3] <- !is.na(teamz[3])
    
  }
  
  Teams_MatchUps <- rbind(Teams_MatchUps, matrix(rep("BYE", 3 * (16 - nrow(Teams_MatchUps))), ncol =3))
  
  #unlist(strsplit(Teams_MatchUps[1], " "))[-length(unlist(strsplit(Teams_MatchUps[1], " ")))]
  
  MatchUp <- cbind(Complete_Names, City_Team_Sign[,"Team_Abb"], seq(32), rep("BYE", 32),rep("BYE", 32) ,numeric(32), rep("BYE", 32), rep("BYE", 32))
  
  
  
  for (i in seq(nrow(Teams_MatchUps))){
    
    Abbz <- Teams_MatchUps[i, ]
    
    MatchUp[which(Abbz[1] == City_Team_Sign[,"Team_Abb"] ), 4] <- Complete_Names[which(Abbz[2] == City_Team_Sign[,"Team_Abb"])]
    MatchUp[which(Abbz[2] == City_Team_Sign[,"Team_Abb"]), 4] <- Complete_Names[which(Abbz[1] == City_Team_Sign[,"Team_Abb"])]
    
    MatchUp[which(Abbz[1] == City_Team_Sign[,"Team_Abb"]), 5] <- Abbz[2]
    MatchUp[which(Abbz[2] == City_Team_Sign[,"Team_Abb"]), 5] <- Abbz[1]
    
    MatchUp[which(Abbz[1] == City_Team_Sign[,"Team_Abb"]), 6] <- which(Abbz[2] == City_Team_Sign[,"Team_Abb"])
    MatchUp[which(Abbz[2] == City_Team_Sign[,"Team_Abb"]), 6] <- which(Abbz[1] == City_Team_Sign[,"Team_Abb"])
    
    MatchUp[which(Abbz[1] == City_Team_Sign[,"Team_Abb"]), 7] <- "Away"
    MatchUp[which(Abbz[2] == City_Team_Sign[,"Team_Abb"]), 7] <- "Home"
    
    MatchUp[which(Abbz[1] == City_Team_Sign[,"Team_Abb"]), 8] <- ifelse(Abbz[3], "OT", "FINAL")
    MatchUp[which(Abbz[2] == City_Team_Sign[,"Team_Abb"]), 8] <- ifelse(Abbz[3], "OT", "FINAL")
    
  }
  
  colnames(MatchUp) <- c("Complete_Names", "Team_Abb", "Rang_Alpha",
                         "Complete_Names_VS", "Team_Abb_VS" ,"Rang_Alpha_VS",
                         "Terrain", "Situation")
  
  # MatchUp[which(MatchUp[,"Team_Abb"] == "WSH"), ] <- "WAS"
  
  
  BYE_TEAMS <- City_Team_Sign[, "Team_Abb"][which(MatchUp[, "Team_Abb_VS"] == "BYE")]
  
  if (length(BYE_TEAMS)){
    Offense_DF[31,] <- c(BYE_TEAMS[1], rep(NA, ncol(Offense_DF) -1))
    Offense_DF[32,] <- c(BYE_TEAMS[2], rep(NA, ncol(Offense_DF) -1)) 
    Downs_DF[31,] <- c(BYE_TEAMS[1], rep(NA, ncol(Downs_DF) -1))
    Downs_DF[32,] <- c(BYE_TEAMS[2], rep(NA, ncol(Downs_DF) -1))
    Turnovers_DF[31,] <- c(BYE_TEAMS[1], rep(NA, ncol(Turnovers_DF) -1))
    Turnovers_DF[32,] <- c(BYE_TEAMS[2], rep(NA, ncol(Turnovers_DF) -1))
  }
  
  
  Weekly_stat <- join_all(list(data.frame(MatchUp), data.frame(Offense_DF),
                               data.frame(Downs_DF), data.frame(Turnovers_DF)), 
                          by = 'Team_Abb', type = 'full')[seq(32),]
  
  
  
  
  Complete_Names <- as.character(levels(Weekly_stat$Complete_Names))[Weekly_stat$Complete_Names]
  Team_Abb <- as.character(levels(Weekly_stat$Team_Abb))[Weekly_stat$Team_Abb]
  Rang_Alpha <- as.numeric(levels(Weekly_stat$Rang_Alpha))[Weekly_stat$Rang_Alpha]
  Complete_Names_VS <- as.character(levels(Weekly_stat$Complete_Names_VS))[Weekly_stat$Complete_Names_VS]
  Team_Abb_VS <- as.character(levels(Weekly_stat$Team_Abb_VS))[Weekly_stat$Team_Abb_VS]
  Rang_Alpha_VS <- as.numeric(levels(Weekly_stat$Rang_Alpha_VS))[Weekly_stat$Rang_Alpha_VS]
  Terrain <- as.character(levels(Weekly_stat$Terrain))[Weekly_stat$Terrain]
  Situation <- as.character(levels(Weekly_stat$Situation))[Weekly_stat$Situation]
  Points <- as.numeric(levels(Weekly_stat$Points))[Weekly_stat$Points]
  Yards <- as.numeric(levels(Weekly_stat$Yards))[Weekly_stat$Yards]
  FirstDowns <- as.numeric(levels(Weekly_stat$FirstDown))[Weekly_stat$FirstDown]
  Int <- as.numeric(levels(Weekly_stat$Int))[Weekly_stat$Int]
  Fum <- as.numeric(levels(Weekly_stat$Fum))[Weekly_stat$Fum]
  Points_VS   <- as.numeric(levels(Weekly_stat$Points))[Weekly_stat$Points][as.numeric(levels(Weekly_stat$Rang_Alpha_VS))[Weekly_stat$Rang_Alpha_VS]]
  Yards_VS    <- as.numeric(levels(Weekly_stat$Yards))[Weekly_stat$Yards][as.numeric(levels(Weekly_stat$Rang_Alpha_VS))[Weekly_stat$Rang_Alpha_VS]]
  FirstDown_VS<- as.numeric(levels(Weekly_stat$FirstDown))[Weekly_stat$FirstDown][as.numeric(levels(Weekly_stat$Rang_Alpha_VS))[Weekly_stat$Rang_Alpha_VS]]
  Int_VS      <- as.numeric(levels(Weekly_stat$Int))[Weekly_stat$Int][as.numeric(levels(Weekly_stat$Rang_Alpha_VS))[Weekly_stat$Rang_Alpha_VS]]
  Fum_VS      <- as.numeric(levels(Weekly_stat$Fum))[Weekly_stat$Fum][as.numeric(levels(Weekly_stat$Rang_Alpha_VS))[Weekly_stat$Rang_Alpha_VS]]
  
  
  for (rankz in which(MatchUp[, "Team_Abb_VS"] == "BYE")){
    Points_VS <- append(Points_VS, NA, (rankz-1))
    Yards_VS <- append(Yards_VS, NA, (rankz-1))
    FirstDown_VS <- append(FirstDown_VS, NA, (rankz-1))
    Int_VS <- append(Int_VS, NA, (rankz-1))
    Fum_VS <- append(Fum_VS, NA, (rankz-1))
  }
  Outcome     <- ifelse(as.numeric(levels(Weekly_stat$Points))[Weekly_stat$Points] > Points_VS, "WIN", "LOSS")
  
  Weekly_statz <- cbind(Complete_Names,Team_Abb,Rang_Alpha,
                        Complete_Names_VS,Team_Abb_VS,Rang_Alpha_VS,
                        Terrain,Situation, Outcome,Points,Yards,FirstDowns,Int,Fum, 
                        Points_VS, Yards_VS, FirstDown_VS, Int_VS, Fum_VS)
  return(Weekly_statz)
  } 

Stat_UTD <- function(Save_RDS = F){
  Year <- year(Sys.Date())
  Week <- min(max(1, (week(Sys.Date())- 36)), 16)  
  Years <- 2018:Year
  Nb_Seasons <- length(Years)
  
  Stats_Years <- character(Nb_Seasons)
  for (i in seq(Nb_Seasons)){
    Stats_Years[i] <- paste("Stats_", Years[i], sep = "")
  }
  
  Current_Year <- Stats_Years[length(Stats_Years)]
  
  for(i in seq(Nb_Seasons)){
    if (Stats_Years[i] != Current_Year){
      assign(Stats_Years[i], do.call("abind", c(sapply(seq(16), 
                                          function(weekz) Year_Week_Stat(Years[i], weekz), simplify = F), list(along=3)))) 
      
    }else{
      assign(Stats_Years[i], do.call("abind", c(sapply(seq(Week), 
                                                       function(weekz) Year_Week_Stat(Years[i], weekz), simplify = F), list(along=3)))) 
    }
  }
  
  assign(paste(Stats_Years[length(Stats_Years)], "_Mod", sep = ""), abind(eval(parse(text = Stats_Years[length(Stats_Years)])),array(NA,c(32,19,(16 - Week))),along = 3))
  
  Stats_UTD <- do.call("abind",c(list( eval(parse(text = Stats_Years[-length(Stats_Years)])),
                          eval(parse(text = paste(Stats_Years[length(Stats_Years)], "_Mod", sep = "")),list(along=4)))))
  
  dim(Stats_UTD) <- c(32, 19, 16, length(Years))
  
  if(Save_RDS){
    saveRDS(Data_2, paste("NFL_Stats", as.character(Year), "_Week_", as.character(Week), sep = "" ))
  }
  
  return(Stats_UTD)
}

Data_2 <- Stat_UTD()

GB <- t(array_2019[12, 9:18, 1:12][,-11])
GB <-data.frame(apply(GB, 2, as.numeric))
var <- c( "Yards", "FirstDowns", "Int", "Fum", "Points_VS", "Yards_VS", "Firstdowns_VS", "Int_VS", "Fum_VS")
colnames(GB) <-  c("Points", var)


k_fold <- numeric(11)
for(testing in 1:11){
set.seed(0)
m <- 200
p <- 0.8

#testing <- 3
training <- seq(11)[-testing]

rez <- as.numeric(GB$Points[training])

for(i in seq(m)){
  vecbin <- sample(seq(length(var)),rbinom(1, length(var), p))
  temp_matrix <- GB[training, vecbin]
  GB_rpart_temp <- rpart(rez ~., data = temp_matrix,      control = rpart.control(minbucket = 3))
  assign(paste("GB_rpart_temp", i, sep = ""), GB_rpart_temp)
  rez <- rez - predict(GB_rpart_temp)
}

rez_fin <- 0
new_data <- GB[testing,]
rez_suivi <- as.numeric(m)
for (i in seq(m)){
  rez_fin = rez_fin + predict(eval(parse(text = paste("GB_rpart_temp", i, sep = ""))), newdata = new_data)
  rez_suivi[i] <- rez_fin
}
print(c(mean(GB$Points),sd(GB$Points)))
print(c(GB$Points[testing],rez_fin))
plot(rez_suivi)
k_fold[testing] <- rez_fin
}
point_max <- max(GB$Points,k_fold)
plot(GB$Points,round(k_fold),xlim = c(0,point_max),ylim = c(0,point_max))
lines(c(0,100),c(0,100))
lines(c(0,100),c(0,100)-3,col = "blue");lines(c(0,100),c(0,100)+3,col = "blue")
lines(c(0,100),c(0,100)-8,col = "red");lines(c(0,100),c(0,100)+8,col = "red")


