#Webcraping 
source("NFL_Webscraping.R")
# Matchups
source("NFL_MatchUps_Scraping.R")
#Offense
source("NFL_Offense_Scraping.R")
#Downs
source("NFL_Downs_Scraping.R")
#Turnovers
source("NFL_Turnovers_Scraping.R")



# Except MatchUps_Scraping, Require a dataframe with the team abbreviation and the data needed
Weekly_stats <- function(Year, Week,
                         Data = list(MatchUps_Scraping, Offense_Scraping, 
                                     Downs_Scraping, Turnovers_Scraping)) {

    join_statz <- join_all(lapply(seq(length(Data)), function(i) data.table(Data[[i]](Year, Week))),
                           by = 'Team_Abb',
                           type = 'left')
    
    VS_Statz <- join_statz[,.(Rang_Alpha_VS, Points, Yards, FirstDowns, Int, Fum)]
    
    colnames(VS_Statz) <- c("Rang_Alpha", 
                            sprintf("%s_VS",tail(colnames(join_statz), ncol(join_statz) - which(colnames(join_statz) == "HomeField"))))
    
    join_statz_tot <- join_all(list(join_statz, VS_Statz),
                               by = 'Rang_Alpha',
                               type = 'left', match = "first")

    # FOR LEVELS
    # Outcome <- data.table("Outcome" = unlist(sapply(as.character(sign(as.numeric(levels(join_statz_tot[["Points"]]))[join_statz_tot[["Points"]]] - as.numeric(levels(join_statz_tot[["Points_VS"]]))[join_statz_tot[["Points_VS"]]])), function(score) switch(score,
    #         "-1" = "LOSS",
    #         "0" = "TIE",
    #         "1" = "WIN",
    #         "NA" = "NA"), simplify = F)))
    
    Outcome <- data.table("Outcome" = unlist(sapply(as.character(sign(as.numeric(join_statz_tot[["Points"]]) - as.numeric(join_statz_tot[["Points_VS"]]))), function(score) switch(score,
            "-1" = "LOSS",
            "0" = "TIE",
            "1" = "WIN",
            "NA" = "NA"), simplify = F)))
    
    
    Weekly_stat <- cbind(join_statz_tot, Outcome)
    
    neworder <<- c("Complete_Names",
    "Team_Abb",
    "Rang_Alpha",
    "Complete_Names_VS",
    "Team_Abb_VS",
    "Rang_Alpha_VS",
    "Situation",
    "HomeField",
    "Outcome",
    "Points",
    "Yards",
    "FirstDowns",
    "Int",
    "Fum",
    "Points_VS",
    "Yards_VS",
    "FirstDowns_VS",
    "Int_VS",
    "Fum_VS"
)
    
    Weekly_stat <- Weekly_stat[ , ..neworder]

  return(Weekly_stat)
}


Stat_UTD <- function(Save_RDS = T) {
  Year <- year(Sys.Date())
  Week <- min(max(1, 
                   ifelse((week(Sys.Date()) - 36)>1,
                   (week(Sys.Date()) - 36),
                   17)))

  Years <- 2018:(ifelse(Week == 17,Year -1, Year ))
  Nb_Seasons <- length(Years)

  setwd("Data")
  
  path_name <- paste("NFL_Stats", as.character(Year), "_Week_", as.character(Week), ".RDS", sep = "")
  
  if (file.exists(path_name)){
    Stats_UTD <-readRDS(path_name)
    setwd(path_base)
    return(Stats_UTD)
  } else{

     
      All_Weeks_NA <- lapply(Years, function(Yearz) lapply(seq(Week), function(Week) Weekly_stats(Yearz, Week)))
    
      Stats_UTD_NA <- lapply(seq(32), 
                            function(Team) lapply(seq(Nb_Seasons), 
                                                  function(Szn) do.call("rbind", (lapply(seq(Week), 
                                                                                         function(i) All_Weeks_NA[[Szn]][[i]][Team,])))))
    

      
      Stats_UTD <- lapply(seq(32), 
                          function(Team) lapply(seq(Nb_Seasons), 
                                                function(Szn)  Stats_UTD_NA[[Team]][[Szn]][-which(Stats_UTD_NA[[Team]][[Szn]][["Complete_Names_VS"]] == "BYE"),]
                                                  ))
      
      Stats_UTD_GAME_NB <- lapply(seq(32), function(Team) lapply(seq(Nb_Seasons), function(Szn) cbind(data.table("Game_Nb" =seq(16)), Stats_UTD[[Team]][[Szn]])))
      
      neworder <- c("Complete_Names",
                     "Team_Abb",
                     "Rang_Alpha",
                     "Game_Nb",
                     "Complete_Names_VS",
                     "Team_Abb_VS",
                     "Rang_Alpha_VS",
                     "Situation",
                     "HomeField",
                     "Outcome",
                     "Points",
                     "Yards",
                     "FirstDowns",
                     "Int",
                     "Fum",
                     "Points_VS",
                     "Yards_VS",
                     "FirstDowns_VS",
                     "Int_VS",
                     "Fum_VS"
      )
      
      Stats_UTD_GAME_NB_Order <- lapply(seq(32), function(Team) lapply(seq(Nb_Seasons), function(Szn) Stats_UTD_GAME_NB[[Team]][[Szn]][ , ..neworder]))
      
      
      
      
      # 
      # All_Weeks <-  lapply(seq(32), 
      #                      function(Team)   All_Weeks_NA[[Szn]][[Week]][as.numeric(unlist(ifelse(length(which(All_Weeks_NA[[Szn]][[Week]][["Complete_Names_VS"]] == "BYE")) > 0, list(-which(All_Weeks_NA[[Szn]][[Week]][["Complete_Names_VS"]] == "BYE")), list(seq(32)  ) ))),]
      #                      ))
    
All_Weeks <-  lapply(seq(2), function(Szn) lapply(seq(16), function(Week) do.call("rbind", lapply(seq(32), function(Team) Stats_UTD_GAME_NB_Order[[Team]][[Szn]][Week,]) )))


    if (Save_RDS) {
      saveRDS(
        list(Stats_UTD_GAME_NB_Order, All_Weeks),
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
  return(list(Stats_UTD_GAME_NB_Order, All_Weeks))
}