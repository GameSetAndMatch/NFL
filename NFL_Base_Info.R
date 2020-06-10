#NFL_Base
#
#
#Every NFL Teams
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


# Stats_Used
stats_name <- c("Points", "Yards", "FirstDown", "Int", "Fum","Points_VS",
                "Yards_VS","FirstDown_VS","Int_VS","Fum_VS","Week_Matchup", "Pos_Team" )

Var_Info <- c("Complete_Names","Team_Abb","Rang_Alpha","Complete_Names_VS","Team_Abb_VS" ,"Rang_Alpha_VS","Field","Situation")
Var_Exp <- c("Yards", "FirstDowns", "Int", "Fum", "Points_VS", "Yards_VS", "Firstdowns_VS", "Int_VS", "Fum_VS", "HomeField")




