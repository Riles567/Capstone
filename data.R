setwd("C:/Users/yz9186ci/Desktop/Capstone/DATA") # Labtop
setwd("C:/Users/arile/Desktop/Capstone/DATA") #desktop

#Packages
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(Stack)
#Data input

  #Player Info

    player <- read.csv("People.csv")
    salary <- read.csv("Salaries.csv")
    names(field)
  
  #WAR
    
    war11 <- read.csv("FanGraphs Leaderboard2011.csv")
    war12 <- read.csv("FanGraphs Leaderboard2012.csv")
    war13 <- read.csv("FanGraphs Leaderboard2013.csv")
    war14 <- read.csv("FanGraphs Leaderboard2014.csv")
    war15 <- read.csv("FanGraphs Leaderboard2015.csv")
    war16 <- read.csv("FanGraphs Leaderboard2016.csv")
    war17 <- read.csv("FanGraphs Leaderboard2017.csv")
    war18 <- read.csv("FanGraphs Leaderboard2018.csv")
    head(war11)
  #Fielding Independent Pitching (FIP)
    
    FIPcon <- read.csv("FanGraphs LeaderboardFIP.csv")
    pitching <- read.csv("Pitching.csv")
  names(FIPcon)
  #Stats for Calculations
    
    team <- read.csv("Teams.csv")
    appear <- read.csv("Appearances.csv")  
    field <- read.csv("Fielding.csv")
    batting <- read.csv("Batting.csv")
  
  #arbitration 2011 to 2018
    
    arb11 <- read_excel("arb2011.xlsx")
    arb12 <- read_excel("arb2012.xlsx")
    arb13 <- read_excel("arb2013.xlsx")
    arb14 <- read_excel("arb2014.xlsx")
    arb15 <- read_excel("arb2015.xlsx")
    arb16 <- read_excel("arb2016.xlsx")
    arb17 <- read_excel("arb2017.xlsx")
    arb18 <- read_excel("arb2018.xlsx")
    
    head(arb11)
  #Defensice Runs Saved
    
    drs11 <- read.csv("DRS2011.csv")
    drs12 <- read.csv("DRS2012.csv")
    drs13 <- read.csv("DRS2013.csv")
    drs14 <- read.csv("DRS2014.csv")
    drs15 <- read.csv("DRS2015.csv")
    drs16 <- read.csv("DRS2016.csv")
    drs17 <- read.csv("DRS2017.csv")
    drs18 <- read.csv("DRS2018.csv")
# Data Cleaning and Calculations
    
    #Function for Cleaning Arbitration Data
      clean <- function(arb){
        arb <- arb[,-3]
        arb.clean <- arb
        arb.clean$`Settled Amt.` <- str_remove_all(arb.clean$`Settled Amt.`, "[$M]")
        arb.clean$Midpoint <- str_remove_all(arb.clean$Midpoint, "[$M]")
        arb.clean$`Team Amt.` <- str_remove_all(arb.clean$`Team Amt.`, "[$M]")
        arb.clean$`Player Amt.` <- str_remove_all(arb.clean$`Player Amt.`, "[$M]")
        arb.clean$Player <- str_remove_all(arb.clean$Player, "[‡†]") 
        arb.clean$Player <- trimws(arb.clean$Player, which = "right")
        arb.clean$`Settled Amt.` <- as.numeric(arb.clean$`Settled Amt.`)
        arb.clean$Midpoint <- as.numeric(arb.clean$Midpoint)
        arb.clean$`Team Amt.` <- as.numeric(arb.clean$`Team Amt.`)
        arb.clean$`Player Amt.` <- as.numeric(arb.clean$`Player Amt.`)
        arb.clean <- na.omit(arb.clean)
      }
    #arbitration data
      
      #all numberic values outside of year is in millions
      
      #2011
        arb11.clean <- clean(arb11)
        head(arb11.clean)
      #2012
        arb12.clean <- clean(arb12)
        head(arb12.clean)
      #2013
        arb13.clean <- clean(arb13)
        head(arb13.clean)
      #2014
        arb14.clean <- clean(arb14)  
        head(arb14.clean)
      #2015
        arb15.clean <- clean(arb15)
        head(arb15.clean)
      #2016
        arb16.clean <- clean(arb16)
        head(arb16.clean)
      #2017
        arb17.clean <- clean(arb17)
        head(arb17.clean)
      #2018
        arb18.clean <- clean(arb18)
        head(arb18.clean)
    
    #DRS Data
        
  #seperating position players and pitchers
      
      #Function for position and pitchers
        pos <- function(war){
          warpos <- filter(war, Pos != "P")
        }
        pit <- function(war){
          warpit <- filter(war, Pos == "P")
        }
      #Seperating War Data
       
        #2011  
          warpos11 <- pos(war11)
          warpit11 <- pit(war11)
        #2012
          warpos12 <- pos(war12)
          warpit12 <- pit(war12)
        #2013
          warpos13 <- pos(war13)
          warpit13 <- pit(war13)
        #2014
          warpos14 <- pos(war14)
          warpit14 <- pit(war14)
        #2015
          warpos15 <- pos(war15)
          warpit15 <- pit(war15)
        #2016
          warpos16 <- pos(war16)
          warpit16 <- pit(war16)
        #2017
          warpos17 <- pos(war17)
          warpit17 <- pit(war17)
        #2018
          warpos18 <- pos(war18)
          warpit18 <- pit(war18)
  
  #Fielding Independent Pitching and Other predictors for Pitchers
      names(player)
      names(pitching)
      names(FIPcon)
      head(FIPcon)
      FIPcon <- select(FIPcon, ï..Season, cFIP)
      
      #Formula (13 * HR + 3*(BB + HBP) - 2*K)/(IP) + FIP COnstant
        pitchers <- left_join(player, pitching, by = c("playerID" = "playerID"), copy = FALSE)
        pitchers <- unite(pitchers, Player, c(nameFirst, nameLast), sep = " ")
        pitchers <- select(pitchers, nameFirst, nameLast, yearID, IPouts, HR, SO, BB, IBB, HBP)
        pitchers <- left_join(pitchers, FIPcon, by = c("yearID" = "ï..Season"), copy = FALSE)
        pitchers <- filter(pitchers, yearID >= 2011, IPouts >= 30, G >= 5)
        pitchers$FIP <- ((13*pitchers$HR + 3*(pitchers$BB + pitchers$IBB + pitchers$HBP) - 2*pitchers$SO)/(pitchers$IPouts/3))+pitchers$cFIP
        pitchers$IP <- pitchers$IPouts/3
        pitchers$WHIP <- (pitchers$BB+  pitchers$HBP + pitchers$IBB + pitchers$H)/pitchers$IP
        pitchers <- select(pitchers, playerID, Player, IP, ERA, FIP, SO, WHIP)
        
  #OPS100
  
  #Predictors for position players
      # hitting
        hitfield <- select(field, playerID, POS, yearID)
        hit <- left_join(player, batting, by = c("playerID" = "playerID"), copy = FALSE)
        hit <- unite(hit, playerName, c(nameFirst, nameLast), sep = " ", remove = FALSE)
        hit <- select(hit, playerID, playerName, yearID, G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, GIDP, SF, SH)
        hit <- filter(hit, yearID >= "2011", G >= 10, AB >= 50)
        hit$SLG <- (hit$H + (2*hit$x2B) + (3*hit$x3B) + (4*hit$HR))/hit$AB
        hit$AVG <- (hit$H + hit$X2B + hit$X3B + hit$HR)/hit$AB
        hit$OBP <- (hit$H + hit$BB + hit$IBB + hit$HBP)/(hit$AB + hit$BB + hit$IBB + hit$HBP + hit$SF + hit$SH)
        hit$OPS <- hit$SLG + hit$OBP
        hit <- group_by(hit, yearID)
        hit$lgOBP <- mean(hit$OBP)
        hit$lgSLG <- mean(hit$SLG)
        hit$OPSplus <- 100 * ((hit$OBP/hit$lgOBP) + (hit$SLG/hit$lgSLG) - 1)/
        summary(hit)
        head(hit)
        View(hit)
      #fielding
# Data Joining
  
  #Stacking the arbitration data into one table
    