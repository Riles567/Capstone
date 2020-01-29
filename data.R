setwd("C:/Users/yz9186ci/Desktop/Capstone/DATA") # Labtop
setwd("C:/Users/arile/Desktop/Capstone/DATA") #desktop

#Packages
  library(readxl)
  library(dplyr)
  library(stringr)
#Data input

  #Player Info
    player <- read.csv("People.csv")
    salary <- read.csv("Salaries.csv")
  
  #WAR
    war11 <- read.csv("FanGraphs Leaderboard2011.csv")
    war12 <- read.csv("FanGraphs Leaderboard2012.csv")
    war13 <- read.csv("FanGraphs Leaderboard2013.csv")
    war14 <- read.csv("FanGraphs Leaderboard2014.csv")
    war15 <- read.csv("FanGraphs Leaderboard2015.csv")
    war16 <- read.csv("FanGraphs Leaderboard2016.csv")
    war17 <- read.csv("FanGraphs Leaderboard2017.csv")
    war18 <- read.csv("FanGraphs Leaderboard2018.csv")

  #Fielding Independent Pitching (FIP)
    FIPcon <- read.csv("FanGraphs LeaderboardFIP.csv")
    pitching <- read.csv("Pitching.csv")
  
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

# Data Cleaning
    
    #arbitration data
      #2011
        arb11 <- arb11[,-3]
        arb11 <- na.omit(arb11)
        arb11.clean <- arb11
        arb11.clean$`Settled Amt.` <- str_remove_all(arb11.clean$`Settled Amt.`,"[$M]")
        arb11.clean$Midpoint <- str_remove_all(arb11.clean$Midpoint, "[$M]")
        arb11.clean$`Team Amt.` <- str_remove_all(arb11.clean$`Team Amt.`, "[$M]")
        arb11.clean$`Player Amt.` <- str_remove_all(arb11.clean$`Player Amt.`, "[$M]")
        
        head(arb11.clean)
      #2012
        arb12 <- arb12[,-3]
        arb12 <- na.omit(arb12)
        arb12.clean <- arb12
        arb12.clean$`Settled Amt.` <- str_remove_all(arb12.clean$`Settled Amt.`, "[$M]")
        arb12.clean$Midpoint <- str_remove_all(arb12.clean$Midpoint, "[$M]")
        arb12.clean$`Team Amt.` <- str_remove_all(arb12.clean$`Team Amt.`, "[$M]")
        arb12.clean$`Player Amt.` <- str_remove_all(arb12.clean$`Player Amt.`, "[$M]")
        head(arb12.clean)
      #2013
        
      #2014
      #2015
      #2016
      #2017
      #2018
     
      
# Data Calculations
  
# Data Joining
  