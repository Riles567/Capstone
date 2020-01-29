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
    #Function for cleaning
      clean <- function(arb){
        arb <- arb[,-3]
        arb.clean <- arb
        arb.clean$`Settled Amt.` <- str_remove_all(arb.clean$`Settled Amt.`, "[$M]")
        arb.clean$Midpoint <- str_remove_all(arb.clean$Midpoint, "[$M]")
        arb.clean$`Team Amt.` <- str_remove_all(arb.clean$`Team Amt.`, "[$M]")
        arb.clean$`Player Amt.` <- str_remove_all(arb.clean$`Team Amt.`, "[$M]")
        arb.clean$Player <- str_remove_all(arb.clean$Player, "[??????]")
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
        arb18.clean$Player <- str_remove_all(arb18.clean$Player, "[??????]")
        head(arb18.clean)
# Data Calculations
  
# Data Joining
  