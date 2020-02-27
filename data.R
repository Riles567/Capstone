setwd("C:/Users/yz9186ci/Desktop/Capstone/DATA") # Labtop
setwd("C:/Users/arile/Desktop/Capstone/DATA") #desktop

#Make sure when opening up the code to open with encoding of UTF-8 for names to work

#Packages
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(Stack)
  library(caret)
  library(nnet)
#Data input

  #Player Info

    player <- read.csv("People.csv")
    salary <- read.csv("Salaries.csv")
  
  #WAR
    
    war11 <- read.csv("FanGraphs Leaderboard2011.csv")
    war11$Year <- 2011
    war12 <- read.csv("FanGraphs Leaderboard2012.csv")
    war12$Year <- 2012
    war13 <- read.csv("FanGraphs Leaderboard2013.csv")
    war13$Year <- 2013
    war14 <- read.csv("FanGraphs Leaderboard2014.csv")
    war14$Year <- 2014
    war15 <- read.csv("FanGraphs Leaderboard2015.csv")
    war15$Year <- 2015
    war16 <- read.csv("FanGraphs Leaderboard2016.csv")
    war16$Year <- 2016
    war17 <- read.csv("FanGraphs Leaderboard2017.csv")
    war17$Year <- 2017
    war18 <- read.csv("FanGraphs Leaderboard2018.csv")
    war18$Year <- 2018
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
    
  #Defensice Runs Saved
    
    drs11 <- read.csv("DRS2011.csv")
    drs11$year <- 2011
    drs12 <- read.csv("DRS2012.csv")
    drs12$year <- 2012
    drs13 <- read.csv("DRS2013.csv")
    drs13$year <- 2013
    drs14 <- read.csv("DRS2014.csv")
    drs14$year <- 2014
    drs15 <- read.csv("DRS2015.csv")
    drs15$year <- 2015
    drs16 <- read.csv("DRS2016.csv")
    drs16$year <- 2016
    drs17 <- read.csv("DRS2017.csv")
    drs17$year <- 2017
    drs18 <- read.csv("DRS2018.csv")
    drs18$year <- 2018
  
  # Ball pack factors
    park11 <- read.csv("ballpark_factors11.csv")
    park12 <- read.csv("ballpark_factors12.csv")
    park13 <- read.csv("ballpark_factors13.csv")
    park14 <- read.csv("ballpark_factors14.csv")
    park15 <- read.csv("ballpark_factors15.csv")
    park16 <- read.csv("ballpark_factors16.csv")
    park17 <- read.csv("ballpark_factors17.csv")
    park18 <- read.csv("ballpark_factors18.csv")

  #Park Factors stack
    park <- Stack(park11, park12)
    park <- Stack(park, park13)
    park <- Stack(park, park14)
    park <- Stack(park, park15)
    park <- Stack(park, park16)
    park <- Stack(park, park17)
    park <- Stack(park, park18)
    park <- select(park, year = ï..Season, Team, Basic)

# Data Cleaning and Calculations
    
    #Function for Cleaning Arbitration Data
    #for cleaning to work when pulling off GitHub the file must be saved with encoding
    #the encoding that it should be saved with is UTF-8
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
      #2012
        arb12.clean <- clean(arb12)
      #2013
        arb13.clean <- clean(arb13)
      #2014
        arb14.clean <- clean(arb14)  
      #2015
        arb15.clean <- clean(arb15)
      #2016
        arb16.clean <- clean(arb16)
      #2017
        arb17.clean <- clean(arb17)
      #2018
        arb18.clean <- clean(arb18)
    
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
      
      FIPcon <- select(FIPcon, ï..Season, cFIP)
      
      #Formula (13 * HR + 3*(BB + HBP) - 2*K)/(IP) + FIP COnstant
        pitchers <- left_join(player, pitching, by = c("playerID" = "playerID"), copy = FALSE)
        pitchers <- unite(pitchers, Player, c(nameFirst, nameLast), sep = " ")
        pitchers <- select(pitchers, Player, yearID, G,IPouts, ERA, HR, H, SO, BB, IBB, HBP)
        pitchers <- left_join(pitchers, FIPcon, by = c("yearID" = "ï..Season"), copy = FALSE)
        pitchers <- filter(pitchers, yearID >= 2011, IPouts >= 30, G >= 5)
        pitchers$FIP <- ((13*pitchers$HR + 3*(pitchers$BB + pitchers$IBB + pitchers$HBP) - 2*pitchers$SO)/(pitchers$IPouts/3))+pitchers$cFIP
        pitchers$IP <- pitchers$IPouts/3
        pitchers$WHIP <- (pitchers$BB +  pitchers$HBP + pitchers$IBB + pitchers$H)/pitchers$IP
        pitchers <- select(pitchers, Player, yearID, IP, ERA, FIP, SO, WHIP)

  #Predictors for position players
        
      # hitting
        hit <- left_join(player, batting, by = c("playerID" = "playerID"), copy = FALSE)
        hit <- unite(hit, playerName, c(nameFirst, nameLast), sep = " ", remove = FALSE)
        hit <- select(hit, playerID, playerName, teamID, yearID, G, AB, R, H, X2B, X3B, HR, RBI, SB, CS, BB, SO, IBB, HBP, GIDP, SF, SH)
        hit <- filter(hit, yearID >= "2011", G >= 10, AB >= 50)
        hit$SLG <- (hit$H + (2*hit$X2B) + (3*hit$X3B) + (4*hit$HR))/hit$AB
        hit$AVG <- (hit$H + hit$X2B + hit$X3B + hit$HR)/hit$AB
        hit$OBP <- (hit$H + hit$BB + hit$IBB + hit$HBP)/(hit$AB + hit$BB + hit$IBB + hit$HBP + hit$SF + hit$SH)
        hit$OPS <- hit$SLG + hit$OBP
        hit$RC <- (hit$H + hit$BB - hit$CS + hit$HBP - hit$GIDP)*((hit$H + (2*hit$X2B) + (3*hit$X3B) + 
                    (4*hit$HR)+(.26*(hit$BB - hit$IBB + hit$HBP))+(.52*(hit$SH + hit$SF + hit$SB))))/(hit$AB + hit$BB + hit$HBP + hit$SH + hit$SF)
        hit <- group_by(hit, yearID)
        hitgroup <- summarise(hit, lgOBP = mean(OBP, na.rm = TRUE), lgSLG = mean(SLG, na.rm = TRUE))
        hit <- ungroup(hit)
        hit <- left_join(hit, hitgroup, by = c("yearID" = "yearID"), copy = FALSE)
        hit <- left_join(hit, park, by = c("yearID" = "year", "teamID" = "Team"), copy = FALSE)
        hit$OPSplus <- 100 * ((hit$OBP/hit$lgOBP) + (hit$SLG/hit$lgSLG) - 1)/hit$Basic
        str(hit)
        str(park)
        head(hit)
# Data Joining
  
  #Combining War and Arbitration data sets

    #pitching WAR data
      arbwarpit11 <- left_join(arb11.clean, warpit11, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpit12 <- left_join(arb12.clean, warpit12, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpit13 <- left_join(arb13.clean, warpit13, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpit14 <- left_join(arb14.clean, warpit14, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpit15 <- left_join(arb15.clean, warpit15, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpit16 <- left_join(arb16.clean, warpit16, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpit17 <- left_join(arb17.clean, warpit17, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpit18 <- left_join(arb18.clean, warpit18, by = c("Player" = "ï..Name"), copy = FALSE)
    
    #posistion players
      arbwarpos11 <- left_join(arb11.clean, warpos11, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpos12 <- left_join(arb12.clean, warpos12, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpos13 <- left_join(arb13.clean, warpos13, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpos14 <- left_join(arb14.clean, warpos14, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpos15 <- left_join(arb15.clean, warpos15, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpos16 <- left_join(arb16.clean, warpos16, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpos17 <- left_join(arb17.clean, warpos17, by = c("Player" = "ï..Name"), copy = FALSE)
      arbwarpos18 <- left_join(arb18.clean, warpos18, by = c("Player" = "ï..Name"), copy = FALSE)

  #WAR pitchers
    arbwarpit <- Stack(arbwarpit11, arbwarpit12)
    arbwarpit <- Stack(arbwarpit, arbwarpit13)
    arbwarpit <- Stack(arbwarpit, arbwarpit14)
    arbwarpit <- Stack(arbwarpit, arbwarpit15)
    arbwarpit <- Stack(arbwarpit, arbwarpit16)
    arbwarpit <- Stack(arbwarpit, arbwarpit17)
    arbwarpit <- Stack(arbwarpit, arbwarpit18)
    arbwarpit <- arbwarpit[,-11]
    arbwarpit <- na.omit(arbwarpit)
    arbwarpit$outcome <-  arbwarpit$outcome <- ifelse(arbwarpit$`Settled Amt.` > arbwarpit$Midpoint, 1,
                            ifelse(arbwarpit$`Settled Amt.`< arbwarpit$Midpoint, 3, 2))
  #WAR Position players
    arbwarpos <- Stack(arbwarpos11, arbwarpos12)
    arbwarpos <- Stack(arbwarpos, arbwarpos13)
    arbwarpos <- Stack(arbwarpos, arbwarpos14)
    arbwarpos <- Stack(arbwarpos, arbwarpos15)
    arbwarpos <- Stack(arbwarpos, arbwarpos16)
    arbwarpos <- Stack(arbwarpos, arbwarpos17)
    arbwarpos <- Stack(arbwarpos, arbwarpos18)
    arbwarpos <- arbwarpos[,-12]
    arbwarpos <- na.omit(arbwarpos)
    arbwarpos$outcome <- ifelse(arbwarpos$`Settled Amt.` > arbwarpos$Midpoint, 1,
           ifelse(arbwarpos$`Settled Amt.`< arbwarpos$Midpoint, 3, 2))
                           
  #DRs Stack
    drs <- Stack(drs11,drs12)
    drs <- Stack(drs, drs13)
    drs <- Stack(drs, drs14)
    drs <- Stack(drs, drs15)
    drs <- Stack(drs, drs16)
    drs <- Stack(drs, drs17)
    drs <- Stack(drs, drs18)
    
  #combining arbwar data with pitching data
    pitch <- left_join(arbwarpit, pitchers, by = c("Player" = "Player", "Year.x" = "yearID"), copy = FALSE)
    pitch <- na.omit(pitch)
    pitch <- select(pitch, Player, Team = Team.x, `Player Amt.`, `Team Amt.`, Midpoint, IP = IP.y, ERA, SO, WHIP, FIP, WAR = Total.WAR, outcome)

  #combining arbwar data with position players data
    hit <- left_join(hit, drs, by = c("playerName" = "ï..Name" , "yearID" = "year"), copy = FALSE)
    hit <- select(hit, playerName, yearID, G, AB, R, H, HR, RBI, SB, SO, AVG, OPSplus, RC, Pos, Inn, DRS)
    
# Predictive model building
  
  
  #Transforming data
    
    #pitching data
      summary(pitch)
      names(pitch)
      pitch.trans <- pitch
      pitch.bc <- preProcess(pitch, method = "BoxCox")
      pitch.bc$bc
      pitch.trans$`Player Amt.` <- pitch.trans$`Player Amt.`^(1/10)
      pitch.trans$`Team Amt.` <- pitch.trans$`Team Amt.`^(3/10)
      pitch.trans$Midpoint <- pitch.trans$Midpoint^(1/5)
      pitch.trans$IP <- pitch.trans$IP^(1/10)
      pitch.trans$SO <- pitch.trans$SO^(1/5)
      pitch.trans$WHIP <- pitch.trans$WHIP^(3/10)
      pitch.trans$FIP <- pitch.trans$FIP^(4/5)
      pitch.trans$outcome <- pitch.trans$outcome^(1/2)
    #Position Player data
      
  #Neural Networks 
    #functions
    nnet.sscv <- function(x,y,fit,p=.667,B=100,size=3,decay=fit$decay,skip=T,
                          linout=T,maxit=10000){
      n = length(y)
      MSEP = rep(0,B)
      MAEP = rep(0,B)
      MAPEP = rep(0,B)
      ss = floor(n*p)
      for (i in 1:B){
        sam = sample(1:n,ss,replace=F)
        fit2 = nnet(x[sam,],y[sam],size=size,linout=linout,skip=skip,decay=decay,
                    maxit=maxit,trace=F)
        ynew = predict(fit2,newdata=x[-sam,])
        MSEP[i]=mean((y[-sam]-ynew)^2)
        MAEP[i]=mean(abs(y[-sam]-ynew))
        MAPEP[i]=mean(abs(y[-sam]-ynew)/y[-sam])
      }
      RMSEP = sqrt(mean(MSEP))
      MAE = mean(MAEP)
      MAPE = mean(MAPEP)
      cat("RMSEP\n")
      cat("===============\n")
      cat(RMSEP,"\n\n")
      cat("MAE\n")
      cat("===============\n")
      cat(MAE,"\n\n")
      cat("MAPE\n")
      cat("===============\n")
      cat(MAPE,"\n\n")
      temp = data.frame(MSEP=MSEP,MAEP=MAEP,MAPEP=MAPEP)
      return(temp)
    }
    #implentation
    names(pitch)
    pitch.test <- pitch.trans[,-1]
    pitch.nn <- nnet(outcome ~., data = pitch.test, size = 10, lineout = T, skip = T, maxit = 100, decay = .01)
    pit.x <- pitch.test[,-pitch.test$outcome]    
    pit.y <- pitch.test[,pitch.test$outcome]
    nrow(pit.x)
    result <- nnet.sscv(pit.x, pit.y, pitch.nn, size = 10)    
    summary(result)
    head(pit.x)
    head(pit.y)
    
    
    
    
   