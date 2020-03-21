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
  library(devtools)
  source('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
  library(randomForest)
  library(party)
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
    team <- filter(team, yearID >= 2011)
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
        team <- filter(team,yearID >= 2018)
        team$mascot <- gsub(".*\\ ","",team$name,ignore.case = T)
        team$Mascot <- ifelse(team$teamID == "BOS",str_replace(team$mascot,"Sox","Red Sox"),
                              ifelse(team$teamID == "CHA", str_replace(team$mascot,"Sox","White Sox"),
                                    ifelse(team$teamID == "LAA", str_replace(team$mascot,"Anaheim","Angels"),
                                           ifelse(team$teamID == "TOR",str_replace(team$mascot,"Jays", "Blue Jays"),team$mascot))))
        team <- select(team, teamID, name, Mascot)
        park <- left_join(park, team, by = c("Team" = "Mascot"), copy = FALSE)
        hit <- left_join(hit, park, by = c("yearID" = "year", "teamID" = "teamID"), copy = FALSE)
        hit$OPSplus <- 100 * ((hit$OBP/hit$lgOBP) + (hit$SLG/hit$lgSLG) - 1)/hit$Basic

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
    arbwarpit$outcome <- ifelse(arbwarpit$`Settled Amt.` > arbwarpit$Midpoint, 1,
                            ifelse(arbwarpit$`Settled Amt.`< arbwarpit$Midpoint, 3, 2))
    arbwarpit$outcome <- as.factor(arbwarpit$outcome)
  
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
    arbwarpos$outcome <- as.factor(arbwarpos$outcome)
  
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
    pitch <- select(pitch, Player, `Player Amt.`, `Team Amt.`, Midpoint, IP = IP.y, ERA, SO, WHIP, FIP, WAR = Total.WAR, outcome)

  #combining arbwar data with position players data
    hit <- left_join(hit, drs, by = c("playerName" = "ï..Name" , "yearID" = "year"), copy = FALSE)
    hit <- select(hit, playerName, yearID, G, AB, R, H, HR, RBI, SB, SO, AVG, OPSplus, RC, Pos, Inn, DRS)
    hit <- left_join(arbwarpos, hit, by = c("Player" = "playerName", "Year.x" = "yearID"))
    hit <- na.omit(hit)
    hit <- select(hit, Player, `Player Amt.`, `Team Amt.`, Midpoint, WAR = Total.WAR, AVG, OPSplus, RC, DRS, RBI, outcome)

# Predictive model building
  
  #Transforming data
    
    #pitching data
      pitch.trans <- pitch
      pitch.bc <- preProcess(pitch, method = "BoxCox")  #need package e1071 installed but don't needed it loaded 
      pitch.bc$bc
      pitch.trans$`Player Amt.` <- pitch.trans$`Player Amt.`^(1/10)
      pitch.trans$`Team Amt.` <- pitch.trans$`Team Amt.`^(3/10)
      pitch.trans$Midpoint <- pitch.trans$Midpoint^(1/5)
      pitch.trans$IP <- pitch.trans$IP^(1/10)
      pitch.trans$SO <- pitch.trans$SO^(1/5)
      pitch.trans$WHIP <- pitch.trans$WHIP^(3/10)
      pitch.trans$FIP <- pitch.trans$FIP^(4/5)
    #Position Player data
      hit.trans <- hit
      hit.bc <- preProcess(hit, method = "BoxCox")
      hit.bc$bc
      hit.trans$`Player Amt.` <- log(hit.trans$`Player Amt.`)
      hit.trans$`Team Amt.` <- hit.trans$`Team Amt.`^(-.1)
      hit.trans$Midpoint <- log(hit.trans$Midpoint)
      hit.trans$AVG <- hit.trans$AVG^(9/10)
      hit.trans$OPSplus <- hit.trans$OPSplus^(9/10)
      hit.trans$RC <- hit.trans$RC^(3/10)
      hit.trans$RBI <- hit.trans$RBI^(2/5)
  #Neural Networks 
    #functions
      misclass.nnet <- function(fit,y) {
        temp <- table(predict(fit,type="class"),y)
        cat("Table of Misclassification\n")
        cat("(row = predicted, col = actual)\n")
        print(temp)
        cat("\n\n")
        numcor <- sum(diag(temp))
        numinc <- length(y) - numcor
        mcr <- numinc/length(y)
        cat(paste("Misclassification Rate = ",format(mcr,digits=3)))
        cat("\n")
      }
      
      cnnet.cv = function (fit, y, data, B = 25, p = 0.667, size = 5, decay = 0.001, maxit = 1000,trace=T) 
      {
        n <- length(y)
        cv <- rep(0, B)
        nin <- floor(n * p)
        out <- n - nin
        for (i in 1:B) {
          sam <- sample(1:n, nin)
          temp <- data[sam, ]
          fit2 <- nnet(formula(fit), data = temp, size = size, 
                       decay = decay, maxit = maxit,trace=trace)
          ynew <- predict(fit2, newdata = data[-sam, ], type = "class")
          tab <- table(y[-sam], ynew)
          mc <- out - sum(diag(tab))
          cv[i] <- mc/out
        }
        cv
      }
      
    #implentation
      #Pitchers
        names(pitch.trans)
        pitch.test <- pitch.trans[,-1]
        names(pitch.test)
        pitch.nn <- nnet(outcome ~., data = pitch.test, size = 10, maxit = 5000, decay = .001)
        pitch.miss <- misclass.nnet(pitch.nn, pitch.test$outcome)
        pitch.cv <- cnnet.cv(pitch.nn, pitch.test$outcome, pitch.test, size = 10, decay = .001, maxit = 10000)
        summary(pitch.cv)
        plot.nnet(pitch.nn)
      #position players
        names(hit)
        hit.test <- hit.trans[,-1]
        hit.nn <- nnet(outcome~., data = hit.test, size = 10, maxit = 100, decay = .01)
        hit.miss <- misclass.nnet(hit.nn, hit.test$outcome)
        hit.cv <- cnnet.cv(hit.nn, hit.test$outcome, hit.test, size = 10, maxit = 10000, decay = .01)
        summary(hit.cv)
        plot.nnet(hit.nn)
  #Random Forest
      #Functions
        crf.sscv = function(fit,y,data,B=25,p=.333,mtry=fit$mtry,ntree=fit$ntree) {
          n = length(y)
          cv <- rep(0,B)
          for (i in 1:B) {
            ss <- floor(n*p)
            sam <- sample(1:n,ss)
            temp <- data[-sam,]
            fit2 <- randomForest(formula(fit),data=temp,mtry=mtry,ntree=ntree)
            ynew <- predict(fit2,newdata=data[sam,],type="class")
            tab <- table(y[sam],ynew)
            mc <- ss - sum(diag(tab))
            cv[i] <- mc/ss
          }
          cv
        }
        
        #Pitchers
          pit.test <- pitch[,-1]
          pit.test <- select(pit.test, player = `Player Amt.`, team = `Team Amt.`, Midpoint, IP, ERA, SO, WHIP, FIP, WAR, outcome)
          names(pit.test)
          pit.rf <- randomForest(outcome~., data = pit.test, mtry = 2)
          pit.cv <- crf.sscv(pit.rf, pit.test$outcome, pit.test)          
          summary(pit.cv) 
        #Postition Players
          hit.test <- hit[,-1]
          hit.test <- select(hit.test, player = `Player Amt.`, team = `Team Amt.`, Midpoint, WAR, AVG, OPSplus, RC, DRS, RBI, outcome)
          names(hit.test)
          hit.rf <- randomForest(outcome~., data = hit.test, mtry = 2)          
          hit.rf.cv <- crf.sscv(hit.rf, hit.test$outcome, hit.test)          
          summary(hit.rf.cv)          
            