Matchscores <- function(Matchid){
  library(Rcrawler)
  
  website <- paste0("https://www.bbc.co.uk/sport/rugby-league/match/EVP",Matchid)
  ##Website in full written out
  
  ##Base Scores
  basescores <- as.data.frame(c(rep(2,13),rep(1,4))) ##Setting starter and subs base points "Scores"
  
  Date <- ContentScraper(Url=website,
                         CssPatterns = c(".sp-c-fixture__date"),astext=TRUE) ##Date of fixture
  
  
  ##Teams
  Opponents <- ContentScraper(Url=website, 
                              CssPatterns=c(".sp-c-fixture--live-session-header"), astext=TRUE)
  Opponents <- strsplit(Opponents[[1]],Date[[1]]) ##Splits to give just team v team
  Opponents <- Opponents[[1]][1] ##unlist
  Opponents <- sub(" v ",",",Opponents) ##replace v by a comma
  Opponents <- strsplit(Opponents,split=",") ##split about the comma
  
  
  
  ##Home team name
  Hometeam.0 <- Opponents[[1]][1] ##Extract home team
  Hometeam <- gsub(" ","",Hometeam.0) ##Delete spaces
  if (Hometeam=="HullKingstonRovers"){
    Hometeam <- "HKR"
  } ##If Hull KR, then change to HKR
  Hometeam <- substr(Hometeam,1,3) ##Truncate to first 3 letters
  Hometeam <- toupper(Hometeam) ##Upper case
  
  
  ##Away team name
  Awayteam.0 <- Opponents[[1]][2] ##Extract away team
  Awayteam <- gsub(" ","",Awayteam.0) ##Delete spaces 
  if (Awayteam=="HullKingstonRovers"){
    Awayteam <- "HKR"
  } ##If Hull KR, then change to HKR
  Awayteam <- substr(Awayteam,1,3) ##Truncate to first 3 letters
  Awayteam <- toupper(Awayteam) ##Upper case
  
  
  ##Tries scored
  Tries <- ContentScraper(Url=website, 
                          CssPatterns=c(".sp-c-match-overview-header--rugby-league .gel-layout__item div div:nth-child(1)"), 
                          astext=TRUE)
  ##Extracts tries from bbc sport
  Tries <- Tries[[1]] ##Converts from list to vector
  Tries <- sub("TRIES",", ", Tries) ##Replaces TRIES with a comma
  Tries <- unlist(strsplit(Tries,", ")) ##Reforms list
  
  
  ##Conversions
  Cons <- ContentScraper(Url=website, 
                         CssPatterns=c(".sp-c-match-overview-header--rugby-league .gel-layout__item div:nth-child(2)"), 
                         astext=TRUE)
  Cons <- Cons[[1]]
  Cons <- sub("CONS",",", Cons)
  Cons <- gsub(", ", ",", Cons)
  Cons <- unlist(strsplit(Cons,","))
  ##As above for conversions
  
  
  ##Penalties
  Pens <- ContentScraper(Url=website, 
                         CssPatterns=c(".gel-layout__item div~ div+ div"), astext=TRUE)
  Pens <- sub("PENS",",", Pens)
  Pens <- gsub(", ", ",", Pens)
  Pens <- unlist(strsplit(Pens,","))
  ##As above for pens
  
  
  
  
  Lineups <- ContentScraper(Url=website, CssPatterns=c(".sp-c-match-overview-header--rugby-league+ .gel-wrap .gel-layout__item"), astext=TRUE)
  Lineups <- Lineups[[1]]
  
  ##As above for lineups
  
  
  Lineups <- sub("Lineups","",Lineups) ##Delete "lineups" text
  
  Together <- sub(Hometeam.0,"",Lineups) ##Delete home team name
  Together <- sub(Awayteam.0,"",Together) ##Delete away team name
  Together <- gsub("Substitutes","",Together) ##Delete substitutes words
  Together <- gsub("\\d+",",",Together) ##Replace numbers by commas
  Together <- sub(",","",Together) ##Delete first comma
  
  Teamlist <- strsplit(Together,",")[[1]]
  Players <- as.data.frame(Teamlist)
  Players <- cbind(Players,c(rep(Hometeam,17),rep(Awayteam,17)))
  Players <- cbind(Players,basescores)
  
  ##Drop Goals
  DG <- ContentScraper(Url=website, 
                       CssPatterns=c(".sp-c-match-overview-header--rugby-league .gel-layout__item"), astext=TRUE)
  DG <- gsub(")",") ",DG)
  
  
  
  ##Drop goal points
  if (grepl("DG",DG)){ ##Only apply if "DG" contained in DG
    DG <- strsplit(DG,"DG") ##Split about DG
    for (i in 1:34){
      if (endsWith(DG[[1]][1],as.character(Players[i,1]))){ ##If home team drop goal,
        ##give player + 1 point
        Players[i,3] <- Players[i,3] + 1
      }
      if(lengths(DG)>1){ ##If DG has home and away team
        if(startsWith(DG[[1]][2],as.character(Players[i,1]))){ ##As previous for away
          Players[i,3] <- Players[i,3]+1
        }
      }
    }
  }
  
  
  ##Adding try points for single tries
  for (i in 1:34){
    if (Players[i,1] %in% Tries){
      Players[i,3] <- Players[i,3]+4
    }
  }
  
  
  ##Adding try points for multiple tries
  for (i in 1:34){
    for (j in 2:5){
      number <- paste("(",j,")",sep = "")
      if (paste(Players[i,1], number) %in% Tries){
        Players[i,3] <- Players[i,3]+j*4
        k <- j
        while (k>1){
          Players[i,3] <- Players[i,3]+k-1
          k <- k-1
        }
      }
    }
  }
  
  ##Adding conversion points and then penalty points
  
  for (i in 1:34){
    if (Players[i,1] %in% Cons){
      Players[i,3] <- Players[i,3]+2
    }
    for (j in 2:15){
      number <- paste("(",j,")",sep = "")
      if (paste(Players[i,1], number) %in% Cons){
        Players[i,3] <- Players[i,3]+j*2
      }
    }
  }
  
  
  ##Penalties
  for (i in 1:34){
    if (Players[i,1] %in% Pens){
      Players[i,3] <- Players[i,3]+2
    }
    for (j in 2:15){
      number <- paste("(",j,")",sep = "")
      if (paste(Players[i,1], number) %in% Pens){
        Players[i,3] <- Players[i,3]+j*2
      }
    }
  }
  
  Players <- unname(Players)
  return(Players)
  
  
}

dummy <- 4

4+dummy
