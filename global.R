library(shiny)
library(shinydashboard)
library(dplyr)
library(fmsb)

# Load Data Set
Men <- read.csv('atp_matches_ioc_fixed.csv', stringsAsFactors = FALSE)[2:37]
choicez <- colnames(Men)


# Correlation Matrix Function - For Plotting
par(xpd=TRUE)
correlationz = function(X) {
  numeric_features <- names(X)[sapply(X, is.numeric)]
  corrw_numtran <- cor(X %>% select(one_of(numeric_features)), 
                       method = "pearson", 
                       use = "pairwise.complete.obs")
}

# Percentage Calculations with Date filtering
Percentage_Calc <- function(X,date1,date2,round) {
  FiltX <- X %>% filter(Date >= date1 & Date <= date2)
  MenWinner <- FiltX %>% filter(complete.cases(Winner_Rank)) %>% group_by(Name = Winner) %>% 
                                                           summarise(Hand = unique(Winner_Hand)[1],
                                                           Height = mean(Winner_Height, na.rm = TRUE),
                                                           Country = Winner_Country[1],
                                                           `Best Rank` = min(Winner_Rank, na.rm = TRUE),
                                                           TotalW = n(),
                                                           TotalL = 0,
                                                           TotalPts = sum(Winner_ServicePts + Loser_ServicePts, na.rm = TRUE),
                                                           Aces = sum(Winner_Aces, na.rm = TRUE),
                                                           DoubleFaults = sum(Winner_DblFaults, na.rm = TRUE),
                                                           ServicePts = sum(Winner_ServicePts, na.rm = TRUE),
                                                           FirstServesWon = sum(Winner_FirstServeWon, na.rm = TRUE),
                                                           SecondServesWon = sum(Winner_SecondServeWon, na.rm = TRUE),
                                                           ServiceGames = sum(Winner_ServiceGames, na.rm = TRUE),
                                                           ServiceBreakSaved = sum(Winner_BreakSaved, na.rm = TRUE),
                                                           ServiceBreaksFaced = sum(Winner_BreakFaced, na.rm = TRUE),
                                                           BreakPoints = sum(Loser_BreakFaced, na.rm = TRUE),
                                                           Breaks = sum(Loser_BreakFaced - Loser_BreakSaved, na.rm = TRUE))
  
  MenLoser <- FiltX %>% filter(complete.cases(Loser_Rank)) %>% group_by(Name = Loser) %>% 
                                                           summarise(Hand = unique(Loser_Hand)[1],
                                                           Height = mean(Loser_Height, na.rm = TRUE)[1],
                                                           Country = Loser_Country[1],
                                                           `Best Rank` = min(Loser_Rank, na.rm = TRUE),
                                                           TotalW = 0,
                                                           TotalL = n(),
                                                           TotalPts = sum(Winner_ServicePts + Loser_ServicePts, na.rm = TRUE),
                                                           Aces = sum(Loser_Aces, na.rm = TRUE),
                                                           DoubleFaults = sum(Loser_DblFaults, na.rm = TRUE),
                                                           ServicePts = sum(Loser_ServicePts, na.rm = TRUE),
                                                           FirstServesWon = sum(Loser_FirstServeWon, na.rm = TRUE),
                                                           SecondServesWon = sum(Loser_SecondServeWon, na.rm = TRUE),
                                                           ServiceGames = sum(Loser_ServiceGames, na.rm = TRUE),
                                                           ServiceBreakSaved = sum(Loser_BreakSaved, na.rm = TRUE),
                                                           ServiceBreaksFaced = sum(Loser_BreakFaced, na.rm = TRUE),
                                                           BreakPoints = sum(Winner_BreakFaced, na.rm = TRUE),
                                                           Breaks = sum(Winner_BreakFaced - Winner_BreakSaved, na.rm = TRUE))
  
  RawDataNumerical <- rbind(MenWinner, MenLoser) %>% select(c(1,6:18)) %>% group_by(Name) %>% summarise_all(sum)
  RawDataCategorical <- rbind(MenWinner, MenLoser) %>% select(1:5) %>% group_by(Name) %>% filter(complete.cases(is.numeric(`Best Rank`))) %>% 
                                                                                                   summarise(Hand = Hand[1],
                                                                                                    Height = mean(Height, na.rm = TRUE),
                                                                                                    Country = Country[1],
                                                                                                    `Best Rank` = as.integer(min(`Best Rank`)))
  
  RawData <- left_join(RawDataCategorical, RawDataNumerical, by = 'Name') %>% distinct()
  
  Percentages <- RawData %>% transmute(Name,
                                       Hand,
                                       Height,
                                       Country,
                                       `Best Rank`,
                                       `Total Matches` = TotalW+TotalL,
                                       `Win Percentage` = TotalW/(TotalW + TotalL),
                                       Ace = Aces/ServicePts,
                                       `Double Fault Percentage` = DoubleFaults/ServicePts,
                                       `Service Point Won Percentage` = (FirstServesWon + SecondServesWon)/ServicePts,
                                       `Percent 1st Serve Won` = FirstServesWon/ServicePts,
                                       `Percent 2nd Serve Won` = SecondServesWon/ServicePts,
                                       `Hold Serve Percentage` = 1 - (ServiceBreaksFaced - ServiceBreakSaved)/ServiceGames,
                                       `Break Point Save Percentage` = ServiceBreakSaved/ServiceBreaksFaced,
                                       `Break Point Conversion` = Breaks/BreakPoints) %>% filter(complete.cases(Height)) %>%
    filter(complete.cases(Ace)) %>%
    filter(complete.cases(Height)) %>% arrange(-`Total Matches`)
  
  if (round == 'yes') {
    Percentages <- cbind(Name = Percentages$Name,round(Percentages[sapply(Percentages, is.numeric)], 2))
  } else {
    
  }
  
  return(Percentages)
}

Stats = colnames(Percentage_Calc(Men[1,], '1990-12-31', '2017-08-28', 'no'))[-c(1,2,4)]

Scatter_Choices = Stats

# Data for the Tennis Trend Map
# Only using Total_Time and Total_Games... need to change
Map_Calc <- function(X,date1,date2) {
  FiltX <- X %>% filter(Date >= date1 & Date <= date2) %>% filter
  MenWinner <- FiltX %>% filter(complete.cases(Winner_Rank)) %>% group_by(Country = Winner_Country) %>% 
    summarise(Height = mean(Winner_Height, na.rm = TRUE),
              `Best Rank` = sum(Winner_Rank, na.rm = TRUE),
              TotalW = n(),
              TotalL = 0,
              TotalGrandSlamWins = (sum(Tournament == 'Wimbledon' & Round == 'F') +
                sum(Tournament == 'US Open' & Round == 'F') +
                sum(Tournament == 'Australian Open' & Round == 'F') +
                sum(Tournament == 'French Open' & Round == 'F')),
              TotalTournyWins = sum(Round == 'F', na.rm = TRUE),
              TotalPts = sum(Winner_ServicePts + Loser_ServicePts, na.rm = TRUE),
              Aces = sum(Winner_Aces, na.rm = TRUE),
              DoubleFaults = sum(Winner_DblFaults, na.rm = TRUE),
              ServicePts = sum(Winner_ServicePts, na.rm = TRUE),
              FirstServesWon = sum(Winner_FirstServeWon, na.rm = TRUE),
              SecondServesWon = sum(Winner_SecondServeWon, na.rm = TRUE),
              ServiceGames = sum(Winner_ServiceGames, na.rm = TRUE),
              ServiceBreakSaved = sum(Winner_BreakSaved, na.rm = TRUE),
              ServiceBreaksFaced = sum(Winner_BreakFaced, na.rm = TRUE),
              BreakPoints = sum(Loser_BreakFaced, na.rm = TRUE),
              Breaks = sum(Loser_BreakFaced - Loser_BreakSaved, na.rm = TRUE))
  
  MenLoser <- FiltX %>% filter(complete.cases(Loser_Rank)) %>% group_by(Country = Loser_Country) %>% 
    summarise(Height = mean(Loser_Height, na.rm = TRUE)[1],
              `Best Rank` = sum(Loser_Rank, na.rm = TRUE),
              TotalW = 0,
              TotalL = n(),
              TotalGrandSlamWins = 0,
              TotalTournyWins = 0,
              TotalPts = sum(Winner_ServicePts + Loser_ServicePts, na.rm = TRUE),
              Aces = sum(Loser_Aces, na.rm = TRUE),
              DoubleFaults = sum(Loser_DblFaults, na.rm = TRUE),
              ServicePts = sum(Loser_ServicePts, na.rm = TRUE),
              FirstServesWon = sum(Loser_FirstServeWon, na.rm = TRUE),
              SecondServesWon = sum(Loser_SecondServeWon, na.rm = TRUE),
              ServiceGames = sum(Loser_ServiceGames, na.rm = TRUE),
              ServiceBreakSaved = sum(Loser_BreakSaved, na.rm = TRUE),
              ServiceBreaksFaced = sum(Loser_BreakFaced, na.rm = TRUE),
              BreakPoints = sum(Winner_BreakFaced, na.rm = TRUE),
              Breaks = sum(Winner_BreakFaced - Winner_BreakSaved, na.rm = TRUE))
  
  Map <- rbind(MenWinner, MenLoser) %>% filter(!Country == 'SN')
  Map <- Map %>% group_by(Country) %>% transmute(`Win Loss Ratio` = sum(TotalW)/(sum(TotalW) + sum(TotalL)),
                                                 `Total Matches` = (sum(TotalW) + sum(TotalL)),
                                                 `Avg Rank` = sum(`Best Rank`)/`Total Matches`,
                                                 `Total Grandslams Won` = sum(TotalGrandSlamWins),
                                                 `Total Tournament Wins` = sum(TotalTournyWins)) %>% 
    filter((`Total Matches` >= 10))
  
  return(Map)
}

Map = Map_Calc(Men[1,], as.character('1990-12-31'), as.character('2017-12-31'))
Map_choice <- colnames(Map)[-1]

# Players
Players <- as.vector((Men %>% distinct(Winner))[,1])
filterby <- c('Tournament', 'Surface', 'No Filter')
Surfaces <- c('All', 'Hard', 'Grass', 'Clay', 'Carpet')

filt_by <- function(df,x){
  filtered <- df %>% distinct(x)
  return(filtered)
}

Player_Calc <- function(init_df,filt1,filt2) {
  X <- init_df %>% filter(filt1 == filt2)
  MenWinner <- X %>% filter(complete.cases(Winner_Rank)) %>% group_by(Name = Winner) %>% 
    summarise(Hand = unique(Winner_Hand)[1],
              Height = mean(Winner_Height, na.rm = TRUE),
              Country = Winner_Country[1],
              `Best Rank` = min(Winner_Rank, na.rm = TRUE),
              TotalW = n(),
              TotalL = 0,
              TotalPts = sum(Winner_ServicePts + Loser_ServicePts, na.rm = TRUE),
              Aces = sum(Winner_Aces, na.rm = TRUE),
              DoubleFaults = sum(Winner_DblFaults, na.rm = TRUE),
              ServicePts = sum(Winner_ServicePts, na.rm = TRUE),
              FirstServesWon = sum(Winner_FirstServeWon, na.rm = TRUE),
              SecondServesWon = sum(Winner_SecondServeWon, na.rm = TRUE),
              ServiceGames = sum(Winner_ServiceGames, na.rm = TRUE),
              ServiceBreakSaved = sum(Winner_BreakSaved, na.rm = TRUE),
              ServiceBreaksFaced = sum(Winner_BreakFaced, na.rm = TRUE),
              BreakPoints = sum(Loser_BreakFaced, na.rm = TRUE),
              Breaks = sum(Loser_BreakFaced - Loser_BreakSaved, na.rm = TRUE))
  
  MenLoser <- X %>% filter(complete.cases(Loser_Rank)) %>% group_by(Name = Loser) %>% 
    summarise(Hand = unique(Loser_Hand)[1],
              Height = mean(Loser_Height, na.rm = TRUE)[1],
              Country = Loser_Country[1],
              `Best Rank` = min(Loser_Rank, na.rm = TRUE),
              TotalW = 0,
              TotalL = n(),
              TotalPts = sum(Winner_ServicePts + Loser_ServicePts, na.rm = TRUE),
              Aces = sum(Loser_Aces, na.rm = TRUE),
              DoubleFaults = sum(Loser_DblFaults, na.rm = TRUE),
              ServicePts = sum(Loser_ServicePts, na.rm = TRUE),
              FirstServesWon = sum(Loser_FirstServeWon, na.rm = TRUE),
              SecondServesWon = sum(Loser_SecondServeWon, na.rm = TRUE),
              ServiceGames = sum(Loser_ServiceGames, na.rm = TRUE),
              ServiceBreakSaved = sum(Loser_BreakSaved, na.rm = TRUE),
              ServiceBreaksFaced = sum(Loser_BreakFaced, na.rm = TRUE),
              BreakPoints = sum(Winner_BreakFaced, na.rm = TRUE),
              Breaks = sum(Winner_BreakFaced - Winner_BreakSaved, na.rm = TRUE))
  
  RawDataNumerical <- rbind(MenWinner, MenLoser) %>% select(c(1,6:18)) %>% group_by(Name) %>% summarise_all(sum)
  RawDataCategorical <- rbind(MenWinner, MenLoser) %>% select(1:5) %>% group_by(Name) %>% filter(complete.cases(is.numeric(`Best Rank`))) %>% 
    summarise(Hand = Hand[1],
              Height = mean(Height, na.rm = TRUE),
              Country = Country[1],
              `Best Rank` = as.integer(min(`Best Rank`)))
  
  RawData <- left_join(RawDataCategorical, RawDataNumerical, by = 'Name') %>% distinct()
  
  player <- RawData %>% transmute(Name,
                                       Hand,
                                       Height,
                                       Country,
                                       `Best Rank`,
                                       `Total Matches` = TotalW+TotalL,
                                       `Win Percentage` = TotalW/(TotalW + TotalL),
                                       Ace = Aces/ServicePts,
                                       `Double Fault Percentage` = DoubleFaults/ServicePts,
                                       `Service Point Won Percentage` = (FirstServesWon + SecondServesWon)/ServicePts,
                                       `Percent 1st Serve Won` = FirstServesWon/ServicePts,
                                       `Percent 2nd Serve Won` = SecondServesWon/ServicePts,
                                       `Hold Serve Percentage` = 1 - (ServiceBreaksFaced - ServiceBreakSaved)/ServiceGames,
                                       `Break Point Save Percentage` = ServiceBreakSaved/ServiceBreaksFaced,
                                       `Break Point Conversion` = Breaks/BreakPoints) %>% filter(complete.cases(Height)) %>%
    filter(complete.cases(Ace)) %>%
    filter(complete.cases(Height)) %>% arrange(-`Total Matches`)
  
  return(player)
}

Line_Charter <- function(init_df,filt1) {
  X <- init_df
  MenWinner <- X %>% filter(complete.cases(Winner_Rank)) %>% filter(Winner == filt1) %>% group_by(Age = Winner_Age) %>% 
    summarise(Hand = unique(Winner_Hand)[1],
              Height = mean(Winner_Height, na.rm = TRUE),
              Country = Winner_Country[1],
              `Best Rank` = min(Winner_Rank, na.rm = TRUE),
              TotalW = n(),
              TotalL = 0,
              TotalPts = sum(Winner_ServicePts + Loser_ServicePts, na.rm = TRUE),
              Aces = sum(Winner_Aces, na.rm = TRUE),
              DoubleFaults = sum(Winner_DblFaults, na.rm = TRUE),
              ServicePts = sum(Winner_ServicePts, na.rm = TRUE),
              FirstServesWon = sum(Winner_FirstServeWon, na.rm = TRUE),
              SecondServesWon = sum(Winner_SecondServeWon, na.rm = TRUE),
              ServiceGames = sum(Winner_ServiceGames, na.rm = TRUE),
              ServiceBreakSaved = sum(Winner_BreakSaved, na.rm = TRUE),
              ServiceBreaksFaced = sum(Winner_BreakFaced, na.rm = TRUE),
              BreakPoints = sum(Loser_BreakFaced, na.rm = TRUE),
              Breaks = sum(Loser_BreakFaced - Loser_BreakSaved, na.rm = TRUE))
  
  MenLoser <- X %>% filter(complete.cases(Loser_Rank)) %>% filter(Loser == filt1) %>% group_by(Age = Loser_Age) %>% 
    summarise(Hand = unique(Loser_Hand)[1],
              Height = mean(Loser_Height, na.rm = TRUE)[1],
              Country = Loser_Country[1],
              `Best Rank` = min(Loser_Rank, na.rm = TRUE),
              TotalW = 0,
              TotalL = n(),
              TotalPts = sum(Winner_ServicePts + Loser_ServicePts, na.rm = TRUE),
              Aces = sum(Loser_Aces, na.rm = TRUE),
              DoubleFaults = sum(Loser_DblFaults, na.rm = TRUE),
              ServicePts = sum(Loser_ServicePts, na.rm = TRUE),
              FirstServesWon = sum(Loser_FirstServeWon, na.rm = TRUE),
              SecondServesWon = sum(Loser_SecondServeWon, na.rm = TRUE),
              ServiceGames = sum(Loser_ServiceGames, na.rm = TRUE),
              ServiceBreakSaved = sum(Loser_BreakSaved, na.rm = TRUE),
              ServiceBreaksFaced = sum(Loser_BreakFaced, na.rm = TRUE),
              BreakPoints = sum(Winner_BreakFaced, na.rm = TRUE),
              Breaks = sum(Winner_BreakFaced - Winner_BreakSaved, na.rm = TRUE))
  
  RawDataNumerical <- rbind(MenWinner, MenLoser) %>% select(c(1,6:18)) %>% group_by(Age) %>% summarise_all(sum)
  RawDataCategorical <- rbind(MenWinner, MenLoser) %>% select(1:5) %>% group_by(Age) %>% filter(complete.cases(is.numeric(`Best Rank`))) %>% 
    summarise(Hand = Hand[1],
              Height = mean(Height, na.rm = TRUE),
              Country = Country[1],
              `Best Rank` = as.integer(min(`Best Rank`)))
  
  RawData <- left_join(RawDataCategorical, RawDataNumerical, by = 'Age') %>% distinct()
  
  player <- RawData %>% transmute(Age,
                                  Ranking = `Best Rank`,
                                  `Total Matches` = cumsum(TotalW+TotalL),
                                  `Win Percentage` = cumsum(TotalW)/`Total Matches`,
                                  Ace = cumsum(Aces)/cumsum(ServicePts),
                                  `Double Fault Percentage` = cumsum(DoubleFaults)/cumsum(ServicePts),
                                  `Service Point Won Percentage` = cumsum((FirstServesWon + SecondServesWon))/cumsum(ServicePts),
                                  `Percent 1st Serve Won` = cumsum(FirstServesWon)/cumsum(ServicePts),
                                  `Percent 2nd Serve Won` = cumsum(SecondServesWon)/cumsum(ServicePts),
                                  `Break Point Defense` = cumsum(ServiceBreaksFaced - ServiceBreakSaved)/cumsum(ServiceGames),
                                  `Break Point Save Percentage` = cumsum(ServiceBreakSaved)/cumsum(ServiceBreaksFaced),
                                  `Break Point Conversion` = cumsum(Breaks)/cumsum(BreakPoints)) %>% 
                                  filter(complete.cases(Ranking))
  
  return(player)
}

stats_age = colnames(Line_Charter(Men,'Roger Federer'))[-1]

wins_against <- function(player1, player2, df, surf) {
  wins <- df %>% filter(Winner == player1 & Loser == player2)
  losses <- df %>% filter(Winner == player2 & Loser == player1)
  if (!surf == 'All') {
    wins <- wins %>% filter(Surface == surf)
    losses <- losses %>% filter(Surface == surf)
  } else {
  }
  wins <- wins %>% summarise(n())
  losses <- losses %>% summarise(n())
  results <- paste(as.character(wins[1]), '-', as.character(losses[1]), collapse = '')
  return(results)
}

match_history <- function(player1, player2, df, surf) {
  df %>% filter(Surface == surf) %>% 
    filter((Winner == input$player1 & Loser == input$player2) & (Winner == input$player2 & Loser == input$player1)) %>% 
    select(Date, Tournament, Round, Surface, Winner, Loser, Score)
  return(df)
}

